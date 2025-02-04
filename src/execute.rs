use core::fmt;
use std::{collections::{HashMap, VecDeque}, env, ffi::CString, fmt::Display, os::fd::{AsFd, AsRawFd, FromRawFd, IntoRawFd, RawFd}, path::Path, sync::{Arc, Mutex}};

use libc::MFD_CLOEXEC;
use nix::{errno::Errno, fcntl::{open, OFlag}, sys::{memfd::{memfd_create, MemFdCreateFlag}, signal::Signal, stat::Mode, wait::WaitStatus}, unistd::{close, dup, dup2, execvpe, fork, pipe, ForkResult, Pid}};
use pest::{iterators::Pair, Parser};

use crate::error::LashErrHigh;
use crate::error::LashErrLow::*;
use crate::error::LashErr::*;
use crate::{error::{LashErr, LashErrLow}, exec_list, expand, helper, shellenv::{self, read_vars, write_meta, write_vars, ChildProc, JobBuilder, LashVal}, LashParse, LashResult, PairExt, Rule};

const SHELL_CMDS: [&str;6] = [
	"for",
	"while",
	"select",
	"match",
	"until",
	"if"
];

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct RustFd {
	fd: RawFd,
}

impl<'a> RustFd {
	pub fn new(fd: RawFd) -> LashResult<Self> {
		if fd < 0 {
			panic!()
		}
		Ok(RustFd { fd })
	}

	/// Create a `RustFd` from a duplicate of `stdin` (FD 0)
	pub fn from_stdin() -> LashResult<Self> {
		let fd = dup(0).map_err(|_| LashErr::Low(LashErrLow::from_io()))?;
		Ok(Self { fd })
	}

	/// Create a `RustFd` from a duplicate of `stdout` (FD 1)
	pub fn from_stdout() -> LashResult<Self> {
		let fd = dup(1).map_err(|_| LashErr::Low(LashErrLow::from_io()))?;
		Ok(Self { fd })
	}

	/// Create a `RustFd` from a duplicate of `stderr` (FD 2)
	pub fn from_stderr() -> LashResult<Self> {
		let fd = dup(2).map_err(|_| LashErr::Low(LashErrLow::from_io()))?;
		Ok(Self { fd })
	}

	/// Create a `RustFd` from a type that provides an owned or borrowed FD
	pub fn from_fd<T: AsFd>(fd: T) -> LashResult<Self> {
		let raw_fd = fd.as_fd().as_raw_fd();
		if raw_fd < 0 {
			return Err(LashErr::Low(LashErrLow::BadFD {
				context: "Attempted to create a RustFd from a negative int".into(),
				fd: raw_fd
			}))
		}
		Ok(RustFd { fd: raw_fd })
	}

	/// Create a `RustFd` by consuming ownership of an FD
	pub fn from_owned_fd<T: IntoRawFd>(fd: T) -> LashResult<Self> {
		let raw_fd = fd.into_raw_fd(); // Consumes ownership
		if raw_fd < 0 {
			return Err(LashErr::Low(LashErrLow::BadFD {
				context: "Attempted to create a RustFd from a negative int".into(),
				fd: raw_fd
			}))
		}
		Ok(RustFd { fd: raw_fd })
	}

	/// Create a new `RustFd` that points to an in-memory file descriptor. In-memory file descriptors can be interacted with as though they were normal files.
	pub fn new_memfd(name: &str, executable: bool) -> LashResult<Self> {
		let c_name = CString::new(name).unwrap();
		let flags = if executable {
			MemFdCreateFlag::empty()
		} else {
			MemFdCreateFlag::MFD_CLOEXEC
		};
		let fd = memfd_create(&c_name, flags).map_err(|_| LashErr::Low(LashErrLow::from_io()))?;
		Ok(RustFd { fd: fd.as_raw_fd() })
	}

	/// Write some bytes to the contained file descriptor
	pub fn write(&self, buffer: &[u8]) -> LashResult<()> {
		if !self.is_valid() {
			panic!()
		}
		let result = unsafe { libc::write(self.fd, buffer.as_ptr() as *const libc::c_void, buffer.len()) };
		if result < 0 {
			return Err(LashErr::Low(LashErrLow::from_io()))
		} else {
			Ok(())
		}
	}

	pub fn read(&self) -> LashResult< String> {
		if !self.is_valid() {
			return Err(LashErr::Low(LashErrLow::BadFD {
				context: "Attempted to read from an invalid RustFd".into(),
				fd: self.fd
			}))
		}
		let mut buffer = [0; 1024];
		let mut output = String::new();

		loop {
			match nix::unistd::read(self.as_raw_fd(), &mut buffer) {
				Ok(0) => break, // End of pipe
				Ok(bytes_read) => {
					output.push_str(&String::from_utf8_lossy(&buffer[..bytes_read]));
				}
				Err(_) => return Err(LashErr::Low(LashErrLow::from_io()))
			}
		}
		Ok(output)
	}

	/// Wrapper for nix::unistd::pipe(), simply produces two `RustFds` that point to a read and write pipe respectfully
	pub fn pipe() -> LashResult<(Self,Self)> {
		let (r_pipe,w_pipe) = pipe().map_err(|_| LashErr::Low(LashErrLow::from_io()))?;
		let r_fd = RustFd::from_owned_fd(r_pipe)?;
		let w_fd = RustFd::from_owned_fd(w_pipe)?;
		Ok((r_fd,w_fd))
	}

	/// Produce a `RustFd` that points to the same resource as the 'self' `RustFd`
	pub fn dup(&self) -> LashResult<Self> {
		if !self.is_valid() {
			return Err(LashErr::Low(LashErrLow::BadFD {
				context: "Attempted to call `dup()` on an invalid RustFd".into(),
				fd: self.fd
			}))
		}
		let new_fd = dup(self.fd).unwrap();
		Ok(RustFd { fd: new_fd })
	}

	/// A wrapper for nix::unistd::dup2(), 'self' is duplicated to the given target file descriptor.
	pub fn dup2<T: AsRawFd>(&self, target: &T) -> LashResult<()> {
		let target_fd = target.as_raw_fd();
		if self.fd == target_fd {
			// Nothing to do here
			return Ok(())
		}
		if !self.is_valid() || target_fd < 0 {
			return Err(LashErr::Low(LashErrLow::BadFD {
				context: "Attempted to call `dup2()` on an invalid RustFd".into(),
				fd: self.fd
			}))
		}

		dup2(self.fd, target_fd).unwrap();
		Ok(())
	}

	/// Open a file using a file descriptor, with the given OFlags and Mode bits
	pub fn open(path: &Path, flags: OFlag, mode: Mode) -> LashResult<Self> {
		let file_fd: RawFd = open(path, flags, mode).unwrap();
		Ok(Self { fd: file_fd })
	}

	pub fn close(&mut self) -> LashResult<()> {
		if !self.is_valid() {
			return Ok(())
		}
		close(self.fd).unwrap();
		self.fd = -1;
		Ok(())
	}

	pub fn mk_shared(self) -> Arc<Mutex<Self>> {
		Arc::new(Mutex::new(self))
	}

	pub fn is_valid(&self) -> bool {
		self.fd > 0
	}
}

impl Display for RustFd {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.fd)
	}
}

impl Drop for RustFd {
	#[track_caller]
	fn drop(&mut self) {
		if self.fd >= 0 && self.close().is_err() {
		}
	}
}

impl AsRawFd for RustFd {
	fn as_raw_fd(&self) -> RawFd {
		self.fd
	}
}

impl IntoRawFd for RustFd {
	fn into_raw_fd(self) -> RawFd {
		let fd = self.fd;
		std::mem::forget(self);
		fd
	}
}

impl FromRawFd for RustFd {
	unsafe fn from_raw_fd(fd: RawFd) -> Self {
		RustFd { fd }
	}
}

#[derive(PartialEq,Debug,Clone)]
pub enum LashWait {
	Success,
	Fail { code: i32, cmd: Option<String> },
	Signaled { sig: Signal },
	Stopped { sig: Signal },
	Terminated { signal: i32 },
	Continued,
	Running,
	Killed { signal: i32 },
	TimeOut,

	// These wait statuses are returned by builtins like `return` and `break`
	RETURN { code: i32 }, // Return from a function
	CONTINUE, // Restart a loop from the beginning
	BREAK, // Break a loop
	EXIT { code: i32 }
}

impl LashWait {
	pub fn new() -> Self {
		LashWait::Success
	}
	pub fn raw(&self) -> i32 {
		match *self {
			LashWait::Success => 0,
			LashWait::Fail { code, cmd: _ } => code,
			_ => unimplemented!("unimplemented signal type: {:?}", self)
		}
	}
	pub fn from_wait(wait: WaitStatus, cmd: Option<String>) -> Self {
		match wait {
			WaitStatus::Exited(_, code) => {
				match code {
					0 => LashWait::Success,
					_ => LashWait::Fail { code, cmd }
				}
			}
			WaitStatus::Signaled(_, sig, _) => LashWait::Signaled { sig },
			WaitStatus::Stopped(_, sig) => LashWait::Stopped { sig },
			WaitStatus::PtraceEvent(_, _, _) => todo!(),
			WaitStatus::PtraceSyscall(_) => todo!(),
			WaitStatus::Continued(_) => LashWait::Continued,
			WaitStatus::StillAlive => LashWait::Running
		}
	}
}

impl Display for LashWait {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			LashWait::Success { .. } => write!(f, "done"),
			LashWait::Fail { code, .. } => write!(f, "exit {}", code),
			LashWait::Signaled { sig } => write!(f, "exit {}", sig),
			LashWait::Stopped { .. } => write!(f, "stopped"),
			LashWait::Terminated { signal } => write!(f, "terminated {}", signal),
			LashWait::Continued => write!(f, "continued"),
			LashWait::Running => write!(f, "running"),
			LashWait::Killed { signal } => write!(f, "killed {}", signal),
			LashWait::TimeOut => write!(f, "time out"),
			_ => write!(f, "{:?}",self)
		}
	}
}


impl Default for LashWait {
	fn default() -> Self {
		LashWait::new()
	}
}

#[derive(Debug)]
pub struct ProcIO {
	pub stdin: Option<Arc<Mutex<RustFd>>>,
	pub stdout: Option<Arc<Mutex<RustFd>>>,
	pub stderr: Option<Arc<Mutex<RustFd>>>,
	pub backup: HashMap<RawFd,RustFd>
}

impl<'a> ProcIO {
	pub fn new() -> Self {
		Self { stdin: None, stdout: None, stderr: None, backup: HashMap::new() }
	}
	pub fn from(stdin: Option<Arc<Mutex<RustFd>>>, stdout: Option<Arc<Mutex<RustFd>>>, stderr: Option<Arc<Mutex<RustFd>>>) -> Self {
		Self {
			stdin,
			stdout,
			stderr,
			backup: HashMap::new(),
		}
	}
	pub fn close_all(&mut self) -> LashResult<()> {
		if let Some(fd) = &self.stdin {
			fd.lock().unwrap().close()?;
		}
		if let Some(fd) = &self.stdout {
			fd.lock().unwrap().close()?;
		}
		if let Some(fd) = &self.stderr {
			fd.lock().unwrap().close()?;
		}
		Ok(())
	}
	pub fn backup_fildescs(&mut self) -> LashResult<()> {
		let mut backup = HashMap::new();
		// Get duped file descriptors
		let dup_in = RustFd::from_stdin()?;
		let dup_out = RustFd::from_stdout()?;
		let dup_err = RustFd::from_stderr()?;
		// Store them in a hashmap
		backup.insert(0,dup_in);
		backup.insert(1,dup_out);
		backup.insert(2,dup_err);
		self.backup = backup;
		Ok(())
	}
	pub fn restore_fildescs(&mut self) -> LashResult<()> {
		// Get duped file descriptors from hashmap
		if !self.backup.is_empty() {
			// Dup2 to restore file descriptors
			if let Some(mut saved_in) = self.backup.remove(&0) {
				saved_in.dup2(&0)?;
				saved_in.close()?;
			}
			if let Some(mut saved_out) = self.backup.remove(&1) {
				saved_out.dup2(&1)?;
				saved_out.close()?;
			}
			if let Some(mut saved_err) = self.backup.remove(&2) {
				saved_err.dup2(&2)?;
				saved_err.close()?;
			}
		}
		Ok(())
	}
	pub fn route_input(&mut self) -> LashResult<()> {
		if let Some(ref mut stdin_fd) = self.stdin {
			let mut fd = stdin_fd.lock().unwrap();
			fd.dup2(&0)?;
			fd.close()?;
		}
		Ok(())
	}
	pub fn route_output(&mut self) -> LashResult<()> {
		if let Some(ref mut stdout_fd) = self.stdout {
			let mut fd = stdout_fd.lock().unwrap();
			fd.dup2(&1)?;
			fd.close()?;
		}
		if let Some(ref mut stderr_fd) = self.stderr {
			let mut fd = stderr_fd.lock().unwrap();
			fd.dup2(&2)?;
			fd.close()?;
		}
		Ok(())
	}
	pub fn route_io(&mut self) -> LashResult<()> {
		self.route_input()?;
		self.route_output()
	}
	pub fn take(&mut self) -> Self {
		std::mem::take(self)
	}
}

impl Clone for ProcIO {
	/// Use this sparingly- this was implemented to make ProcIO more wieldy when used in structs that implement Clone,
	/// but for all intents and purposes the ProcIO struct is meant to be a unique identifier for an open file descriptor.
	/// Use this if you have to, but know that it may cause unintended side effects.
	///
	/// Since ProcIO uses Arc<Mutex<RustFd>>, these clones will refer to the same data as the original. That means modifications will effect both instances.
	fn clone(&self) -> Self {
		ProcIO::from(self.stdin.clone(),self.stdout.clone(),self.stderr.clone())
	}
}

impl Default for ProcIO {
	fn default() -> Self {
		Self::new()
	}
}

pub fn descend<'a>(mut node_stack: Vec<Pair<'a,Rule>>, mut io: ProcIO) -> LashResult<LashWait> {
	let mut last_status = LashWait::new();
	while let Some(node) = node_stack.pop() {
		match node.as_rule() {
			Rule::main | Rule::cmd_list => {
				let inner = node.to_vec_rev();
				node_stack.extend(inner);
			}
			Rule::simple_cmd => last_status = exec_cmd(node, io.take())?,
			Rule::shell_cmd => {
				let shell_cmd = node.to_vec_rev().pop().unwrap();
				match shell_cmd.as_rule() {
					Rule::for_cmd => exec_for_cmd(shell_cmd, io.take())?,
					Rule::match_cmd => exec_match_cmd(shell_cmd, io.take())?,
					Rule::loop_cmd => exec_loop_cmd(shell_cmd, io.take())?,
					Rule::if_cmd => exec_if_cmd(shell_cmd, io.take())?,
					Rule::subshell => todo!(),
					Rule::brace_grp => todo!(),
					Rule::assignment => todo!(),
					Rule::func_def => todo!(),
					_ => unreachable!()
				};
			}
			Rule::op => {
				let option = node.unpack();
				if let Some(op) = option {
					match op.as_rule() {
						Rule::and => {
							let is_success = shellenv::check_status()? == "0";
							if !is_success {
								break
							}
						}
						Rule::or => {
							let is_success = shellenv::check_status()? == "0";
							if is_success {
								break
							}
						}
						_ => unreachable!()
					}
				}
			}
			Rule::pipeline => { }
			Rule::EOI => { /* Do nothing */ }
			_ => todo!("Support for rule '{:?}' is unimplemented",node.as_rule())
		}
	}
	Ok(last_status)
}

fn exec_pipeline<'a>(pipeline: Pair<'a,Rule>, io: ProcIO) -> LashResult<LashWait> {
	let mut inner = pipeline.into_inner();
	let mut prev_read_pipe: Option<RustFd> = None;
	let mut pgid: Option<Pid> = None;
	let mut cmds: Vec<String> = vec![];
	let mut pids: Vec<Pid> = vec![];

	while let Some(node) = inner.next() {
		match node.as_rule() {
			Rule::simple_cmd => {
			}
			Rule::shell_cmd => {
			}
			_ => unreachable!()
		}
	}
}

fn exec_for_cmd<'a>(cmd: Pair<'a,Rule>,io: ProcIO) -> LashResult<LashWait> {
	let mut inner = cmd.into_inner();
	let body_io = ProcIO::from(None, io.stdout, io.stderr);
	let mut saved_vars = HashMap::new();
	let loop_vars = inner.next()
		.unwrap()
		.into_inner()
		.into_iter()
		.map(|var| var.as_str())
		.collect::<Vec<&str>>();
	for var in &loop_vars {
		let existing_val = read_vars(|v| v.get_var(var))?.unwrap_or_default();
		saved_vars.insert(var,existing_val);
	}
	let vars_len = loop_vars.len();
	let loop_arr = inner.next()
		.unwrap()
		.into_inner()
		.map(|elem| LashVal::parse(elem.as_str()).unwrap())
		.collect::<Vec<LashVal>>();
	let loop_body_pair = inner.next().unwrap();

	for (i,element) in loop_arr.iter().enumerate() {
		let var_index = i % vars_len;
		write_vars(|v| v.set_var(loop_vars[var_index], element.clone()))?;
		let loop_body = expand::expand_list(loop_body_pair.clone(),true)?;
		exec_list(Rule::cmd_list, loop_body, Some(body_io.clone()))?;
	}
	for var in &loop_vars {
		let saved_val = saved_vars.remove(var).unwrap_or_default();
		write_vars(|v| v.set_var(var, saved_val))?;
	}
	Ok(LashWait::Success)
}

fn exec_match_cmd<'a>(cmd: Pair<'a,Rule>, io: ProcIO) -> LashResult<LashWait> {
	let mut inner = cmd.into_inner();
	let match_pat = inner.next().unwrap();
	let mut arms = VecDeque::new();

	while let Some(arm) = inner.next() {
		arms.push_back(arm);
	}

	while let Some(arm) = arms.pop_front() {
		let mut inner = arm.into_inner();
		let arm_pat = inner.next().unwrap();
		let arm_body_pair = inner.next().unwrap();

		if arm_pat.as_str().trim() == match_pat.as_str().trim() {
			let arm_body = expand::expand_list(arm_body_pair,true)?;
			exec_list(Rule::cmd_list, arm_body.trim_end_matches(',').to_string(), Some(io))?;
			break
		}
	}
	Ok(LashWait::Success)
}

fn exec_if_cmd<'a>(cmd: Pair<'a,Rule>, mut io: ProcIO) -> LashResult<LashWait> {
	let mut inner = cmd.to_vec_rev();
	let if_cond_pair = inner.pop().unwrap().into_inner().next().unwrap();
	let if_body_pair = inner.pop().unwrap().into_inner().next().unwrap();
	let mut elif_blocks = VecDeque::new();
	let mut else_block = None;
	let cond_io = ProcIO::from(io.stdin, None, None);
	let body_io = ProcIO::from(None, io.stdout, io.stderr);

	while let Some(pair) = inner.pop() {
		match pair.as_rule() {
			Rule::elif_block => elif_blocks.push_back(pair),
			Rule::else_block => {
				else_block = Some(pair);
				break
			}
			_ => unreachable!()
		}
	}

	let if_cond = expand::expand_list(if_cond_pair,true)?;
	exec_list(Rule::cmd_list, if_cond, Some(cond_io.clone()))?;
	if &shellenv::check_status()? == "0" {
		let if_body = expand::expand_list(if_body_pair,true)?;
		exec_list(Rule::cmd_list, if_body, Some(body_io.clone()))?;
		return Ok(LashWait::Success)
	}

	while let Some(elif_block) = elif_blocks.pop_front() {
		let mut inner = elif_block.into_inner();
		let elif_cond_pair = inner.next().unwrap();
		let elif_body_pair = inner.next().unwrap();

		let elif_cond = expand::expand_list(elif_cond_pair,true)?;
		exec_list(Rule::cmd_list, elif_cond, Some(cond_io.clone()))?;
		if &shellenv::check_status()? == "0" {
			let elif_body = expand::expand_list(elif_body_pair,true)?;
			exec_list(Rule::cmd_list, elif_body, Some(body_io.clone()))?;
			return Ok(LashWait::Success)
		}
	}

	if let Some(else_block) = else_block {
		let else_body_pair = else_block.into_inner().next().unwrap();
		let else_body = expand::expand_list(else_body_pair,true)?;
		exec_list(Rule::cmd_list, else_body, Some(body_io.clone()))?;
	}

	Ok(LashWait::Success)
}

fn exec_loop_cmd<'a>(cmd: Pair<'a,Rule>, mut io: ProcIO) -> LashResult<LashWait> {
	let mut inner = cmd.to_vec_rev();
	let loop_kind = inner.pop().unwrap();
	let loop_cond_pair = inner.pop().unwrap().into_inner().next().unwrap();
	let loop_body_pair = inner.pop().unwrap().into_inner().next().unwrap();
	let cond_io = ProcIO::from(io.stdin, None, None);
	let body_io = ProcIO::from(None, io.stdout, io.stderr);


	loop {
		let loop_cond = expand::expand_list(loop_cond_pair.clone(),true)?;
		exec_list(Rule::cmd_list, loop_cond, Some(cond_io.clone()))?;
		let is_success = shellenv::check_status()? == "0";
		match loop_kind.as_str() {
			"while" => {
				if !is_success {
					write_vars(|v| v.set_param("?".into(), "0".into()))?;
					write_meta(|v| v.set_last_command("while"))?;
					break
				}
			}
			"until" => {
				if is_success {
					write_vars(|v| v.set_param("?".into(), "0".into()))?;
					write_meta(|v| v.set_last_command("until"))?;
					break
				}
			}
			_ => unreachable!()
		}
		let loop_body = expand::expand_list(loop_body_pair.clone(),true)?;
		let result = exec_list(Rule::cmd_list, loop_body, Some(body_io.clone()));
		match result {
			Err(LashErr::High(err)) => {
				match err.get_err() {
					LashErrLow::LoopBreak => break,
					LashErrLow::LoopCont => continue,
					_ => return Err(LashErr::High(err))
				}
			}
			Err(e) => return Err(e),
			Ok(_) => continue,
		}
	}
	Ok(LashWait::Success)
}

fn exec_cmd<'a>(cmd: Pair<Rule>, mut io: ProcIO) -> LashResult<LashWait> {
	let mut last_status = LashWait::new();
	let err_target = cmd.clone();
	let mut argv = cmd.clone().to_vec().into_iter().map(|arg| CString::new(arg.as_str().to_string()).unwrap()).collect::<Vec<_>>();

	io.backup_fildescs()?;
	io.route_io()?;

	let command = argv.first().unwrap().clone();

	if SHELL_CMDS.contains(&command.to_str().unwrap()) {
		return Err(High(LashErrHigh::exec_err(format!("This shell command appears malformed"), cmd)))
	}

	let env_vars = env::vars().into_iter().collect::<Vec<(String,String)>>();
	let envp = env_vars.iter().map(|var| CString::new(format!("{}={}",var.0,var.1)).unwrap()).collect::<Vec<_>>();

	match unsafe { fork() } {
		Ok(ForkResult::Child) => {
			let Err(e) = execvpe(&command, &argv, &envp);
			match e {
				Errno::ENOENT => {
					let error = High(LashErrHigh::cmd_not_found(command.to_str().unwrap(), err_target));
					eprintln!("{}",error);
				}
				Errno::EPERM => {
					let error = High(LashErrHigh::no_permission(command.to_str().unwrap(), err_target));
					eprintln!("{}",error);
				}
				_ => unimplemented!("Case for `{}` not implemented", e.to_string())
			}
			std::process::exit(e as i32)
		}
		Ok(ForkResult::Parent { child }) => {
			handle_parent_process(child, command.to_str().unwrap().to_string())?;
		}
		Err(_) => todo!()
	}

	Ok(last_status)
}

fn handle_parent_process<'a>(child: Pid, command: String) -> LashResult<()> {
	let children = vec![
		ChildProc::new(child, Some(&command), None)?
	];
	let job = JobBuilder::new()
		.with_children(children)
		.with_pgid(child)
		.build();

	helper::handle_fg(job)?;
	/*
	if node.flags.contains(NdFlags::BACKGROUND) {
		write_jobs(|j| j.insert_job(job,false))??;
	} else {
		helper::handle_fg(job)?;
	}
	*/
	Ok(())
}
