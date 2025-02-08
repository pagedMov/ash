use core::fmt;
use std::{collections::{HashMap, HashSet, VecDeque}, env, ffi::CString, fmt::Display, os::fd::{AsFd, AsRawFd, FromRawFd, IntoRawFd, RawFd}, path::{Path, PathBuf}, sync::{Arc, Mutex, MutexGuard}};

use libc::MFD_CLOEXEC;
use nix::{errno::Errno, fcntl::{fcntl, open, FcntlArg::F_GETFD, OFlag}, sys::{memfd::{memfd_create, MemFdCreateFlag}, signal::Signal, stat::{fstat, Mode}, wait::WaitStatus}, unistd::{close, dup, dup2, execve, execvpe, fork, pipe, ForkResult, Pid}};
use pest::{iterators::Pair, Parser};

use crate::{builtin::{self, BUILTINS}, error::LashErrHigh, exec_input, helper::{handle_fg, proc_res, StrExtension}, shellenv::{read_logic, read_meta, EnvFlags, SavedEnv}, pair::OptPairExt};
use crate::error::LashErrLow::*;
use crate::error::LashErr::*;
use crate::{error::{LashErr, LashErrLow}, exec_list, expand, helper, shellenv::{self, read_vars, write_meta, write_vars, ChildProc, JobBuilder, LashVal}, LashParse, LashResult, pair::PairExt, Rule};

const SHELL_CMDS: [&str;6] = [
	"for",
	"while",
	"select",
	"match",
	"until",
	"if"
];

bitflags::bitflags! {
	#[derive(Debug,Clone,Copy)]
	pub struct ExecFlags: u32 {
		const NO_FORK    = 0b00000000000000000000000000000001;
		const BACKGROUND = 0b00000000000000000000000000000010;
		const IN_PIPE    = 0b00000000000000000000000000000100;
	}
}

#[derive(Debug,Clone)]
pub struct ExecCtx {
	io: ProcIO,
	redir_queue: VecDeque<Redir>,
	flags: ExecFlags,
	last_status: i32,
	depth: usize,
	state_stack: Vec<Box<ExecCtx>>,
	max_recurse_depth: usize
}

impl ExecCtx {
	pub fn new() -> Self {
		let last_status = shellenv::check_status().unwrap().parse::<i32>().unwrap();
		Self {
			io: ProcIO::new(),
			redir_queue: VecDeque::new(),
			flags: ExecFlags::empty(),
			last_status,
			depth: 0,
			state_stack: vec![], // Each alteration is local to a single layer of recursion
			max_recurse_depth: read_meta(|m| m.get_shopt("core.max_recurse_depth")).unwrap().unwrap().parse::<usize>().unwrap()
		}
	}
	/// Creates a new instance of ExecCtx which retains only the standard input of the original
	/// Used in shell structures like `if` and `while` to direct input to the condition
	pub fn as_cond(&self) -> Self {
		let mut clone = self.clone();
		let io = clone.io_mut();
		*io = ProcIO::from(io.stdin.clone(), None, None);
		clone.redir_queue.retain(|rdr| {
			matches!(rdr.redir_type(), Rule::r#in | Rule::herestring | Rule::heredoc)
		});
		clone
	}
	/// Creates a new instance of ExecCtx which retains only the stdout and stderr of the original
	/// Used in shell structures like `if` and `while` to direct output from the body
	pub fn as_body(&self) -> Self {
		let mut clone = self.clone();
		let io = clone.io_mut();
		*io = ProcIO::from(None, io.stdout.clone(), io.stderr.clone());
		clone.redir_queue.retain(|rdr| {
			!matches!(rdr.redir_type(), Rule::r#in | Rule::herestring | Rule::heredoc)
		});
		clone
	}
	pub fn refresh(&mut self) -> LashResult<()> {
		*self = ExecCtx::new();
		Ok(())
	}
	pub fn push_state(&mut self) -> LashResult<()> {
		let saved_state = Box::new(self.clone());
		self.state_stack.push(saved_state);
		Ok(())
	}
	pub fn pop_state(&mut self) -> LashResult<()> {
		if let Some(state) = self.state_stack.pop() {
			*self = *state;
		}
		Ok(())
	}
	pub fn set_io(&mut self, io: ProcIO) {
		self.io = io
	}
	pub fn ascend(&mut self) -> LashResult<()> {
		self.pop_state()?;
		self.depth = self.depth.saturating_sub(1);
		Ok(())
	}
	pub fn descend(&mut self) -> LashResult<()> {
		self.push_state()?;
		self.depth += 1;
		if self.depth >= self.max_recurse_depth {
			Err(Low(LashErrLow::InternalErr(format!("Hit maximum recursion depth during execution: {}",self.max_recurse_depth))))
		} else {
			Ok(())
		}
	}
	pub fn depth(&self) -> usize {
		self.depth
	}
	pub fn flags_mut(&mut self) -> &mut ExecFlags {
		&mut self.flags
	}
	pub fn flags(&self) -> ExecFlags {
		self.flags
	}
	pub fn io_mut(&mut self) -> &mut ProcIO {
		&mut self.io
	}
	pub fn set_redirs(&mut self, redirs: VecDeque<Redir>) {
		self.redir_queue = redirs
	}
	pub fn extend_redirs(&mut self, redirs: VecDeque<Redir>) {
		self.redir_queue.extend(redirs);
	}
	pub fn push_redir(&mut self,redir: Redir) {
		self.redir_queue.push_back(redir)
	}
	pub fn pop_redir(&mut self) -> Option<Redir> {
		self.redir_queue.pop_back()
	}
	pub fn redirs(&mut self) -> Vec<Redir> {
		let redirs = self.redir_queue.clone();
		// Let's reverse it so that pop() takes them in the right order
		redirs.into_iter().rev().collect::<Vec<_>>()
	}
	pub fn update_status(&mut self) -> LashResult<()> {
		let status = shellenv::check_status()?;
		self.last_status = status.parse::<i32>().unwrap();
		Ok(())
	}
}

#[derive(Debug,Clone)]
pub struct Redir {
	redir_type: Rule,
	source: i32,
	fd_target: Option<i32>,
	file_target: Option<PathBuf>
}

impl Redir {
	pub fn from_pair(pair: Pair<Rule>) -> LashResult<Self> {
		if let Rule::redir = pair.as_rule() {
			let mut inner = pair.into_inner();
			let mut redir_type = None;
			let mut source = None;
			let mut fd_target = None;
			let mut file_target = None;
			while let Some(pair) = inner.next() {
				match pair.as_rule() {
					Rule::fd_out => {
						let fd = pair.as_str().parse::<i32>().unwrap();
						source = Some(fd);
					}
					Rule::file => {
						let path = PathBuf::from(pair.as_str());
						file_target = Some(path);
					}
					Rule::fd_target => {
						let fd = pair.as_str().parse::<i32>().unwrap();
						fd_target = Some(fd);
					}
					Rule::r#in |
					Rule::out |
					Rule::force_out |
					Rule::in_out |
					Rule::append |
					Rule::heredoc |
					Rule::herestring => redir_type = Some(pair.as_rule()),
					_ => unreachable!()
				}
			}
			let source = source.unwrap_or(match redir_type.unwrap() {
				Rule::r#in |
				Rule::herestring |
				Rule::heredoc => 0,
				_ => 1
			});

			Ok(
				Self {
					redir_type: redir_type.unwrap(),
					source,
					fd_target,
					file_target
				}
			)
		} else {
			Err(Low(LashErrLow::InternalErr(format!("Expected a redir rule in redir construction got this: {:?}", pair.as_rule()))))
		}
	}
	fn redir_type(&self) -> Rule {
		self.redir_type
	}
}

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
			return Err(LashErr::Low(LashErrLow::BadFD("Attempted to create a RustFd from a negative int".into())))
		}
		Ok(RustFd { fd: raw_fd })
	}

	/// Create a `RustFd` by consuming ownership of an FD
	pub fn from_owned_fd<T: IntoRawFd>(fd: T) -> LashResult<Self> {
		let raw_fd = fd.into_raw_fd(); // Consumes ownership
		if raw_fd < 0 {
			return Err(LashErr::Low(LashErrLow::BadFD("Attempted to create a RustFd from a negative int".into())))
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
			return Err(LashErr::Low(LashErrLow::BadFD("Attempted to read from an invalid RustFd".into())))
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
			return Err(LashErr::Low(LashErrLow::BadFD("Attempted to call `dup()` on an invalid RustFd".into())))
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
			return Err(LashErr::Low(LashErrLow::BadFD("Attempted to call `dup2()` on an invalid RustFd".into())))
		}

		dup2(self.fd, target_fd).unwrap();
		Ok(())
	}

	/// Open a file using a file descriptor, with the given OFlags and Mode bits
	pub fn open(path: &Path, flags: OFlag, mode: Mode) -> LashResult<Self> {
		let file_fd = open(path, flags, mode);
		if let Ok(file_fd) = file_fd {
			Ok(Self { fd: file_fd })
		} else {
			return Err(Low(LashErrLow::BadFD(format!("Attempted to open non-existant file '{}'",path.to_str().unwrap()))))
		}
	}

	pub fn close(&mut self) -> LashResult<()> {
		if !self.is_valid() {
			return Ok(())
		}
		if matches!(self.as_raw_fd(), 0 | 1 | 2) {
			self.fd = -1;
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

pub struct SavedIO {
	in_fd: RustFd,
	out_fd: RustFd,
	err_fd: RustFd
}

impl SavedIO {
	pub fn new() -> LashResult<Self> {
		Ok(Self {
			in_fd: RustFd::from_stdin()?,
			out_fd: RustFd::from_stdout()?,
			err_fd: RustFd::from_stderr()?
		})
	}
	pub fn restore(self) -> LashResult<()> {
		self.in_fd.dup2(&0)?;
		self.out_fd.dup2(&1)?;
		self.err_fd.dup2(&2)?;
		std::mem::drop(self);
		Ok(())
	}
}

#[derive(Debug)]
pub struct ProcIO {
	pub stdin: Option<Arc<Mutex<RustFd>>>,
	pub stdout: Option<Arc<Mutex<RustFd>>>,
	pub stderr: Option<Arc<Mutex<RustFd>>>,
	pub backup: HashMap<RawFd,RustFd>,
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
		self.route_output()?;
		Ok(())
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
		let mut new = ProcIO::from(self.stdin.clone(),self.stdout.clone(),self.stderr.clone());
		new
	}
}

impl Default for ProcIO {
	fn default() -> Self {
		Self::new()
	}
}

fn dispatch_exec<'a>(node: Pair<'a,Rule>, ctx: &mut ExecCtx) -> LashResult<()> {
		match node.as_rule() {
			Rule::simple_cmd => {
				let command_name = node.clone().into_inner().find(|pair| pair.as_rule() == Rule::cmd_name).unpack()?.as_str();
				if BUILTINS.contains(&command_name) {
					exec_builtin(node,command_name,ctx)?;
				} else if shellenv::is_func(command_name)? {
					exec_func(node,ctx)?;
				} else {
					exec_cmd(node, ctx)?;
				}
			}
			Rule::shell_cmd => {
				let mut shell_cmd_inner = node.to_deque();
				let shell_cmd = shell_cmd_inner.pop_front().unpack()?;
				while shell_cmd_inner.front().is_some_and(|pair| pair.as_rule() == Rule::redir) {
					let redir = Redir::from_pair(shell_cmd_inner.pop_front().unpack()?)?;
					ctx.push_redir(redir);
				}
				match shell_cmd.as_rule() {
					Rule::for_cmd => exec_for_cmd(shell_cmd, ctx)?,
					Rule::match_cmd => exec_match_cmd(shell_cmd, ctx)?,
					Rule::loop_cmd => exec_loop_cmd(shell_cmd, ctx)?,
					Rule::if_cmd => exec_if_cmd(shell_cmd, ctx)?,
					Rule::subshell => exec_subshell(shell_cmd, ctx)?,
					Rule::brace_grp => todo!(),
					Rule::assignment => exec_assignment(shell_cmd, ctx)?,
					Rule::func_def => exec_func_def(shell_cmd, ctx)?,
					_ => unreachable!()
				};
			}
			Rule::pipeline => { exec_pipeline(node, ctx)?; },
			Rule::EOI => { /* Do nothing */ }
			_ => todo!("Support for rule '{:?}' is unimplemented",node.as_rule())
		}
		Ok(())
}

pub fn descend(mut node_stack: VecDeque<Pair<Rule>>, ctx: &mut ExecCtx) -> LashResult<()> {
	ctx.descend()?; // Increment depth counter
	while let Some(node) = node_stack.pop_front() {
		match node.as_rule() {
			Rule::main | Rule::cmd_list => {
				let inner = node.to_deque();
				node_stack.extend(inner);
			}
			Rule::op => {
				let option = node.step(1);
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
			Rule::bg_cmd => {
				if let Some(cmd) = node.step(1) {
					let flags = ctx.flags_mut();
					*flags |= ExecFlags::BACKGROUND;
					dispatch_exec(cmd, ctx)?
				}
			}
			_ => dispatch_exec(node, ctx)?
		}
	}
	ctx.ascend()?; // Decrement depth counter
	Ok(())
}

fn exec_func_def<'a>(func_def: Pair<'a,Rule>, ctx: &mut ExecCtx) -> LashResult<()> {
	let blame = func_def.clone();
	let func_name = func_def.scry(Rule::func_name).unpack()?;
	let body = func_def.scry(Rule::brace_grp).unpack()?;
	helper::write_func(func_name.as_str().trim_end_matches("()"), body.as_str().trim_matches(['{','}']).trim())?;
	shellenv::set_code(0)?;
	Ok(())
}

pub fn exec_subshell<'a>(subsh: Pair<'a,Rule>, ctx: &mut ExecCtx) -> LashResult<()> {
	let mut shebang = None;
	let body = subsh.scry(Rule::subsh_body).unpack()?.as_str();
	if let Some(subshebang) = subsh.scry(Rule::subshebang) {
		let raw_shebang = subshebang.as_str().to_string();
		shebang = Some(expand::expand_shebang(&raw_shebang));
	}

	let argv = helper::prepare_argv(subsh.clone());
	let redirs = helper::prepare_redirs(subsh)?;

	ctx.set_redirs(redirs);
	if let Some(shebang) = shebang {
		let script = format!("{}{}",shebang,body);
		handle_external_subshell(script,argv,ctx)?;
	} else {
		handle_internal_subshell(body.to_string(),argv,ctx)?;
	}

	shellenv::set_code(0)?;
	Ok(())
}

fn handle_external_subshell(script: String, argv: VecDeque<String>, ctx: &mut ExecCtx) -> LashResult<()> {
	let argv = argv.into_iter().map(|arg| CString::new(arg).unwrap()).collect::<Vec<_>>();
	let envp = shellenv::get_cstring_evars()?;
	let memfd = RustFd::new_memfd("anonymous_subshell", true)?;
	memfd.write(script.as_bytes())?;
	let fd_path = CString::new(format!("/proc/self/fd/{memfd}")).unwrap();
	let io = ctx.io_mut();

	io.route_io()?;

	if shellenv::in_pipe()? {
		execve(&fd_path, &argv, &envp).unwrap();
		panic!("execve() failed in subshell execution");
	}

	match unsafe { fork() } {
		Ok(ForkResult::Child) => {
			execve(&fd_path, &argv, &envp).unwrap();
			panic!("execve() failed in subshell execution");
		}
		Ok(ForkResult::Parent { child }) => {
			let children = vec![
				ChildProc::new(child, Some("anonymous_subshell"),None)?
			];
			let job = JobBuilder::new()
				.with_pgid(child)
				.with_children(children)
				.build();
			handle_fg(job)?;
		}
		Err(e) => panic!("Encountered fork error: {}",e)
	}
	io.restore_fildescs()?;
	Ok(())
}

fn handle_internal_subshell(body: String, argv: VecDeque<String>, ctx: &mut ExecCtx) -> LashResult<()> {
	let snapshot = SavedEnv::get_snapshot()?;
	write_vars(|v| {
		v.reset_params();
		for arg in argv {
			v.pos_param_pushback(&arg);
		}
	})?;
	exec_input(body.consume_escapes(), ctx)?;
	snapshot.restore_snapshot()?;
	Ok(())
}

fn exec_pipeline<'a>(pipeline: Pair<'a,Rule>, ctx: &mut ExecCtx) -> LashResult<()> {
	let blame = pipeline.clone();
	let mut inner = pipeline.into_inner().peekable();
	let mut prev_read_pipe: Option<RustFd> = None;
	let mut pgid: Option<Pid> = None;
	let mut cmds: Vec<String> = vec![];
	let mut pids: Vec<Pid> = vec![];

	let mut first = true;
	while let Some(node) = inner.next() {
		let (r_pipe,mut w_pipe) = if inner.peek().is_some() {
			let (r_pipe,w_pipe) = proc_res(RustFd::pipe(),blame.clone())?;
			(Some(r_pipe),Some(w_pipe))
		} else {
			(None,None)
		};

		let cmd_check = node.clone();
		match cmd_check.as_rule() {
			Rule::simple_cmd => {
				let mut argv = cmd_check.into_inner();
				let cmd = argv.next().unwrap();
				cmds.push(cmd.as_str().to_string())
			}
			Rule::shell_cmd => {
				let shell_cmd = cmd_check.step(1).unpack()?;
				match shell_cmd.as_rule() {
					Rule::for_cmd => cmds.push("for".into()),
					Rule::if_cmd => cmds.push("if".into()),
					Rule::match_cmd => cmds.push("match".into()),
					Rule::loop_cmd => {
						let mut inner = shell_cmd.into_inner();
						let loop_kind = inner.next().unpack()?.as_str();
						cmds.push(loop_kind.into());
					}
					Rule::subshell => cmds.push("anonymous subshell".into()),
					_ => todo!()
				}
			}
			_ => unreachable!()
		}

		let io = ctx.io_mut();
		let pipe_io = ProcIO::from(
			prev_read_pipe.take().map(|pipe| pipe.mk_shared()),
			w_pipe.take().map(|pipe| pipe.mk_shared()),
			io.stderr.clone(), // TODO: implement stderr piping
		);

		match unsafe { fork() } {
			Ok(ForkResult::Child) => {
				if first {
					io.route_input().unwrap();
				} else if inner.peek().is_none() {
					io.route_output().unwrap();
				}
				if let Some(mut pipe) = prev_read_pipe {
					pipe.close().unwrap()
				}
				if let Some(mut pipe) = w_pipe {
					pipe.close().unwrap()
				}

				write_meta(|m| m.mod_flags(|f| *f |= EnvFlags::IN_SUB_PROC)).unwrap();
				*io = pipe_io;
				dispatch_exec(node, ctx)?;
				std::process::exit(1)
			}
			Ok(ForkResult::Parent { child }) => {
				pids.push(child);
				if let Some(ref mut read_pipe) = prev_read_pipe {
					read_pipe.close()?;
				}
				if let Some(mut write_pipe) = w_pipe {
					write_pipe.close()?;
				}
				prev_read_pipe = r_pipe;
				if pgid.is_none() {
					pgid = Some(child);
				}
				if inner.peek().is_none() {
					let mut children = vec![];
					let mut commands = cmds.iter();
					for pid in &pids {
						let cmd = commands.next().map(|cmd| cmd.as_str());
						let child = ChildProc::new(*pid,cmd,pgid)?;
						children.push(child);
					}
					let job = JobBuilder::new()
						.with_pgid(pgid.unwrap())
						.with_children(children)
						.build();


					handle_fg(job)?;
				}
			}
			Err(e) => return Err(High(LashErrHigh::exec_err("Command in pipeline failed", blame)))
		}
		if first {
			first = false;
		}
	}
	shellenv::set_code(0)?;
	Ok(())
}

fn exec_assignment<'a>(ass: Pair<'a,Rule>, ctx: &mut ExecCtx) -> LashResult<()> {
	let var_name = ass.scry(Rule::var_ident).unpack()?.as_str().trim_matches(['{','}']);
	let var_val = LashVal::parse(ass.scry(Rule::word).unpack()?.as_str()).unwrap();

	if let Some(cmd) = ass.scry(Rule::cmd_list) {
		// If there are commands attached, export the variables, then execute, then restore environment state
		let saved_vars = read_vars(|v| v.clone())?;
		write_vars(|v| v.export_var(var_name, &var_val.to_string()))?;
		exec_list(Rule::cmd_list, cmd.as_str().to_string(), ctx)?;
		write_vars(|v| *v = saved_vars)?;
	} else {
		// If there are no commands attached, just set the variable
		write_vars(|v| v.set_var(var_name, var_val))?;
	}
	shellenv::set_code(0)?;
	Ok(())
}

fn exec_for_cmd<'a>(cmd: Pair<'a,Rule>,ctx: &mut ExecCtx) -> LashResult<()> {
	let io = ctx.io_mut();
	*io = ProcIO::from(None, io.stdout.clone(), io.stderr.clone());
	let mut saved_vars = HashMap::new();
	let loop_body_pair = cmd.scry(Rule::loop_body).unpack()?;
	let loop_vars = cmd.scry(Rule::for_vars)
		.unpack()?
		.into_inner()
		.into_iter()
		.map(|var| var.as_str())
		.collect::<Vec<&str>>();
	let loop_arr = cmd.scry(Rule::for_arr)
		.unpack()?
		.into_inner()
		.map(|elem| LashVal::parse(elem.as_str()).unwrap())
		.collect::<Vec<LashVal>>();

	let vars_len = loop_vars.len();
	for var in &loop_vars {
		let existing_val = read_vars(|v| v.get_var(var))?.unwrap_or_default();
		saved_vars.insert(var,existing_val);
	}

	for (i,element) in loop_arr.iter().enumerate() {
		let var_index = i % vars_len;
		write_vars(|v| v.set_var(loop_vars[var_index], element.clone()))?;
		let loop_body = expand::expand_list(loop_body_pair.clone())?;
		exec_list(Rule::cmd_list, loop_body, ctx)?;
	}
	for var in &loop_vars {
		let saved_val = saved_vars.remove(var).unwrap_or_default();
		write_vars(|v| v.set_var(var, saved_val))?;
	}
	shellenv::set_code(0)?;
	Ok(())
}

fn exec_match_cmd<'a>(cmd: Pair<'a,Rule>, ctx: &mut ExecCtx) -> LashResult<()> {
	let mut inner = cmd.into_inner();
	let match_pat = inner.next().unpack()?;
	let mut arms = VecDeque::new();

	while let Some(arm) = inner.next() {
		arms.push_back(arm);
	}

	while let Some(arm) = arms.pop_front() {
		let mut inner = arm.into_inner();
		let arm_pat = inner.next().unpack()?;
		let arm_body_pair = inner.next().unpack()?;

		if arm_pat.as_str().trim() == match_pat.as_str().trim() {
			let arm_body = expand::expand_list(arm_body_pair)?;
			exec_list(Rule::cmd_list, arm_body.trim_end_matches(',').to_string(), ctx)?;
			break
		}
	}
	shellenv::set_code(0)?;
	Ok(())
}

fn exec_if_cmd<'a>(cmd: Pair<'a,Rule>, ctx: &mut ExecCtx) -> LashResult<()> {
	let if_cond_pair = cmd.scry(Rule::if_cond).unpack()?;
	let if_body_pair = cmd.scry(Rule::if_body).unpack()?;
	let else_block = cmd.scry(Rule::else_block);
	let mut elif_blocks = cmd.into_inner().filter(|pr| pr.as_rule() == Rule::elif_block).collect::<VecDeque<_>>();

	let mut cond_ctx = ctx.as_cond();
	let mut body_ctx = ctx.as_body();
	let cond_io = cond_ctx.io_mut();
	*cond_io = ProcIO::from(cond_io.stdin.clone(), None, None);
	let body_io = body_ctx.io_mut();
	*body_io = ProcIO::from(None, body_io.stdout.clone(), body_io.stderr.clone());

	let in_pipe = shellenv::in_pipe()?;
	if in_pipe {
		// We are going to temporarily remove this flag here, to make sure that cond/body executions fork the process
		// If we don't do this, the program will exit after executing the first condition
		write_meta(|m| m.mod_flags(|f| *f &= !EnvFlags::IN_SUB_PROC))?;
	}

	let if_cond = expand::expand_list(if_cond_pair)?;
	exec_list(Rule::cmd_list, if_cond, &mut cond_ctx)?;
	if &shellenv::check_status()? == "0" {
		let if_body = expand::expand_list(if_body_pair)?;
		exec_list(Rule::cmd_list, if_body, &mut body_ctx)?;
		return Ok(())
	}

	while let Some(elif_block) = elif_blocks.pop_front() {
		let mut inner = elif_block.into_inner();
		let elif_cond_pair = inner.next().unpack()?;
		let elif_body_pair = inner.next().unpack()?;

		let elif_cond = expand::expand_list(elif_cond_pair)?;
		exec_list(Rule::cmd_list, elif_cond, &mut cond_ctx)?;
		if &shellenv::check_status()? == "0" {
			let elif_body = expand::expand_list(elif_body_pair)?;
			exec_list(Rule::cmd_list, elif_body, &mut body_ctx)?;
			return Ok(())
		}
	}

	if let Some(else_block) = else_block {
		let else_body_pair = else_block.step(1).unpack()?;
		let else_body = expand::expand_list(else_body_pair)?;
		exec_list(Rule::cmd_list, else_body, &mut body_ctx)?;
	}

	if in_pipe {
		// We are going to temporarily remove this flag here, to make sure that cond/body executions fork the process
		// If we don't do this, the program will exit after executing the first condition
		write_meta(|m| m.mod_flags(|f| *f |= EnvFlags::IN_SUB_PROC))?;
	}
	shellenv::set_code(0)?;
	Ok(())
}

fn exec_loop_cmd<'a>(cmd: Pair<'a,Rule>, ctx: &mut ExecCtx) -> LashResult<()> {
	let loop_kind = cmd.scry(Rule::loop_kind).unpack()?;
	let loop_cond_pair = cmd.scry(Rule::loop_cond).unpack()?;
	let loop_body_pair = cmd.scry(Rule::loop_body).unpack()?;
	let mut cond_ctx = ctx.as_cond();
	let mut body_ctx = ctx.as_body();
	let cond_io = cond_ctx.io_mut();
	*cond_io = ProcIO::from(cond_io.stdin.clone(), None, None);
	let body_io = body_ctx.io_mut();
	*body_io = ProcIO::from(None, body_io.stdout.clone(), body_io.stderr.clone());


	loop {
		let loop_cond = expand::expand_list(loop_cond_pair.clone())?;
		exec_list(Rule::cmd_list, loop_cond, &mut cond_ctx)?;
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
		let loop_body = expand::expand_list(loop_body_pair.clone())?;
		let result = exec_list(Rule::cmd_list, loop_body, &mut body_ctx);
		match result {
			Err(LashErr::High(err)) => {
				match err.get_err() {
					LashErrLow::LoopBreak(code) => {
						write_vars(|v| v.set_param("?".into(), code.to_string()))?;
						break
					}
					LashErrLow::LoopCont => continue,
					_ => return Err(LashErr::High(err))
				}
			}
			Err(e) => return Err(e),
			Ok(_) => continue,
		}
	}
	shellenv::set_code(0)?;
	Ok(())
}

fn exec_builtin(cmd: Pair<Rule>, name: &str, ctx: &mut ExecCtx) -> LashResult<()> {
	let blame = cmd.clone();
	match name {
		"test" | "[" => {
			let mut argv = cmd.to_deque();
			argv.pop_front(); // Ignore the command name
			let result = proc_res(builtin::test(&mut argv, ctx), blame)?;
			if result {
				write_vars(|v| v.set_param("?".into(), "0".into()))?;
			} else {
				write_vars(|v| v.set_param("?".into(), "1".into()))?;
			}
		}
		"string" | "float" | "int" | "arr" | "bool" => builtin::assign_builtin(cmd, ctx)?,
		"setopt" => builtin::setopt(cmd, ctx)?,
		"cd" => builtin::cd(cmd, ctx)?,
		"alias" => builtin::alias(cmd, ctx)?,
		"pwd" => builtin::pwd(cmd, ctx)?,
		"export" => builtin::export(cmd, ctx)?,
		"echo" => builtin::echo(cmd, ctx)?,
		_ => unimplemented!("Have not implemented support for builtin `{}` yet",name)
	};
	shellenv::set_code(0)?;
	Ok(())
}

fn exec_func(cmd: Pair<Rule>,ctx: &mut ExecCtx) -> LashResult<()> {
	let blame = cmd.clone();
	let mut argv = helper::prepare_argv(cmd);
	let func_name = argv.pop_front().unwrap();
	let body = read_logic(|l| l.get_func(&func_name).unwrap())?;
	let mut var_table = read_vars(|v| v.clone())?;
	let snapshot = SavedEnv::get_snapshot()?;

	var_table.reset_params();
	for arg in argv {
		var_table.pos_param_pushback(&arg);
	}
	write_vars(|v| *v = var_table)?;
	let result = exec_input(body, ctx);
	snapshot.restore_snapshot()?;

	// Let's check to see if the function was cut by the return command
	match &result {
		Ok(()) => {
			shellenv::set_code(0)?;
			Ok(())
		}
		Err(e) => {
			match e {
				High(high) => Err(High(high.clone())),
				Low(low) => {
					match low {
						FuncReturn(code) => {
							shellenv::set_code(*code as isize)?;
							Ok(())
						}
						_ => {
							proc_res(result,blame)
						}
					}
				}
			}
		}
	}
}

fn exec_cmd<'a>(cmd: Pair<Rule>, ctx: &mut ExecCtx) -> LashResult<()> {
	let blame = cmd.clone();
	let mut argv = helper::prepare_argv(cmd.clone());
	let mut redirs = helper::prepare_redirs(cmd)?;
	redirs.extend(ctx.redirs());
	argv.retain(|arg| !arg.is_empty() && arg != "\"\"" && arg != "''");

	if helper::validate_autocd(&argv)? {
		let arg = argv.pop_front().unwrap();
		let dir = PathBuf::from(&arg);
		return autocd(dir)
	}

	let argv = argv.into_iter().map(|arg| CString::new(arg).unwrap()).collect::<Vec<_>>();


	let command = argv.first().unwrap().clone();

	let io = ctx.io_mut();
	io.route_io()?;


	if SHELL_CMDS.contains(&command.to_str().unwrap()) {
		return Err(High(LashErrHigh::exec_err(format!("This shell command appears malformed"), blame)))
	}

	let env_vars = env::vars().into_iter().collect::<Vec<(String,String)>>();
	let envp = env_vars.iter().map(|var| CString::new(format!("{}={}",var.0,var.1)).unwrap()).collect::<Vec<_>>();

	let mut redirs = CmdRedirs::new(Vec::from(redirs));
	proc_res(redirs.activate(),blame.clone())?;

	if ctx.flags().contains(ExecFlags::NO_FORK) {
		exec_external(command, argv, envp, blame);
	}

	match unsafe { fork() } {
		Ok(ForkResult::Child) => {
			exec_external(command, argv, envp, blame);
		}
		Ok(ForkResult::Parent { child }) => {
			handle_parent_process(child, command.to_str().unwrap().to_string())?;
			redirs.close_all()?;
		}
		Err(_) => todo!()
	}

	Ok(())
}

#[derive(Debug)]
pub struct CmdRedirs {
	open_fds: Vec<RustFd>,
	targets_fd: Vec<Redir>,
	targets_file: Vec<Redir>
}

impl CmdRedirs {
	pub fn new(mut redirs: Vec<Redir>) -> Self {
		let mut targets_fd = vec![];
		let mut targets_file = vec![];
		while let Some(redir) = redirs.pop() {
			let Redir { redir_type: _, source: _, fd_target, file_target: _ } = &redir;
			if fd_target.is_some() {
				targets_fd.push(redir);
			} else {
				targets_file.push(redir);
			}
		}
		Self { open_fds: vec![], targets_fd, targets_file }
	}
	pub fn activate(&mut self) -> LashResult<()> {
		self.open_file_targets()?;
		self.open_fd_targets()?;
		Ok(())
	}
	pub fn close_all(mut self) -> LashResult<()> {
		for fd in self.open_fds.iter_mut() {
			fd.close()?
		}
		Ok(())
	}
	pub fn open_file_targets(&mut self) -> LashResult<()> {
		for redir in &self.targets_file {
			let Redir { redir_type, source, fd_target: _, file_target } = redir;
			let src_fd = RustFd::new(*source)?;
			let path = file_target.as_ref().unwrap(); // We know that there's a file target so unwrap is safe
			let flags = match redir_type {
				Rule::r#in => OFlag::O_RDONLY,
				Rule::out => OFlag::O_WRONLY | OFlag::O_CREAT | OFlag::O_TRUNC,
				Rule::append => OFlag::O_WRONLY | OFlag::O_CREAT | OFlag::O_APPEND,
				_ => unreachable!(),
			};
			let mut file_fd = RustFd::open(path, flags, Mode::from_bits(0o644).unwrap())?;
			file_fd.dup2(&src_fd)?;
			file_fd.close()?;
			self.open_fds.push(src_fd);
		}
		Ok(())
	}
	pub fn open_fd_targets(&mut self) -> LashResult<()> {
		for redir in &self.targets_fd {
			let Redir { redir_type: _, source, fd_target, file_target: _ } = redir;
			let mut tgt_fd = RustFd::new(fd_target.unwrap())?;
			let src_fd = RustFd::new(*source)?;
			tgt_fd.dup2(&src_fd)?;
			tgt_fd.close()?;
			self.open_fds.push(src_fd);
		}
		Ok(())
	}
}

fn autocd(dir: PathBuf) -> LashResult<()> {
	let cwd = env::var("PWD").map_err(|_| Low(LashErrLow::from_io()))?;
	write_vars(|v| v.export_var("OLDPWD", &cwd))?;
	env::set_current_dir(&dir).map_err(|_| Low(LashErrLow::from_io()))?;
	let cwd = env::current_dir().map_err(|_| Low(LashErrLow::from_io()))?;
	write_vars(|v| v.export_var("PWD", cwd.to_str().unwrap()))?;
	Ok(())
}

fn exec_external(command: CString, argv: Vec<CString>, envp: Vec<CString>,blame: Pair<Rule>) -> ! {
	let Err(e) = execvpe(&command, &argv, &envp);
	match e {
		Errno::ENOENT => {
			let error = High(LashErrHigh::cmd_not_found(command.to_str().unwrap(), blame));
			eprintln!("{}",error);
		}
		Errno::EACCES => {
			let error = High(LashErrHigh::no_permission(command.to_str().unwrap(), blame));
			eprintln!("{}",error);
		}
		_ => unimplemented!("Case for `{}` not implemented", e.to_string())
	}
	std::process::exit(e as i32)
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
	Ok(())
}
