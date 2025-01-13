use std::{collections::{HashMap, VecDeque}, ffi::CString, fmt::{self, Display}, os::fd::{AsFd, AsRawFd, FromRawFd, IntoRawFd, RawFd}, panic::Location, path::Path, sync::{mpsc::Receiver, Arc}, thread};

use std::time::Duration;
use libc::{memfd_create, MFD_CLOEXEC};
use nix::{fcntl::{open, OFlag}, sys::{signal::Signal, stat::Mode, wait::{waitpid, WaitStatus}}, unistd::{close, dup, dup2, execve, execvpe, fork, getpid, pipe, setpgid, tcgetpgrp, tcsetpgrp, ForkResult, Pid}, NixPath};
use std::sync::Mutex;

use crate::{builtin, event::{self, ShError, ShEvent}, interp::{expand, helper::{self, StrExtension, VecDequeExtension}, parse::{self, NdFlags, NdType, Node, Span}, token::{Redir, RedirType, Tk, TkType, WdFlags}}, shellenv::{self, read_jobs, read_logic, read_meta, read_vars, term_controller, write_jobs, write_logic, write_meta, write_vars, EnvFlags, Job, RVal, SavedEnv, RSH_PGRP}, RshResult, GLOBAL_EVENT_CHANNEL};

macro_rules! node_operation {
	($node_type:path { $($field:tt)* }, $node:expr, $node_op:block) => {
		if let $node_type { $($field)* } = $node.nd_type.clone() {
			$node_op
		} else { unreachable!() }
	};
}

bitflags::bitflags! {
	#[derive(Clone,Debug,Copy)]
	pub struct ExecFlags: u8 {
		const IN_PIPE    = 0b00000001;
		const BACKGROUND = 0b00000010;
	}
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct RustFd {
	fd: RawFd,
}

impl RustFd {
	pub fn new(fd: RawFd) -> RshResult<Self> {
		if fd < 0 {
			return Err(ShError::from_internal("Attempted to create a new RustFd from a negative FD"));
		}
		Ok(RustFd { fd })
	}

	/// Create a `RustFd` from a duplicate of `stdin` (FD 0)
	pub fn from_stdin() -> RshResult<Self> {
		let fd = dup(0).map_err(|_| ShError::from_io())?;
		Ok(Self { fd })
	}

	/// Create a `RustFd` from a duplicate of `stdout` (FD 1)
	pub fn from_stdout() -> RshResult<Self> {
		let fd = dup(1).map_err(|_| ShError::from_io())?;
		Ok(Self { fd })
	}

	/// Create a `RustFd` from a duplicate of `stderr` (FD 2)
	pub fn from_stderr() -> RshResult<Self> {
		let fd = dup(2).map_err(|_| ShError::from_io())?;
		Ok(Self { fd })
	}

	/// Create a `RustFd` from a type that provides an owned or borrowed FD
	pub fn from_fd<T: AsFd>(fd: T) -> RshResult<Self> {
		let raw_fd = fd.as_fd().as_raw_fd();
		if raw_fd < 0 {
			return Err(ShError::from_internal("Attempted to convert to RustFd from a negative FD"));
		}
		Ok(RustFd { fd: raw_fd })
	}

	/// Create a `RustFd` by consuming ownership of an FD
	pub fn from_owned_fd<T: IntoRawFd>(fd: T) -> RshResult<Self> {
		let raw_fd = fd.into_raw_fd(); // Consumes ownership
		if raw_fd < 0 {
			return Err(ShError::from_internal("Attempted to convert to RustFd from a negative FD"));
		}
		Ok(RustFd { fd: raw_fd })
	}

	/// Create a new `RustFd` that points to an in-memory file descriptor. In-memory file descriptors can be interacted with as though they were normal files.
	pub fn new_memfd(name: &str, executable: bool) -> RshResult<Self> {
		let c_name = CString::new(name).map_err(|_| ShError::from_internal("Invalid name for memfd"))?;
		let flags = if executable {
			0
		} else {
			MFD_CLOEXEC
		};
		let fd = unsafe { memfd_create(c_name.as_ptr(), flags) };
		Ok(RustFd { fd })
	}

	/// Write some bytes to the contained file descriptor
	pub fn write(&self, buffer: &[u8]) -> RshResult<()> {
		if !self.is_valid() {
			return Err(ShError::from_internal("Attempted to write to an invalid RustFd"));
		}
		let result = unsafe { libc::write(self.fd, buffer.as_ptr() as *const libc::c_void, buffer.len()) };
		if result < 0 {
			Err(ShError::from_io())
		} else {
			Ok(())
		}
	}

	pub fn read(&self) -> RshResult<String> {
		if !self.is_valid() {
			return Err(ShError::from_internal("Attempted to read from an invalid RustFd"));
		}
		let mut buffer = [0; 1024];
		let bytes_read = nix::unistd::read(self.as_raw_fd(), &mut buffer).map_err(|_| ShError::from_io())?;
		let output = String::from_utf8_lossy(&buffer[..bytes_read]);
		Ok(output.to_string())
	}

	/// Wrapper for nix::unistd::pipe(), simply produces two `RustFds` that point to a read and write pipe respectfully
	pub fn pipe() -> RshResult<(Self,Self)> {
		let (r_pipe,w_pipe) = pipe().map_err(|_| ShError::from_io())?;
		let r_fd = RustFd::from_owned_fd(r_pipe)?;
		let w_fd = RustFd::from_owned_fd(w_pipe)?;
		Ok((r_fd,w_fd))
	}

	/// Produce a `RustFd` that points to the same resource as the 'self' `RustFd`
	pub fn dup(&self) -> RshResult<Self> {
		if !self.is_valid() {
			return Err(ShError::from_internal("Attempted to dup an invalid fd"));
		}
		let new_fd = dup(self.fd).map_err(|_| ShError::from_io())?;
		Ok(RustFd { fd: new_fd })
	}

	/// A wrapper for nix::unistd::dup2(), 'self' is duplicated to the given target file descriptor.
	#[track_caller]
	pub fn dup2<T: AsRawFd>(&self, target: &T) -> RshResult<()> {
		let target_fd = target.as_raw_fd();
		if self.fd == target_fd {
			// Nothing to do here
			return Ok(())
		}
		if !self.is_valid() || target_fd < 0 {
			return Err(ShError::from_io());
		}

		dup2(self.fd, target_fd).map_err(|_| ShError::from_io())?;
		Ok(())
	}

	/// Open a file using a file descriptor, with the given OFlags and Mode bits
	pub fn open(path: &Path, flags: OFlag, mode: Mode) -> RshResult<Self> {
		let file_fd: RawFd = open(path, flags, mode).map_err(|_| ShError::from_io())?;
		Ok(Self { fd: file_fd })
	}

	#[track_caller]
	pub fn close(&mut self) -> RshResult<()> {
		if !self.is_valid() {
			return Ok(())
		}
		close(self.fd).map_err(|_| {
			ShError::from_io()
		})?;
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
pub enum RshWait {
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
	SIGRETURN, // Return from a function
	SIGCONT, // Restart a loop from the beginning
	SIGBREAK, // Break a loop
	SIGRSHEXIT // Internal call to exit early
}

impl RshWait {
	pub fn new() -> Self {
		RshWait::Success
	}
	pub fn raw(&self) -> i32 {
		match *self {
			RshWait::Success => 0,
			RshWait::Fail { code, cmd: _ } => code,
			_ => unimplemented!("unimplemented signal type: {:?}", self)
		}
	}
	pub fn from_wait(wait: WaitStatus, cmd: Option<String>) -> Self {
		match wait {
			WaitStatus::Exited(_, code) => {
				match code {
					0 => RshWait::Success,
					_ => RshWait::Fail { code, cmd }
				}
			}
			WaitStatus::Signaled(_, sig, _) => RshWait::Signaled { sig },
			WaitStatus::Stopped(_, sig) => RshWait::Stopped { sig },
			WaitStatus::PtraceEvent(_, signal, _) => todo!(),
			WaitStatus::PtraceSyscall(_) => todo!(),
			WaitStatus::Continued(_) => RshWait::Continued,
			WaitStatus::StillAlive => RshWait::Running
		}
	}
}

impl Display for RshWait {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			RshWait::Success { .. } => write!(f, "done"),
			RshWait::Fail { code, .. } => write!(f, "exit {}", code),
			RshWait::Signaled { sig } => write!(f, "exit {}", sig),
			RshWait::Stopped { .. } => write!(f, "stopped"),
			RshWait::Terminated { signal } => write!(f, "terminated {}", signal),
			RshWait::Continued => write!(f, "continued"),
			RshWait::Running => write!(f, "running"),
			RshWait::Killed { signal } => write!(f, "killed {}", signal),
			RshWait::TimeOut => write!(f, "time out"),
			_ => write!(f, "{:?}",self)
		}
	}
}


impl Default for RshWait {
	fn default() -> Self {
		RshWait::new()
	}
}

#[derive(Debug)]
pub struct ProcIO {
	pub stdin: Option<Arc<Mutex<RustFd>>>,
	pub stdout: Option<Arc<Mutex<RustFd>>>,
	pub stderr: Option<Arc<Mutex<RustFd>>>,
	pub backup: HashMap<RawFd,RustFd>
}

impl ProcIO {
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
	pub fn close_all(&mut self) -> RshResult<()> {
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
	pub fn backup_fildescs(&mut self) -> RshResult<()> {
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
	pub fn restore_fildescs(&mut self) -> RshResult<()> {
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
	pub fn route_input(&mut self) -> RshResult<()> {
		if let Some(ref mut stdin_fd) = self.stdin {
			let mut fd = stdin_fd.lock().unwrap();
			fd.dup2(&0)?;
			fd.close()?;
		}
		Ok(())
	}
	pub fn route_output(&mut self) -> RshResult<()> {
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
	pub fn route_io(&mut self) -> RshResult<()> {
		self.route_input()?;
		self.route_output()
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

pub fn traverse_ast(ast: Node) -> RshResult<RshWait> {
	let saved_in = RustFd::from_stdin()?;
	let saved_out = RustFd::from_stdout()?;
	let saved_err = RustFd::from_stderr()?;
	let status = traverse(ast, ProcIO::new())?;
	saved_in.dup2(&0)?;
	saved_out.dup2(&1)?;
	saved_err.dup2(&2)?;
	Ok(status)
}


fn traverse(mut node: Node, io: ProcIO) -> RshResult<RshWait> {
	let last_status;

	// We delay expanding variables in for loop bodies until now; this is done to give the variables time to be properly assigned before execution of the for loop body
	if node.is_executable() && node.flags.contains(NdFlags::FOR_BODY) {
		let new_argv = expand::expand_arguments(&mut node)?;
		node.set_argv(new_argv)?;
	}
	match node.nd_type {
		NdType::Command {..} => {
			last_status = handle_command(node, io)?;
		}
		NdType::Builtin {..} => {
			last_status = handle_builtin(node, io)?;
		}
		NdType::Function {..} => {
			last_status = handle_function(node, io)?;
		}
		NdType::Pipeline {..} => {
			last_status = handle_pipeline(node, io)?;
		}
		NdType::Chain {..} => {
			last_status = handle_chain(node)?;
		}
		NdType::If {..} => {
			last_status = handle_if(node,io)?;
		}
		NdType::For {..} => {
			last_status = handle_for(node,io)?;
		}
		NdType::Loop {..} => {
			last_status = handle_loop(node,io)?;
		}
		NdType::Case {..} => {
			last_status = handle_case(node,io)?;
		}
		NdType::Select {..} => {
			todo!("handle select")
		}
		NdType::Subshell {..} => {
			last_status = handle_subshell(node,io)?;
		}
		NdType::FuncDef {..} => {
			last_status = handle_func_def(node)?;
		}
		NdType::Assignment {..} => {
			last_status = handle_assignment(node)?;
		}
		NdType::Cmdsep => {
			last_status = RshWait::new();
		}
		NdType::Root {..} => {
			last_status = traverse_root(node, None, io)?;
		}
		_ => unimplemented!("Support for node type `{:?}` is not yet implemented",node.nd_type)
	}
	Ok(last_status)
}

fn traverse_root(mut root_node: Node, break_condition: Option<bool>, io: ProcIO) -> RshResult<RshWait> {
	let mut last_status = RshWait::new();
	if !root_node.redirs.is_empty() {
		root_node = parse::propagate_redirections(root_node)?;
	}
	if let NdType::Root { mut deck } = root_node.nd_type {
		for (index,node) in deck.iter_mut().enumerate() {
			// Map stdin to the first node, and map stdout/stderr to the last node
			let node_io = if index == 0 {
				io.clone() // Take all three FDs for the first node
			} else {
				ProcIO::from(None, io.stdout.clone(), io.stderr.clone())
			};

			if root_node.flags.contains(NdFlags::FOR_BODY) {
				node.flags |= NdFlags::FOR_BODY
			}
			last_status = traverse(node.clone(), node_io)?;
			if let Some(condition) = break_condition {
				match condition {
					true => {
						if let RshWait::Fail {..} = last_status {
							break
						}
					}
					false => {
						if let RshWait::Success  = last_status {
							break
						}
					}
				}
			}
		}
	}
	Ok(last_status)
}

fn handle_func_def(node: Node) -> RshResult<RshWait> {
	let last_status = RshWait::new();
	node_operation!(NdType::FuncDef { name, body }, node, {
		write_logic(|l| l.new_func(&name, *body.clone()))?;
	});
	Ok(last_status)
}

fn handle_case(node: Node, io: ProcIO) -> RshResult<RshWait> {
	node_operation!(NdType::Case { input_var, cases }, node, {
		for case in cases {
			let (pat, body) = case;
			if pat == input_var.text() {
				return traverse_root(body.clone(), None, io)
			}
		}
		Ok(RshWait::Fail { code: 1, cmd: Some("case".into()) })
	})
}

fn handle_for(node: Node,io: ProcIO) -> RshResult<RshWait> {
	let mut last_status = RshWait::new();

	match unsafe { fork() } {
		Ok(ForkResult::Child) => {
			let body_io = ProcIO::from(None, io.stdout, io.stderr);
			let redirs = node.get_redirs()?;
			handle_redirs(redirs.into())?;

			node_operation!(NdType::For { loop_vars, mut loop_arr, mut loop_body}, node, {
				loop_body.flags |= NdFlags::FOR_BODY;
				let var_count = loop_vars.len();
				let mut var_index = 0;
				let mut iteration_count = 0;

				let mut arr_buffer = VecDeque::new();
				while let Some(token) = loop_arr.pop_front() {
					let mut expanded = expand::expand_token(token, true)?;
					while let Some(exp_token) = expanded.pop_front() {
						arr_buffer.push_back(exp_token);
					}
				}
				loop_arr.extend(arr_buffer.drain(..));

				while !loop_arr.is_empty() {
					let current_val = loop_arr.pop_front().unwrap().text().to_string();

					let current_var = loop_vars[var_index].text().to_string();
					write_vars(|v| v.set_var(&current_var, RVal::parse(&current_val).unwrap_or_default()))?;

					iteration_count += 1;
					// TODO: modulo is expensive; find a better way to do this
					var_index = iteration_count % var_count;

					last_status = traverse_root(*loop_body.clone(), None, body_io.clone())?;
				}
			});
			std::process::exit(0);
		}
		Ok(ForkResult::Parent { child }) => {
			setpgid(child, child).map_err(|_| ShError::from_internal("Failed to set child PGID"))?;
			if node.flags.contains(NdFlags::BACKGROUND) {
				let job_id = write_jobs(|j| j.new_job(vec![child], vec!["for".into()], child))?;
			} else {
				if !node.flags.contains(NdFlags::FUNCTION) {
					shellenv::attach_tty(child)?;
				}
				let job = Job::new(0, vec![child], vec!["for".into()], child);
				write_jobs(|j| j.new_fg(job))?;
			}
		}
		Err(_) => return Err(ShError::ExecFailed("Fork failed in handle_for".into(), 1, node.span())),
	}

	Ok(last_status)
}

fn handle_loop(node: Node, io: ProcIO) -> RshResult<RshWait> {
	let mut last_status = RshWait::new();
	let cond_io = ProcIO::from(io.stdin, None, None);
	let body_io = ProcIO::from(None, io.stdout, io.stderr);

	node_operation!(NdType::Loop { condition, logic }, node, {
		// Idea: try turning cond and body into Mutexes or RwLocks to avoid excessive cloning in the loop
		// ProcIO already uses Arc<Mutex> so cloning should be pretty cheap
		let cond = *logic.condition;
		let body = *logic.body;
		loop {
			let condition_status = traverse_root(cond.clone(), Some(condition),cond_io.clone())?;

			match condition {
				true => {
					if !matches!(condition_status,RshWait::Success) {
						break
					}
				}
				false => {
					if matches!(condition_status,RshWait::Success) {
						break
					}
				}
			}

			last_status = traverse_root(body.clone(), None, body_io.clone())?;
		}
	});

	Ok(last_status)
}


fn handle_if(node: Node, io: ProcIO) -> RshResult<RshWait> {
	let mut last_status = RshWait::new();
	let cond_io = ProcIO::from(io.stdin,None,None);
	let body_io = ProcIO::from(None,io.stdout,io.stderr);

	node_operation!(NdType::If { mut cond_blocks, else_block }, node, {
		while let Some(block) = cond_blocks.pop_front() {
			let cond = *block.condition;
			let body = *block.body;

			traverse(cond, cond_io.clone())?;
			last_status = shellenv::await_fg_job()?;

			if let RshWait::Success = last_status {
				traverse(body, body_io.clone())?;
				last_status = shellenv::await_fg_job()?;
				return Ok(last_status)
			}
		}
		if let Some(block) = else_block {
			return traverse_root(*block, None, body_io)
		}
	});
	Ok(last_status)
}

fn handle_chain(node: Node) -> RshResult<RshWait> {
	let last_status;

	node_operation!(NdType::Chain { left, right, op }, node, {
		traverse(*left, ProcIO::new())?;
		last_status = shellenv::await_fg_job()?;
		if last_status == RshWait::Success {
			if let NdType::And = op.nd_type {
				traverse(*right,ProcIO::new())?;
			}
		} else if let NdType::Or = op.nd_type {
			traverse(*right,ProcIO::new())?;
		}
	});
	Ok(last_status.clone())
}

fn handle_assignment(node: Node) -> RshResult<RshWait> {
	node_operation!(NdType::Assignment { name, value }, node, {
		let mut value = value.unwrap_or_default();
		if let Some(body) = value.trim_command_sub() {
			let dummy_tk = Tk {
				tk_type: TkType::CommandSub,
				wd: crate::interp::token::WordDesc {
					text: body,
					span: node.span(),
					flags: WdFlags::empty()
				}
			};
			let expanded = expand::expand_cmd_sub(dummy_tk)?;
			value = expanded.text().to_string();
		}
		write_vars(|v| v.set_var(&name, RVal::parse(&value).unwrap_or_default()))?;
	});
	Ok(RshWait::Success)
}

fn handle_builtin(mut node: Node, io: ProcIO) -> RshResult<RshWait> {
	let argv = node.get_argv()?;
	let background = node.flags.contains(NdFlags::BACKGROUND);
	let builtin_result = match argv.first().unwrap().text() {
		"echo" => builtin::echo(node, io),
		"expr" => builtin::expr(node, io),
		"set" => builtin::set_or_unset(node, true),
		"jobs" => builtin::jobs(node, io),
		"fg" => builtin::fg(node),
		"bg" => builtin::bg(node),
		"int" => builtin::int(node),
		"bool" => builtin::bool(node),
		"float" => builtin::float(node),
		"arr" => builtin::array(node),
		"type" => builtin::r#type(node),
		//"dict" => builtin::dict(node),
		"unset" => builtin::set_or_unset(node, false),
		"source" => builtin::source(node),
		"cd" => builtin::cd(node),
		"pwd" => builtin::pwd(node.span()),
		"alias" => builtin::alias(node),
		"unalias" => builtin::unalias(node),
		"export" => builtin::export(node),
		"[" | "test" => builtin::test(node.get_argv()?.into()),
		"builtin" => {
			if let NdType::Builtin { mut argv } = node.nd_type {
				argv.pop_front();
				node.nd_type = NdType::Builtin { argv };
				handle_builtin(node, io)
			} else {
				unreachable!()
			}
		}
		_ => unimplemented!("found this builtin: {}", argv[0].text()),
	};

	if !background {
		let pgid = nix::unistd::getpgrp();
		let job = Job::new(0, vec![pgid], argv.iter().map(|arg| arg.text().to_string()).collect(), pgid);
		write_jobs(|j| j.new_fg(job))?;
		write_jobs(|j| j.complete(0))?;
		builtin_result?;
		shellenv::notify_job_done(pgid)?;
	} else {
		let pgid = nix::unistd::getpgrp();
		let job = Job::new(0, vec![pgid], vec![argv.first().unwrap().text().into()], pgid);
		write_jobs(|j| j.new_job(vec![pgid], vec![argv.first().unwrap().text().into()], pgid))?;
		write_jobs(|j| j.complete(0))?;
		shellenv::notify_job_done(pgid)?;
		builtin_result?;
	}

	Ok(RshWait::Success)
}

pub fn handle_subshell(mut node: Node, mut io: ProcIO) -> RshResult<RshWait> {
	// Expand arguments and get redirections
	expand::expand_arguments(&mut node)?;
	let redirs = node.get_redirs()?;

	// Save environment state
	let env_snapshot = SavedEnv::get_snapshot()?;
	write_vars(|v| v.reset_params())?;

	// Perform subshell node operation
	node_operation!(NdType::Subshell { mut body, mut argv }, node, {
		body = expand::expand_var(body.trim().to_string())?;
		if body.is_empty() {
			return Ok(RshWait::Success);
		}

		let mut c_argv = vec![CString::new("anonymous_subshell").unwrap()];
		while let Some(tk) = argv.pop_front() {
			let c_arg = CString::new(tk.text()).unwrap();
			c_argv.push(c_arg);
		}

		// Handle shebang expansion
		body = expand::expand_shebang(body)?;

		// Write the subshell contents to an in-memory file descriptor
		let mut memfd = RustFd::new_memfd("anonymous_subshell", true)?;
		memfd.write(body.as_bytes())?;

		io.backup_fildescs()?;
		io.route_io()?;

		if node.flags.contains(NdFlags::IN_PIPE) || read_meta(|m| m.flags().contains(EnvFlags::IN_SUBSH))? {
			let mut open_fds = VecDeque::new();
			if !redirs.is_empty() {
				open_fds.extend(handle_redirs(redirs.into())?);
			}
			let fd_path = CString::new(format!("/proc/self/fd/{}", memfd)).unwrap();
			let envp = shellenv::get_cstring_evars()?;
			execve(&fd_path, &c_argv, &envp).unwrap();
			unreachable!();
		}

		// Execute the memfd in a forked child process
		match unsafe { fork() } {
			Ok(ForkResult::Child) => {
				let mut open_fds = VecDeque::new();
				if !redirs.is_empty() {
					open_fds.extend(handle_redirs(redirs.into())?);
				}
				let fd_path = CString::new(format!("/proc/self/fd/{}", memfd)).unwrap();
				let envp = shellenv::get_cstring_evars()?;
				execve(&fd_path, &c_argv, &envp).unwrap();
				unreachable!();
			}
			Ok(ForkResult::Parent { child }) => {
				memfd.close()?;
				if !node.flags.contains(NdFlags::IN_CMD_SUB) {
					setpgid(child, child).map_err(|_| ShError::from_internal("Failed to set child PGID"))?;
					shellenv::attach_tty(child)?;
					let job = Job::new(0, vec![child], vec!["anonymous_subshell".into()], child);
					env_snapshot.restore_snapshot()?;
					write_jobs(|j| j.new_fg(job))?;
				}
			}
			Err(_) => return Err(ShError::ExecFailed("Fork failed in handle_subshell".into(), 1, node.span())),
		}

		io.restore_fildescs()?;
		Ok(RshWait::Success)
	})
}


fn handle_function(mut node: Node, io: ProcIO) -> RshResult<RshWait> {
	let span = node.span();
	if let NdType::Function { body, mut argv } = node.nd_type {
		let mut func = *body; // Unbox the root node for the function
		func.flags |= NdFlags::FUNCTION;
		if node.flags.contains(NdFlags::IN_PIPE) {
			func.flags |= NdFlags::IN_PIPE;
		}
		let mut pos_params = vec![];

		while let Some(tk) = argv.pop_front() {
			pos_params.push(tk.text().to_string());
		}


		while let Some(redir) = node.redirs.pop_front() {
			func.redirs.push_back(redir);
		}

		let env_snapshot = SavedEnv::get_snapshot()?;
		write_vars(|v| v.reset_params())?;
		write_meta(|m| m.mod_flags(|f| *f &= !EnvFlags::INTERACTIVE))?;
		for (index,param) in pos_params.into_iter().enumerate() {
			write_vars(|v| v.set_param((index + 1).to_string(), param))?;
		}


		let mut result = traverse_root(func, None, io.clone());
		if let Err(ref mut e) = result {
			*e = e.overwrite_span(span)
		}
		env_snapshot.restore_snapshot()?;
		result
	} else { unreachable!() }
}

/// Handles the execution of a pipeline of commands.
///
/// This function processes a node representing a pipeline of commands,
/// creating the necessary pipes and forking processes to execute each command
/// in the pipeline. It also handles process group IDs to ensure that the
/// commands in the pipeline are executed as a single job.
///
/// # Arguments
///
/// * `node` - The node representing the pipeline of commands.
/// * `io` - The `ProcIO` object managing input/output redirection.
///
/// # Returns
///
/// * `RshResult<RshWait>` - The result of the pipeline execution.
///
/// # Behavior
///
/// 1. **Initialization**:
///    - Initializes variables to keep track of the previous read pipe,
///      process group ID, command names, and process IDs.
///
/// 2. **Command Iteration**:
///    - Iterates through each command in the pipeline.
///    - Creates pipes between commands to handle inter-process communication.
///    - Prepares the `ProcIO` object for each command, setting up input and output redirections.
///
/// 3. **Forking Processes**:
///    - Forks a new process for each command in the pipeline.
///    - In the child process, it routes input/output as necessary and executes the command.
///    - In the parent process, it manages process group IDs and closes unused pipes.
///
/// 4. **Job Management**:
///    - If the pipeline is to be executed in the background, it creates a new background job.
///    - If the pipeline is to be executed in the foreground, it creates a new foreground job and attaches the terminal to the process group.
///
/// 5. **Completion**:
///    - Returns the result of the pipeline execution.
///
/// # Notes
///
/// - This function does not use `fork_instruction!` due to the specific handling required for process group IDs in pipelines.
/// - It handles shell functions containing pipelines and ensures proper input/output routing.
///
/// # Errors
///
/// - Returns an error if any of the fork or exec operations fail.
/// - Returns an error if setting the process group ID fails.
///
/// # Example
///
/// ```rust
/// let node = Node::from_pipeline("command1 | command2");
/// let io = ProcIO::new();
/// let result = handle_pipeline(node, io);
/// ```
///
/// This example demonstrates how to create a pipeline node and handle its execution using the `handle_pipeline` function.
pub fn handle_pipeline(node: Node, mut io: ProcIO) -> RshResult<RshWait> {
	let span = node.span();

	let mut prev_read_pipe: Option<RustFd> = None;
	let mut pgid: Option<Pid> = None;
	let mut cmds = vec![];
	let mut pids = vec![];

	let last_status = node_operation!(NdType::Pipeline { mut commands, both }, node, {
		let background = commands.back().is_some_and(|cmd| cmd.flags.contains(NdFlags::BACKGROUND));
		let mut count = 0;
		let last = commands.len() - 1;
		while let Some(mut command) = commands.pop_front() {
			let (r_pipe, mut w_pipe) = if !commands.is_empty() {
				let (r_pipe, w_pipe) = RustFd::pipe()?;
				(Some(r_pipe), Some(w_pipe))
			} else {
				(None, None)
			};

			let argv = command.get_argv()?;
			if let NdType::Subshell { .. } = command.nd_type {
				cmds.push("subshell".to_string());
			} else {
				let cmd_name = argv.first().unwrap().text();
				cmds.push(cmd_name.to_string());
			}

			let current_io = ProcIO::from(
				prev_read_pipe.take().map(|pipe| pipe.mk_shared()),
				w_pipe.take().map(|pipe| pipe.mk_shared()),
				io.stderr.clone(),
			);

			match unsafe { fork() } {
				Ok(ForkResult::Child) => {
					// This if statement allows shell functions containing pipelines to be used in pipelines
					// Handles the case of the pipeline starting with outside input, and ending with outside output
					if count == 0 {
						// If io contains input, route it here
						io.route_input()?;
					} else if count == last {
						// If io contains output, route it here
						io.route_output()?;
					}
					if let Some(mut read_pipe) = prev_read_pipe {
						read_pipe.close()?;
					}
					if let Some(mut write_pipe) = w_pipe {
						write_pipe.close()?;
					}
					command.flags |= NdFlags::IN_PIPE;
					let result = traverse(command, current_io)?;
					match result {
						RshWait::Success => std::process::exit(0),
						RshWait::Fail { code, cmd: _ } => std::process::exit(code),
						_ => todo!(),
					}
				}
				Ok(ForkResult::Parent { child }) => {
					pids.push(child);
					if let Some(ref mut read_pipe) = prev_read_pipe {
						read_pipe.close()?;
					}
					if let Some(mut write_pipe) = w_pipe {
						write_pipe.close()?;
					}

					if pgid.is_none() {
						pgid = Some(child)
					}

					prev_read_pipe = r_pipe;

					setpgid(child, pgid.unwrap()).map_err(|_| ShError::from_internal("Failed to set pgid in pipeline"))?;
					if commands.is_empty() {
						if background {
							write_jobs(|j| j.new_job(pids, cmds, pgid.unwrap()))?;
						} else {
							let job = Job::new(0, pids, cmds, pgid.unwrap());
							write_jobs(|j| j.new_fg(job))?;
							shellenv::attach_tty(pgid.unwrap())?
						}
						break;
					}
				}
				Err(e) => {
					return Err(ShError::from_execf("Execution failed in pipeline", e.to_string().parse::<i32>().unwrap_or(1), span))
				}
			}
			count += 1;
		}
		RshWait::new()
	});

	Ok(last_status)
}

fn handle_command(node: Node, mut io: ProcIO) -> RshResult<RshWait> {
	let argv = node.get_argv()?;
	let argv = argv.iter().map(|arg| CString::new(arg.text()).unwrap()).collect::<Vec<CString>>();
	let redirs = node.get_redirs()?;

	if let NdType::Command { ref argv } = node.nd_type {
		if helper::handle_autocd_check(&node, &argv.clone().to_vec())? {
			let path_cand = argv.front().unwrap();
			let argv = node.get_argv()?;
			return handle_autocd(node.clone(), argv, path_cand.flags(), io);
		}
	}

	io.backup_fildescs()?;
	io.route_io()?;

	let (command, envp) = prepare_execvpe(&argv)?;

	if node.flags.contains(NdFlags::IN_PIPE) {
		let mut open_fds = VecDeque::new();
		if !redirs.is_empty() {
			open_fds.extend(handle_redirs(redirs.into())?);
		}
		execvpe(&command, &argv, &envp).unwrap();
		unreachable!();
	}

	match unsafe { fork() } {
		Ok(ForkResult::Child) => {
			let mut open_fds = VecDeque::new();
			if !redirs.is_empty() {
				open_fds.extend(handle_redirs(redirs.into())?);
			}
			execvpe(&command, &argv, &envp).unwrap();
			unreachable!();
		}
		Ok(ForkResult::Parent { child }) => {
			setpgid(child, child).map_err(|_| ShError::from_internal("Failed to set child PGID"))?;
			if node.flags.contains(NdFlags::BACKGROUND) {
				let job_id = write_jobs(|j| j.new_job(vec![child], vec![command.to_str().unwrap().to_string()], child))?;
			} else {
				if !node.flags.contains(NdFlags::FUNCTION) {
					shellenv::attach_tty(child)?;
				}
				let job = Job::new(0, vec![child], vec![command.to_str().unwrap().to_string()], child);
				write_jobs(|j| j.new_fg(job))?;
			}
		}
		Err(_) => return Err(ShError::ExecFailed("Fork failed in handle_command".into(), 1, node.span())),
	}

	io.restore_fildescs()?;
	Ok(RshWait::Success)
}

fn handle_redirs(mut redirs: VecDeque<Node>) -> RshResult<VecDeque<RustFd>> {
	let mut fd_queue: VecDeque<RustFd> = VecDeque::new();
	let mut fd_dupes: VecDeque<Redir> = VecDeque::new();

	while let Some(redir_tk) = redirs.pop_front() {
		if let NdType::Redirection { ref redir } = redir_tk.nd_type {
			let Redir { fd_source, op, fd_target, file_target } = &redir;
			if fd_target.is_some() {
				fd_dupes.push_back(redir.clone());
			} else if let Some(file_path) = file_target {
				let source_fd = RustFd::new(*fd_source)?;
				let flags = match op {
					RedirType::Input => OFlag::O_RDONLY,
					RedirType::Output => OFlag::O_WRONLY | OFlag::O_CREAT | OFlag::O_TRUNC,
					RedirType::Append => OFlag::O_WRONLY | OFlag::O_CREAT | OFlag::O_APPEND,
					_ => unimplemented!()
				};
				let mut file_fd = RustFd::open(Path::new(file_path.text()), flags, Mode::from_bits(0o644).unwrap())?;
				file_fd.dup2(&source_fd)?;
				file_fd.close()?;
				fd_queue.push_back(source_fd);
			}
		}
	}

	while let Some(dupe_redir) = fd_dupes.pop_front() {
		let Redir { fd_source, op: _, fd_target, file_target: _ } = dupe_redir;
		let mut target_fd = RustFd::new(fd_target.unwrap())?;
		let source_fd = RustFd::new(fd_source)?;
		target_fd.dup2(&source_fd)?;
		target_fd.close()?;
		fd_queue.push_back(source_fd);
	}

	Ok(fd_queue)
}

fn prepare_execvpe(argv: &[CString]) -> RshResult<(CString, Vec<CString>)> {
	let command = argv[0].clone();

	// Clone the environment variables into a temporary structure
	let env_vars: Vec<(String, String)> = read_vars(|vars| {
		vars.borrow_evars()
			.iter()
			.map(|(k, v)| (k.clone(), v.clone()))
			.collect()
	})?;

	// Convert the environment variables into CString
	let envp = env_vars
		.iter()
		.map(|(k, v)| {
			let env_pair = format!("{}={}", k, v);
			CString::new(env_pair).expect("Failed to create CString")
		})
	.collect::<Vec<CString>>();

		Ok((command, envp))
}

fn handle_autocd(node: Node, argv: Vec<Tk>,flags: WdFlags,io: ProcIO) -> RshResult<RshWait> {
	let cd_token = Tk::new("cd".into(), node.span(), flags);
	let mut autocd_argv = VecDeque::from(argv);
	autocd_argv.push_front(cd_token.clone());
	let autocd = Node {
		command: Some(cd_token),
		nd_type: NdType::Builtin { argv: autocd_argv },
		span: node.span(),
		flags: node.flags,
		redirs: node.redirs
	};
	traverse(autocd,io)
}

//#[cfg(test)]
//mod test {
	//use ctor::{dtor,ctor};
//use parse::descend;
//use pretty_assertions::assert_eq;
//use rstest::{fixture, rstest};
	//use std::fs;
	////use crate::{interp::token::RshTokenizer, signal};
//
//use super::*;
//
	//#[fixture]
	//fn mock_environment() {
		////fs::create_dir_all("/tmp/rsh_test_dir").unwrap();
//
		//fs::write("/tmp/rsh_test_dir/file1.txt", "This is the content of file1.").unwrap();
//
		//println!("Fake environment initialized successfully!");
	//}
	////#[dtor]
	//fn teardown_fake_environment() {
		//std::env::set_current_dir("/home").unwrap();
		//fs::remove_dir_all("/tmp/rsh_test_dir");
	//}
//
	////fn cap_output(expr: &str) -> String {
		//let test_subshell = Node {
			//command: None,
			//nd_type: NdType::Subshell { body: expr.to_string(), argv: VecDeque::new() },
			//flags: NdFlags::VALID_OPERAND,
			//span: Span::new(),
			////redirs: VecDeque::new()
		//};
		//let (r_pipe, mut w_pipe) = RustFd::pipe().unwrap();
		//let io = ProcIO::from(None, Some(w_pipe.mk_shared()), None);
		//handle_subshell(test_subshell, io).unwrap();
		//r_pipe.read().unwrap()
	////}
//
	//#[test]
	//fn test_stdout() {
		//std::fs::write("/tmp/file1.txt", "hi there");
		//let body = "for i in 1 2 3; do echo $i; done".to_string();
		////let test_subshell = Node {
			//command: None,
			//nd_type: NdType::Subshell { body, argv: VecDeque::new() },
			//flags: NdFlags::VALID_OPERAND,
			//span: Span::new(),
			//redirs: VecDeque::new()
		//};
		//let (r_pipe, mut w_pipe) = RustFd::pipe().unwrap();
		//let io = ProcIO::from(None, Some(w_pipe.mk_shared()), None);
		//handle_subshell(test_subshell, io).unwrap();
		//let output = r_pipe.read().unwrap();
		//std::fs::remove_file("/tmp/file1.txt");
		//assert_eq!(output,"1\n2\n3\n".to_string())
	//}
//}
