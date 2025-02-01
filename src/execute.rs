use std::{collections::{HashMap, VecDeque}, ffi::CString, fmt::{self, Display}, os::fd::{AsFd, AsRawFd, FromRawFd, IntoRawFd, RawFd}, path::Path, sync::Arc};

use libc::{memfd_create, MFD_CLOEXEC};
use nix::{errno::Errno, fcntl::{open, OFlag}, sys::{signal::Signal, stat::Mode, wait::WaitStatus}, unistd::{close, dup, dup2, execve, execvpe, fork, pipe, ForkResult, Pid}};
use std::sync::Mutex;

use crate::{builtin::{self, CdFlags}, event::{self, ShError}, interp::{expand, helper::{self, StrExtension, VecDequeExtension}, parse::{self, NdFlags, NdType, Node}, token::{AssOp, LashTokenizer, Redir, RedirType, Tk, TkType, WdFlags}}, shellenv::{self, read_logic, read_meta, read_vars, write_jobs, write_logic, write_meta, write_vars, ChildProc, EnvFlags, JobBuilder, LashVal, SavedEnv}, LashResult};

#[macro_export]
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
	pub fn new(fd: RawFd) -> LashResult<Self> {
		if fd < 0 {
			return Err(ShError::from_internal("Attempted to create a new RustFd from a negative FD"));
		}
		Ok(RustFd { fd })
	}

	/// Create a `RustFd` from a duplicate of `stdin` (FD 0)
	pub fn from_stdin() -> LashResult<Self> {
		let fd = dup(0).map_err(|_| ShError::from_io())?;
		Ok(Self { fd })
	}

	/// Create a `RustFd` from a duplicate of `stdout` (FD 1)
	pub fn from_stdout() -> LashResult<Self> {
		let fd = dup(1).map_err(|_| ShError::from_io())?;
		Ok(Self { fd })
	}

	/// Create a `RustFd` from a duplicate of `stderr` (FD 2)
	pub fn from_stderr() -> LashResult<Self> {
		let fd = dup(2).map_err(|_| ShError::from_io())?;
		Ok(Self { fd })
	}

	/// Create a `RustFd` from a type that provides an owned or borrowed FD
	pub fn from_fd<T: AsFd>(fd: T) -> LashResult<Self> {
		let raw_fd = fd.as_fd().as_raw_fd();
		if raw_fd < 0 {
			return Err(ShError::from_internal("Attempted to convert to RustFd from a negative FD"));
		}
		Ok(RustFd { fd: raw_fd })
	}

	/// Create a `RustFd` by consuming ownership of an FD
	pub fn from_owned_fd<T: IntoRawFd>(fd: T) -> LashResult<Self> {
		let raw_fd = fd.into_raw_fd(); // Consumes ownership
		if raw_fd < 0 {
			return Err(ShError::from_internal("Attempted to convert to RustFd from a negative FD"));
		}
		Ok(RustFd { fd: raw_fd })
	}

	/// Create a new `RustFd` that points to an in-memory file descriptor. In-memory file descriptors can be interacted with as though they were normal files.
	pub fn new_memfd(name: &str, executable: bool) -> LashResult<Self> {
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
	pub fn write(&self, buffer: &[u8]) -> LashResult<()> {
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

	pub fn read(&self) -> LashResult<String> {
		if !self.is_valid() {
			return Err(ShError::from_internal("Attempted to read from an invalid RustFd"));
		}
		let mut buffer = [0; 1024];
		let mut output = String::new();

		loop {
			match nix::unistd::read(self.as_raw_fd(), &mut buffer) {
				Ok(0) => break, // End of pipe
				Ok(bytes_read) => {
					output.push_str(&String::from_utf8_lossy(&buffer[..bytes_read]));
				}
				Err(_) => return Err(ShError::from_io()),
			}
		}
		Ok(output)
	}

	/// Wrapper for nix::unistd::pipe(), simply produces two `RustFds` that point to a read and write pipe respectfully
	pub fn pipe() -> LashResult<(Self,Self)> {
		let (r_pipe,w_pipe) = pipe().map_err(|_| ShError::from_io())?;
		let r_fd = RustFd::from_owned_fd(r_pipe)?;
		let w_fd = RustFd::from_owned_fd(w_pipe)?;
		Ok((r_fd,w_fd))
	}

	/// Produce a `RustFd` that points to the same resource as the 'self' `RustFd`
	pub fn dup(&self) -> LashResult<Self> {
		if !self.is_valid() {
			return Err(ShError::from_internal("Attempted to dup an invalid fd"));
		}
		let new_fd = dup(self.fd).map_err(|_| ShError::from_io())?;
		Ok(RustFd { fd: new_fd })
	}

	/// A wrapper for nix::unistd::dup2(), 'self' is duplicated to the given target file descriptor.
	#[track_caller]
	pub fn dup2<T: AsRawFd>(&self, target: &T) -> LashResult<()> {
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
	pub fn open(path: &Path, flags: OFlag, mode: Mode) -> LashResult<Self> {
		let file_fd: RawFd = open(path, flags, mode).map_err(|_| ShError::from_io())?;
		Ok(Self { fd: file_fd })
	}

	#[track_caller]
	pub fn close(&mut self) -> LashResult<()> {
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

pub fn traverse_ast(ast: Node, io: Option<ProcIO>) -> LashResult<LashWait> {
	let saved_in = RustFd::from_stdin()?;
	let saved_out = RustFd::from_stdout()?;
	let saved_err = RustFd::from_stderr()?;
	let io = if let Some(proc_io) = io { proc_io } else { ProcIO::new() };
	let status = traverse(ast, io)?;
	saved_in.dup2(&0)?;
	saved_out.dup2(&1)?;
	saved_err.dup2(&2)?;
	Ok(status)
}


fn traverse(mut node: Node, io: ProcIO) -> LashResult<LashWait> {
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
		NdType::Match {..} => {
			last_status = handle_match(node,io)?;
		}
		NdType::Select {..} => {
			todo!("handle select")
		}
		NdType::CommandSub {..} => {
			last_status = handle_cmd_sub(node,io)?;
		}
		NdType::Subshell {..} => {
			last_status = handle_subshell(node,io)?;
		}
		NdType::FuncDef {..} => {
			last_status = handle_func_def(node)?;
		}
		NdType::Assignment {..} => {
			last_status = handle_assignment(node, io)?;
		}
		NdType::Cmdsep => {
			last_status = LashWait::new();
		}
		NdType::Root {..} => {
			last_status = traverse_root(node, None, io)?;
		}
		_ => unimplemented!("Support for node type `{:?}` is not yet implemented",node.nd_type)
	}
	Ok(last_status)
}

fn traverse_root(mut root_node: Node, break_condition: Option<bool>, io: ProcIO) -> LashResult<LashWait> {
	let mut last_status = LashWait::new();
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
						if let LashWait::Fail {..} = last_status {
							break
						}
					}
					false => {
						if let LashWait::Success  = last_status {
							break
						}
					}
				}
			}
		}
	}
	Ok(last_status)
}

fn handle_func_def(node: Node) -> LashResult<LashWait> {
	let last_status = LashWait::new();
	node_operation!(NdType::FuncDef { name, body }, node, {
		write_logic(|l| l.new_func(&name, &body))?;
	});
	Ok(last_status)
}

pub fn handle_cmd_sub(node: Node, io: ProcIO) -> LashResult<LashWait> {
	let last_status;
	let snapshot = SavedEnv::get_snapshot()?;
	node_operation!(NdType::CommandSub { body }, node, {
		last_status = event::execute(&body, NdFlags::empty(), None, Some(io))
	});
	snapshot.restore_snapshot()?;
	last_status
}

fn handle_for(node: Node,io: ProcIO) -> LashResult<LashWait> {
	let mut last_status = LashWait::new();

	match unsafe { fork() } {
		Ok(ForkResult::Child) => {
			let body_io = ProcIO::from(None, io.stdout, io.stderr);
			let redirs = node.get_redirs()?;
			handle_redirs(redirs.into())?;

			node_operation!(NdType::For { loop_vars, mut loop_arr, mut loop_body}, node, {
				loop_body.flags |= NdFlags::FOR_BODY;
				let loop_body = node_operation!(NdType::LoopBody { body }, loop_body, {
					body
				});
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
					write_vars(|v| v.set_var(&current_var, LashVal::parse(&current_val).unwrap_or_default()))?;

					iteration_count += 1;
					// TODO: modulo is expensive; find a better way to do this
					var_index = iteration_count % var_count;

					let result = event::execute(&loop_body, NdFlags::empty(), None, Some(body_io.clone()));
					match result {
						Err(ShError::LoopBreak) => break,
						Ok(_) |
							Err(ShError::LoopCont) => continue,
						Err(_) => break
					}
				}
			});
			std::process::exit(0);
		}
		Ok(ForkResult::Parent { child }) => {
			let children = vec![
				ChildProc::new(child,Some("for"),None)?,
			];
			let job = JobBuilder::new()
				.with_pgid(child)
				.with_children(children)
				.build();

			if node.flags.contains(NdFlags::BACKGROUND) {
				write_jobs(|j| j.insert_job(job,false))??;

			} else {
				if !node.flags.contains(NdFlags::FUNCTION) {
					shellenv::attach_tty(child)?;
				}
				helper::handle_fg(job)?;
			}
		}
		Err(_) => return Err(ShError::ExecFailed("Fork failed in handle_for".into(), 1, node.span())),
	}

	Ok(last_status)
}

fn handle_loop(node: Node, io: ProcIO) -> LashResult<LashWait> {
	let mut last_status = LashWait::new();
	let cond_io = ProcIO::from(io.stdin, None, None);
	let body_io = ProcIO::from(None, io.stdout, io.stderr);

	node_operation!(NdType::Loop { condition, logic }, node, {
		// Idea: try turning cond and body into Mutexes or RwLocks to avoid excessive cloning in the loop
		// ProcIO already uses Arc<Mutex> so cloning should be pretty cheap
		let cond = node_operation!(NdType::LoopCond { cond }, *logic.condition, {
			cond
		});

		let body = node_operation!(NdType::LoopBody { body }, *logic.body, {
			body
		});
		loop {
			let result = event::execute(&cond, NdFlags::empty(), None, Some(cond_io.clone()));
			let is_success = read_vars(|v| v.get_param("?").is_some_and(|val| val == "0"))?;

			match condition {
				true => {
					if !is_success {
						write_vars(|v| v.set_param("?".into(), "0".into()))?;
						write_meta(|v| v.set_last_command("while"))?;
						break
					}
				}
				false => {
					if is_success {
						write_vars(|v| v.set_param("?".into(), "0".into()))?;
						write_meta(|v| v.set_last_command("until"))?;
						break
					}
				}
			}

			let result = event::execute(&body, NdFlags::empty(), None, Some(body_io.clone()));
			match result {
				Err(ShError::LoopBreak) => break,
				Ok(_) |
				Err(ShError::LoopCont) => continue,
				Err(e) => return Err(e),
			}
		}
	});

	Ok(last_status)
}


fn handle_if(node: Node, io: ProcIO) -> LashResult<LashWait> {
	let last_status = LashWait::new();
	let cond_io = ProcIO::from(io.stdin,None,None);
	let body_io = ProcIO::from(None,io.stdout,io.stderr);

	node_operation!(NdType::If { mut cond_blocks, else_block }, node, {
		while let Some(block) = cond_blocks.pop_front() {
			let cond = *block.condition;
			let body = *block.body;

			traverse(cond, cond_io.clone())?;
			let code = read_vars(|v| v.get_param("?"))?;
			let cmd = read_meta(|m| m.get_last_command())?;
			let is_success = code.clone().unwrap_or("0".to_string()) == "0";
			if is_success {
				traverse(body, body_io.clone())?;
				return Ok(last_status)
			}
		}
		if let Some(block) = else_block {
			return traverse_root(*block, None, body_io)
		}
	});
	Ok(last_status)
}

fn handle_match(node: Node, io: ProcIO) -> LashResult<LashWait> {
	let mut last_status = LashWait::new();

	node_operation!(NdType::Match { in_var, arms }, node, {
		let mut match_arms = arms;
		while let Some(arm) = match_arms.pop_front() {
			if let TkType::MatchArm { pat, body } = arm.class() {
				if pat.trim() == in_var.text().trim() {
					event::execute(&body, NdFlags::empty(), Some(node.redirs.clone()), Some(io))?;
					break
				}
			} else { unreachable!() }
		}
		Ok(last_status)
	})
}

fn handle_chain(node: Node) -> LashResult<LashWait> {
	node_operation!(NdType::Chain { commands, op }, node, {
		let mut commands = commands;
		while let Some(cmd) = commands.pop_front() {
			traverse(cmd, ProcIO::new())?;

			let statuses = write_jobs(|j| {
				if let Some(job) = j.get_fg_mut() {
					job.get_statuses()
				} else {
					vec![]
				}
			})?;

			let is_success = read_vars(|v| v.get_param("?").is_some_and(|val| val == "0"))?;

			match op.nd_type {
				NdType::And if !is_success => {
					break
				},
				NdType::Or if is_success => {
					break
				},
				_ => { /* Keep going */ }
			}
		}
	});

	Ok(LashWait::Success)
}

fn handle_assignment(node: Node, io: ProcIO) -> LashResult<LashWait> {
	node_operation!(NdType::Assignment { name, value, op, command }, node, {
		match op {
			AssOp::Equals => {
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
				let value = LashVal::parse(&value).unwrap_or_default();
				if let Some(cmd_node) = command {
					let snapshot = SavedEnv::get_snapshot()?;
					write_vars(|v| v.export_var(&name, &value.to_string()))?;
					traverse_root(*cmd_node, None, io)?;
					snapshot.restore_snapshot()?;
				} else {
					write_vars(|v| v.set_var(&name, value))?;
				}
			}
			AssOp::PlusEquals => {
				if let Some(left_val) = read_vars(|v| v.get_var(&name))? {
					let right_val = LashVal::parse(&value.unwrap_or_default()).map_err(|e| ShError::from_parse(&e, node.span()))?;
					let result = helper::add_vars(left_val, right_val, node.span())?;

					if let Some(cmd_node) = command {
						let snapshot = SavedEnv::get_snapshot()?;
						write_vars(|v| v.export_var(&name, &result.to_string()))?;
						traverse_root(*cmd_node, None, io)?;
						snapshot.restore_snapshot()?;
					} else {
						write_vars(|v| v.set_var(&name, result))?;
					}
				}
			}
			AssOp::MinusEquals => {
				if let Some(left_val) = read_vars(|v| v.get_var(&name))? {
					let right_val = LashVal::parse(&value.unwrap_or_default()).map_err(|e| ShError::from_parse(&e, node.span()))?;
					let result = helper::subtract_vars(left_val, right_val, node.span())?;

					if let Some(cmd_node) = command {
						let snapshot = SavedEnv::get_snapshot()?;
						write_vars(|v| v.export_var(&name, &result.to_string()))?;
						traverse_root(*cmd_node, None, io)?;
						snapshot.restore_snapshot()?;
					} else {
						write_vars(|v| v.set_var(&name, result))?;
					}
				}
			}
		}
	});
	Ok(LashWait::Success)
}

fn handle_builtin(mut node: Node, io: ProcIO) -> LashResult<LashWait> {
	let argv = node.get_argv()?;
	write_meta(|m| m.set_last_command(argv.first().unwrap().text()))?;
	let result = match argv.first().unwrap().text() {
		"continue" => return Err(ShError::LoopCont), // These will propagate back to the loop function and be handled there
		"break" => return Err(ShError::LoopBreak),
		"echo" => builtin::echo(node, io)?,
		"expr" => builtin::expr(node, io)?,
		"set" => builtin::set_or_unset(node, true)?,
		"jobs" => builtin::jobs(node, io)?,
		"fg" => builtin::fg(node)?,
		"bg" => builtin::bg(node)?,
		"exit" => builtin::lash_exit(node)?,
		"return" => builtin::lash_return(node)?,
		"pushd" => builtin::pushd(node)?,
		"popd" => builtin::popd(node)?,
		"setopt" => builtin::setopt(node)?,
		"getopt" => builtin::getopt(node, io)?,
		"string" => builtin::string(node)?,
		"int" => builtin::int(node)?,
		"read_func" => builtin::read_func(node, io)?,
		"bool" => builtin::bool(node)?,
		"float" => builtin::float(node)?,
		"arr" => builtin::array(node)?,
		"type" => builtin::r#type(node)?,
		//"dict" => builtin::dict(node)?,
		"unset" => builtin::set_or_unset(node, false)?,
		"source" => builtin::source(node)?,
		"cd" => builtin::cd(node, CdFlags::CHANGE)?,
		"pwd" => builtin::pwd(node.span())?,
		"alias" => builtin::alias(node)?,
		"unalias" => builtin::unalias(node)?,
		"export" => builtin::export(node)?,
		"shift" => builtin::shift(node)?,
		"[" | "test" => builtin::test(node.get_argv()?.into())?,
		"builtin" => {
			if let NdType::Builtin { mut argv } = node.nd_type {
				argv.pop_front();
				node.nd_type = NdType::Builtin { argv };
				handle_builtin(node, io)?
			} else {
				unreachable!()
			}
		}
		"command" => {
			if let NdType::Builtin { mut argv } = node.nd_type {
				argv.pop_front();
				node.nd_type = NdType::Command { argv };
				handle_command(node, io)?
			} else {
				unreachable!()
			}
		}
		_ => unimplemented!("found this builtin: {}", argv[0].text()),
	};
	match result {
		LashWait::Success => write_vars(|v| v.set_param("?".into(), "0".into()))?,
		LashWait::Fail { code, cmd: _ } => write_vars(|v| v.set_param("?".into(), code.to_string()))?,
		LashWait::Signaled { sig } | LashWait::Stopped { sig } => write_vars(|v| v.set_param("?".into(), (sig as i32).to_string()))?,
		_ => unimplemented!()
	}

	Ok(LashWait::Success)
}

pub fn handle_subshell(mut node: Node, mut io: ProcIO) -> LashResult<LashWait> {
	/*
	 * rsh subshells work differently than subshells in standard shells like bash and zsh.
	 *
	 * In rsh, the text content of a subshell is written to an in-memory file descriptor.
	 * This function then asks the kernel to execute the file descriptor as though it were a script file.
	 *
	 * Of course, this behavior means that the resulting script-fd must have a shebang.
	 * If no shebang is provided, one is dynamically inserted using the path to the rsh binary.
	 * Provided shebangs can use just the program's name, and it will be expanded to the full path
	 * if a program by that name is found in the user's PATH.
	 *
	 * This means that rsh subshells can execute code using any interpreter,
	 * and the output can be captured and operated on. For instance:
	 * (#!python
	 * print("hello world")
	 * ) | (#!bash
	 * read -r line
	 * echo "$line" | awk '{print $1}'
	 * )
	 * This would produce the output `hello`.
	 */
	let redirs = node.get_redirs()?;
	if let NdType::CommandSub { body } = node.nd_type {
		let var_table = read_vars(|v| v.clone())?;
		let expanded_body = expand::expand_var(body, &var_table)?;
		node.nd_type = NdType::Subshell { body: expanded_body, argv: VecDeque::new() }
	} else {
		expand::expand_arguments(&mut node)?;
	}

	// Perform subshell node operation
	node_operation!(NdType::Subshell { mut body, mut argv }, node, {
		if body.is_empty() {
			return Ok(LashWait::Success);
		}

		let mut c_argv = vec![CString::new("anonymous_subshell").unwrap()];
		while let Some(tk) = argv.pop_front() {
			let c_arg = CString::new(tk.text()).unwrap();
			c_argv.push(c_arg);
		}

		// Handle shebang expansion
		body = expand::expand_shebang(body)?;

		// Write the subshell contents to an in-memory file descriptor
		let memfd = RustFd::new_memfd("anonymous_subshell", true)?;
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
				if !node.flags.contains(NdFlags::IN_CMD_SUB) {
					let children = vec![
						ChildProc::new(child,Some("anonymous_subshell"),None)?,
					];
					let job = JobBuilder::new()
						.with_pgid(child)
						.with_children(children)
						.build();
					shellenv::attach_tty(child)?;
					helper::handle_fg(job)?;
				}
			}
			Err(_) => return Err(ShError::ExecFailed("Fork failed in handle_subshell".into(), 1, node.span())),
		}

		io.restore_fildescs()?;
		Ok(LashWait::Success)
	})
}


fn handle_function(mut node: Node, io: ProcIO) -> LashResult<LashWait> {
	let span = node.span();
	if let NdType::Function { body, mut argv } = node.nd_type {
		let mut func = body; // Unbox the root node for the function
		node.flags |= NdFlags::FUNCTION;
		let snapshot = shellenv::SavedEnv::get_snapshot()?;
		let mut var_table = read_vars(|v| v.clone())?;
		let mut pos_params = vec![];
		write_meta(|m| m.set_last_command(argv.front().unwrap().text()))?;

		argv.pop_front(); // Ignore function name
		while let Some(tk) = argv.pop_front() {
			pos_params.push(tk.text().to_string());
		}

		var_table.reset_params();
		write_meta(|m| m.mod_flags(|f| *f &= !EnvFlags::INTERACTIVE))?;
		for param in pos_params {
			var_table.pos_param_pushback(&param);
		}
		write_vars(|v| *v = var_table)?;
		let result = event::execute(&func, node.flags, Some(node.redirs.clone()), Some(io));
		snapshot.restore_snapshot()?;
		match result {
			Ok(_) => { /* Do nothing */ }
			Err(ShError::FuncReturn(code)) => write_vars(|v| v.set_param("?".into(), code.to_string()))?,
			Err(e) => return Err(e)
		}

		Ok(LashWait::Success)
	} else { unreachable!() }
}

pub fn handle_pipeline(node: Node, mut io: ProcIO) -> LashResult<LashWait> {
	/*
	 * This one is pretty complex, but it can't really be helped. File descriptors complicate everything they touch.
	 *
	 * In this, we loop through the commands held in the pipeline node.
	 * For each command, a read pipe and a write pipe are created.
	 * prev_read_pipe starts out as None, but is replaced by the read pipe on each iteration
	 * This has the effect of routing the output of the last command to the input of the next one.
	 * A new ProcIO instance is created for each command, and then used in the traverse() call.
	 *
	 */
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
					/*
					 * The if statement here allows for cases where the first pipeline command actually does have stdin.
					 * For instance, a function `func() { sed 's/hello/goodbye/' | awk '{print $1}' }` being used
					 * in a pipeline like `echo hello world | func` to produce the output `goodbye`.
					 * Works similarly in cases where the stdout leaves the scope of the function instead.
					 */
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
						LashWait::Success => std::process::exit(0),
						LashWait::Fail { code, cmd: _ } => std::process::exit(code),
						_ => todo!(),
					}
				}
				Ok(ForkResult::Parent { child }) => {
					/*
					 * Here we just do some file descriptor handling and job control stuff
					 * A new job is created for the pipeline, and leftover pipes are closed.
					 */
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

					if commands.is_empty() {
						let mut children = Vec::new();
						for (index,pid) in pids.iter().enumerate() {
							let child = ChildProc::new(*pid, cmds.get(index).map(|cmd| cmd.as_str()), pgid)?;
							children.push(child);
						}
						let job = JobBuilder::new()
							.with_pgid(pgid.unwrap())
							.with_children(children)
							.build();

						if background {
							write_jobs(|j| j.insert_job(job,false))??;
						} else {
							helper::handle_fg(job)?;
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
		LashWait::new()
	});

	Ok(last_status)
}

fn handle_command(node: Node, mut io: ProcIO) -> LashResult<LashWait> {
	/*
	 * In this function, we first get the arg vector of the command as CStrings, so as to be compatible with execvpe().
	 *
	 * We also handle input and output with ProcIO::route_io() and handle_redirs(),
	 * these two functions will connect pipes and direct output to file descriptors respectively
	 *
	 * If we are in a pipe, the job control logic is skipped and execvpe() is called in this process
	 * This is because if we are in a pipe, this is already a forked child process, so forking again doesn't make sense.
	 */
	write_meta(|m| m.set_last_command(node.get_argv().unwrap().first().unwrap().text()))?;
	let argv = node.get_argv()?.iter().map(|arg| CString::new(arg.text()).unwrap()).collect::<Vec<CString>>();
	let redirs = node.get_redirs()?;

	// Handle autocd if applicable
	if let NdType::Command { ref argv } = node.nd_type {
		if helper::handle_autocd_check(&node, &argv.clone().to_vec())? {
			let path_cand = argv.front().unwrap();
			let argv = node.get_argv()?;
			return handle_autocd(node.clone(), argv, path_cand.flags(), io);
		}
	}

	// Backup and route I/O
	io.backup_fildescs()?;
	io.route_io()?;

	// Prepare the command and environment
	let (command, envp) = prepare_execvpe(&argv)?;
	let redirs_to_close = handle_redirs(redirs.into())?;

	// Handle redirections and execute the command if in a pipe
	if node.flags.contains(NdFlags::IN_PIPE) {
		execvpe(&command, &argv, &envp).unwrap();
		unreachable!();
	}

	// Fork the process
	match unsafe { fork() } {
		Ok(ForkResult::Child) => {

			// First, try execvpe (searches PATH if no absolute/relative path is provided)
			let Err(e) = execvpe(&command, &argv, &envp);
			if e == Errno::ENOENT {
				// If command not found in PATH, try execve directly
				let Err(_) = execve(&command, &argv, &envp);
				let err = ShError::from_no_cmd(
					&format!(
						"\x1b[1;31mCommand not found\x1b[0m - {}",
						command.to_str().unwrap()
					),
					node.span(),
				);
				event::throw(err).unwrap();
				std::process::exit(1);
			} else {
				let err = ShError::from_execf(format!("Execution failed - {}", e).as_str(), e as i32, node.span());
				event::throw(err).unwrap();
				std::process::exit(e as i32);
			}
		}
		Ok(ForkResult::Parent { child }) => {
			handle_parent_process(child, command, &node)?;
			for mut redir in redirs_to_close {
				redir.close()?;
			}
		}
		Err(_) => return Err(ShError::ExecFailed("Fork failed in handle_command".into(), 1, node.span())),
	}

	// Restore I/O descriptors
	io.restore_fildescs()?;
	Ok(LashWait::Success)
}

fn handle_parent_process(child: Pid, command: CString, node: &Node) -> LashResult<()> {
	let children = vec![
		ChildProc::new(child, Some(command.to_str().unwrap()), None)?
	];
	let job = JobBuilder::new()
		.with_children(children)
		.with_pgid(child)
		.build();

	if node.flags.contains(NdFlags::BACKGROUND) {
		write_jobs(|j| j.insert_job(job,false))??;
	} else {
		helper::handle_fg(job)?;
	}
	Ok(())
}

fn handle_redirs(mut redirs: VecDeque<Node>) -> LashResult<VecDeque<RustFd>> {
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

fn prepare_execvpe(argv: &[CString]) -> LashResult<(CString, Vec<CString>)> {
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

fn handle_autocd(node: Node, argv: Vec<Tk>,flags: WdFlags,io: ProcIO) -> LashResult<LashWait> {
	let cd_token = Tk::new("cd".into(), node.span(), flags);
	let mut autocd_argv = VecDeque::from(argv);
	autocd_argv.push_front(cd_token.clone());
	let autocd = if let Some(body) = read_logic(|l| l.get_func("cd"))? {
		// Let's also handle the rare case of 'cd' being wrapped in a function
		Node {
			command: Some(cd_token),
			nd_type: NdType::Function { body, argv: autocd_argv },
			span: node.span(),
			flags: node.flags,
			redirs: node.redirs
		}
	} else {
		Node {
			command: Some(cd_token),
			nd_type: NdType::Builtin { argv: autocd_argv },
			span: node.span(),
			flags: node.flags,
			redirs: node.redirs
		}
	};
	traverse(autocd,io)
}
