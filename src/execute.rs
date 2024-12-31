use lazy_static::lazy_static;
use libc::{memfd_create, MFD_CLOEXEC};
use nix::sys::memfd::MemFdCreateFlag;
use nix::sys::signal;
use nix::unistd::{close, dup, dup2, execve, execvpe, fork, pipe, ForkResult};
use nix::fcntl::{open,OFlag};
use nix::sys::stat::{fchmod, Mode};
use nix::sys::wait::{waitpid, WaitStatus};
use std::fmt::Display;
use std::os::fd::{AsFd, AsRawFd, FromRawFd, IntoRawFd, RawFd};
use std::ffi::CString;
use std::collections::{HashMap, HashSet, VecDeque};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use log::{info,debug,trace};
use glob::MatchOptions;
use std::{io, thread};

use crate::builtin::{alias, cd, echo, export, pwd, set_or_unset, source, test};
use crate::event::{ShellError, ShellErrorFull};
use crate::interp::helper::VecDequeExtension;
use crate::interp::{expand, parse};
use crate::interp::token::{Redir, RedirType, Tk, WdFlags};
use crate::interp::parse::{descend, NdType, Node, Span};
use crate::shellenv::{EnvFlags, ShellEnv};

pub const GLOB_OPTS: MatchOptions = MatchOptions {
	case_sensitive: false,
	require_literal_separator: true,
	require_literal_leading_dot: false
};

#[derive(Hash,Eq,PartialEq,Debug)]
pub struct SmartFd {
	fd: RawFd
}

impl SmartFd {
	pub fn new(fd: RawFd) -> Result<Self,ShellError> {
		if fd < 0 {
			return Err(ShellError::from_internal("Attempted to create a new SmartFd from a negative FD"));
		}
		Ok(SmartFd { fd })
	}

	/// Create a `SmartFd` from a duplicate of `stdin` (FD 0)
	pub fn from_stdin() -> Result<Self,ShellError> {
			let fd = dup(0).map_err(|_| ShellError::from_io())?;
			Ok(Self { fd })
	}

	/// Create a `SmartFd` from a duplicate of `stdout` (FD 1)
	pub fn from_stdout() -> Result<Self,ShellError> {
			let fd = dup(1).map_err(|_| ShellError::from_io())?;
			Ok(Self { fd })
	}

	/// Create a `SmartFd` from a duplicate of `stderr` (FD 2)
	pub fn from_stderr() -> Result<Self,ShellError> {
			let fd = dup(2).map_err(|_| ShellError::from_io())?;
			Ok(Self { fd })
	}

	/// Create a `SmartFd` from a type that provides an owned or borrowed FD
	pub fn from_fd<T: AsFd>(fd: T) -> Result<Self,ShellError> {
		let raw_fd = fd.as_fd().as_raw_fd();
		if raw_fd < 0 {
			return Err(ShellError::from_internal("Attempted to convert to SmartFd from a negative FD"));
		}
		Ok(SmartFd { fd: raw_fd })
	}

	/// Create a `SmartFd` by consuming ownership of an FD
	pub fn from_owned_fd<T: IntoRawFd>(fd: T) -> Result<Self,ShellError> {
		let raw_fd = fd.into_raw_fd(); // Consumes ownership
		if raw_fd < 0 {
			return Err(ShellError::from_internal("Attempted to convert to SmartFd from a negative FD"));
		}
		Ok(SmartFd { fd: raw_fd })
	}

	pub fn new_memfd(name: &str, executable: bool) -> Result<Self, ShellError> {
		let c_name = CString::new(name).map_err(|_| ShellError::from_internal("Invalid name for memfd"))?;
		let flags = if executable {
			0
		} else {
			MFD_CLOEXEC
		};
		let fd = unsafe { memfd_create(c_name.as_ptr(), flags) };
		Ok(SmartFd { fd })
	}

	pub fn write(&self, buffer: &[u8]) -> Result<(),ShellError> {
		if !self.is_valid() {
			return Err(ShellError::from_internal("Attempted to write to an invalid SmartFd"));
		}
		let result = unsafe { libc::write(self.fd, buffer.as_ptr() as *const libc::c_void, buffer.len()) };
		if result < 0 {
			Err(ShellError::from_io())
		} else {
			Ok(())
		}
	}

	pub fn pipe() -> Result<(Self,Self),ShellError> {
		let (r_pipe,w_pipe) = pipe().map_err(|_| ShellError::from_io())?;
		let r_fd = SmartFd::from_owned_fd(r_pipe)?;
		let w_fd = SmartFd::from_owned_fd(w_pipe)?;
		Ok((r_fd,w_fd))
	}

	pub fn dup(&self) -> Result<Self,ShellError> {
		if !self.is_valid() {
			return Err(ShellError::from_internal("Attempted to dup an invalid fd"));
		}
		let new_fd = dup(self.fd).map_err(|_| ShellError::from_io())?;
		Ok(SmartFd { fd: new_fd })
	}

	pub fn dup2<T: AsRawFd>(&self, target: &T) -> Result<(),ShellError> {
		let target_fd = target.as_raw_fd();
		if self.fd == target_fd {
			// Nothing to do here
			return Ok(())
		}
		if !self.is_valid() || target_fd < 0 {
			return Err(ShellError::from_io());
		}

		dup2(self.fd, target_fd).map_err(|_| ShellError::from_io())?;
		Ok(())
	}

	pub fn open(path: &Path, flags: OFlag, mode: Mode) -> Result<Self,ShellError> {
		let file_fd: RawFd = open(path, flags, mode).map_err(|_| ShellError::from_io())?;
		Ok(Self { fd: file_fd })
	}

	pub fn close(&mut self) -> Result<(),ShellError> {
		if !self.is_valid() {
			return Err(ShellError::from_internal("Attempted to close an invalid SmartFd"));
		}
		close(self.fd).map_err(|_| ShellError::from_io())?;
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

impl Display for SmartFd {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	    write!(f, "{}", self.fd)
	}
}

impl Drop for SmartFd {
	fn drop(&mut self) {
		if self.fd >= 0 {
			self.close().ok();
		}
	}
}

impl AsRawFd for SmartFd {
	fn as_raw_fd(&self) -> RawFd {
		self.fd
	}
}

impl IntoRawFd for SmartFd {
	fn into_raw_fd(self) -> RawFd {
		let fd = self.fd;
		std::mem::forget(self);
		fd
	}
}

impl FromRawFd for SmartFd {
	unsafe fn from_raw_fd(fd: RawFd) -> Self {
		SmartFd { fd }
	}
}

#[derive(Debug)]
pub struct ProcIO {
	pub stdin: Option<Arc<Mutex<SmartFd>>>,
	pub stdout: Option<Arc<Mutex<SmartFd>>>,
	pub stderr: Option<Arc<Mutex<SmartFd>>>,
	pub backup: HashMap<RawFd,SmartFd>
}

impl ProcIO {
	pub fn new() -> Self {
		Self { stdin: None, stdout: None, stderr: None, backup: HashMap::new() }
	}
	pub fn from(stdin: Option<Arc<Mutex<SmartFd>>>, stdout: Option<Arc<Mutex<SmartFd>>>, stderr: Option<Arc<Mutex<SmartFd>>>) -> Self {
			Self {
					stdin,
					stdout,
					stderr,
					backup: HashMap::new(),
			}
	}
	pub fn backup_fildescs(&mut self) -> Result<(), ShellError> {
		let mut backup = HashMap::new();
		// Get duped file descriptors
		let dup_in = SmartFd::from_stdin()?;
		let dup_out = SmartFd::from_stdout()?;
		let dup_err = SmartFd::from_stderr()?;
		// Store them in a hashmap
		backup.insert(0,dup_in);
		backup.insert(1,dup_out);
		backup.insert(2,dup_err);
		self.backup = backup;
		Ok(())
	}
	pub fn restore_fildescs(&mut self) -> Result<(), ShellError> {
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
	pub fn do_plumbing(&mut self) -> Result<(), ShellError> {
		if let Some(ref mut err_pipe) = self.stderr {
			let mut pipe = err_pipe.lock().unwrap();
			pipe.dup2(&2)?;
			pipe.close()?;
		}
		// Redirect stdout
		if let Some(ref mut w_pipe) = self.stdout {
			let mut pipe = w_pipe.lock().unwrap();
			pipe.dup2(&1)?;
			pipe.close()?;
		}

		// Redirect stdin
		if let Some(ref mut r_pipe) = self.stdin {
			let mut pipe = r_pipe.lock().unwrap();
			pipe.dup2(&0)?;
			pipe.close()?;
		}
		Ok(())
	}

	pub fn try_clone(&self) -> Self {
		ProcIO::from(self.stdin.clone(),self.stdout.clone(),self.stderr.clone())
	}
}

impl Default for ProcIO {
	fn default() -> Self {
		Self::new()
	}
}

#[derive(PartialEq,Debug,Clone)]
pub enum RshWaitStatus {
	Success { span: Span },
	Fail { code: i32, cmd: Option<String>, span: Span },
	Stopped { pid: i32 },
	Terminated { signal: i32, pid: i32 },
	Continued { pid: i32 },
	Running { pid: i32 },
	Killed { signal: i32, pid: i32 },
	TimeOut { pid: i32 },

	// These wait statuses are returned by builtins like `return` and `break`
	SIGRETURN, // Return from a function
	SIGCONT, // Restart a loop from the beginning
	SIGBREAK, // Break a loop
	SIGRSHEXIT // Internal call to exit early
}

impl RshWaitStatus {
	pub fn new() -> Self {
		RshWaitStatus::Success { span: Span::new() }
	}
	pub fn s(span: Span) -> Self {
		RshWaitStatus::Success { span }
	}
	pub fn f(code: i32, cmd: Option<String>, span: Span) -> Self {
		RshWaitStatus::Fail { code, cmd, span }
	}
}

impl Default for RshWaitStatus {
	fn default() -> Self {
		RshWaitStatus::new()
	}
}


pub struct NodeWalker<'a> {
	ast: Node,
	shellenv: &'a mut ShellEnv,
}

impl<'a> NodeWalker<'a> {
	pub fn new(ast: Node, shellenv: &'a mut ShellEnv) -> Self {
		Self {
			ast,
			shellenv
		}
	}
	pub fn start_walk(&mut self) -> Result<RshWaitStatus,ShellError> {
		info!("Going on a walk...");
		// Save file descs just in case
		let saved_in = SmartFd::from_stdin()?;
		let saved_out = SmartFd::from_stdout()?;
		let saved_err = SmartFd::from_stderr()?;
		let mut exit_status = RshWaitStatus::new();
		let mut nodes;
		if let NdType::Root { ref mut deck } = self.ast.nd_type {
			nodes = std::mem::take(deck);
		} else { unreachable!() }
		while let Some(node) = nodes.pop_front() {
			exit_status = self.walk(node, ProcIO::new())?;
		}
		// Restore file descs just in case
		saved_in.dup2(&0)?;
		saved_out.dup2(&1)?;
		saved_err.dup2(&2)?;
		Ok(exit_status)
	}

	fn walk(&mut self, node: Node, io: ProcIO) -> Result<RshWaitStatus,ShellError> {
		let last_status;
		match node.nd_type {
			NdType::Command { ref argv } | NdType::Builtin { ref argv } => {
				let mut node = node.clone();
				let command_name = argv.front().unwrap();
				let not_from_alias = !command_name.flags().contains(WdFlags::FROM_ALIAS);
				let is_not_command_builtin = command_name.text() != "command";
				if not_from_alias && is_not_command_builtin {
					node = expand::expand_alias(self.shellenv, node.clone())?;
				}
				if let Some(_func) = self.shellenv.get_function(command_name.text()) {
					last_status = self.handle_function(node, io)?;
				} else if !matches!(node.nd_type, NdType::Command {..} | NdType::Builtin {..}) {
					// If the resulting alias expansion returns a root node
					// then walk the resulting sub-tree
					return self.walk_root(node, None, io)
				} else {
					match node.nd_type {
						NdType::Command {..} => {
							trace!("Found command: {:?}",node);
							last_status = self.handle_command(node, io)?;
						}
						NdType::Builtin {..} => {
							last_status = self.handle_builtin(node, io)?;
						}
						_ => unreachable!()
					}
				}
			}
			//NdType::Function { .. } => {
			//todo!("handle simple commands")
			//}
			NdType::Pipeline {..} => {
				last_status = self.handle_pipeline(node, io)?;
			}
			NdType::Chain {..} => {
				last_status = self.handle_chain(node)?;
			}
			NdType::If {..} => {
				last_status = self.handle_if(node,io)?;
			}
			NdType::For {..} => {
				last_status = self.handle_for(node,io)?;
			}
			NdType::Loop {..} => {
				last_status = self.handle_loop(node,io)?;
			}
			NdType::Case {..} => {
				todo!("handle case")
			}
			NdType::Select {..} => {
				todo!("handle select")
			}
			NdType::Subshell {..} => {
				last_status = self.handle_subshell(node,io)?;
			}
			NdType::FuncDef {..} => {
				last_status = self.handle_func_def(node)?;
			}
			NdType::Assignment {..} => {
				last_status = self.handle_assignment(node)?;
			}
			NdType::Cmdsep => {
				last_status = RshWaitStatus::new();
			}
			_ => unimplemented!("Support for node type `{:?}` is not yet implemented",node.nd_type)
		}
		Ok(last_status)
	}

	fn walk_root(&mut self, mut node: Node, break_condition: Option<bool>, io: ProcIO) -> Result<RshWaitStatus,ShellError> {
		let mut last_status = RshWaitStatus::new();
		if !node.redirs.is_empty() {
			node = parse::propagate_redirections(node)?;
		}
		if let NdType::Root { deck } = node.nd_type {
			for node in deck {
				last_status = self.walk(node, io.try_clone())?;
				if let Some(condition) = break_condition {
					match condition {
						true => {
							if let RshWaitStatus::Fail {..} = last_status {
								break
							}
						}
						false => {
							if let RshWaitStatus::Success {..} = last_status {
								break
							}
						}
					}
				}
			}
		}
		Ok(last_status)
	}

	fn handle_func_def(&mut self, node: Node) -> Result<RshWaitStatus, ShellError> {
		let last_status = RshWaitStatus::new();
		if let NdType::FuncDef { name, body } = node.nd_type {
			self.shellenv.set_function(name, body)?;
			Ok(last_status)
		} else { unreachable!() }
	}

	/// For loops in bash can have multiple loop variables, e.g. `for a b c in 1 2 3 4 5 6`
	/// In this case, loop_vars are also iterated through, e.g. a = 1, b = 2, c = 3, etc
	/// Here, we use the modulo operator to figure out which variable to set on each iteration.
	fn handle_for(&mut self, node: Node, io: ProcIO) -> Result<RshWaitStatus, ShellError> {
		let mut last_status = RshWaitStatus::new();
		let body_io = ProcIO::from(None, io.stdout, io.stderr);
		let redirs = node.get_redirs()?;
		self.handle_redirs(redirs.into())?;

		if let NdType::For { loop_vars, mut loop_arr, loop_body } = node.nd_type {
			let var_count = loop_vars.len();
			let mut var_index = 0;
			let mut iteration_count = 0;

			let mut arr_buffer = VecDeque::new();
			loop_arr.map_rotate(|elem| {
				for token in expand::expand_token(self.shellenv, elem) {
					arr_buffer.push_back(token);
				}
			});
			loop_arr.extend(arr_buffer.drain(..));


			while !loop_arr.is_empty() {

				// Get the current value from the array
				let current_value = loop_arr.pop_front().unwrap().text().to_string();

				// Set the current variable to the current value
				let current_var = loop_vars[var_index].text().to_string();
				self.shellenv.set_variable(current_var, current_value);

				// Update the variable index for the next iteration
				iteration_count += 1;
				var_index = iteration_count % var_count;

				// Execute the body of the loop
				last_status = self.walk_root(*loop_body.clone(), None, body_io.try_clone())?;
			}
		}

		Ok(last_status)
	}

	fn handle_loop(&mut self, node: Node, io: ProcIO) -> Result<RshWaitStatus, ShellError> {
		let mut last_status = RshWaitStatus::new();
		let cond_io = ProcIO::from(io.stdin, None, None);
		let body_io = ProcIO::from(None, io.stdout, io.stderr);

		if let NdType::Loop { condition, logic } = node.nd_type {
			let cond = *logic.condition;
			let body = *logic.body;
			// TODO: keep an eye on this; cloning in the loop body may be too expensive
			loop {
				// Evaluate the loop condition
				let condition_status = self.walk_root(cond.clone(),Some(condition),cond_io.try_clone())?;

				match condition {
					true => {
						if !matches!(condition_status,RshWaitStatus::Success {..}) {
							break; // Exit for a `while` loop when condition is false
						}
					}
					false => {
						if matches!(condition_status,RshWaitStatus::Success {..}) {
							break; // Exit for an `until` loop when condition is true
						}
					}
				}

				// Execute the body of the loop
				last_status = self.walk_root(body.clone(),None,body_io.try_clone())?;

				// Check for break or continue signals
				// match last_status {
				// RshWaitStatus::Break => return Ok(RshWaitStatus::Break),
				// RshWaitStatus::Continue => continue,
				// _ => {}
				// }
			}
			Ok(last_status)
		} else {
			Err(ShellError::from_syntax(
					"Expected a loop node in handle_loop",
					node.span()
			))
		}
	}

	fn handle_if(&mut self, node: Node, io: ProcIO) -> Result<RshWaitStatus, ShellError> {
		let mut last_result = RshWaitStatus::new();
		let cond_io = ProcIO::from(io.stdin, None, None);
		let body_io = ProcIO::from(None, io.stdout, io.stderr);

		if let NdType::If { mut cond_blocks, else_block } = node.nd_type {
			while let Some(block) = cond_blocks.pop_front() {
				let cond = *block.condition;
				let body = *block.body;
				last_result = self.walk_root(cond,Some(false),cond_io.try_clone())?;
				if let RshWaitStatus::Success {..} = last_result {
					return self.walk_root(body,None,body_io)
				}
			}
			if let Some(block) = else_block {
				return self.walk_root(*block,None,body_io)
			}
		}
		Ok(last_result)
	}

	fn handle_chain(&mut self, node: Node) -> Result<RshWaitStatus, ShellError> {
		let mut last_status = RshWaitStatus::new();

		if let NdType::Chain { left, right, op } = node.nd_type {
			match self.walk(*left, ProcIO::new())? {
				RshWaitStatus::Success {..} => {
					if let NdType::And = op.nd_type {
						last_status = self.walk(*right, ProcIO::new())?;
					}
				}
				_ => {
					if let NdType::Or = op.nd_type {
						last_status = self.walk(*right, ProcIO::new())?;
					}
				}
			}

		}
		Ok(last_status)
	}


	fn handle_assignment(&mut self, node: Node) -> Result<RshWaitStatus,ShellError> {
		let span = node.span();
		if let NdType::Assignment { name, value } = node.nd_type {
			let value = value.unwrap_or_default();
			self.shellenv.set_variable(name, value);
		}
		Ok(RshWaitStatus::s(span))
	}

	fn handle_builtin(&mut self, mut node: Node, io: ProcIO) -> Result<RshWaitStatus,ShellError> {
		let argv = expand::expand_arguments(self.shellenv, &mut node)?;
		match argv.first().unwrap().text() {
			"echo" => echo(node, io),
			"set" => set_or_unset(self.shellenv, node, true),
			"unset" => set_or_unset(self.shellenv, node, false),
			"source" => source(self.shellenv, node),
			"cd" => cd(self.shellenv, node),
			"pwd" => pwd(self.shellenv, node.span()),
			"alias" => alias(self.shellenv, node),
			"export" => export(self.shellenv, node),
			"[" | "test" => test(node.get_argv()?.into()),
			"builtin" => {
				// This one allows you to safely wrap builtins in aliases/functions
				if let NdType::Builtin { mut argv } = node.nd_type {
					argv.pop_front();
					node.nd_type = NdType::Builtin { argv };
					self.handle_builtin(node, io)
				} else { unreachable!() }
			}
			_ => unimplemented!("found this builtin: {}",argv[0].text())
		}
	}



	/// Extracts arguments and redirections from an AST node.
	///
	/// This function processes an AST node of type `Command`, `Builtin`, or `Function`
	/// and extracts the arguments and redirections from it. The arguments are converted
	/// to `CString` and collected into a vector, while the redirections are collected
	/// into a `VecDeque`.
	///
	/// # Arguments
	///
	/// * `node` - The AST node to extract arguments and redirections from.
	///
	/// # Returns
	///
	/// A tuple containing:
	/// * A vector of `CString` representing the extracted arguments.
	/// * A `VecDeque` of `Tk` representing the extracted redirections.
	///
	/// # Panics
	///
	/// This function will panic if the provided `node` is not of type `Command`, `Builtin`,
	/// or `Function`.
	///
	/// # Example
	///
	/// let node = Node::Command { argv: vec![...].into_iter().collect() };
	/// let (args, redirs) = shell.extract_args(node);
	///
	/// # Notes
	///
	/// - The function uses `trace!` and `debug!` macros for logging purposes.
	/// - Variable substitutions (`$var`) are resolved using the `shellenv`'s `get_variable` method.
	/// - If a variable substitution is not found, an empty `CString` is added to the arguments.
	fn handle_redirs(&self, mut redirs: VecDeque<Node>) -> Result<VecDeque<SmartFd>, ShellError> {
		let mut fd_queue: VecDeque<SmartFd> = VecDeque::new();
		let mut fd_dupes: VecDeque<Redir> = VecDeque::new();

		while let Some(redir_tk) = redirs.pop_front() {
			if let NdType::Redirection { ref redir } = redir_tk.nd_type {
				let Redir { fd_source, op, fd_target, file_target } = &redir;
				if fd_target.is_some() {
					fd_dupes.push_back(redir.clone());
				} else if let Some(file_path) = file_target {
					let source_fd = SmartFd::new(*fd_source)?;
					let flags = match op {
						RedirType::Input => OFlag::O_RDONLY,
						RedirType::Output => OFlag::O_WRONLY | OFlag::O_CREAT | OFlag::O_TRUNC,
						RedirType::Append => OFlag::O_WRONLY | OFlag::O_CREAT | OFlag::O_APPEND,
						_ => unimplemented!("Heredocs and herestrings are not implemented yet."),
					};
					let mut file_fd: SmartFd = SmartFd::open(Path::new(file_path.text()), flags, Mode::from_bits(0o644).unwrap())?;
					info!("Duping file FD {} to FD {}", file_fd, fd_source);
					file_fd.dup2(&source_fd)?;
					file_fd.close()?;
					fd_queue.push_back(source_fd);
				}
			}
		}

		while let Some(dupe_redir) = fd_dupes.pop_front() {
			let Redir { fd_source, op: _, fd_target, file_target: _ } = dupe_redir;
			let mut target_fd = SmartFd::new(fd_target.unwrap())?;
			let source_fd = SmartFd::new(fd_source)?;
			target_fd.dup2(&source_fd)?;
			target_fd.close()?;
			fd_queue.push_back(source_fd);
		}

		Ok(fd_queue)
	}

	fn handle_subshell(&mut self, node: Node, mut io: ProcIO) -> Result<RshWaitStatus,ShellError> {
		let mut shellenv = self.shellenv.clone();
		shellenv.clear_pos_parameters();
		if let NdType::Subshell { mut body, argv } = node.nd_type {
			body = body[1..body.len() - 1].to_string();
			if !body.starts_with("#!") {
				let interpreter = std::env::current_exe().unwrap();
				let mut shebang = "#!".to_string();
				shebang.push_str(interpreter.to_str().unwrap());
				shebang.push('\n');
				shebang.push_str(&body);
				body = shebang;
			}
			// Step 1: Create a pipe
			let memfd = SmartFd::new_memfd("subshell", true)?;
			memfd.write(body.as_bytes())?;

			match unsafe { fork() } {
				Ok(ForkResult::Child) => {
					let fd_path = format!("/proc/self/fd/{}", memfd);
					let fd_path = CString::new(fd_path).unwrap();
					let env = shellenv.get_cvars();
					execve(&fd_path, &[fd_path.clone()], &env).unwrap();
					unreachable!();
				}
				Ok(ForkResult::Parent { child }) => {
					nix::sys::wait::waitpid(child, None).unwrap();
				}
				Err(_) => {}
			}

		} else { unreachable!() }
		Ok(RshWaitStatus::new())
	}

	fn handle_pipeline(&mut self, node: Node, io: ProcIO) -> Result<RshWaitStatus, ShellError> {
		let (left, right, both) = if let NdType::Pipeline { left, right, both } = &node.nd_type {
			(*left.clone(), *right.clone(), both)
		} else {
			unreachable!()
		};

		let (r_pipe,w_pipe) = SmartFd::pipe()?;

		let mut stderr_left = None;
		if *both { // If the operator is '|&'
			stderr_left = Some(Arc::new(Mutex::new(w_pipe.dup()?)));
		}

		// `left_io` works like this:
		// 1. takes stdin from the io arg, representing input from a previous command if any
		// 2. takes the write pipe as stdout
		// 3. takes the duped stderr fildesc if any
		let left_io = ProcIO::from(io.stdin,Some(Arc::new(Mutex::new(w_pipe))), stderr_left);
		// `right_io` works like this:
		// 1. takes the read pipe as stdin
		// 2. takes stdout from the io arg, representing the output to the next command if any
		// 3. takes stderr from the io arg, representing the error output the the next command if any
		let right_io = ProcIO::from(Some(Arc::new(Mutex::new(r_pipe))), io.stdout, io.stderr);
		// Walk left side of the pipeline
		let left_status = self.walk(left, left_io);

		// Walk right side of the pipeline
		let right_status = self.walk(right, right_io);

		// Return status of the last command
		left_status?;
		right_status
	}

	fn handle_function(&mut self, node: Node, io: ProcIO) -> Result<RshWaitStatus,ShellError> {
		let node_span = node.span();
		if let NdType::Command { mut argv } | NdType::Builtin { mut argv } = node.nd_type {
			let func_name = argv.pop_front().unwrap();
			let mut pos_params = vec![];
			while let Some(token) = argv.pop_front() {
				pos_params.push(token.text().to_string())
			}
			// Unwrap is safe here because we checked for Some() in self.walk()
			let mut func = self.shellenv.get_function(func_name.text()).unwrap();
			for redir in node.redirs {
				func.redirs.push_back(redir.clone());
			}
			let saved_parameters = self.shellenv.parameters.clone();
			self.shellenv.clear_pos_parameters();
			for (index,param) in pos_params.into_iter().enumerate() {
				self.shellenv.set_parameter((index + 1).to_string(), param);
			}
			let mut sub_walker = NodeWalker::new(*func.clone(), self.shellenv);

			// Returns exit status or shell error
			let mut result = sub_walker.walk_root(*func, None, io);
			if let Err(ref mut e) = result {
				// Use the span of the function call rather than the stuff inside the function
				*e = e.overwrite_span(node_span)
			}
			self.shellenv.parameters = saved_parameters;
			result
		} else { unreachable!() }
	}


	fn handle_command(&mut self, mut node: Node, mut io: ProcIO) -> Result<RshWaitStatus, ShellError> {
		let argv = expand::expand_arguments(self.shellenv, &mut node)?;
		let argv = argv
			.iter()
			.map(|arg| CString::new(arg.text()).unwrap())
			.collect::<Vec<CString>>();
		let redirs = node.get_redirs()?;
		let span = node.span();
		// Let's expand aliases here
		if let NdType::Command { ref argv } = node.nd_type {
			// If the shellenv is allowing aliases, and the token is not from an expanded alias
			if !argv.front().unwrap().flags().contains(WdFlags::FROM_ALIAS) {
				let node = expand::expand_alias(self.shellenv, node.clone())?;

				if !matches!(node.nd_type, NdType::Command {..}) {
					// If the resulting alias expansion does not return this node
					// then walk the resulting sub-tree
					return self.walk_root(node, None, io)
				}
			}
			if self.shellenv.shopts.get("autocd").is_some_and(|opt| *opt > 0) && argv.len() == 1 {
				let path_candidate = argv.front().unwrap();
				let is_relative_path = path_candidate.text().starts_with('.');
				let contains_slash = path_candidate.text().contains('/');
				let path_exists = Path::new(path_candidate.text()).is_dir();

				if (is_relative_path || contains_slash) && path_exists {
					let cd_token = Tk::new("cd".into(),span,path_candidate.flags());
					let mut autocd_argv = argv.clone();
					autocd_argv.push_front(cd_token);
					let autocd = Node {
						nd_type: NdType::Builtin { argv: autocd_argv },
						span,
						flags: node.flags,
						redirs: node.redirs
					};
					return self.walk(autocd, io)

				}
			}
		}
		io.backup_fildescs()?; // Save original stin, stdout, and stderr
		io.do_plumbing()?; // Route pipe logic using fildescs


		let cmd = Some(argv[0].clone().into_string().unwrap());
		let command = &argv[0];
		let envp = self.shellenv.get_cvars();

		match unsafe { fork() } {
			Ok(ForkResult::Child) => {
				// Handle redirections
				let mut open_fds: VecDeque<SmartFd> = VecDeque::new();
				if !redirs.is_empty() {
					open_fds.extend(self.handle_redirs(redirs.into())?);
				}

				// Execute the command
				let Err(_) = execvpe(command, &argv, &envp);
				std::process::exit(127);
			}
			Ok(ForkResult::Parent { child }) => {
				io.restore_fildescs()?;

				// Wait for the child process to finish
				match waitpid(child, None) {
					Ok(WaitStatus::Exited(_, code)) => match code {
						0 => Ok(RshWaitStatus::Success { span }),
						_ => Ok(RshWaitStatus::Fail { code, cmd, span }),
					},
					Ok(WaitStatus::Signaled(_,signal,_)) => {
						Ok(RshWaitStatus::Fail { code: 128 + signal as i32, cmd, span })
					}
					Ok(_) => Err(ShellError::from_execf("Unexpected waitpid result", 1, span)),
					Err(err) => Err(ShellError::from_execf(&format!("Waitpid failed: {}", err), 1, span)),
				}
			}
			Err(_) => Err(ShellError::from_execf("Fork failed", 1, span)),
		}
	}
}

#[cfg(test)]
mod tests {
	// Probably need to re-write this later
	// These handle basic uses but cap_output isn't a strong enough implementation to handle complex stuff it seems
	use super::*;
	use std::process;
	use std::fs::File;
	use std::io::Read;
	use libc::{fork, waitpid, WIFEXITED, WEXITSTATUS, c_int};

	fn cap_output(input: &str) -> String {
		// Your existing cap_output logic here
		let mut io = ProcIO::new();
		io.backup_fildescs();
		let (r_pipe, w_pipe) = SmartFd::pipe().unwrap();
		let mut output = vec![];
		io.stdout = Some(w_pipe.mk_shared());
		let mut output_file = unsafe { File::from_raw_fd(r_pipe.as_raw_fd()) };

		let shellenv = ShellEnv::new(EnvFlags::NO_RC | EnvFlags::CLEAN);
		let mut shellenv2 = ShellEnv::new(EnvFlags::NO_RC | EnvFlags::CLEAN);
		let state = parse::descend(input, &shellenv).unwrap();
		let mut walker = NodeWalker::new(state.ast.clone(), &mut shellenv2);

		walker.walk_root(state.ast, None, io.try_clone()).unwrap();

		if let Some(w_pipe) = io.stdout {
			let mut w_fd = w_pipe.lock().unwrap();
			if w_fd.is_valid() {
				w_fd.close().unwrap();
			}
		}

		output_file.read_to_end(&mut output).unwrap();
		String::from_utf8_lossy(&output).to_string()
	}

	fn fork_test<F: FnOnce() -> bool>(test_fn: F) -> bool {
		unsafe {
			match fork() {
				-1 => panic!("Failed to fork"),
				0 => {
					// Child process
					if test_fn() {
						process::exit(0)
					} else {
						process::exit(1)
					}
				}
				pid => {
					// Parent process
					let mut status: c_int = 0;
					waitpid(pid, &mut status, 0);
					WIFEXITED(status) && WEXITSTATUS(status) == 0
				}
			}
		}
	}

	#[test]
	fn test_command() {
		assert!(fork_test(|| {
			let input = "echo hello world";
			let output = cap_output(input);
			output.trim() == "hello world"
		}));
	}

	#[test]
	fn test_builtin_pipeline() {
		assert!(fork_test(|| {
			let input = "echo hello world | sed 's/hello/goodbye/'";
			let output = cap_output(input);
			output.trim() == "goodbye world"
		}));
	}

	#[test]
	fn test_pipeline() {
		assert!(fork_test(|| {
			let input = "echo hello world | cat | sed 's/hello/goodbye/'";
			let output = cap_output(input);
			output.trim() == "goodbye world"
		}));
	}

	#[test]
	fn test_sub() {
		assert!(fork_test(|| {
			let input = "name=world; echo hello $name";
			let output = cap_output(input);
			output.trim() == "hello world"
		}));
	}

	#[test]
	fn test_if() {
		assert!(fork_test(|| {
			let input = "if true; then echo this should print; fi; if false; then echo this should not print; fi";
			let output = cap_output(input);
			output.trim() == "this should print"
		}));
	}

	#[test]
	fn test_for() {
		assert!(fork_test(|| {
			let input = "for i in 1 2 3; do echo hello; done";
			let output = cap_output(input);
			output.trim() == "hello\nhello\nhello"
		}));
	}
}
