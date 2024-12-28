use libc::{c_int, pipe};
use nix::sys::signal;
use nix::unistd::{close, dup, dup2, execvpe, fork, ForkResult};
use nix::fcntl::{open,OFlag};
use nix::sys::stat::Mode;
use nix::sys::wait::{waitpid, WaitStatus};
use std::os::fd::RawFd;
use std::ffi::CString;
use std::collections::{HashMap, VecDeque};
use std::path::Path;
use log::{info,debug,trace};
use glob::MatchOptions;

use crate::builtin::{alias, cd, echo, export, pwd, set_or_unset, source, test};
use crate::event::ShellError;
use crate::interp::{expand, parse};
use crate::interp::token::{Redir, RedirType, Tk, WdFlags};
use crate::interp::parse::{NdType,Node, Span};
use crate::shellenv::{EnvFlags, ShellEnv};

pub const GLOB_OPTS: MatchOptions = MatchOptions {
	case_sensitive: false,
	require_literal_separator: true,
	require_literal_leading_dot: false
};

#[derive(Clone,Debug)]
pub struct ProcIO {
	pub stdin: Option<RawFd>,
	pub stdout: Option<RawFd>,
	pub stderr: Option<RawFd>,
	pub backup: HashMap<RawFd,RawFd>
}

impl ProcIO {
	pub fn new() -> Self {
		Self { stdin: None, stdout: None, stderr: None, backup: HashMap::new() }
	}
	pub fn from(stdin: Option<RawFd>, stdout: Option<RawFd>, stderr: Option<RawFd>) -> Self {
		Self { stdin, stdout, stderr, backup: HashMap::new() }
	}
	pub fn backup_fildescs(&mut self) -> Result<(), ShellError> {
		let mut backup = HashMap::new();
		// Get duped file descriptors
		let dup_in = dup(0).map_err(|e| ShellError::from_io(&e.to_string(), Span::new()))?;
		let dup_out = dup(1).map_err(|e| ShellError::from_io(&e.to_string(), Span::new()))?;
		let dup_err = dup(2).map_err(|e| ShellError::from_io(&e.to_string(), Span::new()))?;
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
			if let Some(saved_in) = self.backup.get(&0) {
				dup2(*saved_in, 0).map_err(|e| ShellError::from_io(&e.to_string(), Span::new()))?;
				close(*saved_in).unwrap();
				self.backup.remove(&0);
			}
			if let Some(saved_out) = self.backup.get(&1) {
				dup2(*saved_out, 1).map_err(|e| ShellError::from_io(&e.to_string(), Span::new()))?;
				close(*saved_out).unwrap();
				self.backup.remove(&1);
			}
			if let Some(saved_err) = self.backup.get(&2) {
				dup2(*saved_err, 2).map_err(|e| ShellError::from_io(&e.to_string(), Span::new()))?;
				close(*saved_err).unwrap();
				self.backup.remove(&2);
			}
		}
		Ok(())
	}
	pub fn do_plumbing(&self) -> Result<(), ShellError> {
		if let Some(err_pipe) = self.stderr {
			if let Err(err) = dup2(err_pipe, 2) {
				eprintln!("Failed to duplicate stderr: {}", err);
				std::process::exit(1);
			}
			close(err_pipe)
				.map_err(|e| ShellError::from_io(format!("failed to close error pipe: {}",e).as_str(), Span::new()))?;
				}
		// Redirect stdout
		if let Some(w_pipe) = self.stdout {
			if let Err(err) = dup2(w_pipe, 1) {
				eprintln!("Failed to duplicate stdout: {}", err);
				std::process::exit(1);
			}
			close(w_pipe)
				.map_err(|e| ShellError::from_io(format!("failed to close error pipe: {}",e).as_str(), Span::new()))?;
				}

		// Redirect stdin
		if let Some(r_pipe) = self.stdin {
			if let Err(err) = dup2(r_pipe, 0) {
				eprintln!("Failed to duplicate stdin: {}", err);
				std::process::exit(1);
			}
			close(r_pipe)
				.map_err(|e| ShellError::from_io(format!("failed to close error pipe: {}",e).as_str(), Span::new()))?;
				}
		Ok(())
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
		let mut exit_status = RshWaitStatus::new();
		let mut nodes;
		if let NdType::Root { ref mut deck } = self.ast.nd_type {
			nodes = std::mem::take(deck);
		} else { unreachable!() }
		while let Some(node) = nodes.pop_front() {
			exit_status = self.walk(node, ProcIO::new())?;
		}
		Ok(exit_status)
	}

	/// This function will walk through the Node tree and direct nodes to the proper functions
	///
	/// # Inputs:
	/// stdin: An optional arg containing a raw file descriptor. This file descriptor is used as an
	/// address to tell the kernel where to look for piped standard input, if any.
	///
	/// stdout: Another optional arg containing a raw fd. This fd tells the kernel where to write
	/// stdout to.
	///
	/// # Outputs: An RshWaitStatus or a shell error.
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
				last_status = self.walk(node, io.clone())?;
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
				last_status = self.walk_root(*loop_body.clone(), None, body_io.clone())?;
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
				let condition_status = self.walk_root(cond.clone(),Some(condition),cond_io.clone())?;

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
				last_status = self.walk_root(body.clone(),None,body_io.clone())?;

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
				last_result = self.walk_root(cond,Some(false),cond_io.clone())?;
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
	///
	fn extract_args(&mut self, argv: Vec<Tk>) -> Vec<CString> {
		let mut args = Vec::new();
		trace!("Extracting arguments from: {:?}", argv);
		for word in argv {
			debug!("checking word: {}",word.text());
			let tokens = expand::expand_token(self.shellenv, word);
			debug!("got expanded tokens: {:?}",tokens);
			for token in tokens {
				if !token.text().is_empty() {
					let cstring = CString::new(token.text()).unwrap();
					args.push(cstring);
				}
			}
		}
		args
	}

	fn handle_redirs(&self, mut redirs: VecDeque<Node>) -> Result<(), ShellError> {
		let mut fd_queue: VecDeque<i32> = VecDeque::new();
		debug!("Handling redirections: {:?}", redirs);

		while let Some(redir) = redirs.pop_front() {
			if let NdType::Redirection { redir } = redir.nd_type {
				let Redir { fd_source, op, fd_target, file_target } = redir;
				if let Some(target) = fd_target {
					dup2(target, fd_source).unwrap();
					fd_queue.push_back(target);
				} else if let Some(file_path) = file_target {
					info!("Opening file for redirection: {:?}", file_path);
					let flags = match op {
						RedirType::Input => OFlag::O_RDONLY,
						RedirType::Output => OFlag::O_WRONLY | OFlag::O_CREAT | OFlag::O_TRUNC,
						RedirType::Append => OFlag::O_WRONLY | OFlag::O_CREAT | OFlag::O_APPEND,
						_ => unimplemented!("Heredocs and herestrings are not implemented yet."),
					};
					let file_fd: RawFd = open(file_path.text(), flags, Mode::from_bits(0o644).unwrap()).unwrap();
					info!("Duping file FD {} to FD {}", file_fd, fd_source);
					dup2(file_fd, fd_source).unwrap();
					fd_queue.push_back(file_fd);
				}
			}
		}

		debug!("Closing FDs: {:?}", fd_queue);
		while let Some(fd) = fd_queue.pop_front() {
			let _ = close(fd);
		}
		Ok(())
	}

	fn handle_subshell(&mut self, node: Node, io: ProcIO) -> Result<RshWaitStatus,ShellError> {
		todo!()
	}

	fn handle_pipeline(&mut self, node: Node, io: ProcIO) -> Result<RshWaitStatus, ShellError> {
		let (left, right, both) = if let NdType::Pipeline { left, right, both } = &node.nd_type {
			(*left.clone(), *right.clone(), both)
		} else {
			unreachable!()
		};

		let mut fds: [c_int; 2] = [0; 2];
		let result = unsafe { pipe(fds.as_mut_ptr()) };
		if result != 0 {
			return Err(ShellError::from_io(&std::io::Error::last_os_error().to_string(), node.span()));
		}

		let r_pipe = fds[0];
		let w_pipe = fds[1];

		let mut stderr_left = None;
		if *both { // If the operator is '|&'
			stderr_left = Some(dup(w_pipe).unwrap());
		}

		// `left_io` works like this:
		// 1. takes stdin from the io arg, representing input from a previous command if any
		// 2. takes the write pipe as stdout
		// 3. takes the duped stderr fildesc if any
		let left_io = ProcIO::from(io.stdin,Some(w_pipe), stderr_left);
		// `right_io` works like this:
		// 1. takes the read pipe as stdin
		// 2. takes stdout from the io arg, representing the output to the next command if any
		// 3. takes stderr from the io arg, representing the error output the the next command if any
		let right_io = ProcIO::from(Some(r_pipe), io.stdout, io.stderr);
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


	fn handle_command(&mut self, node: Node, mut io: ProcIO) -> Result<RshWaitStatus, ShellError> {
		let argv = self.extract_args(node.get_argv()?);
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
				if (path_candidate.text().starts_with('.') || path_candidate.text().contains('/')) && Path::new(path_candidate.text()).is_dir() {
					let cd_token = Tk::new("cd".into(),span,path_candidate.flags());
					let mut autocd_argv = argv.clone();
					autocd_argv.push_front(cd_token);
					let autocd = Node {
						nd_type: NdType::Builtin { argv: autocd_argv },
						span,
						flags: node.flags,
						redirs: node.redirs
					};
					return self.handle_builtin(autocd, io)
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
				if !redirs.is_empty() {
					self.handle_redirs(redirs.into())?;
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
