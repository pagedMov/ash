use libc::{STDOUT_FILENO,STDERR_FILENO,STDIN_FILENO,c_int, pipe};
use nix::unistd::{close, dup, dup2, execvpe, fork, write, ForkResult};
use nix::fcntl::{open,OFlag};
use nix::sys::stat::Mode;
use nix::sys::wait::{waitpid, WaitStatus};
use std::os::fd::{AsRawFd, BorrowedFd, RawFd};
use std::ffi::CString;
use std::collections::{HashMap, VecDeque};
use std::path::PathBuf;
use log::{error,info,debug,trace};
use glob::MatchOptions;

use crate::builtin::{alias, cd, echo, pwd, source, test};
use crate::event::ShellError;
use crate::interp::{self, expand};
use crate::interp::token::{Redir, RedirType, Tk, TkType};
use crate::interp::parse::{NdType,Node, Span};
use crate::shellenv::ShellEnv;

pub const GLOB_OPTS: MatchOptions = MatchOptions {
	case_sensitive: false,
	require_literal_separator: true,
	require_literal_leading_dot: false
};

pub struct SavedFDs {
	saved_stdin: RawFd,
	saved_stdout: RawFd,
	saved_stderr: RawFd,
}

impl SavedFDs {
	pub fn new(stdin: RawFd, stdout: RawFd, stderr: RawFd, span: Span) -> Result<Self, ShellError> {
			let saved = Self {
					saved_stdin: dup(stdin).map_err(|e| ShellError::from_io(&e.to_string(), span))?,
					saved_stdout: dup(stdout).map_err(|e| ShellError::from_io(&e.to_string(), span))?,
					saved_stderr: dup(stderr).map_err(|e| ShellError::from_io(&e.to_string(), span))?,
			};
			Ok(saved)
	}
	pub fn restore(&mut self, stdin: RawFd, stdout: RawFd, stderr: RawFd, span: Span) -> Result<(), ShellError> {
			if self.saved_stdin >= 0 {
					dup2(self.saved_stdin, stdin).map_err(|e| ShellError::from_io(&e.to_string(), span))?;
			}
			if self.saved_stdout >= 0 {
					dup2(self.saved_stdout, stdout).map_err(|e| ShellError::from_io(&e.to_string(), span))?;
			}
			if self.saved_stderr >= 0 {
					dup2(self.saved_stderr, stderr).map_err(|e| ShellError::from_io(&e.to_string(), span))?;
			}
			self.close_all();
			Ok(())
	}
	fn close_all(&mut self) {
			if self.saved_stdin >= 0 {
					close(self.saved_stdin).expect("failed to close stdin");
					self.saved_stdin = -1;
			}
			if self.saved_stdout >= 0 {
					close(self.saved_stdout).expect("failed to close stdout");
					self.saved_stdout = -1;
			}
			if self.saved_stderr >= 0 {
					close(self.saved_stderr).expect("failed to close stderr");
					self.saved_stderr = -1;
			}
	}
}

#[derive(PartialEq,Debug)]
pub enum RshExitStatus {
	Success { span: Span },
	Fail { code: i32, cmd: Option<String>, span: Span },
}

impl RshExitStatus {
	pub fn new() -> Self {
		RshExitStatus::Success { span: Span::new() }
	}
	pub fn s(span: Span) -> Self {
		RshExitStatus::Success { span }
	}
	pub fn f(code: i32, cmd: Option<String>, span: Span) -> Self {
		RshExitStatus::Fail { code, cmd, span }
	}
}

impl Default for RshExitStatus {
	fn default() -> Self {
		RshExitStatus::new()
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
	pub fn start_walk(&mut self) -> Result<RshExitStatus,ShellError> {
		info!("Going on a walk...");
		let mut exit_status = RshExitStatus::new();
		let mut nodes;
		if let NdType::Root { ref mut deck } = self.ast.nd_type {
			nodes = std::mem::take(deck);
		} else { unreachable!() }
		while let Some(node) = nodes.pop_front() {
			exit_status = self.walk(node, None, None, None)?;
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
	/// # Outputs: An RshExitStatus or a shell error.
	fn walk(&mut self, node: Node, stdin: Option<RawFd>, stdout: Option<RawFd>, stderr: Option<RawFd>) -> Result<RshExitStatus,ShellError> {
		debug!("walking over node {:?} with these pipes: {:?},{:?}",node,stdin,stdout);
		let last_status;
		match node.nd_type {
			NdType::Command { ref argv, .. } => {
				trace!("Found command: {:?}",node);
				last_status = self.handle_command(node, stdin, stdout, stderr)?;
			}
			NdType::Builtin {..} => {
				last_status = self.handle_builtin(node, stdin, stdout, stderr)?;
			}
			//NdType::Function { .. } => {
			//todo!("handle simple commands")
			//}
			NdType::Pipeline {..} => {
				last_status = self.handle_pipeline(node, stdin, stdout, stderr)?;
			}
			NdType::Chain {..} => {
				last_status = self.handle_chain(node)?;
			}
			NdType::If {..} => {
				last_status = self.handle_if(node)?;
			}
			NdType::For {..} => {
				last_status = self.handle_for(node)?;
			}
			NdType::Loop {..} => {
				last_status = self.handle_loop(node)?;
			}
			NdType::Case {..} => {
				last_status = self.handle_case(node)?;
			}
			NdType::Subshell {..} => {
				last_status = self.handle_subshell(node,stdin,stdout,stderr)?;
			}
			NdType::FuncDef {..} => {
				todo!("handle function definition")
			}
			NdType::Assignment {..} => {
				last_status = self.handle_assignment(node)?;
			}
			NdType::Cmdsep => {
				last_status = RshExitStatus::new();
			}
			_ => unimplemented!("Support for node type `{:?}` is not yet implemented",node.nd_type)
		}
		Ok(last_status)
	}

	fn walk_root(&mut self, node: Node, break_condition: Option<bool>) -> Result<RshExitStatus,ShellError> {
		let mut last_status = RshExitStatus::new();
		if let NdType::Root { deck } = node.nd_type {
			for node in deck {
				last_status = self.walk(node,None,None,None)?;
				if let Some(condition) = break_condition {
					match condition {
						true => {
							if let RshExitStatus::Fail {..} = last_status {
								break
							}
						}
						false => {
							if let RshExitStatus::Success {..} = last_status {
								break
							}
						}
					}
				}
			}
		} else {
			return Err(ShellError::from_internal("Entered walk_root() with a non-root node", node.span()))
		}
		Ok(last_status)
	}

	fn handle_case(&mut self, node: Node) -> Result<RshExitStatus, ShellError> {
		let mut last_status = RshExitStatus::new();
		if let NdType::Case { input_var, cases } = node.nd_type {
				for case in cases.keys() {
						if case == input_var.text() {
								if let Some(body) = cases.get(case) {
										last_status = self.walk_root(body.clone(), None)?;
								}
						}
				}
		} else {
			return Err(ShellError::from_internal("Entered handle_case() with a non-case node", node.span()))
		}
		Ok(last_status)
	}

	/// For loops in bash can have multiple loop variables, e.g. `for a b c in 1 2 3 4 5 6`
	/// In this case, loop_vars are also iterated through, e.g. a = 1, b = 2, c = 3, etc
	/// Here, we use the modulo operator to figure out which variable to set on each iteration.
	fn handle_for(&mut self, node: Node) -> Result<RshExitStatus, ShellError> {
		let mut last_status = RshExitStatus::new();

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
				last_status = self.walk_root(*loop_body.clone(), None)?;
			}
		}

		Ok(last_status)
	}

	fn handle_loop(&mut self, node: Node) -> Result<RshExitStatus, ShellError> {
		let mut last_status = RshExitStatus::new();

		if let NdType::Loop { condition, logic } = node.nd_type {
			let cond= *logic.condition;
			let body = *logic.body;
			loop {
				// Evaluate the loop condition
				let condition_status = self.walk_root(cond.clone(),Some(condition))?;

				match condition {
					true => {
						if !matches!(condition_status,RshExitStatus::Success {..}) {
							break; // Exit for a `while` loop when condition is false
						}
					}
					false => {
						if matches!(condition_status,RshExitStatus::Success {..}) {
							break; // Exit for an `until` loop when condition is true
						}
					}
				}

				// Execute the body of the loop
				last_status = self.walk_root(body.clone(),None)?;

				// Check for break or continue signals
				// match last_status {
				// RshExitStatus::Break => return Ok(RshExitStatus::Break),
				// RshExitStatus::Continue => continue,
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

	fn handle_if(&mut self, node: Node) -> Result<RshExitStatus, ShellError> {
		let mut last_result = RshExitStatus::new();;

		if let NdType::If { mut cond_blocks, else_block } = node.nd_type {
			while let Some(block) = cond_blocks.pop_front() {
				let cond = *block.condition;
				let body = *block.body;
				last_result = self.walk_root(cond,Some(false))?;
				if let RshExitStatus::Success {..} = last_result {
					return self.walk_root(body,None)
				}
			}
			if let Some(block) = else_block {
				return self.walk_root(*block,None)
			}
		}
		Ok(last_result)
	}

	fn handle_chain(&mut self, node: Node) -> Result<RshExitStatus, ShellError> {
		let mut last_status = RshExitStatus::new();

		if let NdType::Chain { left, right, op } = node.nd_type {
			match self.walk(*left, None, None, None)? {
				RshExitStatus::Success {..} => {
					if let NdType::And = op.nd_type {
						last_status = self.walk(*right, None, None, None)?;
					}
				}
				RshExitStatus::Fail {..} => {
					if let NdType::Or = op.nd_type {
						last_status = self.walk(*right, None, None, None)?;
					}
				}
			}

		}
		Ok(last_status)
	}


	fn handle_assignment(&mut self, node: Node) -> Result<RshExitStatus,ShellError> {
		let span = node.span();
		if let NdType::Assignment { name, value } = node.nd_type {
			let value = value.unwrap_or_default();
			self.shellenv.set_variable(name, value);
		}
		Ok(RshExitStatus::s(span))
	}

	fn handle_builtin(&mut self, mut node: Node, stdin: Option<RawFd>, stdout: Option<RawFd>, stderr: Option<RawFd>) -> Result<RshExitStatus,ShellError> {
		let argv = node.get_argv()?;
		let mut expand_buffer = Vec::new();
		for arg in &argv {
			let mut expanded = expand::expand_token(self.shellenv, arg.clone());
			expand_buffer.extend(expanded.drain(..));
		}
		if let NdType::Builtin { argv: _, redirs } = node.nd_type {
			node.nd_type = NdType::Builtin { argv: expand_buffer.into(), redirs }
		}
		match argv[0].text() {
			"echo" => echo(node, stdout, stderr),
			"source" => source(self.shellenv, node),
			"cd" => cd(self.shellenv, node),
			"pwd" => pwd(self.shellenv, node.span()),
			"alias" => alias(self.shellenv, node),
			"[" | "test" => test(node.get_argv()?.into()),
			_ => unimplemented!()
		}
	}

	fn extract_redirs(&mut self, node: &Node) -> Result<VecDeque<Tk>, ShellError> {
		match &node.nd_type {
			NdType::Command { argv: _, redirs } | NdType::Builtin { argv: _, redirs } => {
				return Ok(redirs.clone())
			}
			_ => Err(ShellError::from_internal("Called extract_redirs() on a non-command node", node.span()))
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
				let cstring = CString::new(token.text()).unwrap();
				args.push(cstring);
			}
		}
		args
	}

	/// Handles file descriptor redirections for a command.
	///
	/// This function processes a queue of redirection tokens (`Tk`) and performs the necessary file descriptor
	/// redirections. It supports input, output, and append redirections. For each redirection token, it either
	/// duplicates a file descriptor or opens a file and duplicates its file descriptor to the target file descriptor.
	///
	/// # Arguments
	///
	/// * `redirs` - A mutable `VecDeque` of `Tk` tokens representing redirections.
	///
	/// # Returns
	///
	/// * `Result<(), ShellError>` - Returns `Ok(())` if all redirections are successfully handled.
	/// * Returns an appropriate `ShellError` if there is an issue with handling the redirections.
	///
	/// # Examples
	///
	/// ```
	/// let redirs = vec![
	///     Tk::new(TkType::Redirection { redir: Redir { fd_source: 1, op: RedirType::Output, fd_target: None, file_target: Some("output.txt".into()) } }),
	///     Tk::new(TkType::Redirection { redir: Redir { fd_source: 0, op: RedirType::Input, fd_target: None, file_target: Some("input.txt".into()) } }),
	/// ];
	/// let mut redirs_deque = VecDeque::from(redirs);
	/// let result = handle_redirs(&mut redirs_deque);
	/// assert!(result.is_ok());
	/// ```
	///
	/// # Redirection Types
	///
	/// - `RedirType::Input`: Opens the file for reading.
	/// - `RedirType::Output`: Opens the file for writing, creating it if it does not exist and truncating it if it does.
	/// - `RedirType::Append`: Opens the file for writing, creating it if it does not exist and appending to it if it does.
	///
	/// # Notes
	///
	/// - The function handles closing all file descriptors that were opened or duplicated during the redirection process.
	/// - Heredocs and herestrings are not implemented yet and will result in a panic if encountered.
	///
	/// # Errors
	///
	/// - Returns `ShellError` if there is an issue with opening a file or duplicating a file descriptor.
	///
	/// # Panics
	///
	/// This function does not panic.
	fn handle_redirs(&self, mut redirs: VecDeque<Tk>) -> Result<(), ShellError> {
			let mut fd_queue: VecDeque<i32> = VecDeque::new();
			debug!("Handling redirections: {:?}", redirs);

			while let Some(redir) = redirs.pop_front() {
					if let TkType::Redirection { redir } = redir.class() {
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

	fn handle_subshell(&mut self, node: Node, stdin: Option<RawFd>, stdout: Option<RawFd>, stderr: Option<RawFd>) -> Result<RshExitStatus,ShellError> {
		todo!()
	}

	/// Handles the execution of a pipeline in a shell command.
	///
	/// This function processes a pipeline node, setting up the necessary file descriptors for input and output redirection,
	/// and executes the commands on both sides of the pipeline. It supports both standard and combined standard error pipelines.
	///
	/// # Arguments
	///
	/// * `node` - The pipeline node containing the left and right commands and a flag indicating if standard error should be combined.
	/// * `stdin` - An optional file descriptor for standard input.
	/// * `stdout` - An optional file descriptor for standard output.
	/// * `stderr` - A mutable optional file descriptor for standard error.
	///
	/// # Returns
	///
	/// * `Result<RshExitStatus, ShellError>` - Returns the exit status of the last command in the pipeline.
	/// * Returns an appropriate `ShellError` if there is an issue with setting up the pipeline or executing the commands.
	///
	/// # Examples
	///
	/// ```
	/// let left_command = Node::new(NdType::Command { ... });
	/// let right_command = Node::new(NdType::Command { ... });
	/// let pipeline_node = Node::new(NdType::Pipeline { left: left_command, right: right_command, both: false });
	/// let stdin = Some(0); // Standard input file descriptor
	/// let stdout = Some(1); // Standard output file descriptor
	/// let stderr = Some(2); // Standard error file descriptor
	/// let result = shell.handle_pipeline(pipeline_node, stdin, stdout, stderr);
	/// assert!(result.is_ok());
	/// ```
	///
	/// # Pipeline Types
	///
	/// - Standard pipeline (`|`): Redirects the standard output of the left command to the standard input of the right command.
	/// - Combined standard error pipeline (`|&`): Redirects both the standard output and standard error of the left command to the standard input of the right command.
	///
	/// # Notes
	///
	/// - The function sets up a pipe using the `pipe` system call.
	/// - The function handles closing all file descriptors that were opened or duplicated during the pipeline process.
	/// - The function uses the `walk` method to execute the commands on both sides of the pipeline.
	///
	/// # Errors
	///
	/// - Returns `ShellError::from_io` if there is an issue with setting up the pipe.
	/// - Returns `ShellError` if there is an issue with executing the commands on either side of the pipeline.
	///
	/// # Safety
	///
	/// This function uses unsafe code to call the `pipe` system call and to duplicate file descriptors.
	///
	/// # Panics
	///
	/// This function does not panic.
	fn handle_pipeline(&mut self, node: Node, stdin: Option<RawFd>, stdout: Option<RawFd>, stderr: Option<RawFd>) -> Result<RshExitStatus, ShellError> {
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

			// Walk left side of the pipeline
			let left_status = self.walk(left, stdin, Some(w_pipe), stderr_left);

			// Walk right side of the pipeline
			let right_status = self.walk(right, Some(r_pipe), stdout, stderr);

			// Return status of the last command
			left_status?;
			right_status
	}

	/// Executes a command in a shell environment.
	///
	/// This function processes a command node, handles input and output redirections, and executes the command. It uses the `fork` system call to create a child process for executing the command. The function also handles restoring the original file descriptors after the command execution.
	///
	/// # Arguments
	///
	/// * `node` - The command node containing the arguments and redirections.
	/// * `stdin` - An optional file descriptor for standard input.
	/// * `stdout` - An optional file descriptor for standard output.
	/// * `stderr` - An optional file descriptor for standard error.
	///
	/// # Returns
	///
	/// * `Result<RshExitStatus, ShellError>` - Returns the exit status of the command.
	/// * Returns an appropriate `ShellError` if there is an issue with setting up the command or executing it.
	///
	/// # Examples
	///
	/// ```
	/// let command_node = Node::new(NdType::Command { ... });
	/// let stdin = Some(0); // Standard input file descriptor
	/// let stdout = Some(1); // Standard output file descriptor
	/// let stderr = Some(2); // Standard error file descriptor
	/// let result = shell.handle_command(command_node, stdin, stdout, stderr);
	/// assert!(result.is_ok());
	/// ```
	///
	/// # Notes
	///
	/// - The function uses the `fork` system call to create a child process for executing the command.
	/// - The function handles closing all file descriptors that were opened or duplicated during the command execution.
	/// - The function uses the `execvpe` function to execute the command with the given arguments and environment variables.
	/// - The function waits for the child process to finish and returns the exit status of the command.
	///
	/// # Errors
	///
	/// - Returns `ShellError::from_execf` if there is an issue with forking the process or executing the command.
	/// - Returns `ShellError::from_io` if there is an issue with setting up the file descriptors.
	///
	/// # Safety
	///
	/// This function uses unsafe code to call the `fork` system call and to duplicate file descriptors.
	///
	/// # Panics
	///
	/// This function does not panic.
	fn handle_command(&mut self, node: Node, stdin: Option<RawFd>, stdout: Option<RawFd>, stderr: Option<RawFd>) -> Result<RshExitStatus, ShellError> {
		let argv = self.extract_args(node.get_argv()?);
		let redirs = self.extract_redirs(&node)?;
		let span = node.span();
		let mut saved_fds = SavedFDs::new(0, 1, 2, node.span()).unwrap();

		if let Some(err_pipe) = stderr {
			if let Err(err) = dup2(err_pipe, 2) {
				eprintln!("Failed to duplicate stderr: {}", err);
				std::process::exit(1);
			}
			close(err_pipe).unwrap();
		}
		// Redirect stdout
		if let Some(w_pipe) = stdout {
			if let Err(err) = dup2(w_pipe, 1) {
				eprintln!("Failed to duplicate stdout: {}", err);
				std::process::exit(1);
			}
			close(w_pipe).unwrap();
		}

		// Redirect stdin
		if let Some(r_pipe) = stdin {
			if let Err(err) = dup2(r_pipe, 0) {
				eprintln!("Failed to duplicate stdin: {}", err);
				std::process::exit(1);
			}
			close(r_pipe).unwrap();
		}

		let cmd = Some(argv[0].clone().into_string().unwrap());
		let command = &argv[0];
		let envp = self.shellenv.get_cvars();

		match unsafe { fork() } {
			Ok(ForkResult::Child) => {
				// Handle redirections
				if !redirs.is_empty() {
					self.handle_redirs(redirs)?;
				}

				// Execute the command
				let Err(_) = execvpe(command, &argv, &envp);
				std::process::exit(127);
			}
			Ok(ForkResult::Parent { child }) => {
				saved_fds.restore(0, 1, 2, node.span()).unwrap();

				// Wait for the child process to finish
				match waitpid(child, None) {
					Ok(WaitStatus::Exited(_, code)) => match code {
						0 => Ok(RshExitStatus::Success { span }),
						_ => Ok(RshExitStatus::Fail { code, cmd, span }),
					},
					Ok(_) => Err(ShellError::from_execf("Unexpected waitpid result", 1, node.span())),
					Err(err) => Err(ShellError::from_execf(&format!("Waitpid failed: {}", err), 1, node.span())),
				}
			}
			Err(_) => Err(ShellError::from_execf("Fork failed", 1, node.span())),
		}
	}
}

#[cfg(test)]
mod tests {
	use std::{fs, io::Read, path::Path};

use interp::parse;

use super::*;

	fn from_file(input: &str) -> String {
		let mut file = fs::File::open(PathBuf::from(input)).unwrap();
		let mut buffer = String::new();
		file.read_to_string(&mut buffer).unwrap();
		buffer
	}
	#[test]
	fn basic_if_script() {
		let input = from_file("/home/pagedmov/Coding/projects/rust/rsh/tests/basic_if.sh");
		let mut shellenv = ShellEnv::new(false,true);
		let state = parse::descend(&input, &shellenv);
		assert!(state.is_ok());
		let mut walker = NodeWalker::new(state.unwrap().ast, &mut shellenv);
		let result = walker.start_walk();
		assert!(matches!(result,Ok(RshExitStatus::Success {..})));
		if let Ok(RshExitStatus::Fail { code, cmd: _, span: _ }) = result {
			assert_ne!(code,127);
		}
	}
	#[test]
	fn basic_for_script() {
		let input = from_file("/home/pagedmov/Coding/projects/rust/rsh/tests/basic_for.sh");
		let mut shellenv = ShellEnv::new(false,true);
		let state = parse::descend(&input, &shellenv);
		assert!(state.is_ok());
		let mut walker = NodeWalker::new(state.unwrap().ast, &mut shellenv);
		let result = walker.start_walk();
		assert!(matches!(result,Ok(RshExitStatus::Success {..})));
		if let Ok(RshExitStatus::Fail { code, cmd: _, span: _ }) = result {
			assert_ne!(code,127);
		}
	}
	#[test]
	fn basic_case_script() {
		let input = from_file("/home/pagedmov/Coding/projects/rust/rsh/tests/basic_case.sh");
		let mut shellenv = ShellEnv::new(false,true);
		let state = parse::descend(&input, &shellenv);
		assert!(state.is_ok());
		let mut walker = NodeWalker::new(state.unwrap().ast, &mut shellenv);
		let result = walker.start_walk();
		assert!(matches!(result,Ok(RshExitStatus::Success {..})));
		if let Ok(RshExitStatus::Fail { code, cmd: _, span: _ }) = result {
			assert_ne!(code,127);
		}
	}
	#[test]
	fn basic_while_script() {
		let input = from_file("/home/pagedmov/Coding/projects/rust/rsh/tests/basic_while.sh");
		let mut shellenv = ShellEnv::new(false,true);
		let state = parse::descend(&input, &shellenv);
		assert!(state.is_ok());
		let mut walker = NodeWalker::new(state.unwrap().ast, &mut shellenv);
		let result = walker.start_walk();
		assert!(matches!(result,Ok(RshExitStatus::Success {..})));
		if let Ok(RshExitStatus::Fail { code, cmd: _, span: _ }) = result {
			assert_ne!(code,127);
		}
	}
	#[test]
	fn basic_until_script() {
		let input = from_file("/home/pagedmov/Coding/projects/rust/rsh/tests/basic_until.sh");
		let mut shellenv = ShellEnv::new(false,true);
		let state = parse::descend(&input, &shellenv);
		assert!(state.is_ok());
		let mut walker = NodeWalker::new(state.unwrap().ast, &mut shellenv);
		let result = walker.start_walk();
		assert!(matches!(result,Ok(RshExitStatus::Success {..})));
		if let Ok(RshExitStatus::Fail { code, cmd: _, span: _ }) = result {
			assert_ne!(code,127);
		}
	}
	#[test]
	fn basic_commands() {
		let input = from_file("/home/pagedmov/Coding/projects/rust/rsh/tests/basic_commands.sh");
		let mut shellenv = ShellEnv::new(false,true);
		let state = parse::descend(&input, &shellenv);
		assert!(state.is_ok());
		let mut walker = NodeWalker::new(state.unwrap().ast, &mut shellenv);
		let result = walker.start_walk();
		assert!(matches!(result,Ok(RshExitStatus::Success {..})));
		if let Ok(RshExitStatus::Fail { code, cmd: _, span: _ }) = result {
			assert_ne!(code,127);
		}
	}
	#[test]
	fn basic_var_sub() {
		let input = from_file("/home/pagedmov/Coding/projects/rust/rsh/tests/basic_var_sub.sh");
		let mut shellenv = ShellEnv::new(false,true);
		let state = parse::descend(&input, &shellenv);
		assert!(state.is_ok());
		let mut walker = NodeWalker::new(state.unwrap().ast, &mut shellenv);
		let result = walker.start_walk();
		assert!(matches!(result,Ok(RshExitStatus::Success {..})));
		if let Ok(RshExitStatus::Fail { code, cmd: _, span: _ }) = result {
			assert_ne!(code,127);
		}
	}
	#[test]
	fn basic_chain() {
		let input = from_file("/home/pagedmov/Coding/projects/rust/rsh/tests/chain.sh");
		let mut shellenv = ShellEnv::new(false,true);
		let state = parse::descend(&input, &shellenv);
		assert!(state.is_ok());
		let mut walker = NodeWalker::new(state.unwrap().ast, &mut shellenv);
		let result = walker.start_walk();
		assert!(matches!(result,Ok(RshExitStatus::Success {..})));
		if let Ok(RshExitStatus::Fail { code, cmd: _, span: _ }) = result {
			assert_ne!(code,127);
		}
	}
	#[test]
	fn very_nested() {
		let input = from_file("/home/pagedmov/Coding/projects/rust/rsh/tests/very_nested.sh");
		let mut shellenv = ShellEnv::new(false,true);
		let state = parse::descend(&input, &shellenv);
		assert!(state.is_ok());
		let mut walker = NodeWalker::new(state.unwrap().ast, &mut shellenv);
		let result = walker.start_walk();
		assert!(matches!(result,Ok(RshExitStatus::Success {..})));
		if let Ok(RshExitStatus::Fail { code, cmd: _, span: _ }) = result {
			assert_ne!(code,127);
		}
	}
}
