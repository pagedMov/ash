use libc::{c_int, pipe};
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

use crate::builtin::{cd, echo};
use crate::event::ShellError;
use crate::interp::{self, expand};
use crate::interp::token::{Redir, RedirType, Tk, TkType};
use crate::interp::parse::{NdType,Node};
use crate::shellenv::ShellEnv;

// Allow both of these types to be handled by redirection
pub trait RedirTarget {}
impl RedirTarget for i32 {}
impl RedirTarget for PathBuf {}

pub const GLOB_OPTS: MatchOptions = MatchOptions {
	case_sensitive: false,
	require_literal_separator: true,
	require_literal_leading_dot: false
};

#[derive(Debug)]
pub enum RshExitStatus {
	Success,
	Fail { code: i32, cmd: Option<String> },
}

impl RshExitStatus {
	pub fn from(code: i32, cmd: Option<String>) -> Self {
		match code {
			0 => RshExitStatus::Success,
			_ => RshExitStatus::Fail { code, cmd }
		}
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
		let mut exit_status = RshExitStatus::Success;
		let mut nodes;
		if let NdType::Root { ref mut deck } = self.ast.nd_type {
			nodes = std::mem::take(deck);
		} else { unreachable!() }
		while let Some(node) = nodes.pop_front() {
			exit_status = self.walk(node, None, None)?;
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
	fn walk(&mut self, node: Node, stdin: Option<RawFd>, stdout: Option<RawFd>) -> Result<RshExitStatus,ShellError> {
		debug!("walking over node {:?} with these pipes: {:?},{:?}",node,stdin,stdout);
		let last_status;
		match node.nd_type {
			NdType::Command {..} => {
				trace!("Found command: {:?}",node);
				last_status = self.handle_command(node, stdin, stdout)?;
			}
			NdType::Builtin {..} => {
				last_status = self.handle_builtin(node, stdin, stdout)?;
			}
			//NdType::Function { .. } => {
			//todo!("handle simple commands")
			//}
			NdType::Pipeline {..} => {
				last_status = self.handle_pipeline(node, stdin, stdout)?;
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
				todo!("handle case-in")
			}
			NdType::Subshell {..} => {
				last_status = self.handle_subshell(node,stdin,stdout)?;
			}
			NdType::FuncDef {..} => {
				todo!("handle function definition")
			}
			NdType::Assignment {..} => {
				last_status = self.handle_assignment(node)?;
			}
			_ => unimplemented!()
		}
		Ok(last_status)
	}

	fn walk_root(&mut self, node: Node, break_condition: Option<bool>) -> Result<RshExitStatus,ShellError> {
		let mut last_status = RshExitStatus::Success;
		if let NdType::Root { deck } = node.nd_type {
			for node in deck {
				last_status = self.walk(node,None,None)?;
				if let Some(condition) = break_condition {
					match condition {
						true => {
							if let RshExitStatus::Fail {..} = last_status {
								break
							}
						}
						false => {
							if let RshExitStatus::Success = last_status {
								break
							}
						}
					}
				}
			}
		}
		Ok(last_status)
	}

	/// For loops in bash can have multiple loop variables, e.g. `for a b c in 1 2 3 4 5 6`
	/// In this case, loop_vars are also iterated through, e.g. a = 1, b = 2, c = 3, etc
	/// Here, we use the modulo operator to figure out which variable to set on each iteration.
	fn handle_for(&mut self, node: Node) -> Result<RshExitStatus, ShellError> {
		let mut last_status = RshExitStatus::Success;

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
		let mut last_status = RshExitStatus::Success;

		if let NdType::Loop { condition, logic } = node.nd_type {
			let cond= *logic.condition;
			let body = *logic.body;
			loop {
				// Evaluate the loop condition
				let condition_status = self.walk_root(cond.clone(),Some(condition))?;

				match condition {
					true => {
						if !matches!(condition_status,RshExitStatus::Success) {
							break; // Exit for a `while` loop when condition is false
						}
					}
					false => {
						if matches!(condition_status,RshExitStatus::Success) {
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
			Err(ShellError::InvalidSyntax(
					"Expected a loop node in handle_loop".to_string(),
			))
		}
	}

	fn handle_if(&mut self, node: Node) -> Result<RshExitStatus, ShellError> {
		let mut last_result = RshExitStatus::Success;

		if let NdType::If { mut cond_blocks, else_block } = node.nd_type {
			while let Some(block) = cond_blocks.pop_front() {
				let cond = *block.condition;
				let body = *block.body;
				last_result = self.walk_root(cond,Some(false))?;
				if let RshExitStatus::Success = last_result {
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
		let mut last_status = RshExitStatus::Success;

		if let NdType::Chain { left, right, op } = node.nd_type {
			match self.walk(*left, None, None)? {
				RshExitStatus::Success => {
					if let NdType::And = op.nd_type {
						last_status = self.walk(*right, None, None)?;
					}
				}
				RshExitStatus::Fail {..} => {
					if let NdType::Or = op.nd_type {
						last_status = self.walk(*right, None, None)?;
					}
				}
			}

		}
		Ok(last_status)
	}


	fn handle_assignment(&mut self, node: Node) -> Result<RshExitStatus,ShellError> {
		if let NdType::Assignment { name, value } = node.nd_type {
			let value = value.unwrap_or_default();
			self.shellenv.set_variable(name, value);
		}
		Ok(RshExitStatus::Success)
	}

	fn handle_builtin(&mut self, node: Node, stdin: Option<RawFd>, stdout: Option<RawFd>) -> Result<RshExitStatus,ShellError> {
		let (argv,redirs) = self.extract_args(node);
		match argv[0].to_str().unwrap() {
			"echo" => echo(argv.into(), redirs, stdout),
			"cd" => cd(self.shellenv, argv.into()),
			_ => unimplemented!()
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
	fn extract_args(&mut self, node: Node) -> (Vec<CString>, VecDeque<Tk>) {
		let mut args = vec![];
		let cmd_redirs;
		match node.nd_type {
			NdType::Command { mut argv, redirs } | NdType::Builtin { mut argv, redirs } => {
				trace!("Extracting arguments from command node: {:?}", args);
				while let Some(word) = argv.pop_front() {
					debug!("checking word: {}",word.text());
					let tokens = expand::expand_token(self.shellenv, word);
					debug!("got expanded tokens: {:?}",tokens);
					for token in tokens {
						let cstring = CString::new(token.text()).unwrap();
						args.push(cstring);
					}
				}
				cmd_redirs = redirs;
			}
			_ => unreachable!()
		}
		debug!("Extracted args: {:?}, redirs: {:?}", args, cmd_redirs);
		(args, cmd_redirs)
	}

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

	fn handle_subshell(&mut self, node: Node, stdin: Option<RawFd>, stdout: Option<RawFd>) -> Result<RshExitStatus,ShellError> {
		todo!()
	}

	fn handle_pipeline(&mut self, node: Node, stdin: Option<RawFd>, stdout: Option<RawFd>) -> Result<RshExitStatus, ShellError> {
		let (left, right) = if let NdType::Pipeline { left, right } = node.nd_type {
			(*left, *right)
		} else {
			unreachable!()
		};
		dbg!(&left);
		dbg!(&right);

		let mut fds: [c_int;2] = [0;2];

		unsafe { pipe(fds.as_mut_ptr()); }

		let r_pipe = fds[0];
		let w_pipe = fds[1];
		dbg!("got pipes", r_pipe, w_pipe);

		// Walk left side of the pipeline
		let left_status = self.walk(left, stdin, Some(w_pipe));

		// Walk right side of the pipeline
		let right_status = self.walk(right, Some(r_pipe), stdout);

		// Return status of the last command
		left_status?;
		right_status
	}

	fn handle_command(&mut self, node: Node, stdin: Option<RawFd>, stdout: Option<RawFd>) -> Result<RshExitStatus, ShellError> {
		let (argv, redirs) = if let NdType::Command { argv, redirs } = node.nd_type {
			(argv, redirs)
		} else {
			unreachable!()
		};
		let saved_in = dup(0).unwrap();
		let saved_out = dup(1).unwrap();
		let saved_err = dup(2).unwrap();

		// Redirect stdout
		if let Some(w_pipe) = stdout {
			dbg!("Redirecting stdout to {}", w_pipe);
			if let Err(err) = dup2(w_pipe, 1) {
				eprintln!("Failed to duplicate stdout: {}", err);
				std::process::exit(1);
			}
			close(w_pipe).expect("failed closing w_pipe in child");
		}

		// Redirect stdin
		if let Some(r_pipe) = stdin {
			dbg!("Redirecting stdin to {}", r_pipe);
			if let Err(err) = dup2(r_pipe, 0) {
				eprintln!("Failed to duplicate stdin: {}", err);
				std::process::exit(1);
			}
			close(r_pipe).expect("failed closing r_pipe in child");
		}


		let cmd = Some(argv[0].text().to_string());
		dbg!(&cmd);
		let command = CString::new(argv[0].text()).unwrap();
		let argv = argv
			.into_iter()
			.map(|tk| CString::new(tk.text()).unwrap())
			.collect::<Vec<CString>>();
			let envp = self.shellenv.get_cvars();

			match unsafe { fork() } {
				Ok(ForkResult::Child) => {
					dbg!("In child process");


					// Handle redirections
					if !redirs.is_empty() {
						self.handle_redirs(redirs)?;
					}

					// Execute the command
					let Err(err) = execvpe(&command, &argv, &envp);
					eprintln!("Exec failed: {}", err);
					std::process::exit(127);
				}
				Ok(ForkResult::Parent { child }) => {
					dbg!("In parent process, waiting for child {}", child);
					let _ = dup2(saved_in,0);
					let _ = dup2(saved_out,1);
					let _ = dup2(saved_err,2);
					let _ = close(saved_in);
					let _ = close(saved_out);
					let _ = close(saved_err);

					// Wait for the child process to finish
					match waitpid(child, None) {
						Ok(WaitStatus::Exited(_, code)) => match code {
							0 => Ok(RshExitStatus::Success),
							_ => Ok(RshExitStatus::Fail { code, cmd }),
						},
						Ok(_) => Err(ShellError::ExecFailed("Unexpected waitpid result".to_string(), 1)),
						Err(err) => Err(ShellError::ExecFailed(format!("Waitpid failed: {}", err), 1)),
					}
				}
				Err(_) => Err(ShellError::ExecFailed("Fork failed".to_string(), 1)),
			}
	}
}

//pub fn run_subshell<F>(func: F) -> Result<RshExitStatus,ShellError>
//where
//F: FnOnce() -> Result<RshExitStatus,ShellError>
//{
//let mut last_status = RshExitStatus::Success;
//match unsafe { fork() } {
//Ok(ForkResult::Child) => {
//if let Err(e) = func() {
//eprintln!("Subshell failed: {}",e);
//std::process::exit(1);
//}
//std::process::exit(0);
//}
//Ok(ForkResult::Parent { child }) => {
//match waitpid(child,None) {
//Ok(WaitStatus::Exited(_,status)) => {
//if status == 0 {
//return Ok(RshExitStatus::Success);
//} else {
//return Ok(RshExitStatus::Fail { code: status, cmd: cmd_name });
//}
//}
//_ => panic!()
//}
//}
//Err(e) => Err(ShellError::ExecFailed(format!("Subshell failed"), 1))
//}
//}

/// Example command invocation using system calls
/// Note that the command name is the first argument in argv
/// execvpe will handle path crawling for you
/// just make sure that the path env var is set up
fn hello_world() -> Result<(), String> {
	// Command and arguments
	let command = "echo";
	let args = ["echo", "hello", "world"];

	// Prepare argv
	let argv = args
		.iter()
		.map(|&arg| CString::new(arg).unwrap())
		.collect::<Vec<CString>>();

		// Prepare envp (using current environment)
		let env_vars: HashMap<String, String> = std::env::vars().collect();
		let envp = env_vars
			.iter()
			.map(|(key, value)| {
				let env_pair = format!("{}={}", key, value);
				CString::new(env_pair).unwrap() })
			.collect::<Vec<CString>>();

		let file_path = "/tmp/hello_world.txt";
		let file_fd: RawFd = open(
			file_path,
			OFlag::O_WRONLY | OFlag::O_CREAT | OFlag::O_TRUNC,
			Mode::from_bits(0o644).unwrap(),
		).unwrap();

		// Fork the process
		match unsafe { fork() } {
			Ok(ForkResult::Child) => {
				// Child process: execute the command
				dup2(file_fd,1).unwrap();
				close(file_fd).unwrap();
				let c_command = CString::new(command).unwrap();
				let _ = execvpe(&c_command, &argv, &envp);
			}
			Ok(ForkResult::Parent { child }) => {
				// Parent process: Wait for the child process to finish
				use nix::sys::wait::waitpid;
				if let Err(err) = waitpid(child, None) {
					return Err(format!("Failed to wait for child process: {}", err));
				}
			}
			Err(err) => return Err(format!("Fork failed: {}", err)),
		}

		Ok(())
}
