use nix::unistd::{close, dup, dup2, execvpe, fork, pipe, ForkResult};
use nix::fcntl::{open,OFlag};
use nix::sys::stat::Mode;
use nix::sys::wait::{waitpid, WaitStatus};
use std::os::fd::{AsRawFd, RawFd};
use std::ffi::CString;
use std::collections::{HashMap, VecDeque};
use std::path::PathBuf;
use log::{error,info,debug,trace};
use glob::MatchOptions;

use crate::builtin::{cd, echo};
use crate::event::ShellError;
use crate::parser::{self, ASTNode, ChainOp, RedirType, Token, TokenType};
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
    Fail(i32),
}

impl RshExitStatus {
    pub fn from(code: i32) -> Self {
        match code {
            0 => RshExitStatus::Success,
            _ => RshExitStatus::Fail(code)
        }
    }
}


pub struct NodeWalker<'a> {
    ast: VecDeque<ASTNode>,
    shellenv: &'a mut ShellEnv,
}

impl<'a> NodeWalker<'a> {
    pub fn new(ast: VecDeque<ASTNode>, shellenv: &'a mut ShellEnv) -> Self {
        Self {
            ast,
            shellenv
        }
    }
    pub fn start_walk(&mut self) -> Result<RshExitStatus,ShellError> {
        info!("Going on a walk...");
        let mut exit_status = RshExitStatus::from(0);
        while let Some(node) = self.ast.pop_front() {
            exit_status = self.walk(node, None, None)?;
        }
        Ok(exit_status)
    }

    /// This function will walk through the ASTNode tree and direct nodes to the proper functions
    ///
    /// # Inputs:
    /// stdin: An optional arg containing a raw file descriptor. This file descriptor is used as an
    /// address to tell the kernel where to look for piped standard input, if any.
    ///
    /// stdout: Another optional arg containing a raw fd. This fd tells the kernel where to write
    /// stdout to.
    ///
    /// # Outputs: An RshExitStatus or a shell error.
    fn walk(&mut self, node: ASTNode, stdin: Option<RawFd>, stdout: Option<RawFd>) -> Result<RshExitStatus,ShellError> {
        debug!("walking over node {:?} with these pipes: {:?},{:?}",node,stdin,stdout);
        let last_status;
        match node {
            ASTNode::Command {..} => {
                trace!("Found command: {:?}",node);
                last_status = self.handle_command(node, stdin, stdout)?;
            }
            ASTNode::Builtin { .. } => {
                last_status = self.handle_builtin(node, stdin, stdout)?;
            }
            ASTNode::Function { .. } => {
                todo!("handle simple commands")
            }
            ASTNode::Pipeline {..} => {
                last_status = self.handle_pipeline(node, stdin)?;
            }
            ASTNode::Chain {..} => {
                last_status = self.handle_chain(node)?;
            }
            ASTNode::If {..} => {
                last_status = self.handle_if(node)?;
            }
            ASTNode::For {..} => {
                last_status = self.handle_for(node)?;
            }
            ASTNode::Loop {..} => {
                last_status = self.handle_loop(node)?;
            }
            ASTNode::Case {..} => {
                todo!("handle case-in")
            }
            ASTNode::FuncDef {..} => {
                todo!("handle function definition")
            }
            ASTNode::Assignment {..} => {
                last_status = self.handle_assignment(node)?;
            }
            _ => unreachable!()
        }
        Ok(last_status)
    }

    // One for checking conditions and one for executing code bodies
    fn walk_condition(&mut self, commands: VecDeque<ASTNode>) -> Result<RshExitStatus,ShellError> {
        let mut last_status = RshExitStatus::Success;
        for command in commands {
            last_status = self.walk(command,None,None)?;
            if let RshExitStatus::Fail(_) = last_status {
                break
            }
        }
        Ok(last_status)
    }
    fn walk_body(&mut self, commands: VecDeque<ASTNode>) -> Result<RshExitStatus,ShellError> {
        let mut last_status = RshExitStatus::Success;
        for command in commands {
            last_status = self.walk(command,None,None)?;
        }
        Ok(last_status)
    }

    /// For loops in bash can have multiple loop variables, e.g. `for a b c in 1 2 3 4 5 6`
    /// In this case, loop_vars are also iterated through, e.g. a = 1, b = 2, c = 3, etc
    /// Here, we use the modulo operator to figure out which variable to set on each iteration.
    fn handle_for(&mut self, node: ASTNode) -> Result<RshExitStatus, ShellError> {
        let mut last_status = RshExitStatus::Success;

        if let ASTNode::For { vars, mut array, body } = node {
            let var_count = vars.len();
            let mut var_index = 0;
            let mut iteration_count = 0;

            while !array.is_empty() {
                // Clear variables at the start of each iteration
                self.clear_variables(&vars);

                // Get the current value from the array
                let current_value = array.pop_front().unwrap().text().to_string();

                // Set the current variable to the current value
                let current_var = vars[var_index].text().to_string();
                self.shellenv.set_variable(current_var, current_value);

                // Update the variable index for the next iteration
                iteration_count += 1;
                var_index = iteration_count % var_count;

                // Execute the body of the loop
                last_status = self.walk_body(body.get_body())?;
            }
        }

        Ok(last_status)
    }

    // Helper function to clear all variables
    fn clear_variables(&mut self, vars: &VecDeque<Token>) {
        for var in vars {
            self.shellenv.set_variable(var.text().into(), "".into());
        }
    }

    fn handle_loop(&mut self, node: ASTNode) -> Result<RshExitStatus, ShellError> {
        let mut last_status = RshExitStatus::Success;

        if let ASTNode::Loop { condition, body } = node {
            loop {
                // Evaluate the loop condition
                let condition_status = self.walk_condition(body.get_cond())?;

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
                last_status = self.walk_body(body.get_body())?;

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

    fn handle_if(&mut self, node: ASTNode) -> Result<RshExitStatus, ShellError> {
        let mut last_result;

        if let ASTNode::If { mut cond_blocks, else_block } = node {
            while let Some(block) = cond_blocks.pop_front() {
                last_result = self.walk_condition(block.get_cond());
                if let Ok(RshExitStatus::Success) = last_result {
                    return self.walk_body(block.get_body())
                }
            }
            if else_block.is_some() {
                return self.walk_body(else_block.unwrap().get_body())
            }
        }
        Ok(RshExitStatus::Fail(1))
    }

    fn handle_chain(&mut self, node: ASTNode) -> Result<RshExitStatus, ShellError> {
        let mut last_status = RshExitStatus::from(0);

        if let ASTNode::Chain { left, right, operator } = node {
            match self.walk(*left, None, None)? {
                RshExitStatus::Success => {
                    if let ChainOp::And = operator {
                        last_status = self.walk(*right, None, None)?;
                    }
                }
                RshExitStatus::Fail(_code) => {
                    if let ChainOp::Or = operator {
                        last_status = self.walk(*right, None, None)?;
                    }
                }
            }

        }
        Ok(last_status)
    }

    fn handle_pipeline(&mut self, node: ASTNode, stdin: Option<RawFd>) -> Result<RshExitStatus, ShellError> {
        let mut last_status = RshExitStatus::from(0);

        // Create the pipe
        let (read, write) = pipe().map_err(|e| {
            ShellError::ExecFailed(format!("Failed to create pipe: {}", e), 1)
        })?;
        let (read, write) = (read.as_raw_fd(), write.as_raw_fd());

        if let ASTNode::Pipeline { left, right } = node {
            // Walk the left side of the pipeline
            debug!("Walking left side of pipeline: {:?}", left);
            last_status = self.walk(*left, stdin, Some(write))
                .expect("Failed to walk left side of pipeline");

            if let RshExitStatus::Fail(_) = last_status {
                return Ok(last_status)
            }

            debug!("Walking right side of pipeline: {:?}", right);
            last_status = self.walk(*right, Some(read), None)
                .expect("Failed to walk right side of pipeline");
        }

        Ok(last_status)
    }

    fn handle_assignment(&mut self, node: ASTNode) -> Result<RshExitStatus,ShellError> {
        if let ASTNode::Assignment { var, val } = node {
            self.shellenv.set_variable(var, val);
        }
        Ok(RshExitStatus::Success)
    }

    fn handle_builtin(&mut self, node: ASTNode, stdin: Option<RawFd>, stdout: Option<RawFd>) -> Result<RshExitStatus,ShellError> {
        let (argv,_redirs) = self.extract_args(node);
        match argv[0].to_str().unwrap() {
            "echo" => echo(argv.into(), stdout),
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
    /// * A `VecDeque` of `Token` representing the extracted redirections.
    ///
    /// # Panics
    ///
    /// This function will panic if the provided `node` is not of type `Command`, `Builtin`,
    /// or `Function`.
    ///
    /// # Example
    ///
    /// let node = ASTNode::Command { argv: vec![...].into_iter().collect() };
    /// let (args, redirs) = shell.extract_args(node);
    ///
    /// # Notes
    ///
    /// - The function uses `trace!` and `debug!` macros for logging purposes.
    /// - Variable substitutions (`$var`) are resolved using the `shellenv`'s `get_variable` method.
    /// - If a variable substitution is not found, an empty `CString` is added to the arguments.
    ///
    fn extract_args(&mut self, node: ASTNode) -> (Vec<CString>, VecDeque<Token>) {
        let mut args = vec![];
        let mut redirs = VecDeque::new();
        match node {
            ASTNode::Command { mut argv } | ASTNode::Builtin { mut argv } | ASTNode::Function { mut argv } => {
                trace!("Extracting arguments from command node: {:?}", args);
                while let Some(word) = argv.pop_front() {
                    trace!("checking word: {}",word.text());
                    match word.class() {
                        TokenType::Redir { .. } => {
                            trace!("Found redirection token: {:?}", word);
                            redirs.push_back(word);
                            redirs.extend(argv.into_iter());
                            break;
                        }
                        _ => {
                            // Expand arguments, only expand braces if arguments is not empty
                            let mut expanded = parser::expand(self.shellenv, word.clone());

                            while let Some(expanded_token) = expanded.pop_front() {
                                let cstring = CString::new(expanded_token.text()).unwrap();
                                args.push(cstring);
                            }
                        }
                    }
                }
            }
            _ => panic!("Unexpected ASTNode variant: {:?}", node),
        }
        debug!("Extracted args: {:?}, redirs: {:?}", args, redirs);
        (args, redirs)
    }

    fn handle_redirs(&self, mut redirs: VecDeque<Token>) -> Result<(), ShellError> {
        let mut fd_queue: VecDeque<i32> = VecDeque::new();
        debug!("Handling redirections: {:?}", redirs);

        while let Some(redir) = redirs.pop_front() {
            match redir.class() {
                TokenType::Redir { redir_type, fd_out, fd_target, file_target } => {
                    if let Some(target) = fd_target {
                        info!("Redirecting FD {} to {}", fd_out, target);
                        dup2(*target, *fd_out).unwrap();
                        fd_queue.push_back(*target);

                    } else if let Some(file_path) = file_target {
                        info!("Opening file for redirection: {:?}", file_path);
                        let flags = match redir_type {
                            RedirType::Input => OFlag::O_RDONLY,
                            RedirType::Output => OFlag::O_WRONLY | OFlag::O_CREAT | OFlag::O_TRUNC,
                            RedirType::Append => OFlag::O_WRONLY | OFlag::O_CREAT | OFlag::O_APPEND,
                            _ => unimplemented!("Heredocs and herestrings are not implemented yet."),
                        };
                        let file_fd: RawFd = open(file_path, flags, Mode::from_bits(0o644).unwrap()).unwrap();
                        info!("Duping file FD {} to FD {}", file_fd, fd_out);
                        dup2(file_fd, *fd_out).unwrap();
                        fd_queue.push_back(file_fd);
                    }
                }
                _ => unreachable!(),
            }
        }

        debug!("Closing FDs: {:?}", fd_queue);
        while let Some(fd) = fd_queue.pop_front() {
            let _ = close(fd);
        }
        Ok(())
    }

    fn handle_command(&mut self, node: ASTNode, stdin: Option<RawFd>, stdout: Option<RawFd>) -> Result<RshExitStatus, ShellError> {
        info!("Handling command: {:?}", node);

        let original_stdin = dup(0).unwrap();
        let original_stdout = dup(1).unwrap();
        let original_stderr = dup(2).unwrap();
        debug!(
            "Saved original FDs: stdin={}, stdout={}, stderr={}",
            original_stdin, original_stdout, original_stderr
        );

        if let Some(read_pipe) = stdin {
            info!("Redirecting stdin to pipe FD {}", read_pipe);
            dup2(read_pipe, 0).unwrap();
            close(read_pipe).unwrap();
        }

        if let Some(write_pipe) = stdout {
            info!("Redirecting stdout to pipe FD {}", write_pipe);
            dup2(write_pipe, 1).unwrap();
            close(write_pipe).unwrap();
        }

        let (argv, redirs) = self.extract_args(node);

        let command = argv[0].clone();
        debug!("Command to execute: {:?}, argv: {:?}", command, argv);

        let envp = self.shellenv.get_cvars();

        match unsafe { fork() } {
            Ok(ForkResult::Child) => {
                info!("In child process");
                if !redirs.is_empty() {
                    info!("Handling redirections in child process");
                    self.handle_redirs(redirs)?;
                }
                eprintln!("Execution failed: {:?}", execvpe(&command, &argv, &envp));
                std::process::exit(127);
            }
            Ok(ForkResult::Parent { child }) => {
                info!("In parent process, waiting for child PID {}", child);
                let result = match waitpid(child, None) {
                    Ok(WaitStatus::Exited(_, status)) => {
                        info!("Child process exited with status {}", status);
                        Ok(RshExitStatus::from(status))
                    }
                    Ok(WaitStatus::Signaled(_, signal, _)) => {
                        error!("Child process terminated by signal {}", signal);
                        Err(ShellError::ExecFailed(
                            format!("Child process terminated by signal: {}", signal),
                            signal as i32,
                        ))
                    }
                    Err(err) => {
                        error!("Failed to wait for child process: {}", err);
                        Err(ShellError::ExecFailed(
                            format!("Failed to wait for child process: {}", err),
                            1,
                        ))
                    }
                    _ => {
                        error!("Unexpected waitpid result");
                        Err(ShellError::ExecFailed("Unexpected waitpid result".to_string(), 1))
                    }
                };

                debug!("Restoring original FDs in parent process");
                dup2(original_stdin, 0).ok();
                dup2(original_stdout, 1).ok();
                dup2(original_stderr, 2).ok();

                debug!("Closing saved FDs in parent process");
                close(original_stdin).ok();
                close(original_stdout).ok();
                close(original_stderr).ok();

                result
            }
            Err(err) => {
                error!("Fork failed: {}", err);
                Err(ShellError::ExecFailed(format!("Fork failed: {}", err), 1))
            }
        }
    }
}

pub fn run_subshell<F>(func: F) -> Result<RshExitStatus,ShellError>
where
    F: FnOnce() -> Result<RshExitStatus,ShellError>
{
    let mut last_status = RshExitStatus::Success;
    match unsafe { fork() } {
        Ok(ForkResult::Child) => {
            if let Err(e) = func() {
                eprintln!("Subshell failed: {}",e);
                std::process::exit(1);
            }
            std::process::exit(0);
        }
        Ok(ForkResult::Parent { child }) => {
            match waitpid(child,None) {
                Ok(WaitStatus::Exited(_,status)) => {
                    if status == 0 {
                        return Ok(RshExitStatus::Success);
                    } else {
                        return Ok(RshExitStatus::Fail(status));
                    }
                }
                _ => panic!()
            }
        }
        Err(e) => Err(ShellError::ExecFailed(format!("Subshell failed"), 1))
    }
}

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
