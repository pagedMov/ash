use nix::unistd::{close, dup, dup2, execvpe, fork, pipe, ForkResult};
use nix::fcntl::{open,OFlag};
use nix::sys::stat::Mode;
use nix::sys::wait::{waitpid, WaitStatus};
use std::os::fd::{AsRawFd, RawFd};
use std::ffi::CString;
use std::collections::{HashMap, VecDeque};
use std::path::PathBuf;
use std::sync::mpsc::Sender;
use log::{error,info,debug,trace};

use crate::builtin::echo;
use crate::event::ShellError;
use crate::parser::{ASTNode, ChainOp, RedirType, Token, TokenType};
use crate::shellenv::{self, ShellEnv};

// Allow both of these types to be handled by redirection
pub trait RedirTarget {}
impl RedirTarget for i32 {}
impl RedirTarget for PathBuf {}

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
    pub fn walk(&mut self, node: ASTNode, stdin: Option<RawFd>, stdout: Option<RawFd>) -> Result<RshExitStatus,ShellError> {
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
            ASTNode::IfThen {..} => {
                last_status = self.handle_if(node)?;
            }
            ASTNode::ForDo {..} => {
                todo!("handle for loops")
            }
            ASTNode::WhileDo {..} | ASTNode::UntilDo { .. } => {
                last_status = self.handle_while_and_until(node)?;
            }
            ASTNode::CaseIn {..} => {
                todo!("handle case-in")
            }
            ASTNode::FuncDef {..} => {
                todo!("handle function definition")
            }
            _ => unreachable!()
        }
        Ok(last_status)
    }

    // One for checking conditions and one for executing code bodies
    pub fn walk_condition(&mut self, commands: VecDeque<ASTNode>) -> Result<RshExitStatus,ShellError> {
        let mut last_status = RshExitStatus::Success;
        for command in commands {
            last_status = self.walk(command,None,None)?;
            if let RshExitStatus::Fail(_) = last_status {
                break
            }
        }
        Ok(last_status)
    }
    pub fn walk_body(&mut self, commands: VecDeque<ASTNode>) -> Result<RshExitStatus,ShellError> {
        let mut last_status = RshExitStatus::Success;
        for command in commands {
            last_status = self.walk(command,None,None)?;
        }
        Ok(last_status)
    }

    pub fn handle_for(&mut self, node: ASTNode) -> Result<RshExitStatus, ShellError> {
        let mut last_status = RshExitStatus::Success;
        if let ASTNode::ForDo { vars, array, body } = node {

        }
        todo!()
    }

    pub fn handle_while_and_until(&mut self, node: ASTNode) -> Result<RshExitStatus, ShellError> {
        let mut last_status = RshExitStatus::Success;
        match node {
            ASTNode::WhileDo { body } => {
                while let Ok(RshExitStatus::Success) = self.walk_condition(body.get_cond()) {
                    last_status = self.walk_body(body.get_body())?;
                }
                Ok(last_status)
            }
            ASTNode::UntilDo { body } => {
                while let Ok(RshExitStatus::Fail(_code)) = self.walk_condition(body.get_cond()) {
                    last_status = self.walk_body(body.get_body())?;
                }
                Ok(last_status)
            }
            _ => unreachable!()
        }
    }

    pub fn handle_if(&mut self, node: ASTNode) -> Result<RshExitStatus, ShellError> {
        if let ASTNode::IfThen { if_block, elif_blocks, else_block } = node {
            let if_result = self.walk_condition(if_block.get_cond());

            if let Ok(RshExitStatus::Success) = if_result {
                return self.walk_body(if_block.get_body())

            } else if !elif_blocks.is_empty() {
                let mut elif_result;
                for path in elif_blocks {
                    elif_result = self.walk_condition(path.get_cond());
                    if let Ok(RshExitStatus::Success) = elif_result {
                        return self.walk_body(path.get_body())
                    }
                }

            } else if let Some(path) = else_block {
                return self.walk_body(path.get_body())
            }
        }
        Ok(RshExitStatus::Fail(1))
    }

    pub fn handle_chain(&mut self, node: ASTNode) -> Result<RshExitStatus, ShellError> {
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

    pub fn handle_pipeline(&mut self, node: ASTNode, stdin: Option<RawFd>) -> Result<RshExitStatus, ShellError> {
        let mut last_status = RshExitStatus::from(0);

        // Create the pipe
        let (read, write) = nix::unistd::pipe().map_err(|e| {
            ShellError::ExecFailed(format!("Failed to create pipe: {}", e), 1)
        })?;
        let (read, write) = (read.as_raw_fd(), write.as_raw_fd());

        if let ASTNode::Pipeline { left, right } = node {
            // Walk the left side of the pipeline
            debug!("Walking left side of pipeline: {:?}", left);
            last_status = self.walk(*left, stdin, Some(write))
                .expect("Failed to walk left side of pipeline");

            debug!("Walking right side of pipeline: {:?}", right);
            last_status = self.walk(*right, Some(read), None)
                .expect("Failed to walk right side of pipeline");
        }

        Ok(last_status)
    }

    pub fn extract_args(&self, node: ASTNode) -> (Vec<CString>, VecDeque<Token>) {
        let mut args = vec![];
        let mut redirs = VecDeque::new();
        if let ASTNode::Command { mut argv } = node {
            trace!("Extracting arguments from command node: {:?}", args);
            while let Some(word) = argv.pop_front() {
                match word.class() {
                    TokenType::Redir { .. } => {
                        trace!("Found redirection token: {:?}", word);
                        redirs.push_back(word);
                        redirs.extend(argv.into_iter());
                        break;
                    }
                    _ => {
                        trace!("Adding argument: {}", word.text());
                        args.push(CString::new(word.text()).unwrap());
                    }
                }
            }
        } else {
            panic!("Unexpected ASTNode variant: {:?}", node);
        }
        debug!("Extracted args: {:?}, redirs: {:?}", args, redirs);
        (args, redirs)
    }

    pub fn handle_redirs(&self, mut redirs: VecDeque<Token>) -> Result<(), ShellError> {
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

    pub fn handle_builtin(&self, node: ASTNode, stdin: Option<RawFd>, stdout: Option<RawFd>) -> Result<RshExitStatus,ShellError> {
        if let ASTNode::Builtin { argv } = node {
            match argv[0].text() {
                "echo" => return echo(argv, stdout),
                _ => unimplemented!()
            };
        };
        unreachable!()
    }

    pub fn handle_command(&self, node: ASTNode, stdin: Option<RawFd>, stdout: Option<RawFd>) -> Result<RshExitStatus, ShellError> {
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
pub fn hello_world() -> Result<(), String> {
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
