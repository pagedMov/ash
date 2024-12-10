use nix::unistd::{close, dup, dup2, dup3, execvpe, fork, pipe, ForkResult};
use nix::fcntl::{open,OFlag};
use nix::sys::stat::Mode;
use nix::sys::wait::{waitpid, WaitStatus};
use std::os::fd::{AsRawFd, RawFd};
use std::ffi::CString;
use std::collections::{HashMap, VecDeque};
use std::path::PathBuf;
use std::sync::mpsc::Sender;
use log::{info,debug,trace};

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
                todo!("write builtins")
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
                todo!("handle if-then")
            }
            ASTNode::ForDo {..} => {
                todo!("handle for loops")
            }
            ASTNode::WhileDo {..} | ASTNode::UntilDo { .. } => {
                todo!("handle while/until loops")
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

    pub fn extract_args(&self,node: ASTNode) -> (Vec<CString>, VecDeque<Token>) {
        let mut argv = vec![];
        let mut redirs = VecDeque::new();
        if let ASTNode::Command { mut words } = node {
            while let Some(word) = words.pop_front() {
                match word.class() {
                    TokenType::Redir { .. } => {
                        // Collect redirection tokens
                        redirs.push_back(word);
                        redirs.extend(words.into_iter());
                        break;
                    }
                    _ => {
                        // Add to argv if not a redirection
                        argv.push(CString::new(word.text()).unwrap());
                    }
                }
            }
        } else {
            panic!("Unexpected ASTNode variant: {:?}", node);
        }
        (argv, redirs)
    }

    pub fn handle_redirs(&self, mut redirs: VecDeque<Token>) -> Result<(), ShellError> {
        let mut fd_queue: VecDeque<i32> = VecDeque::new();

        dup3(oldfd, newfd, flags)
        while let Some(redir) = redirs.pop_front() {
            match redir.class() {
                TokenType::Redir { redir_type, fd_out, fd_target, file_target } => {
                    // TODO: handle unwraps here
                    if let Some(target) = fd_target {
                        println!("duping fd {} to fd {}",fd_out,target);
                        dup2(*target,*fd_out).unwrap();
                        fd_queue.push_back(*target);

                    } else if let Some(file_path) = file_target {
                        println!("opening file: {:?}",file_path);
                        let flags = match redir_type {
                            RedirType::Input => OFlag::O_RDONLY,
                            RedirType::Output => OFlag::O_WRONLY | OFlag::O_CREAT | OFlag::O_TRUNC,
                            RedirType::Append => OFlag::O_WRONLY | OFlag::O_CREAT | OFlag::O_APPEND,
                            _ => unimplemented!("still have to implement heredocs and herestrings")
                        };
                        let file_fd: RawFd = open(file_path, flags, Mode::from_bits(0o644).unwrap()).unwrap();
                        dup2(file_fd, *fd_out).unwrap();
                        fd_queue.push_back(file_fd);
                    }
                }
                _ => unreachable!()
            }
        }
        while let Some(fd) = fd_queue.pop_front() {
            let _ = close(fd);
        }
        Ok(())
    }

    pub fn handle_command(&self, node: ASTNode, stdin: Option<RawFd>, stdout: Option<RawFd>) -> Result<RshExitStatus, ShellError> {
        let original_stdin = dup(0).unwrap();
        let original_stdout = dup(1).unwrap();
        let original_stderr = dup(2).unwrap();

        if let Some(read_pipe) = stdin {
            dup2(read_pipe, 0);
            close(read_pipe);
        }


        if let Some(write_pipe) = stdout {
            dup2(write_pipe, 1);
            close(write_pipe);
        }

        // Command and arguments
        let (argv, redirs) = self.extract_args(node);
        let command = argv[0].clone();

        // Prepare envp (using current environment)
        let envp = self.shellenv.get_cvars();

        // Fork the process
        match unsafe { fork() } {
            Ok(ForkResult::Child) => {
                // Handle redirections
                if !redirs.is_empty() {
                    self.handle_redirs(redirs)?;
                }

                dup2(original_stdin, 0).ok();
                dup2(original_stdout, 1).ok();
                dup2(original_stderr, 2).ok();

                // Close saved descriptors in the child
                close(original_stdin).ok();
                close(original_stdout).ok();
                close(original_stderr).ok();

                // Execute the command
                eprintln!("Execution failed: {:?}", execvpe(&command, &argv, &envp));
                std::process::exit(127); // Exit with a failure code
            }
            Ok(ForkResult::Parent { child }) => {
                // Parent process: Wait for the child process to finish
                let result = match waitpid(child, None) {
                    Ok(WaitStatus::Exited(_, status)) => Ok(RshExitStatus::from(status)),
                    Ok(WaitStatus::Signaled(_, signal, _)) => Err(ShellError::ExecFailed(
                        format!("Child process terminated by signal: {}", signal),
                        signal as i32,
                    )),
                    Err(err) => Err(ShellError::ExecFailed(
                        format!("Failed to wait for child process: {}", err),
                        1,
                    )),
                    _ => Err(ShellError::ExecFailed(
                        "Unexpected waitpid result".to_string(),
                        1,
                    )),
                };

                // Restore original descriptors in the parent
                dup2(original_stdin, 0).ok();
                dup2(original_stdout, 1).ok();
                dup2(original_stderr, 2).ok();

                // Close saved descriptors in the parent
                close(original_stdin).ok();
                close(original_stdout).ok();
                close(original_stderr).ok();

                result
            }
            Err(err) => Err(ShellError::ExecFailed(format!("Fork failed: {}", err), 1)),
        }
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
