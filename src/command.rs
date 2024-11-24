use log::{debug, trace};
use std::fs::File;
use std::os::unix::process::ExitStatusExt;
use std::process::{ChildStdout, Command, ExitStatus, Stdio};
use crate::parser::{RedirectionType, ASTNode};

pub fn node_walk(nodes: Vec<ASTNode>) {
    for node in nodes {
        match &node {
            ASTNode::ShCommand { name: _, args: _, redirs: _ } => {
                debug!("Executing ShCommand node");
                let _ = execute(node);
            }
            ASTNode::Pipeline { commands: _ } => {
                debug!("Executing Pipeline node");
                let _ = execute(node);
            },
            ASTNode::Conditional { condition: _, body1: _, body2: _ } => {
                debug!("Executing Conditional node");
                let _ = exec_conditional(node);
            },
            _ => panic!("Unexpected node type found while walking"),
        }
    }
}

pub fn mk_process(command: ASTNode, stdin: Option<ChildStdout>, pipeout: bool) -> Result<Command, String> {
    debug!("Creating process for command: {:?}", command);

    match command {
        ASTNode::ShCommand { name, args, redirs } => {
            let mut child = Command::new(name.clone());
            debug!("Command: {} with args: {:?}", name, args);
            debug!("Stdin: {:?}",stdin);
            debug!("Pipeout: {}",pipeout);
            child.args(args);

            for redir in redirs {
                debug!("Handling redirection: {:?}", redir);
                let file = File::open(redir.get_filepath())
                    .map_err(|e| format!("Failed to open file '{}': {}", redir.get_filepath(), e))?;

                match redir.get_direction() {
                    RedirectionType::Input => {
                        debug!("Redirecting input from file: {}", redir.get_filepath());
                        if stdin.is_none() {
                            child.stdin(Stdio::from(file));
                        }
                    }
                    RedirectionType::Output => {
                        debug!("Redirecting output to file: {}", redir.get_filepath());
                        child.stdout(Stdio::from(file));
                    }
                }
            }

            if let Some(input) = stdin {
                debug!("Setting stdin from previous command's stdout");
                child.stdin(Stdio::from(input));
            }

            if pipeout {
                debug!("Setting up pipeout for the command");
                child.stdout(Stdio::piped());
            }

            Ok(child)
        }
        _ => Err("Expected ShCommand, got some other ASTNode from the parser".to_string()),
    }
}

pub fn execute(node: ASTNode) -> Result<ExitStatus, String> {
    debug!("Executing ASTNode: {:?}", node);

    match &node {
        ASTNode::ShCommand { name: _, args: _, redirs: _ } => {
            debug!("Executing ShCommand");
            let mut command = mk_process(node, None, false)?;
            debug!("Spawning child process for ShCommand");
            let mut child = command.spawn().map_err(|e| e.to_string())?;
            let status = child.wait().map_err(|e| e.to_string())?;
            debug!("Child process exited with status: {:?}", status);
            Ok(status)
        }

        ASTNode::Pipeline { commands } => {
            debug!("Executing pipeline with {} commands", commands.len());
            let mut stdin: Option<ChildStdout> = None;
            let mut pipeout = true;
            let mut command_iter = commands.iter().peekable();

            while let Some(command) = command_iter.next() {
                if command_iter.peek().is_none() {
                    debug!("Last command found, not piping output");
                    debug!("Command_iter: {:?}", command_iter);
                    pipeout = false;
                }

                debug!("Processing command in pipeline: {:?}", command);
                let mut process = mk_process(command.clone(), stdin, pipeout)?;
                debug!("Spawning child process for pipeline");
                let mut child = process.spawn().map_err(|e| e.to_string())?;
                stdin = child.stdout.take();

                debug!("Waiting for child process to finish");
                child.wait().map_err(|e| e.to_string())?;

                if command_iter.peek().is_none() {
                    debug!("Last command found, not piping output");
                    pipeout = false;
                }
            }

            Ok(ExitStatus::from_raw(0))
        }

        ASTNode::Conditional { condition: _, body1: _, body2: _ } => {
            debug!("Executing conditional statement");
            exec_conditional(node).map_err(|e| e.to_string())
        }

        _ => panic!("Expected Pipeline or ShCommand node, got some other ASTNode type"),
    }
}

pub fn exec_conditional(conditional: ASTNode) -> Result<ExitStatus, String> {
    debug!("Executing conditional ASTNode: {:?}", conditional);

    match conditional {
        ASTNode::Conditional { condition, body1, body2 } => {
            debug!("Evaluating condition of the conditional");

            let condition_status = execute(*condition)?.code();
            debug!("Condition status: {:?}", condition_status);

            if condition_status == Some(0) {
                debug!("Condition is true, executing body1");
                if let Some(body) = body1 {
                    return execute(*body);
                } else if let Some(body) = body2 {
                    debug!("Condition is false, executing body2");
                    return execute(*body);
                } else {
                    return Err("Found empty if statement".to_string());
                }
            }
        }
        _ => panic!("Expected Conditional, found some other ASTNode type"),
    }

    debug!("Condition did not evaluate to true, returning error");
    Err("Condition did not evaluate to true".to_string())
}
