use log::{debug, trace};
use std::fs::File;
use std::path::Path;
use std::io::{self, Write};
use std::env::{self, set_current_dir};
use std::os::unix::process::ExitStatusExt;
use crate::environment::{self, Environment};
use crate::parser::{RedirectionType, ASTNode};
use std::process::{ChildStdout, Command, ExitStatus, Stdio};

pub fn node_walk(nodes: Vec<ASTNode>, environment: &Environment) {
    for node in nodes {
        match &node {
            ASTNode::Builtin { name: _, args: _, redirs: _ } => {
                debug!("Executing ShCommand node");
                let _ = exec_builtin(node, environment, false);
            }
            ASTNode::ShCommand { name: _, args: _, redirs: _ } => {
                debug!("Executing ShCommand node");
                let _ = exec_cmd(node, environment);
            }
            ASTNode::Pipeline { commands: _ } => {
                debug!("Executing Pipeline node");
                let _ = exec_cmd(node, environment);
            },
            ASTNode::Conditional { condition: _, body1: _, body2: _ } => {
                debug!("Executing Conditional node");
                let _ = exec_conditional(node, environment);
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

pub fn exec_builtin (node: ASTNode, environment: &Environment, pipeout: bool) -> Result<(ExitStatus,Option<String>), String> {
    match &node {
        ASTNode::Builtin { name, args, redirs: _ } => {
            match name.as_str() {
                "cd" => {
                    if let Some(path) = args.first() {
                        // Use the provided argument as the new directory
                        env::set_current_dir(Path::new(path)).map_err(|e| e.to_string())?;
                        Ok((ExitStatus::from_raw(0),None))
                    } else {
                        // Fall back to HOME or root directory
                        let root = "/".to_string();
                        let fallback_dir = environment.get_var("HOME").unwrap_or(&root);
                        env::set_current_dir(Path::new(&fallback_dir)).map_err(|e| e.to_string())?;
                        Ok((ExitStatus::from_raw(0),None))
                    }
                },
                "echo" => {
                    let output = args.join(" "); // Combine all arguments with spaces
                    debug!("Echoing: {}",output);
                    if !pipeout { println!("{}",output); }
                    Ok((ExitStatus::from_raw(0),Some(output)))
                },
                "exit" => { todo!() },
                "export" => { todo!() },
                "alias" => { todo!() },
                "unset" => { todo!() },
                _ => { todo!() }
            }
        }
        _ => panic!("Expected builtin token, got something else")
    }
}

pub fn exec_cmd(node: ASTNode, environment: &Environment) -> Result<ExitStatus, String> {
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
            let mut stdin: Option<ChildStdout> = None; // Standard input
            let mut strin: Option<String> = None; // String input for pipes, used by builtins like echo
            let mut pipeout = true;
            let mut command_iter = commands.iter().peekable();

            while let Some(command) = command_iter.next() {
                if command_iter.peek().is_none() {
                    debug!("Last command found, not piping output");
                    pipeout = false;
                }
                debug!("Command_iter: {:?}", command_iter);

                if let ASTNode::Builtin { .. } = command {
                    let (_status,output) = exec_builtin(command.clone(), environment, pipeout)?;
                    if let Some(stdout) = output {
                        strin = Some(stdout);
                        stdin = None;
                    }

                    continue;
                }

                debug!("Processing command in pipeline: {:?}", command);
                let mut process = mk_process(command.clone(), stdin, pipeout)?;

                if strin.is_some() { // Overwrite stdin defined in mk_process
                    process.stdin(Stdio::piped());
                }

                debug!("Spawning child process for pipeline");
                let mut child = process.spawn().map_err(|e| e.to_string())?;

                if let Some(stringinput) = strin { // From builtins like echo
                    debug!("Writing bytes to child stdin from string: {}",stringinput);
                    debug!("Child process: {:?}",child);
                    let _child_stdin = child.stdin.as_mut()
                        .unwrap()
                        .write_all(stringinput.as_bytes());
                    strin = None;
                }


                debug!("Waiting for child process to finish");
                child.wait().map_err(|e| e.to_string())?;
                stdin = child.stdout.take();
            }

            Ok(ExitStatus::from_raw(0))
        }

        ASTNode::Conditional { condition: _, body1: _, body2: _ } => {
            debug!("Executing conditional statement");
            exec_conditional(node, environment).map_err(|e| e.to_string())
        }

        _ => panic!("Expected Pipeline or ShCommand node, got some other ASTNode type"),
    }
}

pub fn exec_conditional(conditional: ASTNode, environment: &Environment) -> Result<ExitStatus, String> {
    debug!("Executing conditional ASTNode: {:?}", conditional);

    match conditional {
        ASTNode::Conditional { condition, body1, body2 } => {
            debug!("Evaluating condition of the conditional");

            let condition_status = exec_cmd(*condition, environment)?.code();
            debug!("Condition status: {:?}", condition_status);

            if condition_status == Some(0) {
                debug!("Condition is true, executing body1");
                if let Some(body) = body1 {
                    exec_cmd(*body, environment)
                } else if let Some(body) = body2 {
                    debug!("Condition is false, executing body2");
                    exec_cmd(*body, environment)
                } else {
                    Err("Found empty if statement".to_string())
                }
            } else if let Some(body) = body2 {
                debug!("Condition is false, executing body2");
                exec_cmd(*body, environment)
            } else {
                Err("Found empty if statement".to_string())
            }
        }
        _ => panic!("Expected Conditional, found some other ASTNode type"),
    }
}
