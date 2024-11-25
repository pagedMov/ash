use std::fs::{OpenOptions, File};
use std::path::Path;
use std::io::{self, Read, Write};
use std::env::{self, set_current_dir};
use std::os::unix::process::ExitStatusExt;
use std::process::{Child, ChildStdout, Command, ExitStatus, Stdio};

use crate::helper;
use crate::environment::{self, Environment};
use crate::parser::{RedirectionType, ASTNode};

pub fn node_walk(nodes: Vec<ASTNode>, environment: &mut Environment) -> Result<(), (ExitStatus,String)> {
    for node in nodes {
        match &node {
            ASTNode::Builtin { name: _, args: _, redirs: _ } => {
                let _ = exec_builtin(node, environment, false);
            }
            ASTNode::ShCommand { name: _, args: _, redirs: _ } => {
                let _ = exec_cmd(node, environment);
            }
            ASTNode::Pipeline { commands: _ } => {
                let _ = exec_cmd(node, environment);
            },
            ASTNode::Conditional { condition: _, body1: _, body2: _ } => {
                let _ = exec_conditional(node, environment);
            },
            _ => panic!("Unexpected node type found while walking"),
        }
    }
    Ok(())
}

pub fn mk_process(command: ASTNode, stdin: Option<Vec<u8>>, pipeout: bool) -> Result<Child, String> {

    match command {
        ASTNode::ShCommand { name, args, redirs } => {
            let mut child = Command::new(name.clone());
            child.args(args);

            for redir in redirs {
                match redir.get_direction() {
                    RedirectionType::Input => {
                        let file = OpenOptions::new()
                            .read(true)
                            .open(redir.get_filepath())
                            .map_err(|e| e.to_string())?;
                        if stdin.is_none() {
                            child.stdin(Stdio::from(file));
                        }
                    }
                    RedirectionType::Output => {
                        let file = OpenOptions::new()
                            .create(true)
                            .truncate(true)
                            .write(true)
                            .open(redir.get_filepath())
                            .map_err(|e| e.to_string())?;
                        child.stdout(Stdio::from(file));
                    }
                }
            }

            if stdin.is_some() {
                child.stdin(Stdio::piped());
            }

            if pipeout {
                child.stdout(Stdio::piped());
            }

            let mut child = child.spawn().map_err(|e| e.to_string())?;

            if let Some(input) = stdin {
                let _child_stdin = child.stdin.as_mut()
                    .unwrap()
                    .write_all(&input);
            }

            Ok(child)
        }
        _ => Err("Expected ShCommand, got some other ASTNode from the parser".to_string()),
    }
}

pub fn exec_builtin (node: ASTNode, environment: &mut Environment, pipeout: bool) -> Result<(ExitStatus,Option<String>), (ExitStatus,String)> {
    match &node {
        ASTNode::Builtin { name, args, redirs } => {
            match name.as_str() {
                "cd" => {
                    if let Some(path) = args.first() {
                        // Use the provided argument as the new directory
                        env::set_current_dir(Path::new(path)).map_err(|e| (ExitStatus::from_raw(1),e.to_string()))?;
                        Ok((ExitStatus::from_raw(0),None))
                    } else {
                        // Fall back to HOME or root directory
                        let root = "/".to_string();
                        let fallback_dir = environment.get_var("HOME").unwrap_or(&root);
                        env::set_current_dir(Path::new(&fallback_dir)).map_err(|e| (ExitStatus::from_raw(1),e.to_string()))?;
                        Ok((ExitStatus::from_raw(0),None))
                    }
                },
                "echo" => {
                    let output = args.join(" "); // Combine all arguments with spaces
                    if !redirs.is_empty() {
                        for redir in redirs {
                            if let RedirectionType::Output = redir.get_direction() {
                                let mut file = OpenOptions::new()
                                    .create(true)
                                    .truncate(true)
                                    .write(true)
                                    .open(redir.get_filepath())
                                    .map_err(|e| (ExitStatus::from_raw(1), e.to_string()))?;

                                let _ = file.write_all(output.as_bytes());
                            }
                        }

                    } else if !pipeout {
                        println!("{}",output);
                    }

                    Ok((ExitStatus::from_raw(0),Some(output)))
                },
                "export" => {
                    for arg in args {
                        if helper::is_var_declaration(arg.to_string()) {
                            if let Some((key, value)) = arg.split_once('=') {
                                environment.export_var(key, value);
                            } else {
                                return Err((ExitStatus::from_raw(1),"Error parsing key-value pair".to_string()));
                            }
                        } else {
                            return Err((ExitStatus::from_raw(1), "Invalid input for export builtin".to_string()));
                        }
                    }
                    Ok((ExitStatus::from_raw(0),None))
                },
                "alias" => { todo!() },
                "unset" => { todo!() },
                "exit" => { todo!() },
                _ => { todo!() }
            }
        }
        _ => panic!("Expected builtin token, got something else")
    }
}

pub fn exec_cmd(node: ASTNode, environment: &mut Environment) -> Result<ExitStatus, (ExitStatus,String)> {

    match &node {
        ASTNode::ShCommand { name: _, args: _, redirs: _ } => {
            let mut child = mk_process(node, None, false).map_err(|e| (ExitStatus::from_raw(1),e))?;
            let status = child.wait().map_err(|e| (ExitStatus::from_raw(1),e.to_string()))?;
            Ok(status)
        }

        ASTNode::Pipeline { commands } => {
            let mut stdin: Option<Vec<u8>> = None; // Standard input
            let mut pipeout = true;
            let mut command_iter = commands.iter().peekable();

            while let Some(command) = command_iter.next() {
                if command_iter.peek().is_none() {
                    pipeout = false;
                }

                if let ASTNode::Builtin { .. } = command {
                    let (_status,output) = exec_builtin(command.clone(), environment, pipeout)?;
                    if let Some(stdout) = output { // Get stdout from shell builtin
                        stdin = Some(stdout.into_bytes());
                    }

                    continue;
                }

                let mut child = mk_process(command.clone(), stdin.clone(), pipeout).map_err(|e| (ExitStatus::from_raw(1),e))?;

                child.wait().map_err(|e| (ExitStatus::from_raw(1),e));

                if let Some(output) = child.stdout.as_mut() { // Get stdout from shell command
                    let mut stdout_buffer: Vec<u8> = vec![];
                    let _ = output.read_to_end(&mut stdout_buffer);
                    stdin = Some(stdout_buffer);
                }
            }

            Ok(ExitStatus::from_raw(0))
        }

        ASTNode::Conditional { condition: _, body1: _, body2: _ } => {
            Ok(exec_conditional(node, environment)?)
        }

        _ => panic!("Expected Pipeline or ShCommand node, got some other ASTNode type"),
    }
}

pub fn exec_conditional(conditional: ASTNode, environment: &mut Environment) -> Result<ExitStatus, (ExitStatus,String)> {

    match conditional {
        ASTNode::Conditional { condition, body1, body2 } => {

            let condition_status = exec_cmd(*condition, environment)?.code();

            if condition_status == Some(0) {
                if let Some(body) = body1 {
                    exec_cmd(*body, environment)
                } else if let Some(body) = body2 {
                    exec_cmd(*body, environment)
                } else {
                    Err((ExitStatus::from_raw(1),"Found empty if statement".to_string()))
                }
            } else if let Some(body) = body2 {
                exec_cmd(*body, environment)
            } else {
                Err((ExitStatus::from_raw(1),"Found empty if statement".to_string()))
            }
        }
        _ => panic!("Expected Conditional, found some other ASTNode type"),
    }
}
