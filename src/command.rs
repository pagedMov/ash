use std::env::{self};
use std::fs::OpenOptions;
use std::io::Write;
use std::os::unix::process::ExitStatusExt;
use std::path::Path;
use std::process::{Command, ExitStatus, Stdio};

use crate::environment::Environment;
use crate::helper;
use crate::parser::{ASTNode, RedirectionType};

pub fn node_walk(
    nodes: Vec<ASTNode>,
    environment: &mut Environment,
) -> Result<(), (ExitStatus, String)> {
    for node in nodes {
        match &node {
            ASTNode::Builtin { .. } => {
                let _ = exec_builtin(node, environment, false);
            }
            ASTNode::ShCommand { .. } | ASTNode::Pipeline { .. } => {
                let _ = exec_cmd(node, environment);
            }
            ASTNode::Conditional { .. } => {
                let _ = exec_conditional(node, environment);
            }
            _ => panic!("Unexpected node type found while walking"),
        }
    }
    Ok(())
}

pub fn mk_process(
    command: ASTNode,
    stdin: Option<Vec<u8>>,
    pipeout: bool,
) -> Result<(ExitStatus, Vec<u8>), (ExitStatus, String)> {
    if let ASTNode::ShCommand { name, args, redirs } = command {
        let mut child = Command::new(name.clone());
        child.args(args);

        for redir in &redirs {
            match redir.get_direction() {
                RedirectionType::Input => {
                    let file = OpenOptions::new()
                        .read(true)
                        .open(redir.get_filepath())
                        .map_err(|e| helper::fail(&e.to_string()))?;
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
                        .map_err(|e| helper::fail(&e.to_string()))?;
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

        let mut child = child.spawn().map_err(|e| helper::fail(&e.to_string()))?;

        if let Some(input) = stdin {
            helper::write_bytes(child.stdin.as_mut().unwrap(), &input)
                .map_err(|e| helper::fail(&e))?
        }

        let status = child.wait().map_err(|e| helper::fail(&e.to_string()))?;
        let mut stdout_buffer = vec![];

        if let Some(output) = child.stdout.as_mut() {
            stdout_buffer = helper::read_bytes(output).map_err(|e| helper::fail(&e))?;
        }

        let mut stderr_buffer = vec![];

        if let Some(error) = child.stderr.as_mut() {
            stderr_buffer = helper::read_bytes(error).map_err(|e| helper::fail(&e))?;
        }

        if !pipeout && redirs.is_empty() {
            helper::write_stdout(stdout_buffer.clone())?;
        }

        helper::write_stderr(stderr_buffer.clone())?;

        return Ok((status, stdout_buffer));
    }
    Err(helper::fail(
        "Expected ShCommand, got some other ASTNode from the parser",
    ))
}

pub fn exec_builtin(
    node: ASTNode,
    environment: &mut Environment,
    pipeout: bool,
) -> Result<(ExitStatus, Option<String>), (ExitStatus, String)> {
    if let ASTNode::Builtin { name, args, redirs } = &node {
        match name.as_str() {
            "cd" => {
                let path = args.first().map(String::as_str).unwrap_or_else(|| {
                    environment
                        .get_var("HOME")
                        .map(String::as_str)
                        .unwrap_or("/")
                });
                env::set_current_dir(Path::new(path))
                    .map_err(|e| (ExitStatus::from_raw(1), e.to_string()))?;
                Ok((helper::succeed(), None))
            }
            "echo" => {
                let output = args.join(" ") + "\n";
                if !redirs.is_empty() {
                    for redir in redirs {
                        if let RedirectionType::Output = redir.get_direction() {
                            let mut file = OpenOptions::new()
                                .create(true)
                                .truncate(true)
                                .write(true)
                                .open(redir.get_filepath())
                                .map_err(|e| {
                                    (ExitStatus::from_raw(1), format!("Echo failed: {}", e))
                                })?;
                            helper::write_bytes(&mut file, output.as_bytes())
                                .map_err(|e| helper::fail(&e))?
                        }
                    }
                } else if !pipeout {
                    helper::write_stdout(output.clone().into_bytes())?;
                }
                Ok((helper::succeed(), Some(output)))
            }
            "export" => {
                for arg in args {
                    if helper::is_var_declaration(arg.clone()) {
                        if let Some((key, value)) = arg.split_once('=') {
                            environment.export_var(key, value);
                        } else {
                            return Err(helper::fail("Error parsing key-value pair"));
                        }
                    } else {
                        return Err(helper::fail("Invalid input for export builtin"));
                    }
                }
                Ok((helper::succeed(), None))
            }
            "alias" => {
                if args.len() != 1 {
                    return Err(helper::fail("alias takes exactly one argument"));
                }
                let arg = args[0].clone();
                if helper::is_var_declaration(arg.clone()) {
                    if let Some((key, value)) = arg.split_once('=') {
                        environment.set_alias(key, value);
                    } else {
                        return Err(helper::fail("Error parsing key-value pair for alias"));
                    }
                } else {
                    return Err(helper::fail("Invalid argument format for alias"));
                }
                Ok((helper::succeed(), None))
            }
            "unset" => todo!(),
            "exit" => todo!(),
            _ => panic!("Expected a shell builtin, got: {}", name),
        }
    } else {
        panic!("Expected builtin token, got something else");
    }
}

pub fn exec_cmd(
    node: ASTNode,
    environment: &mut Environment,
) -> Result<ExitStatus, (ExitStatus, String)> {
    match &node {
        ASTNode::ShCommand { .. } => {
            let (status, _) = mk_process(node, None, false)?;
            Ok(status)
        }
        ASTNode::Pipeline { commands } => {
            let mut stdin: Option<Vec<u8>> = None;
            let mut pipeout: bool;
            let mut final_status = ExitStatus::from_raw(1);

            for command in commands.iter() {
                pipeout = commands.last() != Some(command);

                if let ASTNode::Builtin { .. } = command {
                    let (status, output) = exec_builtin(command.clone(), environment, pipeout)?;
                    if let Some(stdout) = output {
                        stdin = Some(stdout.into_bytes());
                    }
                    final_status = status;
                } else {
                    let (status, stdout) = mk_process(command.clone(), stdin.clone(), pipeout)?;
                    final_status = status;
                    stdin = Some(stdout);
                }
            }

            Ok(final_status)
        }
        ASTNode::Conditional { .. } => exec_conditional(node, environment),
        _ => panic!("Expected Pipeline or ShCommand node, got some other ASTNode type"),
    }
}

pub fn exec_conditional(
    conditional: ASTNode,
    environment: &mut Environment,
) -> Result<ExitStatus, (ExitStatus, String)> {
    if let ASTNode::Conditional {
        condition,
        body1,
        body2,
    } = conditional
    {
        let condition_status = exec_cmd(*condition, environment)?.code();

        if condition_status == Some(0) {
            if let Some(body) = body1 {
                return exec_cmd(*body, environment);
            }
            if let Some(body) = body2 {
                return exec_cmd(*body, environment);
            }
            return Err(helper::fail("Found empty if statement"));
        }

        if let Some(body) = body2 {
            return exec_cmd(*body, environment);
        }

        return Err(helper::fail("Found empty if statement"));
    }
    panic!("Expected Conditional, found some other ASTNode type");
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{Redirection, RedirectionType};

    fn setup_environment() -> Environment {
        Environment::new()
    }

    #[test]
    fn test_exec_builtin_cd() {
        let mut environment = setup_environment();
        let node = ASTNode::Builtin {
            name: "cd".to_string(),
            args: vec!["/".to_string()],
            redirs: vec![],
        };

        let result = exec_builtin(node, &mut environment, false);
        assert!(result.is_ok());
        assert_eq!(std::env::current_dir().unwrap(), Path::new("/"));
    }

    #[test]
    fn test_exec_builtin_echo() {
        let mut environment = setup_environment();
        let node = ASTNode::Builtin {
            name: "echo".to_string(),
            args: vec!["Hello, World!".to_string()],
            redirs: vec![],
        };

        let result = exec_builtin(node, &mut environment, false);
        assert!(result.is_ok());

        if let Ok((_status, Some(output))) = result {
            assert_eq!(output, "Hello, World!\n");
        } else {
            panic!("Expected echo output, got none or error");
        }
    }

    #[test]
    fn test_exec_builtin_export() {
        let mut environment = setup_environment();
        let node = ASTNode::Builtin {
            name: "export".to_string(),
            args: vec!["VAR=value".to_string()],
            redirs: vec![],
        };

        let result = exec_builtin(node, &mut environment, false);
        assert!(result.is_ok());
        assert_eq!(environment.get_var("VAR"), Some(&"value".to_string()));
    }

    #[test]
    fn test_exec_sh_command() {
        let mut environment = setup_environment();
        let node = ASTNode::ShCommand {
            name: "true".to_string(),
            args: vec![],
            redirs: vec![],
        };

        let result = exec_cmd(node, &mut environment);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().code(), Some(0));
    }

    #[test]
    fn test_exec_pipeline() {
        let mut environment = setup_environment();
        let pipeline = ASTNode::Pipeline {
            commands: vec![
                ASTNode::ShCommand {
                    name: "echo".to_string(),
                    args: vec!["Hello".to_string()],
                    redirs: vec![],
                },
                ASTNode::ShCommand {
                    name: "wc".to_string(),
                    args: vec!["-w".to_string()],
                    redirs: vec![],
                },
            ],
        };

        let result = exec_cmd(pipeline, &mut environment);
        assert!(result.is_ok());
        // Assuming "echo Hello | wc -w" outputs 1
        assert_eq!(result.unwrap().code(), Some(0));
    }

    #[test]
    fn test_builtin_redirection_output() {
        let mut environment = setup_environment();
        let temp_file = "/tmp/test_output";

        let node = ASTNode::Builtin {
            name: "echo".to_string(),
            args: vec!["Hello".to_string()],
            redirs: vec![Redirection::new(
                RedirectionType::Output,
                temp_file.to_string(),
            )],
        };

        let result = exec_builtin(node, &mut environment, false);
        assert!(result.is_ok());

        let mut file = OpenOptions::new().read(true).open(temp_file).unwrap();
        let contents = helper::read_bytes(&mut file).unwrap();
        assert_eq!(String::from_utf8(contents).unwrap().trim(), "Hello");

        // Cleanup
        std::fs::remove_file(temp_file).unwrap();
    }

    #[test]
    fn test_command_redirection_output() {
        let mut environment = setup_environment();
        let temp_file = "/tmp/test_output2";

        let node = ASTNode::ShCommand {
            name: "printf".to_string(),
            args: vec!["Hello, World!\n".to_string()],
            redirs: vec![Redirection::new(
                RedirectionType::Output,
                temp_file.to_string(),
            )],
        };

        let result = exec_cmd(node, &mut environment);
        assert!(result.is_ok());

        let mut file = OpenOptions::new().read(true).open(temp_file).unwrap();
        let contents = helper::read_bytes(&mut file).unwrap();
        assert_eq!(String::from_utf8(contents).unwrap().trim(), "Hello, World!");

        // Cleanup
        std::fs::remove_file(temp_file).unwrap();
    }

    #[test]
    fn test_conditional_execution() {
        let mut environment = setup_environment();

        let condition = Box::new(ASTNode::ShCommand {
            name: "true".to_string(),
            args: vec![],
            redirs: vec![],
        });

        let body1 = Box::new(ASTNode::ShCommand {
            name: "echo".to_string(),
            args: vec!["Condition met".to_string()],
            redirs: vec![],
        });

        let body2 = Box::new(ASTNode::ShCommand {
            name: "echo".to_string(),
            args: vec!["Condition not met".to_string()],
            redirs: vec![],
        });

        let conditional = ASTNode::Conditional {
            condition,
            body1: Some(body1),
            body2: Some(body2),
        };

        let result = exec_cmd(conditional, &mut environment);
        assert!(result.is_ok());
        // Assuming "echo Condition met" outputs properly
        assert_eq!(result.unwrap().code(), Some(0));
    }
}
