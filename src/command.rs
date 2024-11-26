use log::{debug, error, info, trace, warn}; // Import logging macros
use std::process::{Command, Stdio};

use crate::builtins;
use crate::environment::Environment;
use crate::helper;
use crate::parser::{ASTNode, RedirectionType};

#[derive(Debug,PartialEq)]
pub struct CommandOutput {
    exit_code: i32,
    stdout: Vec<u8>,
    stderr: Vec<u8>,
}

impl CommandOutput {
    pub fn new(exit_code: i32, stdout: Option<Vec<u8>>, stderr: Option<Vec<u8>>) -> Self {
        trace!("Creating new CommandOutput with exit_code: {}", exit_code);
        let stdout = stdout.unwrap_or_default();
        let stderr = stderr.unwrap_or_default();
        CommandOutput { exit_code, stdout, stderr }
    }

    pub fn simple_success() -> Self {
        debug!("Creating a simple success CommandOutput");
        CommandOutput { exit_code: helper::succeed(), stdout: vec![], stderr: vec![] }
    }

    pub fn exit_code(&self) -> i32 {
        self.exit_code
    }

    pub fn stdout(&self) -> &Vec<u8> {
        &self.stdout
    }

    pub fn stderr(&self) -> &Vec<u8> {
        &self.stderr
    }
}

pub fn node_walk(
    nodes: Vec<ASTNode>,
    environment: &mut Environment,
) -> Result<(), (i32, String)> {
    info!("Starting AST node walk");
    for node in nodes {
        debug!("Processing AST node: {:?}", node);
        match &node {
            ASTNode::Builtin { .. } | ASTNode::ShCommand { .. } | ASTNode::Pipeline { .. } => {
                trace!("Executing shell command or pipeline");
                let command = exec_cmd(&node, environment, None)?;
                let output = command.stdout().to_vec();
                helper::write_stdout(output)?;
            }
            ASTNode::Conditional { .. } => {
                trace!("Executing conditional statement");
                let command = exec_conditional(node, environment)?;
                let output = command.stdout().to_vec();
                helper::write_stdout(output)?;
            }
            _ => {
                error!("Unexpected AST node type: {:?}", node);
                panic!("Unexpected node type found while walking");
            }
        }
    }
    trace!("Completed AST node walk");
    Ok(())
}

pub fn mk_process(
    command: ASTNode,
    stdin: Option<Vec<u8>>,
) -> Result<CommandOutput, (i32, String)> {
    if let ASTNode::ShCommand { name, args, redirs } = command {
        info!("Creating process for command: {}", name);
        debug!("Command args: {:?}, Redirections: {:?}", args, redirs);

        let mut child = Command::new(name.clone());
        child.args(args);

        if stdin.is_some() {
            debug!("Piping input to the command");
            child.stdin(Stdio::piped());
        }
        debug!("Piping stdout for further processing");
        child.stdout(Stdio::piped());

        let mut child = child.spawn().map_err(|e| {
            error!("Failed to spawn child process: {}", e);
            helper::fail(&e.to_string())
        })?;

        if let Some(input) = stdin {
            trace!("Writing piped input to child process");
            helper::write_bytes(child.stdin.as_mut().unwrap(), &input)?
        }

        for redir in redirs {
            match redir.get_direction() {
                RedirectionType::Output => {
                    debug!("Setting up output redirection: {}", redir.get_filepath());
                    let raw_stdout = child.stdout.as_mut().unwrap();
                    let buffer = helper::read_bytes(raw_stdout)?;
                    helper::redirect_output(buffer, redir)?;
                }
                RedirectionType::Input => {
                    debug!("Setting up input redirection: {}", redir.get_filepath());
                    let raw_stdin = child.stdin.as_mut().unwrap();
                    let buffer = helper::redirect_input(redir)?;
                    helper::write_bytes(raw_stdin, &buffer)?;
                }
                RedirectionType::Error => {
                    debug!("Setting up error redirection: {}", redir.get_filepath());
                    let raw_stderr = child.stderr.as_mut().unwrap();
                    let buffer = helper::read_bytes(raw_stderr)?;
                    helper::redirect_output(buffer, redir)?;
                }
            }
        }

        let status = child.wait().map_err(|e| {
            error!("Failed while waiting for child process: {}", e);
            helper::fail(&e.to_string())
        })?;

        debug!("Child process finished with exit code: {}", status.code().unwrap());

        let mut stdout_buffer = vec![];
        if let Some(output) = child.stdout.as_mut() {
            trace!("Reading stdout of child process");
            stdout_buffer = helper::read_bytes(output)?;
        }

        let mut stderr_buffer = vec![];
        if let Some(error) = child.stderr.as_mut() {
            trace!("Reading stderr of child process");
            stderr_buffer = helper::read_bytes(error)?;
        }

        let output = CommandOutput::new(status.code().unwrap(), Some(stdout_buffer), Some(stderr_buffer));
        Ok(output)
    } else {
        error!("Unexpected ASTNode type passed to mk_process");
        Err(helper::fail("Expected ShCommand, got some other ASTNode from the parser"))
    }
}

pub fn exec_cmd(
    node: &ASTNode,
    environment: &mut Environment,
    mut stdin: Option<Vec<u8>>
) -> Result<CommandOutput, (i32, String)> {
    match node {
        ASTNode::Builtin { name, args, redirs } => {
            trace!("Executing builtin command within pipeline");
            let output = match name.as_str() {
                "cd" => builtins::cd(args.to_vec(), environment)?,
                "echo" => builtins::echo(args.to_vec(), redirs.to_vec())?,
                "export" => builtins::export(args.to_vec(), environment)?,
                "alias" => builtins::alias(args.to_vec(), environment)?,
                "exit" => builtins::exit(args.to_vec())?,
                "unset" => builtins::unset(args.to_vec(), environment)?,
                _ => {
                    error!("Unknown builtin command: {}",name);
                    panic!("Expected builtin, got {}",name);
                }
            };
            Ok(output)
        }
        ASTNode::ShCommand { .. } => {
            info!("Executing shell command");
            let output = mk_process(node.clone(), stdin)?;
            debug!("Shell command executed with exit code: {}", output.exit_code());
            Ok(output)
        }
        ASTNode::Pipeline { commands } => {
            info!("Executing pipeline with {} commands", commands.len());
            let mut final_output = CommandOutput::simple_success(); // placeholder

            for (i, command) in commands.iter().enumerate() {
                debug!("Processing command {} in pipeline", i + 1);

                let output: CommandOutput;
                if let Some(input) = &stdin {
                    output = exec_cmd(command, environment, Some(input.to_vec()))?;
                } else {
                    output = exec_cmd(command, environment, None)?;
                }

                let stdout = output.stdout();
                let _stderr = output.stderr();

                stdin = Some(output.stdout().to_vec());

                final_output = output;
            }

            Ok(final_output)
        }
        ASTNode::Conditional { .. } => {
            info!("Executing conditional statement");
            exec_conditional(node.clone(), environment)
        }
        _ => {
            error!("Unexpected ASTNode type: {:?}", node);
            panic!("Expected Pipeline or ShCommand node, got some other ASTNode type");
        }
    }
}

pub fn exec_conditional(
    conditional: ASTNode,
    environment: &mut Environment,
) -> Result<CommandOutput, (i32, String)> {
    if let ASTNode::Conditional {
        condition,
        body1,
        body2,
    } = conditional
    {
        info!("Evaluating conditional statement");
        let condition_status = exec_cmd(&condition, environment, None)?;
        debug!("Condition evaluated with status: {}", condition_status.exit_code());

        if condition_status.exit_code() == 0 {
            trace!("Condition succeeded; executing body1");
            if let Some(body) = body1 {
                return exec_cmd(&body, environment, None);
            }
            trace!("body1 is empty; falling back to body2 (if any)");
            if let Some(body) = body2 {
                return exec_cmd(&body, environment, None);
            }
            warn!("Empty conditional statement encountered");
            return Err(helper::fail("Found empty if statement"));
        }

        trace!("Condition failed; executing body2 (if any)");
        if let Some(body) = body2 {
            return exec_cmd(&body, environment, None);
        }

        warn!("Empty conditional statement encountered");
        return Err(helper::fail("Found empty if statement"));
    }

    error!("Unexpected ASTNode type passed to exec_conditional");
    panic!("Expected Conditional, found some other ASTNode type");
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{Redirection, RedirectionType};
    use std::fs::OpenOptions;
    use std::path::Path;

    fn setup_environment() -> Environment {
        Environment::new()
    }

    #[test]
    fn test_builtins_cd() {
        let mut environment = setup_environment();
        let node = ASTNode::Builtin {
            name: "cd".to_string(),
            args: vec!["/".to_string()],
            redirs: vec![],
        };

        let result = exec_cmd(&node, &mut environment, None);
        assert!(result.is_ok());
        assert_eq!(std::env::current_dir().unwrap(), Path::new("/"));
    }

    #[test]
    fn test_builtins_echo() {
        let mut environment = setup_environment();
        let node = ASTNode::Builtin {
            name: "echo".to_string(),
            args: vec!["Hello, World!".to_string()],
            redirs: vec![],
        };

        let result = exec_cmd(&node, &mut environment, None);
        assert!(result.is_ok());

        if let Ok(output) = result {
            assert_eq!(String::from_utf8(output.stdout().to_vec()).unwrap(), "Hello, World!\n");
        } else {
            panic!("Expected echo output, got none or error");
        }
    }

    #[test]
    fn test_builtins_export() {
        let mut environment = setup_environment();
        let node = ASTNode::Builtin {
            name: "export".to_string(),
            args: vec!["VAR=value".to_string()],
            redirs: vec![],
        };

        let result = exec_cmd(&node, &mut environment, None);
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

        let result = exec_cmd(&node, &mut environment, None);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().exit_code(), 0);
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

        let result = exec_cmd(&pipeline, &mut environment, None);
        assert!(result.is_ok());
        // Assuming "echo Hello | wc -w" outputs 1
        assert_eq!(result.unwrap().exit_code(), 0);
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

        let result = exec_cmd(&node, &mut environment, None);
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

        let result = exec_cmd(&node, &mut environment, None);
        assert!(result.is_ok());

        let mut file = OpenOptions::new().read(true).open(temp_file).unwrap();
        let contents = helper::read_bytes(&mut file).unwrap();
        println!("contents: {:?}",contents);
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

        let result = exec_cmd(&conditional, &mut environment, None);
        assert!(result.is_ok());
        // Assuming "echo Condition met" outputs properly
        assert_eq!(result.unwrap().exit_code(), 0);
    }
}
