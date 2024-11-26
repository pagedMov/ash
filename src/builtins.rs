use crate::command::CommandOutput;
use crate::parser::{Redirection, RedirectionType};
use crate::environment::Environment;
use crate::helper;
use std::path::Path;
use std::env;
use log::{info,trace,error,debug};

pub fn cd(args: Vec<String>, environment: &mut Environment) -> Result<CommandOutput,(i32,String)> {
    debug!("Executing 'cd' with args: {:?}", args);
    let path = args.first().map(String::as_str).unwrap_or_else(|| {
        environment
            .get_var("HOME")
            .map(String::as_str)
            .unwrap_or("/")
    });
    env::set_current_dir(Path::new(path))
        .map_err(|e| {
            error!("Failed to change directory: {}", e);
            helper::fail(&e.to_string())
        })?;
    Ok(CommandOutput::simple_success())
}

pub fn echo(args: Vec<String>, redirs: Vec<Redirection>) -> Result<CommandOutput,(i32,String)> {
    debug!("Executing 'echo' with args: {:?}", args);
    let output = args.join(" ") + "\n";
    if !redirs.is_empty() {
        trace!("Handling output redirections for 'echo'");
        for redir in redirs {
            if let RedirectionType::Output = redir.get_direction() {
                helper::redirect_output(output.clone().into_bytes(), redir.clone())?;
            }
        }
    }
    Ok(CommandOutput::new(helper::succeed(), Some(output.into_bytes()), None))
}

pub fn export(args: Vec<String>, environment: &mut Environment) -> Result<CommandOutput,(i32,String)> {
    for arg in args {
        if helper::is_var_declaration(arg.clone()) {
            if let Some((key, value)) = arg.split_once('=') {
                info!("Exported {} with value {}",key,value);
                environment.export_var(key, value);
            } else {
                return Err(helper::fail("Error parsing key-value pair"));
            }
        } else {
            return Err(helper::fail("Invalid input for export builtin"));
        }
    }
    let output = CommandOutput::simple_success();
    Ok(output)
}

pub fn alias(args: Vec<String>, environment: &mut Environment) -> Result<CommandOutput,(i32,String)> {
    if args.len() != 1 {
        return Err(helper::fail("alias takes exactly one argument"));
    }
    let arg = args[0].clone();
    if helper::is_var_declaration(arg.clone()) {
        if let Some((key, value)) = arg.split_once('=') {
            info!("Created alias {} with value {}",key,value);
            environment.set_alias(key, value);
        } else {
            return Err(helper::fail("Error parsing key-value pair for alias"));
        }
    } else {
        return Err(helper::fail("Invalid argument format for alias"));
    }
    let output = CommandOutput::simple_success();
    Ok(output)
}

pub fn exit(args: Vec<String>) -> Result<CommandOutput,(i32,String)> {
    todo!()
}

pub fn unset(args: Vec<String>, environment: &mut Environment) -> Result<CommandOutput,(i32,String)> {
    todo!()
}
