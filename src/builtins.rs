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

#[cfg(test)]
mod tests {
    use super::*;
    use std::process::Command;

    #[test]
    fn cd_no_args() {
        let mut environment = Environment::new();

        let result = cd(vec![], &mut environment);
        assert_eq!(result, Ok(CommandOutput::simple_success()));
    }

    #[test]
    fn cd_relative_path() {
        let mut environment = Environment::new();
        let _status = Command::new("mkdir")
            .arg("-p")
            .arg("/tmp/subdir")
            .status()
            .expect("Failed to execute command");

        let _ = cd(vec!["/tmp".to_string()], &mut environment);
        let result = cd(vec!["subdir".to_string()], &mut environment);
        let _status = Command::new("rm")
            .arg("-r")
            .arg("/tmp/subdir")
            .status()
            .expect("Failed to execute command");

        assert_eq!(result, Ok(CommandOutput::simple_success()));
    }

    #[test]
    fn cd_directory_not_found() {
        let mut environment = Environment::new();

        let result = cd(vec!["nonexistent_dir".to_string()], &mut environment);
        assert_eq!(result, Err((1, "No such file or directory (os error 2)".to_string())));
    }

    #[test]
    fn cd_bad_permissions() {
        let mut environment = Environment::new();

        let result = cd(vec!["/root".to_string()], &mut environment);
        assert_eq!(result, Err((1, "Permission denied (os error 13)".to_string())));
    }

    #[test]
    fn cd_long_path() {
        let mut environment = Environment::new();
        let mut path = "/tmp/directory0".to_string();
        let _ = Command::new("mkdir")
            .arg("-p")
            .arg(&path)
            .spawn()
            .expect("process failed")
            .wait();
        for i in 1 .. 100 {
            path += format!("/directory{}",i).as_str();
            let _ = Command::new("mkdir") .arg("-p")
                .arg(&path)
                .spawn()
                .expect("process failed")
                .wait();
        }

        let result = cd(vec![path], &mut environment);
            let _ = Command::new("rm")
                .arg("-r")
                .arg("/tmp/directory0/")
                .spawn()
                .expect("process failed")
                .wait();
        assert_eq!(result, Ok(CommandOutput::simple_success()));

    }

    #[test]
    fn echo_extra_whitespace() {
        let result = echo(vec!["Hello".to_string(), "world".to_string()], vec![]);
        assert_eq!(result, Ok(CommandOutput::new(0, Some(b"Hello world\n".to_vec()), None)));
    }

    #[test]
    fn echo_no_input() {
        let result = echo(vec![], vec![]);
        assert_eq!(result, Ok(CommandOutput::new(0, Some(b"\n".to_vec()), None)));
    }

    #[test]
    fn echo_redirection_filetest() {
        let mut environment = Environment::new();
        let redir = Redirection::new(RedirectionType::Output, "output.txt".to_string());
        let result = echo(vec!["Test".to_string()], vec![redir]);

        assert_eq!(result, Ok(CommandOutput::new(0, Some(b"Test\n".to_vec()), None)));
    }

    #[test]
    fn echo_redirection_stdout_test() {
        let result = echo(vec!["Test".to_string()], vec![]);
        assert_eq!(result, Ok(CommandOutput::new(0, Some(b"Test\n".to_vec()), None)));
    }

    #[test]
    fn echo_very_long_input() {
        let long_input = "words".repeat(10000);
        let result = echo(vec![long_input.clone()], vec![]);
        assert_eq!(result, Ok(CommandOutput::new(0, Some(format!("{}\n",long_input).into_bytes()), None)))
    }

    #[test]
    fn echo_escape_sequences() {}

    #[test]
    fn echo_special_characters() {
        let result = echo(vec!["Test$".to_string(), "this*".to_string(), "input?".to_string()], vec![]);
        assert_eq!(result, Ok(CommandOutput::new(0, Some(b"Test$ this* input?\n".to_vec()), None)));
    }

    #[test]
    fn export_valid_input() {
        let mut environment = Environment::new();
        let result = export(vec!["VAR=value".to_string()], &mut environment);

        assert_eq!(result, Ok(CommandOutput::simple_success()));
        assert_eq!(environment.get_var("VAR"), Some(&"value".to_string()));
    }

    #[test]
    fn export_bad_input() {
        let mut environment = Environment::new();
        let result = export(vec!["VAR".to_string()], &mut environment);

        assert_eq!(result, Err((1, "Invalid input for export builtin".to_string())));
    }

    #[test]
    fn export_empty_value() {
        let mut environment = Environment::new();
        let result = export(vec!["VAR=".to_string()], &mut environment);

        assert_eq!(result, Ok(CommandOutput::simple_success()));
        assert_eq!(environment.get_var("VAR"), Some(&"".to_string()));
    }

    #[test]
    fn export_whitespace_in_var_name() {
        let mut environment = Environment::new();
        let result = export(vec!["variable name=value".to_string()], &mut environment);

        assert_eq!(result, Err((1, "Invalid input for export builtin".to_string())));
        assert_eq!(environment.get_var("variable name"),None);
    }

    #[test]
    fn export_multiple_variables() {
        let mut environment = Environment::new();
        let result = export(vec![
            "var1=val1".to_string(),
            "var2=val2".to_string(),
            "var3=val3".to_string()],
            &mut environment);

        assert_eq!(result, Ok(CommandOutput::simple_success()));
        assert_eq!(environment.get_var("var1"),Some(&"val1".to_string()));
        assert_eq!(environment.get_var("var2"),Some(&"val2".to_string()));
        assert_eq!(environment.get_var("var3"),Some(&"val3".to_string()));
    }

    #[test]
    fn export_non_ascii_chars() {}

    #[test]
    fn export_very_long_input() {}

    #[test]
    fn alias_simple_test() {
        let mut environment = Environment::new();
        let result = alias(vec!["alias_name=command".to_string()], &mut environment);

        assert_eq!(result, Ok(CommandOutput::simple_success()));
        assert_eq!(environment.get_alias("alias_name"), Some(&"command".to_string()));
    }

    #[test]
    fn alias_no_args() {
        let mut environment = Environment::new();
        let result = alias(vec![], &mut environment);

        assert_eq!(result, Err((1, "alias takes exactly one argument".to_string())));
    }

    #[test]
    fn alias_overwrite() {}

    #[test]
    fn alias_whitespace_in_alias_name() {}

    #[test]
    fn alias_invalid_chars_in_alias_name() {}

    #[test]
    fn alias_very_long_input() {}

    //#[test]
    //fn exit_simple_test() {
        //let result = exit(vec![]);
        // This will depend on your actual implementation, so ensure `exit()` works correctly in your program.
        //assert_eq!(result, Err((1, "Exit not yet implemented".to_string())));
    //}

    #[test]
    fn exit_non_integer_arg() {}

    #[test]
    fn exit_negative_integer() {}

    #[test]
    fn exit_multiple_args() {}

    #[test]
    fn exit_boundary_codes() {}

    //#[test]
    //fn unset_simple_test() {
        //let mut environment = Environment::new();
        //environment.set_var("VAR", "value");
//
        //let result = unset(vec!["VAR".to_string()], &mut environment);
        //assert_eq!(result, Ok(CommandOutput::simple_success()));
        //assert_eq!(environment.get_var("VAR"), None);
    //}

    #[test]
    fn unset_multiple_vars() {}

    //#[test]
    //fn unset_nonexistent_var() {
        //let mut environment = Environment::new();
//
        //let result = unset(vec!["NONEXISTENT_VAR".to_string()], &mut environment);
        //assert_eq!(result, Err((1, "Variable not found".to_string())));
    //}

    #[test]
    fn unset_readonly_var() {}
}
