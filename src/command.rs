use std::fs::File;
use std::io::{self};
use std::collections::VecDeque;
use std::process::{ChildStdout, Command, Stdio};
use std::path::PathBuf;
use log::{trace, error};

pub fn exec_command(pipein: Option<ChildStdout>, mut tokens: VecDeque<String>) -> Option<ChildStdout> {
    let mut curr_cmd: Vec<String> = vec![];

    let mut filepath: Option<PathBuf> = None;
    while let Some(token) = tokens.pop_front() {
        match token.as_str() {
            "|" => break,
            ">" => { filepath = Some(tokens.pop_front().unwrap().into()) },
            _ => {
                curr_cmd.push(token.to_string());
            }
        }
    }
    let cmd = curr_cmd.remove(0);
    let args = curr_cmd;
    let stdin = if let Some(input) = pipein {
        Stdio::from(input)
    } else {
        Stdio::inherit()
    };

    let stdout = if !tokens.is_empty() || filepath.is_some() {
        Stdio::piped()
    } else {
        Stdio::inherit()
    };
    let mut child = match Command::new(cmd)
        .args(args)
        .stdout(stdout)
        .stdin(stdin)
        .spawn() {
            Ok(child) => {
                trace!("Command spawned successfully");
                child
            },
            Err(e) => {
                error!("Error spawning command: {}", e);
                return None;
            }
        };
    trace!("Waiting for command to finish...");
    if let Err(e) = child.wait() {
        error!("Error waiting for command: {}", e);
    }
    if !tokens.is_empty() {
        exec_command(child.stdout, tokens)
    } else {
        // Handle output redirection to a file
        if let Some(path) = filepath {
            match File::create(path) {
                Ok(mut file) => {
                    // Unwrap the stdout and copy the data to the file
                    if let Some(mut stdout) = child.stdout.take() {
                        io::copy(&mut stdout, &mut file).expect("Failed to write to file");
                    }
                },
                Err(e) => {
                    error!("Error creating file: {}", e);
                }
            }
        }
        child.stdout
    }
}
