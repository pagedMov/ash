use std::collections::VecDeque;
use std::io::Write;
use std::os::fd::{AsFd, AsRawFd, BorrowedFd};
use std::{io, os::fd::RawFd};
use nix::unistd::write;


use crate::execute::RshExitStatus;
use crate::{event::ShellError, parser::Token};

const BUILTINS: [&str; 13] = [
    "echo", "set", "shift", "export", "readonly", "declare", "local", "unset", "trap", "node",
    "exec", "source", "wait",
];

pub fn echo(mut argv: VecDeque<Token>, stdout: Option<RawFd>) -> Result<RshExitStatus,ShellError> {
    argv.pop_front(); // argv[0] is 'echo'
    let mut output = argv
        .into_iter()
        .map(|t| t.text().into())
        .collect::<Vec<String>>();
    output.push("\n".into());
    let output_str = output.join(" ");

    let output = unsafe { BorrowedFd::borrow_raw(stdout.unwrap_or(1)) };

    if let Err(e) = write(output.as_fd(), output_str.as_bytes()) {
        return Err(ShellError::ExecFailed(format!("Congrats on somehow managing to make echo return 1. Cause: {}",e), 1));
    }
    Ok(RshExitStatus::Success)
}
