use std::collections::VecDeque;
use std::ffi::CString;
use std::io::Write;
use std::os::fd::{AsFd, AsRawFd, BorrowedFd};
use std::path::Path;
use std::{io, os::fd::RawFd};
use nix::unistd::write;


use crate::execute::RshExitStatus;
use crate::shellenv::ShellEnv;
use crate::{event::ShellError, parser2::Token};

pub const BUILTINS: [&str; 14] = [
    "echo", "set", "shift", "export", "cd", "readonly", "declare", "local", "unset", "trap", "node",
    "exec", "source", "wait",
];

pub fn catstr(mut c_strings: VecDeque<CString>,newline: bool) -> CString {
    let mut cat: Vec<u8> = vec![];
    let newline_bytes = b"\n\0";
    let space_bytes = b" ";

    while let Some(c_string) = c_strings.pop_front() {
        let bytes = c_string.to_bytes_with_nul();
        if c_strings.is_empty() {
            // Last string: include the null terminator
            if newline {
                cat.extend_from_slice(&bytes[..bytes.len() - 1]);
            } else {
                cat.extend_from_slice(bytes);
            }
        } else {
            // Intermediate strings: exclude the null terminator and add whitespace
            cat.extend_from_slice(&bytes[..bytes.len() - 1]);
            cat.extend_from_slice(space_bytes);
        }
    }
    cat.extend_from_slice(newline_bytes);

    CString::from_vec_with_nul(cat).unwrap()
}

pub fn cd(shellenv: &mut ShellEnv, mut argv: VecDeque<CString>) -> Result<RshExitStatus,ShellError> {
    argv.pop_front();
    if let Some(new_pwd) = argv.pop_front() {
        let path = Path::new(new_pwd.to_str().unwrap());
        shellenv.change_director(path);
    }
    Ok(RshExitStatus::Success)
}

pub fn echo(mut argv: VecDeque<CString>, stdout: Option<RawFd>) -> Result<RshExitStatus,ShellError> {
    log::info!("Doing echo with argv: {:?}",argv);
    argv.pop_front(); // argv[0] is 'echo'
    let output_str = catstr(argv,true);

    let output = unsafe { BorrowedFd::borrow_raw(stdout.unwrap_or(1)) };

    if let Err(e) = write(output.as_fd(), output_str.as_bytes()) {
        return Err(ShellError::ExecFailed(format!("Congrats on somehow managing to make echo return 1. Cause: {}",e), 1));
    }
    Ok(RshExitStatus::Success)
}
