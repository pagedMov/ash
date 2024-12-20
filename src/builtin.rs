use std::collections::VecDeque;
use std::ffi::CString;
use std::io::Write;
use std::os::fd::{AsFd, AsRawFd, BorrowedFd};
use std::path::Path;
use std::{io, os::fd::RawFd};
use log::debug;
use nix::fcntl::{open,OFlag};
use nix::sys::stat::Mode;
use nix::unistd::{write,dup,dup2,close};


use crate::execute::RshExitStatus;
use crate::interp::token::{Redir, RedirType, Tk, TkType};
use crate::shellenv::ShellEnv;
use crate::event::ShellError;

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
        shellenv.change_dir(path);
    }
    Ok(RshExitStatus::Success)
}

fn open_file_descriptors(redirs: VecDeque<Tk>) -> Result<Vec<(i32, i32)>, ShellError> {
    let mut fd_stack: Vec<(i32, i32)> = Vec::new();
    debug!("Handling redirections for builtin: {:?}", redirs);

    for redir in redirs {
        if let TkType::Redirection { redir } = redir.class() {
            let Redir { fd_source, op, fd_target, file_target } = redir;

            let backup_fd = dup(fd_source).map_err(|e| {
                ShellError::ExecFailed(format!("Failed to back up FD {}: {}", fd_source, e), 1)
            })?;
            fd_stack.push((fd_source, backup_fd));

            if let Some(target) = fd_target {
                dup2(target, fd_source).map_err(|e| {
                    ShellError::ExecFailed(format!("Failed to redirect FD {} to {}: {}", fd_source, target, e), 1)
                })?;
            } else if let Some(file_path) = file_target {
                let flags = match op {
                    RedirType::Input => OFlag::O_RDONLY,
                    RedirType::Output => OFlag::O_WRONLY | OFlag::O_CREAT | OFlag::O_TRUNC,
                    RedirType::Append => OFlag::O_WRONLY | OFlag::O_CREAT | OFlag::O_APPEND,
                    _ => unimplemented!("Heredocs and herestrings are not implemented yet."),
                };
                let file_fd = open(file_path.text(), flags, Mode::from_bits(0o644).unwrap()).map_err(|e| {
                    ShellError::ExecFailed(format!("Failed to open file {}: {}", file_path.text(), e), 1)
                })?;
                dup2(file_fd, fd_source).map_err(|e| {
                    ShellError::ExecFailed(format!("Failed to redirect FD {} to file {}: {}", fd_source, file_path.text(), e), 1)
                })?;
                close(file_fd).unwrap_or_else(|e| {
                    debug!("Failed to close file FD {}: {}", file_fd, e);
                });
            }
        }
    }

    Ok(fd_stack)
}

fn close_file_descriptors(fd_stack: Vec<(i32, i32)>) {
    for (orig_fd, backup_fd) in fd_stack {
        if let Err(e) = dup2(backup_fd, orig_fd) {
            log::warn!("Failed to restore FD {} from backup FD {}: {}", orig_fd, backup_fd, e);
        }
        if let Err(e) = close(backup_fd) {
            log::warn!("Failed to close backup FD {}: {}", backup_fd, e);
        }
    }
}

pub fn echo(mut argv: VecDeque<CString>, redirs: VecDeque<Tk>, stdout: Option<RawFd>) -> Result<RshExitStatus, ShellError> {
    log::info!("Executing echo with argv: {:?}", argv);

    argv.pop_front(); // Remove 'echo' from argv
    let output_str = catstr(argv, true);

    let fd_stack = if !redirs.is_empty() {
        open_file_descriptors(redirs)?
    } else {
        Vec::new()
    };

    let output_fd = stdout.unwrap_or(1); // Default to standard output
    let output = unsafe { BorrowedFd::borrow_raw(output_fd) };

    let result = write(output.as_fd(), output_str.as_bytes()).map_err(|e| {
        ShellError::ExecFailed(format!("Failed to write output in echo: {}", e), 1)
    });

    close_file_descriptors(fd_stack);

    result.map(|_| RshExitStatus::Success)
}
