use std::collections::VecDeque;
use std::ffi::{CString, OsStr};
use std::fs;
use std::os::fd::{AsFd, AsRawFd, BorrowedFd};
use std::os::unix::ffi::OsStrExt;
use std::os::unix::fs::{FileTypeExt, MetadataExt};
use std::path::{Path, PathBuf};
use std::os::fd::RawFd;
use libc::{getegid, geteuid};
use log::{debug, info};
use nix::fcntl::{open,OFlag};
use nix::sys::stat::Mode;
use nix::unistd::{access, close, dup, dup2, isatty, write, AccessFlags};
use nix::NixPath;


use crate::execute::RshExitStatus;
use crate::interp::token::{Redir, RedirType, Tk, TkType};
use crate::shellenv::ShellEnv;
use crate::event::ShellError;

pub const BUILTINS: [&str; 14] = [
    "echo", "set", "shift", "export", "cd", "readonly", "declare", "local", "unset", "trap", "node",
    "exec", "source", "wait",
];

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


pub fn source(shellenv: &mut ShellEnv, argv: Vec<CString>) -> Result<RshExitStatus,ShellError> {
	let paths = &argv[1..];
	for path in paths {
		dbg!(path.clone().into_string().unwrap());
		let file_path = Path::new(OsStr::from_bytes(path.as_bytes()));
		shellenv.source_file(file_path.to_path_buf())?
	}
	Ok(RshExitStatus::Success)
}

fn do_test<T,F1,F2>(args: &mut VecDeque<String>, transform: F1, property: F2) -> Result<bool, ShellError>
where
	F1: FnOnce(String) -> Result<T,ShellError>,
	F2: FnOnce(&T) -> bool
{
	if let Some(st) = args.pop_front() {
		let transformed = transform(st)?;
		Ok(property(&transformed))
	} else {
		Err(ShellError::InvalidSyntax("Did not find a comparison target for integer in test".into()))
	}
}

fn do_cmp<T,F1,F2>(arg1: String, args: &mut VecDeque<String>, transform: F1, comparison: F2) -> Result<bool, ShellError>
where
	F1: Fn(String) -> Result<T,ShellError>,
	F2: FnOnce(&T,&T) -> bool
{
	if let Some(st) = args.pop_front() {
		let left = transform(arg1)?;
		let right = transform(st)?;
		Ok(comparison(&left,&right))
	} else {
		Err(ShellError::InvalidSyntax("Did not find a comparison target for integer in test".into()))
	}
}


pub fn test(mut argv: Vec<CString>) -> Result<RshExitStatus,ShellError> {
	info!("Starting builtin test");
	let is_false = || -> Result<RshExitStatus,ShellError> { Ok(RshExitStatus::Fail { code: 1, cmd: Some("test".into()) }) };
	let is_true = || -> Result<RshExitStatus,ShellError> { Ok(RshExitStatus::Success) };
	let is_int = |s: &String| -> bool { s.parse::<i32>().is_ok() };
	let to_int = |s: String| -> Result<i32,ShellError> {
		s.parse::<i32>().map_err(|_| ShellError::InvalidSyntax("Expected an integer in test".into()))
	};
	let is_path = |s: &String| -> bool { PathBuf::from(s).exists() };
	let to_meta = |s: String| -> Result<fs::Metadata,ShellError> {
    fs::metadata(&s).map_err(|_| ShellError::InvalidSyntax(format!("Test: Path '{}' does not exist", s)))
	};
	let string_noop = |s: String| -> Result<String,ShellError> { Ok(s) };
	let mut args = VecDeque::new();
	args.extend(argv.into_iter().map(|s| s.into_string().unwrap()).collect::<Vec<String>>().drain(..));
	let command = args.pop_front().unwrap();
	let is_bracket = match command.as_str() {
		"[" => true,
		"test" => false,
		_ => unreachable!()
	};

	if is_bracket {
		if let Some(arg) = args.back() {
			if arg != "]" {
				return Err(ShellError::InvalidSyntax("Test is missing a closing bracket".into()))
			}
		} else {
			return Err(ShellError::InvalidSyntax("Found a test with no arguments".into()))
		}
	}

	if let Some(arg) = args.pop_front() {
		let result1 = match arg.as_str() {
			"!" => {
				args.push_front(command);
				argv = args.into_iter().map(|s| CString::new(s).unwrap()).collect::<Vec<CString>>();
				let result = test(argv)?;
				match result {
					RshExitStatus::Success => { return is_false() },
					RshExitStatus::Fail {..} => { return is_true() },
				}
			}
			"-t" => do_test(&mut args, to_int, |int| isatty(*int).is_ok())?,
			"-b" => do_test(&mut args, to_meta, |meta| meta.file_type().is_block_device())?,
			"-c" => do_test(&mut args, to_meta, |meta| meta.file_type().is_char_device())?,
			"-d" => do_test(&mut args, to_meta, |meta| meta.is_dir())?,
			"-f" => do_test(&mut args, to_meta, |meta| meta.is_file())?,
			"-g" => do_test(&mut args, to_meta, |meta| meta.mode() & 0o2000 != 0)?,
			"-G" => do_test(&mut args, to_meta, |meta| meta.gid() == unsafe { getegid() })?,
			"-h" => do_test(&mut args, to_meta, |meta| meta.is_symlink())?,
			"-L" => do_test(&mut args, to_meta, |meta| meta.is_symlink())?,
			"-k" => do_test(&mut args, to_meta, |meta| meta.mode() & 0o1000 != 0)?,
			"-N" => do_test(&mut args, to_meta, |meta| meta.mtime() > meta.atime())?,
			"-O" => do_test(&mut args, to_meta, |meta| meta.uid() == unsafe { geteuid() })?,
			"-p" => do_test(&mut args, to_meta, |meta| meta.file_type().is_fifo())?,
			"-s" => do_test(&mut args, to_meta, |meta| meta.len() > 0)?,
			"-S" => do_test(&mut args, to_meta, |meta| meta.file_type().is_socket())?,
			"-u" => do_test(&mut args, to_meta, |meta| meta.mode() & 0o4000 != 0)?,
			"-n" => do_test(&mut args, string_noop, |st: &String| !st.is_empty())?,
			"-z" => do_test(&mut args, string_noop, |st| st.is_empty())?,
			"-e" => do_test(&mut args, string_noop, |st| Path::new(st).exists())?,
			"-r" => do_test(&mut args, string_noop, |st| access(Path::new(st),AccessFlags::R_OK).is_ok())?,
			"-w" => do_test(&mut args, string_noop, |st| access(Path::new(st),AccessFlags::W_OK).is_ok())?,
			"-x" => do_test(&mut args, string_noop, |st| access(Path::new(st),AccessFlags::X_OK).is_ok())?,
			_ if is_int(&arg) => {
				if let Some(cmp) = args.pop_front() {
					match cmp.as_str() {
						"-eq" => do_cmp(arg, &mut args, to_int, |left,right| left == right)?,
						"-ge" => do_cmp(arg, &mut args, to_int, |left,right| left >= right)?,
						"-gt" => do_cmp(arg, &mut args, to_int, |left,right| left > right)?,
						"-le" => do_cmp(arg, &mut args, to_int, |left,right| left <= right)?,
						"-lt" => do_cmp(arg, &mut args, to_int, |left,right| left < right)?,
						"-ne" => do_cmp(arg, &mut args, to_int, |left,right| left != right)?,
						_ => {
							return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
						}
					}
				} else {
					return Err(ShellError::InvalidSyntax("Expected a comparison operator after integer in test".into()));
				}
			}
			_ if is_path(&arg) => {
				if let Some(cmp) = args.pop_front() {
					match cmp.as_str() {
						"-ef" => do_cmp(arg, &mut args, to_meta, |left,right| left.dev() == right.dev() && left.ino() == right.ino())?,
						"-nt" => do_cmp(arg, &mut args, to_meta, |left,right| left.mtime() > right.mtime())?,
						"-ot" => do_cmp(arg, &mut args, to_meta, |left,right| left.mtime() < right.mtime())?,
						_ => {
							return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
						}
					}
				} else {
					return Err(ShellError::InvalidSyntax("Expected a comparison operator after path name in test".into()));
				}
			}
			_ => {
				if args.is_empty() {
					!arg.is_empty()
				} else if let Some(cmp) = args.pop_front() {
					match cmp.as_str() {
						"=" => do_cmp(arg, &mut args, string_noop, |left,right| left == right)?,
						"!=" => do_cmp(arg, &mut args, string_noop, |left,right| left != right)?,
						_ => {
							return Err(ShellError::InvalidSyntax("Expected a comparison operator after string in test".into()));
						}
					}
				} else {
					return Err(ShellError::InvalidSyntax("Expected a comparison operator after string in test".into()));
				}
			}
		};
		if let Some(arg) = args.pop_front() {
			match arg.as_str() {
				"-a" | "-o" => { // And/Or
					args.push_front(command); // Push argv[0] back onto the stack, to trick the recursive test call into thinking it's a new invocation
					argv = args.into_iter().map(|s| CString::new(s).unwrap()).collect::<Vec<CString>>();
					let result2 = match test(argv)? {
						RshExitStatus::Success => { true },
						RshExitStatus::Fail {..} => { false },
					};
					let result = if arg.as_str() == "-a" {
						result1 && result2
					} else {
						result1 || result2
					};
					match result {
						true => return is_true(),
						false => return is_false()
					}
				}
				"]" => {}
				_ => {
					return Err(ShellError::InvalidSyntax("Unexpected extra argument found in test".into()));
				}
			}
		}
		match result1 {
			true => is_true(),
			false => is_false(),
		}
	} else {
		Err(ShellError::InvalidSyntax("Test called with no arguments".into()))
	}
}

pub fn echo(mut argv: VecDeque<CString>, redirs: VecDeque<Tk>, stdout: Option<RawFd>) -> Result<RshExitStatus, ShellError> {
    log::info!("Executing echo with argv: {:?}", argv);

		let saved_in = dup(0).unwrap();
		let saved_out = dup(1).unwrap();
		let saved_err = dup(2).unwrap();
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

		if let Some(w_pipe) = stdout {
			close(w_pipe).expect("failed to close stdout in echo");
		}

    close_file_descriptors(fd_stack);

		let _ = dup2(saved_in,0);
		let _ = dup2(saved_out,1);
		let _ = dup2(saved_err,2);
		let _ = close(saved_in);
		let _ = close(saved_out);
		let _ = close(saved_err);

    result.map(|_| RshExitStatus::Success)
}
