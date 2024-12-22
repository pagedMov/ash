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

pub fn test(mut argv: Vec<CString>) -> Result<RshExitStatus,ShellError> {
	info!("Starting builtin test");
	let is_false = || -> Result<RshExitStatus,ShellError> { Ok(RshExitStatus::Fail { code: 1, cmd: Some("test".into()) }) };
	let is_true = || -> Result<RshExitStatus,ShellError> { Ok(RshExitStatus::Success) };
	let is_int = |s: &String| -> bool { s.parse::<i32>().is_ok() };
	let is_path = |s: &String| -> bool { PathBuf::from(s).exists() };
	let is_string = |s: &String| -> bool { !is_path(s) && !is_int(s) };
	let is_file = |path: &String| -> bool {
		if let Ok(metadata) = fs::metadata(path) {
				metadata.is_file()
		} else {
				false
		}
	};
	let mut args = VecDeque::new();
	args.extend(argv.into_iter().map(|s| s.into_string().unwrap()).collect::<Vec<String>>().drain(..));
	let is_bracket = match args.pop_front().unwrap().as_str() {
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
				argv = args.into_iter().map(|s| CString::new(s).unwrap()).collect::<Vec<CString>>();
				let result = test(argv)?;
				match result {
					RshExitStatus::Success => { return is_false() },
					RshExitStatus::Fail {..} => { return is_true() },
				}
			}
			"-n" => {
				if let Some(st) = args.pop_front() {
					if !is_string(&st) {
						return Err(ShellError::InvalidSyntax("Expected a string after `-n` flag in test".into()));
					} else {
						st.is_empty()
					}
				} else {
					return Err(ShellError::InvalidSyntax("Expected a string after `-n` flag in test".into()));
				}
			},
			"-z" => {
				if let Some(st) = args.pop_front() {
					if !is_string(&st) {
						return Err(ShellError::InvalidSyntax("Expected a string after `-n` flag in test".into()));
					} else {
						st.is_empty()
					}
				} else {
					return Err(ShellError::InvalidSyntax("Expected a string after `-n` flag in test".into()));
				}
			},
			"-b" => {
				if let Some(st) = args.pop_front() {
					if !is_file(&st) {
						return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
					} else if let Ok(metadata) = fs::metadata(st) {
						metadata.file_type().is_block_device()
					} else {
						return Err(ShellError::IoError("Failed to get file metadata in test".into()));
					}
				} else {
					return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
				}
			},
			"-c" => {
				if let Some(st) = args.pop_front() {
					if !is_file(&st) {
						return Err(ShellError::InvalidSyntax("Expected a file name after -c flag".into()));
					} else if let Ok(metadata) = fs::metadata(st) {
						metadata.file_type().is_char_device()
					} else {
						return Err(ShellError::IoError("Failed to get file metadata in test".into()));
					}
				} else {
					return Err(ShellError::InvalidSyntax("Expected a file name after -c flag".into()));
				}
			},
			"-d" => {
				if let Some(st) = args.pop_front() {
					if !is_path(&st) {
						return Err(ShellError::InvalidSyntax("Expected a file name after -d flag".into()));
					} else if let Ok(metadata) = fs::metadata(st) {
						metadata.file_type().is_dir()
					} else {
						return Err(ShellError::IoError("Failed to get file metadata in test".into()));
					}
				} else {
					return Err(ShellError::InvalidSyntax("Expected a file name after -d flag".into()));
				}
			},
			"-e" => {
				if let Some(st) = args.pop_front() {
					Path::new(&st).exists()
				} else {
					return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
				}
			},
			"-f" => {
				if let Some(st) = args.pop_front() {
					is_file(&st)
				} else {
					return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
				}
			},
			"-g" => {
				if let Some(st) = args.pop_front() {
					if !is_path(&st) {
						return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
					} else {
						let mode = fs::metadata(&st).unwrap().mode();
						mode & 0o2000 != 0
					}
				} else {
					return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
				}
			},
			"-G" => {
				if let Some(st) = args.pop_front() {
					if !is_path(&st) {
						return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
					} else {
						let gid = fs::metadata(&st).unwrap().gid();
						let process_gid = unsafe { getegid() };
						gid == process_gid
					}
				} else {
					return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
				}
			},
			"-h" | "-L" => {
				if let Some(st) = args.pop_front() {
					if !is_path(&st) {
						return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
					} else if let Ok(metadata) = fs::metadata(&st) {
						metadata.file_type().is_symlink()
					} else {
						return Err(ShellError::IoError("Failed to get file metadata in test".into()));
					}
				} else {
					return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
				}
			},
			"-k" => {
				if let Some(st) = args.pop_front() {
					if !is_path(&st) {
						return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
					} else if let Ok(metadata) = fs::metadata(&st) {
						let mode = metadata.mode();
						mode & 0o1000 != 0
					} else {
						return Err(ShellError::IoError("Failed to get file metadata in test".into()));
					}
				} else {
					return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
				}
			},
			"-N" => {
				if let Some(st) = args.pop_front() {
					if !is_file(&st) {
						return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
					} else if let Ok(metadata) = fs::metadata(&st) {
						let mtime = metadata.mtime();
						let atime = metadata.atime();
						mtime > atime
					} else {
						return Err(ShellError::IoError("Failed to get file metadata in test".into()));
					}
				} else {
					return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
				}
			},
			"-O" => {
				if let Some(st) = args.pop_front() {
					if !is_path(&st) {
						return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
					} else if let Ok(metadata) = fs::metadata(&st) {
						let file_uid = metadata.uid();
						let proc_uid = unsafe{ geteuid() };
						file_uid == proc_uid
					} else {
						return Err(ShellError::IoError("Failed to get file metadata in test".into()));
					}
				} else {
					return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
				}
			},
			"-p" => {
				if let Some(st) = args.pop_front() {
					if !is_file(&st) {
						return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
					} else if let Ok(metadata) = fs::metadata(&st) {
						metadata.file_type().is_fifo()
					} else {
						return Err(ShellError::IoError("Failed to get file metadata in test".into()));
					}
				} else {
					return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
				}
			},
			"-r" => {
				if let Some(st) = args.pop_front() {
					if !is_path(&st) {
						return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
					} else {
						let path = Path::new(&st);
						access(path, AccessFlags::R_OK).is_ok()
					}
				} else {
					return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
				}
			},
			"-s" => {
				if let Some(st) = args.pop_front() {
					if !is_file(&st) {
						return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
					} else if let Ok(metadata) = fs::metadata(&st) {
						metadata.len() > 0
					} else {
						return Err(ShellError::IoError("Failed to get file metadata in test".into()));
					}
				} else {
					return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
				}
			},
			"-S" => {
				if let Some(st) = args.pop_front() {
					if !is_file(&st) {
						return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
					} else if let Ok(metadata) = fs::metadata(&st) {
						metadata.file_type().is_socket()
					} else {
						return Err(ShellError::IoError("Failed to get file metadata in test".into()));
					}
				} else {
					return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
				}
			},
			"-t" => {
				if let Some(st) = args.pop_front() {
					if !is_int(&st) {
						return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
					} else {
						let fd = st.parse::<i32>().unwrap().as_raw_fd();
						isatty(fd).is_ok()
					}
				} else {
					return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
				}
			},
			"-u" => {
				if let Some(st) = args.pop_front() {
					if !is_file(&st) {
						return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
					} else if let Ok(metadata) = fs::metadata(&st) {
						let mode = metadata.mode();
						mode & 0o4000 != 0
					} else {
						return Err(ShellError::IoError("Failed to get file metadata in test".into()));
					}
				} else {
					return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
				}
			},
			"-w" => {
				if let Some(st) = args.pop_front() {
					if !is_path(&st) {
						return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
					} else {
						let path = Path::new(&st);
						access(path, AccessFlags::W_OK).is_ok()
					}
				} else {
					return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
				}
			},
			"-x" => {
				if let Some(st) = args.pop_front() {
					if !is_path(&st) {
						return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
					} else {
						let path = Path::new(&st);
						access(path, AccessFlags::X_OK).is_ok()
					}
				} else {
					return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
				}
			},
			_ if is_int(&arg) => {
				if let Some(cmp) = args.pop_front() {
					match cmp.as_str() {
						"-eq" => {
							if let Some(arg2) = args.pop_front() {
								if !is_int(&arg2) {
									return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
								} else {
									let left = arg.parse::<i32>().unwrap();
									let right = arg2.parse::<i32>().unwrap();
									left == right
								}
							} else {
								return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
							}
						}
						"-ge" => {
							if let Some(arg2) = args.pop_front() {
								if !is_int(&arg2) {
									return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
								} else {
									let left = arg.parse::<i32>().unwrap();
									let right = arg2.parse::<i32>().unwrap();
									left >= right
								}
							} else {
								return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
							}
						}
						"-gt" => {
							if let Some(arg2) = args.pop_front() {
								if !is_int(&arg2) {
									return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
								} else {
									let left = arg.parse::<i32>().unwrap();
									let right = arg2.parse::<i32>().unwrap();
									left > right
								}
							} else {
								return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
							}
						}
						"-le" => {
							if let Some(arg2) = args.pop_front() {
								if !is_int(&arg2) {
									return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
								} else {
									let left = arg.parse::<i32>().unwrap();
									let right = arg2.parse::<i32>().unwrap();
									left <= right
								}
							} else {
								return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
							}
						}
						"-lt" => {
							if let Some(arg2) = args.pop_front() {
								if !is_int(&arg2) {
									return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
								} else {
									let left = arg.parse::<i32>().unwrap();
									let right = arg2.parse::<i32>().unwrap();
									left < right
								}
							} else {
								return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
							}
						}
						"-ne" => {
							if let Some(arg2) = args.pop_front() {
								if !is_int(&arg2) {
									return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
								} else {
									let left = arg.parse::<i32>().unwrap();
									let right = arg2.parse::<i32>().unwrap();
									left != right
								}
							} else {
								return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
							}
						}
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
						"-ef" => {
							if let Some(arg2) = args.pop_front() {
								if !is_path(&arg2) {
									return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
								} else if let (Ok(left_meta), Ok(right_meta)) = (fs::metadata(&arg), fs::metadata(&arg2)) {
									left_meta.dev() == right_meta.dev() && left_meta.ino() == right_meta.ino()
								} else {
									return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
								}
							} else {
								return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
							}
						},
						"-nt" => {
							if let Some(arg2) = args.pop_front() {
								if !is_path(&arg2) {
									return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
								} else if let (Ok(left_meta), Ok(right_meta)) = (fs::metadata(&arg), fs::metadata(&arg2)) {
									left_meta.mtime() > right_meta.mtime()
								} else {
									return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
								}
							} else {
								return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
							}
						},
						"-ot" => {
							if let Some(arg2) = args.pop_front() {
								if !is_path(&arg2) {
									return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
								} else if let (Ok(left_meta), Ok(right_meta)) = (fs::metadata(&arg), fs::metadata(&arg2)) {
									left_meta.mtime() < right_meta.mtime()
								} else {
									return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
								}
							} else {
								return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
							}
						},
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
						"=" => {
							if let Some(arg2) = args.pop_front() {
								if !is_string(&arg2) {
									return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
								} else {
									arg == arg2
								}
							} else {
								return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
							}
						}
						"!=" => {
							if let Some(arg2) = args.pop_front() {
								if !is_string(&arg2) {
									return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
								} else {
									arg != arg2
								}
							} else {
								return Err(ShellError::InvalidSyntax("Expected a file name after -b flag".into()));
							}
						}
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
				"-a" => {
					argv = args.into_iter().map(|s| CString::new(s).unwrap()).collect::<Vec<CString>>();
					let result2 = match test(argv)? {
						RshExitStatus::Success => { true },
						RshExitStatus::Fail {..} => { false },
					};
					match result1 && result2 {
						true => {
							return is_true();
						}
						false => {
							return is_false();
						}
					}
				}
				"-o" => {
					argv = args.into_iter().map(|s| CString::new(s).unwrap()).collect::<Vec<CString>>();
					let result2 = match test(argv)? {
						RshExitStatus::Success => { true },
						RshExitStatus::Fail {..} => { false },
					};
					match result1 || result2 {
						true => {
							return is_true();
						}
						false => {
							return is_false();
						}
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
