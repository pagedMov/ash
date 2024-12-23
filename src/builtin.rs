use std::collections::VecDeque;
use std::ffi::{CString, OsStr};
use std::fs;
use std::os::fd::{AsFd, AsRawFd, BorrowedFd};
use std::os::unix::ffi::OsStrExt;
use std::os::unix::fs::{FileTypeExt, MetadataExt};
use std::path::{Path, PathBuf};
use std::os::fd::RawFd;
use libc::{getegid, geteuid, STDOUT_FILENO};
use log::{debug, info};
use nix::fcntl::{open,OFlag};
use nix::sys::stat::Mode;
use nix::unistd::{access, close, dup, dup2, isatty, write, AccessFlags};
use nix::NixPath;

use crate::execute::{RshExitStatus, SavedFDs};
use crate::interp::parse::{Node, Span};
use crate::interp::{helper, token};
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

	for redir_tk in redirs {
		if let TkType::Redirection { redir } = redir_tk.class() {
			let Redir { fd_source, op, fd_target, file_target } = redir;

			let backup_fd = dup(fd_source).map_err(|e| {
				ShellError::from_execf(&format!("Failed to back up FD {}: {}", fd_source, e), 1, redir_tk.span())
			})?;
			fd_stack.push((fd_source, backup_fd));

			if let Some(target) = fd_target {
				dup2(target, fd_source).map_err(|e| {
					ShellError::from_execf(&format!("Failed to redirect FD {} to {}: {}", fd_source, target, e), 1, redir_tk.span())
				})?;
			} else if let Some(file_path) = file_target {
				let flags = match op {
					RedirType::Input => OFlag::O_RDONLY,
					RedirType::Output => OFlag::O_WRONLY | OFlag::O_CREAT | OFlag::O_TRUNC,
					RedirType::Append => OFlag::O_WRONLY | OFlag::O_CREAT | OFlag::O_APPEND,
					_ => unimplemented!("Heredocs and herestrings are not implemented yet."),
				};
				let file_fd = open(file_path.text(), flags, Mode::from_bits(0o644).unwrap()).map_err(|e| {
					ShellError::from_execf(&format!("Failed to open file {}: {}", file_path.text(), e), 1, redir_tk.span())
				})?;
				dup2(file_fd, fd_source).map_err(|e| {
					ShellError::from_execf(&format!("Failed to redirect FD {} to file {}: {}", fd_source, file_path.text(), e), 1, redir_tk.span())
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

/// Performs a test on a single argument by transforming it and then applying a property check.
///
/// This function takes a mutable reference to a `VecDeque` of `String` arguments, a transformation function,
/// and a property function. It pops the front element from the `VecDeque`, applies the transformation
/// function to it, and then checks the transformed value against the property function.
///
/// # Arguments
///
/// * `args` - A mutable reference to a `VecDeque` of `String` arguments.
/// * `transform` - A function that takes a `String` and returns a `Result<T, ShellError>`.
/// * `property` - A function that takes a reference to `T` and returns a `bool`.
///
/// # Returns
///
/// * `Ok(bool)` - If the transformation and property check are successful, returns the result of the property check.
/// * `Err(ShellError)` - If the `VecDeque` is empty or the transformation fails, returns an appropriate `ShellError`.
///
/// # Example
///
/// ```
/// let mut args = VecDeque::new();
/// args.push_back("42".to_string());
/// let transform = |s: String| -> Result<i32, ShellError> { Ok(s.parse::<i32>().unwrap()) };
/// let property = |x: &i32| -> bool { *x > 0 };
/// let result = do_test(&mut args, transform, property);
/// assert!(result.unwrap());
/// ```
fn do_test<T, F1, F2>(
	args: &mut VecDeque<Tk>,
	transform: F1,
	property: F2,
	span: Span
) -> Result<bool, ShellError>
where
		F1: FnOnce(Tk) -> Result<T, ShellError>,
		F2: FnOnce(&T) -> bool
{
	if let Some(st) = args.pop_front() {
		let transformed = transform(st).map_err(|_| false);
		if transformed.is_err() {
			return Ok(false)
		}
		Ok(property(&transformed.unwrap()))
	} else {
		Err(ShellError::from_syntax("Did not find a comparison target for integer in test", span))
	}
}

/// Compares two arguments by transforming them and then applying a comparison function.
///
/// This function takes a `String` argument, a mutable reference to a `VecDeque` of `String` arguments,
/// a transformation function, and a comparison function. It pops the front element from the `VecDeque`,
/// applies the transformation function to both the provided `String` and the popped element, and then
/// compares the transformed values using the comparison function.
///
/// # Arguments
///
/// * `arg1` - The first `String` argument to be transformed and compared.
/// * `args` - A mutable reference to a `VecDeque` of `String` arguments.
/// * `transform` - A function that takes a `String` and returns a `Result<T, ShellError>`.
/// * `comparison` - A function that takes two references to `T` and returns a `bool`.
///
/// # Returns
///
/// * `Ok(bool)` - If the transformation and comparison are successful, returns the result of the comparison.
/// * `Err(ShellError)` - If the `VecDeque` is empty or the transformation fails, returns an appropriate `ShellError`.
///
/// # Example
///
/// ```
/// let mut args = VecDeque::new();
/// args.push_back("42".to_string());
/// let transform = |s: String| -> Result<i32, ShellError> { Ok(s.parse::<i32>().unwrap()) };
/// let comparison = |x: &i32, y: &i32| -> bool { x == y };
/// let result = do_cmp("42".to_string(), &mut args, transform, comparison);
/// assert!(result.unwrap());
/// ```
fn do_cmp<T, F1, F2>(
	arg1: Tk,
	args: &mut VecDeque<Tk>,
	transform: F1,
	comparison: F2
) -> Result<bool, ShellError>
where
		F1: Fn(Tk) -> Result<T, ShellError>,
		F2: FnOnce(&T, &T) -> bool
{
	if let Some(st) = args.pop_front() {
		let left = transform(arg1).map_err(|_| false);
		let right = transform(st).map_err(|_| false);
		if left.is_err() || right.is_err() {
			return Ok(false)
		}
		Ok(comparison(&left.unwrap(), &right.unwrap()))
	} else {
		Err(ShellError::from_syntax("Did not find a comparison target for integer in test", arg1.span()))
	}
}

/// Performs a logical operation (AND or OR) on two boolean results.
///
/// This function takes a mutable reference to a `VecDeque` of `String` arguments, the command (`test` or `[`),
/// the first result of a test, and the logical operator (`-a` for AND, `-o` for OR). It recursively calls the `test`
/// function to evaluate the next condition and combines the results using the specified logical operator.
///
/// # Arguments
///
/// * `args` - A mutable reference to a `VecDeque` of `String` arguments.
/// * `command` - The command (`test` or `[`).
/// * `result1` - The result of the first test.
/// * `operator` - The logical operator (`-a` for AND, `-o` for OR).
///
/// # Returns
///
/// * `Result<bool, ShellError>` - Returns the combined result of the logical operation.
///
/// # Errors
///
/// * Returns `ShellError::from_syntax` if there is a syntax error or missing arguments.
fn do_logical_op(
	args: &mut VecDeque<Tk>,
	command: Tk,
	result1: bool,
	operator: Tk
) -> Result<bool, ShellError> {
	args.push_front(command);
	let result2 = test(std::mem::take(args)).map(|res| matches!(res,RshExitStatus::Success {..}))?;
	match operator.text() {
		"!" => { Ok(!result2) },
		"-a" => { Ok(result1 && result2) },
		"-o" => { Ok(result1 || result2) },
		_ => Err(ShellError::from_syntax("Expected a logical operator (-a or -o)", operator.span())),
	}
}

/// Implements the `test` builtin command for a Unix shell.
///
/// The `test` command evaluates conditional expressions and returns a success or failure status based on the evaluation.
/// This function handles various types of tests, including file attributes, string comparisons, and integer comparisons.
///
/// # Arguments
///
/// * `argv` - A vector of `CString` arguments passed to the `test` command.
///
/// # Returns
///
/// * `Ok(RshExitStatus)` - Returns a success or failure status based on the evaluation of the conditional expression.
/// * `Err(ShellError)` - Returns an appropriate `ShellError` if there is a syntax error or other issues with the arguments.
///
/// # Examples
///
/// ```
/// let argv = vec![CString::new("test").unwrap(), CString::new("-f").unwrap(), CString::new("file.txt").unwrap()];
/// let result = test(argv);
/// assert!(result.is_ok());
/// ```
///
/// ```
/// let argv = vec![CString::new("[").unwrap(), CString::new("1").unwrap(), CString::new("-eq").unwrap(), CString::new("1").unwrap(), CString::new("]").unwrap()];
/// let result = test(argv);
/// assert!(result.is_ok());
/// ```
///
/// # Supported Tests
///
/// ## File Tests
///
/// - `-b`: Block device
/// - `-c`: Character device
/// - `-d`: Directory
/// - `-e`: File exists
/// - `-f`: Regular file
/// - `-g`: Set-group-ID flag set
/// - `-G`: Group-ID matches
/// - `-h`: Symbolic link
/// - `-L`: Symbolic link (same as `-h`)
/// - `-k`: Sticky bit set
/// - `-N`: Modified since last read
/// - `-O`: User-ID matches
/// - `-p`: Named pipe
/// - `-r`: Readable
/// - `-s`: File size greater than zero
/// - `-S`: Socket
/// - `-u`: Set-user-ID flag set
/// - `-w`: Writable
/// - `-x`: Executable
///
/// ## String Tests
///
/// - `-n`: String is non-empty
/// - `-z`: String is empty
/// - `=`: Strings are equal
/// - `!=`: Strings are not equal
///
/// ## Integer Tests
///
/// - `-eq`: Equal
/// - `-ge`: Greater than or equal
/// - `-gt`: Greater than
/// - `-le`: Less than or equal
/// - `-lt`: Less than
/// - `-ne`: Not equal
///
/// ## Logical Operators
///
/// - `-a`: And
/// - `-o`: Or
///
/// ## Special Operators
///
/// - `!`: Not
/// - `-t`: File descriptor is associated with a terminal
///
/// # Notes
///
/// - The `test` command can be invoked using the `test` keyword or with square brackets `[ ]`.
/// - The function handles various error conditions, such as missing arguments or invalid syntax.
///
/// # Errors
///
/// - Returns `ShellError::from_syntax` if there is a syntax error or missing arguments.
/// - Returns `ShellError::from_syntax` if an expected comparison operator is missing.
///
/// # Safety
///
/// This function uses unsafe code to get the effective user ID and group ID using `geteuid` and `getegid`.
///
/// # Panics
///
/// This function does not panic.
pub fn test(mut argv: VecDeque<Tk>) -> Result<RshExitStatus, ShellError> {
	info!("Starting builtin test");
	let span = Span::from(argv.front().unwrap().span().start,argv.back().unwrap().span().end);
	let is_false = || -> Result<RshExitStatus, ShellError> { Ok(RshExitStatus::Fail { code: 1, cmd: Some("test".into()), span }) };
	let is_true = || -> Result<RshExitStatus, ShellError> { Ok(RshExitStatus::Success { span }) };
	let is_int = |tk: &Tk| -> bool { tk.text().parse::<i32>().is_ok() };
	let to_int = |tk: Tk| -> Result<i32, ShellError> {
		tk.text().parse::<i32>().map_err(|_| ShellError::from_syntax("Expected an integer in test", tk.span()))
	};
	let is_path = |tk: &Tk| -> bool { PathBuf::from(tk.text()).exists() };
	let to_meta = |tk: Tk| -> Result<fs::Metadata, ShellError> {
		fs::metadata(tk.text()).map_err(|_| ShellError::from_syntax(&format!("Test: Path '{}' does not exist", tk.text()), tk.span()))
	};
	let string_noop = |tk: Tk| -> Result<String, ShellError> { Ok(tk.text().into()) };
	let command = argv.pop_front().unwrap();
	let is_bracket = match command.text() {
		"[" => true,
		"test" => false,
		_ => unreachable!()
	};

	if is_bracket {
		if let Some(arg) = argv.back() {
			if arg.text() != "]" {
				return Err(ShellError::from_syntax("Test is missing a closing bracket", arg.span()))
			}
		} else {
			return Err(ShellError::from_syntax("Found a test with no arguments", command.span()))
		}
	}

	if let Some(arg) = argv.pop_front() {
		let result1 = match arg.text() {
			"!" => do_logical_op(&mut argv, command.clone(), true, arg)?,
			"-t" => do_test(&mut argv, to_int, |int| isatty(*int).is_ok(), arg.span())?,
			"-b" => do_test(&mut argv, to_meta, |meta| meta.file_type().is_block_device(), arg.span())?,
			"-c" => do_test(&mut argv, to_meta, |meta| meta.file_type().is_char_device(), arg.span())?,
			"-d" => do_test(&mut argv, to_meta, |meta| meta.is_dir(), arg.span())?,
			"-f" => do_test(&mut argv, to_meta, |meta| meta.is_file(), arg.span())?,
			"-g" => do_test(&mut argv, to_meta, |meta| meta.mode() & 0o2000 != 0, arg.span())?,
			"-G" => do_test(&mut argv, to_meta, |meta| meta.gid() == unsafe { getegid() }, arg.span())?,
			"-h" => do_test(&mut argv, to_meta, |meta| meta.is_symlink(), arg.span())?,
			"-L" => do_test(&mut argv, to_meta, |meta| meta.is_symlink(), arg.span())?,
			"-k" => do_test(&mut argv, to_meta, |meta| meta.mode() & 0o1000 != 0, arg.span())?,
			"-N" => do_test(&mut argv, to_meta, |meta| meta.mtime() > meta.atime(), arg.span())?,
			"-O" => do_test(&mut argv, to_meta, |meta| meta.uid() == unsafe { geteuid() }, arg.span())?,
			"-p" => do_test(&mut argv, to_meta, |meta| meta.file_type().is_fifo(), arg.span())?,
			"-s" => do_test(&mut argv, to_meta, |meta| meta.len() > 0, arg.span())?,
			"-S" => do_test(&mut argv, to_meta, |meta| meta.file_type().is_socket(), arg.span())?,
			"-u" => do_test(&mut argv, to_meta, |meta| meta.mode() & 0o4000 != 0, arg.span())?,
			"-n" => do_test(&mut argv, string_noop, |st| !st.is_empty(), arg.span())?,
			"-z" => do_test(&mut argv, string_noop, |st| st.is_empty(), arg.span())?,
			"-e" => do_test(&mut argv, string_noop, |st| Path::new(st).exists(), arg.span())?,
			"-r" => do_test(&mut argv, string_noop, |st| access(Path::new(st), AccessFlags::R_OK).is_ok(), arg.span())?,
			"-w" => do_test(&mut argv, string_noop, |st| access(Path::new(st), AccessFlags::W_OK).is_ok(), arg.span())?,
			"-x" => do_test(&mut argv, string_noop, |st| access(Path::new(st), AccessFlags::X_OK).is_ok(), arg.span())?,
			_ if is_int(&arg) => {
				if let Some(cmp) = argv.pop_front() {
					match cmp.text() {
						"-eq" => do_cmp(arg, &mut argv, to_int, |left, right| left == right)?,
						"-ge" => do_cmp(arg, &mut argv, to_int, |left, right| left >= right)?,
						"-gt" => do_cmp(arg, &mut argv, to_int, |left, right| left > right)?,
						"-le" => do_cmp(arg, &mut argv, to_int, |left, right| left <= right)?,
						"-lt" => do_cmp(arg, &mut argv, to_int, |left, right| left < right)?,
						"-ne" => do_cmp(arg, &mut argv, to_int, |left, right| left != right)?,
						_ => {
							return Err(ShellError::from_syntax("Expected an integer after comparison operator in test", cmp.span()));
						}
					}
				} else {
					return Err(ShellError::from_syntax("Expected a comparison operator after integer in test", command.span()));
				}
			}
			_ if is_path(&arg) => {
				if let Some(cmp) = argv.pop_front() {
					match cmp.text() {
						"-ef" => do_cmp(arg, &mut argv, to_meta, |left, right| left.dev() == right.dev() && left.ino() == right.ino())?,
						"-nt" => do_cmp(arg, &mut argv, to_meta, |left, right| left.mtime() > right.mtime())?,
						"-ot" => do_cmp(arg, &mut argv, to_meta, |left, right| left.mtime() < right.mtime())?,
						_ => {
							return Err(ShellError::from_syntax("Expected a file name after -b flag", cmp.span()));
						}
					}
				} else {
					return Err(ShellError::from_syntax("Expected a comparison operator after path name in test", command.span()));
				}
			}
			_ => {
				if argv.is_empty() {
					!arg.text().is_empty() // Default behavior is to return true if a string is not empty
				} else if let Some(cmp) = argv.pop_front() {
					match cmp.text() {
						"=" => do_cmp(arg, &mut argv, string_noop, |left, right| left == right)?,
						"!=" => do_cmp(arg, &mut argv, string_noop, |left, right| left != right)?,
						_ => {
							if cmp.text() == "==" {
								return Err(ShellError::from_syntax("`==` is not a valid comparison operator, use `=` instead", cmp.span()));
							} else {
								return Err(ShellError::from_syntax("Expected a comparison operator after string in test", cmp.span()));
							}
						}
					}
				} else {
					return Err(ShellError::from_syntax("Expected a comparison operator after string in test", command.span()));
				}
			}
		};
		if let Some(arg) = argv.pop_front() {
			match arg.text() {
				"-a" | "-o" => { // And/Or
					let result = do_logical_op(&mut argv, command, result1, arg)?;
					match result {
						true => return is_true(),
						false => return is_false(),
					}
				}
				"]" => {}
				_ => {
					return Err(ShellError::from_syntax("Unexpected extra argument found in test", arg.span()));
				}
			}
		}
		match result1 {
			true => is_true(),
			false => is_false(),
		}
	} else {
		Err(ShellError::from_syntax("Test called with no arguments", command.span()))
	}
}

pub fn alias(shellenv: &mut ShellEnv, node: Node) -> Result<RshExitStatus, ShellError> {
	let mut argv: VecDeque<Tk> = node.get_argv()?.into();
	argv.pop_front();
	while let Some(arg) = argv.pop_front() {
		if !token::REGEX["assignment"].is_match(arg.text()) {
			return Err(ShellError::from_syntax(&format!("Expected an assignment pattern in alias args, got {}",arg.text()), arg.span()))
		}
		let (alias,value) = arg.text().split_once('=').unwrap();
		shellenv.set_alias(alias.into(), value.into());
	}
	Ok(RshExitStatus::Success { span: node.span() })
}

pub fn cd(shellenv: &mut ShellEnv, node: Node) -> Result<RshExitStatus,ShellError> {
	let mut argv = node
		.get_argv()?
		.iter()
		.map(|arg| CString::new(arg.text()).unwrap())
		.collect::<VecDeque<CString>>();
	argv.pop_front();
	let new_pwd;
	if let Some(arg) = argv.pop_front() {
		new_pwd = arg;
	} else if let Some(home_path) = shellenv.get_variable("HOME") {
		new_pwd = CString::new(home_path).unwrap();
	} else {
		new_pwd = CString::new("/").unwrap();
	}
	let path = Path::new(new_pwd.to_str().unwrap());
	shellenv.change_dir(path, node.span())?;
	Ok(RshExitStatus::Success { span: node.span() })
}

pub fn source(shellenv: &mut ShellEnv, node: Node) -> Result<RshExitStatus,ShellError> {
	let mut argv = node
		.get_argv()?
		.iter()
		.map(|arg| CString::new(arg.text()).unwrap())
		.collect::<VecDeque<CString>>();
	argv.pop_front();
	for path in argv {
		dbg!(path.clone().into_string().unwrap());
		let file_path = Path::new(OsStr::from_bytes(path.as_bytes()));
		shellenv.source_file(file_path.to_path_buf(), node.span())?
	}
	Ok(RshExitStatus::Success { span: node.span() })
}

pub fn pwd(shellenv: &ShellEnv, span: Span) -> Result<RshExitStatus, ShellError> {
	if let Some(pwd) = shellenv.get_variable("PWD") {
		let stdout = helper::get_stdout();
		write(stdout, pwd.as_bytes()).map_err(|e| ShellError::from_io(&e.to_string(), span))?;
		Ok(RshExitStatus::Success { span })
	} else {
		Err(ShellError::from_execf("PWD environment variable is unset", 1, span))
	}
}

pub fn echo(node: Node, stdout: Option<RawFd>, stderr: Option<RawFd>) -> Result<RshExitStatus, ShellError> {
	let mut argv = node
		.get_argv()?
		.iter()
		.map(|arg| CString::new(arg.text()).unwrap())
		.collect::<VecDeque<CString>>();

	let redirs = node.get_redirs()?;
	log::info!("Executing echo with argv: {:?}", argv);

	let mut saved_fds = SavedFDs::new(0,1,2, node.span())?;
	argv.pop_front(); // Remove 'echo' from argv
	let output_str = catstr(argv, true);

	let fd_stack = if !redirs.is_empty() {
		open_file_descriptors(redirs.into())?
	} else {
		Vec::new()
	};

	let output_fd = stdout.unwrap_or(1); // Default to standard output
	let output = unsafe { BorrowedFd::borrow_raw(output_fd) };

	if let Some(fd) = stderr {
		dup2(output_fd,fd).unwrap();
		close(fd).unwrap();
	}

	let result = write(output.as_fd(), output_str.as_bytes()).map_err(|e| {
		ShellError::from_execf(&format!("Failed to write output in echo: {}", e), 1, node.span())
	});

	if let Some(w_pipe) = stdout {
		close(w_pipe).expect("failed to close stdout in echo");
	}

	close_file_descriptors(fd_stack);

	saved_fds.restore(0,1,2,node.span())?;

	result.map(|_| RshExitStatus::Success { span: node.span })
}
