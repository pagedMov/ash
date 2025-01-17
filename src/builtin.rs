use std::collections::VecDeque;
use std::ffi::{CString, OsStr};
use std::fs;
use std::os::fd::AsRawFd;
use std::os::unix::ffi::OsStrExt;
use std::os::unix::fs::{FileTypeExt, MetadataExt};
use std::path::{Path, PathBuf};
use bitflags::bitflags;
use libc::{getegid, geteuid};
use nix::fcntl::OFlag;
use nix::sys::signal::{killpg, Signal};
use nix::sys::stat::Mode;
use nix::unistd::{access, fork, isatty, setpgid, AccessFlags, ForkResult, Pid};
use regex::Regex;

use crate::execute::{ProcIO, RshWait, RustFd};
use crate::interp::helper::{self, StrExtension};
use crate::interp::parse::{NdFlags, NdType, Node, Span};
use crate::interp::{expand, token};
use crate::interp::token::{Redir, RedirType, Tk, TkType, WdFlags};
use crate::shellenv::{self, disable_reaping, enable_reaping, read_jobs, read_logic, read_vars, write_jobs, write_logic, write_meta, write_vars, ChildProc, EnvFlags, HashFloat, JobBuilder, JobCmdFlags, JobID, RVal};
use crate::event::ShError;
use crate::RshResult;

pub const BUILTINS: [&str; 30] = [
	"type", "int", "bool", "arr", "float", "dict", "expr", "echo", "jobs", "unset", "fg", "bg", "set", "builtin", "test", "[", "shift", "unalias", "alias", "export", "cd", "readonly", "declare", "local", "unset", "trap", "node", "exec", "source", "wait",
];

bitflags! {
	#[derive(Debug)]
	pub struct EchoFlags: u8 {
		const USE_ESCAPE = 0b0001;
		const NO_NEWLINE = 0b0010;
		const NO_ESCAPE = 0b0100;
		const STDERR = 0b1000;
	}
}

fn open_file_descriptors(redirs: VecDeque<Node>) -> RshResult<Vec<RustFd>> {
	let mut fd_stack: Vec<RustFd> = Vec::new();
	let mut fd_dupes: Vec<(i32,i32)> = Vec::new();

	for redir_tk in redirs {
		if let NdType::Redirection { ref redir } = redir_tk.nd_type {
			let Redir { fd_source, op, fd_target, file_target } = redir;

			if let Some(target) = fd_target {
				fd_dupes.push((*target,*fd_source));
			} else if let Some(file_path) = file_target {
				let source_fd = RustFd::new(*fd_source)?;
				let flags = match op {
					RedirType::Input => OFlag::O_RDONLY,
					RedirType::Output => OFlag::O_WRONLY | OFlag::O_CREAT | OFlag::O_TRUNC,
					RedirType::Append => OFlag::O_WRONLY | OFlag::O_CREAT | OFlag::O_APPEND,
					_ => unimplemented!("Heredocs and herestrings are not implemented yet."),
				};
				let mut file_fd = RustFd::open(Path::new(file_path.text()), flags, Mode::from_bits(0o644).unwrap())?;
				file_fd.dup2(&source_fd)?;
				file_fd.close()?;
				fd_stack.push(source_fd);
			}
		}
	}

	while let Some((target,source)) = fd_dupes.pop() {
		let mut target_fd = RustFd::new(target)?;
		let source_fd = RustFd::new(source)?;
		target_fd.dup2(&source_fd)?;
		target_fd.close()?;

		fd_stack.push(source_fd);
	}

	Ok(fd_stack)
}

pub fn catstr(mut c_strings: VecDeque<CString>,newline: bool) -> CString {
	let mut cat: Vec<u8> = vec![];
	let newline_bytes = b"\n\0";
	let space_bytes = b" ";

	while let Some(c_string) = c_strings.pop_front() {
		let bytes = c_string.to_bytes_with_nul();
		if c_strings.is_empty() {
			// Last string: include the null terminator
			cat.extend_from_slice(&bytes[..bytes.len() - 1]);
		} else {
			// Intermediate strings: exclude the null terminator and add whitespace
			cat.extend_from_slice(&bytes[..bytes.len() - 1]);
			cat.extend_from_slice(space_bytes);
		}
	}
	if newline {
		cat.extend_from_slice(newline_bytes);
	} else {
		cat.extend_from_slice(b"\0");
	}

	CString::from_vec_with_nul(cat).unwrap()
}

pub fn r#type(node: Node) -> RshResult<RshWait> {
	let index_re = Regex::new(r"(\w+)\[(\d+)\]").unwrap();
	let mut argv = VecDeque::from(node.get_argv()?);
	argv.pop_front(); // Ignore `int`

	while let Some(arg) = argv.pop_front() {
		let text = arg.text();
		if let Some(caps) = index_re.captures(text) {
			let var_name = caps.get(1).map_or("", |m| m.as_str());
			let index = caps.get(2).map_or("", |m| m.as_str()).parse::<usize>().unwrap();

			if let Some(RVal::Array(_)) = read_vars(|v| v.get_var(var_name))? {
				if let Ok(arr_element) = read_vars(|v| v.index_arr(var_name, index))? {
					use crate::shellenv::RVal::*;
					match arr_element {
						String(_) => println!("string"),
						Int(_) => println!("int"),
						Float(_) => println!("float"),
						Bool(_) => println!("bool"),
						Array(_) => println!("array"),
						Dict(_) => println!("dict"),
					}
				}
			}
		} else if let Some(var) = read_vars(|v| v.get_var(text))? {
			use crate::shellenv::RVal::*;
			match var {
				String(_) => println!("string"),
				Int(_) => println!("int"),
				Float(_) => println!("float"),
				Bool(_) => println!("bool"),
				Array(_) => println!("array"),
				Dict(_) => println!("dict"),
			}
		}
	}
	Ok(RshWait::Success)
}

pub fn int(node: Node) -> RshResult<RshWait> {
	let mut argv = VecDeque::from(node.get_argv()?);
	argv.pop_front(); // Ignore `int`
	while let Some(arg) = argv.pop_front() {
		if let Some((k,v)) = arg.text().split_once('=') {
			helper::unset_var_conflicts(k)?;
			if let Ok(int) = v.parse::<i32>() {
				write_vars(|v| v.set_var(k, RVal::Int(int)))?
			} else {
				return Err(ShError::from_syntax(format!("Expected an integer in int definition, got `{}`",v).as_str(), node.span()))
			}
		} else {
			return Err(ShError::from_syntax(format!("Expected assignment syntax like `key=value`, got `{}`",arg.text()).as_str(), node.span()))
		}
	}
	Ok(RshWait::Success)
}

pub fn string(node: Node) -> RshResult<RshWait> {
	let mut argv = VecDeque::from(node.get_argv()?);
	argv.pop_front(); // Ignore `int`
	while let Some(arg) = argv.pop_front() {
		if let Some((k,val)) = arg.text().split_once('=') {
			helper::unset_var_conflicts(k)?;
			write_vars(|v| v.set_var(k, RVal::String(val.into())))?
		} else {
			return Err(ShError::from_syntax(format!("Expected assignment syntax like `key=value`, got `{}`",arg.text()).as_str(), node.span()))
		}
	}
	Ok(RshWait::Success)
}

pub fn bool(node: Node) -> RshResult<RshWait> {
	let mut argv = VecDeque::from(node.get_argv()?);
	argv.pop_front(); // Ignore `int`
	while let Some(arg) = argv.pop_front() {
		if let Some((k,val)) = arg.text().split_once('=') {
			helper::unset_var_conflicts(k)?;
			if let Ok(bool) = val.parse::<bool>() {
				write_vars(|v| v.set_var(k, RVal::Bool(bool)))?
			}
		} else {
			return Err(ShError::from_syntax(format!("Expected assignment syntax like `key=value`, got `{}`",arg.text()).as_str(), node.span()))
		}
	}
	Ok(RshWait::Success)
}

pub fn float(node: Node) -> RshResult<RshWait> {
	let mut argv = VecDeque::from(node.get_argv()?);
	argv.pop_front(); // Ignore `int`
	while let Some(arg) = argv.pop_front() {
		if let Some((k,val)) = arg.text().split_once('=') {
			helper::unset_var_conflicts(k)?;
			if let Ok(float) = val.parse::<f64>() {
				write_vars(|v| v.set_var(k, RVal::Float(HashFloat(float))))?
			}
		} else {
			return Err(ShError::from_syntax(format!("Expected assignment syntax like `key=value`, got `{}`",arg.text()).as_str(), node.span()))
		}
	}
	Ok(RshWait::Success)
}

pub fn array(node: Node) -> RshResult<RshWait> {
	let mut argv = VecDeque::from(node.get_argv()?);
	argv.pop_front(); // Ignore `int`
	while let Some(arg) = argv.pop_front() {
		if let Some((k,val)) = arg.text().split_once('=') {
			helper::unset_var_conflicts(k)?;
			if let Ok(array) = helper::parse_vec(val) {
				write_vars(|v| v.set_var(k, RVal::Array(array)))?
			}
		} else {
			return Err(ShError::from_syntax(format!("Expected assignment syntax like `key=value`, got `{}`",arg.text()).as_str(), node.span()))
		}
	}
	Ok(RshWait::Success)
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
/// * `transform` - A function that takes a `String` and returns a `Result<T, ShError>`.
/// * `property` - A function that takes a reference to `T` and returns a `bool`.
///
/// # Returns
///
/// * `Ok(bool)` - If the transformation and property check are successful, returns the result of the property check.
/// * `Err(ShError)` - If the `VecDeque` is empty or the transformation fails, returns an appropriate `ShError`.
///
/// # Example
///
/// ```
/// let mut args = VecDeque::new();
/// args.push_back("42".to_string());
/// let transform = |s: String| -> RshResult<i32> { Ok(s.parse::<i32>().unwrap()) };
/// let property = |x: &i32| -> bool { *x > 0 };
/// let result = do_test(&mut args, transform, property);
/// assert!(result.unwrap());
/// ```
fn do_test<T, F1, F2>(
	args: &mut VecDeque<Tk>,
	transform: F1,
	property: F2,
	span: Span
) -> RshResult<bool>
where
		F1: FnOnce(Tk) -> RshResult<T>,
		F2: FnOnce(&T) -> bool
{
	if let Some(st) = args.pop_front() {
		let transformed = transform(st).map_err(|_| false);
		if transformed.is_err() {
			return Ok(false)
		}
		Ok(property(&transformed.unwrap()))
	} else {
		Err(ShError::from_syntax("Did not find a comparison target for integer in test", span))
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
/// * `transform` - A function that takes a `String` and returns a `Result<T, ShError>`.
/// * `comparison` - A function that takes two references to `T` and returns a `bool`.
///
/// # Returns
///
/// * `Ok(bool)` - If the transformation and comparison are successful, returns the result of the comparison.
/// * `Err(ShError)` - If the `VecDeque` is empty or the transformation fails, returns an appropriate `ShError`.
///
/// # Example
///
/// ```
/// let mut args = VecDeque::new();
/// args.push_back("42".to_string());
/// let transform = |s: String| -> RshResult<i32> { Ok(s.parse::<i32>().unwrap()) };
/// let comparison = |x: &i32, y: &i32| -> bool { x == y };
/// let result = do_cmp("42".to_string(), &mut args, transform, comparison);
/// assert!(result.unwrap());
/// ```
fn do_cmp<T, F1, F2>(
	arg1: Tk,
	args: &mut VecDeque<Tk>,
	transform: F1,
	comparison: F2
) -> RshResult<bool>
where
		F1: Fn(Tk) -> RshResult<T>,
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
		Err(ShError::from_syntax("Did not find a comparison target for integer in test", arg1.span()))
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
/// * `Result<bool, ShError>` - Returns the combined result of the logical operation.
///
/// # Errors
///
/// * Returns `ShError::from_syntax` if there is a syntax error or missing arguments.
fn do_logical_op(
	args: &mut VecDeque<Tk>,
	command: Tk,
	result1: bool,
	operator: Tk
) -> RshResult<bool> {
	args.push_front(command);
	let result2 = test(std::mem::take(args)).map(|res| matches!(res,RshWait::Success ))?;
	match operator.text() {
		"!" => { Ok(!result2) },
		"-a" => { Ok(result1 && result2) },
		"-o" => { Ok(result1 || result2) },
		_ => Err(ShError::from_syntax("Expected a logical operator (-a or -o)", operator.span())),
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
/// * `Ok(RshWait)` - Returns a success or failure status based on the evaluation of the conditional expression.
/// * `Err(ShError)` - Returns an appropriate `ShError` if there is a syntax error or other issues with the arguments.
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
/// - Returns `ShError::from_syntax` if there is a syntax error or missing arguments.
/// - Returns `ShError::from_syntax` if an expected comparison operator is missing.
///
/// # Safety
///
/// This function uses unsafe code to get the effective user ID and group ID using `geteuid` and `getegid`.
///
/// # Panics
///
/// This function does not panic.
pub fn test(mut argv: VecDeque<Tk>) -> RshResult<RshWait> {
	let span = Span::from(argv.front().unwrap().span().start,argv.back().unwrap().span().end);
	let is_false = || -> RshResult<RshWait> { Ok(RshWait::Fail { code: 1, cmd: Some("test".into()), }) };
	let is_true = || -> RshResult<RshWait> { Ok(RshWait::Success ) };
	let is_int = |tk: &Tk| -> bool { tk.text().parse::<i32>().is_ok() };
	let to_int = |tk: Tk| -> RshResult<i32> {
		tk.text().parse::<i32>().map_err(|_| ShError::from_syntax("Expected an integer in test", tk.span()))
	};
	let is_path = |tk: &Tk| -> bool { PathBuf::from(tk.text()).exists() };
	let to_meta = |tk: Tk| -> RshResult<fs::Metadata> {
		fs::metadata(tk.text()).map_err(|_| ShError::from_syntax(&format!("Test: Path '{}' does not exist", tk.text()), tk.span()))
	};
	let string_noop = |tk: Tk| -> RshResult<String> { Ok(tk.text().into()) };
	let command = argv.pop_front().unwrap();
	let is_bracket = match command.text() {
		"[" => true,
		"test" => false,
		_ => unreachable!()
	};

	if is_bracket {
		if let Some(arg) = argv.back() {
			if arg.text() != "]" {
				return Err(ShError::from_syntax("Test is missing a closing bracket", arg.span()))
			}
		} else {
			return Err(ShError::from_syntax("Found a test with no arguments", command.span()))
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
							return Err(ShError::from_syntax("Expected an integer after comparison operator in test", cmp.span()));
						}
					}
				} else {
					return Err(ShError::from_syntax("Expected a comparison operator after integer in test", command.span()));
				}
			}
			_ if is_path(&arg) && argv.front().is_some_and(|arg| matches!(arg.text(), "-ef" | "-nt" | "-ot")) => {
				if let Some(cmp) = argv.pop_front() {
					match cmp.text() {
						"-ef" => do_cmp(arg, &mut argv, to_meta, |left, right| left.dev() == right.dev() && left.ino() == right.ino())?,
						"-nt" => do_cmp(arg, &mut argv, to_meta, |left, right| left.mtime() > right.mtime())?,
						"-ot" => do_cmp(arg, &mut argv, to_meta, |left, right| left.mtime() < right.mtime())?,
						_ => {
							return Err(ShError::from_syntax("Expected a file name after -b flag", cmp.span()));
						}
					}
				} else {
					return Err(ShError::from_syntax("Expected a comparison operator after path name in test", command.span()));
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
								return Err(ShError::from_syntax("`==` is not a valid comparison operator, use `=` instead", cmp.span()));
							} else {
								return Err(ShError::from_syntax("Expected a comparison operator after string in test", cmp.span()));
							}
						}
					}
				} else {
					return Err(ShError::from_syntax("Expected a comparison operator after string in test", command.span()));
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
					return Err(ShError::from_syntax("Unexpected extra argument found in test", arg.span()));
				}
			}
		}
		match result1 {
			true => is_true(),
			false => is_false(),
		}
	} else {
		Err(ShError::from_syntax("Test called with no arguments", command.span()))
	}
}

#[derive(Debug,Clone,PartialEq)]
pub enum ExprToken {
	Val { val: f64 },
	Mod,
	Div,
	IntDiv,
	Mult,
	Add,
	Sub,
	Exp,
	LParen,
	RParen
}

impl ExprToken {
	pub fn from(text: &str) -> Option<Self> {
		match text {
			_ if text.parse::<f64>().is_ok() => Some(Self::Val { val: text.parse::<f64>().unwrap() }),
			"+" => Some(Self::Add),
			"-" => Some(Self::Sub),
			"/" => Some(Self::Div),
			"**" => Some(Self::Exp),
			"*" => Some(Self::Mult),
			"%" => Some(Self::Mod),
			"//" => Some(Self::IntDiv),
			"(" => Some(Self::RParen),
			")" => Some(Self::LParen),
			_ => None
		}
	}
	pub fn calculate(&self, left: f64, right: f64) -> Option<f64> {
		match self {
			ExprToken::Add => Some(left + right),
			ExprToken::Sub => Some(left - right),
			ExprToken::Mult => Some(left * right),
			ExprToken::Div => {
				if right != 0.0 {
					Some(left / right)
				} else {
					None // Avoid division by zero
				}
			}
			ExprToken::Mod => {
				if right != 0.0 {
					Some(left % right)
				} else {
					None // Avoid modulo by zero
				}
			}
			ExprToken::IntDiv => {
				if right != 0.0 {
					Some((left / right).floor())
				} else {
					None // Avoid integer division by zero
				}
			}
			ExprToken::Exp => Some(left.powf(right)),
			_ => None, // LParen, RParen, and Val are not valid operators
		}
	}
	pub fn extract_val(&self) -> Option<f64> {
		if let ExprToken::Val { val } = self {
			Some(*val)
		} else {
			None
		}
	}
	pub fn precedence(&self) -> u8 {
		match self {
			ExprToken::Add | ExprToken::Sub => 1,
			ExprToken::Mult | ExprToken::Div | ExprToken::IntDiv | ExprToken::Mod | ExprToken::Exp => 2,
			_ => 0
		}
	}
	pub fn is_left_associative(&self) -> bool {
		matches!(self, ExprToken::Add | ExprToken::Sub | ExprToken::Mult | ExprToken::Div | ExprToken::IntDiv | ExprToken::Exp)
	}
}

pub fn tokenize_expr(input: &str) -> Vec<ExprToken> {
	let mut tokens = Vec::new();
	let mut current_token = String::new();
	let mut chars = input.chars().peekable();

	while let Some(ch) = chars.next() {
		match ch {
			' ' => process_current_token(&mut current_token, &mut tokens),
			'(' => {
				process_current_token(&mut current_token, &mut tokens);
				tokens.push(ExprToken::LParen);
			}
			')' => {
				process_current_token(&mut current_token, &mut tokens);
				tokens.push(ExprToken::RParen);
			}
			'+' | '-' | '*' | '/' | '%' => {
				process_current_token(&mut current_token, &mut tokens);
				handle_operator(ch, &mut chars, &mut tokens);
			}
			_ if ch.is_ascii_digit() || ch == '.' => current_token.push(ch),
			_ => panic!("Invalid character: {}", ch),
		}
	}

	process_current_token(&mut current_token, &mut tokens);
	tokens
}

fn process_current_token(current_token: &mut String, tokens: &mut Vec<ExprToken>) {
	if !current_token.is_empty() {
		if let Some(token) = ExprToken::from(current_token) {
			tokens.push(token);
			current_token.clear();
		} else {
			panic!("Invalid token: {}", current_token);
		}
	}
}

fn handle_operator(ch: char, chars: &mut std::iter::Peekable<std::str::Chars>, tokens: &mut Vec<ExprToken>) {
	if (ch == '*' || ch == '/') && chars.peek() == Some(&ch) {
		chars.next(); // Consume the second character
		tokens.push(ExprToken::from(&format!("{ch}{ch}")).unwrap());
	} else {
		tokens.push(ExprToken::from(&ch.to_string()).unwrap());
	}
}

/// Use the Shunting-Yard algorithm to reorganize in-set notation into reverse polish notation
pub fn shunting_yard(tokens: Vec<ExprToken>) -> Vec<ExprToken> {
	let mut output: VecDeque<ExprToken> = VecDeque::new();
	let mut operators: Vec<ExprToken> = Vec::new();

	for token in tokens {
		match token {
			ExprToken::Val { .. } => output.push_back(token),
			ExprToken::Add | ExprToken::Sub | ExprToken::Mult | ExprToken::Div | ExprToken::IntDiv | ExprToken::Mod | ExprToken::Exp => {
				while let Some(op) = operators.last() {
					if op.precedence() >= token.precedence() && token.is_left_associative() {
						output.push_back(operators.pop().unwrap());
					} else {
						break;
					}
				}
				operators.push(token);
			}
			ExprToken::LParen => operators.push(token),
			ExprToken::RParen => {
				while let Some(op) = operators.pop() {
					if let ExprToken::LParen = op {
						break;
					} else {
						output.push_back(op);
					}
				}
			}
		}
	}

	while let Some(op) = operators.pop() {
		output.push_back(op);
	}

	output.into()
}

fn float_to_string(value: f64) -> String {
	if value.fract() == 0.0 {
		// Convert to integer if there's no fractional part
		format!("{}", value as i64)
	} else {
		// Keep as a float with precision
		format!("{}", value)
	}
}

pub fn expr(node: Node, io: ProcIO) -> RshResult<RshWait> {
	let mut argv: VecDeque<Tk> = node.get_argv()?.into();
	argv.pop_front(); // Ignore `expr`

	let arg = argv.pop_front();
	let mut result: f64 = 0.0;

	if let Some(expr) = arg {
		if let TkType::String = expr.tk_type {
			let tokens = tokenize_expr(expr.text());

			let mut rpn = VecDeque::from(shunting_yard(tokens));
			let mut token_buffer = VecDeque::new();

			while let Some(token) = rpn.pop_front() {
				if let ExprToken::Val { .. } = token {
					// Push values directly onto the stack
					token_buffer.push_back(token);
				} else {
					// Pop operands for the operator
					let right = token_buffer.pop_back().ok_or_else(|| {
						ShError::InvalidSyntax("Missing right operand for operator".into(), node.span())
					})?;
					let r_val = right.extract_val().unwrap();

					let left = token_buffer.pop_back().ok_or_else(|| {
						ShError::InvalidSyntax("Missing left operand for operator".into(), node.span())
					})?;
					let l_val = left.extract_val().unwrap();

					// Perform the calculation
					if let Some(calculation) = token.calculate(l_val, r_val) {
						// Push the result back onto the stack
						token_buffer.push_back(ExprToken::Val { val: calculation });
						result = calculation; // Keep track of the final result
					} else {
						return Err(ShError::InvalidSyntax(
								format!("Invalid calculation for operator {:?}", token),
								node.span(),
						));
					}
				}
			}

			// At this point, token_buffer should contain the final result as the last value
			if token_buffer.len() != 1 {
				return Err(ShError::InvalidSyntax(
						"Malformed expression: multiple values remain in operand stack".into(),
						node.span(),
				));
			}
		} else {
			return Err(ShError::InvalidSyntax(
					"Expected a string in `expr` argument".into(),
					node.span(),
			));
		}
	}

	// Convert the final result to a string
	let result = float_to_string(result);

	// Do an internal echo call to display the result
	echo_internal(
		vec![result],
		io,
		node.span(),
		node.flags,
		node.redirs.clone()
	)
}


pub fn alias(node: Node) -> RshResult<RshWait> {
	let mut argv: VecDeque<Tk> = node.get_argv()?.into();
	argv.pop_front();
	while let Some(arg) = argv.pop_front() {
		if !token::REGEX["assignment"].is_match(arg.text()) {
			return Err(ShError::from_syntax(&format!("Expected an assignment pattern in alias args, got {}",arg.text()), arg.span()))
		}
		let (alias,value) = arg.text().split_once('=').unwrap();
		let value = value.trim_quotes();
		write_logic(|l| l.new_alias(alias, value.to_string()))?;
	}
	Ok(RshWait::Success )
}

pub fn unalias(node: Node) -> RshResult<RshWait> {
	let mut argv: VecDeque<Tk> = node.get_argv()?.into();
	argv.pop_front();
	while let Some(arg) = argv.pop_front() {
		let alias = arg.text();
		if read_logic(|l| l.get_alias(alias))?.is_some() {
			write_logic(|l| l.remove_alias(alias))?;
		} else {
			eprintln!("Alias not found for `{}`",alias)
		}
	}
	Ok(RshWait::Success)
}

pub fn cd(node: Node) -> RshResult<RshWait> {
	let mut argv = node
		.get_argv()?
		.iter()
		.map(|arg| CString::new(arg.text()).unwrap())
		.collect::<VecDeque<CString>>();
		argv.pop_front();
		let new_pwd;
		if let Some(arg) = argv.pop_front() {
			new_pwd = arg;
		} else if let Some(home_path) = read_vars(|vars| vars.get_evar("HOME"))? {
			new_pwd = CString::new(home_path).unwrap();
		} else {
			new_pwd = CString::new("/").unwrap();
		}
		let path = Path::new(new_pwd.to_str().unwrap());
		std::env::set_current_dir(path).map_err(|_| ShError::from_io())?;
		write_vars(|v| v.export_var("PWD", std::env::current_dir().unwrap().to_str().unwrap()))?;
		Ok(RshWait::Success )
}

/// Brings a background or stopped job to the foreground and manages its execution.
///
/// Logic:
/// 1. Parse the command arguments:
///    - Retrieve the job ID from the arguments (default to `0` if none is provided).
///    - Job ID `0` indicates the "current job" (the most recently backgrounded or stopped job).
///
/// 2. Resolve the job to foreground:
///    - If job ID is `0`, fetch the most recent job from the job table (`job_order.last()`).
///    - Otherwise, retrieve the job corresponding to the provided job ID.
///    - If no job is found, return an error or indicate failure.
///
/// 3. Bring the job to the foreground:
///    - Update the job state in the job table (e.g., mark it as foregrounded and continue it).
///    - If the job is stopped, send it a `SIGCONT` signal to resume execution.
///
/// 4. Reattach the terminal to the job's process group:
///    - Use `attach_tty` to give the job control of the terminal.
///    - Handle any errors that may occur during this step.
///
/// 5. Return the result:
///    - If the job was successfully foregrounded, return `RshWait::Success`.
///    - If the job does not exist or cannot be foregrounded, return `RshWait::Fail` with appropriate details.
///
/// Key Considerations:
/// - Ensure the `job_order` in the job table reflects the correct order of jobs for determining the "current job."
/// - Handle cases where no jobs exist gracefully.
/// - Carefully manage the interaction between the job table and the process group state.
/// - Ensure terminal attachment and signal handling are robust to avoid leaving the shell in an inconsistent state.
pub fn fg(node: Node) -> RshResult<RshWait> {
	let argv = node.get_argv()?;
	let mut argv = VecDeque::from(argv);
	argv.pop_front(); // Ignore 'fg'

	if read_jobs(|j| j.get_fg().is_some())? {
		return Err(ShError::from_internal("Somehow called `fg()` when a foreground process already exists"))
	}

	let curr_job_id = read_jobs(|j| j.curr_job())?;
	if curr_job_id.is_none() {
		return Err(ShError::from_execf("Did not find a task to move to the foreground", 1, node.span()))
	}
	let job_id = match argv.pop_front() {
		Some(arg) => parse_job_id(arg.text(), node.span())?,
		None => curr_job_id.unwrap(),
	};
	write_jobs(|j| {
		let id = JobID::TableID(job_id);
		let query_result = j.query(id.clone());
		if let Some(job) = query_result {
			job.killpg(Signal::SIGCONT).map_err(|_| ShError::from_internal("Failed to send SIGCONT to the job"))?;
			j.bg_to_fg(id);
		} else {
			return Err(ShError::from_execf(format!("Job ID {} not found", job_id).as_str(), 1, node.span()));
		}
		Ok(())
	})?;
	Ok(RshWait::Success)
}

pub fn bg(node: Node) -> RshResult<RshWait> {
	let argv = node.get_argv()?;
	let mut argv = VecDeque::from(argv);
	argv.pop_front(); // Ignore 'bg'

	// Determine the job ID to move to the background
	let curr_job_id = read_jobs(|j| j.curr_job())?;
	if curr_job_id.is_none() {
		return Err(ShError::from_execf("Did not find a task to move to the background", 1, node.span()));
	}
	let job_id = match argv.pop_front() {
		Some(arg) => parse_job_id(arg.text(), node.span())?,
		None => curr_job_id.unwrap(),
	};

	// Perform the job transition
	read_jobs(|j| {
		let id = JobID::TableID(job_id);
		let query_result = j.query(id.clone());
		if let Some(job) = query_result {
			job.killpg(Signal::SIGCONT).map_err(|_| ShError::from_internal("Failed to send SIGCONT to the job"))?;
		} else {
			return Err(ShError::from_execf(format!("Job ID {} not found", job_id).as_str(), 1, node.span()));
		}
		Ok(())
	})?;

	// Print job details
	read_jobs(|j| {
		if let Some(job) = j.query(JobID::TableID(job_id)) {
			let job_order = j.job_order().to_vec();
			println!("{}", job.display(&job_order, JobCmdFlags::PIDS));
		}
	})?;

	Ok(RshWait::Success)
}

fn parse_job_id(arg: &str, span: Span) -> RshResult<usize> {
	if arg.starts_with('%') {
		let arg = arg.strip_prefix('%').unwrap();
		if arg.chars().all(|ch| ch.is_ascii_digit()) {
			return Ok(arg.parse::<usize>().unwrap());
		} else {
			let result = write_jobs(|j| {
				let query_result = j.query(JobID::Command(arg.into()));
				query_result.map(|job| job.table_id().unwrap())
			})?;
			match result {
				None => return Err(ShError::from_internal("Found a job but no table id in parse_job_id")),
				Some(id) => return Ok(id),
			}
		}
	} else if arg.chars().all(|ch| ch.is_ascii_digit()) {
		let result = write_jobs(|j| {
			let pgid_query_result = j.query(JobID::Pgid(Pid::from_raw(arg.parse::<i32>().unwrap())));
			if let Some(job) = pgid_query_result {
				return Some(job.table_id().unwrap());
			}

			if arg.parse::<i32>().unwrap() > 0 {
				let table_id_query_result = j.query(JobID::TableID(arg.parse::<usize>().unwrap()));
				return table_id_query_result.map(|job| job.table_id().unwrap());
			}

			None
		})?;

		match result {
			None => return Err(ShError::from_internal("Found a job but no table id in parse_job_id")),
			Some(id) => return Ok(id),
		}
	} else {
		return Err(ShError::from_syntax(format!("Invalid argument: {}", arg).as_str(), span));
	}
}

pub fn source(node: Node) -> RshResult<RshWait> {
	let mut argv = node
		.get_argv()?
		.iter()
		.map(|arg| CString::new(arg.text()).unwrap())
		.collect::<VecDeque<CString>>();
		argv.pop_front();
		for path in argv {
			let file_path = Path::new(OsStr::from_bytes(path.as_bytes()));
			shellenv::source_file(file_path.to_path_buf()).unwrap()
		}
		Ok(RshWait::Success )
}

fn flags_from_chars(chars: &str) -> EnvFlags {
	let flag_list = chars.chars();
	let mut env_flags = EnvFlags::empty();
	for ch in flag_list {
		match ch {
			'a' => env_flags |= EnvFlags::EXPORT_ALL_VARS,
			'b' => env_flags |= EnvFlags::REPORT_JOBS_ASAP,
			'e' => env_flags |= EnvFlags::EXIT_ON_ERROR,
			'f' => env_flags |= EnvFlags::NO_GLOB,
			'h' => env_flags |= EnvFlags::HASH_CMDS,
			'k' => env_flags |= EnvFlags::ASSIGN_ANYWHERE,
			'm' => env_flags |= EnvFlags::ENABLE_JOB_CTL,
			'n' => env_flags |= EnvFlags::NO_EXECUTE,
			'r' => env_flags |= EnvFlags::ENABLE_RSHELL,
			't' => env_flags |= EnvFlags::EXIT_AFTER_EXEC,
			'u' => env_flags |= EnvFlags::UNSET_IS_ERROR,
			'v' => env_flags |= EnvFlags::PRINT_INPUT,
			'x' => env_flags |= EnvFlags::STACK_TRACE,
			'B' => env_flags |= EnvFlags::EXPAND_BRACES,
			'C' => env_flags |= EnvFlags::NO_OVERWRITE,
			'E' => env_flags |= EnvFlags::INHERIT_ERR,
			'H' => env_flags |= EnvFlags::HIST_SUB,
			'P' => env_flags |= EnvFlags::NO_CD_SYMLINKS,
			'T' => env_flags |= EnvFlags::INHERIT_RET,
			_ => eprintln!("set: no such option '{}'",ch)
		}
	}
	env_flags
}

pub fn set_or_unset(node: Node, set: bool) -> RshResult<RshWait> {
	let span = node.span();
	if let NdType::Builtin { mut argv } = node.nd_type {
		argv.pop_front(); // Ignore 'set'
		if argv.front().is_none_or(|arg| !arg.text().starts_with('-')) {
			return Err(ShError::from_execf("Invalid 'set' invocation", 1, span))
		}
		let flag_arg = argv.pop_front().unwrap();
		let set_flags = flag_arg.text();
		let set_flags = set_flags.strip_prefix('-').unwrap();
		let env_flags = flags_from_chars(set_flags);
		match set {
			true => write_meta(|m| m.mod_flags(|f| *f |= env_flags))?,
			false => write_meta(|m| m.mod_flags(|f| *f &= !env_flags))?,
		}
		Ok(RshWait::new())
	} else { unreachable!() }
}

pub fn pwd(span: Span) -> RshResult<RshWait> {
	if let Some(pwd) = read_vars(|v| v.get_evar("PWD"))? {
		let stdout = RustFd::from_stdout()?;
		stdout.write(pwd.as_bytes())?;
		Ok(RshWait::Success )
	} else {
		Err(ShError::from_execf("PWD environment variable is unset", 1, span))
	}
}

pub fn export(node: Node) -> RshResult<RshWait> {
	let last_status = RshWait::Success ;
	if let NdType::Builtin { mut argv } = node.nd_type {
		argv.pop_front(); // Ignore "export"
		while let Some(ass) = argv.pop_front() {
			if let Some((key,value)) = ass.text().split_once('=') {
				write_vars(|v| v.export_var(key, value))?;
			} else {
				return Err(ShError::from_execf(format!("Expected a variable assignment in export, got this: {}", ass.text()).as_str(), 1, ass.span()))
			}
		}
		Ok(last_status)
	} else { unreachable!() }
}

pub fn jobs(node: Node, mut io: ProcIO,) -> RshResult<RshWait> {
	let mut argv = node.get_argv()?.into_iter().collect::<VecDeque<Tk>>();
	argv.pop_front();
	let mut flags = JobCmdFlags::empty();
	while let Some(arg) = argv.pop_front() {
		let mut chars = arg.text().chars().collect::<VecDeque<char>>();
		if chars.front().is_none_or(|ch| *ch != '-') {
			return Err(ShError::from_execf("Invalid flag in `jobs` invocation", 1, node.span()))
		}
		chars.pop_front(); // Ignore the leading hyphen
		while let Some(ch) = chars.pop_front() {
			let flag = match ch {
				'l' => JobCmdFlags::LONG,
				'p' => JobCmdFlags::PIDS,
				'n' => JobCmdFlags::NEW_ONLY,
				'r' => JobCmdFlags::RUNNING,
				's' => JobCmdFlags::STOPPED,
				_ => return Err(ShError::from_execf("Invalid flag in `jobs` invocation", 1, node.span()))
			};
			flags |= flag;
		}
	}

	match unsafe { fork() } {
		Ok(ForkResult::Child) => {
			read_jobs(|j| j.print_jobs(&flags))?;
			std::process::exit(0);
		}
		Ok(ForkResult::Parent { child }) => {
			write_jobs(|j| j.reset_recents())?;
			setpgid(child, child).map_err(|_| ShError::from_io())?;
			let children = vec![
				ChildProc::new(child, Some("echo"))?
			];
			let job = JobBuilder::new()
				.with_pgid(child)
				.with_children(children)
				.build();

			if node.flags.contains(NdFlags::BACKGROUND) {
				write_jobs(|j| j.insert_job(job))??;
			} else {
				disable_reaping();
				write_jobs(|j| j.new_fg(job))??;
				enable_reaping()?;
			}
		}
		Err(_) => return Err(ShError::from_internal("Failed to fork in print_jobs()"))
	}
	Ok(RshWait::Success)
}

/// Used to call echo internally by passing a vector of strings. Makes it very simple to re-use the I/O logic present in echo elsewhere in the codebase.
pub fn echo_internal(argv: Vec<String>, io: ProcIO, span: Span, flags: NdFlags, redirs: VecDeque<Node>) -> RshResult<RshWait> {
	let mut tokens = vec![
		Tk {
			tk_type: TkType::Ident,
			wd: token::WordDesc {
				text: "echo".into(),
				span,
				flags: WdFlags::empty()
			}
		}
	];
	for arg in argv {
		let token = Tk {
			tk_type: TkType::String,
			wd: token::WordDesc {
				text: arg,
				span,
				flags: WdFlags::empty(),
			},
		};
		tokens.push(token);
	}
	let echo_call = Node {
		command: tokens.first().cloned(),
		nd_type: NdType::Builtin {
			argv: tokens.into(),
		},
		flags,
		redirs,
		span,
	};
	echo(echo_call, io)
}

pub fn echo(node: Node, mut io: ProcIO,) -> RshResult<RshWait> {
	let mut flags = EchoFlags::empty();
	let mut argv = node.get_argv()?.into_iter().collect::<VecDeque<Tk>>();
	argv.pop_front(); // Remove 'echo' from argv
										// Get flags
	if argv.front().is_some_and(|arg| arg.text().starts_with('-')) {
		let next_arg = argv.pop_front().unwrap();
		let mut options = next_arg.text().strip_prefix('-').unwrap().chars();
		loop {
			match options.next() {
				Some('e') => {
					if flags.contains(EchoFlags::NO_ESCAPE) {
						flags &= !EchoFlags::NO_ESCAPE
					}
					flags |= EchoFlags::USE_ESCAPE
				}
				Some('r') => flags |= EchoFlags::STDERR,
				Some('n') => flags |= EchoFlags::NO_NEWLINE,
				Some('E') => {
					if flags.contains(EchoFlags::USE_ESCAPE) {
						flags &= !EchoFlags::USE_ESCAPE
					}
					flags |= EchoFlags::NO_ESCAPE
				}
				_ => break
			}
		}
		if flags.is_empty() {
			argv.push_front(next_arg);
		}
	}
	let argv = argv.into_iter().map(|tk| {
		let text = tk.text();
		if flags.contains(EchoFlags::USE_ESCAPE) {
			CString::new(helper::process_ansi_escapes(text)).unwrap()
		} else {
			CString::new(tk.text()).unwrap()
		}
	}).collect::<VecDeque<CString>>();

	let redirs = node.get_redirs()?;

	let newline_opt = !flags.contains(EchoFlags::NO_NEWLINE);
	let output_str = catstr(argv, newline_opt);
	let mut fd_stack = vec![];
	fd_stack.extend(open_file_descriptors(redirs.into())?);

	let output_fd = if flags.contains(EchoFlags::STDERR) {
		if let Some(ref err_fd) = io.stderr {
			err_fd.lock().unwrap().dup().unwrap_or_else(|_| RustFd::from_stderr().unwrap())
		} else {
			RustFd::from_stderr()?
		}
	} else if let Some(ref out_fd) = io.stdout {
		out_fd.lock().unwrap().dup().unwrap_or_else(|_| RustFd::from_stdout().unwrap())
	} else {
		RustFd::from_stdout()?
	};

	if let Some(ref fd) = io.stderr {
		if !flags.contains(EchoFlags::STDERR) {
			let fd = fd.lock().unwrap();
			output_fd.dup2(&fd.as_raw_fd())?;
		}
	}

	io.backup_fildescs()?;
	io.route_io()?;

	if node.flags.contains(NdFlags::IN_PIPE) {
		output_fd.write(output_str.as_bytes())?;
		std::process::exit(0);
	}

	match unsafe { fork() } {
		Ok(ForkResult::Child) => {
			output_fd.write(output_str.as_bytes())?;
			std::process::exit(0);
		}
		Ok(ForkResult::Parent { child }) => {
			for fd in &mut fd_stack {
				fd.close()?
			}

			setpgid(child, child).map_err(|_| ShError::from_io())?;
			let children = vec![
				ChildProc::new(child, Some("echo"))?
			];
			let job = JobBuilder::new()
				.with_pgid(child)
				.with_children(children)
				.build();

			if node.flags.contains(NdFlags::BACKGROUND) {
				write_jobs(|j| j.insert_job(job))??;
			} else {
				disable_reaping();
				write_jobs(|j| j.new_fg(job))??;
				enable_reaping()?;
			}
		}
		Err(_) => return Err(ShError::from_internal("Failed to fork in echo()"))
	}

	io.restore_fildescs()?;
	Ok(RshWait::Success)
}
