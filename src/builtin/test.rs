use std::fs;
use std::os::unix::fs::{FileTypeExt, MetadataExt};

use nix::unistd::{access, getegid, geteuid, AccessFlags};

use crate::prelude::*;

use crate::{error::{LashErr::*, LashErrLow}, shellenv::Lash, LashResult};

pub fn run_test<T,F1,F2>(arg: Option<String>,alter: F1,check_property: F2) -> LashResult<bool>
where F1: FnOnce(&str) -> LashResult<T>, F2: FnOnce(&T) -> bool {
	if arg.is_none() {
		return Err(Low(LashErrLow::ExecFailed("Missing operand in this test call".into())))
	}
	let altered = alter(arg.unwrap().as_str()).map_err(|_| false);
	if altered.is_err() {
		return Ok(false)
	}
	Ok(check_property(&altered.unwrap()))
}

pub fn do_cmp<T,F1,F2>(lhs: &str, rhs: Option<String>, alter: F1, cmp: F2) -> LashResult<bool>
where F1: Fn(&str) -> LashResult<T>, F2: FnOnce(&T,&T) -> bool {
	if rhs.is_none() {
		return Err(Low(LashErrLow::ExecFailed("Missing operand in this test call".into())))
	}
	let lhs = alter(lhs).map_err(|_| false);
	let rhs = alter(rhs.unwrap().as_str()).map_err(|_| false);
	if lhs.is_err() || rhs.is_err() {
		return Ok(false)
	}
	Ok(cmp(&lhs.unwrap(),&rhs.unwrap()))
}

fn do_log_op<'a>(args: &mut VecDeque<String>, result: bool, operator: &str, lash: &mut Lash) -> LashResult<bool> {
	let rec_result = test(args,lash)?;
	match operator {
		"!" => Ok(!rec_result),
		"-a" => Ok(result && rec_result),
		"-o" => Ok(result || rec_result),
		_ => unreachable!()
	}
}

/// The test function is a special snowflake and takes a mutable reference to an already prepared arg vector
/// instead of a raw pair like the other builtins. This is to make recursion with -a/-o flags easier
pub fn test<'a>(test_call: &mut VecDeque<String>, lash: &mut Lash) -> LashResult<bool> {
	if test_call.back().is_some_and(|arg| arg.as_str() == "]") {
		test_call.pop_back();
	}
	// Here we define some useful closures to use later
	let is_int = |arg: &str| -> bool { arg.parse::<i32>().is_ok() };
	let to_int = |arg: &str| -> LashResult<i32> {
		arg.parse::<i32>().map_err(|_| Low(LashErrLow::InvalidSyntax("Expected an integer for this test flag".into())))
	};
	let is_path = |arg: &str| -> bool { Path::new(arg).exists() };
	let to_meta = |arg: &str| -> LashResult<fs::Metadata> {
		fs::metadata(arg).map_err(|_| Low(LashErrLow::InvalidSyntax("Invalid path used in test".into())))
	};
	let str_no_op = |arg: &str| -> LashResult<String> { Ok(arg.to_string()) };
	let mut result = false;

	// Now we will use our helper functions and pass those closures to use for type conversions on the arg string
	if let Some(arg) = test_call.pop_front() {
		result = match arg.as_str() {
			"!" => do_log_op(test_call, true, arg.as_str(), lash)?,
			"-t" => run_test(test_call.pop_front(), to_int, |int| isatty(*int).is_ok())?,
			"-b" => run_test(test_call.pop_front(), to_meta, |meta| meta.file_type().is_block_device())?,
			"-c" => run_test(test_call.pop_front(), to_meta, |meta| meta.file_type().is_char_device())?,
			"-d" => run_test(test_call.pop_front(), to_meta, |meta| meta.is_dir())?,
			"-f" => run_test(test_call.pop_front(), to_meta, |meta| meta.is_file())?,
			"-g" => run_test(test_call.pop_front(), to_meta, |meta| meta.mode() & 0o2000 != 0)?, // check setgid bit
			"-G" => run_test(test_call.pop_front(), to_meta, |meta| meta.gid() == u32::from(getegid()))?,
			"-h" => run_test(test_call.pop_front(), to_meta, |meta| meta.is_symlink())?,
			"-L" => run_test(test_call.pop_front(), to_meta, |meta| meta.is_symlink())?,
			"-k" => run_test(test_call.pop_front(), to_meta, |meta| meta.mode() & 0o1000 != 0)?, // check sticky bit
			"-N" => run_test(test_call.pop_front(), to_meta, |meta| meta.mtime() > meta.atime())?,
			"-O" => run_test(test_call.pop_front(), to_meta, |meta| meta.uid() == u32::from(geteuid()))?,
			"-p" => run_test(test_call.pop_front(), to_meta, |meta| meta.file_type().is_fifo())?,
			"-s" => run_test(test_call.pop_front(), to_meta, |meta| meta.len() > 0)?,
			"-S" => run_test(test_call.pop_front(), to_meta, |meta| meta.file_type().is_socket())?,
			"-u" => run_test(test_call.pop_front(), to_meta, |meta| meta.mode() & 0o4000 != 0)?, // check setuid bit
			"-n" => run_test(test_call.pop_front(), str_no_op, |st| !st.is_empty())?, // check setuid bit
			"-z" => run_test(test_call.pop_front(), str_no_op, |st| st.is_empty())?,
			"-e" => run_test(test_call.pop_front(), str_no_op, |st| Path::new(st).exists())?,
			"-r" => run_test(test_call.pop_front(), str_no_op, |st| access(Path::new(st),AccessFlags::R_OK).is_ok())?,
			"-w" => run_test(test_call.pop_front(), str_no_op, |st| access(Path::new(st),AccessFlags::W_OK).is_ok())?,
			"-x" => run_test(test_call.pop_front(), str_no_op, |st| access(Path::new(st),AccessFlags::X_OK).is_ok())?,
			_ if is_int(&arg.as_str()) => {
				if let Some(cmp) = test_call.pop_front() {
					match cmp.as_str() {
						"-eq" => do_cmp(arg.as_str(),test_call.pop_front(), to_int, |lhs, rhs| lhs == rhs)?,
						"-ge" => do_cmp(arg.as_str(),test_call.pop_front(), to_int, |lhs, rhs| lhs >= rhs)?,
						"-gt" => do_cmp(arg.as_str(),test_call.pop_front(), to_int, |lhs, rhs| lhs > rhs)?,
						"-le" => do_cmp(arg.as_str(),test_call.pop_front(), to_int, |lhs, rhs| lhs <= rhs)?,
						"-lt" => do_cmp(arg.as_str(),test_call.pop_front(), to_int, |lhs, rhs| lhs < rhs)?,
						"-ne" => do_cmp(arg.as_str(),test_call.pop_front(), to_int, |lhs, rhs| lhs != rhs)?,
						_ => return Err(Low(LashErrLow::InvalidSyntax("Expected an integer after comparison flag in test call".into())))
					}
				} else {
					return Err(Low(LashErrLow::InvalidSyntax("Expected a comparison flag after integer in test call".into())))
				}
			}
			_ if is_path(arg.as_str()) && test_call.front().is_some_and(|arg| matches!(arg.as_str(), "-ef" | "nt" | "-ot")) => {
				let cmp = test_call.pop_front().unwrap();
				match cmp.as_str() {
					"-ef" => do_cmp(cmp.as_str(), test_call.pop_front(), to_meta, |lhs, rhs| lhs.dev() == rhs.dev())?,
					"-nt" => do_cmp(cmp.as_str(), test_call.pop_front(), to_meta, |lhs, rhs| lhs.mtime() > rhs.mtime())?,
					"-ot" => do_cmp(cmp.as_str(), test_call.pop_front(), to_meta, |lhs, rhs| lhs.mtime() < rhs.mtime())?,
					_ => unreachable!()
				}
			}
			_ => {
				if test_call.is_empty() {
					!arg.as_str().is_empty()
				} else if matches!(arg.as_str(), "=") {
					// First arg encountered is an equal sign for some reason. Most likely, an expansion returned nothing, and now there's no word here.
					// Therefore, the only situations which return true are an equally empty right hand side, or a logical continuation flag
					let result = test_call.is_empty() || test_call.front().is_some_and(|arg| matches!(arg.as_str(), "-o" | "-a"));

					if test_call.front().is_some_and(|arg| !matches!(arg.as_str(), "-o" | "-a")) {
						test_call.pop_front();
					}
					result
				} else if let Some(cmp) = test_call.pop_front() {
					match cmp.as_str() {
						"=" => do_cmp(arg.as_str(), test_call.pop_front(), str_no_op, |lhs, rhs| lhs == rhs)?,
						"!=" => do_cmp(arg.as_str(), test_call.pop_front(), str_no_op, |lhs, rhs| lhs != rhs)?,
						_ => {
							if cmp.as_str() == "==" {
								return Err(Low(LashErrLow::InvalidSyntax("'==' is not a valid comparison operator for test calls. Use '=' instead.".into())));
							} else {
								return Err(Low(LashErrLow::InvalidSyntax("Expected either '==' or '!=' after string in test call".into())));
							}
						}
					}
				} else {
					return Err(Low(LashErrLow::InvalidSyntax("Expected either '==' or '!=' after string in test call".into())));
				}
			}
		};
		if let Some(arg) = test_call.pop_front() {
			let word = arg.as_str();
			if word == "-a" && !result {
				return Ok(result); // Short-circuit AND if already false
			}
			if word == "-o" && result {
				return Ok(result); // Short-circuit OR if already true
			}
			if word == "-a" || word == "-o" {
				result = do_log_op(test_call, result, word, lash)?;
			} else {
				return Err(Low(LashErrLow::InvalidSyntax(format!("Unexpected extra argument found in test call: {}",word))));
			}
		}
	}
	Ok(result)
}
