use std::{collections::VecDeque, ffi::CString, os::fd::AsRawFd};

use nix::unistd::{fork, setpgid, ForkResult};
use pest::iterators::Pair;

use crate::{error::{LashErr::*, LashErrHigh}, execute::{CmdRedirs, ExecCtx, ExecFlags, Redir, RustFd}, helper, shellenv::{write_jobs, ChildProc, JobBuilder}, LashResult, Rule};

pub const BUILTINS: [&str; 41] = [
	"return", "break", "contine", "exit", "command", "pushd", "popd", "setopt", "getopt", "type", "string", "int", "bool", "arr", "float", "dict", "expr", "echo", "jobs", "unset", "fg", "bg", "set", "builtin", "test", "[", "shift", "unalias", "alias", "export", "cd", "readonly", "declare", "local", "unset", "trap", "node", "exec", "source", "read_func", "wait",
];

bitflags::bitflags! {
	#[derive(Debug)]
	pub struct EchoFlags: u8 {
		const USE_ESCAPE = 0b00001;
		const NO_NEWLINE = 0b00010;
		const NO_ESCAPE = 0b00100;
		const STDERR = 0b01000;
		const EXPAND_OX_ESC = 0b10000;
	}
	pub struct CdFlags: u8 {
		const CHANGE = 0b0001;
		const PUSH = 0b0010;
		const POP = 0b0100;
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

pub fn echo<'a>(echo_call: Pair<'a,Rule>, ctx: &mut ExecCtx) -> LashResult<()> {
	let mut flags = EchoFlags::empty();
	let blame = echo_call.clone();
	let mut inner = echo_call.into_inner();
	let mut argv = vec![];

	while let Some(arg) = inner.next() {
		match arg.as_rule() {
			Rule::cmd_name => continue,
			Rule::word => {
				if arg.as_str().starts_with('-') {
					let mut options = arg.as_str().strip_prefix('-').unwrap().chars();
					while let Some(opt) = options.next() {
						match opt {
							'e' => {
								if flags.contains(EchoFlags::NO_ESCAPE) {
									flags &= !EchoFlags::NO_ESCAPE
								}
								flags |= EchoFlags::USE_ESCAPE
							}
							'r' => flags |= EchoFlags::STDERR,
							'n' => flags |= EchoFlags::NO_NEWLINE,
							'P' => flags |= EchoFlags::EXPAND_OX_ESC,
							'E' => {
								if flags.contains(EchoFlags::USE_ESCAPE) {
									flags &= !EchoFlags::USE_ESCAPE
								}
								flags |= EchoFlags::NO_ESCAPE
							}
							_ => break
						}
						if flags.is_empty() {
							argv.push(CString::new(arg.as_str()).unwrap());
						}
					}
				} else {
					argv.push(CString::new(arg.as_str()).unwrap());
				}
			}
			Rule::redir => ctx.push_redir(Redir::from_pair(arg)?),
			_ => unreachable!()
		}
	}

	let newline = !flags.contains(EchoFlags::NO_NEWLINE);

	let output = catstr(VecDeque::from(argv), newline);
	let io = ctx.io_mut();
	let target_fd = if flags.contains(EchoFlags::STDERR) {
		if let Some(ref err_fd) = io.stderr {
			err_fd.lock().unwrap().dup().unwrap_or_else(|_| RustFd::from_stderr().unwrap())
		} else {
			RustFd::new(2)?
		}
	} else if let Some(ref out_fd) = io.stdout {
			out_fd.lock().unwrap().dup().unwrap_or_else(|_| RustFd::from_stdout().unwrap())
		} else {
			RustFd::new(1)?
	};

	if let Some(ref fd) = io.stderr {
		if !flags.contains(EchoFlags::STDERR) {
			let fd = fd.lock().unwrap();
			target_fd.dup2(&fd.as_raw_fd())?;
		}
	}

	let mut redirs = CmdRedirs::new(ctx.redirs());
	redirs.activate()?;

	if ctx.flags().contains(ExecFlags::NO_FORK) {
		target_fd.write(output.as_bytes())?;
	}
	match unsafe { fork() } {
		Ok(ForkResult::Child) => {
			target_fd.write(output.as_bytes()).unwrap();
			std::process::exit(0);
		}
		Ok(ForkResult::Parent { child }) => {
			redirs.close_all()?;

			setpgid(child, child).map_err(|_| High(LashErrHigh::io_err(blame.clone())))?;
			let children = vec![
				ChildProc::new(child, Some("echo"), None)?
			];
			let job = JobBuilder::new()
				.with_pgid(child)
				.with_children(children)
				.build();

			if ctx.flags().contains(ExecFlags::BACKGROUND) {
				write_jobs(|j| j.insert_job(job,false))??;
			} else {
				helper::handle_fg(job);
			}
		}
		Err(_) => return Err(High(LashErrHigh::exec_err("Failed to fork in echo()", blame)))
	}

	Ok(())
}
