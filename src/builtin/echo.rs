use crate::{prelude::*, utils};

use crate::{error::{LashErr::*, LashErrHigh}, helper::{self, StrExtension}, shellenv::{write_jobs, ChildProc, JobBuilder, Lash}, LashResult, pest_ext::Rule};

bitflags! {
	#[derive(Debug)]
	pub struct EchoFlags: u8 {
		const USE_ESCAPE = 0b00001;
		const NO_NEWLINE = 0b00010;
		const NO_ESCAPE = 0b00100;
		const STDERR = 0b01000;
		const EXPAND_OX_ESC = 0b10000;
	}
}
pub fn execute<'a>(echo_call: Pair<'a,Rule>, lash: &mut Lash) -> LashResult<()> {
	let mut flags = EchoFlags::empty();
	let blame = echo_call.clone();
	let mut argv = helper::prepare_argv(echo_call.clone(),lash)?;
	argv.pop_front();
	let mut arg_buffer = vec![];
	let redirs = helper::prepare_redirs(echo_call)?;
	lash.ctx_mut().extend_redirs(redirs);

	while let Some(arg) = argv.pop_front() {
		if arg.as_str().starts_with('-') {
			let mut options = arg.as_str().strip_prefix('-').unwrap().chars();
			let mut new_flags = EchoFlags::empty();
			while let Some(opt) = options.next() {
				match opt {
					'e' => {
						if new_flags.contains(EchoFlags::NO_ESCAPE) {
							new_flags &= !EchoFlags::NO_ESCAPE
						}
						new_flags |= EchoFlags::USE_ESCAPE
					}
					'r' => new_flags |= EchoFlags::STDERR,
					'n' => new_flags |= EchoFlags::NO_NEWLINE,
					'P' => new_flags |= EchoFlags::EXPAND_OX_ESC,
					'E' => {
						if new_flags.contains(EchoFlags::USE_ESCAPE) {
							new_flags &= !EchoFlags::USE_ESCAPE
						}
						new_flags |= EchoFlags::NO_ESCAPE
					}
					_ => break
				}
			}
			if new_flags.is_empty() {
				arg_buffer.push(arg.as_str().trim_quotes().to_string());
			} else {
				flags |= new_flags;
			}
		} else {
			arg_buffer.push(arg.as_str().trim_quotes().to_string());
		}
	}

	let output = arg_buffer.join(" ");

	let newline = !flags.contains(EchoFlags::NO_NEWLINE);

	let mut target_fd = if flags.contains(EchoFlags::STDERR) {
		utils::RustFd::new(STDERR_FILENO)?
	} else {
		utils::RustFd::new(STDOUT_FILENO)?
	};

	lash.ctx_mut().activate_redirs()?;

	if lash.borrow_ctx().flags().contains(utils::ExecFlags::NO_FORK) {
		if newline {
			writeln!(target_fd,"{}",output)?;
		} else {
			write!(target_fd,"{}",output)?;
		}
		std::process::exit(0);
	}
	match unsafe { fork() } {
		Ok(ForkResult::Child) => {
			if newline {
				writeln!(target_fd,"{}",output)?;
			} else {
				write!(target_fd,"{}",output)?;
			}
			std::process::exit(0);
		}
		Ok(ForkResult::Parent { child }) => {
			setpgid(child, child).map_err(|_| High(LashErrHigh::io_err(blame.clone())))?;
			let children = vec![
				ChildProc::new(child, Some("echo"), None)?
			];
			let job = JobBuilder::new()
				.with_pgid(child)
				.with_children(children)
				.build();

			if lash.borrow_ctx().flags().contains(utils::ExecFlags::BACKGROUND) {
				write_jobs(|j| j.insert_job(job,false))??;
			} else {
				helper::handle_fg(lash,job)?;
			}
		}
		Err(_) => return Err(High(LashErrHigh::exec_err("Failed to fork in echo()", blame)))
	}

	Ok(())
}
