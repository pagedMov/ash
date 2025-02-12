use crate::{helper, utils, prelude::*, shellenv::{ChildProc, JobBuilder}};

use super::dispatch;

pub fn exec_pipeline<'a>(pipeline: Pair<'a,Rule>, lash: &mut Lash) -> LashResult<()> {
	let blame = pipeline.clone();
	let (in_redirs,out_redirs) = lash.ctx_mut().sort_redirs();
	let _ = lash.ctx_mut().take_redirs();

	let mut inner = pipeline.into_inner().peekable();
	let mut prev_read_pipe: Option<utils::RustFd> = None;
	let mut pgid: Option<Pid> = None;
	let mut cmds: Vec<String> = vec![];
	let mut pids: Vec<Pid> = vec![];

	let mut first = true;
	while let Some(node) = inner.next() {
		let (r_pipe,w_pipe) = if inner.peek().is_some() {
			let (r_pipe,w_pipe) = utils::RustFd::pipe()?;
			(Some(r_pipe),Some(w_pipe))
		} else {
			(None,None)
		};

		let cmd_check = node.clone();
		cmds.push(helper::get_pipeline_cmd(cmd_check)?);

		match unsafe { fork() } {
			Ok(ForkResult::Child) => {
				if let Some(mut pipe) = r_pipe {
					pipe.close()?
				}
				let _ = prev_read_pipe.as_ref()
					.map(|r| utils::Redir::from_raw(0, r.as_raw_fd()))
					.and_then(|redir| Some(lash.ctx_mut().push_redir(redir)));
				let _ = w_pipe.as_ref()
					.map(|w| utils::Redir::from_raw(1, w.as_raw_fd()))
					.and_then(|redir| Some(lash.ctx_mut().push_redir(redir)));
				*lash.ctx_mut().flags_mut() |= utils::ExecFlags::NO_FORK;
				// These two if statements handle the case of existing i/o for the pipeline
				// Stuff like shell functions in the middle of pipelines
				if first {
					// If the pipeline started with input, redirect it here
					lash.ctx_mut().extend_redirs(in_redirs.into());
				}
				if inner.peek().is_none() {
					// If the pipeline ends with output, redirect it here
					lash.ctx_mut().extend_redirs(out_redirs.into());
				}

				dispatch::dispatch_exec(node, lash)?;
				std::process::exit(1)
			}
			Ok(ForkResult::Parent { child }) => {
				if let Some(mut pipe) = w_pipe {
					pipe.close()?
				}
				prev_read_pipe = r_pipe;
				pids.push(child);
				if pgid.is_none() {
					pgid = Some(child);
				}
				if inner.peek().is_none() {
					let mut children = vec![];
					let mut commands = cmds.iter();
					for pid in &pids {
						let cmd = commands.next().map(|cmd| cmd.as_str());
						let child = ChildProc::new(*pid,cmd,pgid)?;
						children.push(child);
					}
					let job = JobBuilder::new()
						.with_pgid(pgid.unwrap())
						.with_children(children)
						.build();

					helper::handle_fg(lash,job)?;
				}
			}
			Err(e) => return Err(High(LashErrHigh::exec_err("Command in pipeline failed", blame)))
		}
		if first {
			first = false;
		}
	}
	lash.set_code(0);
	Ok(())
}
