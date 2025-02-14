use crate::{execute, helper, prelude::*, utils};


pub fn run_exec<'a>(exec_call: Pair<'a,Rule>, slash: &mut Slash) -> SlashResult<()> {
	let mut inner = exec_call.clone().into_inner();
	let exec = inner.next().unwrap().as_str();
	if let Some(pair) = inner.next() {
		match pair.as_rule() {
			Rule::word => {
				// Exec a command
				let new_input = &exec_call.as_str()[exec.len()..].trim(); // slice off 'exec'
				*slash.ctx_mut().flags_mut() |= utils::ExecFlags::NO_FORK; // we ain't comin back
				execute::dispatch::exec_input(new_input.to_string(), slash).blame(exec_call)?;
			}
			Rule::redir => {
				// Mess with file descriptors
			}
			_ => unreachable!()
		}
	}


	Ok(())
}
