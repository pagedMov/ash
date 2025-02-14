use crate::prelude::*;

use crate::{helper::{self}, shellenv::{read_jobs, Slash}, SlashResult};

pub fn execute<'a>(fg_call: Pair<'a,Rule>, slash: &mut Slash) -> SlashResult<()> {
	let mut argv = helper::prepare_argv(fg_call,slash)?;
	argv.pop_front();
	let jobs = read_jobs(|j| j.clone())?;

	let curr_job_id = jobs.curr_job();

	Ok(())
}
