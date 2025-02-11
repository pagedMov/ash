use crate::prelude::*;

use crate::{helper::{self}, shellenv::{read_jobs, Lash}, LashResult};

pub fn execute<'a>(fg_call: Pair<'a,Rule>, lash: &mut Lash) -> LashResult<()> {
	let mut argv = helper::prepare_argv(fg_call,lash)?;
	argv.pop_front();
	let jobs = read_jobs(|j| j.clone())?;

	let curr_job_id = jobs.curr_job();

	Ok(())
}
