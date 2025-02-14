use crate::{helper, prelude::*, shellenv::{read_jobs, write_jobs, JobCmdFlags, JobID}, utils};

pub fn continue_job<'a>(fg_call: Pair<'a,Rule>,slash: &mut Slash, fg: bool) -> SlashResult<()> {
	let mut stdout = utils::SmartFD::new(1)?;
	let mut argv = helper::prepare_argv(fg_call.clone(), slash)?;
	let blame = fg_call.clone();
	let redirs = helper::prepare_redirs(fg_call)?;
	argv.pop_front();
	slash.consume_redirs(redirs)?;

	if read_jobs(|j| j.get_fg().is_some())? {
		return Err(High(SlashErrHigh::exec_err("Somehow called fg when there is already a foreground process", blame)))
	}

	let curr_job_id = if let Some(id) = read_jobs(|j| j.curr_job())? {
		id
	} else {
		return Err(High(SlashErrHigh::exec_err("Did not find a job to move to the foreground", blame)))
	};

	let job_id = match argv.pop_front() {
		Some(arg) => parse_job_id(&arg, blame.clone())?,
		None => curr_job_id
	};

	let mut job = write_jobs(|j| {
		let id = JobID::TableID(job_id);
		let query_result = j.query(id.clone());
		if query_result.is_some() {
			Ok(j.remove_job(id).unwrap())
		} else {
			Err(High(SlashErrHigh::exec_err(format!("Job ID `{}' not found", job_id), blame)))
		}
	})??;

	job.killpg(Signal::SIGCONT)?;

	if fg {
		helper::handle_fg(slash, job)?;
	} else {
		let job_order = read_jobs(|j| j.job_order().to_vec())?;
		writeln!(stdout, "{}", job.display(&job_order, JobCmdFlags::PIDS))?;

		write_jobs(|j| j.insert_job(job, true))??;
	}

	Ok(())
}

pub fn jobs<'a>(jobs_call: Pair<'a,Rule>,slash: &mut Slash) -> SlashResult<()> {
	let mut argv = helper::prepare_argv(jobs_call.clone(), slash)?;
	let mut redirs = helper::prepare_redirs(jobs_call.clone())?;
	let mut stdout = utils::SmartFD::new(1)?;
	slash.consume_redirs(redirs)?;
	let blame = jobs_call;
	argv.pop_front();

	let mut flags = JobCmdFlags::empty();
	while let Some(arg) = argv.pop_front() {
		let mut chars = arg.chars().peekable();
		if chars.peek().is_none_or(|ch| *ch != '-') {
			return Err(High(SlashErrHigh::syntax_err(format!("Invalid flag in `jobs' call: {}",arg), blame)))
		}

		chars.next(); // Ignore the hyphen
		while let Some(ch) = chars.next() {
			let flag = match ch {
				'l' => JobCmdFlags::LONG,
				'p' => JobCmdFlags::PIDS,
				'n' => JobCmdFlags::NEW_ONLY,
				'r' => JobCmdFlags::RUNNING,
				's' => JobCmdFlags::STOPPED,
				_ => return Err(High(SlashErrHigh::syntax_err("Invalid flag in `jobs` invocation", blame)))
			};
			flags |= flag;
		}
	}

	read_jobs(|j| j.print_jobs(&flags, stdout))??;

	Ok(())
}

fn parse_job_id<'a>(arg: &str, blame: Pair<'a,Rule>) -> SlashResult<usize> {
	if arg.starts_with('%') {
		let arg = arg.strip_prefix('%').unwrap();
		if arg.chars().all(|ch| ch.is_ascii_digit()) {
			Ok(arg.parse::<usize>().unwrap())
		} else {
			let result = write_jobs(|j| {
				let query_result = j.query(JobID::Command(arg.into()));
				query_result.map(|job| job.table_id().unwrap())
			})?;
			match result {
				Some(id) => Ok(id),
				None => Err(High(SlashErrHigh::internal_err("Found a job but no table id in parse_job_id()", blame)))
			}
		}
	} else if arg.chars().all(|ch| ch.is_ascii_digit()) {
		let result = write_jobs(|j| {
			let pgid_query_result = j.query(JobID::Pgid(Pid::from_raw(arg.parse::<i32>().unwrap())));
			if let Some(job) = pgid_query_result {
				return Some(job.table_id().unwrap())
			}

			if arg.parse::<i32>().unwrap() > 0 {
				let table_id_query_result = j.query(JobID::TableID(arg.parse::<usize>().unwrap()));
				return table_id_query_result.map(|job| job.table_id().unwrap());
			}

			None
		})?;

		match result {
			Some(id) => Ok(id),
			None => Err(High(SlashErrHigh::internal_err("Found a job but no table id in parse_job_id()", blame)))
		}
	} else {
		Err(High(SlashErrHigh::syntax_err(format!("Invalid fg argument: {}",arg), blame)))
	}
}
