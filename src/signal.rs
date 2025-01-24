use nix::{sys::{signal::{killpg, signal, SigHandler, Signal} , wait::{waitpid, WaitPidFlag, WaitStatus}}, unistd::{getpgid, Pid}};

use crate::{event::ShError, interp::helper, shellenv::{self, read_jobs, write_jobs, JobCmdFlags, JobID, RSH_PGRP}, OxResult};


pub fn sig_handler_setup() {
	unsafe {
		signal(Signal::SIGCHLD, SigHandler::Handler(handle_sigchld)).unwrap();
		signal(Signal::SIGQUIT, SigHandler::Handler(handle_sigquit)).unwrap();
		signal(Signal::SIGTSTP, SigHandler::Handler(handle_sigtstp)).unwrap();
		signal(Signal::SIGHUP, SigHandler::Handler(handle_sighup)).unwrap();
		signal(Signal::SIGINT, SigHandler::Handler(handle_sigint)).unwrap();
		signal(Signal::SIGTTIN, SigHandler::SigIgn).unwrap();
		signal(Signal::SIGTTOU, SigHandler::SigIgn).unwrap();
	}
}

extern "C" fn handle_sighup(_: libc::c_int) {
	write_jobs(|j| {
		for job in j.mut_jobs().iter_mut().flatten() {
			job.killpg(Signal::SIGTERM).unwrap();
		}
	}).unwrap();
	std::process::exit(0);
}

extern "C" fn handle_sigtstp(_: libc::c_int) {
	write_jobs(|j| {
		if let Some(job) = j.get_fg_mut() {
			job.killpg(Signal::SIGTSTP).unwrap();
		}
	}).unwrap();
}

extern "C" fn handle_sigint(_: libc::c_int) {
	write_jobs(|j| {
		if let Some(job) = j.get_fg_mut() {
			job.killpg(Signal::SIGINT).unwrap();
		}
	}).unwrap();
}

pub extern "C" fn ignore_sigchld(_: libc::c_int) {
	// Do nothing
	// This function exists because using SIGIGN to ignore SIGCHLD
	// will cause the kernel to reap the child process implicitly
	// which will prevents the code from being able to reap it elsewhere
	// which is the entire point of shellenv::disable_reaping()
}

extern "C" fn handle_sigquit(_: libc::c_int) {
	write_jobs(|j| {
		for job in j.mut_jobs().iter_mut().flatten() {
			job.killpg(Signal::SIGTERM).unwrap();
		}
	}).unwrap();
	std::process::exit(0);
}

pub extern "C" fn handle_sigchld(_: libc::c_int) {
	/*
	 * This is the signal handler's real job
	 * Each WaitStatus has logic associated with it
	 * But handle_child_exit() is the most important one
	 */
	let flags = WaitPidFlag::WNOHANG | WaitPidFlag::WUNTRACED;
	while let Ok(status) = waitpid(None, Some(flags)) {
		let _ = match status {
			WaitStatus::Exited(pid, _code) => handle_child_exit(pid, status),
			WaitStatus::Signaled(pid, signal, _) => handle_child_signal(pid, signal),
			WaitStatus::Stopped(pid, signal) => handle_child_stop(pid, signal),
			WaitStatus::Continued(pid) => handle_child_continue(pid),
			WaitStatus::StillAlive => break, // No more processes to reap
			_ => unreachable!(),
		};
	}
}

//TODO: extract some of this logic from the closure to spend less time holding a write lock
pub fn handle_child_signal(pid: Pid, sig: Signal) -> OxResult<()> {
	let pgid = getpgid(Some(pid)).unwrap_or(pid);
	write_jobs(|j| {
		if let Some(job) = j.query_mut(JobID::Pgid(pgid)) {
			let child = job.get_children_mut().iter_mut().find(|chld| pid == chld.pid()).unwrap();
			let status = WaitStatus::Signaled(pid, sig, false);
			helper::set_last_status(&status).unwrap();
			child.set_status(status);
		}
	})?;
	if matches!(sig,Signal::SIGINT) {
		shellenv::attach_tty(*RSH_PGRP)?; // Reclaim terminal
	}
	Ok(())
}

pub fn handle_child_stop(pid: Pid, signal: Signal) -> OxResult<()> {
	let pgid = getpgid(Some(pid)).unwrap_or(pid);
	write_jobs(|j| {
		if let Some(job) = j.query_mut(JobID::Pgid(pgid)) {
			let child = job.get_children_mut().iter_mut().find(|chld| pid == chld.pid()).unwrap();
			let status = WaitStatus::Stopped(pid, signal);
			helper::set_last_status(&status).unwrap();
			child.set_status(status);
		} else if j.get_fg_mut().is_some_and(|fg| fg.pgid() == pgid) {
			j.fg_to_bg(WaitStatus::Stopped(pid, signal)).unwrap();
		}
	})?;
	let job = read_jobs(|j| j.query(JobID::Pid(pid)).cloned())?;
	shellenv::attach_tty(*RSH_PGRP)?; // Reclaim terminal
	Ok(())
}

pub fn handle_child_exit(pid: Pid, status: WaitStatus) -> OxResult<()> {
	/*
	 * Here we are going to get metadata on the exited process by querying the job table with the pid.
	 * Then if the discovered job is the fg task, return terminal control to rsh
	 * If it is not the fg task, print the display info for the job in the job table
	 * We can reasonably assume that if it is not a foreground job, then it exists in the job table
	 * If this assumption is incorrect, the code has gone wrong somewhere.
	 */
	let (
		pgid,
		is_fg,
		is_finished
	) = write_jobs(|j| {
		let fg_pgid = j.get_fg().map(|job| job.pgid());
		if let Some(job) = j.query_mut(JobID::Pid(pid)) {
			let pgid = job.pgid();
			let is_fg = fg_pgid.is_some_and(|fg| fg == pgid);
			job.update_by_id(JobID::Pid(pid), status).unwrap();
			let is_finished = !job.is_alive();

			if let Some(child) = job.get_children_mut().iter_mut().find(|chld| pid == chld.pid()) {
				helper::set_last_status(&status).unwrap();
				child.set_status(status);
			}

			Ok((pgid, is_fg, is_finished))
		} else {
			Err(ShError::from_internal("Job not found"))
		}
	})??;

	if is_finished {
		if is_fg {
			shellenv::attach_tty(*RSH_PGRP)?; // Reclaim terminal control
		} else {
			println!();
			let job_order = read_jobs(|j| j.job_order().to_vec())?;
			let result = read_jobs(|j| j.query(JobID::Pgid(pgid)).cloned())?;
			if let Some(job) = result {
				println!("{}",job.display(&job_order,JobCmdFlags::PIDS))
			}
		}
	}
	Ok(())
}

pub fn handle_child_continue(pid: Pid) -> OxResult<()> {
	let pgid = getpgid(Some(pid)).unwrap_or(pid);
	write_jobs(|j| {
		if let Some(job) = j.query_mut(JobID::Pgid(pgid)) {
			job.killpg(Signal::SIGCONT).unwrap();
		}
	})?;
	Ok(())
}
