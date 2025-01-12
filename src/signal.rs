use nix::{sys::{signal::{killpg, signal, SigHandler, Signal} , wait::{waitpid, WaitPidFlag, WaitStatus}}, unistd::{getpgid, tcgetpgrp, Pid}};

use crate::{execute::RshWait, shellenv::{self, read_jobs, write_jobs, JobFlags, RSH_PGRP}, RshResult};

pub fn sig_handler_setup() {
	unsafe {
		signal(Signal::SIGCHLD, SigHandler::Handler(handle_sigchld)).unwrap();
		signal(Signal::SIGQUIT, SigHandler::Handler(handle_sigquit)).unwrap();
		signal(Signal::SIGTSTP, SigHandler::Handler(handle_sigtstp)).unwrap();
		signal(Signal::SIGHUP, SigHandler::Handler(handle_sigtstp)).unwrap();
		signal(Signal::SIGTTIN, SigHandler::SigIgn).unwrap();
		signal(Signal::SIGTTOU, SigHandler::SigIgn).unwrap();
	}
}

extern "C" fn handle_sighup(_: libc::c_int) {
	write_jobs(|j| j.kill_all()).unwrap();
	std::process::exit(0);
}

extern "C" fn handle_sigtstp(_: libc::c_int) {
	let fg_pgrp = shellenv::term_controller();
	let _ = killpg(fg_pgrp, Signal::SIGTSTP);
}

extern "C" fn handle_sigint(_: libc::c_int) {
	// Send SIGINT to the foreground process group
	let fg_pgrp = shellenv::term_controller();
	let _ = killpg(fg_pgrp, Signal::SIGINT);
}

extern "C" fn handle_sigquit(_: libc::c_int) {
	std::process::exit(0);
}

extern "C" fn handle_sigchld(_: libc::c_int) {
	let flags = WaitPidFlag::WUNTRACED;
	while let Ok(status) = waitpid(None, Some(flags)) {
		match status {
			WaitStatus::Exited(pid, _code) => handle_child_exit(pid, status),
			WaitStatus::Signaled(pid, signal, _) => handle_child_signal(pid, signal),
			WaitStatus::Stopped(pid, signal) => handle_child_stop(pid, signal),
			WaitStatus::Continued(pid) => handle_child_continue(pid),
			WaitStatus::StillAlive => break, // No more processes to reap
			_ => unreachable!(),
		};
	}
}

fn handle_child_signal(pid: Pid, sig: Signal) -> RshResult<()> {
	let pgid = getpgid(Some(pid)).unwrap_or(pid);
	write_jobs(|j| {
		if let Some(job) = j.mut_by_pgid(pgid) {
			job.update_status(Some(pid), RshWait::Signaled { sig })
		}
	})?;
	if matches!(sig,Signal::SIGINT) {
		shellenv::attach_tty(*RSH_PGRP)?; // Reclaim terminal
	}
	Ok(())
}

fn handle_child_stop(pid: Pid, signal: Signal) -> RshResult<()> {
	let pgid = getpgid(Some(pid)).unwrap_or(pid);
	write_jobs(|j| {
		if let Some(job) = j.mut_by_pgid(pgid) {
			job.stop(signal);
			killpg(pgid, signal).unwrap();
			if j.is_fg(pgid) {
				j.to_background();
			}
		}
	})?;
	shellenv::attach_tty(*RSH_PGRP)?; // Reclaim terminal
	Ok(())
}

fn handle_child_exit(pid: Pid, status: WaitStatus) -> RshResult<()> {
	let job = read_jobs(|j| j.get_by_pid(pid).cloned())?;
	if job.is_none() {
		shellenv::notify_job_done(pid)?;
		shellenv::attach_tty(*RSH_PGRP)?; // Reclaim terminal control
		return Ok(())
	}
	let pgid = *job.unwrap().pgid();
	write_jobs(|j| {
		if let Some(job) = j.mut_by_pgid(pgid) {
			job.update_status(Some(pid), RshWait::from_wait(status, None));
		}
	})?;
	if read_jobs(|j| j.is_finished(pgid))? {
		shellenv::notify_job_done(pgid)?;
		if read_jobs(|j| j.get_by_pgid(pgid).is_some())? {
			shellenv::attach_tty(*RSH_PGRP)?; // Reclaim terminal control
		} else {
			let job = read_jobs(|j| j.get_by_pgid(pgid).cloned().unwrap())?;
			if job.id() == 0 {
				return Ok(())
			}
			println!();
			let job_order = read_jobs(|j| j.job_order().to_vec())?;
			println!("{}",job.display(&job_order,JobFlags::PIDS))
		}
	} else if read_jobs(|j| j.get_by_pgid(pgid).is_none())? {
		shellenv::attach_tty(*RSH_PGRP)?; // Reclaim terminal control
	}
	Ok(())
}

fn handle_child_continue(pid: Pid) -> RshResult<()> {
	let pgid = getpgid(Some(pid)).unwrap_or(pid);
	write_jobs(|j| {
		if let Some(job) = j.mut_by_pgid(pgid) {
			job.cont().unwrap();
		}
	})?;
	Ok(())
}
