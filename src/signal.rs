use std::thread;

use nix::{sys::{signal::{killpg, Signal} , wait::{waitpid, WaitPidFlag, WaitStatus}}, unistd::{getpgid, Pid}};

use signal_hook::consts::signal::*;
use signal_hook::iterator::Signals;

use crate::{event::{ShError, ShEvent}, execute::RshWait, shellenv::{self, read_jobs, write_jobs, JobFlags, RSH_PGRP}, RshResult};

pub struct SignalListener { }

impl SignalListener {
	pub fn new() -> Self {
		Self { }
	}

	pub fn signal_listen(&self) -> std::io::Result<()> {
		// Define signals to listen for
		let mut signals = Signals::new([
			SIGINT, SIGIO, SIGPIPE, SIGTSTP, SIGQUIT, SIGTERM, SIGCHLD, SIGHUP, SIGWINCH, SIGUSR1, SIGUSR2,
		])?;

		// Spawn a thread to listen for signals
		thread::spawn(move || {
			for signal in signals.forever() {
				let event = match signal {
					SIGINT => ShEvent::Signal(SIGINT),
					SIGIO => ShEvent::Signal(SIGIO),
					SIGPIPE => ShEvent::Signal(SIGPIPE),
					SIGTSTP => ShEvent::Signal(SIGTSTP),
					SIGQUIT => ShEvent::Signal(SIGQUIT),
					SIGTERM => ShEvent::Signal(SIGTERM),
					SIGCHLD => ShEvent::Signal(SIGCHLD),
					SIGHUP => ShEvent::Signal(SIGHUP),
					SIGWINCH => ShEvent::Signal(SIGWINCH),
					SIGUSR1 => ShEvent::Signal(SIGUSR1),
					SIGUSR2 => ShEvent::Signal(SIGUSR2),
					_ => continue, // Ignore unhandled signals
				};

				handle_signal(event);
			}
		});

		Ok(())
	}
}

impl Default for SignalListener {
	fn default() -> Self {
		Self::new()
	}
}

pub fn handle_signal(sig: ShEvent) -> RshResult<()> {
	match sig {
		ShEvent::Signal(SIGCHLD) => handle_sigchld(),
		ShEvent::Signal(SIGTSTP) | ShEvent::Signal(SIGINT) => Ok(()), // Ignore at the prompt
		_ => unimplemented!(),
	}
}

fn handle_sigchld() -> RshResult<()> {
	let flags = WaitPidFlag::WUNTRACED;
	while let Ok(status) = waitpid(None, Some(flags)) {
		match status {
			WaitStatus::Exited(pid, _code) => handle_child_exit(pid, status)?,
			WaitStatus::Signaled(pid, signal, _) => handle_child_signal(pid, signal)?,
			WaitStatus::Stopped(pid, signal) => handle_child_stop(pid, signal)?,
			WaitStatus::Continued(pid) => handle_child_continue(pid)?,
			WaitStatus::StillAlive => break, // No more processes to reap
			_ => unreachable!(),
		}
	}
	Ok(())
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
		shellenv::try_prompt()?;
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
	shellenv::try_prompt()?;
	Ok(())
}

fn handle_child_exit(pid: Pid, status: WaitStatus) -> RshResult<()> {
	let job = read_jobs(|j| j.get_by_pid(pid).cloned())?;
	if job.is_none() {
		return Err(ShError::from_internal(format!("Failed to find a job containing this pid: {}",pid).as_str()))
	}
	let pgid = *job.unwrap().pgid();
	write_jobs(|j| {
		if let Some(job) = j.mut_by_pgid(pgid) {
			job.update_status(Some(pid), RshWait::from_wait(status, None));
		}
	})?;
	if read_jobs(|j| j.is_finished(pgid))? {
		if read_jobs(|j| j.get_by_pgid(pgid).is_none_or(|job| job.is_foreground()))? {
			shellenv::attach_tty(*RSH_PGRP)?; // Reclaim terminal control
			shellenv::try_prompt()?;
		} else {
			let job = read_jobs(|j| j.get_by_pgid(pgid).cloned().unwrap())?;
			if job.id() == 0 {
				return Ok(())
			}
			println!();
			let job_order = read_jobs(|j| j.job_order().to_vec())?;
			println!("{}",job.display(&job_order,JobFlags::PIDS))
		}
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
