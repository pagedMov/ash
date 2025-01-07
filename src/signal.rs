use std::{os::fd::BorrowedFd, sync::mpsc::{self, SendError, Sender}, thread};

use nix::{sys::{signal::{signal, sigprocmask, SaFlags, SigAction, SigHandler, SigSet, SigmaskHow, Signal::SIGTTOU }, wait::{waitpid, WaitPidFlag, WaitStatus}}, unistd::{getpgid, getpgrp, getpid, tcgetpgrp, tcsetpgrp, Pid}};
use tokio::signal::unix::{Signal, SignalKind};

use signal_hook::consts::signal::*;
use signal_hook::iterator::Signals;

use crate::{event::{self, ShError, ShEvent}, execute::RshWait, shellenv::{read_jobs, read_meta, write_jobs, write_meta}, RshResult};

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

            // Send the event to the outbox
            if let Err(err) = event::global_send(event) {
                eprintln!("Failed to send signal event: {:?}", err);
                break;
            }
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

fn extract_pid(status: &WaitStatus) -> Option<Pid> {
    match status {
        WaitStatus::Exited(pid, _)
        | WaitStatus::Signaled(pid, _, _)
        | WaitStatus::Stopped(pid, _)
        | WaitStatus::Continued(pid) => Some(*pid),
        WaitStatus::StillAlive => None,
				_ => unimplemented!()
    }
}

pub fn handle_signal(sig: ShEvent) -> RshResult<()> {
	let sig_ignore = SigAction::new(SigHandler::SigIgn, SaFlags::empty(), SigSet::empty());
	unsafe { nix::sys::signal::sigaction(SIGTTOU, &sig_ignore).unwrap() };
	match sig {
		ShEvent::Signal(SIGINT) => { println!("sigint") },
		ShEvent::Signal(SIGCHLD) => {
			let num_children = read_meta(|m| m.children())?;
			if num_children == 0 {
				// This pid has already been waited on
				event::global_send(ShEvent::Prompt)?;
				return Ok(());
			}
			write_meta(|m| m.reap_child())?;

			match waitpid(None, Some(WaitPidFlag::WNOHANG)) {
				Ok(status) => {
					let child = extract_pid(&status);
					if child.is_none() { return Ok(()) };
					let child = child.unwrap();

					let child_pgrp = getpgid(Some(child)).unwrap_or(Pid::from_raw(0));
					let job: i32 = read_jobs(|j| j.borrow_jobs()
						.clone()
						.values()
						.find(|job| *job.pgid() == child_pgrp)
						.map_or(0, |job| job.id()))?;

					match status {
						WaitStatus::Exited(_, status) => {
							let wait = if status == 0 {
								event::global_send(ShEvent::LastStatus(RshWait::Success))?;
								RshWait::Success
							} else {
								event::global_send(ShEvent::LastStatus(RshWait::Fail { code: status, cmd: None }))?;
								RshWait::Fail { code: status, cmd: None }
							};
							if job == 0 { // Foreground task just finished
								// Reset terminal control group if the shell is not controlling the terminal
								if !unsafe { tcgetpgrp(BorrowedFd::borrow_raw(0)).unwrap_or(Pid::from_raw(0)) == Pid::this() } {
									unsafe { tcsetpgrp(BorrowedFd::borrow_raw(0), getpid()) }.unwrap();
								}
							} else {
								write_jobs(|j| {
									j.get_job(job).map(|job| {
											job.update_from_pid(child, wait);
											Some(())
									})
								})?;
							}
							event::global_send(ShEvent::Prompt)?;
							return Ok(())
						}
						WaitStatus::Signaled(_, signal, _) => {
							println!("child was signaled: {:?}", signal)
						}
						WaitStatus::Stopped(_, signal) => {
							println!("child was stopped: {:?}", signal)
						}
						WaitStatus::Continued(pid) => todo!(),
						WaitStatus::StillAlive => { /* Do nothing */ }
						_ => unreachable!()
					}
					write_meta(|m| m.reap_child())?;
				}
				Err(_) => {
					if !unsafe { tcgetpgrp(BorrowedFd::borrow_raw(0)).unwrap_or(Pid::from_raw(0)) == Pid::this() } {
						unsafe { tcsetpgrp(BorrowedFd::borrow_raw(0), getpid()) }.unwrap();
					}
					event::global_send(ShEvent::Prompt)?
				}
			}
		}
		_ => unimplemented!()
	}
	Ok(())
}
