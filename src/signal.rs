use std::{os::fd::BorrowedFd, sync::mpsc::{self, SendError, Sender}, thread};

use nix::{sys::{signal::{sigprocmask, SigSet, SigmaskHow, }, wait::{waitpid, WaitPidFlag, WaitStatus}}, unistd::{getpgid, getpgrp, getpid, tcgetpgrp, tcsetpgrp, Pid}};
use tokio::signal::unix::{Signal, SignalKind};

use signal_hook::consts::signal::*;
use signal_hook::iterator::Signals;

use crate::{event::{ShError, ShEvent}, execute::RshWait, shellenv::{read_jobs, write_jobs}, RshResult};

pub struct SignalListener {
	outbox: mpsc::Sender<ShEvent>,
	sigint: Signal,
	sigio: Signal,
	sigpipe: Signal,
	sigtstp: Signal,
	sigquit: Signal,
	sigterm: Signal,
	sigchild: Signal,
	sighup: Signal,
	sigwinch: Signal,
	sigusr1: Signal,
	sigusr2: Signal,
}

impl SignalListener {
	pub fn new(outbox: mpsc::Sender<ShEvent>) -> Self {
		Self {
			// Signal listeners
			// TODO: figure out what to do instead of unwrapping
			outbox,
			sigint: tokio::signal::unix::signal(SignalKind::interrupt()).unwrap(),
			sigio: tokio::signal::unix::signal(SignalKind::io()).unwrap(),
			sigpipe: tokio::signal::unix::signal(SignalKind::pipe()).unwrap(),
			sigtstp: tokio::signal::unix::signal(SignalKind::from_raw(20)).unwrap(),
			sigquit: tokio::signal::unix::signal(SignalKind::quit()).unwrap(),
			sigterm: tokio::signal::unix::signal(SignalKind::terminate()).unwrap(),
			sigchild: tokio::signal::unix::signal(SignalKind::child()).unwrap(),
			sighup: tokio::signal::unix::signal(SignalKind::hangup()).unwrap(),
			sigwinch: tokio::signal::unix::signal(SignalKind::window_change()).unwrap(),
			sigusr1: tokio::signal::unix::signal(SignalKind::user_defined1()).unwrap(),
			sigusr2: tokio::signal::unix::signal(SignalKind::user_defined2()).unwrap(),
		}
	}

pub fn signal_listen(&self) -> std::io::Result<()> {
    // Define signals to listen for
    let mut signals = Signals::new([
        SIGINT, SIGIO, SIGPIPE, SIGTSTP, SIGQUIT, SIGTERM, SIGCHLD, SIGHUP, SIGWINCH, SIGUSR1, SIGUSR2,
    ])?;
		let outbox = self.outbox.clone();

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
            if let Err(err) = outbox.send(event) {
                eprintln!("Failed to send signal event: {}", err);
                break;
            }
        }
    });

    Ok(())
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

pub fn handle_signal(sig: ShEvent, tx: Sender<ShEvent>) -> RshResult<()> {
	match sig {
		ShEvent::Signal(SIGINT) => { println!("sigint") },
		ShEvent::Signal(SIGCHLD) => {
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
							tx.send(ShEvent::LastStatus(status)).map_err(|_| ShError::from_internal("Failed to send status to event loop"))?;
							let wait = if status == 0 {
								RshWait::Success
							} else {
								RshWait::Fail { code: status, cmd: None }
							};
							if job == 0 { // Foreground task just finished
								unsafe { tcsetpgrp(BorrowedFd::borrow_raw(0), getpid()) }.unwrap();
								tx.send(ShEvent::Prompt).map_err(|_| ShError::from_internal("Failed to send prompt to event loop"))?;
							} else {
								write_jobs(|j| {
									j.get_job(job).map(|job| {
											job.update_from_pid(child, wait);
											Some(())
									})
								})?;
							}
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
					println!("processed sigchld");
				}
				Err(e) => {
					return Err(ShError::from_internal("Waitpid failed"))
				}
			}
		}
		_ => unimplemented!()
	}
	Ok(())
}
