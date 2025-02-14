use std::{os::fd::AsRawFd, path::PathBuf};

use clap::{ArgAction, Parser as ClapParser};
use error::{SlashErr, SlashErrExt, SlashErrLow, SlashResult};
use execute::dispatch;
use nix::{sys::termios::{self, LocalFlags, Termios}, unistd::isatty};
use shellenv::Slash;

pub mod prompt;
pub mod execute;
pub mod error;
pub mod shellenv;
pub mod shopt;
pub mod helper;
pub mod signal;
pub mod expand;
pub mod builtin;
pub mod prelude;
pub mod utils;
pub mod script;
pub mod pest_ext;


#[derive(Debug,ClapParser)]
#[command(name = "slash")]
#[command(version = "v0.5.0-alpha")]
#[command(about = "A linux shell written in Rust")]
#[command(author = "Kyler Clay <kylerclay@proton.me>")]
struct SlashArgs {
	script: Option<PathBuf>,

	#[arg(long = "no-rc", action = ArgAction::SetTrue, help = "Run without executing .slashrc")]
	no_rc: bool,

	#[arg(long = "rc-path", value_name = "FILE", help = "Set a custom path to .slashrc")]
	rc_path: Option<PathBuf>,

	#[arg(long = "no-history", action = ArgAction::SetTrue, help = "Run without loading .slash_hist" )]
	no_hist: bool,

	#[arg(long = "history-path", value_name = "FILE", help = "Set a custom path to .slash_hist")]
	hist_path: Option<PathBuf>,

	#[arg(short = 'c', value_name = "COMMAND", help = "Run a single command and then exit")]
	command: Option<String>
}

fn set_termios() -> Option<Termios> {
	if isatty(std::io::stdin().as_raw_fd()).unwrap() {
		let mut termios = termios::tcgetattr(std::io::stdin()).unwrap();
		termios.local_flags &= !LocalFlags::ECHOCTL;
		termios::tcsetattr(std::io::stdin(), nix::sys::termios::SetArg::TCSANOW, &termios).unwrap();
		Some(termios)
	} else {
		None
	}
}

fn restore_termios(orig: &Option<Termios>) {
	if let Some(termios) = orig {
		let fd = std::io::stdin();
		termios::tcsetattr(fd, termios::SetArg::TCSANOW, termios).unwrap();
	}
}

fn main() {

	let mut slash = Slash::new(); // The shell environment

	let args = SlashArgs::parse();
	if args.no_rc {
		slash.vars_mut().export_var("PS1", "$> ");
	}

	if !args.no_rc {
		slash.source_rc(args.rc_path).catch();
	}

	let termios = set_termios();
	loop {
		let input = prompt::prompt::run_prompt(&mut slash).catch().unwrap_or_default();

		slash.start_timer();
		slash.ctx_mut().push_state().catch();
		let saved_fds = utils::save_fds().unwrap();

		let result = dispatch::exec_input(input, &mut slash);

		utils::restore_fds(saved_fds,&mut slash).catch();
		slash.ctx_mut().pop_state().catch();

		match result {
			Ok(_) => continue,
			Err(e) => {
				match e {
					SlashErr::Low(SlashErrLow::CleanExit(code)) => {
						restore_termios(&termios);
						std::process::exit(code)
					}
					SlashErr::High(ref high) => {
						if let SlashErrLow::CleanExit(code) = high.get_err() {
							restore_termios(&termios);
							std::process::exit(*code)
						} else {
							eprintln!("{}",e)
						}
					}
					_ => eprintln!("{}",e)
				}
			}
		}
	}
}
