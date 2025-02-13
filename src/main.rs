use std::path::PathBuf;

use clap::{ArgAction, Parser as ClapParser};
use error::{LashErr, LashErrExt, LashErrLow, LashResult};
use execute::dispatch;
use shellenv::Lash;

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
#[command(name = "lash")]
#[command(version = "v0.5.0-alpha")]
#[command(about = "A linux shell written in Rust")]
#[command(author = "Kyler Clay <kylerclay@proton.me>")]
struct LashArgs {
	script: Option<PathBuf>,

	#[arg(long = "no-rc", action = ArgAction::SetTrue, help = "Run without executing .lashrc")]
	no_rc: bool,

	#[arg(long = "rc-path", value_name = "FILE", help = "Set a custom path to .lashrc")]
	rc_path: Option<PathBuf>,

	#[arg(long = "no-history", action = ArgAction::SetTrue, help = "Run without loading .lash_hist" )]
	no_hist: bool,

	#[arg(long = "history-path", value_name = "FILE", help = "Set a custom path to .lash_hist")]
	hist_path: Option<PathBuf>,

	#[arg(short = 'c', value_name = "COMMAND", help = "Run a single command and then exit")]
	command: Option<String>
}

fn main() {

	let mut lash = Lash::new(); // The shell environment

	let args = LashArgs::parse();
	if args.no_rc {
		lash.vars_mut().export_var("PS1", "$> ");
	}

	if !args.no_rc {
		lash.source_rc(args.rc_path).catch();
	}

	loop {
		let input = prompt::prompt::run_prompt(&mut lash).catch().unwrap_or_default();

		lash.start_timer();
		lash.ctx_mut().push_state().catch();
		let saved_fds = utils::save_fds().unwrap();

		let result = dispatch::exec_input(input, &mut lash);

		utils::restore_fds(saved_fds).catch();
		lash.ctx_mut().pop_state().catch();

		match result {
			Ok(_) => continue,
			Err(e) => {
				match e {
					LashErr::Low(LashErrLow::CleanExit(code)) => {
						std::process::exit(code)
					}
					LashErr::High(ref high) => {
						if let LashErrLow::CleanExit(code) = high.get_err() {
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
