use nix::{sys::signal::{kill, Signal}, unistd::Pid};
use rustyline::{error::ReadlineError, Editor};

use crate::error::{LashErr, LashErrHigh, LashErrLow};

pub fn run_prompt() -> String {
	let mut rl = rustyline::DefaultEditor::new().unwrap();
	match rl.readline("$> ") {
		Ok(line) => {
			line
		}
		Err(ReadlineError::Interrupted) => {
			String::new()
		}
		Err(ReadlineError::Eof) => {
			kill(Pid::this(), Signal::SIGQUIT).unwrap();
			String::new()
		}
		Err(e) => {
			panic!()
		}
	}
}
