use std::{env, path::Path};

use nix::{sys::signal::{kill, Signal}, unistd::Pid};
use rustyline::{completion::FilenameCompleter, error::ReadlineError, history::History, Helper};

use crate::prelude::*;
use crate::{error::{LashErr::*, LashErrLow}, expand, shellenv::Lash, LashResult};

use super::rl_init;

#[derive(Helper)]
pub struct LashHelper<'a> {
	pub filename_comp: FilenameCompleter,
	pub lash: &'a mut Lash,
	pub commands: Vec<String>
}

impl<'a> LashHelper<'a> {
	pub fn new(lash: &'a mut Lash) -> Self {
		// Prepopulate some built-in commands (could also load dynamically)
		let commands = vec![
			"cd".to_string(),
			"ls".to_string(),
			"echo".to_string(),
			"exit".to_string(),
		];

		let mut helper = LashHelper {
			filename_comp: FilenameCompleter::new(),
			lash,
			commands,
		};
		helper.update_commands_from_path();
		helper
	}

	pub fn hist_substr_search(&self, term: &str, hist: &dyn History) -> Option<String> {
		let limit = hist.len();
		let mut latest_match = None;
		for i in 0..limit {
			if let Some(hist_entry) = hist.get(i, rustyline::history::SearchDirection::Reverse).ok()? {
				if hist_entry.entry.starts_with(term) {
					latest_match = Some(hist_entry.entry.into_owned());
				}
			}
		}
		latest_match
	}

	// Dynamically add commands (if needed, e.g., external binaries in $PATH)
	pub fn update_commands_from_path(&mut self) {
		if let Ok(paths) = env::var("PATH") {
			let mut external_commands = HashSet::new();
			for path in env::split_paths(&paths) {
				if let Ok(entries) = std::fs::read_dir(path) {
					for entry in entries.flatten() {
						if let Ok(file_name) = entry.file_name().into_string() {
							external_commands.insert(file_name);
						}
					}
				}
			}
			self.commands.extend(external_commands);
		}
	}
}

pub fn run_prompt(lash: &mut Lash) -> LashResult<String> {
	lash.meta_mut().enter_prompt();

	let hist_path = lash.borrow_vars().get_evar("HIST_FILE").unwrap_or_else(|| -> String {
		let home = lash.borrow_vars().get_evar("HOME").unwrap_or_default();
		format!("{}/.lash_hist",home)
	});
	let prompt = match expand::misc::expand_prompt(None,lash) {
		Ok(expanded) => expanded,
		Err(e) => {
			eprintln!("Prompt Expansion Error: {}",e);
			"$> ".into()
		}
	};

	let mut lash_clone = lash.clone();
	let mut rl = rl_init::init_prompt(&mut lash_clone)?;
	lash.stop_timer()?;
	match rl.readline(&prompt) {
		Ok(line) => {
			lash.meta_mut().leave_prompt();
			if !line.is_empty() {
				rl.history_mut()
					.add(&line)
					.map_err(|_| Low(LashErrLow::InternalErr("Failed to write to history file".into())))?;
					rl.history_mut()
						.save(Path::new(&hist_path))
						.map_err(|_| Low(LashErrLow::InternalErr("Failed to write to history file".into())))?;
					lash.meta_mut().set_last_input(&line);
			}
			Ok(line)
		}
		Err(ReadlineError::Interrupted) => {
			lash.meta_mut().leave_prompt();
			Ok(String::new())
		}
		Err(ReadlineError::Eof) => {
			lash.meta_mut().leave_prompt();
			kill(Pid::this(), Signal::SIGQUIT).map_err(|_| Low(LashErrLow::from_io()))?;
			Ok(String::new())
		}
		Err(e) => {
			lash.meta_mut().leave_prompt();
			Err(Low(LashErrLow::InternalErr(format!("rustyline error: {}",e.to_string()))))?
		}
	}
}
