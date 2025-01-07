use crate::{comp::RshHelper, event::{self, ShError, ShEvent}, shellenv::{read_meta, read_vars, write_meta}, RshResult, GLOBAL_EVENT_CHANNEL};
use std::{path::{Path, PathBuf}, sync::mpsc::{Receiver, Sender}};
use signal_hook::consts::signal::*;

use rustyline::{self, config::Configurer, error::ReadlineError, history::{DefaultHistory, History}, ColorMode, Config, EditMode, Editor};

use crate::interp::parse::{descend, NdType};
use crate::interp::expand;



fn init_prompt() -> RshResult<Editor<RshHelper, DefaultHistory>> {
	let mut config = Config::builder();

	// Read options from ShellEnv
	let max_size = read_meta(|m| m.get_shopt("max_hist").unwrap_or(1000))?; // Default to 1000
	let hist_dupes = read_meta(|m| m.get_shopt("hist_ignore_dupes").is_some_and(|opt| opt > 0))?;
	let comp_limit = read_meta(|m| m.get_shopt("comp_limit").unwrap_or(5))?; // Default to 5
	let edit_mode = match read_meta(|m| m.get_shopt("edit_mode").unwrap_or(1))? {
		0 => EditMode::Emacs,
		_ => EditMode::Vi,
	};
	let auto_hist = read_meta(|m| m.get_shopt("auto_hist").is_some_and(|opt| opt > 0))?;
	let prompt_highlight = match read_meta(|m| m.get_shopt("prompt_highlight").unwrap_or(1))? {
		0 => ColorMode::Disabled,
		_ => ColorMode::Enabled,
	};
	let tab_stop = read_meta(|m| m.get_shopt("tab_stop").unwrap_or(1).max(1))?; // Default to at least 1

	// Build configuration
	config = config
		.max_history_size(max_size)
		.unwrap_or_else(|e| {
			eprintln!("Invalid max history size: {}", e);
			std::process::exit(1);
		})
	.history_ignore_dups(hist_dupes)
		.unwrap()
		.completion_prompt_limit(comp_limit)
		.edit_mode(edit_mode)
		.auto_add_history(auto_hist)
		.color_mode(prompt_highlight)
		.tab_stop(tab_stop);

		let config = config.build();

		// Initialize editor
		let mut rl = Editor::with_config(config).unwrap_or_else(|e| {
			eprintln!("Failed to initialize Rustyline editor: {}", e);
			std::process::exit(1);
		});
		rl.set_completion_type(rustyline::CompletionType::List);
		rl.set_helper(Some(RshHelper::new()));

		// Load history file
		let hist_path = read_vars(|vars| vars.get_evar("HIST_FILE"))?.unwrap_or_else(|| -> String {
			let home = read_vars(|vars| vars.get_evar("HOME").unwrap()).unwrap();
			format!("{}/.rsh_hist",home)
		});
		let hist_path = PathBuf::from(hist_path);
		if let Err(e) = rl.load_history(&hist_path) {
			eprintln!("No previous history found or failed to load history: {}", e);
		}

		Ok(rl)
}

pub struct PromptDispatcher {
	inbox: Receiver<ShEvent>,
}

impl PromptDispatcher {
	pub fn new(inbox: Receiver<ShEvent>) -> Self {
		Self { inbox }
	}
	pub fn run(&self) -> RshResult<()> {
		for message in self.inbox.iter() {
			if let ShEvent::Prompt = message {
				let mut rl = init_prompt()?;
				let hist_path = read_vars(|vars| vars.get_evar("HIST_FILE"))?.unwrap_or_else(|| -> String {
					let home = read_vars(|vars| vars.get_evar("HOME").unwrap()).unwrap();
					format!("{}/.rsh_hist",home)
				});
				let prompt = expand::expand_prompt()?;
				match rl.readline(&prompt) {
					Ok(line) => {
						rl.history_mut().add(&line).map_err(|_| ShError::from_internal("Failed to write to history file"))?;
						rl.history_mut().save(Path::new(&hist_path)).map_err(|_| ShError::from_internal("Failed to write to history file"))?;
						write_meta(|m| m.set_last_input(&line))?;
						let result = descend(&line);
						match result {
							Ok(state) => {
								let deck = if let NdType::Root { ref deck } = state.ast.nd_type {
									deck
								} else { unreachable!() };
								if !deck.is_empty() {
									event::global_send(ShEvent::NewAST(state.ast))?
								}
							}
							Err(e) => {
								event::global_send(ShEvent::Error(e))?
							}
						}
					}
					Err(ReadlineError::Interrupted) => {
						event::global_send(ShEvent::Signal(SIGINT))?;
						event::global_send(ShEvent::Prompt)?;
					}
					Err(ReadlineError::Eof) => {
						event::global_send(ShEvent::Signal(SIGQUIT))?;
						event::global_send(ShEvent::Prompt)?;
					}
					Err(e) => {
						eprintln!("{:?}",e);
					}
				}
			} else { return Err(ShError::from_internal(format!("Expected Prompt event, got this: {:?}", message).as_str())) }
		}
		Ok(())
	}
}
