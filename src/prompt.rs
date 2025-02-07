use std::path::{Path, PathBuf};

use nix::{sys::signal::{kill, Signal}, unistd::Pid};
use rustyline::{config::Configurer, error::ReadlineError, history::{DefaultHistory, History}, ColorMode, Config, EditMode, Editor};

use crate::{comp::LashHelper, error::{LashErr::*, LashErrHigh, LashErrLow}, expand, shellenv::{read_meta, read_vars, write_meta}, LashResult};

fn load_history(rl: &mut Editor<LashHelper, DefaultHistory>) -> LashResult<()> {
	let hist_path = read_vars(|vars| vars.get_evar("HIST_FILE"))?.unwrap_or_else(|| {
		let home = read_vars(|vars| vars.get_evar("HOME").unwrap()).unwrap();
		format!("{}/.lash_hist", home)
	});
	let hist_path = PathBuf::from(hist_path);
	if let Err(e) = rl.load_history(&hist_path) {
		eprintln!("No previous history found or failed to load history: {}", e);
	}
	Ok(())
}

fn init_prompt() -> LashResult<Editor<LashHelper, DefaultHistory>> {
	let config = build_editor_config()?;
	let mut rl = initialize_editor(config)?;
	load_history(&mut rl)?;
	Ok(rl)
}

fn initialize_editor(config: Config) -> LashResult<Editor<LashHelper, DefaultHistory>> {
	let mut rl = Editor::with_config(config).unwrap_or_else(|e| {
		eprintln!("Failed to initialize Rustyline editor: {}", e);
		std::process::exit(1);
	});
	rl.set_completion_type(rustyline::CompletionType::List);
	rl.set_helper(Some(LashHelper::new()));
	Ok(rl)
}

fn read_shell_option(option: &str) -> LashResult<String> {
	read_meta(|m| m.get_shopt(option).unwrap_or_default())
}

fn build_editor_config() -> LashResult<Config> {
	let mut config = Config::builder();

	let max_size = read_shell_option("core.max_hist")?.parse::<usize>().unwrap();
	let hist_dupes = read_shell_option("core.hist_ignore_dupes")?.parse::<bool>().unwrap();
	let comp_limit = read_shell_option("prompt.comp_limit")?.parse::<usize>().unwrap();
	let edit_mode = match read_shell_option("prompt.edit_mode")?.trim_matches('"') {
		"emacs" => EditMode::Emacs,
		"vi" => EditMode::Vi,
		_ => {
			return Err(Low(LashErrLow::InternalErr("Invalid shopts.prompt.edit_mode value".into())))
		}
	};
	let auto_hist = read_shell_option("core.auto_hist")?.parse::<bool>().unwrap();
	let prompt_highlight = match read_shell_option("prompt.prompt_highlight")?.parse::<bool>().unwrap() {
		true => ColorMode::Enabled,
		false => ColorMode::Disabled,
	};
	let tab_stop = read_shell_option("prompt.tab_stop")?.parse::<usize>().unwrap();

	config = config
		.max_history_size(max_size)
			.unwrap_or_else(|e| {
				eprintln!("Invalid max history size: {}", e);
				std::process::exit(1);
			})
		.history_ignore_dups(hist_dupes).unwrap()
		.completion_prompt_limit(comp_limit)
		.edit_mode(edit_mode)
		.auto_add_history(auto_hist)
		.color_mode(prompt_highlight)
		.tab_stop(tab_stop);

		Ok(config.build())
}

pub fn run_prompt() -> LashResult<String> {
	write_meta(|m| m.enter_prompt())?;

	let mut rl = init_prompt()?;
	let hist_path = read_vars(|vars| vars.get_evar("HIST_FILE"))?.unwrap_or_else(|| -> String {
		let home = read_vars(|vars| vars.get_evar("HOME").unwrap()).unwrap();
		format!("{}/.lash_hist",home)
	});
	write_meta(|m| m.stop_timer())??;
	let prompt = expand::expand_prompt(None)?;

	match rl.readline(&prompt) {
		Ok(line) => {
			write_meta(|m| m.leave_prompt())?;
			if !line.is_empty() {
				rl.history_mut()
					.add(&line)
					.map_err(|_| Low(LashErrLow::InternalErr("Failed to write to history file".into())))?;
					rl.history_mut()
						.save(Path::new(&hist_path))
						.map_err(|_| Low(LashErrLow::InternalErr("Failed to write to history file".into())))?;
					write_meta(|m| m.set_last_input(&line))?;
			}
			Ok(line)
		}
		Err(ReadlineError::Interrupted) => {
			write_meta(|m| m.leave_prompt())?;
			Ok(String::new())
		}
		Err(ReadlineError::Eof) => {
			write_meta(|m| m.leave_prompt())?;
			kill(Pid::this(), Signal::SIGQUIT).map_err(|_| Low(LashErrLow::from_io()))?;
			Ok(String::new())
		}
		Err(e) => {
			write_meta(|m| m.leave_prompt())?;
			Err(Low(LashErrLow::InternalErr(format!("rustyline error: {}",e.to_string()))))?
		}
	}
}
