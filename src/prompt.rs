use crate::prompt::rustyline::Context;
use im::HashSet;
use nix::NixPath;
use rustyline::completion::Candidate;
use rustyline::history::FileHistory;
use std::path::{Path, PathBuf};
use std::env;

use tokio::sync::mpsc;
use log::info;
use rustyline::{self, completion::{Completer, FilenameCompleter}, config::Configurer, error::ReadlineError, history::{DefaultHistory, History}, ColorMode, Completer, Config, EditMode, Editor, Helper, Highlighter, Hinter, Validator};

use crate::{event::{ShellError,ShellEvent}, shellenv::ShellEnv};
use crate::interp::parse::descend;
use crate::interp::expand;

#[derive(Hinter,Highlighter,Validator,Helper)]

struct RshHelper<'a> {
	filename_comp: FilenameCompleter,
	commands: Vec<String>, // List of built-in or cached commands
	shellenv: &'a ShellEnv
}

impl<'a> RshHelper<'a> {
	pub fn new(shellenv: &'a ShellEnv) -> Self {
		// Prepopulate some built-in commands (could also load dynamically)
		let commands = vec![
			"cd".to_string(),
			"ls".to_string(),
			"echo".to_string(),
			"exit".to_string(),
		];

		let mut helper = RshHelper {
			filename_comp: FilenameCompleter::new(),
			commands,
			shellenv
		};
		helper.update_commands_from_path();
		helper
	}

	// Dynamically add commands (if needed, e.g., external binaries in $PATH)
	fn update_commands_from_path(&mut self) {
		if let Some(paths) = env::var_os("PATH") {
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

impl<'a> Completer for RshHelper<'a> {
	type Candidate = String;

	fn complete(&self, line: &str, pos: usize, _: &Context<'_>) -> Result<(usize, Vec<Self::Candidate>),ReadlineError> {
		let mut completions = Vec::new();
		let num_words = line.split(' ').count();

		// Determine if this is a file path or a command completion
		if !line.is_empty() && num_words > 1 {
			let hist_path = expand::expand_var(self.shellenv, "$HIST_FILE".into());
			info!("history path in init_prompt(): {}",hist_path);
			let hist_path = PathBuf::from(hist_path);
			// Delegate to FilenameCompleter for file path completion
			let mut history = FileHistory::new();
			history.load(&hist_path).unwrap();
			let (start, matches) = self.filename_comp.complete(line, pos, &Context::new(&history))?;
			completions.extend(matches.iter().map(|c| c.display().to_string()));
			return Ok((start, completions));
		}

		// Command completion
		let prefix = &line[..pos]; // The part of the line to match
		completions.extend(
			self.commands
			.iter()
			.filter(|cmd| cmd.starts_with(prefix)) // Match prefix
			.cloned(), // Clone matched command names
		);

		// Return completions, starting from the beginning of the word
		Ok((0, completions))
	}
}

fn init_prompt(shellenv: &ShellEnv) -> Editor<RshHelper, DefaultHistory> {
	let mut config = Config::builder();

	// Read options from ShellEnv
	let max_size = shellenv.get_shopt("max_hist").max(1000); // Default to 1000
	let hist_dupes = shellenv.get_shopt("hist_ignore_dupes") != 0;
	let comp_limit = shellenv.get_shopt("comp_limit").max(5); // Default to 5
	let edit_mode = match shellenv.get_shopt("edit_mode") {
		0 => EditMode::Emacs,
		_ => EditMode::Vi,
	};
	let auto_hist = shellenv.get_shopt("auto_hist") != 0;
	let prompt_highlight = match shellenv.get_shopt("prompt_highlight") {
		0 => ColorMode::Disabled,
		_ => ColorMode::Enabled,
	};
	let tab_stop = shellenv.get_shopt("tab_stop").max(1); // Default to at least 1

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
		rl.set_helper(Some(RshHelper::new(shellenv)));

		// Load history file
		let hist_path = expand::expand_var(shellenv, "$HIST_FILE".into());
		info!("history path in init_prompt(): {}",hist_path);
		let hist_path = PathBuf::from(hist_path);
		if let Err(e) = rl.load_history(&hist_path) {
			eprintln!("No previous history found or failed to load history: {}", e);
		}

		rl
}

pub async fn prompt(sender: mpsc::Sender<ShellEvent>, shellenv: &mut ShellEnv) {
	let mut rl = init_prompt(shellenv);
	let hist_path = expand::expand_var(shellenv, "$HIST_FILE".into());
	let readline = rl.readline(">> ");
	if let Ok(line) = readline {
		let _ = rl.history_mut().add(&line);
		let _ = rl.history_mut().save(Path::new(&hist_path));
		shellenv.set_last_input(&line);
		let state = descend(&line,shellenv);
		match state {
			Ok(parse_state) => {
				let _ = sender.send(ShellEvent::NewAST(parse_state.ast)).await;
			}
			Err(e) => {
				let _ = sender.send(ShellEvent::CatchError(ShellError::ParsingError(e))).await;
			}
		}
	}
}
