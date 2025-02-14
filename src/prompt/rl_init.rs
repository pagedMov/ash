use rustyline::{config::Configurer, history::DefaultHistory, ColorMode, Config, EditMode, Editor};

use crate::{prelude::*, shellenv::EnvMeta};

use super::prompt::SlashHelper;

pub fn load_history(path: &Path, rl: &mut Editor<SlashHelper, DefaultHistory>) -> SlashResult<()> {
	if let Err(e) = rl.load_history(path) {
		eprintln!("No previous history found or failed to load history: {}", e);
	}
	Ok(())
}

pub fn init_prompt<'a>(slash: &'a mut Slash) -> SlashResult<Editor<SlashHelper<'a>, DefaultHistory>> {
	let config = build_editor_config(slash.meta())?;
	let path = format!("{}/.slash_hist",env::var("HOME").unwrap_or_default());
	let hist_path = Path::new(&path);
	let mut rl = initialize_editor(slash,config)?;
	load_history(hist_path,&mut rl)?;
	Ok(rl)
}

pub fn initialize_editor<'a>(slash: &'a mut Slash,config: Config) -> SlashResult<Editor<SlashHelper<'a>, DefaultHistory>> {
	let mut rl = Editor::with_config(config).unwrap_or_else(|e| {
		eprintln!("Failed to initialize Rustyline editor: {}", e);
		std::process::exit(1);
	});
	rl.set_completion_type(rustyline::CompletionType::List);
	rl.set_helper(Some(SlashHelper::new(slash)));
	Ok(rl)
}

pub fn build_editor_config(meta: &EnvMeta) -> SlashResult<Config> {
	let mut config = Config::builder();

	let max_size = meta.get_shopt("core.max_hist")?.parse::<usize>().unwrap();
	let hist_dupes = meta.get_shopt("core.hist_ignore_dupes")?.parse::<bool>().unwrap();
	let comp_limit = meta.get_shopt("prompt.comp_limit")?.parse::<usize>().unwrap();
	let edit_mode = match meta.get_shopt("prompt.edit_mode")?.trim_matches('"') {
		"emacs" => EditMode::Emacs,
		"vi" => EditMode::Vi,
		_ => {
			return Err(Low(SlashErrLow::InternalErr("Invalid shopts.prompt.edit_mode value".into())))
		}
	};
	let auto_hist = meta.get_shopt("core.auto_hist")?.parse::<bool>().unwrap();
	let prompt_highlight = match meta.get_shopt("prompt.prompt_highlight")?.parse::<bool>().unwrap() {
		true => ColorMode::Enabled,
		false => ColorMode::Disabled,
	};
	let tab_stop = meta.get_shopt("prompt.tab_stop")?.parse::<usize>().unwrap();

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
