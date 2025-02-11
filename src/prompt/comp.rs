use crossterm::{cursor::{self, MoveTo}, execute, terminal::{Clear, ClearType}};
use rustyline::{completion::{Candidate, Completer, FilenameCompleter}, error::ReadlineError, Context};
use skim::{prelude::{Key, SkimItemReader, SkimOptionsBuilder}, Skim};

use crate::{helper, prelude::*};

use super::prompt::LashHelper;

pub struct CompRegistry {
	path_completer: FilenameCompleter,
	cmds: HashMap<String, Vec<CompOption>>
}

impl CompRegistry {
	pub fn new() -> Self {
		let cmds = HashMap::new();
		let path_completer = FilenameCompleter::new();
		Self { cmds, path_completer }
	}
	pub fn get_cmd(&self, key: String) -> Option<Vec<CompOption>> {
		let result = self.cmds.get(&key).map(|comp_vec| comp_vec.to_vec());
		result
	}
}

#[derive(Clone,Debug)]
pub enum CompType {
	Variables,
	EnvVars,
	Params,
	AbsPaths,
	Paths,
	Directories,
	Tilde,
	Commands,
	Aliases,
	Functions,
	Builtins,
	Keywords,
	Users,
	Groups,
	Pids,
	Jobs,
	Hosts,
	Mounts,
	Services
}

#[derive(Clone,Debug)]
pub struct CompOption {
	value: String,
	desc: Option<String>,
	comp_type: CompType,
	priority: usize
}

impl Candidate for CompOption {
	fn display(&self) -> &str {
		&self.value
	}
	fn replacement(&self) -> &str {
	  &self.value
	}
}

impl CompOption {
	pub fn path(path: &str) -> Self {
		Self {
			value: path.to_string(),
			desc: None,
			comp_type: CompType::Paths,
			priority: 0
		}
	}
	pub fn by_type(categories: Vec<CompType>) -> Vec<Self> {
		let mut options = vec![];
		for category in categories {
			match category {
				_ => unimplemented!()
			}
		}
		options
	}
	pub fn by_cmd(cmd: String) -> Vec<Self> {
		let mut options = vec![];
		options
	}
}

impl Display for CompOption {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f,"{}",self.value)
	}
}

impl<'a> Completer for LashHelper<'a> {
	type Candidate = CompOption;

	fn complete(
		&self,
		line: &str,
		pos: usize,
		ctx: &Context<'_>,
	) -> Result<(usize, Vec<Self::Candidate>), ReadlineError> {
		let mut completions = Vec::new();
		let line = line.to_string();
		let num_words = line.split_whitespace().count();

		// Determine if this is a file path or a command completion
		if !line.is_empty() && (num_words > 1 || line.split(" ").into_iter().next().is_some_and(|wrd| wrd.starts_with(['.','/','~']))) {
			//TODO: Handle these unwraps
			let (start, matches) = self.filename_comp.complete(&line, pos, ctx)?;
			completions.extend(matches.iter().map(|c| c.display().to_string()));
			let mut comp_opts = completions.into_iter().map(|opt| {
				CompOption::path(&opt)
			}).collect::<Vec<CompOption>>();

			// Invoke fuzzyfinder if there are matches
			if !comp_opts.is_empty() && comp_opts.len() > 1 {
				if let Some(selected) = skim_comp(comp_opts.clone()) {
					let result = helper::slice_completion(&line, &selected);
					let unfinished = line.split_whitespace().last().unwrap();
					let result = CompOption::path(&format!("{unfinished}{result}"));
					return Ok((start, vec![result]));
				}
			}

			// Return completions, starting from the beginning of the word
			if let Some(candidate) = comp_opts.pop() {
				let result = CompOption::path(&helper::slice_completion(&line, &candidate.to_string()));
				comp_opts.push(result);
			}
			return Ok((pos, comp_opts))
		}

		// Command completion
		let prefix = &line[..pos]; // The part of the line to match
		completions.extend(
			self.commands
			.iter()
			.filter(|cmd| cmd.starts_with(prefix)) // Match prefix
			.cloned(), // Clone matched command names
		);

		let mut comp_opts = completions.into_iter().map(|opt| {
			CompOption {
				value: opt,
				desc: None,
				comp_type: CompType::Paths,
				priority: 0
			}
		}).collect::<Vec<CompOption>>();
		// Invoke fuzzyfinder if there are matches
		if comp_opts.len() > 1 {
			if let Some(selected) = skim_comp(comp_opts.clone()) {
				let result = CompOption::path(&helper::slice_completion(&line, &selected));
				return Ok((pos, vec![result]));
			}
		}
		if let Some(candidate) = comp_opts.pop() {
			let expanded = helper::slice_completion(&line, &candidate.to_string());
			let result = CompOption::path(&expanded);
			comp_opts.push(result);
		}
		// Return completions, starting from the beginning of the word
		Ok((pos, comp_opts))
	}
}

pub fn skim_comp(options: Vec<CompOption>) -> Option<String> {
	let mut stdout = io::stdout();

	let (init_col, _) = cursor::position().unwrap();

	// Get terminal dimensions
	let height = options.len().min(10) as u16; // Set maximum number of options to display

	// Prepare options for skim
	let options_join = options.iter().map(|opt| opt.to_string()).collect::<Vec<String>>().join("\n");
	let input = SkimItemReader::default().of_bufread(std::io::Cursor::new(options_join));

	let skim_options = SkimOptionsBuilder::default()
		.prompt(String::new())
		.height(format!("{height}")) // Adjust height based on the options
		.reverse(true)
		.multi(false)
		.build()
		.unwrap();

		let selected = Skim::run_with(&skim_options, Some(input))
			.and_then(|out| {
				if out.final_key == Key::ESC {
					None // Return None if Escape is pressed
				} else {
					out.selected_items.first().cloned()
				}
			})
		.map(|item| item.output().to_string());

		let (_, new_row) = cursor::position().unwrap();

		for i in 0..height + 2 {
			execute!(
				stdout,
				MoveTo(0,new_row + i),
				Clear(ClearType::CurrentLine)
			).unwrap();
		}

		// Restore cursor position to where the prompt was before completion
		execute!(
			stdout,
			MoveTo(init_col, new_row - 1),
		).unwrap();

		selected
}
