use crossterm::{
	cursor::{MoveTo, RestorePosition, Show}, execute, style::Print, terminal::{disable_raw_mode, enable_raw_mode, size, Clear, ClearType, EnterAlternateScreen, LeaveAlternateScreen}
};
use once_cell::sync::Lazy;
use rustyline::{completion::{Candidate, Completer, FilenameCompleter}, error::ReadlineError, history::{FileHistory, History}, validate::{ValidationContext, ValidationResult, Validator}, Context, Helper, Highlighter, Hinter, Validator};
use skim::{prelude::{SkimItemReader, SkimOptionsBuilder}, Skim};
use std::{collections::{HashMap, HashSet}, env, io::stdout, path::PathBuf};

use crate::{event::ShError, interp::parse::Span, shellenv::read_vars};

static DELIM_PAIRS: Lazy<HashMap<String, Vec<String>>> = Lazy::new(|| {
	let mut m = HashMap::new();

	// Parentheses
	m.insert(")".into(), vec!["(".into()]);

	// Braces and brackets
	m.insert("}".into(), vec!["{".into()]);
	m.insert("]".into(), vec!["[".into()]);

	// Conditional statements
	m.insert("if".into(), vec!["then".into()]);
	m.insert("then".into(), vec!["elif".into(), "else".into(), "fi".into()]);
	m.insert("elif".into(), vec!["then".into()]);
	m.insert("else".into(), vec!["fi".into()]);

	// Loops
	m.insert("for".into(), vec!["do".into()]);
	m.insert("while".into(), vec!["do".into()]);
	m.insert("do".into(), vec!["done".into()]);

	// Case statements
	m.insert("case".into(), vec!["esac".into()]);

	m
});

pub fn check_balanced_delims(input: &str) -> Result<bool, ShError> {
	let mut delim_stack = vec![]; // Stack for delimiters like (), {}, []
	let mut keyword_stack = vec![]; // Stack for keywords like if/then/fi
	let mut chars = input.chars();

	while let Some(ch) = chars.next() {
		match ch {
			'\\' => {
				// Skip the next character after a backslash (escape)
				chars.next();
			}
			'{' | '[' => {
				// Push opening delimiters onto the stack
				delim_stack.push(ch);
			}
			'}' | ']' => {
				// Handle closing delimiters
				let expected = match ch {
					')' => '(',
					'}' => '{',
					']' => '[',
					_ => unreachable!(),
				};

				// Check if the top of the stack matches the expected opening delimiter
				if delim_stack.pop() != Some(expected) {
					return Err(ShError::from_syntax(
							format!("Unmatched closing delimiter: {}", ch).as_str(),
							Span::new(),
					));
				}
			}
			'\'' | '"' => {
				// Handle quoted strings: skip everything inside the quotes
				let opening_quote = ch;
				delim_stack.push(ch);
				while let Some(next_char) = chars.next() {
					if next_char == '\\' {
						// Skip escaped characters inside quotes
						chars.next();
					} else if next_char == opening_quote {
						delim_stack.pop();
						// Found the matching closing quote
						break;
					}
				}
			}
			'(' => {
				delim_stack.push(ch);
				while let Some(next_char) = chars.next() {
					if next_char == '\\' {
						chars.next();
					} else if next_char == ')' {
						delim_stack.pop();
						if delim_stack.last().is_none_or(|dlm| *dlm != '(') {
							break;
						}
					} else if next_char == '(' {
						delim_stack.push(next_char);
					}
				}
			}
			_ if ch.is_alphanumeric() || ch == '_' => {
				// Handle keywords
				let mut keyword = String::new();
				keyword.push(ch);

				// Accumulate additional characters for the keyword
				while let Some(next) = chars.clone().next() {
					if next.is_alphanumeric() || next == '_' {
						chars.next(); // Consume the character
						keyword.push(next);
					} else {
						break;
					}
				}

				if keyword_stack.is_empty() {
					match keyword.as_str() {
						"if" | "while" | "for" | "until" | "select" | "case" => {
							keyword_stack.push(keyword.clone())
						}
						_ => { /* Do nothing */ }
					}
				} else {
					match keyword.as_str() {
						"fi" | "done" | "esac" => {
							let expectation = match keyword.as_str() {
								"fi" => vec!["then", "else"],
								"done" => vec!["do"],
								"esac" => vec!["in"],
								_ => unreachable!()
							};
							if keyword_stack.last().is_none_or(|kw| !expectation.contains(&kw.as_str())) {
								return Err(ShError::from_syntax(format!("Unexpected keyword: {}",keyword).as_str(), Span::new()))
							} else {
								keyword_stack.pop();
							}
						}
						"then" | "do" | "in" => {
							let expectation = match keyword.as_str() {
								"then" => vec!["if", "elif"],
								"do" => vec!["in", "while", "until"],
								"in" => vec!["case","for","select"],
								_ => unreachable!()
							};
							if keyword_stack.last().is_none_or(|kw| !expectation.contains(&kw.as_str())) {
								return Err(ShError::from_syntax(format!("Unexpected keyword: {}",keyword).as_str(), Span::new()))
							} else {
								keyword_stack.pop();
								keyword_stack.push(keyword.clone());
							}
						}
						_ => { /* Do nothing */ }
					}
				}
			}
			_ => { /* Do nothing */ }
		}
	}

	// Check if any delimiters or keywords remain unclosed
	if !delim_stack.is_empty() {
		eprintln!("delim_stack: {}", delim_stack.last().unwrap());
		return Ok(false);
	}
	if !keyword_stack.is_empty() {
		eprintln!("keyword_stack: {}", keyword_stack.last().unwrap());
		return Ok(false);
	}

	Ok(true)
}


#[derive(Hinter,Highlighter,Helper)]
pub struct RshHelper {
	filename_comp: FilenameCompleter,
	commands: Vec<String>, // List of built-in or cached commands
}

impl Validator for RshHelper {
	fn validate(&self, ctx: &mut ValidationContext) -> rustyline::Result<ValidationResult> {
		// Get the current input from the context
		let input = ctx.input();

		// Use the `check_balanced_delims` function to validate the input
		match check_balanced_delims(input) {
			Ok(true) => Ok(ValidationResult::Valid(None)), // Input is valid
			Ok(false) => Ok(ValidationResult::Incomplete), // Input is incomplete
			Err(err) => {
				// Convert ShError to a user-friendly error message
				let message = match err {
					ShError::InvalidSyntax(msg, _) => msg,
					_ => "Unknown syntax error".to_string(),
				};
				Ok(ValidationResult::Invalid(Some(message))) // Input is invalid
			}
		}
	}
}

impl RshHelper {
	pub fn new() -> Self {
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

impl Default for RshHelper {
	fn default() -> Self {
		Self::new()
	}
}

impl Completer for RshHelper {
	type Candidate = String;

	fn complete(
		&self,
		line: &str,
		pos: usize,
		_: &Context<'_>,
	) -> Result<(usize, Vec<Self::Candidate>), ReadlineError> {
		let mut completions = Vec::new();
		let num_words = line.split_whitespace().count();

		// Determine if this is a file path or a command completion
		if !line.is_empty() && num_words > 1 {
			//TODO: Handle these unwraps
			let hist_path = read_vars(|vars| vars.get_evar("HIST_FILE")).unwrap().unwrap_or_else(|| -> String {
				let home = read_vars(|vars| vars.get_evar("HOME").unwrap()).unwrap();
				format!("{}/.rsh_hist",home)
			});
			let hist_path = PathBuf::from(hist_path);

			// Delegate to FilenameCompleter for file path completion
			let mut history = FileHistory::new();
			history.load(&hist_path).unwrap();
			let (start, matches) = self.filename_comp.complete(line, pos, &Context::new(&history))?;
			completions.extend(matches.iter().map(|c| c.display().to_string()));

			// Invoke fuzzyfinder if there are matches
			if !completions.is_empty() && completions.len() > 1 {
				if let Some(selected) = skim_comp(completions.clone()) {
					return Ok((start, vec![selected]));
				}
			}

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

		// Invoke fuzzyfinder if there are matches
		if completions.len() > 1 {
			if let Some(selected) = skim_comp(completions.clone()) {
				return Ok((0, vec![selected]));
			}
		}

		// Return completions, starting from the beginning of the word
		Ok((0, completions))
	}
}

pub fn skim_comp(options: Vec<String>) -> Option<String> {
	let mut stdout = stdout();

	// Enter the alternate screen
	execute!(stdout, EnterAlternateScreen).unwrap();
	enable_raw_mode().unwrap();

	// Get terminal dimensions
	let (cols, rows) = size().unwrap();
	let width = cols.min(50); // Set floating window width
	let height = rows.min(10); // Set floating window height
	let start_col = 0;
	let start_row = (rows - height) / 2;

	// Draw the floating window border
	execute!(
		stdout,
		MoveTo(start_col, start_row),
		Print("┌".to_string() + &"─".repeat(width as usize - 2) + "┐")
	)
		.unwrap();

		for i in 1..height - 1 {
			execute!(
				stdout,
				MoveTo(start_col, start_row + i),
				Print("│".to_string() + &" ".repeat(width as usize - 2) + "│")
			)
				.unwrap();
				}

		execute!(
			stdout,
			MoveTo(start_col, start_row + height - 1),
			Print("└".to_string() + &"─".repeat(width as usize - 2) + "┘")
		)
			.unwrap();

		// Prepare options for skim
		let options_join = options.join("\n");
		let input = SkimItemReader::default().of_bufread(std::io::Cursor::new(options_join));

		let skim_options = SkimOptionsBuilder::default()
			.prompt("Select > ".to_string())
			.height("25%".to_string()) // Height in lines relative to floating window
			.multi(false)
			.build()
			.unwrap();

		// Run skim within the alternate screen
		let selected = Skim::run_with(&skim_options, Some(input))
			.and_then(|out| out.selected_items.first().cloned())
			.map(|item| item.output().to_string());

		// Leave the alternate screen and restore original content
		execute!(stdout, Clear(ClearType::All), RestorePosition, LeaveAlternateScreen, Show).unwrap();
		disable_raw_mode().unwrap();

		selected
}
