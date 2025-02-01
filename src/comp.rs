use crossterm::{
	cursor::{self, MoveTo, RestorePosition, Show}, execute, style::{style, Color, Print, Stylize}, terminal::{self, disable_raw_mode, enable_raw_mode, size, Clear, ClearType, EnterAlternateScreen, LeaveAlternateScreen}
};
use log::debug;
use once_cell::sync::Lazy;
use rustyline::{completion::{Candidate, Completer, FilenameCompleter}, error::ReadlineError, highlight::Highlighter, hint::{Hint, Hinter}, history::{FileHistory, History}, validate::{ValidationContext, ValidationResult, Validator}, Context, Helper, Validator};
use skim::{prelude::{Key, SkimItemReader, SkimOptionsBuilder}, Skim};
use std::{borrow::Cow, collections::{HashMap, HashSet, VecDeque}, env, fmt::Display, io::stdout, mem, path::{Path, PathBuf}};

use crate::{builtin::BUILTINS, event::ShError, interp::{expand, helper::{self, StrExtension}, parse::Span, token::{AssOp, LashTokenizer, Tk, WdFlags, KEYWORDS}}, shellenv::{read_logic, read_vars}};

pub const RESET: &str = "\x1b[0m";
pub const BLACK: &str = "\x1b[30m";
pub const RED: &str = "\x1b[1;31m";
pub const GREEN: &str = "\x1b[32m";
pub const YELLOW: &str = "\x1b[33m";
pub const BLUE: &str = "\x1b[34m";
pub const MAGENTA: &str = "\x1b[35m";
pub const CYAN: &str = "\x1b[36m";
pub const WHITE: &str = "\x1b[37m";
pub const BRIGHT_BLACK: &str = "\x1b[90m";
pub const BRIGHT_RED: &str = "\x1b[91m";
pub const BRIGHT_GREEN: &str = "\x1b[92m";
pub const BRIGHT_YELLOW: &str = "\x1b[93m";
pub const BRIGHT_BLUE: &str = "\x1b[94m";
pub const BRIGHT_MAGENTA: &str = "\x1b[95m";
pub const BRIGHT_CYAN: &str = "\x1b[96m";
pub const BRIGHT_WHITE: &str = "\x1b[97m";

pub const ERROR: &str = RED;
pub const COMMAND: &str = GREEN;
pub const KEYWORD: &str = YELLOW;
pub const STRING: &str = BLUE;
pub const ESCAPED: &str = CYAN;
pub const OPERATOR: &str = CYAN;
pub const NUMBER: &str = BRIGHT_BLUE;
pub const PATH: &str = BRIGHT_CYAN;
pub const VARSUB: &str = MAGENTA;
pub const COMMENT: &str = BRIGHT_BLACK;
pub const FUNCNAME: &str = CYAN;

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


pub fn check_balanced_delims(input: &str) -> Result<bool, ShError> {
	let mut delim_stack = vec![]; // Stack for delimiters like (), {}, []
	let mut keyword_stack = vec![]; // Stack for keywords like if/then/fi
	let mut chars = input.chars().peekable();
	let mut checked_chars = String::new();
	let mut is_command = true;

	while let Some(ch) = chars.next() {
		match ch {
			'\n' | ';' => {
				is_command = true;
			}
			' ' => {
				let last_word = checked_chars.split_whitespace().last();
				if last_word.is_some_and(|wrd| !KEYWORDS.contains(&wrd.trim())) {
					is_command = false;
				}
			}
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
							format!("Unmatched closing delimiter: {ch}").as_str(),
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
				while chars.peek().is_some_and(|ch| ch.is_alphanumeric() || *ch == '_' || *ch == '-') {
					let next = chars.next().unwrap(); // Consume the character
					checked_chars.push(next);
					keyword.push(next);
				}

				if is_command && KEYWORDS.contains(&keyword.trim()) {
					keyword_stack.push(keyword.trim().to_string())
				} else {
					match keyword.trim() {
						"fi" | "done" | "esac" => {
							let expectation = match keyword.trim() {
								"fi" => vec!["if", "else"],
								"done" => vec!["do", "while", "until", "for", "match", "in", "select"],
								"esac" => vec!["in"],
								_ => unreachable!()
							};
							if keyword_stack.last().is_some_and(|kw| expectation.contains(&kw.trim())) {
								keyword_stack.pop();
							}
						}
						"then" | "do" | "in" => {
							let expectation = match keyword.trim() {
								"then" => vec!["if", "elif"],
								"do" => vec!["in", "while", "until"],
								"in" => vec!["case","for","select","match"],
								_ => unreachable!()
							};
							if keyword_stack.last().is_some_and(|kw| expectation.contains(&kw.trim())) {
								if keyword != "then" && keyword != "do" {
									keyword_stack.pop();
									keyword_stack.push(keyword.trim().to_string());
								}
							}
						}
						_ => { /* Do nothing */ }
					}
				}
				if !keyword_stack.is_empty() {
					match keyword_stack.last().unwrap().as_str() {
						"done" => {
							keyword_stack.pop();
							if let Some(kw) = keyword_stack.last() {
								if matches!(kw.as_str(), "in") {
									keyword_stack.pop();
								}
							}
						}
						"fi" => {
							keyword_stack.pop();
							if let Some(kw) = keyword_stack.last() {
								if matches!(kw.as_str(), "then" | "else" | "elif" | "if") {
									keyword_stack.pop();
								}
							}
						}
						_ => { /* Do nothing */ }
					}
				}
			}
			_ => { /* Do nothing */ }
		}
		checked_chars.push(ch);
	}

	// Check if any delimiters or keywords remain unclosed
	if !delim_stack.is_empty() {
		return Ok(false)
	}
	if !keyword_stack.is_empty() {
		return Ok(false)
	}

	Ok(true)
}



pub struct LashHint {
	text: String,
	styled_text: String
}

impl LashHint {
	pub fn new(text: String) -> Self {
		let styled_text = style(&text).with(Color::DarkGrey).to_string();
		Self { text, styled_text }
	}
}

impl Hint for LashHint {
	fn display(&self) -> &str {
		&self.styled_text
	}
	fn completion(&self) -> Option<&str> {
		if !self.text.is_empty() {
			Some(&self.text)
		} else {
			None
		}
	}
}

pub fn validate_cmd(target: &str, path: &str) -> bool {
	if target.is_empty() || path.is_empty() {
		return false
	}
	let logic = read_logic(|l| l.clone()).unwrap();
	let is_cmd = path.split(':')
			.map(Path::new)
			.any(|p| p.join(target).exists());
	let is_func = logic.get_func(target).is_some();
	let is_alias = logic.get_alias(target).is_some();
	let is_builtin = BUILTINS.contains(&target);
	let is_file = {
		let mut path_cand = target.to_string();
		if path_cand.starts_with("~/") {
			path_cand = path_cand.strip_prefix("~").unwrap().to_string();
			let home = env::var("HOME").unwrap();
			path_cand = format!("{home}{path_cand}");
		}
		let path = Path::new(&path_cand);
		path.exists() && path.is_file()
	};

	is_cmd | is_func | is_alias | is_builtin | is_file
}

pub fn style_text(code: &str, text: &str) -> String {
	format!("{code}{text}{RESET}")
}

pub fn highlight_token(tk: Tk, path: &str) -> String {
	use crate::interp::token::TkType as TkT;
	match tk.class() {
    TkT::If |
    TkT::Then |
    TkT::Else |
    TkT::Elif |
    TkT::Fi |
    TkT::For |
    TkT::While |
    TkT::Until |
    TkT::Do |
    TkT::Done |
		TkT::Match |
    TkT::Select |
    TkT::In => {
			return style_text(KEYWORD,tk.text());
		}
		TkT::MatchArm {..} => {
			let (pat,body) = tk.text().split_once("=>").unwrap();
			let pat = style_text(RESET,pat);
			let body = if body.trim().starts_with('{') && body.trim().trim_end_matches(',').ends_with('}') {
				let (left,middle,right) = body.split_twice("{","}").unwrap();
				let middle = style_text(STRING,&middle);
				format!("{left}{}{middle}{}{right}",'{','}')
			} else if body.trim().ends_with(',') && !body.trim().ends_with("\\,") {
				let (left,right) = body.split_last(",").unwrap();
				let left = style_text(STRING,&left);
				format!("{left}{}{right}",',')
			} else {
				style_text(STRING,body)
			};
			let sep = style_text(KEYWORD,"=>");
			let formatted = format!("{pat}{sep}{body}");
			return style_text(STRING,&formatted);
		}
    TkT::FuncBody => {
			let body = tk.text().trim_matches(['{','}']);
			let hl_body = style_text(STRING,body);
			return format!("{}{}{}",'{',hl_body,'}');
		}
    TkT::LoopCond | TkT::LoopBody => {
			let mut sub_tokenizer = LashTokenizer::new(tk.text());
			let mut hl_cond = String::new();
			loop {
				let result = sub_tokenizer.tokenize_one(false);
				if result.is_err() {
					return tk.text().to_string()
				} else {
					let block = result.unwrap();
					if block.is_empty() {
						break
					}
					for tk in block {
						let styled_text = highlight_token(tk, path);
						hl_cond.push_str(&styled_text);
					}
				}
			}
			return hl_cond;
		}
    TkT::LogicAnd |
    TkT::LogicOr |
    TkT::Pipe |
    TkT::PipeBoth |
    TkT::Background |
    TkT::Redirection { redir: _ } => {
			return style_text(OPERATOR,tk.text());
		}
    TkT::FuncDef => {
			let func_name = tk.text();
			let hl_name = style_text(STRING, func_name);
			return format!("{}{}",hl_name,"()");
		}
    TkT::Assignment { key, value, op } => {
			let hl_key = style_text(STRING,&key);
			let hl_val = style_text(VARSUB,value.text());
			let op = match op {
				AssOp::PlusEquals => style_text(RESET,"+="),
				AssOp::MinusEquals => style_text(RESET,"-="),
				AssOp::Equals => style_text(RESET,"="),
			};
			return format!("{hl_key}{op}{hl_val}");
		}

    TkT::ProcessSub |
		TkT::CommandSub => {
			let hl_body = style_text(KEYWORD,tk.text());
			return format!("{}{}{}{}","$(",hl_body,RESET,')');
		}
    TkT::Subshell => {
			let hl_body = style_text(KEYWORD,tk.text());
			return format!("{}{}{}{}",'(',hl_body,RESET,')');
		}
    TkT::Array { elements } => {
			let arr = elements.iter().map(|elem| elem.text()).collect::<Vec<&str>>().join(" ");
			return style_text(VARSUB,&arr);
		}
    TkT::Vars { vars } => {
			let vars = vars.iter().map(|var| var.text()).collect::<Vec<&str>>().join(" ");
			return style_text(VARSUB,&vars);
		}
		TkT::DQuote | TkT::SQuote => {
			return style_text(RESET,tk.text())
		}
    TkT::String => {
			return style_text(STRING,tk.text())
		}
    TkT::Ident => {
			if tk.flags().contains(WdFlags::IS_ARG) {
				return style_text(RESET, tk.text());
			} else {
				let is_valid = validate_cmd(tk.text(), path);
				if is_valid {
					return style_text(COMMAND,tk.text());
				} else {
					return style_text(ERROR,tk.text());
				}
			}
		}
    TkT::BraceExpansion => todo!(),
    TkT::VariableSub => {
			return style_text(VARSUB,tk.text())
		}
    TkT::Comment => {
			return style_text(COMMENT,tk.text())
		}
    TkT::Cmdsep => {
			return style_text(RESET,tk.text())
		}
    TkT::CasePat => todo!(),
    TkT::Space |
    TkT::Tab => {
			return style_text(RESET,tk.text())
		}
    TkT::SOI => String::new(),
    TkT::EOI => String::new(),
    TkT::Null => String::new(),
		_ => unreachable!() // (hopefully)
	}
}


#[derive(Helper)]
pub struct LashHelper {
	filename_comp: FilenameCompleter,
	commands: Vec<String>, // List of built-in or cached commands
}

impl Highlighter for LashHelper {
	fn highlight_char(&self, line: &str, pos: usize, kind: rustyline::highlight::CmdKind) -> bool {
	    return true
	}
	fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {

		let mut line_tokenizer = LashTokenizer::new(line);
		let mut hl_buffer = String::new();
		let path = read_vars(|v| v.get_evar("PATH")).unwrap().unwrap_or_default();
		loop {
			let result = line_tokenizer.tokenize_one(false);
			if result.is_err() {
				return Cow::Owned(line.to_string());
			}
			let block = result.unwrap();
			if block.is_empty() {
				break
			}
			for tk in block {
				let hl_tk = highlight_token(tk, &path);
				hl_buffer.push_str(&hl_tk);
			}
			hl_buffer.push_str(RESET);
		}

		Cow::Owned(hl_buffer)
	}
}

impl Hinter for LashHelper {
	fn hint(&self, line: &str, pos: usize, ctx: &Context<'_>) -> Option<Self::Hint> {
		if line.is_empty() {
			return None
		}
		let history = ctx.history();
		let result = self.hist_substr_search(line, history);
		if let Some(hist_line) = result {
			let window = hist_line[line.len()..].to_string();
			let hint = LashHint::new(window);
			Some(hint)
		} else {
			None
		}
	}

type Hint = LashHint;
}

impl Validator for LashHelper {
	fn validate(&self, ctx: &mut ValidationContext) -> rustyline::Result<ValidationResult> {
		// Get the current input from the context
		let input = ctx.input();

		// Use the `check_balanced_delims` function to validate the input
		match check_balanced_delims(input) {
			Ok(true) => Ok(ValidationResult::Valid(None)), // Input is valid
			Ok(false) => Ok(ValidationResult::Incomplete), // Input is incomplete
			Err(err) => {
				let message = match err {
					ShError::InvalidSyntax(msg, _) => msg,
					_ => "Unknown syntax error".to_string(),
				};
				Ok(ValidationResult::Invalid(Some(message))) // Input is invalid
			}
		}
	}
}

impl LashHelper {
	pub fn new() -> Self {
		// Prepopulate some built-in commands (could also load dynamically)
		let commands = vec![
			"cd".to_string(),
			"ls".to_string(),
			"echo".to_string(),
			"exit".to_string(),
		];

		let mut helper = LashHelper {
			filename_comp: FilenameCompleter::new(),
			commands,
		};
		helper.update_commands_from_path();
		helper
	}

	fn hist_substr_search(&self, term: &str, hist: &dyn History) -> Option<String> {
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
	fn update_commands_from_path(&mut self) {
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

impl Default for LashHelper {
	fn default() -> Self {
		Self::new()
	}
}

impl Completer for LashHelper {
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
	let mut stdout = stdout();

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
