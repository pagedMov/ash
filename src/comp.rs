use crossterm::{
	cursor::{self, MoveTo, RestorePosition, Show}, execute, style::{style, Color, Print, Stylize}, terminal::{disable_raw_mode, enable_raw_mode, size, Clear, ClearType, EnterAlternateScreen, LeaveAlternateScreen}
};
use log::debug;
use once_cell::sync::Lazy;
use rustyline::{completion::{Candidate, Completer, FilenameCompleter}, error::ReadlineError, highlight::Highlighter, hint::{Hint, Hinter}, history::{FileHistory, History}, validate::{ValidationContext, ValidationResult, Validator}, Context, Helper, Validator};
use skim::{prelude::{Key, SkimItemReader, SkimOptionsBuilder}, Skim};
use std::{borrow::Cow, collections::{HashMap, HashSet, VecDeque}, env, io::stdout, mem, path::{Path, PathBuf}};

use crate::{builtin::BUILTINS, event::ShError, interp::{helper::{self, StrExtension}, parse::Span, token::{AssOp, OxTokenizer, Tk, WdFlags, KEYWORDS}}, shellenv::{read_logic, read_vars}};

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
pub const OPERATOR: &str = BRIGHT_MAGENTA;
pub const NUMBER: &str = BRIGHT_BLUE;
pub const PATH: &str = BRIGHT_CYAN;
pub const VARSUB: &str = MAGENTA;
pub const COMMENT: &str = BRIGHT_BLACK;
pub const FUNCNAME: &str = CYAN;

#[derive(Debug)]
pub enum SyntaxTk {
	Keyword(String),
	CommandOk(String),
	CommandNotFound(String),
	Comment(String),
	Assignment(String),
	VarSub(String),
	CmdSub(String),
	Subshell(String),
	Path(String),
	Number(String),
	Arg(String),
	String(String),
	FuncName(String),
	Delim(String),
	Escaped(String),
	Operator(String),
	Space,
	Semi,
	Newline,
}

impl SyntaxTk {
	pub fn to_string(&self) -> String {
		match self {
    SyntaxTk::Keyword(word) |
    SyntaxTk::CommandOk(word) |
    SyntaxTk::CommandNotFound(word) |
    SyntaxTk::Comment(word) |
    SyntaxTk::Assignment(word) |
    SyntaxTk::VarSub(word) |
    SyntaxTk::CmdSub(word) |
    SyntaxTk::Subshell(word) |
    SyntaxTk::Path(word) |
    SyntaxTk::Number(word) |
    SyntaxTk::Arg(word) |
    SyntaxTk::String(word) |
    SyntaxTk::FuncName(word) |
    SyntaxTk::Delim(word) |
    SyntaxTk::Escaped(word) |
    SyntaxTk::Operator(word) => word.to_string(),
    SyntaxTk::Space => String::from(' '),
    SyntaxTk::Semi => String::from(';'),
    SyntaxTk::Newline => String::from('\n')
}
	}
	pub fn keyword(word: &str) -> Self {
		let formatted = format!("{}{}{}", KEYWORD, word, RESET);
		Self::Keyword(formatted)
	}
	pub fn assignment(word: &str) -> Option<Self> {
		if let Some((left,right)) = word.split_once('=') {
			let left_fmt = format!("{}{}{}",FUNCNAME,left,RESET);
			let right_fmt = format!("{}{}{}",VARSUB,right,RESET);
			let formatted = vec![left_fmt,right_fmt].join("=");
			Some(Self::Assignment(formatted))
		} else {
			None
		}
	}
	pub fn subshell(word: &str) -> Self {
		let word = word.trim_matches(['(',')']);
		let formatted = format!("{}{}{}{}{}{}{}",
			VARSUB,
			'(',
			RESET,
			word,
			VARSUB,
			')',
			RESET
		);
		Self::Subshell(formatted)
	}
	pub fn command_ok(word: &str) -> Self {
		let formatted = format!("{}{}{}", COMMAND, word, RESET);
		Self::CommandOk(formatted)
	}
	pub fn command_not_found(word: &str) -> Self {
		let formatted = format!("{}{}{}", ERROR, word, RESET);
		Self::CommandNotFound(formatted)
	}

	pub fn comment(text: &str) -> Self {
		let formatted = format!("{}{}{}", COMMENT, text, RESET);
		Self::Comment(formatted)
	}

	pub fn varsub(text: &str) -> Self {
		let formatted = format!("{}{}{}", VARSUB, text, RESET);
		Self::VarSub(formatted)
	}

	pub fn cmdsub(text: &str) -> Self {
		let formatted = format!("{}{}{}", FUNCNAME, text, RESET);
		Self::CmdSub(formatted)
	}

	pub fn path(path: &str) -> Self {
		let formatted = format!("{}{}{}", PATH, path, RESET);
		Self::Path(formatted)
	}

	pub fn number(num: &str) -> Self {
		let formatted = format!("{}{}{}", NUMBER, num, RESET);
		Self::Number(formatted)
	}

	pub fn arg(arg: &str) -> Self {
		let formatted = format!("{}{}{}", RESET, arg, RESET);
		Self::Arg(formatted)
	}

	pub fn string(literal: &str) -> Self {
		let formatted = format!("{}{}{}", STRING, literal, RESET);
		Self::String(formatted)
	}

	pub fn func_name(name: &str) -> Self {
		let formatted = format!("{}{}{}", FUNCNAME, name, RESET);
		Self::FuncName(formatted)
	}

	pub fn delim(delim: &str) -> Self {
		let formatted = format!("{}{}{}", OPERATOR, delim, RESET); // Assuming Delim is styled like Operator
		Self::Delim(formatted)
	}

	pub fn escaped(escaped: &str) -> Self {
		let formatted = format!("{}{}{}", ESCAPED, escaped, RESET);
		Self::Escaped(formatted)
	}

	pub fn operator(op: &str) -> Self {
		let formatted = format!("{}{}{}", OPERATOR, op, RESET);
		Self::Operator(formatted)
	}

	pub fn space() -> Self {
		Self::Space
	}

	pub fn semi() -> Self {
		Self::Semi
	}

	pub fn newline() -> Self {
		Self::Newline
	}
}

#[derive(Clone,PartialEq,Debug)]
pub enum SyntaxCtx {
	Arg, // Command arguments; only appear after commands
	Command, // Starting point for the tokenizer
	VarSub,
	FuncDef, // Function names
	FuncBody, // Function bodies
	Escaped, // Used to denote an escaped character like \a
	Comment, // #Comments like this
	CommandSub, // $(Command substitution)
	Operator, // operators
}

impl Default for SyntaxCtx {
	fn default() -> Self {
	    Self::Command
	}
}

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
				while chars.peek().is_some_and(|ch| ch.is_alphanumeric() || *ch == '_' || *ch == '-') {
					let next = chars.next().unwrap(); // Consume the character
					checked_chars.push(next);
					keyword.push(next);
				}

				if is_command && matches!(keyword.as_str(),"if" | "while" | "for" | "until" | "select" | "case") {
					keyword_stack.push(keyword.clone())
				} else {
					match keyword.as_str() {
						"fi" | "done" | "esac" => {
							let expectation = match keyword.as_str() {
								"fi" => vec!["if", "else"],
								"done" => vec!["do", "while", "until", "for", "in", "select"],
								"esac" => vec!["in"],
								_ => unreachable!()
							};
							if keyword_stack.last().is_some_and(|kw| expectation.contains(&kw.as_str())) {
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
							if keyword_stack.last().is_some_and(|kw| expectation.contains(&kw.as_str())) {
								if keyword != "then" && keyword != "do" {
									keyword_stack.pop();
									keyword_stack.push(keyword.clone());
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
		helper::breakpoint(format!("delim_stack: {}", delim_stack.last().unwrap()));
		return Ok(false);
	}
	if !keyword_stack.is_empty() {
		helper::breakpoint(format!("kw_stack: {}", keyword_stack.last().unwrap()));
		return Ok(false);
	}

	Ok(true)
}



pub struct OxHint {
	text: String,
	styled_text: String
}

impl OxHint {
	pub fn new(text: String) -> Self {
		let styled_text = style(&text).with(Color::DarkGrey).to_string();
		Self { text, styled_text }
	}
}

impl Hint for OxHint {
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
	let is_path = Path::new(target).exists();

	is_cmd | is_func | is_alias | is_builtin | is_path
}

pub fn analyze_token(word: &str, ctx: SyntaxCtx, path: &str) -> SyntaxTk {
	use crate::comp::SyntaxCtx::*;
	use crate::comp::SyntaxTk as STk;
	match ctx {
		Command => {
			if KEYWORDS.contains(&word) {
				return STk::keyword(&word)
			} else if word.starts_with('(') && word.ends_with(')') {
				return STk::subshell(&word)
			} else if word.has_unescaped("=") {
				return STk::assignment(&word).unwrap()
			} else if validate_cmd(&word, &path) {
				return STk::command_ok(&word)
			} else {
				return STk::command_not_found(&word)
			}
		}
		Arg => {
			if (word.starts_with('"') && word.ends_with('"')) || (word.starts_with('\'') && word.ends_with('\'')) {
				return STk::string(&word)
			} else if word.parse::<i32>().is_ok() || word.parse::<f32>().is_ok() {
				return STk::number(&word)
			} else {
				return STk::arg(&word)
			}
		}
		VarSub => {
			return STk::varsub(&word)
		}
    FuncDef => return STk::func_name(&word),
    FuncBody => todo!(),
    Escaped => return STk::escaped(&word),
    Comment => return STk::comment(&word),
    CommandSub => todo!(),
    Operator => todo!(),
	}
}

pub fn style_text(code: &str, text: &str) -> String {
	format!("{}{}{}",code,text,RESET)
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
    TkT::Case |
    TkT::Esac |
    TkT::Select |
    TkT::In => {
			return style_text(KEYWORD,tk.text());
		}
    TkT::FuncBody => {
			let body = tk.text().trim_matches(['{','}']);
			let hl_body = style_text(STRING,body);
			return format!("{}{}{}",'{',hl_body,'}');
		}
    TkT::LoopCond | TkT::LoopBody => {
			let mut sub_tokenizer = OxTokenizer::new(tk.text());
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
			let func_name = tk.text().strip_suffix("()").unwrap();
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
			return format!("{}{}{}",hl_key,op,hl_val);
		}

    TkT::ProcessSub |
		TkT::CommandSub |
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
pub struct OxHelper {
	filename_comp: FilenameCompleter,
	commands: Vec<String>, // List of built-in or cached commands
}

impl Highlighter for OxHelper {
	fn highlight_char(&self, line: &str, pos: usize, kind: rustyline::highlight::CmdKind) -> bool {
	    return true
	}
	fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
		use crate::comp::SyntaxCtx::*;
		use crate::comp::SyntaxTk as STk;

		let mut line_tokenizer = OxTokenizer::new(line);
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

impl Hinter for OxHelper {
	fn hint(&self, line: &str, pos: usize, ctx: &Context<'_>) -> Option<Self::Hint> {
		if line.is_empty() {
			return None
		}
		let history = ctx.history();
		let result = self.hist_substr_search(line, history);
		if let Some(hist_line) = result {
			let window = hist_line[line.len()..].to_string();
			let hint = OxHint::new(window);
			Some(hint)
		} else {
			None
		}
	}

type Hint = OxHint;
}

impl Validator for OxHelper {
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

impl OxHelper {
	pub fn new() -> Self {
		// Prepopulate some built-in commands (could also load dynamically)
		let commands = vec![
			"cd".to_string(),
			"ls".to_string(),
			"echo".to_string(),
			"exit".to_string(),
		];

		let mut helper = OxHelper {
			filename_comp: FilenameCompleter::new(),
			commands,
		};
		helper.update_commands_from_path();
		helper
	}

	fn highlight_one(&self, line: &mut VecDeque<char>) -> String {
		let mut hl_block = String::new();
		let mut prefix = ERROR; // Default case
		let mut cur_word = String::new();
		let mut dub_quote = false;
		let mut paren_stack = vec![];
		let path = read_vars(|v| v.get_evar("PATH")).unwrap().unwrap_or_default();
		while let Some(ch) = line.pop_front() {
			match ch {
				'\\' => {
					let saved_prefix = prefix;
					prefix = ESCAPED;
					let escaped = line.pop_front().map(|ch| ch.to_string()).unwrap_or_default();
					let formatted = format!("{}{}{}{}",prefix,ch,escaped,saved_prefix);
					hl_block.push_str(&formatted);
				}
				'(' if cur_word.is_empty() => {
					cur_word.push(ch);
					paren_stack.push(ch);
					while let Some(subsh_ch) = line.pop_front() {
						cur_word.push(subsh_ch);
						if subsh_ch == '\\' {
							if let Some(esc_ch) = line.pop_front() {
								cur_word.push(esc_ch);
							}
						}
						if subsh_ch == '(' {
							paren_stack.push(subsh_ch);
						}
						if subsh_ch == ')' {
							paren_stack.pop();
							if paren_stack.is_empty() {
								break
							}
						}
					}
					let formatted = format!("{}{}{}",VARSUB,mem::take(&mut cur_word),RESET);
					hl_block.push_str(&formatted);
				}
				'$' => {
					let mut var_name = String::from(format!("{}{}",VARSUB,ch));
					if line.front() == Some(&'{') {
						while let Some(var_ch) = line.pop_front() {
							var_name.push(var_ch);
							if var_ch == '\\' {
								if let Some(esc_ch) = line.pop_front() {
									var_name.push(esc_ch);
								}
							}
							if var_ch == '}' {
								var_name.push_str(RESET);
								break
							}
						}
					} else {
						while let Some(var_ch) = line.pop_front() {
							var_name.push(var_ch);
							if var_ch == '\\' {
								if let Some(esc_ch) = line.pop_front() {
									var_name.push(esc_ch);
								}
							}
							if var_ch == ' ' || var_ch == '\t' || var_ch == ';' || var_ch == '\n' {
								var_name.push_str(RESET);
								break
							}
						}
					}
					hl_block.push_str(&var_name);
				}
				'"' => {
					dub_quote = !dub_quote;
					let formatted = if dub_quote {
						prefix = STRING;
						format!("{}{}",prefix,ch)
					} else {
						prefix = RESET;
						format!("{}{}",ch,prefix)
					};
					hl_block.push_str(&formatted);
				}
				_ if dub_quote => {
					hl_block.push(ch);
				}
				'\'' => {
					let mut sng_quoted = String::from(format!("{}{}",STRING,ch));
					while let Some(quoted_ch) = line.pop_front() {
						sng_quoted.push(quoted_ch);
						if quoted_ch == '\'' {
							sng_quoted.push_str(&format!("{}{}",quoted_ch,RESET));
							break
						}
					}
				}
				' ' | '\t' | ';' | '\n' => {
					if hl_block.trim().is_empty() && !cur_word.is_empty() {
						if KEYWORDS.contains(&cur_word.as_str()) {
							prefix = KEYWORD;
						} else if validate_cmd(&cur_word, &path) {
							prefix = COMMAND;
						}
						let formatted = format!("{}{}{}",prefix,mem::take(&mut cur_word),RESET);
						hl_block.push_str(&formatted);
					} else if !cur_word.is_empty() {
						let formatted = format!("{}{}{}",RESET,mem::take(&mut cur_word),RESET);
						hl_block.push_str(&formatted);
					}
					hl_block.push(ch);
					if matches!(ch, ';' | '\n') {
						break
					}
				}
				_ => {
					cur_word.push(ch);
				}
			}
		}

		if !cur_word.is_empty() {
			if hl_block.trim().is_empty() && !cur_word.is_empty() {
				let cmd_found = validate_cmd(&cur_word, &path);
				if cmd_found {
					prefix = COMMAND;
				}
				let formatted = format!("{}{}{}",prefix,cur_word,RESET);
				hl_block.push_str(&formatted);
			} else if !cur_word.is_empty() {
				let formatted = format!("{}{}{}",RESET,cur_word,RESET);
				hl_block.push_str(&formatted);
			}
		}

		hl_block
	}

	fn hist_substr_search(&self, term: &str, hist: &dyn History) -> Option<String> {
		let limit = hist.len();
		for i in 0..limit {
			if let Some(hist_entry) = hist.get(i, rustyline::history::SearchDirection::Forward).ok()? {
				if hist_entry.entry.starts_with(term) {
					return Some(hist_entry.entry.into_owned());
				}
			}
		}
		None
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

impl Default for OxHelper {
	fn default() -> Self {
		Self::new()
	}
}

impl Completer for OxHelper {
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
		if !line.is_empty() && (num_words > 1 || line.split(" ").into_iter().next().is_some_and(|wrd| wrd.starts_with(['.','/']))) {
			//TODO: Handle these unwraps
			let hist_path = read_vars(|vars| vars.get_evar("HIST_FILE")).unwrap().unwrap_or_else(|| -> String {
				let home = read_vars(|vars| vars.get_evar("HOME").unwrap()).unwrap();
				format!("{}/.ox_hist",home)
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
					let result = helper::slice_completion(line, &selected);
					let unfinished = line.split_whitespace().last().unwrap();
					let result = format!("{}{}",unfinished,result);
					return Ok((start, vec![result]));
				}
			}

			// Return completions, starting from the beginning of the word
			if let Some(candidate) = completions.pop() {
				let result = helper::slice_completion(line, &candidate);
				completions.push(result);
			}
			return Ok((pos, completions))
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
				let result = helper::slice_completion(line, &selected);
				return Ok((pos, vec![result]));
			}
		}
		if let Some(candidate) = completions.pop() {
			let result = helper::slice_completion(line, &candidate);
			completions.push(result);
		}
		// Return completions, starting from the beginning of the word
		Ok((pos, completions))
	}
}

pub fn skim_comp(options: Vec<String>) -> Option<String> {
	let mut stdout = stdout();
	enable_raw_mode().unwrap();

	// Get the current cursor position
	let (prompt_col, prompt_row) = cursor::position().unwrap();

	// Get terminal dimensions
	let height = options.len().min(10) as u16; // Set maximum number of options to display

	// Prepare options for skim
	let options_join = options.join("\n");
	let input = SkimItemReader::default().of_bufread(std::io::Cursor::new(options_join));

	let skim_options = SkimOptionsBuilder::default()
		.prompt("Select > ".to_string())
		.height(format!("{height}")) // Adjust height based on the options
		.multi(false)
		.build()
		.unwrap();

				// Run skim and detect if Escape was pressed
				let selected = Skim::run_with(&skim_options, Some(input))
					.and_then(|out| {
						if out.final_key == Key::ESC {
							None // Return None if Escape is pressed
						} else {
							out.selected_items.first().cloned()
						}
					})
				.map(|item| item.output().to_string());

				// Clear the rendered options after selection or cancellation
				for i in 0..height {
					execute!(
						stdout,
						MoveTo(0, prompt_row + 1 + i), // Clear each rendered line
						Clear(ClearType::CurrentLine)
					)
						.unwrap();
						}

				// Restore cursor position to where the prompt was
				execute!(
					stdout,
					MoveTo(prompt_col, prompt_row), // Restore original cursor position
					Clear(ClearType::FromCursorDown) // Clear anything below the prompt
				)
					.unwrap();

				disable_raw_mode().unwrap();

				selected
}
