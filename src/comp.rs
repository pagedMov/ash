use std::{collections::{HashMap, HashSet, VecDeque}, env, fmt::Display, io::stdout, path::Path};

use crossterm::{cursor::{self, MoveTo}, execute, style::{style, Color, Stylize}, terminal::{Clear, ClearType}};
use once_cell::sync::Lazy;
use pest::{iterators::Pair, Parser, Span};
use regex::Regex;
use rustyline::{completion::{Candidate, Completer, FilenameCompleter}, error::ReadlineError, highlight::Highlighter, hint::{Hint, Hinter}, history::History, validate::Validator, Context, Helper};
use skim::{prelude::{Key, SkimItemReader, SkimOptionsBuilder}, Skim};

use crate::{builtin::BUILTINS, helper::{self, StrExtension, StringExt}, pair::{OptPairExt, PairExt}, shellenv::{read_logic, read_vars}, LashParse, LashResult, Rule};


pub static REGEX: Lazy<HashMap<&'static str, Regex>> = Lazy::new(|| {
	let mut regex = HashMap::new();
	regex.insert("var_index", Regex::new(r"(\w+)\[(\d+)\]").unwrap());
	regex.insert("redirection",Regex::new(r"^(?P<fd_out>[0-9]+)?(?P<operator>>{1,2}|<{1,3})(?:(?:[&]?(?P<fd_target>[0-9]+))|(?P<file_target>[a-zA-Z0-9-\.]*))?$").unwrap());
	regex.insert("rsh_shebang",Regex::new(r"^#!((?:/[^\s]+)+)((?:\s+arg:[a-zA-Z][a-zA-Z0-9_\-]*)*)$").unwrap());
	regex.insert("brace_expansion",Regex::new(r"(\$?)\{(?:[\x20-\x7e,]+|[0-9]+(?:\.\.[0-9+]){1,2}|[a-z]\.\.[a-z]|[A-Z]\.\.[A-Z])\}").unwrap());
	regex.insert("process_sub",Regex::new(r"^>\(.*\)$").unwrap());
	regex.insert("command_sub",Regex::new(r"^\$\([^\)]+\)$").unwrap());
	regex.insert("arithmetic",Regex::new(r"^\$\(\([^\)]+\)\)$").unwrap());
	regex.insert("subshell",Regex::new(r"^\([^\)]+\)$").unwrap());
	regex.insert("test",Regex::new(r"^\[\s*(.*?)\s*\]$").unwrap());
	regex.insert("sng_string",Regex::new(r#"^\'([^\']*)\'$"#).unwrap());
	regex.insert("dub_string",Regex::new(r#"^\"([^\"]*)\"$"#).unwrap());
	regex.insert("var_sub",Regex::new(r"\$(?:([A-Za-z_][A-Za-z0-9_]*)|\{([A-Za-z_][A-Za-z0-9_]*)\})").unwrap());
	regex.insert("assignment",Regex::new(r"^[A-Za-z_][A-Za-z0-9_]*=.*$").unwrap());
	regex.insert("funcdef",Regex::new(r"^[\x20-\x7E]*\(\)\s+\{[\s\S]*?\}").unwrap());
	regex.insert("operator",Regex::new(r"(?:&&|\|\||[><]=?|[|&])").unwrap());
	regex.insert("cmdsep",Regex::new(r"^(?:\n|;)$").unwrap());
	regex.insert("ident",Regex::new(r"^[\x20-\x7E]*$").unwrap());
	regex.insert("find_do",Regex::new(r"(?P<loop_cond>.*?)(?P<kw>[\n;]*\s*do\s+)$").unwrap());
	regex.insert("find_done",Regex::new(r"(?P<loop_body>.*?)(?P<kw>[\n;]*\s*done(?:[\s;]*|\z))$").unwrap());
	regex.insert("ansi",Regex::new(r"\x1B\[[0-9;]*m").unwrap());
	regex
});

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

fn balance_delims(haystack: &str) -> bool {
	let open = ['{', '[', '('];
	let close = ['}', ']', ')'];
	let matches: HashMap<_, _> = close.into_iter().zip(open.into_iter()).collect();
	let mut quoted = false;

	let mut stack = vec![];
	let mut chars = haystack.chars();

	while let Some(ch) = chars.next() {
		match ch {
			'\\' => {
				chars.next(); // Skip next char (escaped character)
			}
			'{' | '(' | '[' => stack.push(ch),
			'"' | '\'' => {
				quoted = true;
				while let Some(qt_ch) = chars.next() {
					if qt_ch == ch {
						quoted = false;
						break; // End quoted string
					}
				}
			}
			'}' | ')' | ']' => {
				let pop_char = stack.pop();
				if pop_char != matches.get(&ch).copied() {
					return false; // Mismatch or extra closing delimiter
				}
			}
			_ => {}
		}
	}

	stack.is_empty() && !quoted // Ensure no leftover unclosed delimiters
}

fn balance_keywords(haystack: &str) -> bool {
	let mut stack: Vec<&str> = vec![];
	let open = ["if", "for", "while", "until", "match", "select"];
	let close = ["fi", "done", "done", "done", "done", "done"];
	let mut is_cmd = true;

	let matches: HashMap<&str, &str> = open.into_iter().zip(close.into_iter()).collect();

	let mut working_buffer = String::new();
	let mut chars = haystack.chars().peekable();

	while let Some(ch) = chars.next() {
		working_buffer.push(ch);

		// Handle escaped characters
		if ch == '\\' {
			if let Some(ch) = chars.next() {
				working_buffer.push(ch);
			}
			continue;
		}

		// Handle string literals
		if ch == '\'' || ch == '"' {
			while let Some(qt_ch) = chars.next() {
				working_buffer.push(qt_ch);
				if qt_ch == ch {
					break;
				}
			}
			continue;
		}

		// Detect new command context
		match ch {
			';' | '\n' | '(' => is_cmd = true, // '(' starts a subshell
			'|' => {
				// Check for `||` or `|`
				if matches!(chars.peek(), Some('|')) {
					let ch = chars.next().unwrap();
					working_buffer.push(ch)
				}
				is_cmd = true;
			}
			'&' => {
				// Check for `&&`
				if matches!(chars.peek(), Some('&')) {
					let ch = chars.next().unwrap();
					working_buffer.push(ch)
				}
				is_cmd = true;
			}
			' ' | '\t' => is_cmd = false,
			_ => { /* Do nothing */ }
		}

		// Handle opening keywords
		for &opener in &open {
			if is_cmd && working_buffer.ends_with(opener) {
				if matches!(chars.peek(), Some(' ' | '\t' | '\n' | ';')) {
					stack.push(opener);
				}
			}
		}

		// Handle closing keywords
		for &closer in &close {
			if is_cmd && working_buffer.ends_with(closer) {
				if matches!(chars.peek(), Some(' ' | '\t' | '\n' | ';')) {
					let opener = stack.pop();
					let expected_opener = matches.iter().find(|(_, &v)| v == closer).map(|(&k, _)| k);

					if opener != expected_opener {
						return false;
					}
				}
			}
		}
	}

	stack.is_empty() // Ensure all openers have been closed
}

#[derive(Debug,Clone)]
struct LashHighlighter {
	expect: Vec<Vec<Rule>>
}

impl LashHighlighter {
	pub fn new() -> Self {
		Self { expect: vec![] }
	}

	pub fn then_expectation() -> Vec<Rule> {
		vec![Rule::elif,Rule::r#else,Rule::fi]
	}

	pub fn expecting(&self, rule: Rule) -> bool {
		self.expect.last().is_some_and(|expect| expect.contains(&rule))
	}

	pub fn validate_cmd(&self,target: &str, path: &str) -> bool {
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

	fn style_text(&self,code: &str, text: &str) -> String {
		format!("{code}{text}{RESET}")
	}

	fn highlight_struct<'a>(&mut self,pair: Pair<'a,Rule>, mut buffer: String) -> String {
		let struct_pair = pair.into_inner().next().unwrap();
		let span = struct_pair.as_span();
		match struct_pair.as_rule() {
			Rule::r#match |
			Rule::select |
			Rule::r#for => {
				self.expect.push(vec![Rule::in_kw]);
				let kw = self.style_text(KEYWORD, struct_pair.as_str());
				buffer.replace_span(span,&kw);
				buffer
			}
			Rule::r#if | Rule::elif => {
				self.expect.push(vec![Rule::r#then]);
				let kw = self.style_text(KEYWORD, struct_pair.as_str());
				buffer.replace_span(span,&kw);
				buffer
			}
			Rule::then if self.expecting(Rule::then) => {
				self.expect.pop();
				self.expect.push(vec![Rule::elif,Rule::r#else,Rule::fi]);
				let kw = self.style_text(KEYWORD, struct_pair.as_str());
				buffer.replace_span(span,&kw);
				buffer
			}
			Rule::r#else if self.expecting(Rule::r#else) => {
				self.expect.pop();
				self.expect.push(vec![Rule::fi]);
				let kw = self.style_text(KEYWORD, struct_pair.as_str());
				buffer.replace_span(span,&kw);
				buffer
			}
			Rule::fi if self.expecting(Rule::fi) => {
				self.expect.pop();
				let kw = self.style_text(KEYWORD, struct_pair.as_str());
				buffer.replace_span(span,&kw);
				buffer
			}
			Rule::r#while |
			Rule::until => {
				self.expect.push(vec![Rule::r#do]);
				let kw = self.style_text(KEYWORD, struct_pair.as_str());
				buffer.replace_span(span,&kw);
				buffer
			}
			Rule::r#do if self.expecting(Rule::r#do) => {
				self.expect.pop();
				self.expect.push(vec![Rule::done]);
				let kw = self.style_text(KEYWORD, struct_pair.as_str());
				buffer.replace_span(span,&kw);
				buffer
			}
			Rule::done => {
				self.expect.pop();
				let kw = self.style_text(KEYWORD, struct_pair.as_str());
				buffer.replace_span(span,&kw);
				buffer
			}
			Rule::in_kw if self.expecting(Rule::in_kw) => {
				self.expect.pop();
				self.expect.push(vec![Rule::r#do]);
				let kw = self.style_text(KEYWORD, struct_pair.as_str());
				buffer.replace_span(span,&kw);
				buffer
			}
			Rule::hl_subshell => {
				let body = struct_pair.scry(Rule::subsh_body).unwrap();
				let highlighted = self.highlight_input(body.as_str()).fill_from(body.as_str());
				let sub_left = self.style_text(STRING,"(");
				let sub_right = self.style_text(STRING,")");
				let subsh = format!("{sub_left}{highlighted}{sub_right}");
				buffer.replace_span( span, &subsh);
				buffer
				}
			Rule::hl_assign => {
				let (var,val) = struct_pair.as_str().split_once('=').unwrap();
				let styled_var = self.style_text(VARSUB,var);
				let styled_val = self.style_text(STRING,val);
				let display = [styled_var,styled_val].join("=").to_string();
				buffer.replace_span(span,&display);
				buffer
				}
			Rule::for_with_in | Rule::for_with_vars | Rule::in_with_vars => {
				match struct_pair.as_rule() {
					Rule::for_with_in => self.expect.push(vec![Rule::r#do]),
					Rule::for_with_vars => self.expect.push(vec![Rule::r#in]),
					Rule::in_with_vars => self.expect.push(vec![Rule::r#do]),
					_ => unreachable!()
				}
				let mut inner = if let Rule::for_with_in = struct_pair.as_rule() {
					let for_pair = struct_pair.scry(&[Rule::for_with_vars,Rule::r#for][..]).unwrap();
					let in_pair = struct_pair.scry(&[Rule::in_with_vars,Rule::in_kw][..]).unwrap();
					let mut for_deque = for_pair.to_deque();
					let in_deque = in_pair.to_deque();
					for_deque.extend(in_deque);
					for_deque
				} else {
					struct_pair.to_deque()
				};
				while let Some(word) = inner.pop_back() {
					let span = word.as_span();
					let code = match word.as_rule() {
						Rule::var => VARSUB,
						_ => KEYWORD
					};
					let styled = self.style_text(code, word.as_str());
					buffer.replace_span( span, &styled);
				}
				buffer
			}
			Rule::func_name => {
				let stripped = struct_pair.as_str().strip_suffix("()").unwrap();
				let styled = self.style_text(FUNCNAME, stripped);
				let display = format!("{}()",styled);
				buffer.replace_span( span, &display);
				buffer
			}
			_ => buffer
		}
	}

	fn highlight_redir<'a>(&mut self, pair: Pair<'a,Rule>) -> String {
		debug_assert!(pair.as_rule() == Rule::hl_redir);
		let mut body = pair.as_str().to_string();

		let redir = LashParse::parse(Rule::hl_redir, pair.as_str()).unwrap().into_iter().next().unwrap();
		let mut inner = redir.into_inner().rev();
		while let Some(part) = inner.next() {
			let span = part.as_span();
			match part.as_rule() {
				Rule::file => { /* Don't highlight it */ }
				Rule::fd_out | Rule::fd_target => {
					// Validate fd
					let fd = part.as_str();
					let fd_path = format!("/proc/self/fd/{fd}");
					let exists = Path::new(&fd_path).exists();
					let styled = if exists {
						self.style_text(COMMAND, fd)
					} else {
						self.style_text(ERROR, fd)
					};
					body.replace_span(span, &styled)
				}
				_ => {
					let styled = self.style_text(OPERATOR, part.as_str());
					body.replace_span(span, &styled)
				}
			}
		}
		body
	}

	fn highlight_dquote<'a>(&mut self,pair: Pair<'a,Rule>) -> String {
		debug_assert!(pair.as_rule() == Rule::dquoted);

		let body = pair.scry(Rule::dquote_body).unwrap().as_str();
		let sub_parse = LashParse::parse(Rule::syntax_hl, body);
		if let Ok(parse) = sub_parse {
			let mut buffer = body.to_string();
			let mut words = parse.into_iter().next().unwrap().to_deque();
			while let Some(word) = words.pop_back() {
				if word.as_rule() == Rule::words {
					let mut inner_words = word.into_inner().collect::<VecDeque<_>>();
					while let Some(in_wd) = inner_words.pop_back() {
						let span = in_wd.as_span();
						if in_wd.clone().step(1).is_some() {
							let wd_type = in_wd.step(1).unwrap();
							match wd_type.as_rule() {
								Rule::cmd_sub => {
									let body = wd_type.as_str().trim_start_matches("$(").trim_end_matches(')');
									let highlighted = self.highlight_input(body).fill_from(body);
									let sub_left = self.style_text(STRING,"$(");
									let sub_right = format!("{}{}",STRING,")");
									let cmd_sub = format!("{sub_left}{highlighted}{sub_right}");
									buffer.replace_span(span, &cmd_sub);
								}
								Rule::var_sub | Rule::param_sub => {
									let word = wd_type.as_str();
									let styled = format!("{}{}{}",VARSUB,word,STRING);
									buffer.replace_span(span, &styled);
								}
								_ => { /* Do nothing */ }
							}
						}
					}
				}
			}
			format!("\"{}{}{}\"",STRING,buffer,RESET)
		} else {
			pair.as_str().to_string()
		}
	}

	fn highlight_words<'a>(&mut self,pair: Pair<'a,Rule>, mut buffer: String, path: &str) -> String {
		let mut is_cmd = true;
		let mut words = pair.to_deque();
		while let Some(word_pair) = words.pop_back() {
			if word_pair.as_rule() == Rule::hl_redir {
				let span = word_pair.as_span();
				let styled = self.highlight_redir(word_pair);
				buffer.replace_span(span,&styled);
				continue
			}

			let sub_type = word_pair.clone().step(1);
			if sub_type.clone().is_some_and(|pr| pr.as_rule() != Rule::tilde_sub) {
				let sub_type = sub_type.unwrap();
				let span = sub_type.as_span();
				match sub_type.as_rule() {
					Rule::loud_ident => { /* Pass */ }
					Rule::dquoted => {
						let styled = self.highlight_dquote(sub_type);
						buffer.replace_span(span,&styled);
					}
					Rule::hl_redir => {
						let styled = self.highlight_redir(sub_type);
						buffer.replace_span(span,&styled);
					}
					Rule::squoted => {
						let body = sub_type.as_str().trim_matches('\'');
						let styled = self.style_text(STRING,body);
						let squoted = format!("{}{}{}",'\'',styled,'\'');
						buffer.replace_span(span,&squoted);
					}
					Rule::param_sub | Rule::var_sub => {
						let word = sub_type.as_str();
						let styled = self.style_text(VARSUB,word);
						buffer.replace_span(span,&styled);
					}
					Rule::arr_index => {
						// If it works, it works
						let (left,right) = sub_type.as_str().split_once('[').unwrap();
						let styled_name = self.style_text(VARSUB,&left);
						let styled = [styled_name,right.to_string()].join("[").to_string();
						buffer.replace_span(span,&styled);
					}
					Rule::cmd_sub => {
						let body = sub_type.as_str().trim_start_matches("$(").trim_end_matches(')');
						let highlighted = self.highlight_input(body).fill_from(body);
						let sub_left = self.style_text(STRING,"$(");
						let sub_right = self.style_text(STRING,")");
						let cmd_sub = format!("{sub_left}{highlighted}{sub_right}");
						buffer.replace_span(span, &cmd_sub);
					}
					Rule::proc_sub => {
						let body = sub_type.as_str().trim_start_matches(">(").trim_start_matches("<(").trim_end_matches(')');
						let highlighted = self.highlight_input(body).fill_from(body);
						let sub_left = if sub_type.as_str().starts_with("<(") {
							self.style_text(STRING,"<(")
						} else {
							self.style_text(STRING,">(")
						};
						let sub_right = self.style_text(STRING,")");
						let proc_sub = format!("{sub_left}{highlighted}{sub_right}");
						buffer.replace_span(span, &proc_sub);
					}
					Rule::hl_glob => {
						let glob_kind = sub_type.scry(Rule::hl_globs).unwrap().step(1).unwrap();
						let glob_span = glob_kind.as_span();
						match glob_kind.as_rule() {
							Rule::glob_opt | Rule::glob_wild => {
								let styled = self.style_text(KEYWORD,glob_kind.as_str());
								buffer.replace_span(glob_span, &styled);
							}
							Rule::glob_brackets => {
								let body = glob_kind.as_str().trim_matches(['[',']']);
								let left_brack = format!("{}{}{}",KEYWORD,'[',RESET);
								let right_brack = format!("{}{}{}",KEYWORD,']',RESET);
								let rebuilt = format!("{left_brack}{body}{right_brack}");
								buffer.replace_span(glob_span,&rebuilt);
							}
							_ => unreachable!("Unexpected rule: {:?}",sub_type.as_rule())
						}
					}
					Rule::hl_brace_word => {
						let body = sub_type.scry(Rule::brace_expand).unwrap().as_str().trim_matches(['{','}']);
						let left_brace = format!("{}{}{}",KEYWORD,'{',RESET);
						let right_brace = format!("{}{}{}",KEYWORD,'}',RESET);
						let rebuilt = format!("{left_brace}{body}{right_brace}");
						buffer.replace_span(span,&rebuilt);
					}
					_ => unreachable!("Unexpected rule: {:?}",sub_type.as_rule())
				}
			} else {
				let word = word_pair.as_str();
				let span = word_pair.as_span();
				if words.is_empty() {
					let code = if self.validate_cmd(word, path) {
						COMMAND
					} else {
						ERROR
					};
					let styled_word = self.style_text(code, word);
					buffer.replace_span(span, &styled_word);
				} else {
					let code = RESET;
					let styled_word = self.style_text(code, word);
					buffer.replace_span(span, &styled_word);
				}

			}
		}
		buffer.to_string()
	}

	fn highlight_pair<'a>(&mut self,pair: Pair<'a,Rule>, mut buffer: String) -> String {
		let path = env::var("PATH").unwrap_or_default();
		let span = pair.as_span();
		match pair.as_rule() {
			Rule::loud_sep => {
				let hl = self.style_text(RESET, &pair.as_str());
				buffer.replace_span(span, &hl)
			}
			Rule::loud_operator => {
				let hl = self.style_text(OPERATOR, &pair.as_str());
				buffer.replace_span( span, &hl)
			}
			Rule::words => buffer = self.highlight_words(pair, buffer, &path),
			Rule::shell_struct => buffer = self.highlight_struct(pair, buffer),
			_ => unreachable!("Reached highlight pair with this unexpected rule: {:?}",pair.as_rule())
		}
		buffer
	}

	fn highlight_input(&mut self,input: &str) -> String {
		let parsed_input = LashParse::parse(Rule::syntax_hl, input);
		match parsed_input {
			Ok(parsed_input) => {
				let mut buffer = parsed_input.as_str().to_string().fill_from(input);
				let mut inner = parsed_input.into_iter().next().unwrap().to_vec();
				while let Some(pair) = inner.pop() {
					buffer = self.highlight_pair(pair, buffer);
				}
				buffer
			}
			Err(_) => {
				input.to_string()
			}
		}
	}
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

#[derive(Helper)]
pub struct LashHelper {
	filename_comp: FilenameCompleter,
	highlighter: LashHighlighter,
	commands: Vec<String>
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

impl Highlighter for LashHelper {
	fn highlight<'l>(&self, line: &'l str, pos: usize) -> std::borrow::Cow<'l, str> {
		let _ = pos;
		let mut highlighter = self.highlighter.clone();
		std::borrow::Cow::Owned(highlighter.highlight_input(line))
	}

	fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
		&'s self,
		prompt: &'p str,
		default: bool,
	) -> std::borrow::Cow<'b, str> {
		let _ = default;
		std::borrow::Cow::Borrowed(prompt)
	}

	fn highlight_hint<'h>(&self, hint: &'h str) -> std::borrow::Cow<'h, str> {
		std::borrow::Cow::Borrowed(hint)
	}

	fn highlight_candidate<'c>(
		&self,
		candidate: &'c str, // FIXME should be Completer::Candidate
		completion: rustyline::CompletionType,
	) -> std::borrow::Cow<'c, str> {
		let _ = completion;
		std::borrow::Cow::Borrowed(candidate)
	}

	fn highlight_char(&self, line: &str, pos: usize, kind: rustyline::highlight::CmdKind) -> bool {
		let _ = (line, pos, kind);
		true
	}
}

impl Validator for LashHelper {
	fn validate(&self, ctx: &mut rustyline::validate::ValidationContext) -> rustyline::Result<rustyline::validate::ValidationResult> {
	    let input = ctx.input();

			match balance_delims(input) && balance_keywords(input) {
				true => Ok(rustyline::validate::ValidationResult::Valid(None)),
				false => Ok(rustyline::validate::ValidationResult::Incomplete),
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
			highlighter: LashHighlighter::new(),
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
