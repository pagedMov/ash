use bitflags::bitflags;
use crossterm::event;
use nix::NixPath;
use once_cell::sync::Lazy;
use regex::Regex;
use std::borrow::Borrow;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::mem::take;

use crate::event::ShErrorFull;
use crate::interp::expand::expand_cmd_sub;
use crate::interp::parse::Span;
use crate::event::ShError;
use crate::shellenv::read_logic;
use crate::shellenv::read_meta;
use crate::shellenv::read_vars;
use crate::shellenv::write_meta;
use crate::OxResult;
use crate::builtin::BUILTINS;

use super::expand;
use super::helper;
use super::helper::StrExtension;

pub static REGEX: Lazy<HashMap<&'static str, Regex>> = Lazy::new(|| {
	let mut regex = HashMap::new();
	regex.insert("var_index", Regex::new(r"(\w+)\[(\d+)\]").unwrap());
	regex.insert("redirection",Regex::new(r"^(?P<fd_out>[0-9]+)?(?P<operator>>{1,2}|<{1,3})(?:[&]?(?P<fd_target>[0-9]+))?$").unwrap());
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
	regex
});

pub const KEYWORDS: [&str;14] = [
	"if", "while", "until", "for", "case", "select",
	"then", "elif", "else", "in",
	"do", "done", "fi", "esac"
];
pub const OPENERS: [&str;6] = [
	"if", "while", "until", "for", "case", "select",
];
pub const CMDSEP: [char;2] = [
	';', '\n'
];
pub const WHITESPACE: [char;2] = [
	' ', '\t'
];

bitflags! {
	#[derive(Debug,Clone,Copy,PartialEq,Eq)]
	pub struct FnFlags: u32 {
		const RECURSE = 0b0001;
	}

	#[derive(Debug,Hash,Clone,Copy,PartialEq,Eq)]
	pub struct WdFlags: u32 {
		const KEYWORD =    0b000000000000000001;
		const BUILTIN =    0b000000000000000010;
		const FUNCTION =   0b000000000000000100;
		const ALIAS =      0b000000000000001000;
		const IS_ARG =     0b000000000000010000;
		const DUB_QUOTED = 0b000000000000100000;
		const SNG_QUOTED = 0b000000000001000000;
		const IN_BRACE =   0b000000000010000000;
		const IN_PAREN =   0b000000000100000000;
		const IS_SUB =     0b000000001000000000;
		const IS_OP =      0b000000010000000000;
		const EXPECT_IN =  0b000000100000000000;
		const IS_PATH =    0b000001000000000000;
		const FROM_ALIAS = 0b000010000000000000;
		const FROM_FUNC  = 0b000100000000000000;
		const FROM_VAR   = 0b001000000000000000;
	}
}

#[derive(Debug,Hash,Clone,PartialEq,Eq)]
pub struct WordDesc {
	pub text: String,
	pub span: Span,
	pub flags: WdFlags
	}

impl WordDesc {
	pub fn empty() -> Self {
		Self {
			text: String::new(),
			span: Span::new(),
			flags: WdFlags::empty()
		}
	}
	pub fn set_span(self, span: Span) -> Self {
		Self {
			text: self.text,
			span,
			flags: self.flags
		}
	}
	pub fn add_char(&mut self, ch: char) -> Self {
		let mut text = std::mem::take(&mut self.text);
		text.push(ch);

		Self {
			text,
			span: Span::from(self.span.start, self.span.end),
			flags: self.flags,
		}
	}
	pub fn cat_string(mut self, s: &str) -> Self {
		self.text.push_str(s);
		Self {
			text: self.text,
			span: Span::from(self.span.start, self.span.end + 1),
			flags: self.flags
		}
	}
	pub fn contains_flag(&self, flag: WdFlags) -> bool {
		self.flags.contains(flag)
	}

	pub fn add_flag(self, flag: WdFlags) -> Self {
		let mut flags = self.flags;
		flags |= flag;
		Self {
			text: self.text,
			span: self.span,
			flags
		}
	}

	pub fn remove_flag(self, flag: WdFlags) -> Self {
		let mut flags = self.flags;
		flags &= !flag;
		Self {
			text: self.text,
			span: self.span,
			flags
		}
	}

	pub fn toggle_flag(self, flag: WdFlags) -> Self {
		let mut flags = self.flags;
		flags ^= flag;
		Self {
			text: self.text,
			span: self.span,
			flags
		}
	}
	pub fn reset_flags(self) -> Self {
		Self {
			text: self.text,
			span: self.span,
			flags: WdFlags::empty()
		}
	}
	pub fn push_span(&mut self, count: usize) {
		self.span = Span::from(self.span.start,self.span.end + count);
	}
	pub fn push_span_start(&mut self, count: usize) {
		let mut span_start = self.span.start;
		let mut span_end = self.span.end;
		span_start += count;
		if span_end < span_start {
			span_end = span_start
		}
		self.span = Span::from(span_start,span_end);
	}
	pub fn delimit(self, delim: char) -> Self {
		let flag = match delim {
			'{' => WdFlags::IN_BRACE,
			'(' => WdFlags::IN_PAREN,
			_ => unreachable!()
		};
		self.add_flag(flag)
	}
	}

impl Default for WordDesc {
	fn default() -> Self {
		Self::empty()
	}
}

#[derive(Debug,Hash,Clone,PartialEq,Eq)]
pub struct Tk {
	pub tk_type: TkType,
	pub wd: WordDesc
}

#[derive(Debug,Hash,Clone,PartialEq,Eq)]
pub enum TkType {
	// Control Flow Keywords
	If,
	Then,
	Else,
	Elif,
	Fi,
	For,
	While,
	Until,
	Do,
	Done,
	Case,
	Esac,
	Select,
	In,
	FuncBody,

	Redirection { redir: Redir },
	FuncDef,
	Assignment { key: String, value: Box<Tk> }, // `=`
	LogicAnd, // `&&`
	LogicOr, // `||`
	Pipe, // `|`
	PipeBoth, // '|&'
	Background, // `&`

	// Grouping and Subshells
	ProcessSub,
	Subshell, // `(`
	Array { elements: Vec<Tk> },
	Vars { vars: Vec<Tk> },

	// Strings and Identifiers
	String, // Generic string literal
	Ident,  // Identifier for variables, functions, etc.
	Expanded, // Token from an expansion

	// Expansions
	BraceExpansion,
	VariableSub, // `$var`, `${var}`
	CommandSub, // `$(command)`

	// Comments
	Comment, // `#`

	// Special Characters
	Cmdsep, // ';' or '\n'
	CasePat,
	Whitespace, // Space or tab
	SOI,
	EOI,
	Null, // Default

}

impl Tk {
	pub fn new(text: String, span: Span, flags: WdFlags) -> Self {
		Self {
			tk_type: TkType::String,
			wd: WordDesc {
				text,
				span,
				flags
			}
		}
	}
	pub fn from(wd: WordDesc, context: TkState) -> OxResult<Self> {
		use crate::interp::token::TkState::*;
		use crate::interp::token::TkType as TkT;
		match context {
			Root => panic!("not supposed to be here"),
			Ident => Ok(Tk { tk_type: TkT::Ident, wd }),
			Arg => Ok(Tk { tk_type: TkT::Ident, wd: wd.add_flag(WdFlags::IS_ARG) }),
			Command => Ok(Tk { tk_type: TkT::Ident, wd }),
			Array => Ok(Tk { tk_type: TkT::Array { elements: vec![] }, wd }),
			If => Ok(Tk { tk_type: TkT::If, wd: wd.add_flag(WdFlags::KEYWORD) }),
			For => Ok(Tk { tk_type: TkT::For, wd: wd.add_flag(WdFlags::KEYWORD) }),
			Loop => Ok({
				if wd.text == "while" {
					Tk { tk_type: TkT::While, wd: wd.add_flag(WdFlags::KEYWORD) }
				} else if wd.text == "until" {
					Tk { tk_type: TkT::Until, wd: wd.add_flag(WdFlags::KEYWORD) }
				} else { unreachable!("reached loop context with this: {}",wd.text) }
			}),
			Case => Ok(Tk { tk_type: TkT::Case, wd: wd.add_flag(WdFlags::KEYWORD) }),
			Select => Ok(Tk { tk_type: TkT::Select, wd: wd.add_flag(WdFlags::KEYWORD) }),
			In | CaseIn => Ok(Tk { tk_type: TkT::In, wd: wd.add_flag(WdFlags::KEYWORD) }),
			CasePat => Ok(Tk { tk_type: TkT::Ident, wd }),
			Elif => Ok(Tk { tk_type: TkT::Elif, wd: wd.add_flag(WdFlags::KEYWORD) }),
			Else => Ok(Tk { tk_type: TkT::Else, wd: wd.add_flag(WdFlags::KEYWORD) }),
			Do => Ok(Tk { tk_type: TkT::Do, wd: wd.add_flag(WdFlags::KEYWORD) }),
			Then => Ok(Tk { tk_type: TkT::Then, wd: wd.add_flag(WdFlags::KEYWORD) }),
			Done => Ok(Tk { tk_type: TkT::Done, wd: wd.add_flag(WdFlags::KEYWORD) }),
			Fi => Ok(Tk { tk_type: TkT::Fi, wd: wd.add_flag(WdFlags::KEYWORD) }),
			Esac => Ok(Tk { tk_type: TkT::Esac, wd: wd.add_flag(WdFlags::KEYWORD) }),
			Subshell => Ok(Tk { tk_type: TkT::Subshell, wd }),
			SQuote => Ok(Tk { tk_type: TkT::String, wd }),
			DQuote => Ok(Tk { tk_type: TkT::String, wd }),
			Redirect => todo!(),
			CommandSub => Ok(Tk { tk_type: TkT::CommandSub, wd }),
			Separator => Ok(Tk { tk_type: TkT::Cmdsep, wd }),
			_ => Err(ShError::from_parse(format!("Parse error: {}", wd.text).as_str(), wd.span))
		}
	}
	pub fn start_of_input() -> Self {
		Tk {
			tk_type: TkType::SOI,
			wd: WordDesc { text: "".into(), span: Span::from(0,0), flags: WdFlags::empty() }
		}
	}
	pub fn end_of_input(end: usize) -> Self {
		Tk {
			tk_type: TkType::EOI,
			wd: WordDesc { text: "".into(), span: Span::from(end,end), flags: WdFlags::empty() }
		}
	}
	pub fn cmdsep(wd: &WordDesc, pos: usize) -> Self {
		Tk {
			tk_type: TkType::Cmdsep,
			wd: WordDesc { text: wd.text.clone(), span: Span::from(pos,pos), flags: WdFlags::empty() }
		}
	}
	pub fn pipe(wd: &WordDesc, pos: usize) -> Self {
		Tk {
			tk_type: TkType::Pipe,
			wd: WordDesc { text: wd.text.clone(), span: Span::from(pos,pos), flags: WdFlags::IS_OP }
		}
	}
	pub fn bg(wd: &WordDesc, pos: usize) -> Self {
		Tk {
			tk_type: TkType::Pipe,
			wd: WordDesc { text: wd.text.clone(), span: Span::from(pos,pos), flags: WdFlags::IS_OP }
		}
	}
	pub fn and(wd: &WordDesc, pos: usize) -> Self {
		Tk {
			tk_type: TkType::Pipe,
			wd: WordDesc { text: wd.text.clone(), span: Span::from(pos,pos + 1), flags: WdFlags::IS_OP }
		}
	}
	pub fn or(wd: &WordDesc, pos: usize) -> Self {
		Tk {
			tk_type: TkType::Pipe,
			wd: WordDesc { text: wd.text.clone(), span: Span::from(pos,pos + 1), flags: WdFlags::IS_OP }
		}
	}
	pub fn text(&self) -> &str {
		self.wd.text.as_str()
	}
	pub fn span(&self) -> Span {
		self.wd.span
	}
	pub fn class(&self) -> TkType {
		self.tk_type.clone()
	}
	pub fn flags(&self) -> WdFlags {
		self.wd.flags
	}
}

impl Default for Tk {
	fn default() -> Self {
		Self {
			tk_type: TkType::Null,
			wd: WordDesc::empty()
		}
	}
}


#[derive(Debug,Hash,Clone,PartialEq,Eq)]
pub struct Redir {
	pub fd_source: i32,
	pub op: RedirType,
	pub fd_target: Option<i32>,
	pub file_target: Option<Box<Tk>>
}

#[derive(Debug,Hash,Clone,PartialEq,Eq)]
pub enum RedirType {
	Output,
	Append,
	Input,
	Herestring
}

// Contexts for the state machine
// Some of these aren't used
#[derive(Eq,Debug,Hash,PartialEq)]
pub enum TkState {
	Root, // Outer contex, allows for anything and everything. Closed by the EOI token
	ArrDec, // Used in arrays like for i in ArrDec1 ArrDec2
	VarDec, // Used in for loop vars like for VarDec1 VarDec2 in array
	SingleVarDec, // Used in case and select
	Ident, // Generic words used for var and arr declarations
	Arg, // Command arguments; only appear after commands
	Command, // Starting point for the tokenizer
	FuncDef, // Function names
	FuncBody, // Function bodies
	Array, // Used in for loops and select statements
	If, // If statement opener
	For, // For loop opener
	Loop, // While/Until opener
	Case, // Case opener
	Select, // Select opener
	In, // Used in for, case, and select statements
	CaseBlock, // this)kind of thing;;
	CaseIn, // 'In' context used for case statements, signaling the tokenizer to look for stuff 'like)this;;esac'
	CasePat, // the left side of this)kind of thing
	CaseBody, // the right side of this)kind of thing
	Elif, // Secondary if/then blocks
	Else, // Else statements
	Do, // Select, for, and while/until condition/body separator
	Then, // If statement condition/body separator
	Done, // Select, for, and while/until closer
	Fi, // If statement closer
	Esac, // Case statement closer
	Subshell, // Subshells, look (like this)
	SQuote, // 'Single quoted strings'
	DQuote, // "Double quoted strings"
	Escaped, // Used to denote an escaped character like \a
	Redirect, // >, <, <<, <<<, 1>&2, 2>, etc.
	Comment, // #Comments like this
	Whitespace, // Space or tabs
	CommandSub, // $(Command substitution)
	Operator, // operators
	Separator, // Semicolon or newline to end an invocation
	DeadEnd, // Used for closing keywords like 'fi' and 'done' that demand a separator immediately after
	Invalid // Used when an unexpected state is discovered
}

impl TkState {
	pub fn check_str(slice: &str, context: TkState) -> Self {
		use crate::interp::token::TkState::*;
		match slice {
			// Loop and conditional openers
			"for" if matches!(context, Command) => For,
			"if" if matches!(context, Command) => If,
			"while" if matches!(context, Command) => Loop,
			"until" if matches!(context, Command) => Loop,

			// Conditional separators and terminators
			"then" if matches!(context, If | Elif) => Then,
			"elif" if matches!(context, Then | Elif) => Elif,
			"else" if matches!(context, If | Then | Elif) => Else,
			"fi" if matches!(context, If | Then | Elif | Else) => Fi,

			// Loop terminators
			"do" if matches!(context, For | Loop) => Do,
			"done" if matches!(context, Do) => Done,

			// Case-specific keywords
			"case" if matches!(context, Command) => Case,
			"in" if matches!(context, Case | For | Select) => In,
			"esac" if matches!(context, CaseIn | CaseBlock | CaseBody) => Esac,

			// Select-specific keywords
			"select" if matches!(context, Command) => Select,

			// General flow control
			"in" if matches!(context, For | Case | Select) => In,
			"|" if Self::executable().contains(&context) => Operator,
			"&" if Self::executable().contains(&context) => Operator,

			// Defaults to Ident for non-keyword or unmatched cases
			_ => Ident,
		}
	}
	pub fn executable() -> Vec<Self> {
		use crate::interp::token::TkState::*;
		let mut execs = vec![
			Command,
			Subshell
		];
		execs.extend(Self::openers());
		execs
	}
	pub fn body() -> Vec<Self> {
		use crate::interp::token::TkState::*;
		vec![
			Command,
			Arg,
			Separator,
			Redirect,
			SQuote,
			DQuote,
			CommandSub,
			Subshell
		]
	}
	pub fn openers() -> Vec<Self> {
		use crate::interp::token::TkState::*;
		vec![
			For,
			Loop,
			If,
			Case,
			Select
		]
	}
}

#[derive(Debug)]
pub struct OxTokenizer {
	input: String,
	char_stream: VecDeque<char>,
	context: Vec<TkState>,
	expanded: bool,
	pub tokens: Vec<Tk>,
	pub spans: VecDeque<Span>
}

impl OxTokenizer {
	pub fn new(input: &str) -> Self {
		let mut input = input.trim().to_string();
		let char_stream = input.chars().collect::<VecDeque<char>>();
		let tokens = vec![Tk { tk_type: TkType::SOI, wd: WordDesc::empty() }];
		if input.starts_with("#!") { // Ignore shebangs
			let mut lines = input.lines();
			lines.next();
			input = lines.collect::<Vec<&str>>().join("\n");
		}
		Self { input, char_stream, context: vec![TkState::Command], expanded: false, tokens, spans: VecDeque::new() }
	}
	pub fn input(&self) -> String {
		self.input.clone()
	}
	fn push_ctx(&mut self, ctx: TkState) {
		self.context.push(ctx)
	}
	fn pop_ctx(&mut self) {
		if self.context.len() > 1 {
			self.context.pop();
		}
	}
	fn ctx(&self) -> &TkState {
		self.context.last().unwrap_or(&TkState::Command)
	}
	#[track_caller]
	fn advance(&mut self) -> Option<char> {
		self.char_stream.pop_front()
	}
	fn split_input(&self) -> Vec<String> {
		/*
		 * Used to split raw input during expansion
		 * TODO: find some way to combine this logic with complete_word()
		 */
		let input = &self.input;
		let mut words = vec![];
		let mut current_word = String::new();
		let mut double_quote = false;
		let mut single_quote = false;
		let mut paren_stack = vec![];
		let mut bracket_stack = vec![];
		let mut brace_stack = vec![];
		let mut chars = input.chars().peekable();

		while let Some(ch) = chars.next() {
			match ch {
				'\\' => {
					// Handle escape sequences
					if let Some(next) = chars.next() {
						current_word.push(ch); // Include the backslash
						current_word.push(next);
					}
				}
				'"' if !single_quote => {
					// Toggle double-quote state
					double_quote = !double_quote;
					current_word.push(ch);
				}
				'\'' if !double_quote => {
					// Toggle single-quote state
					single_quote = !single_quote;
					current_word.push(ch);
				}
				'(' if !single_quote && !double_quote && brace_stack.is_empty() && bracket_stack.is_empty() => {
					// Open parenthesis
					paren_stack.push(ch);
					current_word.push(ch);
				}
				')' if !single_quote && !double_quote && !paren_stack.is_empty() => {
					// Close parenthesis
					paren_stack.pop();
					current_word.push(ch);
				}
				'{' if !single_quote && !double_quote && paren_stack.is_empty() && bracket_stack.is_empty() => {
					// Open bracket
					brace_stack.push(ch);
					current_word.push(ch);
				}
				'}' if !single_quote && !double_quote && !brace_stack.is_empty() => {
					// Close bracket
					brace_stack.pop();
					current_word.push(ch);
				}
				'[' if !single_quote && !double_quote && paren_stack.is_empty() && brace_stack.is_empty() => {
					// Open bracket
					bracket_stack.push(ch);
					current_word.push(ch);
				}
				']' if !single_quote && !double_quote && !bracket_stack.is_empty() => {
					// Close bracket
					bracket_stack.pop();
					current_word.push(ch);
				}
				' ' | '\t' if !double_quote && !single_quote && paren_stack.is_empty() && bracket_stack.is_empty() && brace_stack.is_empty() => {
					// End of word (outside quotes, parentheses, and brackets)
					current_word.push(ch);
					if !current_word.is_empty() {
						words.push(current_word);
						current_word = String::new();
					}
				}
				'\n' | ';' if !double_quote && !single_quote && paren_stack.is_empty() && bracket_stack.is_empty() && brace_stack.is_empty() => {
					// End of word and standalone separator (outside quotes, parentheses, and brackets)
					current_word.push(ch);
					words.push(current_word);
					current_word = String::new();
				}
				_ => {
					// Regular character or inside quotes, parentheses, or brackets
					current_word.push(ch);
				}
			}
		}

		if !current_word.is_empty() {
			words.push(current_word);
		}
		words
	}

	// First pass over input: expand aliases
	fn alias_pass(&mut self) -> OxResult<()> {
		let aliases = read_logic(|m| m.borrow_aliases().clone())?;
		let mut replaced = true;
		let mut is_command = true;

		while replaced {
			replaced = false;
			let mut words = self.split_input();
			let mut words = words.iter_mut().peekable();
			let mut new_input = String::new();

			while let Some(word) = words.next() {
				let is_last_word = words.peek().is_none();
				if is_command {
					if let Some(alias) = aliases.get(word.trim()) {
						*word = alias.clone();
						replaced = true;
					}
					is_command = false;
				}
				if word.ends_with(';') || word.ends_with('\n') {
					is_command = true;
				}
				if is_last_word && !word.trim().is_empty() {
					new_input.push_str(word);
				} else if !word.trim().is_empty() {
					new_input.push_str(format!("{} ",word).as_str());
				}
			}

			self.input = new_input;
		}

		self.char_stream = self.input.chars().collect::<VecDeque<char>>();

		Ok(())
	}
	// Second pass over input: expand variables
	fn var_pass(&mut self) -> OxResult<()> {
		let vartable = read_vars(|v| v.clone())?;
		let mut replaced = true;

		while replaced {
			replaced = false;
			let words = self.split_input();
			let mut new_input = String::new();

			for mut word in words {
				if word.starts_with('$') {
					let var_candidate = word.strip_prefix('$').unwrap();
					if let Some(var) = vartable.get_var(var_candidate) {
						word = var.to_string();
						replaced = true;
					}
				}
				if !word.trim().is_empty() { new_input.push_str(&word); }
			}

			self.input = new_input;
		}

		self.char_stream = self.input.chars().collect::<VecDeque<char>>();

		Ok(())
	}
	// Third pass over input: expand command subs
	fn cmd_sub_pass(&mut self) -> OxResult<()> {
		let mut replaced = true;

		while replaced {
			replaced = false;
			let words = self.split_input();
			let mut new_input = String::new();

			for mut word in words {
				let not_sng_quote = !(word.starts_with('\'') || word.ends_with('\''));
				let not_brace_grp = !(word.starts_with('{') || word.ends_with('}'));

				// Ensure expansion only runs when the word is not enclosed in single quotes or braces
				if helper::has_valid_delims(&word, "$(", ")") && not_sng_quote && not_brace_grp {
					if let Some((left, middle, right)) = word.split_twice("$(", ")") {
						let dummy_tk = Tk {
							tk_type: TkType::CommandSub,
							wd: WordDesc {
								text: middle.to_string(),
								span: Span::new(),
								flags: WdFlags::empty(),
							},
						};
						let new_tk = expand::expand_cmd_sub(dummy_tk)?;
						word = format!("{}{}{}", left, new_tk.text(), right);
						replaced = true;
					}
				}
				if !word.trim().is_empty() { new_input.push_str(&word); }
			}

			self.input = new_input;
		}

		self.char_stream = self.input.chars().collect::<VecDeque<char>>();

		Ok(())
	}
	fn precompute_spans(&mut self) {
		let is_sep = |ch: char| -> bool { matches!(ch, ';' | '\n') };
		let chars = self.input.chars().enumerate().peekable(); // Note: Still `peekable`, but unused for now.
		let mut spans = vec![];
		let mut working_span: Option<Span> = None;

		for (i, ch) in chars {

			if ch.is_whitespace() || is_sep(ch) {
				// Finalize and push the current span if it exists
				if let Some(mut span) = working_span.take() {
					span.end = i; // Finalize span *before* the current whitespace/separator
					spans.push(span);
				}
			} else {
				// Start a new span or extend the current one
				if working_span.is_none() {
					working_span = Some(Span::from(i,i)); // Start a new span
				} else if let Some(ref mut span) = working_span {
					span.end += 1; // Extend the span
				}
			}
		}

		// Push the last span if it exists
		if let Some(mut span) = working_span {
			span.end = self.input.len();
			spans.push(span);
		}

		self.spans = VecDeque::from(spans);
	}
	pub fn tokenize_one(&mut self) -> OxResult<Vec<Tk>> {
		use crate::interp::token::TkState::*;
		if !self.expanded { // Replacing these first is the simplest way
			self.alias_pass()?;
			self.var_pass()?;
			self.cmd_sub_pass()?;
			self.precompute_spans();
			write_meta(|m| m.set_last_input(&self.input))?;
			self.expanded = true;
		}
		let mut wd = WordDesc::empty();
		while !self.char_stream.is_empty() {
			if *self.ctx() == DeadEnd && self.char_stream.front().is_some_and(|ch| !matches!(ch, ';' | '\n')) {
				return Err(ShError::from_parse("Expected a semicolon or newline here", wd.span))
			}
			match self.char_stream.front().unwrap() {
				'\\' => {
					let escape = self.advance().unwrap();
					wd = wd.add_char(escape);
					let escaped_char = self.advance();
					if let Some(ch) = escaped_char {
						wd = wd.add_char(ch)
					}
				}
				';' | '\n' => {
					self.advance();
					self.tokens.push(Tk::cmdsep(&wd, wd.span.end));
					if *self.ctx() == DeadEnd {
						while self.context.len() != 1 {
							if !matches!(*self.ctx(), For | FuncDef | Loop | If | Case | Select) {
								self.pop_ctx();
							} else {
								self.pop_ctx();
								break
							}
						}
					}
					if *self.ctx() != Command {
						self.pop_ctx();
					}
					if self.context.len() == 1 {
						break
					}
				}
				'#' => self.push_ctx(Comment),
				'(' if *self.ctx() == Command => self.push_ctx(Subshell),
				'{' if *self.ctx() == FuncDef => self.push_ctx(FuncBody),
				'$' if matches!(*self.ctx(), Command | Arg) => {
					let dollar = self.advance().unwrap();
					if self.char_stream.front().is_some_and(|ch| *ch == '(') {
						self.push_ctx(CommandSub)
					}
					self.char_stream.push_front(dollar);
				}
				'|' | '&' if matches!(*self.ctx(),Command | Arg) => {
					if *self.ctx() == Arg {
						self.pop_ctx();
					}
					let op = self.advance().unwrap();
					match op {
						'|' => {
							if self.char_stream.front().is_some_and(|ch| *ch == '|') {
								self.advance();
								self.tokens.push(Tk::or(&wd, wd.span.end))
							} else {
								self.tokens.push(Tk::pipe(&wd, wd.span.end))
							}
						}
						'&' => {
							if self.char_stream.front().is_some_and(|ch| *ch == '&') {
								self.advance();
								self.tokens.push(Tk::and(&wd, wd.span.end))
							} else {
								self.tokens.push(Tk::bg(&wd, wd.span.end))
							}
						}
						_ => unreachable!()
					}
					continue
				}
				' ' | '\t' => {
					self.advance();
					continue
				}
				_ => { /* Do nothing */ }
			}
			match *self.ctx() {
				Command => self.command_context(take(&mut wd))?,
				Arg => self.arg_context(take(&mut wd))?,
				DQuote | SQuote => self.string_context(take(&mut wd))?,
				VarDec | SingleVarDec => self.vardec_context(take(&mut wd))?,
				ArrDec => self.arrdec_context(take(&mut wd))?,
				Subshell | CommandSub => self.subshell_context(take(&mut wd))?,
				FuncBody => self.func_context(take(&mut wd))?,
				Case => self.case_context(take(&mut wd))?,
				Comment => {
					while self.char_stream.front().is_some_and(|ch| *ch != '\n') {
						self.advance();
					}
					self.advance(); // Consume the newline too
					self.pop_ctx();
				}
				_ => unreachable!("{:?},{:?}",self.context,self.char_stream.iter().collect::<String>())
			}
		}
		Ok(take(&mut self.tokens))
	}
	fn command_context(&mut self, mut wd: WordDesc) -> OxResult<()> {
		use crate::interp::token::TkState::*;
		wd = self.complete_word(wd,None)?;
		if wd.text.ends_with("()") {
			wd.text = wd.text.strip_suffix("()").unwrap().to_string();
			self.tokens.push(Tk { tk_type: TkType::FuncDef, wd });
			self.push_ctx(FuncDef);
		} else if KEYWORDS.contains(&wd.text.as_str()) {
			match wd.text.as_str() {
				"then" | "elif" | "else" | "do" => { /* Do nothing, already in command ctx */ }
				"if" => { self.push_ctx(If); self.push_ctx(Command); }
				"while" | "until" => { self.push_ctx(Loop); self.push_ctx(Command); }
				"for" => { self.push_ctx(For); self.push_ctx(VarDec); }
				"case" => self.push_ctx(Case),
				"select" => self.push_ctx(Select),
				_ => self.push_ctx(DeadEnd)
			}
			match wd.text.as_str() {
				"if" => self.tokens.push(Tk { tk_type: TkType::If, wd: wd.add_flag(WdFlags::KEYWORD) }),
				"then" => self.tokens.push(Tk { tk_type: TkType::Then, wd: wd.add_flag(WdFlags::KEYWORD) }),
				"elif" => self.tokens.push(Tk { tk_type: TkType::Elif, wd: wd.add_flag(WdFlags::KEYWORD) }),
				"else" => self.tokens.push(Tk { tk_type: TkType::Else, wd: wd.add_flag(WdFlags::KEYWORD) }),
				"fi" => self.tokens.push(Tk { tk_type: TkType::Fi, wd: wd.add_flag(WdFlags::KEYWORD) }),
				"for" => self.tokens.push(Tk { tk_type: TkType::For, wd: wd.add_flag(WdFlags::KEYWORD) }),
				"do" => self.tokens.push(Tk { tk_type: TkType::Do, wd: wd.add_flag(WdFlags::KEYWORD) }),
				"done" => self.tokens.push(Tk { tk_type: TkType::Done, wd: wd.add_flag(WdFlags::KEYWORD) }),
				"while" => self.tokens.push(Tk { tk_type: TkType::While, wd: wd.add_flag(WdFlags::KEYWORD) }),
				"until" => self.tokens.push(Tk { tk_type: TkType::Until, wd: wd.add_flag(WdFlags::KEYWORD) }),
				"case" => {
					self.push_ctx(TkState::Case);
					self.tokens.push(Tk { tk_type: TkType::Case, wd: wd.add_flag(WdFlags::KEYWORD) })
				},
				"select" => self.tokens.push(Tk { tk_type: TkType::Select, wd: wd.add_flag(WdFlags::KEYWORD) }),
				"esac" => self.tokens.push(Tk { tk_type: TkType::Esac, wd: wd.add_flag(WdFlags::KEYWORD) }),
				_ => unreachable!("text: {}", wd.text)
			}
		} else if matches!(wd.text.as_str(), ";" | "\n") {
			self.tokens.push(Tk::cmdsep(&wd, wd.span.end));
			self.pop_ctx();
		} else {
			let flags = match *self.ctx() {
				Arg => WdFlags::IS_ARG,
				Command => {
					if BUILTINS.contains(&wd.text.as_str()) {
						WdFlags::BUILTIN
					} else {
						WdFlags::empty()
					}
				}
				_ => unreachable!()
			};
			if wd.text.has_unescaped("=") {
				let (var,val) = wd.text.split_once("=").unwrap();
				let mut val_wd = wd.clone();
				val_wd.text = val.to_string();
				let mut sub_tokenizer = OxTokenizer::new(&val_wd.text);
				let mut val_tk = VecDeque::from(sub_tokenizer.tokenize_one()?);
				val_tk.pop_front();
				if val_tk.is_empty() {
					return Err(ShError::from_syntax("No value found for this assignment", wd.span))
				}
				let mut val_tk = val_tk.pop_front().unwrap();

				if val.starts_with('"') && val.ends_with('"') {
					val_tk.tk_type = TkType::String;
					val_tk.wd.text = val.trim_matches('"').to_string();
				}

				let mut expanded = expand::expand_token(val_tk, true)?;
				if let Some(tk) = expanded.pop_front() {
					let ass_token = Tk {
						tk_type: TkType::Assignment { key: var.to_string(), value: Box::new(tk) },
						wd
					};
					self.tokens.push(ass_token);
				}
				self.push_ctx(Arg)
			} else {
				self.tokens.push(Tk { tk_type: TkType::Ident, wd: wd.reset_flags().add_flag(flags) });
				self.push_ctx(Arg)
			}
		}
		Ok(())
	}
	fn arg_context(&mut self, mut wd: WordDesc) -> OxResult<()> {
		use crate::interp::token::TkState::*;
		wd = self.complete_word(wd,None)?;
		match wd.text.as_str() {
			"||" | "&&" | "|" | "|&" => {
				while self.char_stream.front().is_some_and(|ch| *ch == '\n') {
					self.advance(); // Allow line continuation
				}
				// Push the token
			}
			_ => { /* Do nothing */ }
		}
		match wd.text.as_str() {
			"||" => {
				self.tokens.push(Tk { tk_type: TkType::LogicOr, wd: wd.add_flag(WdFlags::IS_OP) });
				self.push_ctx(Command)
			}
			"&&" => {
				self.tokens.push(Tk { tk_type: TkType::LogicAnd, wd: wd.add_flag(WdFlags::IS_OP) });
				self.push_ctx(Command)
			}
			"|" => {
				self.tokens.push(Tk { tk_type: TkType::Pipe, wd: wd.add_flag(WdFlags::IS_OP) });
				self.push_ctx(Command)
			}
			"|&" => {
				self.tokens.push(Tk { tk_type: TkType::PipeBoth, wd: wd.add_flag(WdFlags::IS_OP) });
				self.push_ctx(Command)
			}
			"&" => {
				self.tokens.push(Tk { tk_type: TkType::Background, wd: wd.add_flag(WdFlags::IS_OP) });
				self.push_ctx(Command)
			}
			";" | "\n" => {
				self.tokens.push(Tk::cmdsep(&wd,wd.span.start));
				self.pop_ctx();
			}
			_ if REGEX["redirection"].is_match(&wd.text) => {
				wd = wd.add_flag(WdFlags::IS_OP);
				let mut fd_out;
				let operator;
				let fd_target;
				if let Some(caps) = REGEX["redirection"].captures(&wd.text) {
					fd_out = caps.name("fd_out").and_then(|fd| fd.as_str().parse::<i32>().ok()).unwrap_or(1);
					operator = caps.name("operator").unwrap().as_str();
					fd_target = caps.name("fd_target").and_then(|fd| fd.as_str().parse::<i32>().ok())
				} else { unreachable!() }
				let op = match operator {
					">" => RedirType::Output,
					">>" => RedirType::Append,
					"<" => RedirType::Input,
					"<<<" => RedirType::Herestring,
					_ => unimplemented!()
				};
				if matches!(op, RedirType::Input | RedirType::Herestring) {
					fd_out = 0
				}
				let redir = Redir {
					fd_source: fd_out,
					op,
					fd_target,
					file_target: None
				};
				self.tokens.push(Tk { tk_type: TkType::Redirection { redir }, wd })
			}
			_ => {
				// Now we will just expand the token here
				// This catches variable subs, command subs, brace expansions, the whole nine yards
				let mut token = Tk { tk_type: TkType::Ident, wd: wd.add_flag(WdFlags::IS_ARG) };
				if token.text().starts_with('"') && token.text().ends_with('"') {
					token.tk_type = TkType::String;
					token.wd.text = token.text().trim_matches('"').to_string();
				}
				// We will expand variables later for these contexts
				if !self.context.contains(&For) && !self.context.contains(&Case) && !self.context.contains(&Select) {
					let mut expanded = expand::expand_token(token.clone(), true)?;
					if !expanded.is_empty() {
						self.tokens.extend(expanded.drain(..));
					} else if !token.text().starts_with('$') {
						self.tokens.push(token);
					}
				} else {
					self.tokens.push(token);
				}
			}
		}
		Ok(())
	}
	fn string_context(&mut self, mut wd: WordDesc) -> OxResult<()> {
		use crate::interp::token::TkState::*;
		// Pop the opening quote
		self.advance();
		while let Some(ch) = self.advance() {
			match ch {
				'\\' => {
					wd = wd.add_char(ch);
					let next_char = self.advance();
					if let Some(ch) = next_char {
						wd = wd.add_char(ch)
					}
				}
				'"' if *self.ctx() == TkState::DQuote => {
					self.push_ctx(Arg);
					break
				}
				'\'' if *self.ctx() == TkState::SQuote => {
					self.push_ctx(Arg);
					break
				}
				_ => {
					wd = wd.add_char(ch)
				}
			}
		}
		if *self.ctx() == DQuote {
			let token = Tk { tk_type: TkType::String, wd };
			let mut expanded = expand::expand_token(token, true)?;
			self.tokens.extend(expanded.drain(..));
		} else {
			self.tokens.push(Tk { tk_type: TkType::String, wd });
		}
		self.pop_ctx();
		Ok(())
	}
	fn vardec_context(&mut self, mut wd: WordDesc) -> OxResult<()> {
		use crate::interp::token::TkState::*;
		let mut found = false;
		loop {
			let span = wd.span;
			if self.char_stream.is_empty() {
				return Err(ShError::from_parse("Did not find an `in` keyword for this statement", span))
			}
			wd = self.complete_word(wd,None)?;
			match wd.text.as_str() {
				"in" => {
					if !found {
						return Err(ShError::from_parse("Did not find a variable for this statement", wd.span))
					}
					self.tokens.push(Tk { tk_type: TkType::In, wd: wd.add_flag(WdFlags::KEYWORD) });
					break
				}
				_ => {
					if *self.ctx() == SingleVarDec && found {
						return Err(ShError::from_parse(format!("Only expected one variable in this statement, found: {}",wd.text).as_str(), wd.span))
					}
					found = true;
					self.tokens.push(Tk { tk_type: TkType::Ident, wd: take(&mut wd) });
					wd = wd.set_span(span);
				}
			}
		}
		self.pop_ctx();
		self.push_ctx(ArrDec);
		Ok(())
	}
	fn arrdec_context(&mut self, mut wd: WordDesc) -> OxResult<()> {
		use crate::interp::token::TkState::*;
		let mut found = false;
		while self.char_stream.front().is_some_and(|ch| !matches!(ch, ';' | '\n')) {
			found = true;
			wd = self.complete_word(wd,None)?;
			self.tokens.push(Tk { tk_type: TkType::Ident, wd: take(&mut wd) });
		}
		if self.char_stream.front().is_some_and(|ch| matches!(ch, ';' | '\n')) {
			if !found {
				return Err(ShError::from_parse("Did not find any array elements for this statement", wd.span))
			}
			self.advance();
			self.tokens.push(Tk::cmdsep(&wd,wd.span.end));
		}
		self.pop_ctx();
		self.push_ctx(Command);
		Ok(())
	}
	fn subshell_context(&mut self, mut wd: WordDesc) -> OxResult<()> {
		use crate::interp::token::TkState::*;
		self.advance();
		if *self.ctx() == CommandSub { self.advance(); }
		let mut paren_stack = vec!['('];
		while let Some(ch) = self.advance() {
			match ch {
				'(' => {
					paren_stack.push(ch);
					wd = wd.add_char(ch)
				}
				')' => {
					paren_stack.pop();
					if paren_stack.is_empty() {
						break
					}
					wd = wd.add_char(ch);
				}
				_ => {
					wd = wd.add_char(ch)
				}
			}
		}
		wd = wd.set_span(self.spans.pop_front().unwrap_or_default());
		let mut tk = match *self.ctx() {
			Subshell => {
				Tk {
					tk_type: TkType::Subshell,
					wd
				}
			}
			CommandSub => {
				Tk {
					tk_type: TkType::CommandSub,
					wd
				}
			}
			_ => unreachable!()
		};
		if *self.ctx() == CommandSub {
			tk = expand_cmd_sub(tk)?;
		}
		if !paren_stack.is_empty() {
			return Err(ShError::from_syntax("This subshell is missing a closing parenthesis", tk.span()))
		}
		self.pop_ctx();
		self.tokens.push(tk);
		Ok(())
	}
	fn func_context(&mut self, mut wd: WordDesc) -> OxResult<()> {
		use crate::interp::token::TkState::*;
		self.advance();
		if *self.ctx() == CommandSub { self.advance(); }
		let mut brace_stack = vec!['{'];
		while let Some(ch) = self.advance() {
			match ch {
				'{' => {
					brace_stack.push(ch);
					wd = wd.add_char(ch)
				}
				'}' => {
					brace_stack.pop();
					if brace_stack.is_empty() {
						break
					}
					wd = wd.add_char(ch);
				}
				_ => {
					wd = wd.add_char(ch)
				}
			}
		}
		wd = wd.set_span(self.spans.pop_front().unwrap_or_default());
		wd.text = wd.text.trim().to_string();
		if !brace_stack.is_empty() {
			return Err(ShError::from_syntax("This function is missing a closing brace", wd.span))
		}
		self.tokens.push(Tk { tk_type: TkType::FuncBody, wd });
		self.push_ctx(DeadEnd);
		Ok(())
	}
	fn case_context(&mut self, mut wd: WordDesc) -> OxResult<()> {
		use crate::interp::token::TkState::*;
		let span = wd.span;
		let mut closed = false;
		self.push_ctx(SingleVarDec);
		self.vardec_context(take(&mut wd))?;
		self.pop_ctx();
		if self.char_stream.front().is_some_and(|ch| matches!(*ch, ';' | '\n')) {
			self.advance();
			self.tokens.push(Tk::cmdsep(&wd,span.end + 1));
		}
		let mut arms = vec![];
		while !self.char_stream.is_empty() {
			let mut arm = String::new();
			while !arm.ends_with(";;") && !arm.ends_with("esac") {
				if let Some(ch) = self.advance() {
					arm.push(ch);
				}
			}
			if let Some(arm) = arm.strip_suffix(";;") {
				arms.push(arm.to_string())
			} else if arm.trim().starts_with("esac") {
				closed = true;
				break
			} else {
				return Err(ShError::from_syntax("Did not find a `;;` for this case arm", span))
			}
		}

		if !closed {
			return Err(ShError::from_syntax("This case statement is missing a closing `esac`", span))
		}

		for arm in arms {
			if let Some((pat, body)) = arm.split_once(')') {
				let pat_tk = Tk {
					tk_type: TkType::CasePat,
					wd: WordDesc {
						text: pat.to_string(),
						flags: WdFlags::empty(),
						span: wd.span
					}
				};
				let mut body_tokens = vec![];
				let mut body_tokenizer = OxTokenizer::new(body);
				while let Ok(mut block) = body_tokenizer.tokenize_one() {
					if !block.is_empty() {
						if block.first().is_some_and(|tk| tk.tk_type == TkType::SOI) {
							block = block[1..].to_vec();
						}
						body_tokens.append(&mut block);
					} else {
						break
					}
				}
				self.tokens.push(pat_tk);
				self.tokens.append(&mut body_tokens)
			} else {
				return Err(ShError::from_syntax("This case arm doesn't have a separating ')'", span))
			}
		}
		self.tokens.push(Tk { tk_type: TkType::Esac, wd });
		self.push_ctx(DeadEnd);
		Ok(())
	}
	fn complete_word(&mut self, mut wd: WordDesc, brk_pattern: Option<char>) -> OxResult<WordDesc> {
		let mut dub_quote = false;
		let mut sng_quote = false;
		let mut paren_stack = vec![];
		let mut bracket_stack = vec![];
		while self.char_stream.front().is_some_and(|ch| ch.is_whitespace()) {
			self.advance();
		}
		while let Some(ch) = self.advance() {
			match ch {
				_ if brk_pattern.is_some_and(|pat| pat == ch) => {
					break
				}
				'\\' => {
					wd = wd.add_char(ch);
					if let Some(char) = self.advance() {
						wd = wd.add_char(char)
					}
				}
				'[' if paren_stack.is_empty() && !sng_quote && !dub_quote => {
					bracket_stack.push(ch);
					wd = wd.add_char(ch);
				}
				']' if !bracket_stack.is_empty() && !sng_quote && !dub_quote => {
					bracket_stack.pop();
					wd = wd.add_char(ch);
				}
				'(' if bracket_stack.is_empty() && !sng_quote && !dub_quote => {
					paren_stack.push(ch);
					wd = wd.add_char(ch);
				}
				')' if !paren_stack.is_empty() && !sng_quote && !dub_quote => {
					paren_stack.pop();
					wd = wd.add_char(ch);
					// Now we check for words in a command context that look like `this()`
					// If it does, it's a function definition, so break here
					if *self.ctx() == TkState::Command && wd.text.ends_with("()") {
						break;
					}
				}
				'\'' if !dub_quote => {
					// Single quote handling
					sng_quote = !sng_quote;
					wd = wd.add_flag(WdFlags::SNG_QUOTED).add_char(ch);
				}
				'"' if !sng_quote => {
					// Double quote handling
					dub_quote = !dub_quote;
					wd = wd.add_char(ch);
				}
				'|' | '&' if bracket_stack.is_empty() && paren_stack.is_empty() && !dub_quote && !sng_quote => {
					let redir_check = wd.text.chars().next();
					if redir_check.is_some_and(|ch| ch.is_ascii_digit() || matches!(ch, '>' | '<')) {
						wd = wd.add_char(ch)
					} else {
						self.char_stream.push_front(ch);
						break;
					}
				}
				_ if !bracket_stack.is_empty() || !paren_stack.is_empty() || dub_quote || sng_quote => {
					// Inside a quoted string
					wd = wd.add_char(ch);
				}
				_ if !matches!(ch, ' ' | '\t' | '\n' | ';') => {
					// Regular character
					wd = wd.add_char(ch);
				}
				'\n' | ';' | ' ' | '\t' if !bracket_stack.is_empty() || !paren_stack.is_empty() || dub_quote || sng_quote => {
					wd = wd.add_char(ch)
				}
				'\n' | ';' => {
					// Preserve cmdsep or operator for tokenizing
					self.char_stream.push_front(ch);
					break;
				}
				' ' | '\t' => {
					// Whitespace handling
					while self.char_stream.front().is_some_and(|c| matches!(c, ' ' | '\t')) {
						self.advance();
					}
					break;
				}
				_ => {
					// Default case (shouldn't be hit in the current logic)
					wd = wd.add_char(ch);
				}
			}
		}

		wd.span = self.spans.pop_front().unwrap_or_default();
		Ok(wd)
	}
}

// TODO: Case tests, select tests
#[cfg(test)]
mod tests {
	use crate::shellenv::write_vars;
	use super::*;

	#[test]
	fn tokenizer_simple() {
		let input = "echo hello";
		let mut tokenizer = OxTokenizer::new(input);

		let tokens = tokenizer.tokenize_one().unwrap();
		insta::assert_debug_snapshot!(tokens);
	}

	#[test]
	fn tokenizer_multiple_args() {
		let input = "ls -l /home/user";
		let mut tokenizer = OxTokenizer::new(input);

		let tokens = tokenizer.tokenize_one().unwrap();
		insta::assert_debug_snapshot!(tokens);
	}

	#[test]
	fn tokenizer_quoted_arg() {
		let input = "echo \"Hello, world!\"";
		let mut tokenizer = OxTokenizer::new(input);

		let tokens = tokenizer.tokenize_one().unwrap();
		insta::assert_debug_snapshot!(tokens);
	}

	#[test]
	fn tokenizer_single_quoted_arg() {
		let input = "echo 'single quoted arg'";
		let mut tokenizer = OxTokenizer::new(input);

		let tokens = tokenizer.tokenize_one().unwrap();
		insta::assert_debug_snapshot!(tokens);
	}

	#[test]
	fn tokenizer_variable_expansion() {
		let input = "echo $USER";
		let mut tokenizer = OxTokenizer::new(input);

		let tokens = tokenizer.tokenize_one().unwrap();
		insta::assert_debug_snapshot!(tokens);
	}

	#[test]
	fn tokenizer_pipeline() {
		let input = "ls | grep file";
		let mut tokenizer = OxTokenizer::new(input);

		let tokens = tokenizer.tokenize_one().unwrap();
		insta::assert_debug_snapshot!(tokens);
	}


	#[test]
	fn tokenizer_redirection() {
		let input = "echo hello > output.txt";
		let mut tokenizer = OxTokenizer::new(input);

		let tokens = tokenizer.tokenize_one().unwrap();
		insta::assert_debug_snapshot!(tokens);
	}

	#[test]
	fn tokenizer_subshell() {
		let input = "(echo hi)";
		let mut tokenizer = OxTokenizer::new(input);

		let tokens = tokenizer.tokenize_one().unwrap();
		insta::assert_debug_snapshot!(tokens);
	}

	#[test]
	fn tokenizer_subshell_multiline() {
		let input = "(#!python
print(\"hello world\")
)";
		let mut tokenizer = OxTokenizer::new(input);

		let tokens = tokenizer.tokenize_one().unwrap();
		insta::assert_debug_snapshot!(tokens);
	}

	#[test]
	fn tokenizer_func_def() {
		let input = "func() {
			echo hi
			cat file.txt
		}";
		let mut tokenizer = OxTokenizer::new(input);

		let tokens = tokenizer.tokenize_one().unwrap();
		insta::assert_debug_snapshot!(tokens);
	}

	#[test]
	fn tokenizer_expand_params() {
		let input = "echo $@";
		let mut tokenizer = OxTokenizer::new(input);
		write_vars(|v| v.set_param("1".into(), "one".into())).unwrap();
		write_vars(|v| v.set_param("2".into(), "two".into())).unwrap();
		write_vars(|v| v.set_param("3".into(), "three".into())).unwrap();
		write_vars(|v| v.set_param("4".into(), "four".into())).unwrap();

		let tokenizer = tokenizer.tokenize_one().unwrap();

		insta::assert_debug_snapshot!(tokenizer)
	}

	#[test]
	fn tokenizer_if() {
		let input = "if true; then echo hi; elif false; then echo hello; else echo greetings; fi";
		let mut tokenizer = OxTokenizer::new(input);

		let tokens = tokenizer.tokenize_one().unwrap();
		insta::assert_debug_snapshot!(tokens);
	}

	#[test]
	fn tokenizer_for_loop() {
		let input = "for i in 1 2 3; do echo $i; done";
		let mut tokenizer = OxTokenizer::new(input);

		let tokens = tokenizer.tokenize_one().unwrap();
		insta::assert_debug_snapshot!(tokens);
	}

	#[test]
	fn tokenizer_while_loop() {
		let input = "while true; do echo working; done";
		let mut tokenizer = OxTokenizer::new(input);

		let tokens = tokenizer.tokenize_one().unwrap();
		insta::assert_debug_snapshot!(tokens);
	}

	#[test]
	fn tokenizer_until_loop() {
		let input = "until true; do echo waiting; done";
		let mut tokenizer = OxTokenizer::new(input);

		let tokens = tokenizer.tokenize_one().unwrap();
		insta::assert_debug_snapshot!(tokens);
	}

	#[test]
	fn tokenizer_final_boss() {
		let input = "if while if true; then echo while condition; fi; do if true; then echo inside first while; fi; done; then echo wow; elif until while if true; then echo double loop; fi; do if true; then echo another double loop; fi; done; do echo; done; then echo again; else echo; fi";
		let mut tokenizer = OxTokenizer::new(input);

		let tokens = tokenizer.tokenize_one().unwrap();
		insta::assert_debug_snapshot!(tokens);
	}
}
