use bitflags::bitflags;
use crossterm::event;
use nix::NixPath;
use once_cell::sync::Lazy;
use pest::iterators::Pair;
use pest_derive::Parser;
use pest::Parser;
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
use crate::shellenv::EnvFlags;
use crate::shellenv::PARAMS;
use crate::LashResult;
use crate::builtin::BUILTINS;

use super::expand;
use super::helper;
use super::helper::StrExtension;

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
	regex
});

pub const KEYWORDS: [&str;15] = [
	"match", "if", "while", "until", "for", "case", "select",
	"then", "elif", "else", "in",
	"do", "done", "fi", "esac"
];
pub const OPENERS: [&str;7] = [
	"if", "while", "until", "for", "case", "select", "match"
];
pub const CMDSEP: [char;2] = [
	';', '\n'
];
pub const WHITESPACE: [char;2] = [
	' ', '\t'
];
pub const META_TOKENS: [TkType;4] = [
	TkType::DQuote,
	TkType::SQuote,
	TkType::Space,
	TkType::Tab
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

/*
 * This struct contains metadata for tokens
 * The span is the actual space in the input that the token uses up
 * Spans are used for error handling to blame lines
 *
 * The flags field contains a ton of metadata regarding the context surrounding the token
 */
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
pub enum AssOp {
	PlusEquals,
	MinusEquals,
	Equals
}

#[derive(Debug,Hash,Clone,PartialEq,Eq)]
pub struct Tk {
	pub tk_type: TkType,
	pub wd: WordDesc
}

// Used to identify tokens easily
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
	Match,
	MatchArm { pat: String, body: String },
	Select,
	In,
	FuncBody,
	LoopCond,
	LoopBody,

	Redirection { redir: Redir },
	FuncDef,
	Assignment { key: String, value: Box<Tk>, op: AssOp },
	LogicAnd, // `&&`
	LogicOr, // `||`
	Pipe, // `|`
	PipeBoth, // '|&'
	Background, // `&`

	PlusEquals,
	MinusEquals,

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
	SQuote,
	DQuote,

	// Comments
	Comment, // `#`

	// Special Characters
	Cmdsep, // ';' or '\n'
	CasePat,
	Space,
	Tab,
	SOI,
	EOI,
	Null, // Default

}

impl Tk {
	/*
	 * This is a load-bearing struct, used widely throughout the entire program
	 * The constructor methods are used mainly in the tokenizer, but the getter methods are used everywhere
	 *
	 * Most of the actual token metadata is held in the `wd` field, and the getter methods extract this info
	 *
	 * tk_type is used in place of some trait implementation, to avoid having to use something like &dyn Tk
	 * Avoiding dynamic dispatch in this way is faster and simpler
	 */
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
	pub fn from(wd: WordDesc, context: TkState) -> LashResult<Self> {
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
			Select => Ok(Tk { tk_type: TkT::Select, wd: wd.add_flag(WdFlags::KEYWORD) }),
			In => Ok(Tk { tk_type: TkT::In, wd: wd.add_flag(WdFlags::KEYWORD) }),
			Elif => Ok(Tk { tk_type: TkT::Elif, wd: wd.add_flag(WdFlags::KEYWORD) }),
			Else => Ok(Tk { tk_type: TkT::Else, wd: wd.add_flag(WdFlags::KEYWORD) }),
			Do => Ok(Tk { tk_type: TkT::Do, wd: wd.add_flag(WdFlags::KEYWORD) }),
			Then => Ok(Tk { tk_type: TkT::Then, wd: wd.add_flag(WdFlags::KEYWORD) }),
			Done => Ok(Tk { tk_type: TkT::Done, wd: wd.add_flag(WdFlags::KEYWORD) }),
			Fi => Ok(Tk { tk_type: TkT::Fi, wd: wd.add_flag(WdFlags::KEYWORD) }),
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
	pub fn space(wd: &WordDesc) -> Self {
		Tk {
			tk_type: TkType::Space,
			wd: wd.clone()
		}
	}
	pub fn tab(wd: &WordDesc) -> Self {
		Tk {
			tk_type: TkType::Tab,
			wd: wd.clone()
		}
	}
	pub fn d_quote(wd: &WordDesc) -> Self {
		Tk {
			tk_type: TkType::DQuote,
			wd: wd.clone()
		}
	}
	pub fn s_quote(wd: &WordDesc) -> Self {
		Tk {
			tk_type: TkType::SQuote,
			wd: wd.clone()
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
			tk_type: TkType::Background,
			wd: WordDesc { text: wd.text.clone(), span: Span::from(pos,pos), flags: WdFlags::IS_OP }
		}
	}
	pub fn and(wd: &WordDesc, pos: usize) -> Self {
		Tk {
			tk_type: TkType::LogicAnd,
			wd: WordDesc { text: wd.text.clone(), span: Span::from(pos,pos + 1), flags: WdFlags::IS_OP }
		}
	}
	pub fn or(wd: &WordDesc, pos: usize) -> Self {
		Tk {
			tk_type: TkType::LogicOr,
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
	VarSub,
	Ident, // Generic words used for var and arr declarations
	Arg, // Command arguments; only appear after commands
	Command, // Starting point for the tokenizer
	FuncDef, // Function names
	FuncBody, // Function bodies
	Array, // Used in for loops and select statements
	If, // If statement opener
	For, // For loop opener
	Loop, // While/Until opener
	Match, // Match opener
	Select, // Select opener
	In, // Used in for, case, and select statements
	Elif, // Secondary if/then blocks
	Else, // Else statements
	Do, // Select, for, and while/until condition/body separator
	Then, // If statement condition/body separator
	Done, // Select, for, and while/until closer
	Fi, // If statement closer
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

#[derive(Parser,Debug)]
#[grammar = "lash.pest"]
pub struct LashPest;

#[derive(Debug)]
pub struct LashTokenizer<'a> {
	input: String,
	pub tokens: Vec<Pair<'a,Rule>>,
}

impl<'a> LashTokenizer<'a> {
	pub fn new(input: &str) -> Self {
		let mut input = input.to_string();
		if input.starts_with("#!") { // Ignore shebangs
			let mut lines = input.lines();
			lines.next();
			input = lines.collect::<Vec<&str>>().join("\n").trim().to_string();
		}
		Self { input, tokens: vec![] }
	}
	pub fn input(&self) -> String {
		self.input.clone()
	}
	fn split_input(&self) -> Vec<String> {
		/*
		 * Used to split raw input during expansion
		 * Important to do it this way rather than just using split(' '),
		 * This function follows all of the rules associated with shell syntax
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

	fn alias_pass(&mut self) -> LashResult<()> {
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
						*word = format!("{} ", alias.clone());
						replaced = true;
					}
					is_command = false;
				}
				if word.ends_with(';') || word.ends_with('\n') {
					is_command = true;
				}
				new_input.push_str(&word);
			}

			self.input = new_input;
		}

		Ok(())
	}
	fn glob_pass(&mut self) -> LashResult<()> {
		let mut words = VecDeque::from(self.split_input());
		let mut new_input = String::new();
		let mut is_command = true;
		let mut separator = None;

		while let Some(mut word) = words.pop_front() {
			if is_command { // Do not expand globs in a command name
				is_command = false;
				new_input.push_str(&word);
				continue
			}
			if word.trim() == "match" {
				while let Some(word) = words.pop_front() {
					if word.trim() == "done" {
						break
					}
				}
			}
			if word.ends_with(['\n',';']) { // Detach the separator and save it
				if word.len() == 1 { // Unless it's the only character
					new_input.push_str(&word);
					continue
				}
				// We will re-attach it to the last word in the glob result later
				separator = Some(word.chars().last().unwrap());
				word = word[..word.len() - 1].to_string();
				is_command = true;
			}
			if helper::contains_glob(&word) {
				// Get the glob result
				let mut glob_vec = word.expand_globs();
				if glob_vec.is_empty() {
					if let Some(sep) = separator.take() {
						new_input.push(sep);
					}
					continue
				};
				if let Some(sep) = separator.take() {
					// Re-attach the separator to the last word
					let mut last_word = glob_vec.pop().unwrap();
					last_word.push(sep);
					glob_vec.push(last_word);
				}
				for (i,expanded) in glob_vec.iter().enumerate() {
					if i > 0 { // Add spaces between glob words
						new_input.push(' ');
					}
					new_input.push_str(expanded);
				}
			} else {
				if let Some(sep) = separator.take() {
					word.push(sep);
				}
				new_input.push_str(&word);
			}
		}

		self.input = new_input;

		Ok(())
	}
	fn var_pass(&mut self) -> LashResult<()> {
		let vartable = read_vars(|v| v.clone())?;
		let mut is_command = true;
		let mut replaced = true;

		while replaced {
			replaced = false;
			let mut words = self.split_input().into_iter().collect::<VecDeque<String>>();
			let mut new_input = vec![];

			while let Some(word) = words.pop_front() {
				if word.has_varsub() {
					let (left,right) = helper::split_at_varsub(&word);
					if right.is_empty() {
						new_input.push(left);
						continue
					}
					let mut middle = String::new();
					let is_braced = right.starts_with('{');
					let mut r_chars = right.chars().peekable();
					if is_braced {
						r_chars.next();
					}
					while let Some(ch) = r_chars.peek() {
						match ch {
							'\\' => {
								let next = r_chars.next().unwrap();
								middle.push(next);
								if let Some(esc_ch) = r_chars.next() {
									middle.push(esc_ch)
								}
							}
							'"' | '\'' | ' ' | '\t' | ';' | '\n' | '/' => {
								// TODO: Find a better way to break this loop
								// Tried 'ch.is_whitespace() || !ch.is_alphanumeric()' but that failed to expand params
								// Something similar will probably work
								break
							}
							'}' if is_braced => {
								r_chars.next();
								break
							}
							_ => {
								let next = r_chars.next().unwrap();
								middle.push(next);
							}
						}
						if PARAMS.contains(&middle.as_str()) {
							break
						}
					}
					let value = vartable.get_var(&middle).unwrap_or_default();
					let remainder = r_chars.collect::<String>();
					let expanded = format!("{}{}{}",left,value,remainder).trim_matches([' ', '\t']).to_string();
					new_input.push(expanded);
					replaced = true;
				} else {
					if &word == "while" || &word == "until" || &word == "for" {
						/*
						 * Here we are going to defer the expansion of variables until later.
						 * If we expand them here, they become static strings in the loop
						 */
						while let Some(word) = words.pop_front() {
							new_input.push(word.trim().to_string());
							if &word == "done" {
								break
							}
						}
					}
					if is_command {
						is_command = false;
					}
					if word.ends_with([';','\n']) {
						is_command = true;
					}
					if !word.trim().is_empty() { new_input.push(word.trim_matches(' ').to_string()); }
			}
			}

			new_input.extend(words.drain(..));

			self.input = new_input.join(" ");
		}

		Ok(())
	}
	fn cmd_sub_pass(&mut self) -> LashResult<()> {
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
						let saved_input = self.input.clone();
						self.input = middle; // We are going to cheat here and expand variables for just the cmd sub body
						self.var_pass()?;
						let middle = self.input.clone();
						let dummy_tk = Tk {
							tk_type: TkType::CommandSub,
							wd: WordDesc {
								text: middle.to_string(),
								span: Span::new(),
								flags: WdFlags::empty(),
							},
						};
						self.input = saved_input;
						let new_tk = expand::expand_cmd_sub(dummy_tk)?;
						if left.ends_with('=') {
							word = format!("{}\"{}\"{}", left, new_tk.text(), right);
						} else {
							word = format!("{}{}{}", left, new_tk.text(), right);
						}
						replaced = true;
					}
				}
				if !word.trim().is_empty() { new_input.push_str(&word); }
			}

			self.input = new_input;
		}

		Ok(())
	}
	pub fn expand_one(&mut self) -> LashResult<String> {
		/*
		 * Here we use basically the same logic as tokenize_one for maintaining context
		 * The idea is to localize expansion to just the section of the input being worked on
		 * So that variables are not expanded prematurely, like in the case of
		 *
		 * i=10
		 *
		 * i=$(expr "$i + 1")
		 *
		 * `i` should not expand in the command sub until it is time to execute it
		 */
		use crate::interp::token::TkState::*;
		let mut pushed = false; // Guard condition to prevent the same word from pushing a ctx twice or more
		let mut chars = self.input.chars().peekable();
		let mut working_buffer = String::new();
		let mut local_ctx = vec![Command];
		let mut paren_stack = vec![];
		let mut brace_stack = vec![];
		let mut bracket_stack = vec![];

		while let Some(ch) = chars.next() {
			match ch {
				'\\' => {
					working_buffer.push(ch);
					if let Some(ch) = chars.next() {
						working_buffer.push(ch);
					}
				}
				'"' => {
					working_buffer.push(ch);
					while let Some(ch) = chars.next() {
						working_buffer.push(ch);
						match ch {
							'\\' => {
								if let Some(ch) = chars.next() {
									working_buffer.push(ch);
								}
							}
							'"' => {
								break
							}
							_ => { /* Continue */ }
						}
					}
				}
				'\'' => {
					working_buffer.push(ch);
					while let Some(ch) = chars.next() {
						working_buffer.push(ch);
						match ch {
							'\\' => {
								if let Some(ch) = chars.next() {
									working_buffer.push(ch);
								}
							}
							'\'' => {
								break
							}
							_ => { /* Continue */ }
						}
					}
				}
				'{' => {
					working_buffer.push(ch);
					brace_stack.push(ch);
					while let Some(ch) = chars.next() {
						working_buffer.push(ch);
						match ch {
							'\\' => {
								if let Some(ch) = chars.next() {
									working_buffer.push(ch);
								}
							}
							'{' => brace_stack.push(ch),
							'}' => {
								brace_stack.pop();
								if brace_stack.is_empty() {
									break
								}
							}
							_ => { /* Continue */ }
						}
					}
				}
				'(' => {
					working_buffer.push(ch);
					paren_stack.push(ch);
					while let Some(ch) = chars.next() {
						working_buffer.push(ch);
						match ch {
							'\\' => {
								if let Some(ch) = chars.next() {
									working_buffer.push(ch);
								}
							}
							'(' => paren_stack.push(ch),
							')' => {
								paren_stack.pop();
								if paren_stack.is_empty() {
									break
								}
							}
							_ => { /* Continue */ }
						}
					}
				}
				'[' => {
					working_buffer.push(ch);
					brace_stack.push(ch);
					while let Some(ch) = chars.next() {
						working_buffer.push(ch);
						match ch {
							'\\' => {
								if let Some(ch) = chars.next() {
									working_buffer.push(ch);
								}
							}
							'[' => bracket_stack.push(ch),
							']' => {
								bracket_stack.pop();
								if bracket_stack.is_empty() {
									break
								}
							}
							_ => { /* Continue */ }
						}
					}
				}
				_ => {
					if !ch.is_whitespace() {
						if working_buffer.chars().last().is_some_and(|c| matches!(c, ';' | '\n' | ' ')) {
							// We have reached a new word
							// No need to worry about duplicate contexts anymore, so this is false now
							pushed = false;
						}
					}
					working_buffer.push(ch);
				}
			}
			if working_buffer.split_whitespace().last() == Some("for") {
				if !pushed {
					local_ctx.push(For);
					pushed = true;
				}
			}
			if working_buffer.split_whitespace().last() == Some("if") {
				if !pushed {
					local_ctx.push(If);
					pushed = true;
				}
			}
			if working_buffer.split_whitespace().last() == Some("match") {
				if !pushed {
					local_ctx.push(Match);
					pushed = true;
				}
			}
			if working_buffer.split_whitespace().last() == Some("while") {
				if !pushed {
					local_ctx.push(Loop);
					pushed = true;
				}
			}
			if working_buffer.split_whitespace().last() == Some("until") {
				if !pushed {
					local_ctx.push(Loop);
					pushed = true;
				}
			}
			if working_buffer.split_whitespace().last() == Some("select") {
				if !pushed {
					local_ctx.push(Select);
					pushed = true;
				}
			}
			match local_ctx.last().unwrap() {
				Command => {
					if working_buffer.ends_with(';') || working_buffer.ends_with('\n') {
						break
					}
				}
				For | Select | Loop | Match => {
					if working_buffer.split_whitespace().last() == Some("done") {
						if matches!(chars.peek(), Some(';') | Some('\n')) {
							working_buffer.push(chars.next().unwrap())
						}
						local_ctx.pop();
						if local_ctx.last() == Some(&Command) {
							break
						}
					}
				}
				If => {
					if working_buffer.split_whitespace().last() == Some("fi") {
						if matches!(chars.peek(), Some(';') | Some('\n')) {
							working_buffer.push(chars.next().unwrap())
						}
						local_ctx.pop();
						if local_ctx.last() == Some(&Command) {
							break
						}
					}
				}
				_ => unreachable!()
			}
		}

		let sliced_input = self.input[working_buffer.len()..].trim_start().to_string(); // Remainder
		self.input = working_buffer.clone();
		self.cmd_sub_pass()?; // The order does matter here
		self.glob_pass()?;
		self.alias_pass()?;
		self.var_pass()?;
		self.input = format!("{}{}",self.input,sliced_input); // Re-attach (might not even be necessary)

		// We return the remaining unexpanded input here
		Ok(sliced_input)
	}
	pub fn prepare_input(&mut self, expand: bool) -> LashResult<String> {
		write_meta(|m| m.set_last_input(&self.input))?;

		if self.input.is_empty() { return Ok(String::new()) };
		let remainder = if expand { self.expand_one()? } else { String::new() };

		let input = self.input.clone();


		self.input = remainder;
		//dbg!(&self.tokens.iter().map(|tk| (tk.class(),tk.text())).collect::<Vec<(TkType,&str)>>());
		Ok(input)
	}
}
