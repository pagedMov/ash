use bitflags::bitflags;
use once_cell::sync::Lazy;
use regex::Regex;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::mem::take;

use crate::interp::expand::expand_cmd_sub;
use crate::interp::parse::Span;
use crate::event::ShError;
use crate::shellenv::read_logic;
use crate::RshResult;
use crate::builtin::BUILTINS;

use super::expand;
use super::helper::StrExtension;

pub static REGEX: Lazy<HashMap<&'static str, Regex>> = Lazy::new(|| {
	let mut regex = HashMap::new();
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
pub struct TkizerCtx {
	alias_expansion: Option<String>
	}

impl TkizerCtx {
	pub fn new() -> Self {
		Self { alias_expansion: None }
	}
	pub fn with_alias(name: &str) -> Self {
		Self { alias_expansion: Some(name.to_string()) }
	}
	pub fn check_alias_exp(&self) -> bool {
		self.alias_expansion.is_some()
	}
	pub fn get_alias_exp(&self) -> Option<String> {
		self.alias_expansion.clone()
	}
}

impl Default for TkizerCtx {
	fn default() -> Self {
		Self::new()
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
	Assignment, // `=`
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
	pub fn from(wd: WordDesc, context: TkState) -> RshResult<Self> {
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
	Heredoc,
	Herestring
}


#[derive(Eq,Debug,Hash,PartialEq)]
pub enum TkState {
	Root, // Outer contex, allows for anything and everything. Closed by the EOI token
	ArrDec, // Used in arrays like for i in ArrDec1 ArrDec2
	VarDec, // Used in for loop vars like for VarDec1 VarDec2 in array
	SingleVarDec, // Used in case and select
	Ident, // Generic words used for var and arr declarations
	Arg, // Command arguments; only appear after commands
	Command, // Starting point for the tokenizer
	FuncDef, // defining a function like() { this }
FuncBody, // The stuff { inside braces }
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

pub struct RshTokenizer {
	input: String,
	char_stream: VecDeque<char>,
	context: Vec<TkState>,
	pub tokens: Vec<Tk>,
	pub span: Span,
}

impl RshTokenizer {
	pub fn new(input: &str) -> Self {
		let input = input.trim().to_string();
		let char_stream = input.chars().collect::<VecDeque<char>>();
		let tokens = vec![Tk { tk_type: TkType::SOI, wd: WordDesc::empty() }];
		Self { input, char_stream, context: vec![TkState::Command], tokens, span: Span::new() }
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
	fn advance(&mut self) -> Option<char> {
		if self.span.end != self.input.len() {
			self.span.end += 1;
		}
		self.char_stream.pop_front()
	}
	pub fn tokenize_one(&mut self, ctx: TkizerCtx) -> RshResult<Vec<Tk>> {
		use crate::interp::token::TkState::*;
		let mut wd = WordDesc::empty();
		while !self.char_stream.is_empty() {
			self.span.start = self.span.end;
			wd = wd.set_span(self.span);
			if *self.ctx() == DeadEnd && !matches!(self.char_stream.front().unwrap(), ';' | '\n') {
				return Err(ShError::from_parse("Expected a semicolon or newline here", self.span))
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
					self.tokens.push(Tk::cmdsep(&wd, self.span.end));
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
					self.span.end -= 1;
				}
				' ' | '\t' => {
					self.advance();
					continue
				}
				_ => { /* Do nothing */ }
			}
			match *self.ctx() {
				Command => self.command_context(take(&mut wd),ctx.clone())?,
				Arg => self.arg_context(take(&mut wd))?,
				DQuote | SQuote => self.string_context(take(&mut wd))?,
				VarDec => self.vardec_context(take(&mut wd))?,
				SingleVarDec => self.vardec_context(take(&mut wd))?,
				ArrDec => self.arrdec_context(take(&mut wd))?,
				Subshell | CommandSub => self.subshell_context(take(&mut wd))?,
				FuncBody => self.func_context(take(&mut wd)),
				Case => self.case_context(take(&mut wd))?,
				Comment => {
					while self.char_stream.front().is_some_and(|ch| *ch != '\n') {
						self.advance();
					}
					self.advance(); // Consume the newline too
					self.push_ctx(Command)
				}
				_ => unreachable!("{:?},{:?}",self.context,self.char_stream.iter().collect::<String>())
			}
		}
		Ok(take(&mut self.tokens))
	}
	fn command_context(&mut self, mut wd: WordDesc, ctx: TkizerCtx) -> RshResult<()> {
		use crate::interp::token::TkState::*;
		wd = self.complete_word(wd);
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
				"case" => self.tokens.push(Tk { tk_type: TkType::Case, wd: wd.add_flag(WdFlags::KEYWORD) }),
				"select" => self.tokens.push(Tk { tk_type: TkType::Select, wd: wd.add_flag(WdFlags::KEYWORD) }),
				_ => unreachable!("text: {}", wd.text)
			}
		} else if ctx.get_alias_exp().is_none_or(|name| name != wd.text) && read_logic(|l| l.get_alias(wd.text.as_str()))?.is_some()  {
			if let Some(content) = read_logic(|l| l.get_alias(wd.text.as_str())).unwrap() {
				let mut sub_tokenizer = RshTokenizer::new(&content);
				loop {
					let deck = sub_tokenizer.tokenize_one(TkizerCtx::with_alias(wd.text.as_str()))?;
					if deck.is_empty() { break };
					let mut deck = deck[1..].to_vec(); // Shave off the SOI/EOI tokens
					for tk in deck.iter_mut() {
						tk.wd.flags |= WdFlags::FROM_ALIAS
					}

					self.tokens.append(&mut deck);
				}
			}
		} else if matches!(wd.text.as_str(), ";" | "\n") {
			self.tokens.push(Tk::cmdsep(&wd, self.span.end));
			self.span.start = self.span.end;
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
				self.tokens.push(Tk { tk_type: TkType::Assignment, wd });
				self.push_ctx(Arg)
			} else {
				self.tokens.push(Tk { tk_type: TkType::Ident, wd: wd.reset_flags().add_flag(flags) });
				self.push_ctx(Arg)
			}
		}
		Ok(())
	}
	fn arg_context(&mut self, mut wd: WordDesc) -> RshResult<()> {
		use crate::interp::token::TkState::*;
		wd = self.complete_word(wd);
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
				self.span.start = self.span.end;
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
				let token = Tk { tk_type: TkType::Ident, wd: wd.add_flag(WdFlags::IS_ARG) };
				// We will expand variables later for these contexts
				if !self.context.contains(&For) && !self.context.contains(&Case) && !self.context.contains(&Select) {
					let mut expanded = expand::expand_token(token, true)?;
					self.tokens.extend(expanded.drain(..));
				} else {
					self.tokens.push(token);
				}
			}
		}
		Ok(())
	}
	fn string_context(&mut self, mut wd: WordDesc) -> RshResult<()> {
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
			wd.span = self.span;
			let token = Tk { tk_type: TkType::String, wd };
			let mut expanded = expand::expand_token(token, true)?;
			self.tokens.extend(expanded.drain(..));
		} else {
			self.tokens.push(Tk { tk_type: TkType::String, wd });
		}
		self.pop_ctx();
		Ok(())
	}
	fn vardec_context(&mut self, mut wd: WordDesc) -> RshResult<()> {
		use crate::interp::token::TkState::*;
		let mut found = false;
		loop {
			let span = wd.span;
			if self.char_stream.is_empty() {
				return Err(ShError::from_parse("Did not find an `in` keyword for this statement", span))
			}
			wd = self.complete_word(wd);
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
	fn arrdec_context(&mut self, mut wd: WordDesc) -> RshResult<()> {
		use crate::interp::token::TkState::*;
		let mut found = false;
		while self.char_stream.front().is_some_and(|ch| !matches!(ch, ';' | '\n')) {
			found = true;
			wd = self.complete_word(wd);
			wd.span = self.span;
			self.tokens.push(Tk { tk_type: TkType::Ident, wd: take(&mut wd) });
			wd = wd.set_span(self.span)
		}
		if self.char_stream.front().is_some_and(|ch| matches!(ch, ';' | '\n')) {
			if !found {
				return Err(ShError::from_parse("Did not find any array elements for this statement", wd.span))
			}
			self.advance();
			self.tokens.push(Tk::cmdsep(&wd,self.span.end));
			self.span.start = self.span.end;
		}
		self.pop_ctx();
		self.push_ctx(Command);
		Ok(())
	}
	fn subshell_context(&mut self, mut wd: WordDesc) -> RshResult<()> {
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
		wd.span = self.span;
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
		self.pop_ctx();
		self.tokens.push(tk);
		Ok(())
	}
	fn func_context(&mut self, mut wd: WordDesc) {
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
		wd.span = self.span;
		wd.text = wd.text.trim().to_string();
		self.tokens.push(Tk { tk_type: TkType::FuncBody, wd });
		self.push_ctx(DeadEnd);
	}
	fn case_context(&mut self, mut wd: WordDesc) -> RshResult<()> {
		use crate::interp::token::TkState::*;
		let span = wd.span;
		self.push_ctx(SingleVarDec);
		self.vardec_context(take(&mut wd))?;
		if self.char_stream.front().is_some_and(|ch| matches!(*ch, ';' | '\n')) {
			self.advance();
			self.tokens.push(Tk::cmdsep(&wd,span.end + 1));
			self.span.start = self.span.end;
		}
		while !self.char_stream.is_empty() {
			// Get pattern
			let mut pat = String::new();
			while let Some(ch) = self.advance() {
				if ch == ')' { break }
				pat.push(ch);
			}
			wd.text = pat;
			if wd.text == "esac" {
				self.tokens.push(Tk { tk_type: TkType::Esac, wd: take(&mut wd) });
				break
			}
			let span = wd.span;
			self.tokens.push(Tk { tk_type: TkType::CasePat, wd: take(&mut wd) });

			// Get body
			let mut body = String::new();
			let mut prev_char = None;
			while let Some(ch) = self.advance() {
				if ch == ';' && prev_char == Some(';') {
					break
				}
				prev_char = Some(ch);
				body.push(ch);
			}
			let mut body_tokenizer = RshTokenizer::new(&body);
			let mut body_tokens = vec![];
			while let Ok(mut block) = body_tokenizer.tokenize_one(TkizerCtx::new()) {
				if !block.is_empty() {
					body_tokens.append(&mut block);
				}
			}
			self.tokens.append(&mut body_tokens);
			wd = wd.set_span(span);
		}
		self.push_ctx(DeadEnd);
		Ok(())
	}
	fn complete_word(&mut self, mut wd: WordDesc) -> WordDesc {
		let mut dub_quote = false;
		let mut sng_quote = false;
		let mut paren_stack = vec![];
		let mut bracket_stack = vec![];
		while self.char_stream.front().is_some_and(|ch| ch.is_whitespace()) {
			self.advance();
		}
		self.span.start = self.span.end;
		while let Some(ch) = self.advance() {
			wd = wd.set_span(self.span);
			match ch {
				'\\' => {
					wd = wd.add_char(ch);
					if let Some(char) = self.advance() {
						wd = wd.add_char(char)
					}
				}
				'[' if paren_stack.is_empty() => {
					bracket_stack.push(ch);
					wd = wd.add_char(ch)
				}
				']' if !bracket_stack.is_empty() => {
					bracket_stack.pop();
					wd = wd.add_char(ch)
				}
				'(' if bracket_stack.is_empty() => {
					paren_stack.push(ch);
					wd = wd.add_char(ch)
				}
				')' if !paren_stack.is_empty() => {
					paren_stack.pop();
					wd = wd.add_char(ch)
				}
				'\'' if !dub_quote => {
					// Single quote handling
					sng_quote = !sng_quote;
					wd = wd.add_char(ch);
				}
				'"' if !sng_quote => {
					// Double quote handling
					dub_quote = !dub_quote;
					wd = wd.add_char(ch);
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
					// Preserve cmdsep for tokenizing
					self.char_stream.push_front(ch);
					self.span.end -= 1;
					wd.span = self.span;
					break;
				}
				' ' | '\t' => {
					// Whitespace handling
					self.char_stream.push_front(ch);
					self.span.end -= 1;
					wd.span.end -= 1;
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
		wd
	}
	fn clean_tokens(&mut self) {
		let mut buffer = VecDeque::new();
		let mut tokens = VecDeque::from(take(&mut self.tokens));
		while let Some(token) = tokens.pop_front() {
			if matches!(token.tk_type, TkType::SOI | TkType::EOI | TkType::Cmdsep) || !token.text().is_empty() {
				buffer.push_back(token);
			}
		}
		self.tokens.extend(buffer.drain(..));
	}
}

// TODO: Case tests, select tests
#[cfg(test)]
mod tests {
	use nix::unistd::{getuid, User};

	use crate::{interp::token::TkizerCtx, shellenv::write_vars};

	use super::*;
	fn token(tk_type: TkType, text: &str, start: usize, end: usize, flags: WdFlags) -> Tk {
		Tk {
			tk_type,
			wd: WordDesc {
				text: text.to_string(),
				span: Span::from(start,end),
				flags
			}
		}
	}

	#[test]
	fn tokenizer_simple() {
		let input = "echo hello";
		let mut tokenizer = RshTokenizer::new(input);

		let tokens = tokenizer.tokenize_one(TkizerCtx::new()).unwrap();
		let expected = vec![
			Tk::start_of_input(),
			token(TkType::Ident, "echo", 0, 4, WdFlags::BUILTIN),
			token(TkType::String, "hello", 5, 10, WdFlags::IS_ARG),
		];
		pretty_assertions::assert_eq!(tokens,expected);
	}

	#[test]
	fn tokenizer_multiple_args() {
		let input = "ls -l /home/user";
		let mut tokenizer = RshTokenizer::new(input);

		let tokens = tokenizer.tokenize_one(TkizerCtx::new()).unwrap();
		let expected = vec![
			Tk::start_of_input(),
			token(TkType::Ident, "ls", 0, 2, WdFlags::empty()),
			token(TkType::String, "-l", 3, 5, WdFlags::IS_ARG),
			token(TkType::String, "/home/user", 6, 16, WdFlags::IS_ARG),
		];
		pretty_assertions::assert_eq!(tokens, expected);
	}

	#[test]
	fn tokenizer_quoted_arg() {
		let input = "echo \"Hello, world!\"";
		let mut tokenizer = RshTokenizer::new(input);

		let tokens = tokenizer.tokenize_one(TkizerCtx::new()).unwrap();
		let expected = vec![
			Tk::start_of_input(),
			token(TkType::Ident, "echo", 0, 4, WdFlags::BUILTIN),
			token(TkType::String, "Hello, world!", 5, 20, WdFlags::IS_ARG),
		];
		pretty_assertions::assert_eq!(tokens, expected);
	}

	#[test]
	fn tokenizer_single_quoted_arg() {
		let input = "echo 'single quoted arg'";
		let mut tokenizer = RshTokenizer::new(input);

		let tokens = tokenizer.tokenize_one(TkizerCtx::new()).unwrap();
		let expected = vec![
			Tk::start_of_input(),
			token(TkType::Ident, "echo", 0, 4, WdFlags::BUILTIN),
			token(TkType::String, "single quoted arg", 5, 24, WdFlags::IS_ARG),
		];
		pretty_assertions::assert_eq!(tokens, expected);
	}

	#[test]
	fn tokenizer_variable_expansion() {
		let input = "echo $USER";
		let user = User::from_uid(getuid()).unwrap().unwrap().name;
		let mut tokenizer = RshTokenizer::new(input);

		let tokens = tokenizer.tokenize_one(TkizerCtx::new()).unwrap();
		let expected = vec![
			Tk::start_of_input(),
			token(TkType::Ident, "echo", 0, 4, WdFlags::BUILTIN),
			token(TkType::String, user.as_str(), 5, 10, WdFlags::IS_ARG),
		];
		pretty_assertions::assert_eq!(tokens, expected);
	}

	#[test]
	fn tokenizer_pipeline() {
		let input = "ls | grep file";
		let mut tokenizer = RshTokenizer::new(input);

		let tokens = tokenizer.tokenize_one(TkizerCtx::new()).unwrap();
		let expected = vec![
			Tk::start_of_input(),
			token(TkType::Ident, "ls", 0, 2, WdFlags::empty()),
			token(TkType::Pipe, "|", 3, 4, WdFlags::IS_OP),
			token(TkType::Ident, "grep", 5, 9, WdFlags::empty()),
			token(TkType::String, "file", 10, 14, WdFlags::IS_ARG),
		];
		pretty_assertions::assert_eq!(tokens, expected);
	}


	#[test]
	fn tokenizer_redirection() {
		let input = "echo hello > output.txt";
		let mut tokenizer = RshTokenizer::new(input);

		let tokens = tokenizer.tokenize_one(TkizerCtx::new()).unwrap();
		let expected = vec![
			Tk::start_of_input(),
			token(TkType::Ident, "echo", 0, 4, WdFlags::BUILTIN),
			token(TkType::String, "hello", 5, 10, WdFlags::IS_ARG),
			token(TkType::Redirection { redir: Redir { fd_source: 1, op: RedirType::Output, fd_target: None, file_target: None } }, ">", 11, 12, WdFlags::IS_OP),
			token(TkType::String, "output.txt", 13, 23, WdFlags::IS_ARG),
		];
		pretty_assertions::assert_eq!(tokens, expected);
	}

	#[test]
	fn tokenizer_cmd_sub() {
		let input = "echo $(echo hi)";
		let mut tokenizer = RshTokenizer::new(input);

		let tokens = tokenizer.tokenize_one(TkizerCtx::new()).unwrap();
		let expected = vec![
			Tk::start_of_input(),
			token(TkType::Ident, "echo", 0, 4, WdFlags::BUILTIN),
			token(TkType::String, "hi", 5, 15, WdFlags::empty()),
		];
		pretty_assertions::assert_eq!(tokens, expected);
	}

	#[test]
	fn tokenizer_subshell() {
		let input = "(echo hi)";
		let mut tokenizer = RshTokenizer::new(input);

		let tokens = tokenizer.tokenize_one(TkizerCtx::new()).unwrap();
		let expected = vec![
			Tk::start_of_input(),
			token(TkType::Subshell, "echo hi", 0, 9, WdFlags::empty()),
		];
		pretty_assertions::assert_eq!(tokens, expected);
	}

	#[test]
	fn tokenizer_subshell_multiline() {
		let input = "(#!python
print(\"hello world\")
)";
		let mut tokenizer = RshTokenizer::new(input);

		let tokens = tokenizer.tokenize_one(TkizerCtx::new()).unwrap();
		let expected = vec![
			Tk::start_of_input(),
			token(
				TkType::Subshell,
				"#!python\nprint(\"hello world\")\n",
				0,
				input.len(),
				WdFlags::empty(),
			),
		];
		pretty_assertions::assert_eq!(tokens, expected);
	}

	#[test]
	fn tokenizer_func_def() {
		let input = "func() {
			echo hi
			cat file.txt
		}";
		let mut tokenizer = RshTokenizer::new(input);

		let tokens = tokenizer.tokenize_one(TkizerCtx::new()).unwrap();
		let expected = vec![
			Tk::start_of_input(),
			token(TkType::FuncDef, "func", 0, 6, WdFlags::empty()),
			token(TkType::FuncBody, "echo hi\n\t\t\tcat file.txt", 7, 39, WdFlags::empty()),
		];
		pretty_assertions::assert_eq!(tokens, expected);
	}

	#[test]
	fn tokenizer_expand_params() {
		let input = "echo $@";
		let mut tokenizer = RshTokenizer::new(input);
		write_vars(|v| v.set_param("1".into(), "one".into())).unwrap();
		write_vars(|v| v.set_param("2".into(), "two".into())).unwrap();
		write_vars(|v| v.set_param("3".into(), "three".into())).unwrap();
		write_vars(|v| v.set_param("4".into(), "four".into())).unwrap();

		let tokenizer = tokenizer.tokenize_one(TkizerCtx::new()).unwrap();

		insta::assert_debug_snapshot!(tokenizer)
	}

	#[test]
	fn tokenizer_if() {
		let input = "if true; then echo hi; elif false; then echo hello; else echo greetings; fi";
		let mut tokenizer = RshTokenizer::new(input);

		let tokens = tokenizer.tokenize_one(TkizerCtx::new()).unwrap();
		let expected = vec![
			Tk::start_of_input(),
			token(TkType::If, "if", 0, 2, WdFlags::KEYWORD),
			token(TkType::Ident, "true", 3, 7, WdFlags::empty()),
			token(TkType::Cmdsep, "", 8, 8, WdFlags::empty()),
			token(TkType::Then, "then", 9, 13, WdFlags::KEYWORD),
			token(TkType::Ident, "echo", 14, 18, WdFlags::BUILTIN),
			token(TkType::String, "hi", 19, 21, WdFlags::IS_ARG),
			token(TkType::Cmdsep, "", 22, 22, WdFlags::empty()),
			token(TkType::Elif, "elif", 23, 27, WdFlags::KEYWORD),
			token(TkType::Ident, "false", 28, 33, WdFlags::empty()),
			token(TkType::Cmdsep, "", 34, 34, WdFlags::empty()),
			token(TkType::Then, "then", 35, 39, WdFlags::KEYWORD),
			token(TkType::Ident, "echo", 40, 44, WdFlags::BUILTIN),
			token(TkType::String, "hello", 45, 50, WdFlags::IS_ARG),
			token(TkType::Cmdsep, "", 51, 51, WdFlags::empty()),
			token(TkType::Else, "else", 52, 56, WdFlags::KEYWORD),
			token(TkType::Ident, "echo", 57, 61, WdFlags::BUILTIN),
			token(TkType::String, "greetings", 62, 71, WdFlags::IS_ARG),
			token(TkType::Cmdsep, "", 72, 72, WdFlags::empty()),
			token(TkType::Fi, "fi", 73, 75, WdFlags::KEYWORD),
			];
		pretty_assertions::assert_eq!(tokens, expected);
	}

	#[test]
	fn tokenizer_for_loop() {
		let input = "for i in 1 2 3; do echo $i; done";
		let mut tokenizer = RshTokenizer::new(input);

		let tokens = tokenizer.tokenize_one(TkizerCtx::new()).unwrap();
		let expected = vec![
			Tk::start_of_input(),
			token(TkType::For, "for", 0, 3, WdFlags::KEYWORD),
			token(TkType::Ident, "i", 4, 5, WdFlags::empty()),
			token(TkType::In, "in", 6, 8, WdFlags::KEYWORD),
			token(TkType::Ident, "1", 9, 11, WdFlags::empty()),
			token(TkType::Ident, "2", 11, 13, WdFlags::empty()),
			token(TkType::Ident, "3", 13, 14, WdFlags::empty()),
			token(TkType::Cmdsep, "", 15, 15, WdFlags::empty()),
			token(TkType::Do, "do", 16, 18, WdFlags::KEYWORD),
			token(TkType::Ident, "echo", 19, 23, WdFlags::BUILTIN),
			token(TkType::Ident, "$i", 24, 26, WdFlags::IS_ARG),
			token(TkType::Cmdsep, "", 27, 27, WdFlags::empty()),
			token(TkType::Done, "done", 28, 32, WdFlags::KEYWORD),
		];
		pretty_assertions::assert_eq!(tokens, expected);
	}

	#[test]
	fn tokenizer_while_loop() {
		let input = "while true; do echo working; done";
		let mut tokenizer = RshTokenizer::new(input);

		let tokens = tokenizer.tokenize_one(TkizerCtx::new()).unwrap();
		let expected = vec![
			Tk::start_of_input(),
			token(TkType::While, "while", 0, 5, WdFlags::KEYWORD),
			token(TkType::Ident, "true", 6, 10, WdFlags::empty()),
			token(TkType::Cmdsep, "", 11, 11, WdFlags::empty()),
			token(TkType::Do, "do", 12, 14, WdFlags::KEYWORD),
			token(TkType::Ident, "echo", 15, 19, WdFlags::BUILTIN),
			token(TkType::String, "working", 20, 27, WdFlags::IS_ARG),
			token(TkType::Cmdsep, "", 28, 28, WdFlags::empty()),
			token(TkType::Done, "done", 29, 33, WdFlags::KEYWORD),
		];
		pretty_assertions::assert_eq!(tokens, expected);
	}

	#[test]
	fn tokenizer_until_loop() {
		let input = "until true; do echo waiting; done";
		let mut tokenizer = RshTokenizer::new(input);

		let tokens = tokenizer.tokenize_one(TkizerCtx::new()).unwrap();
		let expected = vec![
			Tk::start_of_input(),
			token(TkType::Until, "until", 0, 5, WdFlags::KEYWORD),
			token(TkType::Ident, "true", 6, 10, WdFlags::empty()),
			token(TkType::Cmdsep, "", 11, 11, WdFlags::empty()),
			token(TkType::Do, "do", 12, 14, WdFlags::KEYWORD),
			token(TkType::Ident, "echo", 15, 19, WdFlags::BUILTIN),
			token(TkType::String, "waiting", 20, 27, WdFlags::IS_ARG),
			token(TkType::Cmdsep, "", 28, 28, WdFlags::empty()),
			token(TkType::Done, "done", 29, 33, WdFlags::KEYWORD),
		];
		pretty_assertions::assert_eq!(tokens, expected);
	}

	#[test]
	fn tokenizer_final_boss() {
		let input = "if while if true; then echo while condition; fi; do if true; then echo inside first while; fi; done; then echo wow; elif until while if true; then echo double loop; fi; do if true; then echo another double loop; fi; done; do echo; done; then echo again; else echo; fi";
		let mut tokenizer = RshTokenizer::new(input);

		let tokens = tokenizer.tokenize_one(TkizerCtx::new()).unwrap();
		let expected = vec![
			Tk::start_of_input(),
			// if while if true
			token(TkType::If, "if", 0, 2, WdFlags::KEYWORD),
			token(TkType::While, "while", 3, 8, WdFlags::KEYWORD),
			token(TkType::If, "if", 9, 11, WdFlags::KEYWORD),
			token(TkType::Ident, "true", 12, 16, WdFlags::empty()),
			token(TkType::Cmdsep, "", 17, 17, WdFlags::empty()),
			// then echo while condition
			token(TkType::Then, "then", 18, 22, WdFlags::KEYWORD),
			token(TkType::Ident, "echo", 23, 27, WdFlags::BUILTIN),
			token(TkType::String, "while", 28, 33, WdFlags::IS_ARG),
			token(TkType::String, "condition", 34, 43, WdFlags::IS_ARG),
			token(TkType::Cmdsep, "", 44, 44, WdFlags::empty()),
			// fi
			token(TkType::Fi, "fi", 45, 47, WdFlags::KEYWORD),
			token(TkType::Cmdsep, "", 48, 48, WdFlags::empty()),
			// do if true; then echo inside first while; fi; done
			token(TkType::Do, "do", 49, 51, WdFlags::KEYWORD),
			token(TkType::If, "if", 52, 54, WdFlags::KEYWORD),
			token(TkType::Ident, "true", 55, 59, WdFlags::empty()),
			token(TkType::Cmdsep, "", 60, 60, WdFlags::empty()),
			token(TkType::Then, "then", 61, 65, WdFlags::KEYWORD),
			token(TkType::Ident, "echo", 66, 70, WdFlags::BUILTIN),
			token(TkType::String, "inside", 71, 77, WdFlags::IS_ARG),
			token(TkType::String, "first", 78, 83, WdFlags::IS_ARG),
			token(TkType::String, "while", 84, 89, WdFlags::IS_ARG),
			token(TkType::Cmdsep, "", 90, 90, WdFlags::empty()),
			token(TkType::Fi, "fi", 91, 93, WdFlags::KEYWORD),
			token(TkType::Cmdsep, "", 94, 94, WdFlags::empty()),
			token(TkType::Done, "done", 95, 99, WdFlags::KEYWORD),
			token(TkType::Cmdsep, "", 100, 100, WdFlags::empty()),
			// then echo wow
			token(TkType::Then, "then", 101, 105, WdFlags::KEYWORD),
			token(TkType::Ident, "echo", 106, 110, WdFlags::BUILTIN),
			token(TkType::String, "wow", 111, 114, WdFlags::IS_ARG),
			token(TkType::Cmdsep, "", 115, 115, WdFlags::empty()),
			// elif until while if true; then echo double loop; fi; do if true; then echo another double loop; fi; do echo; done; then echo again
			token(TkType::Elif, "elif", 116, 120, WdFlags::KEYWORD),
			token(TkType::Until, "until", 121, 126, WdFlags::KEYWORD),
			token(TkType::While, "while", 127, 132, WdFlags::KEYWORD),
			token(TkType::If, "if", 133, 135, WdFlags::KEYWORD),
			token(TkType::Ident, "true", 136, 140, WdFlags::empty()),
			token(TkType::Cmdsep, "", 141, 141, WdFlags::empty()),
			token(TkType::Then, "then", 142, 146, WdFlags::KEYWORD),
			token(TkType::Ident, "echo", 147, 151, WdFlags::BUILTIN),
			token(TkType::String, "double", 152, 158, WdFlags::IS_ARG),
			token(TkType::String, "loop", 159, 163, WdFlags::IS_ARG),
			token(TkType::Cmdsep, "", 164, 164, WdFlags::empty()),
			token(TkType::Fi, "fi", 165, 167, WdFlags::KEYWORD),
			token(TkType::Cmdsep, "", 168, 168, WdFlags::empty()),
			token(TkType::Do, "do", 169, 171, WdFlags::KEYWORD),
			token(TkType::If, "if", 172, 174, WdFlags::KEYWORD),
			token(TkType::Ident, "true", 175, 179, WdFlags::empty()),
			token(TkType::Cmdsep, "", 180, 180, WdFlags::empty()),
			token(TkType::Then, "then", 181, 185, WdFlags::KEYWORD),
			token(TkType::Ident, "echo", 186, 190, WdFlags::BUILTIN),
			token(TkType::String, "another", 191, 198, WdFlags::IS_ARG),
			token(TkType::String, "double", 199, 205, WdFlags::IS_ARG),
			token(TkType::String, "loop", 206, 210, WdFlags::IS_ARG),
			token(TkType::Cmdsep, "", 211, 211, WdFlags::empty()),
			token(TkType::Fi, "fi", 212, 214, WdFlags::KEYWORD),
			token(TkType::Cmdsep, "", 215, 215, WdFlags::empty()),
			token(TkType::Done, "done", 216, 220, WdFlags::KEYWORD),
			token(TkType::Cmdsep, "", 221, 221, WdFlags::empty()),
			token(TkType::Do, "do", 222, 224, WdFlags::KEYWORD),
			token(TkType::Ident, "echo", 225, 229, WdFlags::BUILTIN),
			token(TkType::Cmdsep, "", 230, 230, WdFlags::empty()),
			token(TkType::Done, "done", 231, 235, WdFlags::KEYWORD),
			token(TkType::Cmdsep, "", 236, 236, WdFlags::empty()),
			token(TkType::Then, "then", 237, 241, WdFlags::KEYWORD),
			token(TkType::Ident, "echo", 242, 246, WdFlags::BUILTIN),
			token(TkType::String, "again", 247, 252, WdFlags::IS_ARG),
			token(TkType::Cmdsep, "", 253, 253, WdFlags::empty()),
			// else echo
			token(TkType::Else, "else", 254, 258, WdFlags::KEYWORD),
			token(TkType::Ident, "echo", 259, 263, WdFlags::BUILTIN),
			token(TkType::Cmdsep, "", 264, 264, WdFlags::empty()),
			// fi
			token(TkType::Fi, "fi", 265, 267, WdFlags::KEYWORD),
			];
		pretty_assertions::assert_eq!(tokens, expected);
	}
}
