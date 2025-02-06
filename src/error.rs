use std::{fmt::{Debug, Display}, hash::Hash};

use nix::errno::Errno;
use pest::{error::ErrorVariant, iterators::Pair, Span};

use crate::{helper, Rule};

// These error types represent different stages of the lash error reporting mechanism
// A low error is thrown in deep contexts where there is no token to blame.
// A mid error is created when a low error is caught in a context where there is a token to blame.
// A high error is created when a mid error is caught in the top level context

#[derive(Debug)]
pub struct BlameSpan {
	start: usize,
	end: usize
}

#[derive(Debug)]
pub struct BlamePair {
	input: String,
	span: BlameSpan
}

impl BlamePair {
	fn start(&self) -> usize {
		self.span.start
	}
	fn end(&self) -> usize {
		self.span.end
	}
}

impl BlamePair {
	pub fn from(pair: Pair<Rule>) -> Self {
		let input = pair.get_input().to_string();
		let pair_span = pair.as_span();
		let span = BlameSpan { start: pair_span.start(), end: pair_span.end() };
		Self { input, span }
	}
}

#[derive(Debug)]
pub enum LashErr {
	Low(LashErrLow),
	High(LashErrHigh)
}

impl Display for LashErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			LashErr::Low(low) => write!(f,"{}",low),
			LashErr::High(high) => write!(f,"{}",high),
		}
	}
}

/// Simple errors
#[derive(Debug)]
pub enum LashErrLow {
	Parse(String),
	IoError(std::io::Error),
	ErrNo(Errno),
	CmdNotFound(String),
	BadPermission(String),
	BadFD(String),
	InvalidSyntax(String),
	InternalErr(String),
	ExecFailed(String),

	// Not actual errors, used to propagate logic from commands like `exit` and `return`
	CleanExit(i32),
	FuncReturn(i32),
	LoopCont,
	LoopBreak(i32),
}

impl LashErrLow {
	pub fn from_io() -> Self {
		Self::IoError(std::io::Error::last_os_error())
	}
}

impl Display for LashErrLow {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			LashErrLow::Parse(msg) => write!(f,"Parse Error: {}",msg),
			LashErrLow::IoError(error) => write!(f,"I/O Error: {}",error.to_string()),
			LashErrLow::ErrNo(no) => write!(f,"ERRNO: {}",no.to_string()),
			LashErrLow::BadFD(msg) => write!(f,"{}",msg),
			LashErrLow::InvalidSyntax(msg) => write!(f,"Syntax Error: {}",msg),
			LashErrLow::InternalErr(msg) => write!(f,"Internal Error: {}",msg),
			LashErrLow::ExecFailed(msg) => write!(f,"Execution Failed: {}",msg),
			LashErrLow::CmdNotFound(name) => write!(f,"Command not found: {}",name),
			LashErrLow::BadPermission(name) => write!(f,"Permission denied: {}",name),
			LashErrLow::CleanExit(_) |
			LashErrLow::FuncReturn(_) |
			LashErrLow::LoopCont |
			LashErrLow::LoopBreak(_) => write!(f, ""),
		}
	}
}

#[derive(Debug)]
pub struct LashErrHigh {
	pest_err: String,
	low_err: LashErrLow
}

impl Display for LashErrHigh {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f,"{}",self.pest_err)
	}
}

impl LashErrHigh {
	pub fn blame(pair: Pair<Rule>, low_err: LashErrLow) -> Self {
		let message = low_err.to_string();
		let pest_err = helper::build_lash_err::<Rule>(pair, message);
		Self { pest_err, low_err }
	}

	pub fn parse_err(msg: impl Into<String>, pair: Pair<Rule>) -> Self {
		Self::blame(pair, LashErrLow::Parse(msg.into()))
	}

	pub fn io_err(pair: Pair<Rule>) -> Self {
		Self::blame(pair, LashErrLow::IoError(std::io::Error::last_os_error()))
	}

	pub fn bad_fd(msg: impl Into<String>, pair: Pair<Rule>) -> Self {
		Self::blame(pair, LashErrLow::BadFD(msg.into()))
	}

	pub fn cmd_not_found(name: impl Into<String>, pair: Pair<Rule>) -> Self {
		Self::blame(pair, LashErrLow::CmdNotFound(name.into()))
	}

	pub fn no_permission(name: impl Into<String>, pair: Pair<Rule>) -> Self {
		Self::blame(pair, LashErrLow::BadPermission(name.into()))
	}

	pub fn syntax_err(msg: impl Into<String>, pair: Pair<Rule>) -> Self {
		Self::blame(pair, LashErrLow::InvalidSyntax(msg.into()))
	}

	pub fn internal_err(msg: impl Into<String>, pair: Pair<Rule>) -> Self {
		Self::blame(pair, LashErrLow::InternalErr(msg.into()))
	}

	pub fn exec_err(msg: impl Into<String>, pair: Pair<Rule>) -> Self {
		Self::blame(pair, LashErrLow::ExecFailed(msg.into()))
	}

	pub fn get_err(&self) -> &LashErrLow {
		&self.low_err
	}
}
