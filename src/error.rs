use std::{fmt::{Debug, Display}, hash::Hash};

use nix::errno::Errno;
use pest::{error::ErrorVariant, iterators::Pair, Span};

use crate::{helper, Rule};

pub trait LashErrExt<T> {
	/// Transforms a LashResult into an Option
	/// If LashResult is an error, this function will display it before returning None
	fn catch(self) -> Option<T>;
}

impl<T> LashErrExt<T> for Result<T,LashErr> {
	fn catch(self) -> Option<T> {
		match self {
			Err(err) => {
				eprintln!("{}",err);
				None
			}
			Ok(thing) => Some(thing)
		}
	}
}

/// These error types represent different stages of the lash error reporting mechanism
/// A low error is thrown in deep contexts where there is no token to blame.
/// A high error is created when a low error is caught in a context where there is a token to blame.
#[derive(Debug)]
pub enum LashErr {
	Low(LashErrLow),
	High(LashErrHigh)
}

impl From<std::io::Error> for LashErr {
	fn from(value: std::io::Error) -> Self {
		Self::Low(LashErrLow::IoError(value.to_string()))
	}
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
#[derive(Debug,Clone)]
pub enum LashErrLow {
	Parse(String),
	IoError(String),
	ErrNo(Errno),
	CmdNotFound(String),
	BadPermission(String),
	BadFD(String),
	InvalidSyntax(String),
	InternalErr(String),
	IndexErr(String),
	ExecFailed(String),

	// Not actual errors, used to propagate logic from commands like `exit` and `return`
	CleanExit(i32),
	FuncReturn(i32),
	LoopCont,
	LoopBreak(i32),
}


impl LashErrLow {
	pub fn from_io() -> Self {
		Self::IoError(std::io::Error::last_os_error().to_string())
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
			LashErrLow::IndexErr(msg) => write!(f,"Index Error: {}",msg),
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

#[derive(Debug,Clone)]
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
		Self::blame(pair, LashErrLow::IoError(std::io::Error::last_os_error().to_string()))
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
