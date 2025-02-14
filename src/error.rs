use crate::prelude::*;

use nix::errno::Errno;

use crate::helper;

pub type SlashResult<T> = Result<T,SlashErr>;

pub trait SlashErrExt<T> {
	/// Transforms a SlashResult into an Option
	/// If SlashResult is an error, this function will display it before returning None
	fn catch(self) -> Option<T>;
	/// This method will transform the contained SlashErr, if the result is an error
	/// It takes a pair to blame the error on, if the contained error is SlashErrLow,
	/// then it will be converted to a SlashErrHigh
	/// If the contained error is SlashErrHigh, the blamed pair will be replaced
	fn blame(self, pair: Pair<Rule>) -> Result<T,SlashErr>;
	/// The same as blame(), though does not overwrite the contained pair in a SlashErrHigh.
	/// Will still transform a SlashErrLow into a SlashErrHigh
	fn blame_no_overwrite(self, pair: Pair<Rule>) -> Result<T,SlashErr>;
}

impl<T> SlashErrExt<T> for Result<T,SlashErr> {
	fn blame(self, pair: Pair<Rule>) -> Result<T,SlashErr> {
		match self {
			Ok(thing) => Ok(thing),
			Err(err) => {
				let new_err = match err {
					Low(low) => SlashErrHigh::blame(pair, low),
					High(high) => {
						let low = high.get_err();
						SlashErrHigh::blame(pair, low.clone())
					}
				};
				Err(High(new_err))
			}
		}
	}
	fn blame_no_overwrite(self, pair: Pair<Rule>) -> Result<T,SlashErr> {
		match self {
			Ok(thing) => Ok(thing),
			Err(err) => {
				let new_err = match err {
					Low(low) => SlashErrHigh::blame(pair, low),
					High(high) => high
				};
				Err(High(new_err))
			}
		}
	}
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

/// These error types represent different stages of the slash error reporting mechanism
/// A low error is thrown in deep contexts where there is no token to blame.
/// A high error is created when a low error is caught in a context where there is a token to blame.
#[derive(Debug,Clone)]
pub enum SlashErr {
	Low(SlashErrLow),
	High(SlashErrHigh)
}

impl From<std::io::Error> for SlashErr {
	fn from(value: std::io::Error) -> Self {
		Self::Low(SlashErrLow::IoError(value.to_string()))
	}
}

impl From<pest::error::Error<Rule>> for SlashErr {
	fn from(value: pest::error::Error<Rule>) -> Self {
		Self::Low(SlashErrLow::Parse(value.to_string()))
	}
}

impl Display for SlashErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			SlashErr::Low(low) => write!(f,"{}",low),
			SlashErr::High(high) => write!(f,"{}",high),
		}
	}
}

/// Simple errors
#[derive(Debug,Clone)]
pub enum SlashErrLow {
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

impl SlashErrLow {
	pub fn from_io() -> Self {
		Self::IoError(std::io::Error::last_os_error().to_string())
	}
}

impl Display for SlashErrLow {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			SlashErrLow::Parse(msg) => write!(f,"Parse Error: {}",msg),
			SlashErrLow::IoError(error) => write!(f,"I/O Error: {}",error.to_string()),
			SlashErrLow::ErrNo(no) => write!(f,"ERRNO: {}",no.to_string()),
			SlashErrLow::BadFD(msg) => write!(f,"{}",msg),
			SlashErrLow::InvalidSyntax(msg) => write!(f,"Syntax Error: {}",msg),
			SlashErrLow::InternalErr(msg) => write!(f,"Internal Error: {}",msg),
			SlashErrLow::IndexErr(msg) => write!(f,"Index Error: {}",msg),
			SlashErrLow::ExecFailed(msg) => write!(f,"Execution Failed: {}",msg),
			SlashErrLow::CmdNotFound(name) => write!(f,"Command not found: {}",name),
			SlashErrLow::BadPermission(name) => write!(f,"Permission denied: {}",name),
			SlashErrLow::FuncReturn(_) => write!(f, "Found return outside of function"),
			SlashErrLow::LoopCont => write!(f, "Found continue outside of loop"),
			SlashErrLow::LoopBreak(_) => write!(f, "Found break outside of loop"),
			SlashErrLow::CleanExit(_) => write!(f, ""),
		}
	}
}

#[derive(Debug,Clone)]
pub struct SlashErrHigh {
	pest_err: String,
	low_err: SlashErrLow
}

impl Display for SlashErrHigh {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f,"{}",self.pest_err)
	}
}

impl SlashErrHigh {
	pub fn blame(pair: Pair<Rule>, low_err: SlashErrLow) -> Self {
		let message = low_err.to_string();
		let pest_err = helper::build_slash_err::<Rule>(pair, message);
		Self { pest_err, low_err }
	}

	pub fn parse_err(msg: impl Into<String>, pair: Pair<Rule>) -> Self {
		Self::blame(pair, SlashErrLow::Parse(msg.into()))
	}

	pub fn io_err(pair: Pair<Rule>) -> Self {
		Self::blame(pair, SlashErrLow::IoError(std::io::Error::last_os_error().to_string()))
	}

	pub fn bad_fd(msg: impl Into<String>, pair: Pair<Rule>) -> Self {
		Self::blame(pair, SlashErrLow::BadFD(msg.into()))
	}

	pub fn cmd_not_found(name: impl Into<String>, pair: Pair<Rule>) -> Self {
		Self::blame(pair, SlashErrLow::CmdNotFound(name.into()))
	}

	pub fn no_permission(name: impl Into<String>, pair: Pair<Rule>) -> Self {
		Self::blame(pair, SlashErrLow::BadPermission(name.into()))
	}

	pub fn syntax_err(msg: impl Into<String>, pair: Pair<Rule>) -> Self {
		Self::blame(pair, SlashErrLow::InvalidSyntax(msg.into()))
	}

	pub fn internal_err(msg: impl Into<String>, pair: Pair<Rule>) -> Self {
		Self::blame(pair, SlashErrLow::InternalErr(msg.into()))
	}

	pub fn exec_err(msg: impl Into<String>, pair: Pair<Rule>) -> Self {
		Self::blame(pair, SlashErrLow::ExecFailed(msg.into()))
	}

	pub fn get_err(&self) -> &SlashErrLow {
		&self.low_err
	}
}

pub fn infer_parse_err(input: &str, err: pest::error::Error<Rule>) -> String {
	let mut err_msg = err.to_string();

	err_msg
}
