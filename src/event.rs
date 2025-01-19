use std::{ffi::c_int, fmt::Display, io, panic::Location, sync::mpsc::{self, Receiver, Sender}};
use libc::{getpid, tcgetpgrp};
use signal_hook::iterator::Signals;


use crate::{deconstruct, execute::{self, RshWait}, interp::{helper, parse::{descend, NdType, Node, Span}, token::RshTokenizer}, prompt, shellenv::{self, read_jobs, read_meta, write_jobs, write_meta}, signal::{self, }, RshResult};

#[derive(Debug, Clone, PartialEq)]
pub enum ShError {
	CommandNotFound(String, Span),
	InvalidSyntax(String, Span),
	ParsingError(String, Span),
	ExecFailed(String, i32, Span),
	IoError(String, u32, String),
	InternalError(String),
}

impl ShError {
	#[track_caller]
	pub fn from_io() -> Self {
		let err = io::Error::last_os_error();
		let loc = Location::caller();
		eprintln!("{}, {}, {}",loc.line(),loc.file(), err);
		ShError::IoError(err.to_string(),loc.line(),loc.file().to_string())
	}
	pub fn from_execf(msg: &str, code: i32, span: Span) -> Self {
		ShError::ExecFailed(msg.to_string(), code, span)
	}
	pub fn from_parse(msg: &str, span: Span) -> Self {
		ShError::ParsingError(msg.to_string(), span)
	}
	pub fn from_syntax(msg: &str, span: Span) -> Self {
		ShError::InvalidSyntax(msg.to_string(), span)
	}
	pub fn from_no_cmd(msg: &str, span: Span) -> Self {
		ShError::CommandNotFound(msg.to_string(), span)
	}
	pub fn from_internal(msg: &str) -> Self {
		ShError::InternalError(msg.to_string())
	}
	// This is used in the context of functions
	// To prevent the error from trying to use the span
	// Of the offending command that is inside of the function
	pub fn overwrite_span(&self, new_span: Span) -> Self {
		match self {
			ShError::IoError(err,loc,file) => ShError::IoError(err.to_string(),*loc,file.to_string()),
			ShError::CommandNotFound(msg,_) => ShError::CommandNotFound(msg.to_string(),new_span),
			ShError::InvalidSyntax(msg,_) => ShError::InvalidSyntax(msg.to_string(),new_span),
			ShError::ParsingError(msg,_) => ShError::ParsingError(msg.to_string(),new_span),
			ShError::ExecFailed(msg,code,_) => ShError::ExecFailed(msg.to_string(),*code,new_span),
			ShError::InternalError(msg) => ShError::InternalError(msg.to_string()),
		}
	}
	pub fn is_fatal(&self) -> bool {
		match self {
			ShError::IoError(..) => true,
			ShError::CommandNotFound(..) => false,
			ShError::ExecFailed(..) => false,
			ShError::ParsingError(..) => false,
			ShError::InvalidSyntax(..) => false,
			ShError::InternalError(..) => false,
		}
	}
}

impl Display for ShError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			ShError::CommandNotFound(cmd, span) => {
				write!(f, "{}", cmd)
			}
			ShError::InvalidSyntax(msg, span) => {
				write!(f, "{}", msg)
			}
			ShError::ParsingError(msg, span) => {
				write!(f, "{}", msg)
			}
			ShError::ExecFailed(cmd, code, span) => {
				write!(f, "{}", cmd)
			}
			ShError::IoError(msg, code, path) => {
				write!(f, "{}", msg)
			}
			ShError::InternalError(msg) => {
				write!(f, "{}", msg)
			}
		}
	}
}

#[derive(Debug,Clone)]
pub enum ShEvent {
	Input(String),
	NewNodeDeck(Vec<Node>),
	Exit(i32),
	Signal(c_int),
	Error(ShError),
	LastStatus(RshWait),
	Prompt
}

pub fn main_loop() -> RshResult<()> {
	loop {
		let input = prompt::run()?;
		write_meta(|m| m.leave_prompt())?;
		if !input.is_empty() {
			let mut tokenizer = RshTokenizer::new(&input);

			loop {
				let result = descend(&mut tokenizer);
				match result {
					Ok(Some(state)) => {
						let deck = helper::extract_deck_from_root(&state.ast)?;
						if !deck.is_empty() {
							// Send each deck immediately for execution
							if let Err(e) = execute::traverse_ast(state.ast) {
								eprintln!("{:?}",e);
							}
						} else {
							break;
						}
					}
					Ok(None) => break,
					Err(e) => {
						eprintln!("{:?}",e);
					}
				}
			}
		}
	}
}
