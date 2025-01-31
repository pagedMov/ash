use std::{collections::VecDeque, ffi::c_int, fmt::Display, io, panic::Location, thread};


use bitflags::Flags;
use nix::unistd::{getpgrp, isatty, Pid};

use crate::{execute::{self, OxWait, ProcIO}, interp::{helper, parse::{descend, NdFlags, Node, Span}, token::OxTokenizer}, prompt, shellenv::{self, read_meta, read_vars, write_meta, EnvFlags}, OxResult};

#[derive(Debug, Clone, PartialEq)]
pub enum ShError {
	CommandNotFound(String, Span),
	InvalidSyntax(String, Span),
	ParsingError(String, Span),
	ExecFailed(String, i32, Span),
	IoError(String, u32, String),
	InternalError(String),
	Generic(String, Span),
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
			ShError::Generic(msg,_) => ShError::Generic(msg.to_string(),new_span),
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
			ShError::Generic(..) => false,
		}
	}
}

impl Display for ShError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			ShError::CommandNotFound(cmd, _) => {
				write!(f, "{}", cmd)
			}
			ShError::InvalidSyntax(msg, _) => {
				write!(f, "{}", msg)
			}
			ShError::ParsingError(msg, _) => {
				write!(f, "{}", msg)
			}
			ShError::ExecFailed(cmd, _, _) => {
				write!(f, "{}", cmd)
			}
			ShError::IoError(msg, _, _) => {
				write!(f, "{}", msg)
			}
			ShError::InternalError(msg) => {
				write!(f, "{}", msg)
			}
			ShError::Generic(msg, _) => {
				write!(f, "{}", msg)
			}
		}
	}
}

#[derive(Debug)]
pub struct ShErrorWindow {
	line: usize,
	col: usize,
	window: String
}

impl ShErrorWindow {
	pub fn new(input: &str, span: Span) -> Self {
		let delta = span.end.saturating_sub(span.start);
		let mut chars = input.chars();
		let mut lines = input.lines();

		let mut pos = 0;
		let mut line = 0;
		let mut col = 0;

		while pos != span.start {
			let char = chars.next();
			if char == Some('\n') {
				line += 1;
				col = 0;
			} else {
				col += 1;
			}
			pos += 1
		}

		let first_line = lines.nth(line).map(|ln| ln.to_string()).unwrap_or_default();
		let second_line = {
			let caret = String::from("\x1b[31m^\x1b[0m");
			let padding = String::from(" ").repeat(col);
			let span_line = String::from("\x1b[31m~\x1b[0m").repeat(delta.saturating_sub(2)); // subtract 2 to make room for the carets
			if delta <= 1 {
				format!("{}{}",padding,caret)
			} else {
				format!("{}{}{}{}",padding,caret,span_line,caret)
			}
		};
		let window = format!("{}\n{}",first_line,second_line);
		Self { line, col, window }
	}

	pub fn get_vals(&self) -> (usize,usize,&str) {
		(self.line + 1,self.col + 1,self.window.as_str())
	}
}

#[derive(Debug)]
pub struct ShErrorFull {
	window: Option<ShErrorWindow>,
	err: ShError
}

impl ShErrorFull {
	pub fn from(err: ShError, input: &str) -> Self {
		match &err {
			ShError::CommandNotFound(_, span) |
				ShError::InvalidSyntax(_, span) |
				ShError::ParsingError(_, span) |
				ShError::Generic(_, span) |
				ShError::ExecFailed(_, _, span) => {
					let window = Some(ShErrorWindow::new(input, *span));
					Self { window, err }
				}
			ShError::IoError(_, _, _) => Self { window: None, err },
			ShError::InternalError(_) => Self { window: None, err }
		}
	}

	pub fn get_err_msg(&self) -> String {
		match &self.err {
			ShError::CommandNotFound(msg, _) |
				ShError::InvalidSyntax(msg, _) |
				ShError::ParsingError(msg, _) |
				ShError::ExecFailed(msg, _, _) |
				ShError::IoError(msg, _, _) |
				ShError::InternalError(msg) |
				ShError::Generic(msg, _) => msg.to_string()
		}
	}

	pub fn debug(input: &str, span: Span) {
		let window = Some(ShErrorWindow::new(input,span));
		let err = ShError::Generic(input.to_string(),span);
		eprintln!("{}", Self { window, err });
	}
}

impl Display for ShErrorFull {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let msg = self.get_err_msg();
		match &self.window {
			Some(window) => {
				let (line,col,window) = window.get_vals();
				write!(f,"{};{} - {}\n\n{}",line,col,msg,window)
			}
			None => {
				write!(f,"{}",msg)
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
	LastStatus(OxWait),
	Prompt
}

pub fn throw(err: ShError) -> OxResult<()> {
	let input = read_meta(|m| m.get_last_input())?;
	eprintln!("{}", ShErrorFull::from(err,&input));
	Ok(())
}

pub fn execute(input: &str, flags: NdFlags, redirs: Option<VecDeque<Node>>, io: Option<ProcIO>) -> OxResult<OxWait> {
	let mut last_status = OxWait::Fail { code: 1, cmd: None };
	if !input.is_empty() {
		let mut tokenizer = OxTokenizer::new(input);

		loop {
			let result = descend(&mut tokenizer);
			match result {
				Ok(mut state) => {
					if !flags.is_empty() {
						state.ast.flags |= flags;
					}
					if let Some(ref redirs) = redirs {
						state.ast.redirs = redirs.clone()
					}
					let deck = helper::extract_deck_from_root(&state.ast)?;
					if !deck.is_empty() {
						// Send each deck immediately for execution
						let result = execute::traverse_ast(state.ast, io.clone());
						if let Err(e) = result {
							throw(e)?;
						} else {
							last_status = result.unwrap();
						}
					} else {
						break;
					}
				}
				Err(e) => {
					throw(e)?;
				}
			}
		}
	}
	Ok(last_status)
}

pub fn main_loop() -> OxResult<()> {
	if read_meta(|m| m.flags().contains(EnvFlags::IN_SUBSH))? {
		eprintln!("I shouldnt be here");
	}
	loop {
		if shellenv::term_controller() != getpgrp() {
			// If we don't control the terminal at this point for some reason, take control of it
			// Bad things will happen if we reach the prompt without terminal control
			shellenv::attach_tty(getpgrp())?;
		}
		let input = prompt::run()?;
		write_meta(|m| m.leave_prompt())?;
		write_meta(|m| m.start_timer())?;
		execute(&input, NdFlags::empty(), None, None)?;
	}
}
