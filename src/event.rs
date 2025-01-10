use std::{ffi::c_int, io, panic::Location, sync::mpsc::{self, Receiver, Sender}};
use libc::{getpid, tcgetpgrp};
use signal_hook::iterator::Signals;


use crate::{execute::RshWait, interp::parse::{Node, Span}, shellenv::{read_meta, write_meta}, signal::{self, }, RshResult, GLOBAL_EVENT_CHANNEL};

pub fn global_send(shell_event: ShEvent) -> RshResult<()> {
	GLOBAL_EVENT_CHANNEL.get().unwrap().send(shell_event).map_err(|_| ShError::from_internal("Failed to send shell event over global channel"))
}

#[track_caller]
pub fn fire_prompt() -> RshResult<()> {
	let tc_group = unsafe { tcgetpgrp(0) };
	let rsh_pid = unsafe { getpid() };
	// Temporarily disabling this guard condition
	//if tc_group != rsh_pid {
		//dbg!(Location::caller());
		//panic!("attempted to fire prompt without terminal control. tc group: {}, rsh pid: {}",tc_group,rsh_pid);
	//}
	if tc_group == rsh_pid {
		global_send(ShEvent::Prompt)
	} else {
		Ok(())
	}
}

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

#[derive(Debug,Clone)]
pub enum ShEvent {
	Input(String),
	NewAST(Node),
	Exit(i32),
	Signal(c_int),
	Error(ShError),
	LastStatus(RshWait),
	Prompt
}

pub struct EventLoop {
	pub exec_tx: Sender<Node>,
	pub prompt_tx: Sender<ShEvent>,
	pub inbox: Receiver<ShEvent>,
	pub sender: Sender<ShEvent>
}

impl EventLoop {
	pub fn new(exec_tx: Sender<Node>,prompt_tx: Sender<ShEvent>) -> Self {
		let (sender,inbox) = mpsc::channel();
		Self {
			exec_tx,
			prompt_tx,
			inbox,
			sender
		}
	}
	pub fn listen(&self) -> RshResult<RshWait> {
		use crate::event::ShEvent::*;
		let mut status = RshWait::new();
		for event in self.inbox.iter() {
			match event {
				Input(text) => {
					write_meta(|m| m.set_last_input(&text))?
				}
				NewAST(node) => {
					// Forward to execution thread
					self.exec_tx.send(node).unwrap();
				}
				Prompt => {
					// Forward to prompt thread
					self.prompt_tx.send(ShEvent::Prompt).map_err(|_| ShError::from_internal("failed to send signal to prompt thread"))?
				}
				Exit(code) => {
					// TODO: Implement more cleanup logic later
					std::process::exit(code);
				}
				LastStatus(status) => {
					match status {
						RshWait::Success => { /* Do nothing */ },
						RshWait::Fail { code, cmd } => {
							if code == 127 && cmd.is_some() {
								eprintln!("Command not found: {}",cmd.unwrap())
							}
						}
						_ => { /* Do Nothing */ }
					}
				}
				Error(err) => {
					// TODO: re-implement proper error handling
					eprintln!("{:?}",err);
				}
				Signal(_) => {
					// Forward to signal thread
					if let Err(e) = signal::handle_signal(event) {
						self.sender.send(ShEvent::Error(e)).unwrap();
					}
				}
			}
		}
		Ok(status)
	}
}
