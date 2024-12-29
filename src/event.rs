use std::fmt;
use std::os::fd::BorrowedFd;

use nix::unistd::write;
use tokio::sync::mpsc;
use log::{error,debug,info};
use tokio::signal::unix::{signal, Signal, SignalKind};

use crate::execute::RshWaitStatus;
use crate::interp::parse::{Node, Span};
use crate::{execute, prompt};
use crate::shellenv::ShellEnv;

#[derive(Debug, PartialEq)]
pub enum ShellError {
	CommandNotFound(String, Span),
	InvalidSyntax(String, Span),
	ParsingError(String, Span),
	ExecFailed(String, i32, Span),
	IoError(String, Span),
	InternalError(String, Span),
}

impl ShellError {
	pub fn from_io(msg: &str, span: Span) -> Self {
		ShellError::IoError(msg.to_string(), span)
	}
	pub fn from_execf(msg: &str, code: i32, span: Span) -> Self {
		ShellError::ExecFailed(msg.to_string(), code, span)
	}
	pub fn from_parse(msg: &str, span: Span) -> Self {
		ShellError::ParsingError(msg.to_string(), span)
	}
	pub fn from_syntax(msg: &str, span: Span) -> Self {
		ShellError::InvalidSyntax(msg.to_string(), span)
	}
	pub fn from_no_cmd(msg: &str, span: Span) -> Self {
		ShellError::CommandNotFound(msg.to_string(), span)
	}
	pub fn from_internal(msg: &str, span: Span) -> Self {
		ShellError::InternalError(msg.to_string(), span)
	}
	// This is used in the context of functions
	// To prevent the error from trying to use the span
	// Of the offending command that is inside of the function
	pub fn overwrite_span(&self, new_span: Span) -> Self {
		match self {
			ShellError::IoError(msg,_) => ShellError::IoError(msg.to_string(),new_span),
			ShellError::CommandNotFound(msg,_) => ShellError::CommandNotFound(msg.to_string(),new_span),
			ShellError::InvalidSyntax(msg,_) => ShellError::InvalidSyntax(msg.to_string(),new_span),
			ShellError::ParsingError(msg,_) => ShellError::ParsingError(msg.to_string(),new_span),
			ShellError::ExecFailed(msg,code,_) => ShellError::ExecFailed(msg.to_string(),*code,new_span),
			ShellError::InternalError(msg,_) => ShellError::InternalError(msg.to_string(),new_span),
		}
	}
	pub fn is_fatal(&self) -> bool {
		match self {
			ShellError::IoError(..) => true,
			ShellError::CommandNotFound(..) => false,
			ShellError::ExecFailed(..) => false,
			ShellError::ParsingError(..) => false,
			ShellError::InvalidSyntax(..) => false,
			ShellError::InternalError(..) => false,
		}
	}
}

/// A custom error type for the shell program that provides detailed error context.
/// This struct encapsulates the original input and the error, and offers methods
/// to format the error context for more informative output.
pub struct ShellErrorFull {
	/// The original input string that led to the error.
	input: String,
	/// The shell error encountered during execution.
	error: ShellError,
}

impl ShellErrorFull {
	/// Creates a new `ShellErrorFull` instance from the given input and error.
	///
	/// # Arguments
	///
	/// * `input` - The original input string that caused the error.
	/// * `error` - The `ShellError` encountered during execution.
	///
	/// # Returns
	///
	/// A `ShellErrorFull` instance encapsulating the input and error.
	pub fn from(input: String, error: ShellError) -> Self {
		Self { input, error }
	}

	/// Formats and prints the error context, including the line, column, offending input,
	/// and a pointer indicating the error location.
	///
	/// # Arguments
	///
	/// * `span` - The span indicating the start and end positions of the error in the input.
	fn format_error_context(&self, span: Span) {
		let (line, col) = Self::get_line_col(&self.input, span.start);
		let (window, window_offset) = Self::generate_window(&self.input, line, col);
		let span_diff = span.end - span.start;
		let pointer = Self::get_pointer(span_diff, window_offset);

		println!();
		println!("{};{}:", line + 1, col + 1);
		println!("{}", window);
		println!("{}", pointer);
	}

	/// Generates a string pointer indicating the location of the error in the input.
	/// This pointer is composed of a caret (^) and tildes (~) to mark the span.
	///
	/// # Arguments
	///
	/// * `span_diff` - The length of the error span.
	/// * `offset` - The offset position of the error within the window.
	///
	/// # Returns
	///
	/// A string representing the pointer to the error location.
	fn get_pointer(span_diff: usize, offset: usize) -> String {
		let padding = " ".repeat(offset);
		let visible_span = span_diff.min(40 - offset);

		let mut pointer = String::new();
		pointer.push('^');
		if visible_span > 1 {
			pointer.push_str(&"~".repeat(visible_span - 2));
			pointer.push('^');
		}

		format!("{}{}", padding, pointer)
	}

	/// Determines the line and column number of a given offset within the input string.
	///
	/// # Arguments
	///
	/// * `input` - The original input string.
	/// * `offset` - The character offset for which to find the line and column.
	///
	/// # Returns
	///
	/// A tuple containing the zero-based line and column numbers.
	fn get_line_col(input: &str, offset: usize) -> (usize, usize) {
		let mut line = 0;
		let mut col = 0;

		for (i, ch) in input.chars().enumerate() {
			if i == offset {
				break;
			}
			if ch == '\n' {
				line += 1;
				col = 0;
			} else {
				col += 1;
			}
		}

		(line, col)
	}

	/// Generates a window of text surrounding the error location for context.
	/// The window is centered around the error column, with a maximum width of 40 characters.
	///
	/// # Arguments
	///
	/// * `input` - The original input string.
	/// * `error_line` - The zero-based line number of the error.
	/// * `error_col` - The zero-based column number of the error.
	///
	/// # Returns
	///
	/// A tuple containing a string with the window of text and the offset for the error pointer.
	fn generate_window(input: &str, error_line: usize, error_col: usize) -> (String, usize) {
		let window_width = 40;
		let lines: Vec<&str> = input.lines().collect();

		if lines.len() <= error_line {
			return ("Error line out of range".into(), 0);
		}

		let offending_line = lines[error_line];
		let line_len = offending_line.len();

		let start = if error_col > 10 {
			error_col.saturating_sub(10)
		} else {
			0
		};
		let end = (start + window_width).min(line_len);

		(offending_line[start..end].to_string(), error_col - start)
	}
}

impl fmt::Display for ShellErrorFull {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match &self.error {
			ShellError::IoError(msg, span) => {
				writeln!(f, "I/O Error: {}", msg)?;
				self.format_error_context(*span);
			}
			ShellError::ExecFailed(msg, code, span) => {
				writeln!(
					f,
					"Execution failed (exit code {}): {}",
					code,
					msg
				)?;
				self.format_error_context(*span);
			}
			ShellError::ParsingError(msg, span) => {
				writeln!(f, "Parsing error: {}", msg)?;
				self.format_error_context(*span);
			}
			ShellError::InvalidSyntax(msg, span) => {
				writeln!(f, "Syntax Error: {}", msg)?;
				self.format_error_context(*span);
			}
			ShellError::CommandNotFound(msg, span) => {
				writeln!(f, "Command not found: {}", msg)?;
				self.format_error_context(*span);
			}
			ShellError::InternalError(msg, span) => {
				writeln!(f, "Internal Error: {}", msg)?;
				self.format_error_context(*span);
			}
		}
		writeln!(f)?;
		Ok(())
	}
}

#[derive(Debug,PartialEq)]
pub enum ShellEvent {
	Prompt,
	Signal(Signals),
	SubprocessExited(u32,i32),
	NewAST(Node),
	CatchError(ShellError),
	Exit(i32)
}

#[derive(Debug,PartialEq)]
pub enum Signals {
	SIGINT,
	SIGIO,
	SIGPIPE,
	SIGTSTP,
	SIGQUIT,
	SIGTERM,
	SIGCHLD,
	SIGHUP,
	SIGWINCH,
	SIGUSR1,
	SIGUSR2
}

pub struct EventLoop<'a> {
	sender: mpsc::Sender<ShellEvent>,
	receiver: mpsc::Receiver<ShellEvent>,
	shellenv: &'a mut ShellEnv
}

impl<'a> EventLoop<'a> {
	/// Creates a new `EventLoop` instance with a message passing channel.
	///
	/// # Returns
	/// A new instance of `EventLoop` with a sender and receiver for inter-task communication.
	pub fn new(shellenv: &'a mut ShellEnv) -> Self {
		let (sender, receiver) = mpsc::channel(100);
		Self {
			sender,
			receiver,
			shellenv
		}
	}

	/// Provides a clone of the `sender` channel to send events to the event loop.
	///
	/// # Returns
	/// A clone of the `mpsc::Sender<ShellEvent>` for sending events.
	pub fn inbox(&self) -> mpsc::Sender<ShellEvent> {
		self.sender.clone()
	}

	/// Starts the event loop and listens for incoming events.
	///
	/// This method spawns a separate task to listen for system signals using a `SignalListener`,
	/// and then begins processing events from the channel.
	///
	/// # Returns
	/// A `Result` containing the exit code (`i32`) or a `ShellError` if an error occurs.
	pub async fn listen(&mut self) -> Result<i32, ShellError> {
		let mut signal_listener = SignalListener::new(self.inbox());
		tokio::spawn(async move {
			signal_listener.signal_listen().await
		});
		self.event_listen().await
	}

	/// Processes events from the event loop's receiver channel.
	///
	/// This method handles different types of `ShellEvent` messages, such as prompting the user,
	/// handling exit signals, processing new AST nodes, and responding to subprocess exits or errors.
	///
	/// # Returns
	/// A `Result` containing the exit code (`i32`) or a `ShellError` if an error occurs.
	pub async fn event_listen(&mut self) -> Result<i32, ShellError> {
		debug!("Event loop started.");
		let mut code: i32 = 0;

		// Send an initial prompt event to the loop.
		self.sender.send(ShellEvent::Prompt).await.unwrap();
		while let Some(event) = self.receiver.recv().await {
			match event {
				ShellEvent::Prompt => {
					// Trigger the prompt logic.
					prompt::prompt(self.inbox(),self.shellenv).await;
				}
				ShellEvent::Exit(exit_code) => {
					// Handle exit events and set the exit code.
					code = exit_code;
				}
				ShellEvent::NewAST(tree) => {
					// Log and process a new AST node.
					debug!("new tree:\n {:#?}", tree);
					let mut walker = execute::NodeWalker::new(tree,self.shellenv);
					let stderr = unsafe { BorrowedFd::borrow_raw(2) };

					match walker.start_walk() {
						Ok(code) => {
							info!("Last exit status: {:?}",code);
							if let RshWaitStatus::Fail { code, cmd, span } = &code {
								if *code == 127 {
									if let Some(cmd) = cmd {
										let err = ShellErrorFull::from(self.shellenv.get_last_input(),ShellError::from_no_cmd(cmd, *span));
										write(stderr, format!("{}",err).as_bytes()).unwrap();
									}
								};
							};

							self.shellenv.handle_exit_status(code);
						},
						Err(e) => {
							let err = ShellErrorFull::from(self.shellenv.get_last_input(),e);
							let _ = write(stderr, format!("{}",err).as_bytes());
						}
					}
				}
				ShellEvent::SubprocessExited(pid, exit_code) => {
					// Log the exit of a subprocess.
					debug!("Process '{}' exited with code {}", pid, exit_code);
				}
				ShellEvent::Signal(signal) => {
					// Handle received signals.
					self.handle_signal(signal).await;
				}
				ShellEvent::CatchError(err) => {
					// Handle errors, exiting if fatal.
					let fatal = err.is_fatal();
					let error_display = ShellErrorFull::from(self.shellenv.get_last_input(), err);
					if fatal {
						error!("Fatal: {}", error_display);
						std::process::exit(1);
					} else {
						println!("{}",error_display);
					}
				}
			}
		}
		Ok(code)
	}
	async fn handle_signal(&self, signal: Signals) {
		match signal {
			Signals::SIGQUIT => {
				std::process::exit(0);
			}
			_ => { },
		}
	}
}

pub struct SignalListener {
	outbox: mpsc::Sender<ShellEvent>,
	sigint: Signal,
	sigio: Signal,
	sigpipe: Signal,
	sigtstp: Signal,
	sigquit: Signal,
	sigterm: Signal,
	sigchild: Signal,
	sighup: Signal,
	sigwinch: Signal,
	sigusr1: Signal,
	sigusr2: Signal,
}

impl SignalListener {
	pub fn new(outbox: mpsc::Sender<ShellEvent>) -> Self {
		Self {
			// Signal listeners
			// TODO: figure out what to do instead of unwrapping
			outbox,
			sigint: signal(SignalKind::interrupt()).unwrap(),
			sigio: signal(SignalKind::io()).unwrap(),
			sigpipe: signal(SignalKind::pipe()).unwrap(),
			sigtstp: signal(SignalKind::from_raw(20)).unwrap(),
			sigquit: signal(SignalKind::quit()).unwrap(),
			sigterm: signal(SignalKind::terminate()).unwrap(),
			sigchild: signal(SignalKind::child()).unwrap(),
			sighup: signal(SignalKind::hangup()).unwrap(),
			sigwinch: signal(SignalKind::window_change()).unwrap(),
			sigusr1: signal(SignalKind::user_defined1()).unwrap(),
			sigusr2: signal(SignalKind::user_defined2()).unwrap(),
		}
	}
	pub async fn signal_listen(&mut self) -> Result<i32, ShellError> {
		let sigint = &mut self.sigint;
		let sigio = &mut self.sigio;
		let sigpipe = &mut self.sigpipe;
		let sigtstp = &mut self.sigtstp;
		let sigquit = &mut self.sigquit;
		let sigterm = &mut self.sigterm;
		let sigchild = &mut self.sigchild;
		let sighup = &mut self.sighup;
		let sigwinch = &mut self.sigwinch;
		let sigusr1 = &mut self.sigusr1;
		let sigusr2 = &mut self.sigusr2;

		loop {
			tokio::select! {
				_ = sigint.recv() => {
					self.outbox.send(ShellEvent::Signal(Signals::SIGINT)).await.unwrap();
					// Handle SIGINT
				}
				_ = sigio.recv() => {
					self.outbox.send(ShellEvent::Signal(Signals::SIGIO)).await.unwrap();
					// Handle SIGIO
				}
				_ = sigpipe.recv() => {
					self.outbox.send(ShellEvent::Signal(Signals::SIGPIPE)).await.unwrap();
					// Handle SIGPIPE
				}
				_ = sigtstp.recv() => {
					self.outbox.send(ShellEvent::Signal(Signals::SIGTSTP)).await.unwrap();
					// Handle SIGPIPE
				}
				_ = sigquit.recv() => {
					self.outbox.send(ShellEvent::Signal(Signals::SIGQUIT)).await.unwrap();
					// Handle SIGQUIT
				}
				_ = sigterm.recv() => {
					self.outbox.send(ShellEvent::Signal(Signals::SIGTERM)).await.unwrap();
					// Handle SIGTERM
				}
				_ = sigchild.recv() => {
					self.outbox.send(ShellEvent::Signal(Signals::SIGCHLD)).await.unwrap();
					// Handle SIGCHLD
				}
				_ = sighup.recv() => {
					self.outbox.send(ShellEvent::Signal(Signals::SIGHUP)).await.unwrap();
					// Handle SIGHUP
				}
				_ = sigwinch.recv() => {
					self.outbox.send(ShellEvent::Signal(Signals::SIGWINCH)).await.unwrap();
					// Handle SIGWINCH
				}
				_ = sigusr1.recv() => {
					self.outbox.send(ShellEvent::Signal(Signals::SIGUSR1)).await.unwrap();
					// Handle SIGUSR1
				}
				_ = sigusr2.recv() => {
					self.outbox.send(ShellEvent::Signal(Signals::SIGUSR2)).await.unwrap();
					// Handle SIGUSR2
				}
			}
		}
	}
}
