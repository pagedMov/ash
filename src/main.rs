//pub mod event;
//pub mod prompt;
//pub mod parser;
//mod rsh;
//
//use log::debug;
//
//use tokio::io::{self, AsyncBufReadExt, BufReader};
//use tokio_stream::{wrappers::LinesStream, StreamExt};
//
//use crate::event::EventLoop;
//
//#[tokio::main]
//async fn main() {
//env_logger::init();
//let mut event_loop = EventLoop::new();
//
//debug!("Starting event loop");
//TODO: use the value returned by this for something
//let _ = event_loop.listen().await;
//}


pub mod prompt;
pub mod event;
pub mod execute;
pub mod shellenv;
pub mod interp;
pub mod builtin;
pub mod comp;
pub mod signal;

use std::{env, fs::File, io::Read, os::fd::AsRawFd, path::PathBuf, sync::mpsc::{self, Receiver, Sender}, thread};

use event::{EventLoop, ShError, ShEvent};
use execute::{traverse_ast, ExecDispatcher, RshWait};
use interp::parse::{descend, Span};
use nix::{sys::{signal::{signal, SigHandler, Signal::{SIGTTIN, SIGTTOU}}, termios::{self, LocalFlags}}, unistd::isatty};
use once_cell::sync::{Lazy, OnceCell};
use prompt::PromptDispatcher;
use shellenv::{read_vars, write_meta, write_vars, EnvFlags};
use signal::SignalListener;
use termios::Termios;

//use crate::event::EventLoop;

pub type RshResult<T> = Result<T, ShError>;

static GLOBAL_EVENT_CHANNEL: OnceCell<Sender<ShEvent>> = OnceCell::new();

fn set_termios() -> Option<Termios> {
	if isatty(std::io::stdin().as_raw_fd()).unwrap() {
		let mut termios = termios::tcgetattr(std::io::stdin()).unwrap();
		termios.local_flags &= !LocalFlags::ECHOCTL;
		termios::tcsetattr(std::io::stdin(), nix::sys::termios::SetArg::TCSANOW, &termios).unwrap();
		Some(termios)
	} else {
		None
	}
}

fn restore_termios(orig: &Option<Termios>) {
	if let Some(termios) = orig {
		let fd = std::io::stdin();
		termios::tcsetattr(fd, termios::SetArg::TCSANOW, termios).unwrap();
	}
}

#[tokio::main]
async fn main() {
	env_logger::init();
	let mut args = env::args().collect::<Vec<String>>();

	// Ignore SIGTTOU
	unsafe { signal(SIGTTOU, SigHandler::SigIgn).unwrap() };
	unsafe { signal(SIGTTIN, SigHandler::SigIgn).unwrap() };

	let (exec_tx,exec_rx) = mpsc::channel();
	let (prompt_tx,prompt_rx) = mpsc::channel();
	let event_loop = EventLoop::new(exec_tx,prompt_tx.clone());
	GLOBAL_EVENT_CHANNEL.set(event_loop.sender.clone()).unwrap();
	let prompt_dispatch = PromptDispatcher::new(prompt_rx);
	let exec_dispatch = ExecDispatcher::new(exec_rx);
	let signal_listener = SignalListener::new();

	if args[0].starts_with('-') {
		// TODO: handle unwrap
		let home = read_vars(|vars| vars.get_evar("HOME")).unwrap().unwrap();
		let path = PathBuf::from(format!("{}/.rsh_profile",home));
		shellenv::source_file(path).unwrap();
	}
	if !args.contains(&"--no-rc".into()) {
		let home = read_vars(|vars| vars.get_evar("HOME")).unwrap().unwrap();
		let path = PathBuf::from(format!("{}/.rshrc",home));
		if let Err(e) = shellenv::source_file(path) {
			eprintln!("Failed to source rc file: {:?}",e);
		}
	}
	if args.iter().any(|arg| arg == "--subshell") {
		let index = args.iter().position(|arg| arg == "--subshell").unwrap();
		args.remove(index);
		write_meta(|m| m.mod_flags(|f| *f |= EnvFlags::IN_SUBSH)).unwrap();
	}
	let result = match args.len() {
		1 => { // interactive
			let termios = set_termios();
			write_meta(|m| m.mod_flags(|f| *f |= EnvFlags::INTERACTIVE)).unwrap();
			signal_listener.signal_listen().unwrap();

			let event_loop_handle = thread::Builder::new()
					.name("event_loop".into())
					.spawn(move || {
							event_loop.listen()
					})
					.unwrap();

			let prompt_handle = thread::Builder::new()
					.name("prompt_dispatch".into())
					.spawn(move || {
							prompt_dispatch.run()
					})
					.unwrap();

			let exec_handle = thread::Builder::new()
					.name("exec_dispatch".into())
					.spawn(move || {
							exec_dispatch.run()
					})
					.unwrap();

			prompt_tx.send(ShEvent::Prompt).unwrap();
			let result = event_loop_handle.join().unwrap();
			restore_termios(&termios);
			result
		},
		_ => {
			thread::spawn( move || {
				main_noninteractive(args)
			}).join().unwrap()
		}
	};
	match result {
		Ok(status) => {
			match status {
				RshWait::Success {..} => std::process::exit(0),
				RshWait::Fail { code, cmd: _, } => std::process::exit(code),
				_ => unreachable!()
			}
		}
		Err(err) => {
			eprintln!("{:?}",err);
			std::process::exit(1)
		}
	}

}



fn main_noninteractive(args: Vec<String>) -> RshResult<RshWait> {
	let mut pos_params: Vec<String> = vec![];
	let input;

	// Input Handling
	if args[1] == "-c" {
		if args.len() < 3 {
			eprintln!("Expected a command after '-c' flag");
			return Ok(RshWait::Fail { code: 1, cmd: None, });
		}
		input = args[2].clone(); // Store the command string
	} else {
		let script_name = &args[1];
		if args.len() > 2 {
			pos_params = args[2..].to_vec();
		}
		let file = File::open(script_name);
		match file {
			Ok(mut script) => {
				let mut buffer = vec![];
				if let Err(e) = script.read_to_end(&mut buffer) {
					eprintln!("Error reading file: {}\n", e);
					return Ok(RshWait::Fail { code: 1, cmd: None, });
				}
				input = String::from_utf8_lossy(&buffer).to_string(); // Convert file contents to String
			}
			Err(e) => {
				eprintln!("Error opening file: {}\n", e);
					return Ok(RshWait::Fail { code: 1, cmd: None, });
			}
		}
	}

	// Code Execution Logic
	write_meta(|m| m.set_last_input(&input))?;
	for (index,param) in pos_params.into_iter().enumerate() {
		let key = format!("{}",index + 1);
		write_vars(|v| v.set_param(key, param))?;
	}
	let state = descend(&input);
	match state {
		Ok(parse_state) => {
			let (sender,receiver) = mpsc::channel();
			let dispatch = ExecDispatcher::new(receiver);
			let result = traverse_ast(parse_state.ast);
			match result {
				Ok(code) => {
					if let RshWait::Fail { code, cmd } = code {
						panic!()
					} else { Ok(code) }
				}
				Err(e) => {
					eprintln!("{:?}", e);
					panic!()
				}
			}
		}
		Err(e) => {
			eprintln!("{:?}", e);
			panic!()
		}
	}
}

//#[tokio::main]
//async fn main() {
//env_logger::init();
//loop {
//let mut stdout = io::stdout();
//stdout.write_all(b"> ").await.unwrap();
//stdout.flush().await.unwrap();
//
//let stdin = io::stdin();
//let mut reader = BufReader::new(stdin);
//
//let mut input = String::new();
//reader.read_line(&mut input).await.unwrap();
//
//let trimmed_input = input.trim().to_string();
//trace!("Received input! {}",trimmed_input);
//let shellenv = ShellEnv::new(false,false);
//let state = descend(&input,&shellenv);
//if let Err(e) = state {
//println!("{}",e);
//} else {
//println!("debug tree:");
//println!("{}",state.unwrap().ast);
//}
//}
//}
