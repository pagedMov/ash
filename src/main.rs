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
pub mod debug;
pub mod signal;

use std::{env, fs::File, io::Read, path::PathBuf, sync::mpsc, thread};

use event::{EventLoop, ShError, ShEvent};
use execute::{traverse_ast, ExecDispatcher, RshWait};
use interp::parse::{descend, Span};
use nix::sys::signal::{signal, SigHandler, Signal::SIGTTOU};
use prompt::PromptDispatcher;
use shellenv::{read_vars, write_meta, write_vars, EnvFlags};
use signal::SignalListener;

//use crate::event::EventLoop;

pub type RshResult<T> = Result<T, ShError>;


#[tokio::main]
async fn main() {
	let args = env::args().collect::<Vec<String>>();

	// Ignore SIGTTOU
	unsafe { signal(SIGTTOU, SigHandler::SigIgn).unwrap() };
	println!("we new now");

	if args[0].starts_with('-') {
		// TODO: handle unwrap
		let home = read_vars(|vars| vars.get_evar("HOME")).unwrap().unwrap();
		let path = PathBuf::from(format!("{}/.rsh_profile",home));
		shellenv::source_file(path).unwrap();
	}
	let result = match args.len() {
		1 => {
			write_meta(|m| m.mod_flags(|f| *f |= EnvFlags::INTERACTIVE)).unwrap();
			let (exec_tx,exec_rx) = mpsc::channel();
			let (prompt_tx,prompt_rx) = mpsc::channel();
			let event_loop = EventLoop::new(exec_tx,prompt_tx.clone());
			let event_loop_inbox = event_loop.sender.clone();
			let prompt_dispatch = PromptDispatcher::new(prompt_rx, event_loop_inbox.clone());
			let exec_dispatch = ExecDispatcher::new(exec_rx, event_loop_inbox.clone());
			let signal_listener = SignalListener::new(event_loop_inbox.clone());

			let signal_handle = thread::Builder::new()
				.name("signal_loop".into())
				.spawn(move || {
					signal_listener.signal_listen();
				})
				.unwrap();

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
			event_loop_handle.join().unwrap()
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
			let (tx,_) = mpsc::channel();
			let (sender,receiver) = mpsc::channel();
			let dispatch = ExecDispatcher::new(receiver,tx);
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
