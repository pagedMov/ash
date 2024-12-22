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

use std::{env, fs::File, io::Read, os::fd::{AsFd, AsRawFd, BorrowedFd, RawFd}};

use event::EventLoop;
use execute::{NodeWalker, RshExitStatus};
use interp::parse::descend;
use libc::STDERR_FILENO;
use log::info;
use nix::{fcntl::{open, OFlag}, sys::stat::Mode, unistd::write};

//use crate::event::EventLoop;
use crate::shellenv::ShellEnv;


#[tokio::main]
async fn main() {
	let args = env::args().collect::<Vec<String>>();

	match args.len() {
		1 => {
			main_interactive().await;
		},
		_ => {
			main_noninteractive(args).await;
		}
	}
}

async fn main_noninteractive(args: Vec<String>) {
	let stderr = unsafe { BorrowedFd::borrow_raw(STDERR_FILENO) };
	let input;

	// Input Handling
	if args[1] == "-c" {
		if args.len() < 3 {
			write(stderr.as_fd(), b"Expected an argument after '-c' flag\n").unwrap();
			return;
		}
		input = args[2].clone(); // Store the command string
	} else {
		let script_name = &args[1];
		let file = File::open(script_name);
		match file {
			Ok(mut script) => {
				let mut buffer = vec![];
				if let Err(e) = script.read_to_end(&mut buffer) {
					write(stderr.as_fd(), format!("Error reading file: {}\n", e).as_bytes()).unwrap();
					return;
				}
				input = String::from_utf8_lossy(&buffer).to_string(); // Convert file contents to String
			}
			Err(e) => {
				write(stderr.as_fd(), format!("Error opening file: {}\n", e).as_bytes()).unwrap();
				return;
			}
		}
	}

	// Code Execution Logic
	let mut shellenv = ShellEnv::new(false, false);
	let state = descend(&input, &shellenv);
	match state {
		Ok(parse_state) => {
			let mut walker = NodeWalker::new(parse_state.ast, &mut shellenv);
			match walker.start_walk() {
				Ok(code) => {
					info!("Last exit status: {:?}", code);
					if let RshExitStatus::Fail { code, cmd } = code {
						if code == 127 {
							if let Some(cmd) = cmd {
								write(stderr, format!("Command not found: {}\n", cmd).as_bytes()).unwrap();
							}
						}
					}
				}
				Err(e) => {
					write(stderr.as_fd(), format!("Execution error: {}\n", e).as_bytes()).unwrap();
				}
			}
		}
		Err(e) => {
			write(stderr.as_fd(), format!("Parsing error: {}\n", e).as_bytes()).unwrap();
		}
	}
}

async fn main_interactive() {
	env_logger::init();
	let mut shellenv = ShellEnv::new(false,true);
	let mut event_loop = EventLoop::new(&mut shellenv);
	let _ = event_loop.listen().await;
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
