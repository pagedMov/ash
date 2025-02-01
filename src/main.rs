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
pub mod shopt;

use std::{collections::VecDeque, env, io::Write, fs::OpenOptions, os::fd::AsRawFd, path::PathBuf};

use builtin::echo_internal;
use env_logger::Builder;
use event::ShError;
use execute::{traverse_ast, LashWait, RustFd};
use interp::{parse::{descend, NdType}, token::LashTokenizer};
use log::LevelFilter;
use nix::{fcntl::OFlag, sys::{stat::Mode, termios::{self, LocalFlags}}, unistd::{Pid,isatty}};
use shellenv::{read_vars, write_jobs, write_meta, write_vars, EnvFlags, RSH_PATH};
use termios::Termios;

//use crate::event::EventLoop;

pub type LashResult<T> = Result<T, ShError>;
const RSH_VERSION: &str = "v0.3.0-alpha";

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

fn init_logger() {
	let log_file = OpenOptions::new()
		.create(true)
		.append(true)
		.open("/home/pagedmov/lash.log")
		.unwrap();

		Builder::new()
			.filter(None, LevelFilter::Debug) // Set log level
			.format(move |buf, record| {
				writeln!(log_file.try_clone().unwrap(), "{} - {}", record.level(), record.args())
			})
		.init();
}

fn initialize_proc_constants() {
	/*
	 * These two variables are set using Lazy,
	 * in order to kick-start the lazy evaluation, we dereference them very early
	 */
	let _ = *RSH_PATH;
}

#[tokio::main]
async fn main() {
	env_logger::init();
	initialize_proc_constants();
	let mut interactive = true;
	let mut args = env::args().collect::<Vec<String>>();

	// Ignore SIGTTOU
	signal::sig_handler_setup();

	let mut pos_params = vec![];
	let mut args_deque = VecDeque::from(args.clone());
	args_deque.pop_front(); // Ignore command name
	while let Some(arg) = args_deque.pop_front() {
		match arg.as_str() {
			"-c" | "--subshell" => {
				args_deque.pop_front();
			}
			_ if !arg.starts_with('-') => {
				pos_params.push(arg);
			}
			_ => { /* Do nothing */ }
		}
	}
	let mut var_table = read_vars(|v| v.clone()).unwrap();
	for pos_param in pos_params {
		var_table.pos_param_pushback(&pos_param);
	}
	write_vars(|v| *v = var_table).unwrap();

	let script_path = if let Some(path) = args.iter().find(|arg| {
		let path = PathBuf::from(arg.as_str());
		path.exists() && !(**arg == args[0])
	}) {
		interactive = false;
		Some(path.to_string())
	} else {
		None
	};

	if args[0].starts_with('-') {
		// TODO: handle unwrap
		let home = read_vars(|vars| vars.get_evar("HOME")).unwrap().unwrap();
		let path = PathBuf::from(format!("{}/.lash_profile",home));
		if path.exists() {
			shellenv::source_file(path).unwrap();
		}
	}
	if args.contains(&"--version".into()) {
		println!("{}",RSH_VERSION);
		std::process::exit(0);
	}
	if !args.contains(&"--no-rc".into()) {
		let home = read_vars(|vars| vars.get_evar("HOME")).unwrap().unwrap();
		let path = PathBuf::from(format!("{}/.lashrc",home));
		if path.exists() {
			shellenv::source_file(path).unwrap();
		}
	}
	if args.iter().any(|arg| arg == "--subshell") {
		let index = args.iter().position(|arg| arg == "--subshell").unwrap();
		interactive = false;
		args.remove(index);
		write_meta(|m| m.mod_flags(|f| *f |= EnvFlags::IN_SUBSH)).unwrap();
	}
	match interactive {
		true => { // interactive
			let termios = set_termios();
			write_meta(|m| m.mod_flags(|f| *f |= EnvFlags::INTERACTIVE)).unwrap();

			let status = event::main_loop();

			if let Err(e) = status {
				match e {
					ShError::CleanExit(code) => {
						write_jobs(|j| j.hang_up()).ok();
						restore_termios(&termios);
						std::process::exit(code);
					}
					_ => unreachable!() // Only CleanExit will break the loop
				}
			}

		},
		false => {
			main_noninteractive(args,script_path).unwrap();
		}
	};
}



fn main_noninteractive(args: Vec<String>, script_path: Option<String>) -> LashResult<LashWait> {
	let mut pos_params: Vec<String> = vec![];
	let input;

	// Input Handling
	if args[1] == "-c" {
		if args.len() < 3 {
			eprintln!("Expected a command after '-c' flag");
			return Ok(LashWait::Fail { code: 1, cmd: None, });
		}
		input = args[2].clone(); // Store the command string
	} else {
		let script_name = if let Some(path) = script_path { path } else { args[1].clone() };
		let path = PathBuf::from(script_name);
		if args.len() > 2 {
			pos_params = args[2..].to_vec();
		}
		let mode = Mode::S_IRUSR | Mode::S_IRGRP;
		let file_desc = RustFd::open(&path, OFlag::O_RDONLY, mode);
		match file_desc {
			Ok(script) => {
				input = script.read().expect("Failed to read from script FD");
			}
			Err(e) => {
				eprintln!("Error opening file: {}\n", e);
				return Ok(LashWait::Fail { code: 1, cmd: None, });
			}
		}
	}

	// Code Execution Logic
	write_meta(|m| m.set_last_input(&input))?;
	for (index,param) in pos_params.into_iter().enumerate() {
		let key = format!("{}",index + 1);
		write_vars(|v| v.set_param(key, param))?;
	}
	let mut tokenizer = LashTokenizer::new(&input);
	let mut last_result = LashWait::new();
	loop {
		let state = descend(&mut tokenizer);
		match state {
			Ok(parse_state) => {
				if deconstruct!(NdType::Root { deck }, &parse_state.ast.nd_type, {
					deck.is_empty()
				}) { break Ok(LashWait::Success) }
				let result = traverse_ast(parse_state.ast, None);
				match result {
					Ok(code) => {
						if let LashWait::Fail { code, cmd } = code {
							panic!()
						} else {
							last_result = code;
						}
					}
					Err(e) => {
						event::throw(e).unwrap();
					}
				}
			}
			Err(e) => {
				event::throw(e).unwrap();
			}
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
