use std::{collections::VecDeque, env, path::PathBuf};

use clap::{Arg, ArgAction, Command, Parser as ClapParser};
use error::{LashErr, LashErrExt, LashErrHigh, LashErrLow};
use execute::{ExecCtx, ProcIO, Redir, RustFd, SavedIO};
use expand::expand_list;
use helper::StrExtension;
use pair::{OptPairExt, PairExt};
use pest::{iterators::{Pair, Pairs}, Parser, Span};
use pest_derive::Parser as PestParser;
use shellenv::{read_meta, write_meta, write_vars, EnvFlags};

pub mod prompt;
pub mod execute;
pub mod error;
pub mod shellenv;
pub mod shopt;
pub mod helper;
pub mod signal;
pub mod expand;
pub mod builtin;
pub mod comp;
pub mod pair;


pub type LashResult<T> = Result<T,error::LashErr>;

#[derive(pest_derive::Parser)]
#[grammar = "pest/lash_lang.pest"]
pub struct LashParse;

pub fn exec_input(mut input: String, ctx: &mut ExecCtx) -> LashResult<()> {
	let mut lists = LashParse::parse(Rule::main, &input).map_err(|e| LashErr::Low(LashErrLow::Parse(e.to_string())))?.next().unwrap().into_inner().collect::<VecDeque<_>>();
	lists.pop_back();
	// Chew through the input one list at a time
	while let Some(list) = lists.pop_front() {
		let mut cmds = list.into_inner();
		while let Some(cmd) = cmds.next() {
			if cmd.as_rule() == Rule::op {
				let op = cmd.scry(&[Rule::and,Rule::or][..]).unpack()?;
				match op.as_rule() {
					Rule::and => {
						if shellenv::check_status()? != "0" {
							break
						} else {
							continue
						}
					}
					Rule::or => {
						if shellenv::check_status()? == "0" {
							break
						} else {
							continue
						}
					}
					_ => unreachable!()
				}
			}
			let cmd_rule = cmd.as_rule();
			let blame = cmd.clone();
			let expanded = expand::expand_cmd(cmd)?;
			let re_parse = LashParse::parse(cmd_rule, &expanded)
				.map_err(|e| LashErr::Low(LashErrLow::Parse(e.to_string())))?
				.next().unwrap();
			let node_stack = VecDeque::from([re_parse]);
			helper::proc_res(execute::descend(node_stack, ctx), blame)?;
		}
	}
	Ok(())
}

pub fn save_fds() -> LashResult<(RustFd,RustFd,RustFd)> {
	Ok((
		RustFd::from_stdin()?,
		RustFd::from_stdout()?,
		RustFd::from_stderr()?
	))
}

pub fn restore_fds(mut stdio: (RustFd,RustFd,RustFd)) -> LashResult<()> {
	stdio.0.dup2(&0)?;
	stdio.0.close()?;
	stdio.1.dup2(&1)?;
	stdio.1.close()?;
	stdio.2.dup2(&2)?;
	stdio.2.close()?;
	Ok(())
}


#[derive(Debug,ClapParser)]
#[command(name = "lash")]
#[command(version = "v0.5.0-alpha")]
#[command(about = "A linux shell written in Rust")]
#[command(author = "Kyler Clay <kylerclay@proton.me>")]
struct LashArgs {
	script: Option<PathBuf>,

	#[arg(long = "no-rc", action = ArgAction::SetTrue, help = "Run without executing .lashrc")]
	no_rc: bool,

	#[arg(long = "rc-path", value_name = "FILE", help = "Set a custom path to .lashrc")]
	rc_path: Option<PathBuf>,

	#[arg(long = "no-history", action = ArgAction::SetTrue, help = "Run without loading .lash_hist" )]
	no_hist: bool,

	#[arg(long = "history-path", value_name = "FILE", help = "Set a custom path to .lash_hist")]
	hist_path: Option<PathBuf>,

	#[arg(short = 'c', value_name = "COMMAND", help = "Run a single command and then exit")]
	command: Option<String>
}

fn main() {
	let mut ctx = ExecCtx::new();
	loop {
		let is_initialized = read_meta(|m| m.flags().contains(EnvFlags::INITIALIZED)).catch();
		let args = LashArgs::parse();
		if args.no_rc {
			env::set_var("PS1", "$> ");
			write_vars(|v| v.export_var("PS1", "$> ")).catch();
		}
		if is_initialized == Some(false) && !args.no_rc {
			shellenv::source_rc(args.rc_path).catch();
		}
		let input = prompt::run_prompt().catch().unwrap_or_default();
		write_meta(|m| m.start_timer()).catch();
		ctx.push_state().catch();

		let saved_fds = save_fds().unwrap();
		let result = exec_input(input, &mut ctx);
		restore_fds(saved_fds).catch();

		ctx.pop_state().catch();
		match result {
			Ok(_) => continue,
			Err(e) => eprintln!("{}",e)
		}
	}
}
