use std::{collections::VecDeque, env, path::PathBuf};

use clap::{Arg, ArgAction, Command, Parser as ClapParser};
use error::{LashErr, LashErrExt, LashErrHigh, LashErrLow};
use execute::{ExecCtx, ProcIO, Redir, RustFd, SavedIO};
use expand::expand_list;
use helper::StrExtension;
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
		let expanded = expand_list(list)?;
		exec_list(Rule::cmd_list, expanded, ctx)?;
	}
	Ok(())
}

/// Separate and expand logical blocks
/// This function parses the input, and then processes each command list
/// Each list is then expanded by expand_list, which covers each kind of text expansion
pub fn get_cmd_lists<'a>(input: &'a str) -> LashResult<Vec<String>> {
	let ast = LashParse::parse(Rule::main, &input).map_err(|e| LashErr::Low(LashErrLow::Parse(e.to_string())))?;
	let mut lists = ast.into_iter().next().unwrap().into_inner();
	let mut list_bodies = vec![];
	while let Some(list) = lists.next() {
		if list.as_rule() == Rule::EOI { break };
		let expanded = expand::expand_list(list)?;
		list_bodies.push(expanded);
	}

	Ok(list_bodies)
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

pub fn exec_list<'a>(rule: Rule, input: String, ctx: &mut ExecCtx) -> LashResult<()> {
	let ast = LashParse::parse(rule, &input).map_err(|e| LashErr::Low(LashErrLow::Parse(e.to_string())))?;
	let node_stack = ast.into_iter().collect::<VecDeque<_>>();
	if node_stack.is_empty() {
		return Ok(())
	}
	let blame_target = node_stack.front().unwrap().clone();
	helper::proc_res(execute::descend(node_stack, ctx), blame_target)
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
			write_meta(|m| m.mod_flags(|f| *f |= EnvFlags::INITIALIZED)).catch();
			let home = env::var("HOME").unwrap();
			if let Err(e) = shellenv::source_file(PathBuf::from(format!("{home}/.lashrc"))) {
				shellenv::set_code(1).catch();
				eprintln!("Failed to source lashrc: {}",e);
			}
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
