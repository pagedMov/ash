use std::{collections::VecDeque, env, path::PathBuf};

use error::{LashErr, LashErrHigh, LashErrLow};
use execute::{ExecCtx, ProcIO, Redir, SavedIO};
use helper::StrExtension;
use pest::{iterators::{Pair, Pairs}, Parser, Span};
use pest_derive::Parser;
use shellenv::{read_meta, write_meta, EnvFlags};

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

#[derive(Parser)]
#[grammar = "pest/lash_lang.pest"]
pub struct LashParse;

fn main() {
	let mut ctx = ExecCtx::new();
	loop {
		let is_initialized = read_meta(|m| m.flags().contains(EnvFlags::INITIALIZED)).unwrap();
		if !is_initialized {
			write_meta(|m| m.mod_flags(|f| *f |= EnvFlags::INITIALIZED)).unwrap();
			let home = env::var("HOME").unwrap();
			shellenv::source_file(PathBuf::from(format!("{home}/.lashrc"))).unwrap();
		}
		let input = prompt::run_prompt().unwrap();
		ctx.push_state().unwrap();
		let saved_fds = SavedIO::new().unwrap();
		let result = exec_input(input, &mut ctx);
		saved_fds.restore().unwrap();
		ctx.pop_state().unwrap();
		match result {
			Ok(_) => continue,
			Err(e) => eprintln!("{}",e)
		}
	}
}

pub fn exec_input(input: String, ctx: &mut ExecCtx) -> LashResult<()> {
	let lists = get_cmd_lists(&input);
	if let Err(e) = lists {
		return Err(LashErr::Low(LashErrLow::Parse(e.to_string())))
	}
	for list in lists.unwrap() {
		exec_list(Rule::cmd_list, list, ctx)?;
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

pub fn exec_list<'a>(rule: Rule, input: String, ctx: &mut ExecCtx) -> LashResult<()> {
	let ast = LashParse::parse(rule, &input).map_err(|e| LashErr::Low(LashErrLow::Parse(e.to_string())))?;
	let node_stack = ast.into_iter().collect::<VecDeque<_>>();
	if node_stack.is_empty() {
		return Ok(())
	}
	let blame_target = node_stack.front().unwrap().clone();
	helper::proc_res(execute::descend(node_stack, ctx), blame_target)
}
