use std::collections::VecDeque;

use error::{LashErr, LashErrHigh, LashErrLow};
use execute::{ExecCtx, ProcIO, SavedIO};
use pest::{iterators::{Pair, Pairs}, Parser, Span};
use pest_derive::Parser;

pub mod prompt;
pub mod execute;
pub mod error;
pub mod shellenv;
pub mod shopt;
pub mod helper;
pub mod signal;
pub mod expand;
pub mod builtin;

pub trait PairExt<'a> {
	fn to_vec(self) -> Vec<Pair<'a,Rule>>;
	fn to_vec_rev(self) -> Vec<Pair<'a,Rule>>;
	fn contains_rules(&self, rule: &[Rule]) -> bool;
	fn unpack(self) -> Option<Pair<'a,Rule>>;
	fn unpack_count(self, count: usize) -> Option<Pair<'a, Rule>>;
}

impl<'a> PairExt<'a> for Pair<'a,Rule> {
	fn to_vec(self) -> Vec<Pair<'a,Rule>> {
		self.into_inner().collect::<Vec<_>>()
	}
	fn to_vec_rev(self) -> Vec<Pair<'a,Rule>> {
		self.into_inner().rev().collect::<Vec<_>>()
	}
	fn contains_rules(&self, rules: &[Rule]) -> bool {
	  let clone = self.clone();
		let mut stack = clone.to_vec_rev();
		while let Some(pair) = stack.pop() {
			if rules.contains(&pair.as_rule()) {
				return true;
			}
			let inner = pair.to_vec_rev();
			if !inner.is_empty() {
				stack.extend(inner);
			}
		}
		false
	}
	/// These methods are used when a rule follows a structure like `rule1 -> rule2 -> rule3 -> "text"` or something
	/// Allows you to quickly descend through layers
	fn unpack(self) -> Option<Pair<'a,Rule>> {
	  self.into_inner().into_iter().next()
	}
	fn unpack_count(self, count: usize) -> Option<Pair<'a, Rule>> {
		let mut next = Some(self);
		for _ in 0..count {
			next = next?.into_inner().into_iter().next();
		}
		next
	}
}

pub type LashResult<T> = Result<T,error::LashErr>;

#[derive(Parser)]
#[grammar = "pest/lash_lang.pest"]
pub struct LashParse;

fn main() {
	let mut ctx = ExecCtx::new();
	loop {
		let input = prompt::run_prompt();
		if &input == "break" { break };
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
	let node_stack = ast.into_iter().collect::<Vec<_>>();
	if node_stack.is_empty() {
		return Ok(())
	}
	let blame_target = node_stack.last().unwrap().clone();
	helper::proc_res(execute::descend(node_stack, ctx), blame_target)
}
