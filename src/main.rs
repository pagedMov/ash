use std::collections::VecDeque;

use error::{LashErr, LashErrHigh, LashErrLow};
use execute::{LashWait, ProcIO};
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
	loop {
		let input = prompt::run_prompt();
		if &input == "break" { break };
		let lists = get_cmd_lists(&input).unwrap();
		for list in lists {
			let result = exec_list(Rule::cmd_list, list, None);
			match result {
				Ok(_) => continue,
				Err(e) => eprintln!("{}",e)
			}
		}
	}
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
		let expanded = expand::expand_list(list,false)?;
		list_bodies.push(expanded);
	}

	Ok(list_bodies)
}

pub fn exec_list<'a>(rule: Rule, input: String, io: Option<ProcIO>) -> LashResult<LashWait> {
	let ast = LashParse::parse(rule, &input).map_err(|e| LashErr::Low(LashErrLow::Parse(e.to_string())))?;
	let mut node_stack = ast.into_iter().collect::<Vec<_>>();
	let io = io.unwrap_or_default();
	execute::descend(node_stack, io)
}
