use std::collections::VecDeque;

use pest::iterators::Pair;

use crate::{error::{LashErr, LashErrLow}, execute::{ExecCtx, Redir}, helper::StrExtension, LashResult, Rule};

pub const ARG_RULES: [Rule; 2] = [Rule::arg_assign,Rule::word];

pub trait Rules {
	fn matches(&self, rule: Rule) -> bool;
}

impl Rules for Rule {
	fn matches(&self, rule: Rule) -> bool {
		rule == *self
	}
}

impl Rules for &[Rule] {
	fn matches(&self, rule: Rule) -> bool {
		self.contains(&rule)
	}
}

pub trait OptPairExt<'a> {
	fn unpack(self) -> LashResult<Pair<'a,Rule>>;
}

impl<'a> OptPairExt<'a> for Option<Pair<'a,Rule>> {
	/// There are many places in the lash codebase where we can be reasonably certain that an Option<Pair> will be Some
	/// However, if we are wrong for whatever reason, it's probably better to not crash the program by calling unwrap()
	///
	/// This function is essentially a safe unwrap that returns our error type instead of panicking
	fn unpack(self) -> LashResult<Pair<'a,Rule>> {
		if let Some(pair) = self {
			Ok(pair)
		} else {
			Err(LashErr::Low(LashErrLow::InternalErr("Called unpack() on a None value".into())))
		}
	}
}

pub trait PairExt<'a> {
	fn to_vec(self) -> Vec<Pair<'a,Rule>>;
	fn to_deque(self) -> VecDeque<Pair<'a,Rule>>;
	fn contains_rules(&self, rule: &[Rule]) -> bool;
	fn process_args(&self, ctx: &mut ExecCtx) -> Vec<String>;
	fn filter<R: Rules>(&self, rules: R) -> VecDeque<Pair<'a,Rule>>;
	fn scry<R: Rules>(&self, rules: R) -> Option<Pair<'a,Rule>>;
	fn step(self, count: usize) -> Option<Pair<'a,Rule>>;
}

impl<'a> PairExt<'a> for Pair<'a,Rule> {
	/// Goes straight down, a certain number of times.
	/// Useful for rules like `esc_sequence > esc_vis_grp > esc_sequence > esc_runtime: "\\D"` which have a long chain of single rules to descend through
	fn step(self, count: usize) -> Option<Pair<'a, Rule>> {
		let mut current = self;

		for _ in 0..count {
			current = current.into_inner().next()?; // Go one level deeper
		}

		Some(current)
	}
	/// Filter inner pairs by a rule type
	fn filter<R: Rules>(&self, rules: R) -> VecDeque<Pair<'a,Rule>> {
		self.clone().into_inner().filter(|pr| rules.matches(pr.as_rule())).collect::<VecDeque<_>>()
	}
	/// Traverse the pair and return the first pair that contains the given rule
	fn scry<R: Rules>(&self, rules: R) -> Option<Pair<'a,Rule>> {
		let mut stack = self.clone().to_deque();
		while let Some(pair) = stack.pop_front() {
			if rules.matches(pair.as_rule()) {
				return Some(pair)
			} else {
				let pair_inner = pair.to_deque();
				stack.extend(pair_inner);
			}
		}
		None
	}
	fn to_vec(self) -> Vec<Pair<'a,Rule>> {
		self.into_inner().collect::<Vec<_>>()
	}
	fn to_deque(self) -> VecDeque<Pair<'a,Rule>> {
		self.into_inner().collect::<VecDeque<_>>()
	}
	/// Automatically process command arguments, sorting words and redirections
	fn process_args(&self, ctx: &mut ExecCtx) -> Vec<String> {
		let mut argv = vec![];
		if self.as_rule() != Rule::simple_cmd {
			return argv
		}
		let inner = self.clone().into_inner();
		for arg in inner {
			match arg.as_rule() {
				Rule::word | Rule::cmd_name | Rule::arg_assign => argv.push(arg.as_str().trim_quotes()),
				Rule::redir => ctx.push_redir(Redir::from_pair(arg).unwrap()),
				_ => unreachable!("Unexpected rule: {:?}",arg.as_rule())
			}
		}
		argv
	}
	fn contains_rules(&self, rules: &[Rule]) -> bool {
	  let clone = self.clone();
		let mut stack = clone.to_deque();
		while let Some(pair) = stack.pop_front() {
			if rules.contains(&pair.as_rule()) {
				return true;
			}
			let inner = pair.to_deque();
			if !inner.is_empty() {
				stack.extend(inner);
			}
		}
		false
	}
}
