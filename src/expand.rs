use std::{iter::Rev, mem::take};

use pest::{iterators::{Pair, Pairs}, Parser, Span};

use crate::{helper::{self, StrExtension}, shellenv::{read_logic, read_vars, LogicTable, VarTable}, OptPairExt, PairExt};
use crate::{error::{LashErr, LashErrHigh}, LashParse, LashResult, Rule};

//FIXME: This function is most likely a structural weakness. It seems sound now, but something is off-putting about it.
// This currently works by expanding words *before* execution, but looking into some kind of JIT method also seems reasonable
// And way less error prone
pub fn expand_list<'a>(list: Pair<'a,Rule>) -> LashResult<String> {
	let mut buffer = list.get_input().to_string();
	let mut result = String::new();
	let input_len = buffer.len();
	let inner = list.into_inner().rev();
	// We check to see if we are in a command chain here
	// If we are, we return the whole input instead of just a slice
	let slice = inner.clone().count() == 1;

	for cmd in inner {
		if matches!(cmd.as_rule(), Rule::shell_cmd) {
			result = buffer.clone();
			continue
		}

		// Get the span of the current cmd_list part
		let span = cmd.as_span();
		let expanded = expand_cmd(cmd)?; // Expand it
		// Replace the span with the expanded slice in the original input
		result = replace_span(buffer.clone(),span,&expanded);

		// Find the difference in length between the original input and expanded text
		let delta: i32 = result.len() as i32 - input_len as i32;
		let span_end = if delta > 0 {
			span.end() + delta as usize
		} else if delta < 0 {
			span.end().saturating_sub(delta.unsigned_abs() as usize)
		} else {
			span.end()
		};

		// Get just the expanded slice from result
		result = result[span.start()..span_end].to_string();
		// Reset the buffer with the newly expanded string
		buffer = replace_span(buffer,span,&result);

	}
	if slice {
		// Return just the expanded slice
		Ok(result)
	} else {
		// Return the entire expanded buffer
		Ok(buffer)
	}
}

pub fn expand_cmd<'a>(cmd: Pair<'a,Rule>) -> LashResult<String> {
	if cmd.as_rule() == Rule::op { return Ok(cmd.as_str().to_string()) }
	let mut buffer = cmd.as_str().to_string();
	// Order matters
	let expand_rules = [
		Rule::var_sub,
		Rule::param_sub,
		Rule::glob_word,
		Rule::cmd_sub,
		Rule::proc_sub,
		Rule::brace_word,
		Rule::tilde_sub
	];
	buffer = alias_pass(buffer)?;
	for rule in expand_rules {
		// Expand each rule in order
		buffer = rule_pass(rule,buffer)?;
	}
	Ok(buffer)
}

pub fn alias_pass<'a>(buffer: String) -> LashResult<String> {
	let mut result = buffer.clone();
	let mut list = LashParse::parse(Rule::find_expansions, &buffer).unwrap().next().unpack()?.to_vec();
	let logic = read_logic(|l| l.clone())?;

	while let Some(word) = list.pop() {
		if let Some(body) = logic.get_alias(word.as_str()) {
			let span = word.as_span();
			result = replace_span(result, span, &body);
		}
	}
	Ok(result)
}

pub fn rule_pass<'a>(rule: Rule, buffer: String) -> LashResult<String> {
	// Need to clone buffer here to detach 'result' from the lifetime of 'list'
	let mut result = buffer.clone();
	let mut list = LashParse::parse(Rule::find_expansions, &buffer).unwrap().next().unpack()?.to_vec();

	while let Some(word) = list.pop() {
		if word.contains_rules(&[rule]) {
			let span = word.as_span();
			let expanded = match rule {
				Rule::var_sub => {
					read_vars(|v| v.get_var(&word.as_str()[1..]))?.unwrap_or_default().to_string()
				}
				Rule::param_sub => {
					let param = read_vars(|v| v.get_param(&word.as_str()[1..]))?.unwrap_or_default().to_string();
					param
				}
				Rule::glob_word => expand_glob(word),
				Rule::brace_word => expand_brace(word),
				Rule::cmd_sub => expand_cmd_sub(word),
				Rule::proc_sub => expand_proc_sub(word),
				Rule::tilde_sub => expand_tilde(word),
				_ => unreachable!()
			};
			result = replace_span(result, span, &expanded);
		}
	}

	Ok(result)
}

fn expand_glob(pair: Pair<Rule>) -> String {
	let word = pair.as_str();
	let mut result = String::new();
	for entry in glob::glob(word).unwrap() {
		if let Ok(path) = entry {
			result = format!("{} {}",result,path.to_str().unwrap());
		}
	}
	result.trim().to_string()
}

fn expand_brace(pair: Pair<Rule>) -> String {
	todo!()
}

fn expand_cmd_sub(pair: Pair<Rule>) -> String {
	todo!()
}

fn expand_proc_sub(pair: Pair<Rule>) -> String {
	todo!()
}

fn expand_param_sub(pair: Pair<Rule>) -> String {
	todo!()
}

fn expand_tilde(pair: Pair<Rule>) -> String {
	todo!()
}

pub fn expand_shebang(shebang: &str) -> String {
	let mut command = shebang.trim_start_matches("#!").trim().to_string();
	if command.has_unescaped("/") {
		return format!("{}{command}{}","#!","\n");
	}
	if let Some(path) = helper::which(&command) {
		return format!("{}{path}{}","#!","\n");
	} else {
		return shebang.to_string()
	}
}

pub fn replace_span(buffer: String, pos: Span, replace: &str) -> String {
	let start = pos.start();
	let end = pos.end();
	let left = &buffer[..start];
	let right = &buffer[end..];
	format!("{}{}{}",left,replace,right)
}
