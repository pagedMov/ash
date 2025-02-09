use std::{collections::VecDeque, env, io::Read, iter::Rev, mem::take, os::fd::AsRawFd};

use chrono::Local;
use nix::unistd::{fork, ForkResult};
use pest::{iterators::{Pair, Pairs}, Parser, Span};

use crate::{error::LashErrLow, exec_input, execute::{exec_subshell, ExecCtx, ExecFlags, ProcIO, Redir, RustFd}, helper::{self, StrExtension}, shellenv::{read_logic, read_vars, LashVal, LogicTable, VarTable}, pair::OptPairExt, pair::PairExt};
use crate::{error::LashErr, LashParse, LashResult, Rule};

//FIXME: This function is most likely a structural weakness. It seems sound now, but something is off-putting about it.
// This currently works by expanding words *before* execution, but looking into some kind of JIT method also seems reasonable
// And way less error prone
pub fn expand_list<'a>(list: Pair<'a,Rule>) -> LashResult<String> {
	let mut buffer = list.get_input().to_string();
	let list_body = list.as_str().to_string();
	let mut result = String::new();
	let inner = list.into_inner().rev();
	// We check to see if we are in a command chain here
	// If we are, we return the whole input instead of just a slice
	let slice = inner.clone().count() == 1;

	for cmd in inner {
		if matches!(cmd.as_rule(), Rule::shell_cmd) && !cmd.scry(Rule::assignment).is_some() {
			result = list_body.clone();
			continue
		}

		// Get the span of the current cmd_list part
		let span = cmd.as_span();
		result = expand_cmd(cmd)?; // Expand it

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
		Rule::dquoted,
		Rule::cmd_sub,
		Rule::arr_index,
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
	const BREAKER_RULES: &[Rule] = &[
		Rule::assignment, // These rules signal the expansion logic to break up the pair
		Rule::arg_assign, // and push the inner pairs back onto the stack
		Rule::std_assign,
		Rule::plus_assign,
		Rule::minus_assign,
		Rule::increment,
		Rule::decrement
	];

	while let Some(word) = list.pop() {
		if BREAKER_RULES.contains(&word.as_rule()) {
			list.extend(word.to_vec());
			continue
		}
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
				Rule::dquoted => expand_string(word)?,
				Rule::arr_index => expand_index(word)?,
				Rule::glob_word => expand_glob(word),
				Rule::brace_word => expand_brace(word),
				Rule::cmd_sub => expand_cmd_sub(word)?,
				Rule::proc_sub => expand_proc_sub(word),
				Rule::tilde_sub => expand_tilde(word)?,
				_ => unreachable!()
			};
			result = replace_span(result, span, &expanded);
		}
	}

	Ok(result)
}

fn expand_string(pair: Pair<Rule>) -> LashResult<String> {
	let body = pair.scry(Rule::dquote_body);
	if body.is_none() {
		return Ok(String::new());
	}
	let body = body.unwrap();
	// TODO: this is three unwraps in a row. One is guaranteed to work, but still
	let mut sub_expansions = LashParse::parse(Rule::find_expansions, body.as_str()).unwrap().next().unwrap().to_vec();
	let mut result = body.as_str().to_string();
	while let Some(word) = sub_expansions.pop() {
		let span = word.as_span();
		let mut inner = word.clone().into_inner();
		if word.clone().into_inner().count() == 0 {
			continue
		} else {
			let sub_type = inner.next().unpack()?;
			let expanded = match sub_type.as_rule() {
				Rule::var_sub => {
					read_vars(|v| v.get_var(&word.as_str()[1..]))?.unwrap_or_default().to_string()
				}
				Rule::param_sub => {
					let param = read_vars(|v| v.get_param(&word.as_str()[1..]))?.unwrap_or_default().to_string();
					param
				}
				Rule::cmd_sub => {
					let result = expand_cmd_sub(word)?;
					result
				}
				Rule::arr_index => expand_index(word)?,
				Rule::proc_sub => expand_proc_sub(word),
				_ => continue
			};
			result = replace_span(result, span, &expanded);
		}
	}
	result = format!("\"{}\"",result);
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

fn expand_index(pair: Pair<Rule>) -> LashResult<String> {
	let mut inner = pair.step(1).unpack()?.into_inner().peekable();
	let arr_name = inner.next().unpack()?;

	let array = read_vars(|v| v.get_var(arr_name.as_str()))?;
	let mut cur_val = match array {
		Some(LashVal::Array(vec)) => Some(LashVal::Array(vec)),
		_ => return Ok(String::new()), // If not an array, return empty
	};

	while let Some(index) = inner.next() {
		let idx = index.as_str().parse::<usize>().map_err(|_| LashErr::Low(LashErrLow::IndexErr(format!("Index '{}' out of range for array '{}'",index,arr_name.as_str()))))?;
		cur_val = match cur_val {
			Some(LashVal::Array(vec)) => vec.get(idx).cloned(),
			_ => return Ok(String::new()), // Invalid nesting (e.g., indexing a non-array)
		};
	}

	Ok(cur_val.map_or_else(String::new, |val| val.to_string()))
}

fn expand_brace(pair: Pair<Rule>) -> String {
	todo!()
}

pub fn expand_cmd_sub(mut pair: Pair<Rule>) -> LashResult<String> {
	if pair.as_rule() == Rule::word {
		pair = pair.step(1).unpack()?;
	}
	assert!(pair.as_rule() == Rule::cmd_sub);
	// Get the subshell token
	let body = pair.step(1).unpack()?;

	let (mut r_pipe, mut w_pipe) = RustFd::pipe()?;
	let redir = Redir::from_raw(1,w_pipe.as_raw_fd());
	let mut ctx = ExecCtx::new();
	let flags = ctx.flags_mut();
	*flags |= ExecFlags::NO_FORK; // Tell the child proc to not fork since it's already in a fork
	ctx.push_redir(redir);

	match unsafe { fork() } {
		Ok(ForkResult::Child) => {
			r_pipe.close()?;
			// Execute the subshell body with the ctx payload
			exec_input(body.as_str().consume_escapes(), &mut ctx)?;
			std::process::exit(1);
		}
		Ok(ForkResult::Parent { child: _ }) => {
			w_pipe.close()?;
		}
		Err(_) => panic!()
	}

	let mut buffer = String::new();
	r_pipe.read_to_string(&mut buffer)?;
	r_pipe.close()?;
	Ok(buffer.trim().to_string())
}

fn expand_proc_sub(pair: Pair<Rule>) -> String {
	todo!()
}

fn expand_tilde(pair: Pair<Rule>) -> LashResult<String> {
	let tilde_sub = pair.step(1).unpack()?;
	debug_assert!(tilde_sub.as_rule() == Rule::tilde_sub, "Found this: {:?}",tilde_sub.as_rule());
	let word = tilde_sub.as_str();
	let home = env::var("HOME").unwrap_or_default();
	Ok(word.replacen("~", &home, 1))
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

pub fn expand_prompt(input: Option<&str>) -> LashResult<String> {
	let mut prompt = read_vars(|v| v.get_evar("PS1"))?.unwrap_or_default();
	prompt = prompt.replace("\n", "");
	let mut result = prompt.clone();
	let mut prompt_parse = LashParse::parse(Rule::prompt, &prompt)
		.map_err(|e| LashErr::Low(LashErrLow::Parse(e.to_string())))?
		.into_iter()
		.next()
		.unpack()?
		.into_inner()
		.rev();

	while let Some(esc) = prompt_parse.next() {
		let span = esc.as_span();
		let expanded = expand_esc(esc)?;
		result = replace_span(result, span, &expanded);
	}
	Ok(result)
}

pub fn expand_time(fmt: &str) -> String {
	let right_here_right_now = Local::now();
	right_here_right_now.format(fmt).to_string()
}

pub fn expand_esc<'a>(pair: Pair<'a,Rule>) -> LashResult<String> {
	let pair = pair.step(1).unpack()?;
	Ok(match pair.as_rule() {
		Rule::esc_bell => "\x07".into(),
		Rule::esc_dquote => "\"".into(),
		Rule::esc_squote => "'".into(),
		Rule::esc_return => "\r".into(),
		Rule::esc_newline => "\n".into(),
		Rule::esc_vis_grp => helper::handle_prompt_visgroup(pair)?,
		Rule::esc_user_seq => {
			let query = pair.step(1).unpack()?.as_str();
			helper::escseq_custom(query)?
		}
		Rule::esc_ascii_oct => {
			let octal = pair.step(1).unpack()?.as_str();
			if let Ok(val) = u8::from_str_radix(octal, 8) {
				(val as char).to_string()
			} else {
				return Err(LashErr::Low(LashErrLow::InternalErr("from_str_radix() failed in prompt expansion".into())))
			}
		}
		Rule::esc_ansi_seq  => {
			let params = pair.step(1).unpack()?.as_str();
			format!("\x1B[{params}m")
		}
		Rule::esc_12hour_short => expand_time("%I:%M %p"),
		Rule::esc_24hour_short => expand_time("%H:%M"),
		Rule::esc_runtime => helper::escseq_cmdtime()?,
		Rule::esc_12hour => expand_time("%I:%M:%S"),
		Rule::esc_24hour => expand_time("%H:%M:%S"),
		Rule::esc_weekday => expand_time("%a %b %d"),
		Rule::esc_pwd => helper::escseq_working_directory()?,
		Rule::esc_pwd_short => helper::escseq_basename_working_directory()?,
		Rule::esc_hostname => helper::escseq_full_hostname()?,
		Rule::esc_hostname_short => helper::escseq_short_hostname()?,
		Rule::esc_shellname => helper::escseq_shell_name()?,
		Rule::esc_username => helper::escseq_username()?,
		Rule::esc_prompt_symbol => helper::escseq_prompt_symbol()?.to_string(),
		Rule::esc_exit_code => helper::escseq_exitcode()?,
		Rule::esc_success_symbol => helper::escseq_success()?,
		Rule::esc_failure_symbol => helper::escseq_fail()?,
		_ => unreachable!("Got this rule in prompt expansion: {:?}",pair.as_rule())
	})
}

pub fn replace_from_right(buffer: &str, pat: &str, new: &str) -> String {
	let mut working_buffer = VecDeque::new();
	let mut chars = buffer.chars().collect::<VecDeque<_>>();
	let mut pos = chars.len();
	let mut prev_char = None;
	let mut result = buffer.to_string();  // Start with a copy of the original buffer

	// Traverse the string from the end to the beginning
	while let Some(ch) = chars.pop_back() {
		pos -= 1;
		working_buffer.push_front(ch);

		// Keep the last 'pat.len()' characters in the working buffer
		if working_buffer.len() > pat.len() {
			prev_char = working_buffer.pop_back();
		}

		// Check if the current buffer matches the pattern
		if working_buffer.iter().collect::<String>().as_str() == pat {
			let not_substring = chars.back().map_or(true, |ch| ch.is_whitespace() || *ch == ';')
				&& prev_char.map_or(true, |ch| ch.is_whitespace() || ch == ';');

			// Only replace if it's a valid match (i.e., not part of a larger word)
			if not_substring {
				result.replace_range(pos..pos+pat.len(), new);
				break;  // Replace only the first valid occurrence
			}
		}
	}
	result
}

#[track_caller]
pub fn replace_span(buffer: String, pos: Span, replace: &str) -> String {
	//dbg!(std::panic::Location::caller());
	let start = pos.start();
	let end = pos.end();
	let left = &buffer[..start];
	let right = &buffer[end..];
	format!("{}{}{}",left,replace,right)
}
