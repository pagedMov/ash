use std::{env, iter::Rev, mem::take, os::fd::AsRawFd};

use chrono::Local;
use nix::unistd::{fork, ForkResult};
use pest::{iterators::{Pair, Pairs}, Parser, Span};

use crate::{error::LashErrLow, exec_input, execute::{exec_subshell, ExecCtx, ExecFlags, ProcIO, Redir, RustFd}, helper::{self, StrExtension}, shellenv::{read_logic, read_vars, LashVal, LogicTable, VarTable}, OptPairExt, PairExt};
use crate::{error::LashErr, LashParse, LashResult, Rule};

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
	let mut inner = pair.into_inner().next().unpack()?.into_inner().peekable();
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
		pair = pair.into_inner().next().unpack()?;
	}
	assert!(pair.as_rule() == Rule::cmd_sub);
	// Get the subshell token
	let subshell = pair.into_inner().next().unpack()?;
	let body = subshell.into_inner().next().unpack()?;

	// Set up output redirection. I originally used ProcIO for this, but I had a hunch that turning it in a redir like this would be better.
	// I don't really know why it would be, and it certainly looks goofier, but it works so whatever
	let (mut r_pipe, mut w_pipe) = RustFd::pipe()?;
	let redir = &format!("1>&{}",w_pipe.as_raw_fd().to_string());
	let sub_redir = LashParse::parse(Rule::redir,redir).unwrap().next().unpack()?;
	let mut ctx = ExecCtx::new();
	let flags = ctx.flags_mut();
	*flags |= ExecFlags::NO_FORK; // Tell the child proc to not fork since it's already in a fork
	ctx.push_redir(Redir::from_pair(sub_redir)?);

	match unsafe { fork() } {
		Ok(ForkResult::Child) => {
			r_pipe.close()?;
			// Execute the subshell body with the ctx payload
			exec_input(body.as_str().to_string(), &mut ctx)?;
			std::process::exit(1);
		}
		Ok(ForkResult::Parent { child: _ }) => {
			w_pipe.close()?;
		}
		Err(_) => panic!()
	}

	let buffer = r_pipe.read()?;
	r_pipe.close()?;
	Ok(buffer)
}

fn expand_proc_sub(pair: Pair<Rule>) -> String {
	todo!()
}

fn expand_tilde(pair: Pair<Rule>) -> LashResult<String> {
	let tilde_sub = pair.into_inner().next().unpack()?;
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
		let expanded = expand_esc(esc, &mut result)?;
		result = replace_span(result, span, &expanded);
	}
	Ok(result)
}

pub fn expand_time(fmt: &str) -> String {
	let right_here_right_now = Local::now();
	right_here_right_now.format(fmt).to_string()
}

pub fn expand_esc<'a>(pair: Pair<'a,Rule>, buffer: &mut String) -> LashResult<String> {
	let pair = pair.into_inner().next().unpack()?;
	Ok(match pair.as_rule() {
		Rule::esc_bell => "\x07".into(),
		Rule::esc_dquote => "\"".into(),
		Rule::esc_squote => "'".into(),
		Rule::esc_return => "\r".into(),
		Rule::esc_newline => "\n".into(),
		Rule::esc_vis_grp => helper::handle_prompt_visgroup(pair, buffer)?,
		Rule::esc_user_seq => {
			let query = pair.into_inner().next().unpack()?.as_str();
			helper::escseq_custom(query)?
		}
		Rule::esc_ascii_oct => {
			let octal = pair.into_inner().next().unpack()?.as_str();
			if let Ok(val) = u8::from_str_radix(octal, 8) {
				(val as char).to_string()
			} else {
				return Err(LashErr::Low(LashErrLow::InternalErr("from_str_radix() failed in prompt expansion".into())))
			}
		}
		Rule::esc_ansi_seq  => {
			let params = pair.into_inner().next().unpack()?.as_str();
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
	if let Some(pos) = buffer.rfind(pat) {
		let mut result = buffer.to_string();
		result.replace_range(pos..pos+pat.len(), new);
		result
	} else {
		buffer.to_string()
	}
}

pub fn replace_span(buffer: String, pos: Span, replace: &str) -> String {
	let start = pos.start();
	let end = pos.end();
	let left = &buffer[..start];
	let right = &buffer[end..];
	format!("{}{}{}",left,replace,right)
}
