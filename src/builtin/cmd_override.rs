use crate::{helper, prelude::*, utils};

use super::BUILTINS;

pub fn execute<'a>(pair: Pair<'a,Rule>, lash: &mut Lash, builtin: bool) -> LashResult<()> {
	let blame = pair.clone();
	let old_cmd = pair.as_str();
	let mut inner = pair.into_inner();
	let override_cmd = inner.next().unwrap();
	let span = override_cmd.as_span();
	let relative_span_end = span.end() - span.start();
	let new_cmd = &old_cmd[relative_span_end..];
	if new_cmd.trim().is_empty() {
		return Err(High(LashErrHigh::exec_err("Expected a command name here", blame)))
	}
	let new_pair = LashParse::parse(Rule::cmd_list,new_cmd.trim_start())?
		.next()
		.unpack()?
		.step(1)
		.unpack()?;
	let command_name = new_pair.clone().into_inner().find(|pair| pair.as_rule() == Rule::cmd_name).unpack()?.as_str();
	if builtin {
		if BUILTINS.contains(&command_name) {
			crate::execute::dispatch::exec_builtin(new_pair, command_name, lash)?
		} else {
			return Err(High(LashErrHigh::exec_err("Expected a builtin command here", blame)))
		}
	} else {
		if !BUILTINS.contains(&command_name) {
			*lash.ctx_mut().flags_mut() |= utils::ExecFlags::IGN_FUNC;
			crate::execute::dispatch::dispatch_exec(new_pair, lash)?
		} else {
			return Err(High(LashErrHigh::exec_err("Expected a command here", blame)))
		}
	}
	Ok(())
}
