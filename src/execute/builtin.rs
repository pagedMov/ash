use crate::{builtin::{self, BUILTINS}, dispatch, helper, prelude::*};

pub fn exec_builtin(cmd: Pair<Rule>, name: &str, lash: &mut Lash) -> LashResult<()> {
	let blame = cmd.clone();
	match name {
		"test" | "[" => {
			let mut argv = helper::prepare_argv(cmd,lash)?;
			argv.pop_front(); // Ignore the command name
			let result = helper::proc_res(builtin::test::test(&mut argv, lash), blame)?;
			if result {
				lash.set_code(0);
				return Ok(())
			} else {
				lash.set_code(1);
				return Ok(())
			}
		}
		"string" | "float" | "int" | "arr" | "bool" => builtin::assign::execute(cmd, lash)?,
		"pushd" => builtin::dir_stack::pushd(cmd, lash)?,
		"source" => builtin::source::execute(cmd, lash)?,
		"popd" => builtin::dir_stack::popd(cmd, lash)?,
		"setopt" => builtin::opts::setopt(cmd, lash)?,
		"cd" => builtin::cd::execute(cmd, lash)?,
		"alias" => builtin::alias::execute(cmd, lash)?,
		"pwd" => builtin::pwd::execute(cmd, lash)?,
		"export" => builtin::export::execute(cmd, lash)?,
		"echo" => builtin::echo::execute(cmd, lash)?,
		"builtin" | "command" => {
			let old_cmd = cmd.as_str();
			let mut inner = cmd.into_inner();
			let builtin_cmd = inner.next().unwrap(); // Cut off 'builtin'
			let span = builtin_cmd.as_span();
			let relative_span_end = span.end() - span.start();
			let new_cmd = &old_cmd[relative_span_end..];
			if new_cmd.trim().is_empty() {
				return Err(High(LashErrHigh::exec_err("Expected a builtin command here", blame)))
			}
			let new_pair = LashParse::parse(Rule::cmd_list,new_cmd.trim_start())?
				.next()
				.unpack()?
				.step(1)
				.unpack()?;
			let command_name = new_pair.clone().into_inner().find(|pair| pair.as_rule() == Rule::cmd_name).unpack()?.as_str();
			match name {
				"builtin" => {
					if BUILTINS.contains(&command_name) {
						exec_builtin(new_pair, command_name, lash)?
					} else {
						return Err(High(LashErrHigh::exec_err("Expected a builtin command here", blame)))
					}
				}
				"command" => {
					if !BUILTINS.contains(&command_name) {
						dispatch::dispatch_exec(new_pair, lash)?
					} else {
						return Err(High(LashErrHigh::exec_err("Expected a non-builtin command here", blame)))
					}
				}
				_ => unreachable!()
			}
		}
		_ => unimplemented!("Have not implemented support for builtin `{}` yet",name)
	};
	lash.set_code(0);
	Ok(())
}
