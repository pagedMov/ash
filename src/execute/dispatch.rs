use crate::{builtin::{self, BUILTINS}, expand, helper, prelude::*, script, utils::{ExecFlags, Redir}};

use super::{pipeline, command, func};

pub fn dispatch_exec<'a>(node: Pair<'a,Rule>, lash: &mut Lash) -> LashResult<()> {
		match node.as_rule() {
			Rule::simple_cmd => {
				let command_name = node.clone().into_inner().find(|pair| pair.as_rule() == Rule::cmd_name).unpack()?.as_str();
				if !lash.ctx().flags().contains(ExecFlags::IGN_FUNC) && lash.is_func(command_name)? {
					func::exec_func(node,lash)?;
				} else if BUILTINS.contains(&command_name) {
					exec_builtin(node,command_name,lash)?;
				} else {
					command::exec_cmd(node, lash)?;
				}
			}
			Rule::shell_cmd => {
				let mut shell_cmd_inner = node.to_deque();
				let shell_cmd = shell_cmd_inner.pop_front().unpack()?;
				while shell_cmd_inner.front().is_some_and(|pair| pair.as_rule() == Rule::redir) {
					let redir = Redir::from_pair(shell_cmd_inner.pop_front().unpack()?)?;
					lash.ctx_mut().push_redir(redir);
				}
				match shell_cmd.as_rule() {
					Rule::for_cmd => script::fordo::exec_for_cmd(shell_cmd, lash)?,
					Rule::match_cmd => script::matchdo::exec_match_cmd(shell_cmd, lash)?,
					Rule::loop_cmd => script::loopdo::exec_loop_cmd(shell_cmd, lash)?,
					Rule::if_cmd => script::ifthen::exec_if_cmd(shell_cmd, lash)?,
					Rule::subshell => super::subshell::exec_subshell(shell_cmd, lash)?,
					Rule::brace_grp => todo!(),
					Rule::assignment => super::assignment::exec_assignment(shell_cmd, lash)?,
					Rule::func_def => super::func::exec_func_def(shell_cmd, lash)?,
					_ => unreachable!()
				};
			}
			Rule::pipeline => { pipeline::exec_pipeline(node, lash)?; },
			Rule::EOI => { /* Do nothing */ }
			_ => todo!("Support for rule '{:?}' is unimplemented",node.as_rule())
		}
		Ok(())
}

pub fn descend(mut node_stack: VecDeque<Pair<Rule>>, lash: &mut Lash) -> LashResult<()> {
	lash.ctx_mut().descend()?; // Increment depth counter
	while let Some(node) = node_stack.pop_front() {
		match node.as_rule() {
			Rule::main | Rule::cmd_list => {
				let inner = node.to_deque();
				node_stack.extend(inner);
			}
			Rule::op => {
				let option = node.step(1);
				if let Some(op) = option {
					match op.as_rule() {
						Rule::and => {
							let is_success = lash.get_status() == 0;
							if !is_success {
								break
							}
						}
						Rule::or => {
							let is_success = lash.get_status() == 0;
							if is_success {
								break
							}
						}
						_ => unreachable!()
					}
				}
			}
			Rule::bg_cmd => {
				if let Some(cmd) = node.step(1) {
					let flags = lash.ctx_mut().flags_mut();
					*flags |= ExecFlags::BACKGROUND;
					dispatch_exec(cmd, lash)?
				}
			}
			_ => dispatch_exec(node, lash)?
		}
	}
	lash.ctx_mut().ascend()?; // Decrement depth counter
	Ok(())
}

pub fn exec_input(mut input: String, lash: &mut Lash) -> LashResult<()> {
	input = expand::dispatch::expand_aliases(input, 0, vec![],lash)?;
	let mut lists = LashParse::parse(Rule::main, &input).map_err(|e| Low(LashErrLow::Parse(e.to_string())))?.next().unwrap().into_inner().collect::<VecDeque<_>>();
	lists.pop_back();
	// Chew through the input one list at a time
	while let Some(list) = lists.pop_front() {
		let mut cmds = list.into_inner();
		while let Some(cmd) = cmds.next() {
			if cmd.as_rule() == Rule::op {
				let op = cmd.scry(&[Rule::and,Rule::or][..]).unpack()?;
				match op.as_rule() {
					Rule::and => {
						if lash.get_status() != 0 {
							break
						} else {
							continue
						}
					}
					Rule::or => {
						if lash.get_status() == 0 {
							break
						} else {
							continue
						}
					}
					_ => unreachable!()
				}
			}
			let blame = cmd.clone();
			let node_stack = VecDeque::from([cmd]);
			helper::proc_res(descend(node_stack, lash), blame)?;
		}
	}
	Ok(())
}

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
		"return" => builtin::control::func_return(cmd, lash)?,
		"break" => builtin::control::loop_break(cmd, lash)?,
		"continue" => builtin::control::loop_continue()?,
		"pushd" => builtin::dir_stack::pushd(cmd, lash)?,
		"source" => builtin::source::execute(cmd, lash)?,
		"popd" => builtin::dir_stack::popd(cmd, lash)?,
		"setopt" => builtin::opts::setopt(cmd, lash)?,
		"getopt" => builtin::opts::getopt(cmd, lash)?,
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
						*lash.ctx_mut().flags_mut() |= ExecFlags::IGN_FUNC;
						dispatch_exec(new_pair, lash)?
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
