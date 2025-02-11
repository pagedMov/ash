use crate::{builtin::BUILTINS, expand, helper, prelude::*, script, utils::{ExecFlags, Redir}};

use super::{builtin, pipeline, command, func};

pub fn dispatch_exec<'a>(node: Pair<'a,Rule>, lash: &mut Lash) -> LashResult<()> {
		match node.as_rule() {
			Rule::simple_cmd => {
				let command_name = node.clone().into_inner().find(|pair| pair.as_rule() == Rule::cmd_name).unpack()?.as_str();
				if lash.is_func(command_name)? {
					func::exec_func(node,lash)?;
				} else if BUILTINS.contains(&command_name) {
					builtin::exec_builtin(node,command_name,lash)?;
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
