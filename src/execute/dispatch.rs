use crate::{builtin::{self, BUILTINS}, error::SlashErrExt, expand, helper, prelude::*, script, utils::{ExecFlags, Redir}};

use super::{pipeline, command, func};

pub fn dispatch_exec<'a>(node: Pair<'a,Rule>, slash: &mut Slash) -> SlashResult<()> {
		match node.as_rule() {
			Rule::simple_cmd => {
				let command_name = node.clone().into_inner().find(|pair| pair.as_rule() == Rule::cmd_name).unpack()?.as_str();
				if !slash.ctx().flags().contains(ExecFlags::IGN_FUNC) && slash.is_func(command_name)? {
					func::exec_func(node,slash)?;
				} else if BUILTINS.contains(&command_name) {
					exec_builtin(node,command_name,slash)?;
				} else {
					command::exec_cmd(node, slash)?;
				}
			}
			Rule::shell_cmd => {
				let mut shell_cmd_inner = node.to_deque();
				let shell_cmd = shell_cmd_inner.pop_front().unpack()?;
				while shell_cmd_inner.front().is_some_and(|pair| pair.as_rule() == Rule::redir) {
					let redir = Redir::from_pair(shell_cmd_inner.pop_front().unpack()?)?;
					slash.ctx_mut().push_redir(redir);
				}
				match shell_cmd.as_rule() {
					Rule::for_cmd => script::fordo::exec_for_cmd(shell_cmd, slash)?,
					Rule::match_cmd => script::matchdo::exec_match_cmd(shell_cmd, slash)?,
					Rule::loop_cmd => script::loopdo::exec_loop_cmd(shell_cmd, slash)?,
					Rule::if_cmd => script::ifthen::exec_if_cmd(shell_cmd, slash)?,
					Rule::subshell => super::subshell::exec_subshell(shell_cmd, slash)?,
					Rule::brace_grp => todo!(),
					Rule::assignment => super::assignment::exec_assignment(shell_cmd, slash)?,
					Rule::func_def => super::func::exec_func_def(shell_cmd, slash)?,
					_ => unreachable!()
				};
			}
			Rule::pipeline => { pipeline::exec_pipeline(node, slash)?; },
			Rule::EOI => { /* Do nothing */ }
			_ => todo!("Support for rule '{:?}' is unimplemented",node.as_rule())
		}
		Ok(())
}

pub fn descend(mut node_stack: VecDeque<Pair<Rule>>, slash: &mut Slash) -> SlashResult<()> {
	slash.ctx_mut().descend()?; // Increment depth counter
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
							let is_success = slash.get_status() == 0;
							if !is_success {
								break
							}
						}
						Rule::or => {
							let is_success = slash.get_status() == 0;
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
					let flags = slash.ctx_mut().flags_mut();
					*flags |= ExecFlags::BACKGROUND;
					dispatch_exec(cmd, slash)?
				}
			}
			_ => dispatch_exec(node, slash)?
		}
	}
	slash.ctx_mut().ascend()?; // Decrement depth counter
	Ok(())
}

pub fn exec_input(mut input: String, slash: &mut Slash) -> SlashResult<()> {
	input = expand::dispatch::expand_aliases(input, 0, vec![],slash)?;
	let mut lists = SlashParse::parse(Rule::main, &input).map_err(|e| Low(SlashErrLow::Parse(e.to_string())))?.next().unwrap().into_inner().collect::<VecDeque<_>>();
	lists.pop_back();
	// Chew through the input one list at a time
	while let Some(list) = lists.pop_front() {
		let mut cmds = list.into_inner();
		while let Some(cmd) = cmds.next() {
			if cmd.as_rule() == Rule::op {
				let op = cmd.scry(&[Rule::and,Rule::or][..]).unpack()?;
				match op.as_rule() {
					Rule::and => {
						if slash.get_status() != 0 {
							break
						} else {
							continue
						}
					}
					Rule::or => {
						if slash.get_status() == 0 {
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
			descend(node_stack, slash).blame_no_overwrite(blame)?;
		}
	}
	Ok(())
}

pub fn exec_builtin(cmd: Pair<Rule>, name: &str, slash: &mut Slash) -> SlashResult<()> {
	let blame = cmd.clone();
	match name {
		"test" | "[" => {
			let mut argv = helper::prepare_argv(cmd,slash)?;
			argv.pop_front(); // Ignore the command name
			let result = builtin::test::test(&mut argv, slash).blame(blame)?;
			if result {
				slash.set_code(0);
				return Ok(())
			} else {
				slash.set_code(1);
				return Ok(())
			}
		}
		"string" | "float" | "int" | "arr" | "bool" => builtin::assign::execute(cmd, slash)?,
		"exec" => builtin::exec::run_exec(cmd, slash)?,
		"fg" => builtin::job::continue_job(cmd, slash, true)?,
		"bg" => builtin::job::continue_job(cmd, slash, false)?,
		"jobs" => builtin::job::jobs(cmd, slash)?,
		"return" => builtin::control::func_return(cmd, slash)?,
		"break" => builtin::control::loop_break(cmd, slash)?,
		"continue" => builtin::control::loop_continue()?,
		"pushd" => builtin::dir_stack::pushd(cmd, slash)?,
		"source" => builtin::source::execute(cmd, slash)?,
		"popd" => builtin::dir_stack::popd(cmd, slash)?,
		"setopt" => builtin::opts::setopt(cmd, slash)?,
		"getopt" => builtin::opts::getopt(cmd, slash)?,
		"exit" => builtin::control::exit(cmd, slash)?,
		"cd" => builtin::cd::execute(cmd, slash)?,
		"alias" => builtin::alias::execute(cmd, slash)?,
		"unalias" => builtin::alias::unalias(cmd, slash)?,
		"pwd" => builtin::pwd::execute(cmd, slash)?,
		"export" => builtin::export::execute(cmd, slash)?,
		"echo" => builtin::echo::execute(cmd, slash)?,
		"builtin" => builtin::cmd_override::execute(cmd, slash, true)?,
		"command" => builtin::cmd_override::execute(cmd, slash, false)?,
		_ => return Err(High(SlashErrHigh::exec_err(format!("Have not implemented support for builtin `{}` yet",name),blame)))
	};
	slash.set_code(0);
	Ok(())
}
