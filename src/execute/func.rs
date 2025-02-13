use crate::{helper, prelude::*};

use super::dispatch;

pub fn exec_func_def<'a>(func_def: Pair<'a,Rule>, lash: &mut Lash) -> LashResult<()> {
	let blame = func_def.clone();
	let func_name = func_def.scry(Rule::func_name).unpack()?;
	let body = func_def.scry(Rule::brace_grp).unpack()?;
	helper::write_func(
		lash,
		func_name.as_str().trim_end_matches("()"),
		body.as_str().trim_matches(['{','}']).trim()
	)?;
	lash.set_code(0);
	Ok(())
}

pub fn exec_func(cmd: Pair<Rule>,lash: &mut Lash) -> LashResult<()> {
	let blame = cmd.clone();
	let mut argv = helper::prepare_argv(cmd,lash)?;
	let func_name = argv.pop_front().unwrap();
	let body = lash.logic().get_func(&func_name).unwrap();
	let mut var_table = lash.vars().clone();
	let snapshot = lash.clone();

	var_table.reset_params();
	for arg in argv {
		var_table.pos_param_pushback(&arg);
	}
	*lash.vars_mut() = var_table;
	let result = dispatch::exec_input(body, lash);
	*lash = snapshot;

	let code = helper::extract_return(&result);
	if let Ok(code) = code {
		lash.set_code(code);
		Ok(())
	} else {
		result
	}
}
