use crate::{prelude::*, shellenv::SlashVal};

pub fn exec_for_cmd<'a>(cmd: Pair<'a,Rule>,slash: &mut Slash) -> SlashResult<()> {
	let mut saved_vars = HashMap::new();
	let loop_body = cmd.scry(Rule::loop_body).unpack()?.as_str();
	let loop_vars = cmd.scry(Rule::for_vars)
		.unpack()?
		.into_inner()
		.into_iter()
		.map(|var| var.as_str())
		.collect::<Vec<&str>>();
	let loop_arr = cmd.scry(Rule::for_arr)
		.unpack()?
		.into_inner()
		.map(|elem| SlashVal::parse(elem.as_str()).unwrap())
		.collect::<Vec<SlashVal>>();

	let vars_len = loop_vars.len();
	for var in &loop_vars {
		let existing_val = slash.vars().get_var(var).unwrap_or_default();
		saved_vars.insert(var,existing_val);
	}

	for (i,element) in loop_arr.iter().enumerate() {
		let var_index = i % vars_len;
		slash.vars_mut().set_var(loop_vars[var_index], element.clone());
		slash.exec_as_body(loop_body)?;
	}
	for var in &loop_vars {
		let saved_val = saved_vars.remove(var).unwrap_or_default();
		slash.vars_mut().set_var(var, saved_val);
	}
	slash.set_code(0);
	Ok(())
}
