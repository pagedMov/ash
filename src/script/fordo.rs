use crate::{prelude::*, shellenv::LashVal};

pub fn exec_for_cmd<'a>(cmd: Pair<'a,Rule>,lash: &mut Lash) -> LashResult<()> {
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
		.map(|elem| LashVal::parse(elem.as_str()).unwrap())
		.collect::<Vec<LashVal>>();

	let vars_len = loop_vars.len();
	for var in &loop_vars {
		let existing_val = lash.vars().get_var(var).unwrap_or_default();
		saved_vars.insert(var,existing_val);
	}

	for (i,element) in loop_arr.iter().enumerate() {
		let var_index = i % vars_len;
		lash.vars_mut().set_var(loop_vars[var_index], element.clone());
		lash.exec_as_body(loop_body)?;
	}
	for var in &loop_vars {
		let saved_val = saved_vars.remove(var).unwrap_or_default();
		lash.vars_mut().set_var(var, saved_val);
	}
	lash.set_code(0);
	Ok(())
}
