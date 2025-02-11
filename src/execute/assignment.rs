use crate::{error::LashErr::*, helper, prelude::*, shellenv::LashVal};

use super::dispatch;

pub fn exec_assignment<'a>(ass: Pair<'a,Rule>, lash: &mut Lash) -> LashResult<()> {
	let cmd = ass.scry(Rule::cmd_list);
	let blame = ass.clone();
	let var_name: String = ass.scry(Rule::var_ident).unpack()?.as_str().to_string();
	let assign_type = ass.scry(&[
		Rule::increment,
		Rule::decrement,
		Rule::plus_assign,
		Rule::minus_assign,
		Rule::std_assign][..]).unpack()?;
	let val = ass.scry(Rule::word).map(|pr| helper::try_expansion(lash,pr).unwrap_or_default()).unwrap_or_default();
	let vars = lash.vars_mut();
	match assign_type.as_rule() {
		Rule::increment => {
			if let Some(val) = vars.get_var_mut(&var_name) {
				helper::proc_res(val.increment(), blame)?;
			}
		}
		Rule::decrement => {
			if let Some(val) = vars.get_var_mut(&var_name) {
				helper::proc_res(val.decrement(), blame)?;
			}
		}
		Rule::plus_assign => {
			let rhs = LashVal::parse(ass.scry(Rule::word).unpack()?.as_str())?;
			let var_val = vars.get_var(&var_name);
			if var_val.clone().is_some_and(|val| &val.fmt_type() == "int") {
				if let LashVal::Int(lhs) = var_val.unwrap() {
					if let LashVal::Int(rhs) = rhs {
						let value = LashVal::Int(lhs + rhs);
						vars.set_var(&var_name, LashVal::Int(lhs + rhs));
					} else {
						let msg = "The right side of this assignment is invalid; expected an integer";
						return Err(High(LashErrHigh::syntax_err(msg, blame)))
					}
				} else {
					let msg = "The left side of this assignment is invalid; expected an integer";
					return Err(High(LashErrHigh::syntax_err(msg, blame)))
				}
			} else {
				let msg = "The variable in this assignment is unset";
				return Err(High(LashErrHigh::syntax_err(msg, blame)))
			}
		}
		Rule::minus_assign => {
			let rhs = LashVal::parse(ass.scry(Rule::word).unpack()?.as_str())?;
			let var_val = vars.get_var(&var_name);
			if var_val.clone().is_some_and(|val| &val.fmt_type() == "int") {
				if let LashVal::Int(lhs) = var_val.unwrap() {
					if let LashVal::Int(rhs) = rhs {
						vars.set_var(&var_name, LashVal::Int(lhs - rhs));
					} else {
						let msg = "The right side of this assignment is invalid; expected an integer";
						return Err(High(LashErrHigh::syntax_err(msg, blame)))
					}
				} else {
					let msg = "The left side of this assignment is invalid; expected an integer";
					return Err(High(LashErrHigh::syntax_err(msg, blame)))
				}
			} else {
				let msg = "The variable in this assignment is unset";
				return Err(High(LashErrHigh::syntax_err(msg, blame)))
			}
		}
		Rule::std_assign => {
			let val = LashVal::parse(ass.scry(Rule::word).unpack()?.as_str())?;
			vars.set_var(&var_name, val.clone());
		}
		Rule::cmd_list => {}
		_ => unreachable!()
	}

	// TODO: cleanup this logic, it currently doesn't isolate the variable setting to the execution context
	if let Some(cmd) = cmd {
		// If there are commands attached, export the variables, then execute, then restore environment state
		let mut lash_clone = lash.clone();
		lash_clone.vars_mut().export_var(&var_name, &val.to_string());
		dispatch::exec_input(cmd.as_str().to_string(), &mut lash_clone)?;
	}
	lash.set_code(0);
	Ok(())
}
