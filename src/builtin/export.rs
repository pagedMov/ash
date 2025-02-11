use crate::prelude::*;

use crate::{helper, pest_ext::ARG_RULES, shellenv::Lash, LashResult};

pub fn execute<'a>(export_call: Pair<'a,Rule>, lash: &mut Lash) -> LashResult<()> {
	let mut argv = export_call.filter(&ARG_RULES[..]);
	while let Some(arg) = argv.pop_front() {
		match arg.as_rule() {
			Rule::cmd_name => continue,
			Rule::arg_assign => {
				let mut assign_inner = arg.into_inner();
				let var_name = assign_inner.next().unpack()?.as_str();
				let val = match assign_inner.next() {
					Some(pair) => helper::try_expansion(lash,pair)?,
					None => String::new()
				};
				lash.vars_mut().export_var(var_name, &val);
			}
			_ => {
				let msg = String::from("Expected an assignment in export args, got this");
				return Err(High(LashErrHigh::syntax_err(msg, arg)))
			}
		}
	}

	Ok(())
}

#[cfg(test)]
mod tests {
	use crate::execute;

use super::*;

	#[test]
	fn test_export() {
		let mut lash = Lash::new();
		let input = "export VAR=\"foo bar\"";

		execute::dispatch::exec_input(input.to_string(), &mut lash).unwrap();

		let external_var = env::var("VAR").unwrap();
		let internal_var = lash.borrow_vars().get_evar("VAR").unwrap();
		assert_eq!(external_var, "foo bar".to_string());
		assert_eq!(internal_var, "foo bar".to_string());
	}
}
