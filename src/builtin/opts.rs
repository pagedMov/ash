use crate::pest_ext::ARG_RULES;
use crate::prelude::*;

use crate::utils::SmartFD;
use crate::{error::{LashErr::*, LashErrHigh}, helper::{self}, shellenv::Lash, LashResult};

pub fn setopt<'a>(setopt_call: Pair<'a,Rule>, lash: &mut Lash) -> LashResult<()> {
	let mut argv = setopt_call.filter(&ARG_RULES[..]);
	while let Some(arg) = argv.pop_front() {
		if arg.as_rule() == Rule::arg_assign {
			let opt_path = arg.scry(Rule::var_ident).unpack()?.as_str();
			let val = match arg.scry(Rule::word) {
				Some(pair) => helper::try_expansion(lash,pair)?,
				None => String::new()
			};
			lash.meta_mut().set_shopt(opt_path, &val)?;
		} else {
			let msg = "Expected an assignment in setopt args";
			return Err(High(LashErrHigh::syntax_err(msg, arg)))
		}
	}
	Ok(())
}

pub fn getopt<'a>(getopt_call: Pair<'a,Rule>, lash: &mut Lash) -> LashResult<()> {
	let mut argv = getopt_call.filter(&ARG_RULES[..]);
	let redirs = helper::prepare_redirs(getopt_call)?;
	lash.consume_redirs(redirs)?;
	let mut stdout = SmartFD::new(1)?;
	while let Some(arg) = argv.pop_front() {
		let opt_name = arg.as_str();
		let opt_val = lash.meta().get_shopt(opt_name)?;
		writeln!(stdout,"{}",opt_val)?;
	}
	Ok(())
}

#[cfg(test)]
mod tests {
	use crate::execute;

use super::*;

	#[test]
	fn test_setopt() {
		let mut lash = Lash::new();
		let input = "setopt prompt.custom.foo=\"bar\"";

		execute::dispatch::exec_input(input.to_string(), &mut lash).unwrap();
		let opt = lash.meta().get_shopt("prompt.custom.foo").unwrap();

		assert_eq!(opt,"bar".to_string())
	}
}
