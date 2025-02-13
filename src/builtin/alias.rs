use crate::{helper, pest_ext::ARG_RULES, prelude::*, utils};

pub fn execute<'a>(alias_call: Pair<'a,Rule>, lash: &mut Lash) -> LashResult<()> {
	let mut stdout = utils::SmartFD::new(STDOUT_FILENO)?;

	let mut args = alias_call.filter(&ARG_RULES[..]);
	let redirs = helper::prepare_redirs(alias_call)?;

	lash.ctx_mut().extend_redirs(redirs);

	let ctx_redirs = lash.ctx_mut().take_redirs();
	if !ctx_redirs.is_empty() {
		let mut redirs = lash.ctx_mut().consume_redirs();
		redirs.activate()?;
	}

	while let Some(arg) = args.pop_front() {
		match arg.as_rule() {
			Rule::arg_assign => {
				let mut assign_inner = arg.into_inner();
				let alias = assign_inner.next().unpack()?.as_str();
				let body = assign_inner.next().map(|pair| pair.as_str()).unwrap_or_default();
				helper::write_alias(lash, alias, &body.trim_quotes())?;
			}
			Rule::word => {
				let alias = lash.logic().get_alias(arg.as_str());
				if let Some(alias) = alias {
					write!(stdout,"{alias}\n")?;
				}
			}
			_ => unreachable!()
		}
	}
	Ok(())
}

pub fn unalias<'a>(pair: Pair<'a,Rule>, lash: &mut Lash) -> LashResult<()> {
	let mut argv = helper::prepare_argv(pair, lash)?;
	argv.pop_front();
	while let Some(arg) = argv.pop_front() {
		if lash.logic().get_alias(&arg).is_some() {
			lash.logic_mut().remove_alias(&arg);
		}
	}
	Ok(())
}

#[cfg(test)]
mod tests {
	use crate::execute;

use super::*;

	#[test]
	fn test_alias() {
		let mut lash = Lash::new();
		let input = "alias foo=\"bar\";";
		execute::dispatch::exec_input(input.to_string(), &mut lash).unwrap();
		assert!(lash.logic().get_alias("foo").is_some_and(|al| &al == "bar"))
	}
}
