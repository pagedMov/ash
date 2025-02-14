use crate::{helper, pest_ext::ARG_RULES, prelude::*, utils};

/// Creates a new alias from the given arguments
/// Can create more than one alias at a time
/// Expects the "arg_assign" rule in the inner pairs
/// "arg_assign" appears in argument positions and looks like this: foo=bar
pub fn execute<'a>(alias_call: Pair<'a,Rule>, slash: &mut Slash) -> SlashResult<()> {
	let mut stdout = utils::SmartFD::new(STDOUT_FILENO)?;

	let mut args = alias_call.filter(&ARG_RULES[..]);
	let redirs = helper::prepare_redirs(alias_call)?;

	slash.ctx_mut().extend_redirs(redirs);

	let ctx_redirs = slash.ctx_mut().take_redirs();
	if !ctx_redirs.is_empty() {
		let mut redirs = slash.ctx_mut().consume_redirs();
		redirs.activate()?;
	}

	while let Some(arg) = args.pop_front() {
		match arg.as_rule() {
			Rule::arg_assign => {
				let mut assign_inner = arg.into_inner();
				let alias = assign_inner.next().unpack()?.as_str();
				let body = assign_inner.next().map(|pair| pair.as_str()).unwrap_or_default();
				helper::write_alias(slash, alias, &body.trim_quotes())?;
			}
			Rule::word => {
				let alias = slash.logic().get_alias(arg.as_str());
				if let Some(alias) = alias {
					write!(stdout,"{alias}\n")?;
				}
			}
			_ => unreachable!()
		}
	}
	Ok(())
}

/// Removes an alias from the logic table
pub fn unalias<'a>(pair: Pair<'a,Rule>, slash: &mut Slash) -> SlashResult<()> {
	let mut argv = helper::prepare_argv(pair, slash)?;
	argv.pop_front();
	while let Some(arg) = argv.pop_front() {
		if slash.logic().get_alias(&arg).is_some() {
			slash.logic_mut().remove_alias(&arg);
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
		let mut slash = Slash::new();
		let input = "alias foo=\"bar\";";
		execute::dispatch::exec_input(input.to_string(), &mut slash).unwrap();
		assert!(slash.logic().get_alias("foo").is_some_and(|al| &al == "bar"))
	}
}
