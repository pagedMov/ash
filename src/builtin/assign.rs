use crate::pest_ext::ARG_RULES;
use crate::prelude::*;

use crate::{error::{SlashErr::*, SlashErrHigh}, helper::{self}, shellenv::{HashFloat, Slash, SlashVal}, SlashResult};

pub fn execute<'a>(assign: Pair<'a,Rule>, slash: &mut Slash) -> SlashResult<()> {
	let blame = assign.clone();
	let mut argv = assign.filter(&ARG_RULES[..]);
	let cmd_name = assign.scry(Rule::cmd_name).unpack()?;
	while let Some(arg) = argv.pop_front() {
		match arg.as_rule() {
			Rule::arg_assign => {
				let var_name = arg.scry(Rule::var_ident).unpack()?;
				if let Some(val) = arg.scry(&[Rule::word,Rule::array][..]) {
					let rule = val.as_rule();
					let val = helper::try_expansion(slash,val)?;
					let slash_val = match cmd_name.as_str() {
						"string" => {
							SlashVal::String(val.trim_quotes().to_string())
						}
						"int" => {
							let slash_int = val.as_str().parse::<i32>();
							if slash_int.is_err() {
								let msg = format!("Expected an integer in `int` assignment");
								return Err(High(SlashErrHigh::syntax_err(msg, blame)))
							}
							SlashVal::Int(slash_int.unwrap())
						}
						"bool" => {
							let slash_bool = val.as_str().parse::<bool>();
							if slash_bool.is_err() {
								let msg = format!("Expected a boolean in `bool` assignment");
								return Err(High(SlashErrHigh::syntax_err(msg, blame)))
							}
							SlashVal::Bool(slash_bool.unwrap())
						}
						"float" => {
							let slash_float = val.as_str().parse::<f64>();
							if slash_float.is_err() {
								let msg = format!("Expected a floating point value in `float` assignment");
								return Err(High(SlashErrHigh::syntax_err(msg, blame)))
							}
							SlashVal::Float(HashFloat(slash_float.unwrap()))
						}
						"arr" => {
							if let Rule::array = rule {
								let val = SlashVal::parse(val.as_str())?;
								val
							} else {
								let msg = format!("Expected an array in `array` assignment");
								return Err(High(SlashErrHigh::syntax_err(msg, blame)))
							}
						}
						_ => unimplemented!("Have not yet implemented var type builtin '{}'",cmd_name.as_str())
					};
					slash.vars_mut().set_var(var_name.as_str(), slash_val);
				} else {
					slash.vars_mut().unset_var(var_name.as_str());
				}
			}
			Rule::redir => { /* Do nothing */ }
			_ => {
				let msg = format!("Expected assignment in '{}' args, found this: '{}'",cmd_name.as_str(),arg.as_str());
				return Err(High(SlashErrHigh::syntax_err(msg, blame)))
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
	fn test_assign_int() {
		let mut slash = Slash::new();
		let input = "int var=5";

		execute::dispatch::exec_input(input.to_string(), &mut slash).unwrap();
		assert_eq!(slash.vars().get_var("var"), Some(SlashVal::Int(5)))
	}
	#[test]
	fn test_assign_string() {
		let mut slash = Slash::new();
		let input = "string var=\"foo bar\"";

		execute::dispatch::exec_input(input.to_string(), &mut slash).unwrap();
		assert_eq!(slash.vars().get_var("var"), Some(SlashVal::String("foo bar".to_string())))
	}
	#[test]
	fn test_assign_float() {
		let mut slash = Slash::new();
		let input = "float pi=3.14";

		execute::dispatch::exec_input(input.to_string(), &mut slash).unwrap();
		assert_eq!(slash.vars().get_var("pi"), Some(SlashVal::Float(HashFloat(3.14))))
	}
	#[test]
	fn test_assign_bool() {
		let mut slash = Slash::new();
		let input = "bool var=true";

		execute::dispatch::exec_input(input.to_string(), &mut slash).unwrap();
		assert_eq!(slash.vars().get_var("var"), Some(SlashVal::Bool(true)));

		let mut slash = Slash::new();
		let input = "bool var=false";

		execute::dispatch::exec_input(input.to_string(), &mut slash).unwrap();
		assert_eq!(slash.vars().get_var("var"), Some(SlashVal::Bool(false)))
	}
	#[test]
	fn test_assign_array() {
		let mut slash = Slash::new();
		let input = "arr list=[1, \"foo\", 3.14, true, [1,2,3]]";

		execute::dispatch::exec_input(input.to_string(), &mut slash).unwrap();
		let array = slash.vars().get_var("list").unwrap();

		if let SlashVal::Array(mut list) = array {
			assert_eq!(list.remove(4), SlashVal::Array(vec![SlashVal::Int(1),SlashVal::Int(2),SlashVal::Int(3)]));
			assert_eq!(list.remove(3), SlashVal::Bool(true));
			assert_eq!(list.remove(2), SlashVal::Float(HashFloat(3.14)));
			assert_eq!(list.remove(1), SlashVal::String("foo".into()));
			assert_eq!(list.remove(0), SlashVal::Int(1));
		} else { panic!() }
	}
}
