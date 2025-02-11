use crate::pest_ext::ARG_RULES;
use crate::prelude::*;

use crate::{error::{LashErr::*, LashErrHigh}, helper::{self}, shellenv::{HashFloat, Lash, LashVal}, LashResult};

pub fn execute<'a>(assign: Pair<'a,Rule>, lash: &mut Lash) -> LashResult<()> {
	let blame = assign.clone();
	let mut argv = assign.filter(&ARG_RULES[..]);
	let cmd_name = assign.scry(Rule::cmd_name).unpack()?;
	while let Some(arg) = argv.pop_front() {
		match arg.as_rule() {
			Rule::arg_assign => {
				let var_name = arg.scry(Rule::var_ident).unpack()?;
				if let Some(val) = arg.scry(&[Rule::word,Rule::array][..]) {
					let rule = val.as_rule();
					let val = helper::try_expansion(lash,val)?;
					let lash_val = match cmd_name.as_str() {
						"string" => {
							LashVal::String(val.trim_quotes().to_string())
						}
						"int" => {
							let lash_int = val.as_str().parse::<i32>();
							if lash_int.is_err() {
								let msg = format!("Expected an integer in `int` assignment");
								return Err(High(LashErrHigh::syntax_err(msg, blame)))
							}
							LashVal::Int(lash_int.unwrap())
						}
						"bool" => {
							let lash_bool = val.as_str().parse::<bool>();
							if lash_bool.is_err() {
								let msg = format!("Expected a boolean in `bool` assignment");
								return Err(High(LashErrHigh::syntax_err(msg, blame)))
							}
							LashVal::Bool(lash_bool.unwrap())
						}
						"float" => {
							let lash_float = val.as_str().parse::<f64>();
							if lash_float.is_err() {
								let msg = format!("Expected a floating point value in `float` assignment");
								return Err(High(LashErrHigh::syntax_err(msg, blame)))
							}
							LashVal::Float(HashFloat(lash_float.unwrap()))
						}
						"arr" => {
							if let Rule::array = rule {
								let val = LashVal::parse(val.as_str())?;
								val
							} else {
								let msg = format!("Expected an array in `array` assignment");
								return Err(High(LashErrHigh::syntax_err(msg, blame)))
							}
						}
						_ => unimplemented!("Have not yet implemented var type builtin '{}'",cmd_name.as_str())
					};
					lash.vars_mut().set_var(var_name.as_str(), lash_val);
				} else {
					lash.vars_mut().unset_var(var_name.as_str());
				}
			}
			Rule::redir => { /* Do nothing */ }
			_ => {
				let msg = format!("Expected assignment in '{}' args, found this: '{}'",cmd_name.as_str(),arg.as_str());
				return Err(High(LashErrHigh::syntax_err(msg, blame)))
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
		let mut lash = Lash::new();
		let input = "int var=5";

		execute::dispatch::exec_input(input.to_string(), &mut lash).unwrap();
		assert_eq!(lash.borrow_vars().get_var("var"), Some(LashVal::Int(5)))
	}
	#[test]
	fn test_assign_string() {
		let mut lash = Lash::new();
		let input = "string var=\"foo bar\"";

		execute::dispatch::exec_input(input.to_string(), &mut lash).unwrap();
		assert_eq!(lash.borrow_vars().get_var("var"), Some(LashVal::String("foo bar".to_string())))
	}
	#[test]
	fn test_assign_float() {
		let mut lash = Lash::new();
		let input = "float pi=3.14";

		execute::dispatch::exec_input(input.to_string(), &mut lash).unwrap();
		assert_eq!(lash.borrow_vars().get_var("pi"), Some(LashVal::Float(HashFloat(3.14))))
	}
	#[test]
	fn test_assign_bool() {
		let mut lash = Lash::new();
		let input = "bool var=true";

		execute::dispatch::exec_input(input.to_string(), &mut lash).unwrap();
		assert_eq!(lash.borrow_vars().get_var("var"), Some(LashVal::Bool(true)));

		let mut lash = Lash::new();
		let input = "bool var=false";

		execute::dispatch::exec_input(input.to_string(), &mut lash).unwrap();
		assert_eq!(lash.borrow_vars().get_var("var"), Some(LashVal::Bool(false)))
	}
	#[test]
	fn test_assign_array() {
		let mut lash = Lash::new();
		let input = "arr list=[1, \"foo\", 3.14, true, [1,2,3]]";

		execute::dispatch::exec_input(input.to_string(), &mut lash).unwrap();
		let array = lash.borrow_vars().get_var("list").unwrap();

		if let LashVal::Array(mut list) = array {
			assert_eq!(list.remove(4), LashVal::Array(vec![LashVal::Int(1),LashVal::Int(2),LashVal::Int(3)]));
			assert_eq!(list.remove(3), LashVal::Bool(true));
			assert_eq!(list.remove(2), LashVal::Float(HashFloat(3.14)));
			assert_eq!(list.remove(1), LashVal::String("foo".into()));
			assert_eq!(list.remove(0), LashVal::Int(1));
		} else { panic!() }
	}
}
