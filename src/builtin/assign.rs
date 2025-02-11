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
				if let Some(val) = arg.scry(Rule::word) {
					let rule = val.as_rule();
					let val = helper::try_expansion(lash,val)?;
					let lash_val = match cmd_name.as_str() {
						"string" => {
							LashVal::String(val)
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
								LashVal::parse(val.as_str())?
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
