use crate::prelude::*;

pub fn exec_loop_cmd<'a>(cmd: Pair<'a,Rule>, slash: &mut Slash) -> SlashResult<()> {
	let loop_kind = cmd.scry(Rule::loop_kind).unpack()?.as_str();
	let loop_cond = cmd.scry(Rule::loop_cond).unpack()?.as_str().to_string();
	let loop_body = cmd.scry(Rule::loop_body).unpack()?.as_str().to_string();

	loop {
		slash.exec_as_cond(&loop_cond)?;
		let is_success = slash.get_status() == 0;
		match loop_kind {
			"while" => {
				if !is_success {
					break
				}
			}
			"until" => {
				if is_success {
					break
				}
			}
			_ => unreachable!()
		}
		let result = slash.exec_as_body(&loop_body);
		match result {
			Err(High(err)) => {
				match err.get_err() {
					SlashErrLow::LoopBreak(code) => {
						slash.set_code(*code);
						return Ok(())
					}
					SlashErrLow::LoopCont => continue,
					_ => return Err(High(err))
				}
			}
			Err(e) => return Err(e),
			Ok(_) => continue,
		}
	}
	slash.set_code(0);
	Ok(())
}
