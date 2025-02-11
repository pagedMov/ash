use crate::{prelude::*, shellenv::EnvFlags};

pub fn exec_if_cmd<'a>(cmd: Pair<'a,Rule>, lash: &mut Lash) -> LashResult<()> {
	let if_cond = cmd.scry(Rule::if_cond).unpack()?.as_str().to_string();
	let if_body = cmd.scry(Rule::if_body).unpack()?.as_str().to_string();
	let else_block = cmd.scry(Rule::else_block);
	let mut elif_blocks = cmd.into_inner().filter(|pr| pr.as_rule() == Rule::elif_block).collect::<VecDeque<_>>();

	let in_pipe = lash.in_pipe();
	if in_pipe {
		// We are going to temporarily remove this flag here, to make sure that cond/body executions fork the process
		// If we don't do this, the program will exit after executing the first condition
		lash.meta_mut().mod_flags(|f| *f &= !EnvFlags::IN_SUB_PROC);
	}

	lash.exec_as_cond(&if_cond)?;
	if lash.get_status() == 0 {
		lash.exec_as_body(&if_body)?;
		return Ok(())
	}

	while let Some(elif_block) = elif_blocks.pop_front() {
		let inner = elif_block.into_inner().next().unpack()?;
		let elif_cond = inner.scry(Rule::if_cond).unpack()?.as_str().to_string();
		let elif_body = inner.scry(Rule::if_body).unpack()?.as_str().to_string();

		lash.exec_as_cond(&elif_cond)?;
		if lash.get_status() == 0 {
			lash.exec_as_body(&elif_body)?;
			return Ok(())
		}
	}

	if let Some(else_block) = else_block {
		let else_body = else_block.step(1).unpack()?.as_str().to_string();
		lash.exec_as_body(&else_body)?;
	}

	lash.set_code(0);
	Ok(())
}
