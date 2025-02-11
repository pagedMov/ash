use crate::{execute, prelude::*, utils};

pub fn expand_cmd_sub(mut pair: Pair<Rule>,lash: &mut Lash) -> LashResult<String> {
	if pair.as_rule() == Rule::word {
		pair = pair.step(1).unpack()?;
	}
	assert!(pair.as_rule() == Rule::cmd_sub);
	// Get the subshell token
	let body = pair.as_str();
	let body = &body[2..body.len() - 1]; // From '$(this)' to 'this'

	let (mut r_pipe, mut w_pipe) = utils::RustFd::pipe()?;
	let redir = utils::Redir::from_raw(1,w_pipe.as_raw_fd());
	let mut sub_lash = lash.clone();
	let flags = sub_lash.ctx_mut().flags_mut();
	*flags |= utils::ExecFlags::NO_FORK; // Tell the child proc to not fork since it's already in a fork
	sub_lash.ctx_mut().push_redir(redir);

	match unsafe { fork() } {
		Ok(ForkResult::Child) => {
			r_pipe.close()?;
			// Execute the subshell body with the ctx payload
			execute::dispatch::exec_input(body.consume_escapes(), &mut sub_lash)?;
			std::process::exit(1);
		}
		Ok(ForkResult::Parent { child: _ }) => {
			w_pipe.close()?;
		}
		Err(_) => panic!()
	}

	let mut buffer = String::new();
	r_pipe.read_to_string(&mut buffer)?;
	r_pipe.close()?;

	Ok(buffer.trim().to_string())
}

/// Used in tests
pub fn cmd_sub_from_str(input: &str,lash: &mut Lash) -> LashResult<String> {
	// Get the subshell token
	let body = input;

	let (mut r_pipe, mut w_pipe) = utils::RustFd::pipe()?;
	let redir = utils::Redir::from_raw(1,w_pipe.as_raw_fd());
	let mut sub_lash = lash.clone();
	let flags = sub_lash.ctx_mut().flags_mut();
	*flags |= utils::ExecFlags::NO_FORK; // Tell the child proc to not fork since it's already in a fork
	sub_lash.ctx_mut().push_redir(redir);

	match unsafe { fork() } {
		Ok(ForkResult::Child) => {
			r_pipe.close()?;
			// Execute the subshell body with the ctx payload
			execute::dispatch::exec_input(body.consume_escapes(), &mut sub_lash)?;
			std::process::exit(1);
		}
		Ok(ForkResult::Parent { child: _ }) => {
			w_pipe.close()?;
		}
		Err(_) => panic!()
	}

	let mut buffer = String::new();
	r_pipe.read_to_string(&mut buffer)?;
	r_pipe.close()?;

	Ok(buffer.trim().to_string())
}

pub fn expand_proc_sub(pair: Pair<Rule>) -> String {
	todo!()
}
