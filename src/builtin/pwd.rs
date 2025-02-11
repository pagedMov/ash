use crate::{prelude::*, utils};

use crate::{error::{LashErr::*, LashErrHigh}, helper::{self}, shellenv::Lash, LashResult};

pub fn execute<'a>(pwd_call: Pair<'a,Rule>, lash: &mut Lash) -> LashResult<()> {
	let blame = pwd_call.clone();
	let redirs = helper::prepare_redirs(pwd_call)?;

	lash.ctx_mut().extend_redirs(redirs);

	let redirs = lash.ctx_mut().take_redirs();
	if !redirs.is_empty() {
		let mut redirs = lash.ctx_mut().consume_redirs();
		redirs.activate()?;
	}

	if let Ok(pwd) = env::var("PWD") {
		let mut stdout = utils::RustFd::new(STDOUT_FILENO)?;
		write!(stdout,"{}",pwd)?;
		Ok(())
	} else {
		let msg = String::from("PWD environment variable is unset");
		Err(High(LashErrHigh::exec_err(msg, blame)))
	}
}
