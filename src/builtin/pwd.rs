use crate::{prelude::*, utils};

use crate::{error::{SlashErr::*, SlashErrHigh}, helper::{self}, shellenv::Slash, SlashResult};

pub fn execute<'a>(pwd_call: Pair<'a,Rule>, slash: &mut Slash) -> SlashResult<()> {
	let blame = pwd_call.clone();
	let redirs = helper::prepare_redirs(pwd_call)?;

	slash.ctx_mut().extend_redirs(redirs);

	let redirs = slash.ctx_mut().take_redirs();
	if !redirs.is_empty() {
		let mut redirs = slash.ctx_mut().consume_redirs();
		redirs.activate()?;
	}

	if let Ok(pwd) = env::var("PWD") {
		let mut stdout = utils::SmartFD::new(STDOUT_FILENO)?;
		write!(stdout,"{}",pwd)?;
		Ok(())
	} else {
		let msg = String::from("PWD environment variable is unset");
		Err(High(SlashErrHigh::exec_err(msg, blame)))
	}
}

#[cfg(test)]
mod tests {
	use crate::{execute, expand};

use super::*;

}
