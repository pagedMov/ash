use crate::prelude::*;

use crate::{error::{SlashErr::*, SlashErrHigh}, helper::{self}, shellenv::Slash, SlashResult};

pub fn execute<'a>(src_call: Pair<'a,Rule>, slash: &mut Slash) -> SlashResult<()> {
	let blame = src_call.clone();
	let mut argv = helper::prepare_argv(src_call,slash)?;
	argv.pop_front();
	while let Some(arg) = argv.pop_front() {
		let path = PathBuf::from(arg.as_str());
		if path.exists() && path.is_file() {
			slash.source_file(arg.as_str())?;
		} else {
			let msg = String::from("source failed: File not found");
			return Err(High(SlashErrHigh::exec_err(msg, blame)))
		}
	}
	Ok(())
}
