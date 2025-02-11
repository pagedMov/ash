use crate::prelude::*;

use crate::{error::{LashErr::*, LashErrHigh}, helper::{self}, shellenv::Lash, LashResult};

pub fn execute<'a>(src_call: Pair<'a,Rule>, lash: &mut Lash) -> LashResult<()> {
	let blame = src_call.clone();
	let mut argv = helper::prepare_argv(src_call,lash)?;
	argv.pop_front();
	while let Some(arg) = argv.pop_front() {
		let path = PathBuf::from(arg.as_str());
		if path.exists() && path.is_file() {
			lash.source_file(arg.as_str())?;
		} else {
			let msg = String::from("source failed: File not found");
			return Err(High(LashErrHigh::exec_err(msg, blame)))
		}
	}
	Ok(())
}
