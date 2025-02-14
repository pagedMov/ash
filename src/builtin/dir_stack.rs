use crate::prelude::*;

use crate::{helper, shellenv::Slash, SlashResult};

pub fn popd<'a>(popd_call: Pair<'a,Rule>, slash: &mut Slash) -> SlashResult<()> {
	let mut argv = helper::prepare_argv(popd_call.clone(),slash)?;
	argv.pop_front();
	let arg = argv.pop_front();
	let mut path = None;
	if let Some(arg) = arg {
		match arg.as_str().parse::<usize>() {
			Ok(count) => {
				for _ in 0..count {
					let popped = slash.meta_mut().pop_dir();
					if let Some(popped) = popped {
						path = Some(popped);
					}
				}
			}
			Err(_) => {
				let msg = "`popd` expects a positive integer";
				return Err(High(SlashErrHigh::syntax_err(msg, popd_call)))
			}
		}
	} else {
		path = slash.meta_mut().pop_dir();
	}
	match path {
		Some(path) => {
			if path.exists() {
				if path.is_dir() {
					slash.change_dir(&path)?;
				} else {
					return Err(High(SlashErrHigh::syntax_err("Path is not a directory", popd_call)))
				}
			} else {
				return Err(High(SlashErrHigh::syntax_err("Path does not exist", popd_call)))
			}
		}
		None => {
			let msg = "`popd` called with an empty directory stack";
			return Err(High(SlashErrHigh::exec_err(msg, popd_call)))
		}
	}
	Ok(())
}

pub fn pushd<'a>(pushd_call: Pair<'a,Rule>, slash: &mut Slash) -> SlashResult<()> {
	let blame = pushd_call.clone();
	let mut argv = helper::prepare_argv(pushd_call,slash)?;
	argv.pop_front();
	match argv.pop_front() {
		Some(arg) => {
			let path = Path::new(arg.as_str());
			if path.exists() {
				if path.is_dir() {
					slash.change_dir(path)?;
				} else {
					return Err(High(SlashErrHigh::syntax_err("Path is not a directory", blame)))
				}
			} else {
				return Err(High(SlashErrHigh::syntax_err("Path does not exist", blame)))
			}
		}
		None => return Err(High(SlashErrHigh::syntax_err("Expected a directory path in pushd args", blame)))
	}
	Ok(())
}
