use crate::{helper, prelude::*};

pub fn exit<'a>(pair: Pair<'a,Rule>, slash: &mut Slash) -> SlashResult<()> {
	let mut argv = helper::prepare_argv(pair, slash)?;
	argv.pop_front();
	let code = if let Some(arg) = argv.pop_front() {
		let word = arg.as_str();
		if let Ok(code) = word.parse::<i32>() {
			code
		} else {
			0
		}
	} else {
		0
	};
	Err(Low(SlashErrLow::CleanExit(code)))
}

pub fn func_return<'a>(pair: Pair<'a,Rule>, slash: &mut Slash) -> SlashResult<()> {
	let mut argv = helper::prepare_argv(pair, slash)?;
	argv.pop_front();
	let code = if let Some(arg) = argv.pop_front() {
		let word = arg.as_str();
		if let Ok(code) = word.parse::<i32>() {
			code
		} else {
			0
		}
	} else {
		0
	};
	Err(Low(SlashErrLow::FuncReturn(code)))
}

pub fn loop_break<'a>(pair: Pair<'a,Rule>, slash: &mut Slash) -> SlashResult<()> {
	let mut argv = helper::prepare_argv(pair, slash)?;
	argv.pop_front();
	let code = if let Some(arg) = argv.pop_front() {
		let word = arg.as_str();
		if let Ok(code) = word.parse::<i32>() {
			code
		} else {
			0
		}
	} else {
		0
	};
	Err(Low(SlashErrLow::LoopBreak(code)))
}

pub fn loop_continue<'a>() -> SlashResult<()> {
	Err(Low(SlashErrLow::LoopCont))
}
