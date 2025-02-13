use crate::{helper, prelude::*};

pub fn exit<'a>(pair: Pair<'a,Rule>, lash: &mut Lash) -> LashResult<()> {
	let mut argv = helper::prepare_argv(pair, lash)?;
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
	Err(Low(LashErrLow::CleanExit(code)))
}

pub fn func_return<'a>(pair: Pair<'a,Rule>, lash: &mut Lash) -> LashResult<()> {
	let mut argv = helper::prepare_argv(pair, lash)?;
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
	Err(Low(LashErrLow::FuncReturn(code)))
}

pub fn loop_break<'a>(pair: Pair<'a,Rule>, lash: &mut Lash) -> LashResult<()> {
	let mut argv = helper::prepare_argv(pair, lash)?;
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
	Err(Low(LashErrLow::LoopBreak(code)))
}

pub fn loop_continue<'a>() -> LashResult<()> {
	Err(Low(LashErrLow::LoopCont))
}
