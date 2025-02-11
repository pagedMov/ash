use crate::prelude::*;

pub fn exec_match_cmd<'a>(cmd: Pair<'a,Rule>, lash: &mut Lash) -> LashResult<()> {
	let mut inner = cmd.into_inner();
	let match_pat = inner.next().unpack()?;
	let mut arms = VecDeque::new();

	while let Some(arm) = inner.next() {
		arms.push_back(arm);
	}

	while let Some(arm) = arms.pop_front() {
		let mut inner = arm.into_inner();
		let arm_pat = inner.next().unpack()?;
		let arm_body = inner.next().unpack()?.as_str();

		if arm_pat.as_str().trim() == match_pat.as_str().trim() {
			lash.exec_as_body(arm_body)?;
			break
		}
	}
	lash.set_code(0);
	Ok(())
}
