use crate::{prelude::*, shellenv::LashVal};

pub fn expand_index(pair: Pair<Rule>,lash: &mut Lash) -> LashResult<String> {
	let mut inner = pair.step(1).unpack()?.into_inner().peekable();
	let arr_name = inner.next().unpack()?;

	let array = lash.borrow_vars().get_var(arr_name.as_str());
	let mut cur_val = match array {
		Some(LashVal::Array(vec)) => Some(LashVal::Array(vec)),
		_ => return Ok(String::new()), // If not an array, return empty
	};

	while let Some(index) = inner.next() {
		let idx = index.as_str().parse::<usize>().map_err(|_| Low(LashErrLow::IndexErr(format!("Index '{}' out of range for array '{}'",index,arr_name.as_str()))))?;
		cur_val = match cur_val {
			Some(LashVal::Array(vec)) => vec.get(idx).cloned(),
			_ => return Ok(String::new()), // Invalid nesting (e.g., indexing a non-array)
		};
	}

	Ok(cur_val.map_or_else(String::new, |val| val.to_string()))
}
