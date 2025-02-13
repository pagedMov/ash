use crate::prelude::*;

pub fn expand_string(pair: Pair<Rule>, lash: &mut Lash) -> LashResult<String> {
	let body = pair.scry(Rule::dquote_body);
	if body.is_none() {
		return Ok(String::new());
	}
	let body = body.unwrap();
	// TODO: this is three unwraps in a row. One is guaranteed to work, but still
	let mut sub_expansions = LashParse::parse(Rule::find_expansions, body.as_str())?.next().unwrap().to_vec();
	let mut result = body.as_str().to_string();
	while let Some(word) = sub_expansions.pop() {
		let span = word.as_span();
		let mut inner = word.clone().into_inner();
		if word.clone().into_inner().count() == 0 {
			continue
		} else {
			let sub_type = inner.next().unpack()?;
			let expanded = match sub_type.as_rule() {
				Rule::var_sub => {
					lash.vars().get_var(&word.as_str()[1..]).unwrap_or_default().to_string()
				}
				Rule::param_sub => {
					let param = lash.vars().get_param(&word.as_str()[1..]).unwrap_or_default().to_string();
					param
				}
				Rule::cmd_sub => {
					let result = super::cmdsub::expand_cmd_sub(word,lash)?;
					result
				}
				Rule::arr_index => super::index::expand_index(word,lash)?,
				Rule::proc_sub => super::cmdsub::expand_proc_sub(word),
				_ => continue
			};
			result.replace_span(span, &expanded);
		}
	}
	result = format!("\"{}\"",result);
	Ok(result)
}
