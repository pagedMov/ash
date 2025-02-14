use crate::{expand, prelude::*};

struct Expansion<'a> {
	expanded: String,
	span: Span<'a>
}

struct ExpansionIR<'a> {
	input: String,
	expansions: Vec<Expansion<'a>>
}

impl<'a> ExpansionIR<'a> {
	pub fn new(input: &str) -> Self {
		Self { input: input.to_string(), expansions: vec![] }
	}
	pub fn push_expansion(&mut self, exp: Expansion<'a>) {
		self.expansions.push(exp)
	}
	pub fn sort_expansions(&mut self) {
		self.expansions.sort_by(|a,b| a.span.start().cmp(&b.span.start()));
	}
	pub fn expand(mut self) -> String {
		self.sort_expansions();
		let mut result = self.input.clone();
		while let Some(exp) = self.expansions.pop() {
			result.replace_span(exp.span, &exp.expanded);
		}
		result
	}
}

pub fn expand_list<'a>(list: Pair<'a,Rule>,slash: &mut Slash) -> SlashResult<String> {
	let mut buffer = list.get_input().to_string();
	let list_body = list.as_str().to_string();
	let mut result = String::new();
	let inner = list.into_inner().rev();
	// We check to see if we are in a command chain here
	// If we are, we return the whole input instead of just a slice
	let slice = inner.clone().count() == 1;

	for cmd in inner {
		if matches!(cmd.as_rule(), Rule::shell_cmd) && !cmd.scry(Rule::assignment).is_some() {
			result = list_body.clone();
			continue
		}

		// Get the span of the current cmd_list part
		let span = cmd.as_span();
		result = expand_cmd(cmd,slash)?; // Expand it

		// Reset the buffer with the newly expanded string
		buffer.replace_span(span,&result);

	}
	if slice {
		// Return just the expanded slice
		Ok(result)
	} else {
		// Return the entire expanded buffer
		Ok(buffer)
	}
}

pub fn expand_cmd<'a>(cmd: Pair<'a,Rule>, slash: &mut Slash) -> SlashResult<String> {
	if cmd.as_rule() == Rule::op { return Ok(cmd.as_str().to_string()) }
	let mut buffer = cmd.as_str().to_string();
	// Order matters
	let expand_rules = [
		Rule::var_sub,
		Rule::param_sub,
		Rule::glob_word,
		Rule::dquoted,
		Rule::cmd_sub,
		Rule::arr_index,
		Rule::proc_sub,
		Rule::brace_word,
		Rule::tilde_sub
	];
	buffer = alias_pass(buffer,slash)?;
	for rule in expand_rules {
		// Expand each rule in order
		buffer = rule_pass(rule,buffer,slash)?;
	}
	Ok(buffer)
}

pub fn alias_pass<'a>(buffer: String, slash: &mut Slash) -> SlashResult<String> {
	let mut result = buffer.clone();
	let mut list = SlashParse::parse(Rule::find_expansions, &buffer).unwrap().next().unpack()?.to_vec();
	let logic = slash.logic().clone();

	while let Some(word) = list.pop() {
		if let Some(body) = logic.get_alias(word.as_str()) {
			let span = word.as_span();
			result.replace_span(span, &body);
		}
	}
	Ok(result)
}

pub fn expand_aliases(input: String, depth: usize, mut cached: Vec<String>, slash: &mut Slash) -> SlashResult<String> {
	if depth > 10 {
		return Ok(input)
	}
	let mut result = input.clone();
	let mut alias_pass = SlashParse::parse(Rule::main, &input)?;
	let logic = slash.logic().clone();

	let mut cmd_names = alias_pass.next().unwrap().seek_all(Rule::cmd_name);
	while let Some(cmd_name) = cmd_names.pop_back() {
		if let Some(alias) = logic.get_alias(cmd_name.as_str()) {
			if !cached.contains(&alias) {
				let span = cmd_name.as_span();
				result.replace_span(span,&alias);
				cached.push(alias);
			}
		}
	}
	if result != input {
		return expand_aliases(result, depth + 1, cached, slash)
	} else {
		Ok(result)
	}
}

pub fn rule_pass<'a>(rule: Rule, buffer: String, slash: &mut Slash) -> SlashResult<String> {
	// Need to clone buffer here to detach 'result' from the lifetime of 'list'
	let mut result = buffer.clone();
	let mut list = SlashParse::parse(Rule::find_expansions, &buffer).unwrap().next().unpack()?.to_vec();
	const BREAKER_RULES: &[Rule] = &[
		Rule::assignment, // These rules signal the expansion logic to break up the pair
		Rule::arg_assign, // and push the inner pairs back onto the stack
		Rule::std_assign,
		Rule::plus_assign,
		Rule::minus_assign,
		Rule::increment,
		Rule::decrement
	];

	while let Some(word) = list.pop() {
		if BREAKER_RULES.contains(&word.as_rule()) {
			list.extend(word.to_vec());
			continue
		}
		if word.contains_rules(rule) {
			let span = word.as_span();
			let expanded = match rule {
				Rule::var_sub => {
					slash.vars().get_var(&word.as_str()[1..]).unwrap_or_default().to_string()
				}
				Rule::param_sub => {
					let param = slash.vars().get_param(&word.as_str()[1..]).unwrap_or_default().to_string();
					param
				}
				Rule::dquoted => expand::string::expand_string(word,slash)?,
				Rule::arr_index => expand::index::expand_index(word,slash)?,
				Rule::glob_word => expand::glob::expand_glob(word),
				Rule::brace_word => expand::brace::expand_brace(word),
				Rule::cmd_sub => expand::cmdsub::expand_cmd_sub(word,slash)?,
				Rule::proc_sub => expand::cmdsub::expand_proc_sub(word),
				Rule::tilde_sub => expand::misc::expand_tilde(word)?,
				_ => unreachable!()
			};
			result.replace_span(span, &expanded);
		}
	}

	Ok(result)
}

pub fn rule_queue() -> Vec<Rule> {
	vec![
		Rule::cmd_sub,
		Rule::param_sub,
		Rule::var_sub,
		Rule::dquoted
	]
}

pub fn expand_word<'a>(pair: Pair<'a,Rule>, slash: &mut Slash) -> SlashResult<String> {
	let word = pair.as_str();
	let mut rule_queue = rule_queue();
	let expansions = match SlashParse::parse(Rule::expand_word_loud, word) {
		Ok(mut parsed) => parsed.next().unwrap(),
		Err(_) => return Ok(word.to_string())
	};
	let mut exp_ir = ExpansionIR::new(&word);

	while let Some(rule) = rule_queue.pop() {
		let mut matches = expansions.seek_all(rule);
		while let Some(pair) = matches.pop_front() {
			let span = pair.as_span();
			let expanded = match rule {
				Rule::cmd_sub => expand::cmdsub::expand_cmd_sub(pair,slash)?,
				Rule::param_sub => {
					let param_name = &pair.as_str()[1..];
					let param = slash.vars().get_param(param_name).unwrap_or_default().to_string();
					param
				}
				Rule::var_sub => {
					let var_name = &pair.as_str()[1..];
					let result = slash.vars().get_var(var_name).unwrap_or_default().to_string();
					result
				}
				Rule::dquoted => expand::string::expand_string(pair,slash)?,
				_ => unreachable!()
			};
			let exp = Expansion { expanded, span };
			exp_ir.push_expansion(exp);
		}
	}

	let result = exp_ir.expand();

	Ok(result)
}
