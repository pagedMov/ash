use glob::glob;
use log::{debug, info, trace};
use std::collections::VecDeque;
use crate::interp::token::{TkType,Tk,WordDesc};
use crate::interp::parse::ParseState;
use crate::interp::helper::{self,StrExtension};
use crate::shellenv::ShellEnv;

use super::parse::RshErr;

pub fn expand(mut state: ParseState) -> Result<ParseState,RshErr> {
	let mut buffer = VecDeque::new();
	while let Some(tk) = state.tokens.pop_front() {
		for token in expand_token(state.shellenv, tk) {
			buffer.push_back(token);
		}
	}
	let tokens = std::mem::take(&mut buffer);
	Ok(ParseState {
		input: state.input,
		shellenv: state.shellenv,
		tokens,
		ast: state.ast
	})
}

pub fn check_globs(string: String) -> bool {
	string.chars().any(|t| matches!(t, '?' | '*' | '[' | ']'))
}


pub fn expand_token(shellenv: &ShellEnv, token: Tk) -> VecDeque<Tk> {
	trace!("expand(): Starting expansion with token: {:?}", token);
	let mut working_buffer: VecDeque<Tk> = VecDeque::new();
	let mut product_buffer: VecDeque<Tk> = VecDeque::new();

	//TODO: find some way to clean up this surprisingly functional mess
	// Escaping breaks this right now I think

	working_buffer.push_back(token.clone());
	while let Some(mut token) = working_buffer.pop_front() {
		let is_glob = check_globs(token.text().into());
		let is_brace_expansion = helper::is_brace_expansion(token.text());

		if !is_glob && !is_brace_expansion {
			debug!("expanding var for {}",token.text());
			if token.text().has_unescaped('$') {
				info!("found unescaped dollar in: {}",token.text());
				token.wd.text = expand_var(shellenv, token.text().into());
			}
			if helper::is_brace_expansion(token.text()) || token.text().has_unescaped('$') {
				working_buffer.push_front(token);
			} else {
				product_buffer.push_back(token)
			}

		} else if is_brace_expansion && token.text().has_unescaped('{') {
			trace!("expand(): Beginning brace expansion on {}", token.text());
			// Perform brace expansion
			let expanded = expand_braces(token.text().to_string());
			for mut expanded_token in expanded {
				expanded_token = expand_var(shellenv, expanded_token);
				working_buffer.push_back(
					Tk {
						tk_type: TkType::String,
						wd: WordDesc {
							text: expanded_token,
							span: token.span(),
							flags: token.flags()
						}
					}
				);
			};
		} else if is_glob {
			// Expand glob patterns
			for path in glob(token.text()).unwrap().flatten() {
				working_buffer.push_back(
					Tk {
						tk_type: TkType::String,
						wd: WordDesc {
							text: path.to_str().unwrap().to_string(),
							span: token.span(),
							flags: token.flags()
						}
					}
				);
			}
		} else {
			product_buffer.push_back(token);
		}
	}

	for token in &mut product_buffer {
		let mut text = std::mem::take(&mut token.wd.text);
		text = text.replace('\\',"");
		token.wd.text = text;
	}
	product_buffer
}

pub fn expand_braces(word: String) -> VecDeque<String> {
	let mut results = VecDeque::new();
	let mut buffer = VecDeque::from(vec![word]);

	while let Some(current) = buffer.pop_front() {
		if let Some((prefix, amble, postfix)) = parse_first_brace(&current) {
			let expanded = expand_amble(amble);
			for part in expanded {
				buffer.push_back(format!("{}{}{}", prefix, part, postfix));
			}
		} else {
			// No braces left to expand
			results.push_back(current);
		}
	}

	results
}

fn parse_first_brace(word: &str) -> Option<(String, String, String)> {
	let mut prefix = String::new();
	let mut amble = String::new();
	let mut postfix = String::new();
	let mut char_iter = word.chars().peekable();
	let mut brace_stack = VecDeque::new();

	while let Some(&c) = char_iter.peek() {
		debug!("found character for prefix {}",c);
		if c == '{' {
			brace_stack.push_back(c);
			char_iter.next();
			break;
		} else if c == '\\'{
			prefix.push(char_iter.next()?);
			prefix.push(char_iter.next()?);
		} else {
			prefix.push(char_iter.next()?);
		}
	}

	// Parse amble
	while let Some(c) = char_iter.next() {
			debug!("amble: found char {}", c);
			debug!("Current brace stack: {:?}", brace_stack);

			match c {
					'{' => {
							brace_stack.push_back(c);
							amble.push(c);
					}
					'}' => {
							if brace_stack.pop_back().is_none() {
									debug!("Unmatched closing brace found");
									break; // Or handle the error gracefully
							}
							if brace_stack.is_empty() {
									break;
							} else {
									amble.push(c);
							}
					}
					'\\' => {
							amble.push(c);
							if let Some(ch) = char_iter.next() {
									amble.push(ch);
							} else {
									debug!("Dangling backslash found at end of input");
							}
					}
					_ => amble.push(c),
			}

			debug!("Remaining input: {:?}", char_iter.clone().collect::<String>());
	}

	// Parse postfix
	postfix.extend(char_iter);

	if !brace_stack.is_empty() {
		None // Unmatched braces
	} else if !amble.is_empty() {
		Some((prefix, amble, postfix))
	} else {
		None // No braces found
	}
}

fn expand_amble(amble: String) -> VecDeque<String> {
	if amble.contains("..") {
		// Handle range expansion
		if let Some(expanded) = expand_range(&amble) {
			return expanded;
		}
	} else if amble.contains(',') {
		// Handle comma-separated values
		return amble.split(',').map(|s| s.to_string()).collect::<VecDeque<String>>();
	}

	VecDeque::from(vec![amble]) // If no expansion is needed, return as-is
}

fn expand_range(range: &str) -> Option<VecDeque<String>> {
	let parts: Vec<&str> = range.trim_matches('{').trim_matches('}').split("..").collect();
	if let [start, end] = parts.as_slice() {
		if let (Ok(start_num), Ok(end_num)) = (start.parse::<i32>(), end.parse::<i32>()) {
			// Numeric range
			return Some((start_num..=end_num).map(|n| n.to_string()).collect());
		} else if start.len() == 1 && end.len() == 1 {
			// Alphabetic range
			let start_char = start.chars().next().unwrap();
			let end_char = end.chars().next().unwrap();
			return Some(
				(start_char..=end_char)
				.map(|c| c.to_string())
				.collect(),
			);
		}
	}

	None // Invalid range
}

pub fn expand_var(shellenv: &ShellEnv, string: String) -> String {
	let mut left = String::new();
	let mut right = String::new();
	let mut chars = string.chars().collect::<VecDeque<char>>();
	while let Some(ch) = chars.pop_front() {
		match ch {
			'\\' => {
				left.push(ch);
				left.push(if let Some(ch) = chars.pop_front() { ch } else { break })
			}
			'$' => {
				right.extend(chars.drain(..));
				break
			},
			_ => left.push(ch)
		}
	}
	if right.is_empty() {
		return string.to_string()
	}
	let mut right_chars = right.chars().collect::<VecDeque<char>>();
	let mut var_name = String::new();
	while let Some(ch) = right_chars.pop_front() {
		match ch {
			_ if ch.is_alphanumeric() => {
				var_name.push(ch);
			}
			'_' => {
				var_name.push(ch);
			}
			'{' => continue,
			'}' => break,
			_ => {
				right_chars.push_front(ch);
				break
			}
		}
	}
	let right = right_chars.iter().collect::<String>();

	let value = shellenv.get_variable(&var_name).unwrap_or_default();
	let expanded = format!("{}{}{}",left,value,right);
	if expanded.has_unescaped('$') {
		expand_var(shellenv,expanded)
	} else {
		expanded
	}
}
