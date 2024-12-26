use glob::glob;
use log::{debug, info, trace};
use std::collections::VecDeque;
use crate::event::ShellError;
use crate::interp::token::{Tk, TkType, WdFlags, WordDesc};
use crate::interp::helper::{self,StrExtension};
use crate::shellenv::ShellEnv;

use super::parse::{NdType, Node, Span};

pub fn check_globs(string: String) -> bool {
	string.chars().any(|t| matches!(t, '?' | '*'))
}

/// Handles the expansion of command arguments
/// Replaces a command node's argv field with the resulting expansions
///
/// # Arguments
///
/// * `shellenv` - The shell environment containing information used for variable expansion
/// # `node` - The AST node being operated on
///
/// # Returns
///
/// A result containing either the produced vector of tokens, or a `ShellError` if `node.get_argv()` fails,
/// or if the method is called with a node that is not a Builtin or a Command.
pub fn expand_arguments(shellenv: &ShellEnv, node: &mut Node) -> Result<Vec<Tk>,ShellError> {
	let argv = node.get_argv()?;
	let mut expand_buffer = Vec::new();
	for arg in &argv {
		let mut expanded = expand_token(shellenv, arg.clone());
		expand_buffer.extend(expanded.drain(..));
	}
	match &node.nd_type {
		NdType::Builtin {..} => {
			node.nd_type = NdType::Builtin { argv: expand_buffer.into() };
			Ok(argv)
		}
		NdType::Command {..}  => {
			node.nd_type = NdType::Command { argv: expand_buffer.into() };
			Ok(argv)
		}
		_ => Err(ShellError::from_internal("Called expand arguments on a non-command node", node.span()))
	}
}

pub fn expand_alias(shellenv: &ShellEnv, mut token: Tk) -> Result<Vec<Tk>, ShellError> {
	let mut expanded = vec![];
	let mut span = Span::from(0,0);
	if let Some(alias) = shellenv.get_alias(token.text()) {
		dbg!(&alias);
		let alias_content = alias.split(' ');
		dbg!(&alias_content);
		for word in alias_content {
			span = Span::from(span.end, span.end + word.len());
			let wd = WordDesc { text: word.into(), span, flags: token.flags() };
			let expanded_token = Tk {
				tk_type: TkType::Ident,
				wd
			};
			expanded.push(expanded_token)
		}
	} else {
		expanded = vec![token];
	}
	Ok(expanded)
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
			if token.text().has_unescaped('$') && !token.wd.contains_flag(WdFlags::SNG_QUOTED) {
				info!("found unescaped dollar in: {}",token.text());
				token.wd.text = expand_var(shellenv, token.text().into());

			} else if !token.wd.contains_flag(WdFlags::IS_ARG) {
				dbg!(&token);
				let alias_tokens = expand_alias(shellenv, token.clone()).unwrap();

				if let Some(alias_token) = alias_tokens.first() {
					dbg!(&alias_tokens);
					if !(alias_token.text() == token.text() && alias_tokens.len() == 1) {
						product_buffer.extend(alias_tokens);
						continue
					}
				}
			}
			if helper::is_brace_expansion(token.text()) || token.text().has_unescaped('$') {
				working_buffer.push_front(token);
			} else {
				product_buffer.push_back(token)
			}

		} else if is_brace_expansion && token.text().has_unescaped('{') && token.tk_type != TkType::String {
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
		} else if shellenv.get_alias(token.text()).is_some() {
			let alias_content = shellenv.get_alias(token.text()).unwrap().split(' ');
			for word in alias_content {
				working_buffer.push_back(
					Tk {
						tk_type: TkType::String,
						wd: WordDesc {
							text: word.into(),
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
