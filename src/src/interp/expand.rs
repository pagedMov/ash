use chrono::Local;
use glob::glob;
use std::collections::{HashMap, VecDeque};
use std::path::PathBuf;
use crate::event::ShError;
use crate::execute::{self, ProcIO, RustFd};
use crate::interp::parse::NdFlags;
use crate::interp::token::{Tk, TkType, WdFlags, WordDesc};
use crate::interp::helper::{self,StrExtension};
use crate::shellenv::{self, read_logic, read_vars, RVal, VarTable, RSH_PATH};
use crate::OxideResult;

use super::parse::{NdType, Node, Span};
use super::token::REGEX;

static GLOB_CHARS: [&str;2] = ["*", "?"];

pub fn check_globs(tk: Tk) -> bool {
	let text = tk.text();
	let flags = tk.flags();

	if !flags.contains(WdFlags::IS_ARG) || flags.contains(WdFlags::SNG_QUOTED) {
		return false; // Skip if not an argument
	}

	// Check for unescaped glob characters

	let has_globs = GLOB_CHARS.iter().any(|&ch| text.has_unescaped(ch));
	let has_brackets = helper::has_valid_delims(text, "[", "]");
	has_globs || has_brackets
}

pub fn expand_shebang(mut body: String) -> OxideResult<String> {
	// If no shebang, use the path to rsh
	// and attach the '--subshell' argument to signal to the rsh subprocess that it is in a subshell context
	if !body.starts_with("#!") {
		//let interpreter = std::env::current_exe().unwrap();
		let interpreter = PathBuf::from(RSH_PATH.clone());
		let mut shebang = "#!".to_string();
		shebang.push_str(interpreter.to_str().unwrap());
		shebang = format!("{} {}", shebang, "--subshell");
		shebang.push('\n');
		shebang.push_str(&body);
		return Ok(shebang);
	}

	// If there is an abbreviated shebang (e.g. `#!python`), find the path to the interpreter using the PATH env var, and expand the command name to the full path (e.g. `#!python` -> `#!/usr/bin/python`)
	if body.starts_with("#!") && !body.lines().next().unwrap_or_default().contains('/') {
		let mut command = String::new();
		let mut body_chars = body.chars().collect::<VecDeque<char>>();
		body_chars.pop_front(); body_chars.pop_front();

		while let Some(ch) = body_chars.pop_front() {
			if matches!(ch, ' ' | '\t' | '\n' | ';') {
				while body_chars.front().is_some_and(|ch| matches!(ch, ' ' | '\t' | '\n' | ';')) {
					body_chars.pop_front();
				}
				body = body_chars.iter().collect::<String>();
				break;
			} else {
				command.push(ch);
			}
		}
		if let Some(path) = helper::which(&command) {
			let path = format!("{}{}{}", "#!", path, '\n');
			return Ok(format!("{}{}", path, body));
		}
	}

	Ok(body)
}

pub fn expand_arguments(node: &mut Node) -> OxideResult<Vec<Tk>> {
	let argv = node.get_argv()?;
	let mut cmd_name = None;
	let mut glob = true;
	let mut expand_buffer = Vec::new();
	for arg in &argv {
		if cmd_name.is_none() {
			cmd_name = Some(arg.text());
			if cmd_name == Some("expr") { // We don't expand globs for the `expr` builtin
				glob = false;
			}
			expand_buffer.push(arg.clone()); // We also don't expand command names
			continue
		}
		let mut expanded = expand_token(arg.clone(),glob)?;
		while let Some(token) = expanded.pop_front() {
			if !token.text().is_empty() {
				// Do not return empty tokens
				expand_buffer.push(token);
			}
		}
	}
	match &node.nd_type {
		NdType::Builtin {..} => {
			node.nd_type = NdType::Builtin { argv: expand_buffer.clone().into() };
			Ok(expand_buffer)
		}
		NdType::Command {..}  => {
			node.nd_type = NdType::Command { argv: expand_buffer.clone().into() };
			Ok(expand_buffer)
		}
		NdType::Subshell { body, argv: _ } => {
			node.nd_type = NdType::Subshell { body: body.to_string(), argv: expand_buffer.clone().into() };
			Ok(expand_buffer)
		}
		_ => Err(ShError::from_internal("Called expand arguments on a non-command node"))
	}
}

pub fn esc_seq(c: char) -> Option<char> {
	//TODO:
	match c {
		'a' => Some('\x07'),
		'n' => Some('\n'),
		't' => Some('\t'),
		'\\' => Some('\\'),
		'"' => Some('"'),
		'\'' => Some('\''),
		_ => panic!()
	}
}

pub fn expand_time(fmt: &str) -> String {
	let right_here_right_now = Local::now();
	right_here_right_now.format(fmt).to_string()
}

pub fn expand_prompt() -> OxideResult<String> {
	// Determine the default color based on the user ID
	let default_color = if read_vars(|vars| vars.get_evar("UID").is_some_and(|uid| uid == "0"))? {
		"31" // Red if uid is 0, aka root user
	} else {
		"32" // Green if anyone else
	};

	// Get the current working directory
	let cwd: String = read_vars(|vars| vars.get_evar("PWD").map_or("".into(), |cwd| cwd).to_string())?;

	// Determine the default path color
	let default_path = if cwd.as_str() == "/" {
		"\\e[36m\\w\\e[0m".to_string()
	} else {
		format!("\\e[1;{}m\\w\\e[1;36m/\\e[0m", default_color)
	};

	// Get the PS1 environment variable or use a default prompt
	let ps1 = read_vars(|vars| vars.get_evar("PS1"))?;
	let use_default = ps1.clone().is_none_or(|ps1| ps1.trim().is_empty());
	let ps1 = if use_default {
		format!("\\n{}\\n\\e[{}mdebug \\$\\e[36m>\\e[0m ", default_path, default_color)
	} else {
		ps1.unwrap()
	};

	// Process the PS1 string to expand escape sequences
	let mut result = String::new();
	let mut chars = ps1.chars().collect::<VecDeque<char>>();

	while let Some(c) = chars.pop_front() {
		match c {
			'\\' => {
				if let Some(esc_c) = chars.pop_front() {
					match esc_c {
						'a' => result.push('\x07'),
						'n' => result.push('\n'),
						'r' => result.push('\r'),
						'\\' => result.push('\\'),
						'\'' => result.push('\''),
						'"' => result.push('"'),
						'd' => result.push_str(expand_time("%a %b %d").as_str()),
						't' => result.push_str(expand_time("%H:%M:%S").as_str()),
						'T' => result.push_str(expand_time("%I:%M:%S").as_str()),
						'A' => result.push_str(expand_time("%H:%M").as_str()),
						'@' => result.push_str(expand_time("%I:%M %p").as_str()),
						_ if esc_c.is_digit(8) => {
							let octal_char = helper::escseq_octal_escape(&mut chars, esc_c)?;
							result.push(octal_char);
						}
						'e' => helper::escseq_ansi_escape(&mut chars, &mut result),
						'[' => helper::escseq_non_printing_sequence(&mut chars, &mut result),
						']' => (), // Do nothing, it's just a marker
						'w' => result.push_str(&helper::escseq_working_directory()?),
						'W' => result.push_str(&helper::escseq_basename_working_directory()?),
						'H' => result.push_str(&helper::escseq_full_hostname()?),
						'h' => result.push_str(&helper::escseq_short_hostname()?),
						's' => result.push_str(&helper::escseq_shell_name()?),
						'u' => result.push_str(&helper::escseq_username()?),
						'$' => result.push(helper::escseq_prompt_symbol()?),
						_ => {
							result.push('\\');
							result.push(esc_c);
						}
					}
				} else {
					result.push('\\');
				}
			}
			_ => result.push(c)
		}
	}
	Ok(result)
}


pub fn expand_alias(alias: &str) -> OxideResult<String> {
	if let Some(alias_content) = read_logic(|log| log.get_alias(alias))? {
		Ok(alias_content)
	} else {
		Err(ShError::from_internal(format!("Did not find an alias for this: {}",alias).as_str()))
	}
}

pub fn check_home_expansion(text: &str) -> bool {
	text.has_unescaped("~") && (
		text.starts_with('~') ||
		text.has_unescaped("~/")
	)
}

pub fn expand_token(token: Tk, expand_glob: bool) -> OxideResult<VecDeque<Tk>> {
	let mut working_buffer: VecDeque<Tk> = VecDeque::new();
	let mut product_buffer: VecDeque<Tk> = VecDeque::new();
	let split_words = token.tk_type != TkType::String;

	//TODO: find some way to clean up this surprisingly functional mess
	// Escaping breaks this right now I think

	working_buffer.push_back(token.clone());
	while let Some(mut token) = working_buffer.pop_front() {
		// If expand_glob is true, then check for globs. Otherwise, is_glob is false
		let is_glob = if expand_glob { check_globs(token.clone()) } else { expand_glob };
		let is_brace_expansion = helper::is_brace_expansion(token.text());
		let is_cmd_sub = helper::has_valid_delims(token.text(), "$(", ")") || token.tk_type == TkType::CommandSub;

		if is_cmd_sub {
			let new_token = expand_cmd_sub(token)?;
			product_buffer.push_back(new_token);
			continue
		}

		let expand_home = check_home_expansion(token.text());
		if expand_home {
			// If this unwrap fails, god help you
			let home = read_vars(|vars| vars.get_evar("HOME").unwrap())?;
			token.wd.text = token.wd.text.replace("~",&home);
		}

		if !matches!(token.tk_type, TkType::Expanded) && !is_glob && !is_brace_expansion {
			if token.text().has_unescaped("$") && !token.wd.flags.intersects(WdFlags::FROM_VAR | WdFlags::SNG_QUOTED) {
				if token.text().has_unescaped("$@") {
					let mut param_tokens = expand_params(token)?;
					while let Some(param) = param_tokens.pop_back() {
						working_buffer.push_front(param);
					}
					continue
				}
				let vars = shellenv::borrow_var_table().unwrap();
				token.wd.text = expand_var(token.text().into(),&vars)?;
			}
			if helper::is_brace_expansion(token.text()) || token.text().has_unescaped("$") {
				working_buffer.push_front(token);
			} else {
				if expand_home {
					// If this unwrap fails, god help you
					let home = read_vars(|vars| vars.get_evar("HOME").unwrap())?;
					token.wd.text = token.wd.text.replace("~",&home);
				}
				product_buffer.push_back(token)
			}

		} else if is_brace_expansion && token.text().has_unescaped("{") && token.tk_type != TkType::String {
			// Perform brace expansion
			let expanded = expand_braces(token.text().to_string())?;
			for expanded_token in expanded {
				product_buffer.push_back(
					Tk {
						tk_type: TkType::Expanded,
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
						tk_type: TkType::Expanded,
						wd: WordDesc {
							text: path.to_str().unwrap().to_string(),
							span: token.span(),
							flags: token.flags()
						}
					}
				);
			}
		} else if let Some(alias_content) = read_logic(|log| log.get_alias(token.text()))? {
			let alias_content = alias_content.split(' ');
			for word in alias_content {
				working_buffer.push_back(
					Tk {
						tk_type: TkType::Expanded,
						wd: WordDesc {
							text: word.into(),
							span: token.span(),
							flags: token.flags()
						}
					}
				);
			}
		} else {
			if expand_home {
				// If this unwrap fails, god help you
				let home = read_vars(|vars| vars.get_evar("HOME").unwrap())?;
				token.wd.text = token.wd.text.replace("~",&home);
			}
			product_buffer.push_back(token);
		}
	}
	if split_words {
		helper::split_tokens(&mut product_buffer);
	} else {
		helper::combine_tokens(&mut product_buffer);
	}
	Ok(product_buffer)
}


pub fn clean_escape_chars(token_buffer: &mut VecDeque<Tk>) {
	for token in token_buffer {
		let mut text = std::mem::take(&mut token.wd.text);
		text = text.replace('\\',"");
		token.wd.text = text;
	}
}

pub fn expand_cmd_sub(token: Tk) -> OxideResult<Tk> {
	let new_token;
	if let TkType::CommandSub = token.tk_type {
		let body = token.text().to_string();
		let node = Node {
			command: None,
			nd_type: NdType::Subshell { body, argv: VecDeque::new() },
			flags: NdFlags::VALID_OPERAND | NdFlags::IN_CMD_SUB,
			redirs: VecDeque::new(),
			span: token.span(),
		};
		let (mut r_pipe, w_pipe) = RustFd::pipe()?;
		let io = ProcIO::from(None, Some(w_pipe.mk_shared()), None);
		execute::handle_subshell(node, io)?;
		let buffer = r_pipe.read()?;
		new_token = Tk {
			tk_type: TkType::String,
			wd: WordDesc {
				text: buffer.trim().to_string(),
				span: token.span(),
				flags: token.flags(),
			},
		};
		r_pipe.close()?;
	} else if let TkType::String = token.tk_type {
		let mut text = token.text().to_string();
		let mut result = String::new();

		while let Some((left, body, right)) = text.split_twice("$(", ")") {
			// Construct the node for the subshell body
			let node = Node {
				command: None,
				nd_type: NdType::Subshell { body: body.to_string(), argv: VecDeque::new() },
				flags: NdFlags::VALID_OPERAND | NdFlags::IN_CMD_SUB,
				redirs: VecDeque::new(),
				span: token.span(),
			};

			// Create the pipe for subshell communication
			let (mut r_pipe, w_pipe) = RustFd::pipe()?;
			let io = ProcIO::from(None, Some(w_pipe.mk_shared()), None);
			execute::handle_subshell(node, io)?;

			// Read the result of the subshell
			let buffer = r_pipe.read()?;
			r_pipe.close()?;

			// Append the resolved text to the result
			result.push_str(&left);
			result.push_str(buffer.trim());

			// Update the remaining text
			text = right.to_string();
		}

		// Append any remaining text after the last substitution
		result.push_str(&text);

		// Create the new token with the fully expanded string
		new_token = Tk {
			tk_type: TkType::String,
			wd: WordDesc {
				text: result,
				span: token.span(),
				flags: token.flags(),
			},
		};
	} else {
		return Err(ShError::from_internal(format!("Called expand_cmd_sub() on a non-commandsub token: {:?}", token.tk_type).as_str()))
	}
	Ok(new_token)
}

pub fn build_word(words: &[&str], buffer: &mut String) {
	for word in words {
		buffer.push_str(word);
	}
}

fn precompute_cartesian(left: &[String], right: &[String]) -> VecDeque<String> {
	use rayon::prelude::*;
	left.par_iter() // Parallelize the outer iterator
		.flat_map(|l| {
			right.par_iter()
				.map(move |r| format!("{}{}", l, r)) // Compute Cartesian product
		})
	.collect::<Vec<_>>() // Collect results into a Vec
		.into() // Convert Vec to VecDeque
}

fn split_braces(word: &str) -> Option<(&str, &str, &str)> {
	if let Some(start) = word.find('{') {
		if let Some(end) = word[start..].find('}') {
			let preamble = &word[..start];
			let amble = &word[start + 1..start + end];
			let postamble = &word[start + end + 1..];
			return Some((preamble, amble, postamble));
		}
	}
	None
}

pub fn expand_braces(word: String) -> OxideResult<VecDeque<String>> {
	let mut result = VecDeque::new(); // Final results
	let mut working_buffer = String::new(); // Reusable buffer
	let mut product_stack = VecDeque::new(); // First pass results
	let mut work_stack = VecDeque::from([word]); // Work queue
	let vars = shellenv::borrow_var_table().unwrap();

	while let Some(current) = work_stack.pop_front() {
		if let Some((preamble,amble,postamble)) = split_braces(&current) {
			if let Some((left, right)) = amble.split_once("}{") {
				let left_parts = expand_amble(left);
				let right_parts = expand_amble(right);

				let precomputed_products = precompute_cartesian(&left_parts, &right_parts);
				for product in precomputed_products {
					build_word(&[preamble, &product, postamble], &mut working_buffer);
					let result = std::mem::take(&mut working_buffer);
					work_stack.push_back(result);
				}
			} else {
				let parts = expand_amble(amble);
				for part in parts {
					build_word(&[preamble, part.as_str(), postamble], &mut working_buffer);
					let result = std::mem::take(&mut working_buffer);
					work_stack.push_back(result);
				}
			}
		} else {
			product_stack.push_back(current); // Base case
		}
	}
	while let Some(product) = product_stack.pop_front() {
		let expanded = expand_var(product,&vars)?;
		result.push_back(expanded);
	}
	Ok(result)
}

pub fn expand_amble(amble: &str) -> Vec<String> {
	let mut result = vec![];
	if amble.contains("..") && amble.len() >= 4 {
		let num_range = amble.chars().next().is_some_and(|ch| ch.is_ascii_digit())
			&& amble.chars().last().is_some_and(|ch| ch.is_ascii_digit());

		let lower_alpha_range = amble.chars().next().is_some_and(|ch| ch.is_ascii_lowercase())
			&& amble.chars().last().is_some_and(|ch| ch.is_ascii_lowercase())
			&& amble.chars().next() <= amble.chars().last(); // Ensure valid range

		let upper_alpha_range = amble.chars().next().is_some_and(|ch| ch.is_ascii_uppercase())
			&& amble.chars().last().is_some_and(|ch| ch.is_ascii_uppercase())
			&& amble.chars().next() <= amble.chars().last(); // Ensure valid range

		if lower_alpha_range || upper_alpha_range {
			let left = amble.chars().next().unwrap();
			let right = amble.chars().last().unwrap();
			for i in left..=right {
				result.push(i.to_string());
			}
		}
		if num_range {
			let (left,right) = amble.split_once("..").unwrap();
			for i in left.parse::<i32>().unwrap()..=right.parse::<i32>().unwrap() {
				result.push(i.to_string());
			}
		}
	} else {
		let mut cur_string = String::new();
		let mut chars = amble.chars();
		let mut brace_stack = vec![];
		while let Some(ch) = chars.next() {
			match ch {
				'{' => {
					cur_string.push(ch);
					brace_stack.push(ch);
				}
				'}' => {
					cur_string.push(ch);
					brace_stack.pop();
				}
				',' => {
					if brace_stack.is_empty() {
						result.push(cur_string);
						cur_string = String::new();
					} else {
						cur_string.push(ch)
					}
				}
				'\\' => {
					let next = chars.next();
					if !matches!(next, Some('}') | Some('{')) {
						cur_string.push(ch)
					}
					if let Some(next) = next {
						cur_string.push(next)
					}
				}
				_ => cur_string.push(ch)
			}
		}
		result.push(cur_string);
	}
	result
}

pub fn expand_var(mut string: String, var_table: &VarTable) -> OxideResult<String> {
	loop {
		let mut left = String::new();
		let mut right = String::new();
		let mut chars = string.chars().collect::<VecDeque<char>>();

		while let Some(ch) = chars.pop_front() {
			match ch {
				'\\' => {
					left.push(ch);
					if let Some(next_ch) = chars.pop_front() {
						left.push(next_ch);
					} else {
						break;
					}
				}
				'$' => {
					right.extend(chars.drain(..));
					break;
				}
				_ => left.push(ch),
			}
		}

		if right.is_empty() {
			return Ok(string); // No more variables to expand
		}

		let mut right_chars = right.chars().collect::<VecDeque<char>>();
		let mut var_name = String::new();
		while let Some(ch) = right_chars.pop_front() {
			match ch {
				_ if ch.is_alphanumeric() => {
					var_name.push(ch);
				}
				'_' | '[' | ']' => var_name.push(ch),
				'-' | '*' | '?' | '$' | '@' | '#' => {
					var_name.push(ch);
					break;
				}
				'{' => continue,
				'}' => break,
				_ => {
					right_chars.push_front(ch);
					break;
				}
			}
		}
		let right = right_chars.iter().collect::<String>();

		let value: RVal = if REGEX["var_index"].is_match(&var_name) {
			if let Some(caps) = REGEX["var_index"].captures(&var_name) {
				let var_name = caps.get(1).map_or("", |m| m.as_str());
				let index = caps.get(2).map_or("", |m| m.as_str());

				read_vars(|v| v.index_arr(var_name, index.parse::<usize>().unwrap()).unwrap())?
			} else {
				return Err(ShError::from_syntax("This is a weird way to index a variable", Span::new()));
			}
		} else {
			var_table.get_var(&var_name).unwrap_or_default()
		};

		let expanded = format!("{}{}{}", left, value, right);

		if expanded.has_unescaped("$") {
			string = expanded; // Update string and continue the loop for further expansion
		} else {
			return Ok(expanded); // All variables expanded
		}
	}
}

fn expand_params(token: Tk) -> OxideResult<VecDeque<Tk>> {
	let mut expanded_tokens = VecDeque::new();
	// Get the positional parameter string from shellenv and split it
	let arg_string = read_vars(|vars| vars.get_param("@").unwrap_or_default())?;
	let arg_split = arg_string.split(' ');

	// Split the token's text at the first instance of '$@' and make two new tokens
	// Subsequent instances will be handled in later iterations of expand()
	let (left,right) = token.text().split_once("$@").unwrap();
	let left_token = Tk::new(left.to_string(), token.span(), token.flags());
	let right_token = Tk::new(right.to_string(), token.span(), token.flags());

	// Push the left token into the deque
	if !left_token.text().is_empty() {
		expanded_tokens.push_back(left_token);
	}
	for arg in arg_split {
		// For each arg, make a new token and push it into the deque
		let new_token = Tk::new(arg.to_string(),token.span(), token.flags() | WdFlags::FROM_VAR);
		if !new_token.text().is_empty() {
			expanded_tokens.push_back(new_token);
		}
	}
	// Now push the right token into the deque
	if !right_token.text().is_empty() {
		expanded_tokens.push_back(right_token);
	}
	Ok(expanded_tokens)
}
