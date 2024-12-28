use chrono::Local;
use glob::glob;
use log::{debug, info, trace};
use std::collections::VecDeque;
use std::mem::take;
use std::path::{Component, PathBuf};
use crate::event::ShellError;
use crate::interp::token::{Tk, TkType, WdFlags, WordDesc};
use crate::interp::helper::{self,StrExtension};
use crate::shellenv::{EnvFlags, ShellEnv};

use super::parse::{self, NdType, Node, ParseState};
use super::token;

pub fn check_globs(string: String) -> bool {
	string.has_unescaped("?") ||
		string.has_unescaped("*")
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

pub fn expand_prompt(shellenv: &ShellEnv) -> String {
	// TODO:
	//\j - number of managed jobs
	//\l - shell's terminal device name
	//\v - rsh version
	//\V - rsh release; version + patch level
	//\! - history number of this command
	//\# - command number of this command
	let default_color = if shellenv.env_vars.get("UID").is_some_and(|uid| uid == "0") {
		"31" // Red if uid is 0, aka root user
	} else {
		"32" // Green if anyone else
	};
	let cwd: String = shellenv.env_vars.get("PWD").map_or("", |cwd| cwd).to_string();
	let default_path = if cwd.as_str() == "/" {
		"\\e[36m\\w\\e[0m".to_string()
	} else {
		format!("\\e[1;{}m\\w\\e[1;36m/\\e[0m",default_color)
	};
	let ps1: String = shellenv.env_vars.get("PS1").map_or(format!("\\n{}\\n\\e[{}m\\$\\e[36m>\\e[0m ",default_path,default_color), |ps1| ps1.clone());
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
							let mut octal_digits = String::new();
							octal_digits.push(esc_c); // Add the first digit

							for _ in 0..2 {
								if let Some(next_c) = chars.front() {
									if next_c.is_digit(8) {
										octal_digits.push(chars.pop_front().unwrap());
									} else {
										break;
									}
								}
							}

							if let Ok(value) = u8::from_str_radix(&octal_digits, 8) {
								result.push(value as char);
							} else {
								// Invalid sequence, treat as literal
								result.push_str(&format!("\\{}", octal_digits));
							}
						}
						'e' => {
							result.push('\x1B');
							if chars.front().is_some_and(|&ch| ch == '[') {
								result.push(chars.pop_front().unwrap()); // Consume '['
								while let Some(ch) = chars.pop_front() {
									result.push(ch);
									if ch == 'm' {
										break; // End of ANSI sequence
									}
								}
							}
						}
						'[' => {
							// Handle \[ (start of non-printing sequence)
							while let Some(ch) = chars.pop_front() {
								if ch == ']' {
									break; // Stop at the closing \]
								}
								result.push(ch); // Add non-printing content
							}
						}
						']' => {
							// Handle \] (end of non-printing sequence)
							// Do nothing, it's just a marker
						}
						'w' => {
							let mut cwd = shellenv.env_vars.get("PWD").map_or(String::new(), |pwd| pwd.to_string());
							let home = shellenv.env_vars.get("HOME").map_or("", |home| home);
							if cwd.starts_with(home) {
								cwd = cwd.replacen(home, "~", 1); // Use `replacen` to replace only the first occurrence
							}
							// TODO: unwrap is probably safe here since this option is initialized with the environment but it might still cause issues later if this is left unhandled
							let trunc_len = shellenv.shopts.get("trunc_prompt_path").unwrap_or(&0);
							if *trunc_len > 0 {
								let mut path = PathBuf::from(cwd);
								let mut cwd_components: Vec<_> = path.components().collect();
								if cwd_components.len() > *trunc_len {
									cwd_components = cwd_components.split_off(cwd_components.len() - *trunc_len);
									path = cwd_components.iter().collect(); // Rebuild the PathBuf
								}
								cwd = path.to_string_lossy().to_string();
							}
							result.push_str(&cwd);
						}
						'W' => {
							let cwd = PathBuf::from(shellenv.env_vars.get("PWD").map_or("", |pwd| pwd));
							let mut cwd = cwd.components().last().map(|comp| comp.as_os_str().to_string_lossy().to_string()).unwrap_or_default();
							let home = shellenv.env_vars.get("HOME").map_or("", |home| home);
							if cwd.starts_with(home) {
								cwd = cwd.replacen(home, "~", 1); // Replace HOME with '~'
							}
							result.push_str(&cwd);
						}
						'H' => {
							let hostname = shellenv.env_vars.get("HOSTNAME").map_or("unknown host", |host| host);
							result.push_str(hostname);
						}
						'h' => {
							let hostname = shellenv.env_vars.get("HOSTNAME").map_or("unknown host", |host| host);
							if let Some((hostname, _)) = hostname.split_once('.') {
								result.push_str(hostname);
							} else {
								result.push_str(hostname); // No '.' found, use the full hostname
							}
						}
						's' => {
							let sh_name = shellenv.env_vars.get("SHELL").map_or("rsh", |sh| sh);
							result.push_str(sh_name);
						}
						'u' => {
							let user = shellenv.env_vars.get("USER").map_or("unknown", |user| user);
							result.push_str(user);
						}
						'$' => {
							let uid = shellenv.env_vars.get("UID").map_or("0", |uid| uid);
							match uid {
								"0" => result.push('#'),
								_ => result.push('$'),
							}
						}
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
	result
}

pub fn process_ansi_escapes(input: &str) -> String {
	let mut result = String::new();
	let mut chars = input.chars().collect::<VecDeque<char>>();

	while let Some(c) = chars.pop_front() {
		if c == '\\' {
			if let Some(next) = chars.pop_front() {
				match next {
					'a' => result.push('\x07'), // Bell
					'b' => result.push('\x08'), // Backspace
					't' => result.push('\t'),   // Tab
					'n' => result.push('\n'),   // Newline
					'r' => result.push('\r'),   // Carriage return
					'e' | 'E' => result.push('\x1B'), // Escape (\033 in octal)
					'0' => {
						// Octal escape: \0 followed by up to 3 octal digits
						let mut octal_digits = String::new();
						while octal_digits.len() < 3 && chars.front().is_some_and(|ch| ch.is_digit(8)) {
							octal_digits.push(chars.pop_front().unwrap());
						}
						if let Ok(value) = u8::from_str_radix(&octal_digits, 8) {
							let character = value as char;
							result.push(character);
							// Check for ANSI sequence if the result is ESC (\033 or \x1B)
							if character == '\x1B' && chars.front().is_some_and(|&ch| ch == '[') {
								result.push(chars.pop_front().unwrap()); // Consume '['
								while let Some(ch) = chars.pop_front() {
									result.push(ch);
									if ch == 'm' {
										break; // Stop at the end of the ANSI sequence
									}
								}
							}
						}
					}
					_ => {
						// Unknown escape, treat literally
						result.push('\\');
						result.push(next);
					}
				}
			} else {
				// Trailing backslash, treat literally
				result.push('\\');
			}
		} else if c == '\x1B' {
			// Handle raw ESC characters (e.g., \033 in octal or actual ESC char)
			result.push(c);
			if chars.front().is_some_and(|&ch| ch == '[') {
				result.push(chars.pop_front().unwrap()); // Consume '['
				while let Some(ch) = chars.pop_front() {
					result.push(ch);
					if ch == 'm' {
						break; // Stop at the end of the ANSI sequence
					}
				}
			}
		} else {
			result.push(c);
		}
	}

	result
}

pub fn expand_alias(shellenv: &ShellEnv, mut node: Node) -> Result<Node, ShellError> {
	match node.nd_type {
		NdType::Command { ref mut argv } | NdType::Builtin { ref mut argv } => {
			if let Some(cmd_tk) = argv.pop_front() {
				if let Some(alias_content) = shellenv.get_alias(cmd_tk.text()) {
					let new_argv = take(argv);
					let mut child_shellenv = shellenv.clone();
					child_shellenv.mod_flags(|f| *f |= EnvFlags::NO_ALIAS);
					let state = ParseState {
						input: alias_content,
						shellenv: &child_shellenv,
						tokens: VecDeque::new(),
						ast: Node::new(),
					};

					let mut state = token::tokenize(state)?;
					let mut alias_tokens = state.tokens;
					// Trim `SOI` and `EOI` tokens
					alias_tokens.pop_back();
					alias_tokens.pop_front();
					alias_tokens.extend(new_argv);

					// Guard against recursive aliasing
					// i.e. "alias grep="grep --color-auto"
					if alias_tokens.front().is_some_and(|tk| tk.text() == cmd_tk.text()) {
						for token in &mut alias_tokens {
							token.wd = token.wd.add_flag(WdFlags::FROM_ALIAS);
						}
					}

					state.tokens = alias_tokens;
					state = parse::parse(state)?;
					for redir in node.redirs {
						state.ast.redirs.push_back(redir.clone())
					}
					Ok(state.ast)
				} else {
					argv.push_front(cmd_tk);
					Ok(node)
				}
			} else {
				Ok(node)
			}
		}
		_ => unreachable!()
	}
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

		let expand_home = token.flags().contains(WdFlags::IS_ARG) && token.text().has_unescaped("~");
		if expand_home {
			// If this unwrap fails, god help you
			let home = shellenv.get_variable("HOME").unwrap();
			token.wd.text = token.wd.text.replace("~",&home);
		}

		if !is_glob && !is_brace_expansion {
			debug!("expanding var for {}",token.text());
			if token.text().has_unescaped("$") && !token.wd.flags.intersects(WdFlags::FROM_VAR | WdFlags::SNG_QUOTED) {
				info!("found unescaped dollar in: {}",token.text());
				if token.text().has_unescaped("$@") {
					let mut param_tokens = expand_params(shellenv,token);
					while let Some(param) = param_tokens.pop_back() {
						working_buffer.push_front(param);
					}
					continue
				}
				token.wd.text = expand_var(shellenv, token.text().into());
			}
			if helper::is_brace_expansion(token.text()) || token.text().has_unescaped("$") {
				working_buffer.push_front(token);
			} else {
				let expand_home = token.flags().contains(WdFlags::IS_ARG) && token.text().has_unescaped("~");
				if expand_home {
					// If this unwrap fails, god help you
					let home = shellenv.get_variable("HOME").unwrap();
					token.wd.text = token.wd.text.replace("~",&home);
				}
				product_buffer.push_back(token)
			}

		} else if is_brace_expansion && token.text().has_unescaped("{") && token.tk_type != TkType::String {
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
			let expand_home = token.flags().contains(WdFlags::IS_ARG) && token.text().has_unescaped("~");
			if expand_home {
				// If this unwrap fails, god help you
				let home = shellenv.get_variable("HOME").unwrap();
				token.wd.text = token.wd.text.replace("~",&home);
			}
			product_buffer.push_back(token);
		}
	}

	product_buffer
}

pub fn clean_escape_chars(token_buffer: &mut VecDeque<Tk>) {
	for token in token_buffer {
		let mut text = std::mem::take(&mut token.wd.text);
		text = text.replace('\\',"");
		token.wd.text = text;
	}
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
			'-' | '*' | '?' | '$' | '#' => {
				var_name.push(ch);
				break
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
	if expanded.has_unescaped("$") {
		expand_var(shellenv,expanded)
	} else {
		expanded
	}
}

fn expand_params(shellenv: &ShellEnv, token: Tk) -> VecDeque<Tk> {
	let mut expanded_tokens = VecDeque::new();
	// Get the positional parameter string from shellenv and split it
	let arg_string = shellenv.get_variable("@").unwrap_or_default();
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
		expanded_tokens.push_back(new_token);
	}
	// Now push the right token into the deque
	if !right_token.text().is_empty() {
		expanded_tokens.push_back(right_token);
	}
	expanded_tokens
}
