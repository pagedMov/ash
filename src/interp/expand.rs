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
use crate::OxResult;

use super::parse::{NdType, Node, Span};
use super::token::REGEX;

static GLOB_CHARS: [&str;2] = ["*", "?"];

pub fn check_globs(tk: Tk) -> bool {
	let text = tk.text();
	let flags = tk.flags();
	let kind = &tk.tk_type;
	dbg!(&tk);

	if !flags.contains(WdFlags::IS_ARG) || flags.intersects(WdFlags::SNG_QUOTED | WdFlags::DUB_QUOTED) || *kind == TkType::String {
		return false; // Skip if not an argument
	}

	// Check for unescaped glob characters

	let has_globs = GLOB_CHARS.iter().any(|&ch| text.has_unescaped(ch));
	let has_brackets = helper::has_valid_delims(text, "[", "]");
	has_globs || has_brackets
}

pub fn expand_shebang(mut body: String) -> OxResult<String> {
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

pub fn expand_arguments(node: &mut Node) -> OxResult<Vec<Tk>> {
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

#[derive(Debug)]
pub enum PromptTk {
	PlainText(String),  // Simple chararacter
	OctalSeq(String),   // Represents an octal sequence (e.g., \033 for ANSI escape codes)
	AnsiSeq(String),    // Represents an ANSI escape sequence (e.g., \e[31m for red text)
	NonPrint(String),   // Represents a non-printable sequence
	UserSequence(String), // User-defined escape sequence
	WorkingDir,         // Full working directory (e.g., /home/user/projects)
	WorkingDirAbridged, // Abridged working directory (e.g., ~/projects)
	Hostname,           // Full hostname of the machine
	HostnameAbridged,   // Abridged hostname (e.g., "host" from "host.domain.com")
	ShellName,          // Name of the shell (e.g., "bash", "zsh", "ox")
	Username,           // Username of the current user
	PromptSymbol,       // The shell's prompt symbol (e.g., $ for user, # for root)
	ExitSuccess,
	ExitFail,
	ExitCode,
	GitSigns,           // Git repository status symbols (e.g., dirty tree, ahead/behind)
	GitBranch,          // Git repository branch
	Bell,               // '\a': Bell character (ASCII 7)
	Newline,            // '\n': Newline character
	CarriageReturn,     // '\r': Carriage return
	Backslash,          // '\\': Literal backslash
	SingleQuote,        // '\''': Literal single quote
	DoubleQuote,        // '\"': Literal double quote
	HideGroupStart,
	HideGroupEnd,
	WeekdayDate,        // '%a %b %d': Abbreviated weekday, month, and day (e.g., "Mon Jan 01")
	Time24Hr,           // '%H:%M:%S': 24-hour time with seconds (e.g., "14:30:15")
	Time12Hr,           // '%I:%M:%S': 12-hour time with seconds (e.g., "02:30:15 PM")
	Time24HrNoSeconds,  // '%H:%M': 24-hour time without seconds (e.g., "14:30")
	Time12HrShort,      // '%I:%M %p': 12-hour time with AM/PM (e.g., "02:30 PM")
}

pub fn tokenize_prompt(ps1: &str) -> VecDeque<PromptTk> {
	let mut tokens = VecDeque::new();
	let mut chars = ps1.chars().collect::<VecDeque<char>>();
	let mut buffer = String::new();

	while let Some(c) = chars.pop_front() {
		match c {
			'\\' => {
				// Flush any accumulated plain text as a token
				if !buffer.is_empty() {
					tokens.push_back(PromptTk::PlainText(buffer.clone()));
					buffer.clear();
				}

				// Process the escape sequence
				if let Some(esc_c) = chars.pop_front() {
					match esc_c {
						'a' => tokens.push_back(PromptTk::Bell),
						'n' => tokens.push_back(PromptTk::Newline),
						'r' => tokens.push_back(PromptTk::CarriageReturn),
						'\\' => tokens.push_back(PromptTk::Backslash),
						'\'' => tokens.push_back(PromptTk::SingleQuote),
						'"' => tokens.push_back(PromptTk::DoubleQuote),
						'd' => tokens.push_back(PromptTk::WeekdayDate),
						't' => tokens.push_back(PromptTk::Time24Hr),
						'T' => tokens.push_back(PromptTk::Time12Hr),
						'A' => tokens.push_back(PromptTk::Time24HrNoSeconds),
						'@' => tokens.push_back(PromptTk::Time12HrShort),
						_ if esc_c.is_digit(8) => {
							let octal_seq = helper::capture_octal_escape(&mut chars, esc_c);
							tokens.push_back(PromptTk::OctalSeq(octal_seq));
						}
						'e' => {
							let ansi_seq = helper::capture_ansi_escape(&mut chars);
							tokens.push_back(PromptTk::AnsiSeq(ansi_seq));
						}
						'[' => {
							let non_print = helper::capture_non_print_sequence(&mut chars);
							tokens.push_back(PromptTk::NonPrint(non_print));
						}
						'w' => tokens.push_back(PromptTk::WorkingDir),
						'W' => tokens.push_back(PromptTk::WorkingDirAbridged),
						'H' => tokens.push_back(PromptTk::Hostname),
						'h' => tokens.push_back(PromptTk::HostnameAbridged),
						's' => tokens.push_back(PromptTk::ShellName),
						'u' => tokens.push_back(PromptTk::Username),
						'$' => tokens.push_back(PromptTk::PromptSymbol),
						'?' => tokens.push_back(PromptTk::ExitCode),
						'G' => tokens.push_back(PromptTk::GitSigns),
						'B' => tokens.push_back(PromptTk::GitBranch),
						'S' => tokens.push_back(PromptTk::ExitSuccess),
						'F' => tokens.push_back(PromptTk::ExitFail),
						'(' => tokens.push_back(PromptTk::HideGroupStart),
						')' => tokens.push_back(PromptTk::HideGroupEnd),
						_ => {
							// Unrecognized escape, treat it as raw text
							tokens.push_back(PromptTk::PlainText(format!("\\{}", esc_c)));
						}
					}
				} else {
					// Handle dangling backslash as raw text
					tokens.push_back(PromptTk::PlainText("\\".to_string()));
				}
			}
			_ => {
				// Accumulate plain text
				buffer.push(c);
			}
		}
	}

	// Flush remaining plain text
	if !buffer.is_empty() {
		tokens.push_back(PromptTk::PlainText(buffer));
	}

	tokens
}

pub fn expand_prompt() -> OxResult<String> {
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

	// Tokenize the PS1 string
	let mut tokens = tokenize_prompt(&ps1);

	// Expand tokens into the final prompt string
	let mut result = String::new();
	while let Some(token) = tokens.pop_front() {
		match token {
			PromptTk::PlainText(text) => result.push_str(&text),
			PromptTk::Bell => result.push('\x07'),
			PromptTk::Newline => result.push('\n'),
			PromptTk::CarriageReturn => result.push('\r'),
			PromptTk::Backslash => result.push('\\'),
			PromptTk::SingleQuote => result.push('\''),
			PromptTk::DoubleQuote => result.push('"'),
			PromptTk::WeekdayDate => result.push_str(expand_time("%a %b %d").as_str()),
			PromptTk::Time24Hr => result.push_str(expand_time("%H:%M:%S").as_str()),
			PromptTk::Time12Hr => result.push_str(expand_time("%I:%M:%S").as_str()),
			PromptTk::Time24HrNoSeconds => result.push_str(expand_time("%H:%M").as_str()),
			PromptTk::Time12HrShort => result.push_str(expand_time("%I:%M %p").as_str()),
			PromptTk::OctalSeq(octal) => {
				let octal_char = helper::escseq_octal_escape(&mut octal.chars().collect(), '0')?;
				result.push(octal_char);
			}
			PromptTk::AnsiSeq(ansi) => result.push_str(&ansi),
			PromptTk::NonPrint(non_print) => result.push_str(&non_print),
			PromptTk::HideGroupStart => result.push_str(&helper::handle_prompt_hidegroup(&mut tokens)?),
			PromptTk::HideGroupEnd => result.push(')'),
			PromptTk::WorkingDir => result.push_str(&helper::escseq_working_directory()?),
			PromptTk::WorkingDirAbridged => result.push_str(&helper::escseq_basename_working_directory()?),
			PromptTk::Hostname => result.push_str(&helper::escseq_full_hostname()?),
			PromptTk::HostnameAbridged => result.push_str(&helper::escseq_short_hostname()?),
			PromptTk::ShellName => result.push_str(&helper::escseq_shell_name()?),
			PromptTk::Username => result.push_str(&helper::escseq_username()?),
			PromptTk::PromptSymbol => result.push(helper::escseq_prompt_symbol()?),
			PromptTk::GitSigns => result.push_str(&helper::escseq_gitsigns()?),
			PromptTk::GitBranch => result.push_str(&helper::escseq_gitsigns()?),
			PromptTk::ExitSuccess => result.push_str(&helper::escseq_success()?),
			PromptTk::ExitFail => result.push_str(&helper::escseq_fail()?),
			PromptTk::ExitCode => result.push_str(&helper::escseq_exitcode()?),
			PromptTk::UserSequence(seq) => result.push_str(&helper::escseq_custom(&seq)?),
		}
	}

	Ok(result)
}


pub fn expand_alias(alias: &str) -> OxResult<String> {
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

pub fn expand_token(token: Tk, expand_glob: bool) -> OxResult<VecDeque<Tk>> {
	let mut working_buffer: VecDeque<Tk> = VecDeque::new();
	let mut product_buffer: VecDeque<Tk> = VecDeque::new();
	let split_words = token.tk_type != TkType::String;

	//TODO: find some way to clean up this surprisingly functional mess
	// Escaping breaks this right now I think

	working_buffer.push_back(token.clone());
	while let Some(mut token) = working_buffer.pop_front() {
		// If expand_glob is true, then check for globs. Otherwise, is_glob is false
		//let is_glob = if expand_glob { check_globs(token.clone()) } else { expand_glob };
		let is_glob = false;
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

pub fn expand_cmd_sub(token: Tk) -> OxResult<Tk> {
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

pub fn expand_braces(word: String) -> OxResult<VecDeque<String>> {
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

pub fn expand_var(mut string: String, var_table: &VarTable) -> OxResult<String> {
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

fn expand_params(token: Tk) -> OxResult<VecDeque<Tk>> {
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

#[cfg(test)]
mod tests {
	use crate::shellenv::{self, read_meta, write_logic, write_meta, write_vars, RVal};
	use nix::unistd::{getuid, User};

	use super::*;

	fn token(tk_type: TkType, text: &str, span: Span, flags: WdFlags) -> Tk {
		Tk {
			tk_type,
			wd: WordDesc {
				text: text.to_string(),
				span,
				flags,
			},
		}
	}

	#[test]
	fn test_expand_var() {
		let input = "$USER".to_string();
		let user = User::from_uid(getuid()).unwrap().unwrap().name;
		let vars = shellenv::borrow_var_table().unwrap();
		let expanded = expand_var(input,&vars).unwrap();
		assert_eq!(expanded, user.to_string());
	}

	#[test]
	fn test_expand_var_indexed() {
		let array = RVal::Array(vec![RVal::parse("first").unwrap(), RVal::parse("second").unwrap()]);
		write_vars(|vars| { vars.set_var("arr", array); }).unwrap();
		let input = "$arr[0]".to_string();
		let vars = shellenv::borrow_var_table().unwrap();
		let expanded = expand_var(input,&vars).unwrap();
		assert_eq!(expanded, "first");
	}

	#[test]
	fn test_expand_token() {
		let token = token(TkType::String, "~/projects", Span::new(), WdFlags::empty());
		let expanded = expand_token(token, true).unwrap();
		let user = User::from_uid(getuid()).unwrap().unwrap().name;
		assert_eq!(expanded.len(), 1);
		assert_eq!(expanded[0].text(), format!("/home/{}/projects",user).as_str());
	}

	#[test]
	fn test_expand_braces() {
		let input1 = "{1,2,3}".to_string();
		let input2 = "{1..5}".to_string();
		let input3 = "{a..c}{1..5}".to_string();
		let expanded1 = expand_braces(input1).unwrap();
		let expanded2 = expand_braces(input2).unwrap();
		let expanded3 = expand_braces(input3).unwrap();
		assert_eq!(expanded1, VecDeque::from(vec!["1".into(), "2".into(), "3".into()]));
		assert_eq!(expanded2, VecDeque::from(vec!["1".into(), "2".into(), "3".into(), "4".into(), "5".into()]));
		assert_eq!(expanded3, VecDeque::from(vec!["a1".into(), "a2".into(), "a3".into(), "a4".into(), "a5".into(), "b1".into(), "b2".into(), "b3".into(), "b4".into(), "b5".into(), "c1".into(), "c2".into(), "c3".into(), "c4".into(), "c5".into(),]));
	}

	#[test]
	fn test_expand_arguments() {
		let mut node = Node {
			command: None,
			nd_type: NdType::Builtin {
				argv: VecDeque::from(vec![
								token(TkType::Ident, "echo", Span::new(), WdFlags::empty()),
								token(TkType::String, "hello", Span::new(), WdFlags::empty()),
				]),
			},
			span: Span::new(),
			flags: NdFlags::empty(),
			redirs: VecDeque::new(),
		};

		let expanded = expand_arguments(&mut node).unwrap();
		assert_eq!(expanded.len(), 2);
		assert_eq!(expanded[0].text(), "echo");
		assert_eq!(expanded[1].text(), "hello");
	}

	#[test]
	fn test_expand_prompt() {
		let default_color = if read_vars(|vars| vars.get_evar("UID").is_some_and(|uid| uid == "0")).unwrap() {
			"31" // Red if uid is 0, aka root user
		} else {
			"32" // Green if anyone else
		};
		let mut cwd: String = read_vars(|vars| vars.get_evar("PWD").map_or("".into(), |cwd| cwd).to_string()).unwrap();
		let trunc_backup = read_meta(|m| m.get_shopt("trunc_prompt_path")).unwrap().unwrap();
		write_meta(|m| m.set_shopt("trunc_prompt_path", "0")).unwrap();
		let default_path = if cwd.as_str() == "/" {
			"\\e[36m\\w\\e[0m".to_string()
		} else {
			format!("\\e[1;{}m\\w\\e[1;36m/\\e[0m",default_color)
		};
		let home = read_vars(|v| v.get_evar("HOME").unwrap()).unwrap();
		if cwd.starts_with(&home) {
			cwd = cwd.replacen(&home, "~", 1);
		}
		let ps1: String = format!("\\n{}\\n\\e[{}mdebug \\$\\e[36m>\\e[0m ",default_path,default_color);
		write_vars(|v| v.export_var("PS1", &ps1)).unwrap();
		let prompt = expand_prompt().unwrap();
		let expected = format!("\n\u{1b}[1;32m{}\u{1b}[1;36m/\u{1b}[0m\n\u{1b}[32mdebug $\u{1b}[36m>\u{1b}[0m ",cwd);

		assert_eq!(prompt, expected);
		write_meta(|m| m.set_shopt("trunc_prompt_path", &trunc_backup)).unwrap();
	}

	#[test]
	fn test_process_ansi_escapes() {
		let input = r"\033[31mRed Text\033[0m";
		let processed = helper::process_ansi_escapes(input);
		assert_eq!(processed, "\x1B[31mRed Text\x1B[0m");
	}

	#[test]
	fn test_check_home_expansion() {
		assert!(check_home_expansion("~/projects"));
		assert!(!check_home_expansion("/projects"));
	}

	#[test]
	fn test_split_tokens() {
		let mut tokens = VecDeque::from(vec![
			token(TkType::String, "arg1 arg2", Span::new(), WdFlags::empty()),
			token(TkType::String, "foo\"bar baz\"", Span::new(), WdFlags::empty()),
		]);
		helper::split_tokens(&mut tokens);
		assert_eq!(tokens.len(), 3);
		assert_eq!(tokens[0].text(), "arg1");
		assert_eq!(tokens[1].text(), "arg2");
		assert_eq!(tokens[2].text(), "foobar baz");
	}

	#[test]
	fn test_clean_escape_chars() {
		let mut tokens = VecDeque::from(vec![
			token(TkType::String, r"arg\ with\ space", Span::new(), WdFlags::empty()),
		]);
		clean_escape_chars(&mut tokens);
		assert_eq!(tokens[0].text(), "arg with space");
	}
}
