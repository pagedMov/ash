use crate::{event::ShError, execute::{self, ProcIO, RustFd}, interp::token::REGEX, shellenv::{attach_tty, disable_reaping, enable_reaping, read_jobs, read_logic, read_meta, read_vars, write_jobs, write_logic, write_vars, DisplayWaitStatus, HashFloat, Job, OxVal}, OxResult};
use nix::{sys::wait::WaitStatus, unistd::{dup2, getpgrp}, NixPath};
use serde_json::Value;
use std::{alloc::GlobalAlloc, collections::{HashMap, VecDeque}, env, f32::INFINITY, fs, io::{self, Read}, mem::take, os::{fd::AsRawFd, unix::fs::PermissionsExt}, path::{Path, PathBuf}, thread, time::Duration};

use super::{expand::{self, PromptTk}, parse::{NdFlags, NdType, Node, Span}, token::{Tk, TkType, WdFlags, WordDesc}};

#[macro_export]
macro_rules! deconstruct {
	($type:path { $($field:ident),* }, $var:expr, $logic:block) => {
		if let $type { $($field),* } = $var {
			$logic
		} else {
			unreachable!()
		}
	};
}

pub trait VecExtension<T> {
	fn extended(self, vec: Vec<T>) -> Vec<T>;
}

impl<T> VecExtension<T> for Vec<T> {
	fn extended(self, vec: Vec<T>) -> Vec<T> {
		let mut new_vec = self;
		new_vec.extend(vec);
		new_vec
	}
}

pub trait VecDequeExtension<T> {
	fn to_vec(self) -> Vec<T>;
	fn map_rotate<F>(&mut self, transform: F)
	where
			F: FnMut(T) -> T;
}

impl<T> VecDequeExtension<T> for VecDeque<T> {
	fn to_vec(self) -> Vec<T> {
		self.into_iter().collect::<Vec<T>>()
	}

	fn map_rotate<F>(&mut self, mut transform: F)
		where F: FnMut(T) -> T,
	{
		let mut buffer = VecDeque::new();
		while let Some(element) = self.pop_front() {
			let transformed = transform(element);
			buffer.push_back(transformed);
		}

		while let Some(element) = buffer.pop_front() {
			self.push_back(element);
		}
	}
}

pub trait StrExtension {
	fn trim_command_sub(&self) -> Option<String>;
	fn split_last(&self, pat: &str) -> Option<(String,String)>;
	fn has_unescaped(&self, pat: &str) -> bool;
	fn has_varsub(&self) -> bool;
	fn has_unquoted(&self, pat: &str) -> bool;
	fn trim_quotes(&self) -> String;
	fn split_outside_quotes(&self) -> Vec<String>;
	fn split_twice(&self,left: &str, right: &str) -> Option<(String,String,String)>;
	fn expand_globs(&self) -> Vec<String>;
	fn consume_escapes(&self) -> String;
}

impl StrExtension for str {
	fn has_varsub(&self) -> bool {
		let mut chars = self.chars().peekable();
		let mut paren_stack = vec![];
		while let Some(ch) = chars.next() {
			match ch {
				'$' if chars.peek() != Some(&'(') => return true,
				'\\' => {
					chars.next();
				}
				'(' => {
					paren_stack.push('(');
					while let Some(paren_ch) = chars.next() {
						match paren_ch {
							'\\' => {
								chars.next();
							}
							')' => {
								paren_stack.pop();
								if paren_stack.is_empty() {
									break
								}
							}
							'(' => paren_stack.push('('),
							_ => { /* Do nothing */ }
						}
					}
				}
				'\'' => {
					while let Some(squote_ch) = chars.next() {
						if squote_ch == '\'' {
							break
						}
					}
				}
				_ => { /* Do nothing */ }
			}
		}
		false
	}
	fn consume_escapes(&self) -> String {
		// This function consumes one layer of escape characters
		// Meaning that double escapes will still be left with one escape, i.e. \\ -> \
		let mut product = String::new();
		let mut chars = self.chars();
		while let Some(ch) = chars.next() {
			match ch {
				'\\' => {
					if let Some(ch) = chars.next() {
						product.push(ch)
					}
				}
				_ => {
					product.push(ch)
				}
			}
		}
		product
	}
	fn expand_globs(&self) -> Vec<String> {
		let result = match glob::glob(self) {
			Ok(paths) => {
				let mut working_buffer = vec![];
				for path_result in paths {
					if let Ok(path) = path_result {
						working_buffer.push(path.to_str().unwrap().to_string());
					}
				}
				if !working_buffer.is_empty() {
					working_buffer
				} else {
					vec![self.to_string()]
				}
			}
			Err(_) => vec![self.to_string()]
		};
		result
	}
	/// This function looks for two patterns to split at.
	/// The left one must come first, and the right one second.
	/// If the string matches this pattern, it will return all three parts as a tuple
	/// It also respects escaped characters
	fn split_twice(&self, left: &str, right: &str) -> Option<(String, String, String)> {

		// Find the position of the left delimiter
		if let Some(left_pos) = self.find(left) {
			let prefix = &self[..left_pos]; // Everything before the left delimiter
			let remainder = &self[left_pos + left.len()..]; // Everything after the left delimiter

			// Find the position of the right delimiter in the remainder
			if let Some(right_pos) = remainder.find(right) {
				let product = &remainder[..right_pos]; // Between left and right delimiters
				let suffix = &remainder[right_pos + right.len()..]; // Everything after the right delimiter


				// Return the parts as owned Strings
				return Some((prefix.to_string(), product.to_string(), suffix.to_string()));
			}
		}

		None
	}
	fn split_outside_quotes(&self) -> Vec<String> {
		let mut result = Vec::new();
		let mut working_str = String::new();
		let mut dquoted = false;
		let mut squoted = false;
		let mut chars = self.chars();

		while let Some(ch) = chars.next() {
			match ch {
				'\\' => {
					working_str.push(ch);
					if let Some(next_ch) = chars.next() {
						working_str.push(next_ch);
					}
				}
				' ' | '\t' if !dquoted && !squoted => {
					// Push the current token if not in quotes
					if !working_str.is_empty() {
						result.push(std::mem::take(&mut working_str));
					}
				}
				'"' if !squoted => {
					// Toggle double-quote state
					dquoted = !dquoted;
				}
				'\'' if !dquoted => {
					// Toggle single-quote state
					squoted = !squoted;
				}
				_ => {
					// Append to the current token
					working_str.push(ch);
				}
			}
		}

		// Push the last token if any
		if !working_str.is_empty() {
			result.push(working_str);
		}

		result
	}
	fn trim_command_sub(&self) -> Option<String> {
		self.strip_prefix("$(")
			.and_then(|s| s.strip_suffix(")"))
			.map(|s| s.trim().to_string())
	}
	fn trim_quotes(&self) -> String {
		let chars = self.chars();
		let mut result = String::new();
		let mut in_quote = false;
		let mut opening_quote: Option<char> = None;

		for ch in chars {
			match ch {
				'\'' | '"' if !in_quote => {
					opening_quote = Some(ch);
					in_quote = true;
				}
				'\'' | '"' if Some(ch) == opening_quote => {
					in_quote = false;
					opening_quote = None;
				}
				_ => result.push(ch),
			}
		}
		result.trim().to_string()
	}

	fn split_last(&self, pat: &str) -> Option<(String, String)> {
		let mut last_index = None;
		let pat_len = pat.len();

		// Iterate through the string and find the last occurrence of `pat`
		for i in 0..=self.len().saturating_sub(pat_len) {
			if &self[i..i + pat_len] == pat {
				last_index = Some(i);
			}
		}

		// If no occurrence is found, return None
		last_index.map(|index| (
				self[..index].to_string(),        // Everything before `pat`
				self[index + pat_len..].to_string(), // Everything after `pat`
		))
	}

	/// Checks to see if a string slice contains a specified unescaped text pattern. This method assumes that '\' is the escape character.
	///
	fn has_unescaped(&self, pat: &str) -> bool {
		let mut chars = self.chars().collect::<VecDeque<char>>();
		let mut working_pat = String::new();
		let mut escaped = false;

		while let Some(ch) = chars.pop_front() {
			if !escaped && working_pat.contains(pat) {
				return true;
			}
			match ch {
				' ' | '\t' => {
					// Check for unescaped match when encountering a space/tab
					// Reset for next segment
					escaped = false;
					working_pat.clear();
				}
				'\\' => {
					escaped = true;
				}
				_ => {
					working_pat.push(ch);
				}
			}
		}

		// Check for unescaped match at the end of the string
		!escaped && working_pat.contains(pat)
	}
	fn has_unquoted(&self, pat: &str) -> bool {
		let mut chars = self.chars().peekable();
		let mut in_double_quotes = false;
		let mut in_single_quotes = false;

		// Sliding window to track the current state of the string
		let mut window = String::with_capacity(pat.len());

		while let Some(ch) = chars.next() {
			match ch {
				'"' if !in_single_quotes => {
					// Toggle double-quote state
					in_double_quotes = !in_double_quotes;
				}
				'\'' if !in_double_quotes => {
					// Toggle single-quote state
					in_single_quotes = !in_single_quotes;
				}
				'\\' if in_double_quotes || in_single_quotes => {
					// Skip the escaped character in quotes
					chars.next();
				}
				_ if !in_double_quotes && !in_single_quotes => {
					// Append to the sliding window only if not in quotes
					window.push(ch);
					if window.len() > pat.len() {
						window.remove(0); // Maintain window size
					}
					if window == pat {
						return true;
					}
				}
				_ => {
					// Inside quotes, ignore the content
				}
			}
		}

		false
	}

}

pub fn split_at_varsub(word: &str) -> (String,String) {
	let mut left = String::new();
	let mut right = String::new();
	let mut chars = word.chars().peekable();
	let mut brace_stack = vec![];
	while let Some(ch) = chars.next() {
		match ch {
			'\\' => {
				left.push(ch);
				if let Some(esc_ch) = chars.next() {
					left.push(ch);
				}
			}
			'{' => {
				brace_stack.push(ch);
				left.push(ch);
				while let Some(ch) = chars.next() {
					left.push(ch);
					match ch {
						'{' => brace_stack.push(ch),
						'}' => {
							brace_stack.pop();
							if brace_stack.is_empty() {
								break
							}
						}
						_ => { /* Do nothing */ }
					}
				}
			}
			'$' if chars.peek() != Some(&'(') => {
				right = chars.collect::<String>();
				break
			}
			_ => left.push(ch)
		}
	}
	let result = (left,right);
	result
}

/// I don't feel like learning how to use a debugger
/// So I made this instead
pub fn breakpoint<T: std::fmt::Debug>(var: T) {
	eprintln!("{:?}",var);
	eprintln!("Press any key to continue...");
	let _ = std::io::stdin().bytes().next(); // Waits for any keystroke before continuing
}

pub fn contains_glob(word: &str) -> bool {
	let mut chars = word.chars();
	while let Some(c) = chars.next() {
		match c {
			'\\' => {
				chars.next(); // Skip escaped characters
			}
			'\'' => {
				while let Some(c) = chars.next() {
					if c == '\'' { break }
				}
			}
			'"' => {
				while let Some(c) = chars.next() {
					if c == '"' { break }
				}
			}
			'(' => {
				while let Some(c) = chars.next() {
					if c == ')' { break }
				}
			}
			'*' | '?' => return true,
			'[' => {
				while let Some(next) = chars.next() {
					match next {
						'\\' => { chars.next(); continue }
						']' => return true,
						_ => continue
					}
				}
			}
			_ => { /* Do nothing */ }
		}
	}
	false
}

pub fn parse_vec(input: &str) -> Result<Vec<OxVal>,String> {
	if !input.starts_with('[') {
		return Err("Did not find an opening bracket for this array definition".into())
	}
	if !input.ends_with(']') {
		return Err("Did not find a closing bracket for this array definition".into())
	}
	let array_str = input.strip_prefix('[').unwrap().strip_suffix(']').unwrap();
	if let Some(vec) = vec_by_type(array_str) {
		Ok(vec)
	} else {
		Err("Failed to parse this array".into())
	}
}

pub fn vec_by_type(str: &str) -> Option<Vec<OxVal>> {
	str.split(',')
		.map(str::trim)
		.map(OxVal::parse)
		.collect::<Result<Vec<OxVal>, _>>()
		.ok()
}

// This is used when pesky system calls want to emit their own errors
// Which ends up being redundant, since rsh has it's own error reporting system
// Kind of hacky but fuck it
// It works by taking the function as an argument and then executing it in
// an isolated context where stderr is briefly redirected to /dev/null.
pub fn suppress_err<F: FnOnce() -> T, T>(f: F) -> T {
	let stderr = io::stderr();
	let stderr_fd = stderr.as_raw_fd();
	let devnull = fs::OpenOptions::new().write(true).open("/dev/null").unwrap();
	let devnull_fd = devnull.as_raw_fd();

	dup2(devnull_fd, stderr_fd).unwrap();
	let result = f();
	dup2(stderr_fd,stderr_fd).unwrap();
	result
}

pub fn slice_completion(line: &str, candidate: &str) -> String {
	let mut buffer = String::new();
	let mut chars = candidate.chars();
	let mut found = false;
	while let Some(ch) = chars.next() {
		buffer.push(ch);
		if line.ends_with(&buffer) {
			found = true;
			break
		}
	}
	let result = chars.collect::<String>();
	if result.is_empty() && !found {
		buffer
	} else {
		result
	}
}

pub fn combine_tokens(tokens: &mut VecDeque<Tk>) {
	if tokens.is_empty() {
		return;
	}

	// Combine spans
	let span_start = tokens.front().unwrap().span().start;
	let span_end = tokens.back().unwrap().span().end;
	let span = Span::from(span_start, span_end);

	// Combine text and flags
	let mut flags = WdFlags::empty();
	let new_text = tokens.iter().map(|token| {
		flags |= token.flags();  // Accumulate flags
		token.text()
	}).collect::<Vec<_>>().join(" ");

	// Return a new combined token
	*tokens = VecDeque::from(vec![Tk {
		tk_type: TkType::String,
		wd: WordDesc {
			text: new_text,
			span,
			flags,
		},
	}])
}

pub fn split_tokens(tokens: &mut VecDeque<Tk>) {
	let mut new_buffer = VecDeque::new();

	while let Some(tk) = tokens.pop_front() {
		let split = tk.text().split_outside_quotes();
		for word in split {
			new_buffer.push_back(Tk {
				tk_type: TkType::String,
				wd: WordDesc {
					text: word,
					span: tk.span(),
					flags: tk.flags(),
				}
			});
		}
	}

	// Replace the original buffer with the new one
	*tokens = new_buffer;
}

pub fn which(command: &str) -> Option<String> {
	if let Some(env_path) = read_vars(|v| v.get_evar("PATH")).unwrap() {
		for path in env::split_paths(&env_path) {
			let full_path = path.join(command);
			if full_path.is_file() && is_exec(&full_path) {
				return Some(full_path.to_string_lossy().to_string())
			}
		}
	}
	None
}

pub fn is_exec(path: &Path) -> bool {
	fs::metadata(path)
		.map(|meta| meta.is_file() && (meta.permissions().mode() & 0o111) != 0)
		.unwrap_or(false)
}

pub fn handle_autocd_check(node: &Node, argv: &[Tk]) -> OxResult<bool> {
	if read_meta(|m| m.get_shopt("core.autocd").is_ok_and(|opt| opt.parse::<bool>().unwrap()))? && argv.len() == 1 {
		let path_cand = argv.first().unwrap();
		let is_relative = path_cand.text().starts_with('.');
		let contains_slash = path_cand.text().contains('/');
		let path_exists = Path::new(path_cand.text()).is_dir();

		if (is_relative || contains_slash) && path_exists {
			return Ok(true);
		}
	}
	Ok(false)
}

pub fn overwrite_func(alias: &str) -> OxResult<()> {
	if read_logic(|l| l.get_func(alias))?.is_some() {
		write_logic(|l| l.remove_func(alias))?;
	}
	Ok(())
}

pub fn overwrite_alias(func: &str) -> OxResult<()> {
	if read_logic(|l| l.get_alias(func))?.is_some() {
		write_logic(|l| l.remove_alias(func))?;
	}
	Ok(())
}

pub fn unset_var_conflicts(key: &str) -> OxResult<()> {
	if read_vars(|v| v.get_var(key))?.is_some() {
		write_vars(|v| v.unset_var(key))?
	}
	if read_vars(|v| v.get_evar(key))?.is_some() {
		std::env::remove_var(key);
		write_vars(|v| v.unset_evar(key))?;
	}

	Ok(())
}

pub fn set_last_status(status: &WaitStatus) -> OxResult<()> {
	match status {
		WaitStatus::Exited(pid, code) => {
			write_vars(|v| v.set_param("?".into(), code.to_string()))?;
		}
		WaitStatus::Signaled(_, signal, _) | WaitStatus::Stopped(_,signal) => {
			write_vars(|v| v.set_param("?".into(), (*signal as i32).to_string()))?;
		}
		WaitStatus::Continued(_) | WaitStatus::StillAlive => { /* Do nothing */ }
		_ => unimplemented!()
	}
	Ok(())
}

pub fn handle_fg(job: Job) -> OxResult<()> {
	disable_reaping();
	let statuses = write_jobs(|j| j.new_fg(job))??;
	for status in statuses {
		match status {
			WaitStatus::Stopped(pid, sig) => {
				attach_tty(getpgrp())?;
				crate::signal::handle_child_stop(pid, sig)?
			},
			WaitStatus::Signaled(pid, sig, _) => {
				attach_tty(getpgrp())?;
				crate::signal::handle_child_signal(pid, sig)?
			},
			_ => { /* Do nothing */ }
		}
	}
	write_jobs(|j| {
		j.update_job_statuses().unwrap();
		j.reset_fg();
	})?;
	enable_reaping()
}

pub fn check_git(path: PathBuf) -> Option<PathBuf> {
	let mut current_path = path.as_path();

	if current_path.join(".git").exists() {
			return Some(current_path.join(".git"));
	}

	while let Some(parent) = current_path.parent() {
		if parent.join(".git").exists() {
			return Some(parent.join(".git"));
		}
		current_path = parent;
	}

	None
}

pub fn flatten_tree(left: Node, right: Node) -> VecDeque<Node> {
	let mut flattened = VecDeque::new();
	let mut stack = vec![(left, right)];

	while let Some((current_left, current_right)) = stack.pop() {
		flattened.push_front(current_right);

		match current_left.nd_type {
    NdType::PipelineBranch { left, right, both: _ } | NdType::ChainTree { left, right, op: _ } => {
			    stack.push((*left, *right));
		    }
    _ => {
			flattened.push_front(current_left);
		}
}
	}

	flattened
}

pub fn capture_octal_escape(chars: &mut VecDeque<char>, first_digit: char) -> String {
	let mut octal = first_digit.to_string();

	for _ in 0..2 {
		if let Some(c) = chars.front() {
			if c.is_digit(8) {
				octal.push(chars.pop_front().unwrap());
			} else {
				break;
			}
		}
	}

	octal
}

pub fn capture_non_print_sequence(chars: &mut VecDeque<char>) -> String {
	let mut sequence = String::new();

	while let Some(c) = chars.pop_front() {
		if c == ']' {
			break; // End of the non-printing sequence
		}
		sequence.push(c);
	}

	sequence
}

pub fn capture_user_sequence(chars: &mut VecDeque<char>) -> String {
	let mut sequence = String::from("prompt.custom.");
	while let Some(c) = chars.pop_front() {
		match c {
			'\\' if chars.front().is_some_and(|c| *c == '}') => {
				chars.pop_front();
				break
			}
			_ => sequence.push(c)
		}
	}

	sequence
}

pub fn capture_ansi_escape(chars: &mut VecDeque<char>) -> String {
	let mut sequence = String::from("\x1b"); // Start with the escape character (ESC)

	while let Some(c) = chars.pop_front() {
		sequence.push(c);

		// ANSI sequences typically end with a letter (e.g., 'm' or 'K')
		if c.is_alphabetic() {
			break;
		}
	}

	sequence
}

pub fn handle_prompt_hidegroup(tokens: &mut VecDeque<PromptTk>) -> OxResult<String> {
	let mut result = String::new();
	let mut found = false;
	while let Some(token) = tokens.pop_front() {
		match token {
			PromptTk::PlainText(text) => result.push_str(&text),
			PromptTk::Bell => result.push('\x07'),
			PromptTk::Newline => result.push('\n'),
			PromptTk::CarriageReturn => result.push('\r'),
			PromptTk::Backslash => result.push('\\'),
			PromptTk::SingleQuote => result.push('\''),
			PromptTk::DoubleQuote => result.push('"'),
			PromptTk::WeekdayDate =>{
				let output = expand::expand_time("%a %b %d");
				if !output.is_empty() {
					found = true;
					result.push_str(&output);
				}
			}
			PromptTk::Time24Hr =>{
				let output = expand::expand_time("%H:%M:%S");
				if !output.is_empty() {
					found = true;
					result.push_str(&output);
				}
			}
			PromptTk::Time12Hr =>{
				let output = expand::expand_time("%I:%M:%S");
				if !output.is_empty() {
					found = true;
					result.push_str(&output);
				}
			}
			PromptTk::Time24HrNoSeconds =>{
				let output = expand::expand_time("%H:%M");
				if !output.is_empty() {
					found = true;
					result.push_str(&output);
				}
			}
			PromptTk::Time12HrShort =>{
				let output = expand::expand_time("%I:%M %p");
				if !output.is_empty() {
					found = true;
					result.push_str(&output);
				}
			}
			PromptTk::OctalSeq(octal) => {
				let octal_char = escseq_octal_escape(&mut octal.chars().collect(), '0')?;
				result.push(octal_char);
			}
			PromptTk::AnsiSeq(ansi) => result.push_str(&ansi),
			PromptTk::NonPrint(non_print) => result.push_str(&non_print),
			PromptTk::HideGroupStart => result.push_str(&handle_prompt_hidegroup(tokens)?),
			PromptTk::HideGroupEnd => break,
			PromptTk::WorkingDir =>{
				let output = &escseq_working_directory()?;
				if !output.is_empty() {
					found = true;
					result.push_str(output);
				}
			}
			PromptTk::WorkingDirAbridged =>{
				let output = &escseq_basename_working_directory()?;
				if !output.is_empty() {
					found = true;
					result.push_str(output);
				}
			}
			PromptTk::Hostname =>{
				let output = &escseq_full_hostname()?;
				if !output.is_empty() {
					found = true;
					result.push_str(output);
				}
			}
			PromptTk::HostnameAbridged =>{
				let output = &escseq_short_hostname()?;
				if !output.is_empty() {
					found = true;
					result.push_str(output);
				}
			}
			PromptTk::ShellName =>{
				let output = &escseq_shell_name()?;
				if !output.is_empty() {
					found = true;
					result.push_str(output);
				}
			}
			PromptTk::Username =>{
				let output = &escseq_username()?;
				if !output.is_empty() {
					found = true;
					result.push_str(output);
				}
			}
			PromptTk::PromptSymbol =>{
				let output = escseq_prompt_symbol()?;
				found = true;
				result.push(output);
			}
			PromptTk::ExitSuccess =>{
				let output = &escseq_success()?;
				if !output.is_empty() {
					found = true;
					result.push_str(output);
				}
			}
			PromptTk::ExitFail =>{
				let output = &escseq_fail()?;
				if !output.is_empty() {
					found = true;
					result.push_str(output);
				}
			}
			PromptTk::ExitCode =>{
				let output = &escseq_exitcode()?;
				if !output.is_empty() {
					found = true;
					result.push_str(output);
				}
			}
			PromptTk::CmdTime =>{
				let output = &escseq_cmdtime()?;
				if !output.is_empty() {
					found = true;
					result.push_str(output);
				}
			}
			PromptTk::UserSequence(seq) =>{
				let output = &escseq_custom(&seq)?;
				if !output.is_empty() {
					found = true;
					result.push_str(output);
				}
			}
		}
	}
	if found {
		Ok(result)
	} else {
		Ok(String::new())
	}
}

pub fn format_cmd_runtime(dur: Duration) -> String {
	const ETERNITY: u64 = INFINITY as u64;
	let mut seconds    = dur.as_secs();
	let mut minutes    = 0;
	let mut hours      = 0;
	let mut days       = 0;
	let mut weeks      = 0;
	let mut months     = 0;
	let mut years      = 0;
	let mut decades    = 0;
	let mut centuries  = 0;
	let mut millennia  = 0;
	let mut epochs     = 0;
	let mut aeons      = 0;
	let mut eternities = 0;

	if seconds == 0 {
		let millis = dur.as_millis();
		return format!("{}ms",millis);
	}
	if seconds >= 60 {
		minutes = seconds / 60;
		seconds %= 60;
	}
	if minutes >= 60 {
		hours = minutes / 60;
		minutes %= 60;
	}
	if hours >= 24 {
		days = hours / 24;
		hours %= 24;
	}
	if days >= 7 {
		weeks = days / 7;
		days %= 7;
	}
	if weeks >= 4 {
		months = weeks / 4;
		weeks %= 4;
	}
	if months >= 12 {
		years = months / 12;
		weeks %= 12;
	}
	if years >= 10 {
		decades = years / 10;
		years %= 10;
	}
	if decades >= 10 {
		centuries = decades / 10;
		decades %= 10;
	}
	if centuries >= 10 {
		millennia = centuries / 10;
		centuries %= 10;
	}
	if millennia >= 1000 {
		epochs = millennia / 1000;
		millennia %= 1000;
	}
	if epochs >= 1000 {
		aeons = epochs / 1000;
		epochs %= aeons;
	}
	if aeons >= ETERNITY {
		eternities = aeons / ETERNITY;
		aeons %= ETERNITY;
	}

	// Format the result
	let mut result = Vec::new();
	if eternities > 0 {
		let mut string = format!("{} eternit", eternities);
		if eternities > 1 {
			string.push_str("ies");
		} else {
			string.push('y');
		}
		result.push(string)
	}
	if aeons > 0 {
		let mut string = format!("{} aeon", aeons);
		if aeons > 1 {
			string.push('s')
		}
		result.push(string)
	}
	if epochs > 0 {
		let mut string = format!("{} epoch", epochs);
		if epochs > 1 {
			string.push('s')
		}
		result.push(string)
	}
	if millennia > 0 {
		let mut string = format!("{} millenni", millennia);
		if millennia > 1 {
			string.push_str("um")
		} else {
			string.push('a')
		}
		result.push(string)
	}
	if centuries > 0 {
		let mut string = format!("{} centur", centuries);
		if centuries > 1 {
			string.push_str("ies")
		} else {
			string.push('y')
		}
		result.push(string)
	}
	if decades > 0 {
		let mut string = format!("{} decade", decades);
		if decades > 1 {
			string.push('s')
		}
		result.push(string)
	}
	if years > 0 {
		let mut string = format!("{} year", years);
		if years > 1 {
			string.push('s')
		}
		result.push(string)
	}
	if months > 0 {
		let mut string = format!("{} month", months);
		if months > 1 {
			string.push('s')
		}
		result.push(string)
	}
	if weeks > 0 {
		let mut string = format!("{} week", weeks);
		if weeks > 1 {
			string.push('s')
		}
		result.push(string)
	}
	if days > 0 {
		let mut string = format!("{} day", days);
		if days > 1 {
			string.push('s')
		}
		result.push(string)
	}
	if hours > 0 {
		let string = format!("{}h", hours);
		result.push(string);
	}
	if minutes > 0 {
		let string = format!("{}m", minutes);
		result.push(string);
	}
	if seconds > 0 || result.is_empty() {
		let string = format!("{}s", seconds);
		result.push(string);
	}

	result.join(" ")
}

pub fn escseq_cmdtime() -> OxResult<String> {
	Ok(env::var("OX_CMD_TIME").unwrap_or_default())
}

pub fn escseq_custom(query: &str) -> OxResult<String> {
	let body = read_meta(|m| m.get_shopt(query))?.unwrap_or_default().trim_matches(['(',')']).to_string();
	let subshell = Node {
		command: None,
		nd_type: NdType::CommandSub { body },
		span: Span::new(),
		flags: NdFlags::VALID_OPERAND,
		redirs: VecDeque::new()
	};
	let (r_pipe,w_pipe) = RustFd::pipe()?;
	let io = ProcIO::from(None,Some(w_pipe.mk_shared()),None);
	execute::handle_cmd_sub(subshell, io)?;
	Ok(r_pipe.read()?.trim().to_string())
}

pub fn escseq_exitcode() -> OxResult<String> {
	Ok(read_vars(|v| v.get_param("?"))?.unwrap_or_default())
}

pub fn escseq_success() -> OxResult<String> {
	let code = read_vars(|v| v.get_param("?"))?;
	let success = read_meta(|m| m.get_shopt("prompt.exit_status.success"))??.trim_matches('"').to_string();
	if let Some(code) = code {
		match code.as_str() {
			"0" => Ok(success),
			_ => Ok(String::new()),
		}
	} else {
		Ok(success)
	}
}

pub fn escseq_fail() -> OxResult<String> {
	let code = read_vars(|v| v.get_param("?"))?;
	let failure = read_meta(|m| m.get_shopt("prompt.exit_status.failure"))??.trim_matches('"').to_string();
	if let Some(code) = code {
		match code.as_str() {
			"0" => Ok(String::new()),
			_ => Ok(failure),
		}
	} else {
		Ok(String::new())
	}
}

pub fn escseq_octal_escape(chars: &mut VecDeque<char>, first_digit: char) -> OxResult<char> {
	let mut octal_digits = String::new();
	octal_digits.push(first_digit); // Add the first digit

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
		Ok(value as char)
	} else {
		// Invalid sequence, treat as literal
		Err(ShError::from_internal(&format!("Invalid octal sequence: \\{}", octal_digits)))
	}
}

/// Handles ANSI escape sequences.
pub fn escseq_ansi_escape(chars: &mut VecDeque<char>, result: &mut String) {
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

/// Handles non-printing sequences.
pub fn escseq_non_printing_sequence(chars: &mut VecDeque<char>, result: &mut String) {
	while let Some(ch) = chars.pop_front() {
		if ch == ']' {
			break; // Stop at the closing \]
		}
		result.push(ch); // Add non-printing content
	}
}

/// Handles the current working directory.
pub fn escseq_working_directory() -> OxResult<String> {
	let mut cwd = env::var("PWD").unwrap_or_default();
	let home = env::var("HOME").unwrap_or_default();
	if cwd.starts_with(&home) {
		cwd = cwd.replacen(&home, "~", 1); // Use `replacen` to replace only the first occurrence
	}
	let trunc_len = read_meta(|meta| meta.get_shopt("prompt.trunc_prompt_path").unwrap_or("0".into()))?.parse::<usize>().unwrap();
	if trunc_len > 0 {
		let mut path = PathBuf::from(cwd);
		let mut cwd_components: Vec<_> = path.components().collect();
		if cwd_components.len() > trunc_len {
			cwd_components = cwd_components.split_off(cwd_components.len() - trunc_len);
			path = cwd_components.iter().collect(); // Rebuild the PathBuf
		}
		cwd = path.to_string_lossy().to_string();
	}
	Ok(cwd)
}

/// Handles the basename of the current working directory.
pub fn escseq_basename_working_directory() -> OxResult<String> {
	let cwd = PathBuf::from(read_vars(|vars| vars.get_evar("PWD").map_or("".to_string(), |pwd| pwd.to_string()))?);
	let mut cwd = cwd.components().last().map(|comp| comp.as_os_str().to_string_lossy().to_string()).unwrap_or_default();
	let home = read_vars(|vars| vars.get_evar("HOME").map_or("".into(), |home| home))?;
	if cwd.starts_with(&home) {
		cwd = cwd.replacen(&home, "~", 1); // Replace HOME with '~'
	}
	Ok(cwd)
}

/// Handles the full hostname.
pub fn escseq_full_hostname() -> OxResult<String> {
	let hostname: String = read_vars(|vars| vars.get_evar("HOSTNAME").map_or("unknown host".into(), |host| host))?;
	Ok(hostname)
}

/// Handles the short hostname.
pub fn escseq_short_hostname() -> OxResult<String> {
	let hostname = read_vars(|vars| vars.get_evar("HOSTNAME").map_or("unknown host".into(), |host| host))?;
	if let Some((hostname, _)) = hostname.split_once('.') {
		Ok(hostname.to_string())
	} else {
		Ok(hostname) // No '.' found, use the full hostname
	}
}

/// Handles the shell name.
pub fn escseq_shell_name() -> OxResult<String> {
	let sh_name = read_vars(|vars| vars.get_evar("SHELL").map_or("rsh".into(), |sh| sh))?;
	Ok(sh_name)
}

/// Handles the username.
pub fn escseq_username() -> OxResult<String> {
	let user = read_vars(|vars| vars.get_evar("USER").map_or("unknown".into(), |user| user))?;
	Ok(user)
}

/// Handles the prompt symbol based on the user ID.
pub fn escseq_prompt_symbol() -> OxResult<char> {
	let uid = read_vars(|vars| vars.get_evar("UID").map_or("0".into(), |uid| uid))?;
	match uid.as_str() {
		"0" => Ok('#'),
		_ => Ok('$'),
	}
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

pub fn extract_deck_from_root(node: &Node) -> OxResult<VecDeque<Node>> {
	if let NdType::Root { deck } = &node.nd_type {
		Ok(deck.clone())
	} else {
		Err(ShError::from_internal(format!("Called extract_deck_from_root with non-root node: {:?}", node.nd_type).as_str()))
	}
}

pub fn determine_job_symbol(id: usize, current: Option<&usize>, prev: Option<&usize>) -> &'static str {
	if current.is_some_and(|cur| *cur == id) {
		"+"
	} else if prev.is_some_and(|prev| *prev == id) {
		"-"
	} else {
		" "
	}
}

pub fn format_command_status(i: usize, cmd: &String, job: &Job, init: bool, pids: bool) -> String {
	const GREEN: &str = "\x1b[32m";
	const RED: &str = "\x1b[31m";
	const CYAN: &str = "\x1b[35m";
	const RESET: &str = "\x1b[0m";

	let pid = if pids || init {
		let mut pid = job.get_pids().get(i).unwrap().to_string();
		pid.push(' ');
		pid
	} else {
		"".to_string()
	};

	let mut status0 = if init {
		"".into()
	} else {
		let status = DisplayWaitStatus(*job.get_statuses().get(i).unwrap());
		status.to_string()
	};

	if status0.len() < 6 && !status0.is_empty() {
		let diff = 6 - status0.len();
		let pad = " ".repeat(diff);
		status0.push_str(&pad);
	}

	let status1 = format!("{}{}", pid, status0);
	let status2 = format!("{}\t{}", status1, cmd);
	let status_final = if status0.starts_with("done") {
		format!("{}{}{}", GREEN, status2, RESET)
	} else if status0.starts_with("exit") || status0.starts_with("stopped") {
		format!("{}{}{}", RED, status2, RESET)
	} else {
		format!("{}{}{}", CYAN, status2, RESET)
	};

	if i != job.get_commands().len() - 1 {
		format!("{} |", status_final)
	} else {
		status_final
	}
}

pub fn format_status_line(i: usize, status_final: &str, job: &Job, long: bool, padding: &str) -> String {
	if long {
		format!(
			"{}{} {}",
			if i != 0 { padding } else { "" },
			job.get_pids().get(i).unwrap(),
			status_final
		)
	} else {
		format!(
			"{}{}",
			if i != 0 { padding } else { "" },
			status_final
		)
	}
}

pub fn has_valid_delims(input: &str, open: &str, close: &str) -> bool {
	let mut open_found = false;
	let mut working_buffer = String::new();
	let mut chars = input.chars();

	while let Some(ch) = chars.next() {
		match ch {
			'\\' => {
				if let Some(next) = chars.next() {
					working_buffer.push(next); // Preserve escaped character in buffer
				}
				continue;
			}
			' ' | '\t' | ';' | '\n' => {
				working_buffer.clear();
			}
			_ => {
				working_buffer.push(ch);
				if !open_found && working_buffer.ends_with(open) {
					open_found = true;
					working_buffer.clear();
				}
				if open_found && working_buffer.ends_with(close) {
					return true;
				}
			}
		}
	}
	false
}


pub fn subtract_vars(left: OxVal, right: OxVal, span: Span) -> OxResult<OxVal> {
	match left {
		OxVal::String(left_str) => {
			let right_str = right.to_string();
			if let Some(result) = left_str.strip_suffix(&right_str) {
				Ok(OxVal::String(result.to_string()))
			} else {
				Ok(OxVal::String(left_str))
			}
		}
		OxVal::Int(left_num) => {
			if let OxVal::Int(right_num) = right {
				let diff = left_num - right_num;
				Ok(OxVal::Int(diff))
			} else {
				Err(ShError::from_parse(format!("Tried to subtract non-integer type '{}' from integer", right.fmt_type()).as_str(), span))
			}
		}
		OxVal::Float(left_num) => {
			if let OxVal::Float(right_num) = right {
				let diff = left_num.to_f64() - right_num.to_f64();
				Ok(OxVal::Float(HashFloat::from_f64(diff)))
			} else {
				Err(ShError::from_parse(format!("Tried to subtract non-float type '{}' from float", right.fmt_type()).as_str(), span))
			}
		}
		OxVal::Bool(left_bool) => {
			if let OxVal::Bool(right_bool) = right {
				// Treat as "remove" operation (XOR-like behavior)
				let result = left_bool != right_bool;
				Ok(OxVal::Bool(result))
			} else {
				Err(ShError::from_parse(format!("Tried to subtract non-bool type '{}' from bool", right.fmt_type()).as_str(), span))
			}
		}
		OxVal::Array(mut vec) => {
			if let Some(pos) = vec.iter().position(|elem| *elem == right) {
				vec.remove(pos);
				Ok(OxVal::Array(vec))
			} else {
				Ok(OxVal::Array(vec))
			}
		}
		OxVal::Dict(_) => todo!(),
	}
}

pub fn add_vars(left: OxVal, right: OxVal, span: Span) -> OxResult<OxVal> {
	match left {
		OxVal::String(_) => {
			let left_string = left.to_string();
			let right_string = right.to_string();
			Ok(OxVal::String(format!("{}{}",left_string,right_string)))
		}
		OxVal::Int(left_num) => {
			if let OxVal::Int(right_num) = right {
				let sum = left_num + right_num;
				Ok(OxVal::Int(sum))
			} else {
				Err(ShError::from_parse(format!("Tried to add non-integer type '{}' to integer",right.fmt_type()).as_str(), span))
			}
		}
		OxVal::Float(left_num) => {
			if let OxVal::Float(right_num) = right {
				let sum = left_num.to_f64() + right_num.to_f64();
				Ok(OxVal::Float(HashFloat::from_f64(sum)))
			} else {
				Err(ShError::from_parse(format!("Tried to add non-float type '{}' to float",right.fmt_type()).as_str(), span))
			}
		}
		OxVal::Bool(left_bool) => {
			if let OxVal::Bool(right_bool) = right {
				let result = left_bool | right_bool;
				Ok(OxVal::Bool(result))
			} else {
				Err(ShError::from_parse(format!("Tried to add non-bool type '{}' to bool",right.fmt_type()).as_str(), span))
			}
		}
		OxVal::Array(vec) => {
			let mut new_vec = vec.clone();
			new_vec.push(right);
			Ok(OxVal::Array(new_vec))
		}
		OxVal::Dict(btree_map) => todo!(),
	}
}

pub fn is_brace_expansion(text: &str) -> bool {
	if let Some((_,middle,_)) = text.split_twice("{","}") {
		// Let's not expand patterns like {word}
		if !(middle.contains(',') || middle.contains("..")) {
			return false
		}
	}
	if REGEX["brace_expansion"].is_match(text) &&
		REGEX["brace_expansion"].captures(text).unwrap()[1].is_empty() {
			let mut brace_count: i32 = 0;
			let mut chars = text.chars();
			while let Some(ch) = chars.next() {
				match ch {
					'{' => brace_count += 1,
					'}' => {
						brace_count -= 1;
						if brace_count < 0 {
							// found a closing brace before an open brace
							return false
						}
					},
					'\\' => { chars.next(); },
					_ => { /* Do nothing */ }
				}
			}
			brace_count == 0
		} else {
			false
		}
}
