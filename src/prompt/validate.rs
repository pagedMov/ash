use rustyline::validate::Validator;

use crate::prelude::*;

use super::prompt::LashHelper;

fn balance_delims(haystack: &str) -> bool {
	let open = ['{', '[', '('];
	let close = ['}', ']', ')'];
	let matches: HashMap<_, _> = close.into_iter().zip(open.into_iter()).collect();
	let mut quoted = false;

	let mut stack = vec![];
	let mut chars = haystack.chars();

	while let Some(ch) = chars.next() {
		match ch {
			'\\' => {
				chars.next(); // Skip next char (escaped character)
			}
			'{' | '(' | '[' => stack.push(ch),
			'"' | '\'' => {
				quoted = true;
				while let Some(qt_ch) = chars.next() {
					if qt_ch == ch {
						quoted = false;
						break; // End quoted string
					}
				}
			}
			'}' | ')' | ']' => {
				let pop_char = stack.pop();
				if pop_char != matches.get(&ch).copied() {
					return false; // Mismatch or extra closing delimiter
				}
			}
			_ => {}
		}
	}

	stack.is_empty() && !quoted // Ensure no leftover unclosed delimiters
}

fn balance_keywords(haystack: &str) -> bool {
	let mut stack: Vec<&str> = vec![];
	let open = ["if", "for", "while", "until", "match", "select"];
	let close = ["fi", "done", "done", "done", "done", "done"];
	let mut is_cmd = true;

	let matches: HashMap<&str, &str> = open.into_iter().zip(close.into_iter()).collect();

	let mut working_buffer = String::new();
	let mut chars = haystack.chars().peekable();

	while let Some(ch) = chars.next() {
		working_buffer.push(ch);

		// Handle escaped characters
		if ch == '\\' {
			if let Some(ch) = chars.next() {
				working_buffer.push(ch);
			}
			continue;
		}

		// Handle string literals
		if ch == '\'' || ch == '"' {
			while let Some(qt_ch) = chars.next() {
				working_buffer.push(qt_ch);
				if qt_ch == ch {
					break;
				}
			}
			continue;
		}

		// Detect new command context
		match ch {
			';' | '\n' | '(' => is_cmd = true, // '(' starts a subshell
			'|' => {
				// Check for `||` or `|`
				if matches!(chars.peek(), Some('|')) {
					let ch = chars.next().unwrap();
					working_buffer.push(ch)
				}
				is_cmd = true;
			}
			'&' => {
				// Check for `&&`
				if matches!(chars.peek(), Some('&')) {
					let ch = chars.next().unwrap();
					working_buffer.push(ch)
				}
				is_cmd = true;
			}
			' ' | '\t' => is_cmd = false,
			_ => { /* Do nothing */ }
		}

		// Handle opening keywords
		for &opener in &open {
			if is_cmd && working_buffer.ends_with(opener) {
				if matches!(chars.peek(), Some(' ' | '\t' | '\n' | ';')) {
					stack.push(opener);
				}
			}
		}

		// Handle closing keywords
		for &closer in &close {
			if is_cmd && working_buffer.ends_with(closer) {
				if matches!(chars.peek(), Some(' ' | '\t' | '\n' | ';')) {
					let opener = stack.pop();
					let expected_opener = matches.iter().find(|(_, &v)| v == closer).map(|(&k, _)| k);

					if opener != expected_opener {
						return false;
					}
				}
			}
		}
	}

	stack.is_empty() // Ensure all openers have been closed
}

impl<'a> Validator for LashHelper<'a> {
	fn validate(&self, ctx: &mut rustyline::validate::ValidationContext) -> rustyline::Result<rustyline::validate::ValidationResult> {
	    let input = ctx.input();

			match balance_delims(input) && balance_keywords(input) {
				true => Ok(rustyline::validate::ValidationResult::Valid(None)),
				false => Ok(rustyline::validate::ValidationResult::Incomplete),
			}
	}
}
