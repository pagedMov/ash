use crossterm::style::{style, Color, Stylize};
use rustyline::{hint::{Hint, Hinter}, Context};

use crate::prelude::*;

use super::prompt::SlashHelper;

pub struct SlashHint {
	text: String,
	styled_text: String
}

impl SlashHint {
	pub fn new(text: String) -> Self {
		let styled_text = style(&text).with(Color::DarkGrey).to_string();
		Self { text, styled_text }
	}
}

impl Hint for SlashHint {
	fn display(&self) -> &str {
		&self.styled_text
	}
	fn completion(&self) -> Option<&str> {
		if !self.text.is_empty() {
			Some(&self.text)
		} else {
			None
		}
	}
}

impl<'a> Hinter for SlashHelper<'a> {
	fn hint(&self, line: &str, pos: usize, ctx: &Context<'_>) -> Option<Self::Hint> {
		if line.is_empty() {
			return None
		}
		let history = ctx.history();
		let result = self.hist_substr_search(line, history);
		if let Some(hist_line) = result {
			let window = hist_line[line.len()..].to_string();
			let hint = SlashHint::new(window);
			Some(hint)
		} else {
			None
		}
	}

	type Hint = SlashHint;
}
