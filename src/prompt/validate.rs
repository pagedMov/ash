use rustyline::validate::Validator;

use crate::prelude::*;

use super::prompt::SlashHelper;

fn try_parse(input: &str) -> bool {
	SlashParse::parse(Rule::main, input).is_ok()
}

impl<'a> Validator for SlashHelper<'a> {
	fn validate(&self, ctx: &mut rustyline::validate::ValidationContext) -> rustyline::Result<rustyline::validate::ValidationResult> {
	    let input = ctx.input();

			match try_parse(input) {
				true => Ok(rustyline::validate::ValidationResult::Valid(None)),
				false => Ok(rustyline::validate::ValidationResult::Incomplete),
			}
	}
}
