use crate::{helper, prelude::*};

pub fn expand_tilde(pair: Pair<Rule>) -> SlashResult<String> {
	debug_assert!(pair.as_rule() == Rule::tilde_sub, "Found this: {:?}",pair.as_rule());
	let word = pair.as_str();
	dbg!(&word);
	if !word.starts_with('~') {
		dbg!(&word);
		if pair.as_rule() == Rule::arg_assign {
			let value = pair.scry(Rule::word).unpack()?;
			dbg!(&value);
			if !value.as_str().starts_with('~') {
				return Ok(word.to_string())
			}
		} else {
			return Ok(word.to_string())
		}
	}
	let home = env::var("HOME").unwrap_or_default();
	Ok(word.replacen("~", &home, 1))
}

pub fn expand_shebang(slash: &mut Slash,shebang: &str) -> String {
	let mut command = shebang.trim_start_matches("#!").trim().to_string();
	if command.has_unescaped("/") {
		return format!("{}{command}{}","#!","\n");
	}
	if let Some(path) = helper::which(slash,&command) {
		return format!("{}{path}{}","#!","\n");
	} else {
		return shebang.to_string()
	}
}

pub fn expand_prompt(input: Option<&str>,slash: &mut Slash) -> SlashResult<String> {
	let mut prompt = slash.vars().get_evar("PS1").unwrap_or_default();
	prompt = prompt.replace("\n", "");
	let mut result = prompt.clone();
	let mut prompt_parse = SlashParse::parse(Rule::prompt, &prompt)
		.map_err(|e| Low(SlashErrLow::Parse(e.to_string())))?
		.into_iter()
		.next()
		.unpack()?
		.into_inner()
		.rev();

	while let Some(esc) = prompt_parse.next() {
		let span = esc.as_span();
		let expanded = expand_esc(slash,esc)?;
		result.replace_span( span, &expanded);
	}
	Ok(result)
}

pub fn expand_time(fmt: &str) -> String {
	let right_here_right_now = chrono::Local::now();
	right_here_right_now.format(fmt).to_string()
}

pub fn expand_esc<'a>(slash: &mut Slash,pair: Pair<'a,Rule>) -> SlashResult<String> {
	let pair = pair.step(1).unpack()?;
	Ok(match pair.as_rule() {
		Rule::esc_bell => "\x07".into(),
		Rule::esc_dquote => "\"".into(),
		Rule::esc_squote => "'".into(),
		Rule::esc_return => "\r".into(),
		Rule::esc_newline => "\n".into(),
		Rule::esc_vis_grp => helper::handle_prompt_visgroup(slash,pair)?,
		Rule::esc_user_seq => {
			let query = pair.step(1).unpack()?.as_str();
			helper::escseq_custom(slash,query)?
		}
		Rule::esc_ascii_oct => {
			let octal = pair.step(1).unpack()?.as_str();
			if let Ok(val) = u8::from_str_radix(octal, 8) {
				(val as char).to_string()
			} else {
				return Err(Low(SlashErrLow::InternalErr("from_str_radix() failed in prompt expansion".into())))
			}
		}
		Rule::esc_ansi_seq  => {
			let params = pair.step(1).unpack()?.as_str();
			format!("\x1B[{params}m")
		}
		Rule::esc_12hour_short => expand_time("%I:%M %p"),
		Rule::esc_24hour_short => expand_time("%H:%M"),
		Rule::esc_runtime => helper::escseq_cmdtime()?,
		Rule::esc_12hour => expand_time("%I:%M:%S"),
		Rule::esc_24hour => expand_time("%H:%M:%S"),
		Rule::esc_weekday => expand_time("%a %b %d"),
		Rule::esc_pwd => helper::escseq_working_directory(slash)?,
		Rule::esc_pwd_short => helper::escseq_basename_working_directory(slash)?,
		Rule::esc_hostname => helper::escseq_full_hostname(slash)?,
		Rule::esc_hostname_short => helper::escseq_short_hostname(slash)?,
		Rule::esc_shellname => helper::escseq_shell_name(slash)?,
		Rule::esc_username => helper::escseq_username(slash)?,
		Rule::esc_prompt_symbol => helper::escseq_prompt_symbol(slash)?.to_string(),
		Rule::esc_exit_code => helper::escseq_exitcode(slash)?,
		Rule::esc_success_symbol => helper::escseq_success(slash)?,
		Rule::esc_failure_symbol => helper::escseq_fail(slash)?,
		_ => unreachable!("Got this rule in prompt expansion: {:?}",pair.as_rule())
	})
}
