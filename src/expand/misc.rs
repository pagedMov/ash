use crate::{helper, prelude::*};

pub fn expand_tilde(pair: Pair<Rule>) -> LashResult<String> {
	debug_assert!(pair.as_rule() == Rule::tilde_sub, "Found this: {:?}",pair.as_rule());
	let word = pair.as_str();
	let home = env::var("HOME").unwrap_or_default();
	Ok(word.replacen("~", &home, 1))
}

pub fn expand_shebang(lash: &mut Lash,shebang: &str) -> String {
	let mut command = shebang.trim_start_matches("#!").trim().to_string();
	if command.has_unescaped("/") {
		return format!("{}{command}{}","#!","\n");
	}
	if let Some(path) = helper::which(lash,&command) {
		return format!("{}{path}{}","#!","\n");
	} else {
		return shebang.to_string()
	}
}

pub fn expand_prompt(input: Option<&str>,lash: &mut Lash) -> LashResult<String> {
	let mut prompt = lash.vars().get_evar("PS1").unwrap_or_default();
	prompt = prompt.replace("\n", "");
	let mut result = prompt.clone();
	let mut prompt_parse = LashParse::parse(Rule::prompt, &prompt)
		.map_err(|e| Low(LashErrLow::Parse(e.to_string())))?
		.into_iter()
		.next()
		.unpack()?
		.into_inner()
		.rev();

	while let Some(esc) = prompt_parse.next() {
		let span = esc.as_span();
		let expanded = expand_esc(lash,esc)?;
		result.replace_span( span, &expanded);
	}
	Ok(result)
}

pub fn expand_time(fmt: &str) -> String {
	let right_here_right_now = chrono::Local::now();
	right_here_right_now.format(fmt).to_string()
}

pub fn expand_esc<'a>(lash: &mut Lash,pair: Pair<'a,Rule>) -> LashResult<String> {
	let pair = pair.step(1).unpack()?;
	Ok(match pair.as_rule() {
		Rule::esc_bell => "\x07".into(),
		Rule::esc_dquote => "\"".into(),
		Rule::esc_squote => "'".into(),
		Rule::esc_return => "\r".into(),
		Rule::esc_newline => "\n".into(),
		Rule::esc_vis_grp => helper::handle_prompt_visgroup(lash,pair)?,
		Rule::esc_user_seq => {
			let query = pair.step(1).unpack()?.as_str();
			helper::escseq_custom(lash,query)?
		}
		Rule::esc_ascii_oct => {
			let octal = pair.step(1).unpack()?.as_str();
			if let Ok(val) = u8::from_str_radix(octal, 8) {
				(val as char).to_string()
			} else {
				return Err(Low(LashErrLow::InternalErr("from_str_radix() failed in prompt expansion".into())))
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
		Rule::esc_pwd => helper::escseq_working_directory(lash)?,
		Rule::esc_pwd_short => helper::escseq_basename_working_directory(lash)?,
		Rule::esc_hostname => helper::escseq_full_hostname(lash)?,
		Rule::esc_hostname_short => helper::escseq_short_hostname(lash)?,
		Rule::esc_shellname => helper::escseq_shell_name(lash)?,
		Rule::esc_username => helper::escseq_username(lash)?,
		Rule::esc_prompt_symbol => helper::escseq_prompt_symbol(lash)?.to_string(),
		Rule::esc_exit_code => helper::escseq_exitcode(lash)?,
		Rule::esc_success_symbol => helper::escseq_success(lash)?,
		Rule::esc_failure_symbol => helper::escseq_fail(lash)?,
		_ => unreachable!("Got this rule in prompt expansion: {:?}",pair.as_rule())
	})
}
