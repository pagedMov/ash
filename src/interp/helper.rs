use crate::interp::token::{Tk, WdFlags, WordDesc, CMDSEP, KEYWORDS, BUILTINS, REGEX, WHITESPACE};
use log::{debug,trace};
use std::collections::VecDeque;

use super::parse::RshErr;

pub fn get_delimiter(wd: &WordDesc) -> char {
    let flags = wd.flags;
    match () {
        _ if flags.contains(WdFlags::IN_BRACKET) => ']',
        _ if flags.contains(WdFlags::IN_BRACE) => '}',
        _ if flags.contains(WdFlags::IN_PAREN) => ')',
        _ => unreachable!("No active delimiter found in WordDesc flags"),
    }
}
pub fn is_brace_expansion(token: &Tk) -> bool {
    REGEX["brace_expansion"].is_match(token.text())
    && REGEX["brace_expansion"].captures(token.text()).unwrap()[1].is_empty()
}
pub fn delimited(wd: &WordDesc) -> bool {
    wd.flags.contains(WdFlags::IN_BRACKET) ||
    wd.flags.contains(WdFlags::IN_BRACE) ||
    wd.flags.contains(WdFlags::IN_PAREN)
}
pub fn cmdsep(c: &char) -> bool {
    CMDSEP.contains(c)
}
pub fn keywd(wd: &WordDesc) -> bool {
    KEYWORDS.contains(&wd.text.as_str()) && !wd.flags.contains(WdFlags::IS_ARG)
}
pub fn builtin(wd: &WordDesc) -> bool {
    BUILTINS.contains(&wd.text.as_str()) && !wd.flags.contains(WdFlags::IS_ARG)
}
pub fn wspace(c: &char) -> bool {
    WHITESPACE.contains(c)
}
pub fn quoted(wd: &WordDesc) -> bool {
    wd.flags.contains(WdFlags::SNG_QUOTED) || wd.flags.contains(WdFlags::DUB_QUOTED)
}
pub fn clean_var_sub(wd: WordDesc) -> WordDesc {
	let mut text = wd.text.clone();
	text = text.strip_prefix('$').unwrap().to_string();
	if text.starts_with('{') && text.ends_with('}') {
		text.strip_prefix('{').unwrap();
		text.strip_prefix('}').unwrap();
	}
	WordDesc {
		text,
		span: wd.span,
		flags: wd.flags
	}
}
pub fn finalize_word(word_desc: &WordDesc, tokens: &mut VecDeque<Tk>) -> Result<WordDesc,RshErr> {
    let mut word_desc = word_desc.clone();
    let span = (word_desc.span.1,word_desc.span.1);
    trace!("finalizing word `{}` with flags `{:?}`",word_desc.text,word_desc.flags);
    if !word_desc.text.is_empty() {
        if keywd(&word_desc) {
            word_desc = word_desc.add_flag(WdFlags::KEYWORD);
        } else if builtin(&word_desc) {
            word_desc = word_desc.add_flag(WdFlags::BUILTIN);
        }
        if word_desc.flags.contains(WdFlags::EXPECT_IN) && matches!(word_desc.text.as_str(), "in") {
            debug!("setting in flag to keyword");
            word_desc = word_desc.remove_flag(WdFlags::IS_ARG);
            word_desc = word_desc.add_flag(WdFlags::KEYWORD);
        }
        tokens.push_back(Tk::from(word_desc)?);
    }

    // Always return a fresh WordDesc with reset state
    Ok(WordDesc {
        text: String::new(),
        span,
        flags: WdFlags::empty(),
    })
}
