use crate::interp::token::{Tk, WdFlags, WordDesc, CMDSEP, KEYWORDS, REGEX, WHITESPACE};
use log::{debug,trace};
use std::collections::VecDeque;

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
pub fn wspace(c: &char) -> bool {
    WHITESPACE.contains(c)
}
pub fn quoted(wd: &WordDesc) -> bool {
    wd.flags.contains(WdFlags::SNG_QUOTED) || wd.flags.contains(WdFlags::DUB_QUOTED)
}
pub fn finalize_word(word_desc: &WordDesc, tokens: &mut VecDeque<Tk>) -> WordDesc {
    let mut word_desc = word_desc.clone();
    let span = (word_desc.span.1,word_desc.span.1);
    trace!("finalizing word `{}` with flags `{:?}`",word_desc.text,word_desc.flags);
    if !word_desc.text.is_empty() {
        if keywd(&word_desc) && !word_desc.flags.contains(WdFlags::IS_ARG) {
            word_desc = word_desc.add_flag(WdFlags::KEYWORD);
        }
        if word_desc.flags.contains(WdFlags::EXPECT_IN) && matches!(word_desc.text.as_str(), "in") {
            debug!("setting in flag to keyword");
            word_desc = word_desc.remove_flag(WdFlags::IS_ARG);
            word_desc = word_desc.add_flag(WdFlags::KEYWORD);
        }
        tokens.push_back(Tk::from(word_desc));
    }

    // Always return a fresh WordDesc with reset state
    WordDesc {
        text: String::new(),
        span,
        flags: WdFlags::empty(),
    }
}
