use bitflags::bitflags;
use once_cell::sync::Lazy;
use log::{trace,debug};
use regex::Regex;
use std::collections::HashMap;
use im::Vector;
use crate::interp::parse::ParseState;
use crate::interp::helper;

pub const KEYWORDS: [&str;14] = [
    "if", "while", "until", "for", "case", "select",
    "then", "elif", "else", "in",
    "do", "done", "fi", "esac"
];
pub const BUILTINS: [&str; 14] = [
    "echo", "set", "shift", "export", "cd", "readonly", "declare", "local", "unset", "trap", "node",
    "exec", "source", "wait",
];
pub const FUNCTIONS: [&str; 1] = [
    // Will replace this with an actual functions implementation later
    // Used for now for word flags
    "PLACEHOLDER_TEXT",
];
pub const ALIASES: [&str; 1] = [
    // Will replace this with an actual aliases implementation later
    // Used for now for word flags
    "PLACEHOLDER_TEXT",
];
pub const CMDSEP: [char;2] = [
    ';', '\n'
];
pub const WHITESPACE: [char;2] = [
    ' ', '\t'
];

bitflags! {
    #[derive(Debug,Clone,Copy,PartialEq)]
    pub struct FnFlags: u32 {
        const RECURSE = 0b0001;
    }
    #[derive(Debug,Clone,Copy,PartialEq)]
    pub struct WdFlags: u32 {
        const KEYWORD =    0b00000000000001;
        const BUILTIN =    0b00000000000010;
        const FUNCTION =   0b00000000000100;
        const ALIAS =      0b00000000001000;
        const IS_ARG =     0b00000000010000;
        const DUB_QUOTED = 0b00000000100000;
        const SNG_QUOTED = 0b00000001000000;
        const IN_BRACE =   0b00000010000000;
        const IN_BRACKET = 0b00000100000000;
        const IN_PAREN =   0b00001000000000;
        const IS_SUB =     0b00010000000000;
        const IS_OP =      0b00100000000000;
        const IN_CASE1 =   0b01000000000000; // After keyword, i.e. 'case var in'
        const IN_CASE2 =   0b10000000000000; // After 'in', i.e pattern)block;;pattern)block;;
    }
}

macro_rules! define_patterns {
    ($($name:expr => $pattern:expr),* $(,)?) => {{
        let mut m = HashMap::new();
        $(m.insert($name, Regex::new($pattern).unwrap());)*
        m
    }};
}

macro_rules! match_token {
    ($word_desc:expr, $($key:expr => $token:expr,)* _ => $default:expr $(,)?) => {
        {
            let text = &$word_desc.text;
            if false { unreachable!() } // Ensures the chain starts cleanly
            $(
                else if REGEX[$key].is_match(text) {
                    $token
                }
            )*
            else {
                $default
            }
        }
    };
    ($word_desc:expr, $($key:expr => $token:expr,)* $(,)?) => {
        {
            let text = &$word_desc.text;
            if false { unreachable!() }
            $(
                else if REGEX[$key].is_match(text) {
                    $token
                }
            )*
            else {
                panic!("Unrecognized token: {}", text);
            }
        }
    };
}

pub static REGEX: Lazy<HashMap<&'static str, Regex>> = Lazy::new(|| {
    define_patterns! {
        "redirection" => r"^([0-9]+)?(>{1,2}|<{1,3})(?:[&]?([0-9]+))?$",
        "rsh_shebang" => r"^#!((?:/[^\s]+)+)((?:\s+arg:[a-zA-Z][a-zA-Z0-9_\-]*)*)$",
        "brace_expansion" => r"(\$?)\{(?:[\x20-\x7e,]+|[0-9]+(?:\.\.[0-9+]){1,2}|[a-z]\.\.[a-z]|[A-Z]\.\.[A-Z])\}",
        "process_sub" => r"^>\(.*\)$",
        "command_sub" => r"^\$\([^\)]+\)$",
        "arithmetic" => r"^\$\(\([^\)]+\)\)$",
        "subshell" => r"^\([^\)]+\)$",
        "test" => r"^\[\s*(.*?)\s*\]$",
        "string" => r#"^\"([^\"]*)\"$"#,
        "var_sub" => r"\$(?:([A-Za-z_][A-Za-z0-9_]*)|\{([A-Za-z_][A-Za-z0-9_]*)\})",
        "assignment" => r"^[A-Za-z_][A-Za-z0-9_]*=.*$",
        "operator" => r"(?:&&|\|\||[><]=?|[|&])",
        "cmdsep" => r"^(?:\n|;)$",
        "ident" => r"^[\x20-\x7E]*$",
    }
});

#[derive(Debug,Clone,PartialEq)]
pub struct Tk {
    pub tk_type: TkType,
    pub wd: WordDesc
}

#[derive(Debug,Clone,PartialEq)]
pub enum TkType {
    // Control Flow Keywords
    If,
    Then,
    Else,
    Elif,
    Fi,
    For,
    While,
    Until,
    Do,
    Done,
    Case,
    Esac,
    Select,
    In,
    Function,

    Redirection { redir: Redir },
    Assignment, // `=`
    LogicAnd, // `&&`
    LogicOr, // `||`
    Pipe, // `|`
    Background, // `&`

    // Grouping and Subshells
    ProcessSub,
    Subshell, // `(`
    BraceGroupStart, // `{`
    BraceGroupEnd,   // `}`

    // Arrays
    ArrayStart, // `(` inside array declarations
    ArrayEnd,   // `)`

    // Strings and Identifiers
    String, // Generic string literal
    Ident,  // Identifier for variables, functions, etc.

    // Expansions
    BraceExpansion,
    VariableSub, // `$var`, `${var}`
    CommandSub, // `$(command)`
    ArithmeticSub, // `$((expression))`

    // Comments
    Comment, // `#`

    // Special Characters
    Cmdsep, // ';' or '\n'
    CaseSep, // ')'
    CaseDelim, // ';;'
    Whitespace, // Space or tab
    SOI,
    EOI,

    // Misc
    Shebang
}

impl Tk {
    pub fn start_of_input() -> Self {
        Tk {
            tk_type: TkType::SOI,
            wd: WordDesc { text: "".into(), span: (0,0), flags: WdFlags::empty() }
        }
    }
    pub fn end_of_input() -> Self {
        Tk {
            tk_type: TkType::EOI,
            wd: WordDesc { text: "".into(), span: (0,0), flags: WdFlags::empty() }
        }
    }
    pub fn cmdsep(pos: usize) -> Self {
        Tk {
            tk_type: TkType::Cmdsep,
            wd: WordDesc { text: ";".into(), span: (pos,pos), flags: WdFlags::empty() }
        }
    }
    pub fn casesep(pos: usize) -> Self {
        Tk {
            tk_type: TkType::CaseSep,
            wd: WordDesc { text: ")".into(), span: (pos,pos), flags: WdFlags::empty() }
        }
    }
    pub fn case_delim(pos: usize) -> Self {
        Tk {
            tk_type: TkType::CaseDelim,
            wd: WordDesc { text: ";;".into(), span: (pos,pos+1), flags: WdFlags::empty() }
        }
    }
    pub fn from(mut wd: WordDesc) -> Self {
        debug!("Tk::from(): Evaluating node type for: {}", wd.text);

        // TODO: Implement sub-shell substitutions
        // These must be evaluated here, and resolved into a string node containing their output
        let text = wd.text.as_str();
        let tk_type = match text {
            _ if wd.flags.contains(WdFlags::KEYWORD) => {
                Self::get_keyword_token(&wd)
            }
            _ if REGEX["assignment"].is_match(text) => {
                trace!("Matched assignment: {}", text);
                TkType::Assignment
            },
            _ if REGEX["redirection"].is_match(text) => {
                trace!("Matched redirection: {}", text);
                Self::build_redir(&wd)
            },
            _ if REGEX["process_sub"].is_match(text) => {
                trace!("Matched process substitution: {}", text);
                wd = wd.set_flags(|f| f | WdFlags::IS_SUB);
                TkType::ProcessSub
            },
            _ if REGEX["command_sub"].is_match(text) => {
                trace!("Matched command substitution: {}", text);
                wd = wd.set_flags(|f| f | WdFlags::IS_SUB);
                TkType::CommandSub
            },
            _ if REGEX["arithmetic"].is_match(text) => {
                trace!("Matched arithmetic substitution: {}", text);
                wd = wd.set_flags(|f| f | WdFlags::IS_SUB);
                TkType::ArithmeticSub
            },
            _ if REGEX["var_sub"].is_match(text) => {
                trace!("Matched variable substitution: {}", text);
                wd = wd.set_flags(|f| f | WdFlags::IS_SUB);
                TkType::VariableSub
            },
            _ if REGEX["rsh_shebang"].is_match(text) => {
                trace!("Matched rsh_shebang: {}", text);
                TkType::Shebang
            },
            _ if REGEX["subshell"].is_match(text) => {
                trace!("Matched subshell: {}", text);
                TkType::Subshell
            },
            _ if REGEX["string"].is_match(text) => {
                trace!("Matched string: {}", text);
                TkType::String
            },
            _ if REGEX["operator"].is_match(text) => {
                trace!("Matched operator: {}", text);
                Self::get_operator_type(&wd)
            },
            _ if REGEX["ident"].is_match(text) => {
                trace!("Matched identifier: {}", text);
                TkType::Ident
            },
            _ => {
                trace!("Failed to classify text: {}", wd.text);
                panic!("failed to classify this: {}", wd.text)
            }
        };
        Tk { tk_type, wd }
    }
    fn get_keyword_token(wd: &WordDesc) -> TkType {
        let text = wd.text.clone();
        match text.as_str() {
            "if" => TkType::If,
            "elif" => TkType::Elif,
            "else" => TkType::Else,
            "then" => TkType::Then,
            "fi" => TkType::Fi,
            "for" => TkType::For,
            "while" => TkType::While,
            "until" => TkType::Until,
            "do" => TkType::Do,
            "done" => TkType::Done,
            "case" => TkType::Case,
            "esac" => TkType::Esac,
            "select" => TkType::Select,
            "in" => TkType::In,
            _ => panic!("Unrecognized keyword: {}", text),
        }
    }
    fn build_redir(wd: &WordDesc) -> TkType {
        let text = wd.text.clone();
        if let Some(caps) = REGEX["redirection"].captures(text.as_str()) {
            let fd_out = caps.get(1).and_then(|m| m.as_str().parse::<usize>().ok()).unwrap_or(1);
            let operator = caps.get(2).map(|m| m.as_str()).unwrap_or_default();
            let fd_target = caps.get(3).and_then(|m| m.as_str().parse::<usize>().ok());

            let redir_type = match operator {
                ">" => RedirType::Output,
                ">>" => RedirType::Append,
                "<" => RedirType::Input,
                "<<" => RedirType::Heredoc,
                "<<<" => RedirType::Herestring,
                _ => panic!("invalid redirection operator: {}",operator)
            };
            let redir = Redir {
                fd_out,
                op: redir_type,
                fd_target,
                file_target: None // We will do this part in the parsing phase
            };
            TkType::Redirection { redir }
        } else { unreachable!() }
    }
    fn get_operator_type(word_desc: &WordDesc) -> TkType {
        match word_desc.text.as_str() {
            "&" => TkType::Background,
            "&&" => TkType::LogicAnd,
            "|" => TkType::Pipe,
            "||" => TkType::LogicOr,
            _ => unreachable!()
        }
    }
    pub fn text(&self) -> &str {
        self.wd.text.as_str()
    }
    pub fn span(&self) -> (usize,usize) {
        self.wd.span
    }
    pub fn class(&self) -> TkType {
        self.tk_type.clone()
    }
    pub fn flags(&self) -> WdFlags {
        self.wd.flags
    }
}


#[derive(Debug,Clone,PartialEq)]
pub struct Redir {
    pub fd_out: usize,
    pub op: RedirType,
    pub fd_target: Option<usize>,
    pub file_target: Option<Box<Tk>>
}

#[derive(Debug,Clone,PartialEq)]
pub enum RedirType {
    Output,
    Append,
    Input,
    Heredoc,
    Herestring
}

#[derive(Debug,Clone,PartialEq)]
pub struct WordDesc {
    pub text: String,
    pub span: (usize,usize),
    pub flags: WdFlags
}

impl WordDesc {
    pub fn add_char(&self, c: char) -> Self {
        let mut text = self.text.clone();
        text.push(c);

        Self {
            text,
            span: (self.span.0, self.span.1 + 1),
            flags: self.flags,
        }
    }
    pub fn cat_string(&self, s: &str) -> Self {
        let mut text = self.text.clone();
        text.push_str(s);
        Self {
            text,
            span: (self.span.0, self.span.1 + 1),
            flags: self.flags
        }
    }
    pub fn set_flags<F>(&self, expr: F) -> Self
    where
        F: FnOnce(WdFlags) -> WdFlags,
    {
        let flags = expr(self.flags);
        let text = self.text.clone();
        let span = self.span;
        Self {
            text,
            span,
            flags
        }
    }
    pub fn reset_flags(&self) -> Self {
        Self {
            text: self.text.clone(),
            span: self.span,
            flags: WdFlags::empty()
        }
    }
    pub fn push_span(&self, count: usize) -> Self {
        let text = self.text.clone();
        let span = (self.span.0,self.span.1 + count);
        let flags = self.flags;
        Self { text, span, flags }
    }
    pub fn delimit(&self, delim: char) -> Self {
        let flag = match delim {
            '{' => WdFlags::IN_BRACE,
            '[' => WdFlags::IN_BRACKET,
            '(' => WdFlags::IN_PAREN,
            _ => unreachable!()
        };
        self.reset_flags().set_flags(|f| f | flag)
    }
}

pub fn test_redirection(c: char, mut chars: Vector<char>, mut word_desc: WordDesc) -> Option<(Vector<char>,WordDesc)> {
    debug!("nodeize(): Maybe found redirection?");
    let mut redir_test_stack = chars.clone();
    let mut redir_test_string: String = c.into();
    let mut count = 0;

    while let Some(check_char) = redir_test_stack.pop_front() {
        debug!( "nodeize(): Checking character for redir test: {}", check_char);
        trace!("current string: {}",redir_test_string);
        if matches!(check_char, '&' | '0'..='9' | '>' | '<') {
            debug!("nodeize(): Adding checked character: {}", check_char);
            redir_test_string.push(check_char);
            count += 1
        } else {
            break
        }
    }

    if REGEX["redirection"].is_match(redir_test_string.as_str()) {
        debug!("nodeize(): redirection found, constructing...");
        word_desc = word_desc.add_char(c);
        while count != 0 {

            let redir_char = chars.pop_front().unwrap();
            debug!("nodeize(): count = {}, adding \'{}\'", count, redir_char);
            word_desc = word_desc.add_char(redir_char);
            count -= 1
        }
        Some((chars,word_desc))
    } else { None }
}

pub fn tokenize(state: ParseState) -> ParseState {
    debug!("Starting tokenization with input: {:?}", state.input);

    let mut word_desc = WordDesc {
        text: String::new(),
        span: (0, 0),
        flags: WdFlags::empty(),
    };
    let mut chars = state.input.chars().collect::<Vector<char>>();
    let mut tokens: Vector<Tk> = Vector::from(vec![Tk::start_of_input()]);
    let mut is_arg = false; // Start in "command mode" since SOI implies a command
    trace!("Initialized state: word_desc: {:?}, tokens: {:?}", word_desc, tokens);

    while let Some(c) = chars.pop_front() {
        trace!("Processing character: {:?}", c);
        if matches!(c,'&' | '0'..='9' | '>' | '<') { // Test for redirection
            trace!("got something that looks like a redirection: {}",c);
            if let Some(data) = test_redirection(c, chars.clone(), word_desc.clone()) {
                chars = data.0; // Update chars and word_desc if test is successful
                word_desc = data.1;
                continue
            }
        }
        word_desc = match c {
            _ if helper::delimited(&word_desc) => {
                let closer = helper::get_delimiter(&word_desc);
                if c != closer {
                    word_desc.add_char(c)
                } else if c == '\\' {
                    let word_desc = word_desc.add_char(c);
                    if let Some(ch) = chars.pop_front() {
                        trace!("Adding escaped character: {:?}", ch);
                        word_desc.add_char(ch)
                    } else {
                        trace!("No character after escape, returning unchanged word_desc");
                        word_desc
                    }
                } else {
                    trace!("Finalizing delimited word");
                    if is_arg {
                        trace!("Current word is a command; resetting IS_ARG flag");
                        word_desc = word_desc.set_flags(|f| f | WdFlags::IS_ARG);
                    }
                    word_desc = word_desc.add_char(c);
                    match word_desc {
                        _ if word_desc.flags.contains(WdFlags::IN_BRACE) => word_desc.set_flags(|f| f & !WdFlags::IN_BRACE),
                        _ if word_desc.flags.contains(WdFlags::IN_PAREN) => word_desc.set_flags(|f| f & !WdFlags::IN_PAREN),
                        _ if word_desc.flags.contains(WdFlags::IN_BRACKET) => word_desc.set_flags(|f| f & !WdFlags::IN_BRACKET),
                        _ => unreachable!()
                    }
                }
            }
            '(' | '{' | '[' => {
                word_desc = word_desc.add_char(c);
                word_desc.delimit(c)
            }
            _ if helper::quoted(&word_desc) => {
                trace!("Inside quoted context: {:?}", word_desc.flags);
                match c {
                    '"' if !word_desc.flags.contains(WdFlags::SNG_QUOTED) => {
                        trace!("Closing double quote found");
                        let word_desc = helper::finalize_word(&word_desc, &mut tokens);
                        is_arg = true; // After a quote, it's part of a command argument
                        word_desc
                    }
                    '\'' if !word_desc.flags.contains(WdFlags::DUB_QUOTED) => {
                        trace!("Closing single quote found");
                        let word_desc = helper::finalize_word(&word_desc, &mut tokens);
                        is_arg = true;
                        word_desc
                    }
                    '\\' => {
                        trace!("Escape character found inside quoted context");
                        let word_desc = word_desc.add_char(c);
                        if let Some(ch) = chars.pop_front() {
                            trace!("Adding escaped character: {:?}", ch);
                            word_desc.add_char(ch)
                        } else {
                            trace!("No character after escape, returning unchanged word_desc");
                            word_desc
                        }
                    }
                    '$' if !word_desc.flags.contains(WdFlags::SNG_QUOTED) => {
                        trace!("Substitution found inside double quotes");
                        word_desc.set_flags(|f| f | WdFlags::IS_SUB).add_char(c)
                    }
                    _ => {
                        trace!("Adding character {:?} to quoted word", c);
                        word_desc.add_char(c)
                    }
                }
            }
            '\\' => {
                trace!("Escape character found");
                let word_desc = word_desc.add_char(c);
                if let Some(next_c) = chars.pop_front() {
                    word_desc.add_char(next_c)
                } else {
                    word_desc
                }
            }
            '"' => {
                if is_arg {
                    word_desc = word_desc.set_flags(|f| f | WdFlags::IS_ARG);
                }
                trace!("Double quote found, toggling DUB_QUOTED flag");
                word_desc.set_flags(|f| f | WdFlags::DUB_QUOTED).push_span(1)
            }
            '\'' => {
                if is_arg {
                    word_desc = word_desc.set_flags(|f| f | WdFlags::IS_ARG);
                }
                trace!("Single quote found, toggling SNG_QUOTED flag");
                word_desc.set_flags(|f| f | WdFlags::SNG_QUOTED).push_span(1)
            }
            '&' | '|' => {
                word_desc = helper::finalize_word(&word_desc, &mut tokens);
                word_desc = word_desc.add_char(c);
                if let Some(ch) = chars.pop_front() {
                    trace!("checking operator");
                    trace!("found this: {}, checked against this: {}, found {}",c,ch, c==ch);
                    match ch {
                        '|' | '&' if ch == c => { word_desc = word_desc.add_char(ch); }
                        _ => { chars.push_front(ch); }
                    }
                    trace!("returning word_desc with this word: {}",word_desc.text);
                    trace!("word_desc: {:?}",word_desc);
                } else {
                    match c {
                        '&' => { word_desc = word_desc.add_char(c); }, // Background operator
                        _ => panic!("Expected an expression after this operator '{}'",c)
                    }
                };
                is_arg = false;
                word_desc = word_desc.set_flags(|f| f | WdFlags::IS_OP);
                helper::finalize_word(&word_desc, &mut tokens)
            }
            _ if helper::cmdsep(&c) => {
                trace!("Command separator found: {:?}", c);
                if is_arg {
                    word_desc = word_desc.set_flags(|f| f | WdFlags::IS_ARG);
                }
                word_desc = helper::finalize_word(&word_desc, &mut tokens);
                if let Some(ch) = chars.front() {
                    if *ch == ';' {
                        tokens.push_back(Tk::case_delim(word_desc.span.1 + 1));
                        chars.pop_front();
                    } else {
                        tokens.push_back(Tk::cmdsep(word_desc.span.1 + 1));
                    }
                } else {
                    tokens.push_back(Tk::cmdsep(word_desc.span.1 + 1));
                }
                is_arg = false; // Next word is part of a new command
                word_desc
            }
            ')' => {
                trace!("Case separator found: {:?}", c);
                word_desc.set_flags(|f| f | WdFlags::IS_ARG); // Make sure this doesn't get interpreted as a keyword
                word_desc = helper::finalize_word(&word_desc, &mut tokens);
                tokens.push_back(Tk::casesep(word_desc.span.1 + 1));
                is_arg = false; // Next word is part of a new command
                word_desc
            }
            _ if helper::wspace(&c) => {
                trace!("Whitespace found: {:?}", c);
                trace!("is_arg: {}",is_arg);
                if !word_desc.text.is_empty() {
                    let keywd = helper::keywd(&word_desc);
                    if is_arg {
                        trace!("Setting IS_ARG flag after whitespace");
                        word_desc = word_desc.set_flags(|f| f | WdFlags::IS_ARG);
                    }
                    let word_desc = helper::finalize_word(&word_desc, &mut tokens);
                    if !keywd {
                        is_arg = true;
                    }
                    word_desc.push_span(1)
                } else {
                    word_desc.clone()
                }
            }
            _ => {
                trace!("Default case: adding character {:?}", c);
                let word_desc = word_desc.add_char(c);
                trace!("Word state: {}", word_desc.text);
                word_desc
            }
        };
    }

    // Finalize any remaining word
    if !word_desc.text.is_empty() {
        trace!("finalizing word: {:?}",word_desc);
        if helper::delimited(&word_desc) {
            panic!("unclosed delimiter")
        }
        if helper::quoted(&word_desc) {
            panic!("unclosed quotation")
        }
        if is_arg {
            word_desc = word_desc.set_flags(|f| f | WdFlags::IS_ARG);
        }
        let _ = helper::finalize_word(&word_desc, &mut tokens);
    }

    tokens.push_back(Tk::end_of_input());
    debug!("Tkization complete. Tks: {:?}", tokens);

    ParseState {
        input: state.input,
        shellenv: state.shellenv,
        tokens,
        ast: state.ast,
    }
}
