use once_cell::sync::Lazy;
use log::{debug, error, info, trace};
use regex::Regex;
use std::cmp::Ordering;
use std::collections::{HashMap, VecDeque};
use std::fmt;
use std::path::PathBuf;
use tokio::sync::mpsc::{channel, Receiver, Sender};

use crate::event::{ShellError, ShellEvent};
use crate::builtin::BUILTINS;
macro_rules! define_patterns {
    ($($name:expr => $pattern:expr),* $(,)?) => {{
        let mut m = HashMap::new();
        $(m.insert($name, Regex::new($pattern).unwrap());)*
        m
    }};
}

pub static REGEX: Lazy<HashMap<&'static str, Regex>> = Lazy::new(|| {
    define_patterns! {
        "redirection" => r"^([0-9]+)?(>{1,2}|<{1,3})(?:[&]?([0-9]+))?$",
        "path" => r"^(\/(?:[^\/]+\/)*[^\/]*|(?:\.[\/\w\-]+\/?))$",
        "rsh_shebang" => r"^#!((?:/[^\s]+)+)((?:\s+arg:[a-zA-Z][a-zA-Z0-9_\-]*)*)$",
        "brace_numrange" => r"\{([0-9]+)\.\.([0-9]+)(?:\.\.([0-9]+))?\}",
        "brace_alpharange" => r"\{([0-9]+)\.\.([0-9]+)(?:\.\.([0-9]+))?\}",
        "process_sub" => r"^>\(.*\)$",
        "command_sub" => r"^\$\([^\)]+\)$",
        "arithmetic" => r"^\$\(\([^\)]+\)\)$",
        "subshell" => r"^\([^\)]+\)$",
        "test" => r"^\[\s*(.*?)\s*\]$",
        "string" => r#"^\"([^\"]*)\"$"#,
        "var_sub" => r"^\$([A-Za-z_][A-Za-z0-9_]*)$",
        "assignment" => r"^[A-Za-z_][A-Za-z0-9_]*=.*$",
        "operator" => r"(?:&&|\|\||[><]=?|[|&])",
        "cmdsep" => r"^(?:\n|;)$",
        "ident" => r"^[\x20-\x7E]*$",
    }
});

const FUNCTIONS: [&str; 1] = [
    // Will replace this with an actual functions implementation later
    // Used for now for word flags
    "PLACEHOLDER_TEXT",
];

const ALIASES: [&str; 1] = [
    // Will replace this with an actual aliases implementation later
    // Used for now for word flags
    "PLACEHOLDER_TEXT",
];

#[derive(Debug, PartialEq)]
pub enum Keywords {
    If,
    Then,
    Else,
    Elif,
    Fi,
    For,
    Select,
    While,
    Until,
    Do,
    Done,
    Case,
    Esac,
    In,
}

impl Keywords {
    pub fn new(string: &str) -> Self {
        match string {
            "if" => Keywords::If,
            "then" => Keywords::Then,
            "else" => Keywords::Else,
            "elif" => Keywords::Elif,
            "fi" => Keywords::Fi,
            "for" => Keywords::For,
            "select" => Keywords::Select,
            "while" => Keywords::While,
            "until" => Keywords::Until,
            "do" => Keywords::Do,
            "done" => Keywords::Done,
            "case" => Keywords::Case,
            "esac" => Keywords::Esac,
            "in" => Keywords::In,
            _ => unreachable!(),
        }
    }
    pub fn from(strings: Vec<&str>) -> Vec<Self> {
        let mut keywords: Vec<Self> = vec![];
        for string in strings {
            match string {
                "if" => keywords.push(Keywords::If),
                "then" => keywords.push(Keywords::Then),
                "else" => keywords.push(Keywords::Else),
                "elif" => keywords.push(Keywords::Elif),
                "fi" => keywords.push(Keywords::Fi),
                "for" => keywords.push(Keywords::For),
                "select" => keywords.push(Keywords::Select),
                "while" => keywords.push(Keywords::While),
                "until" => keywords.push(Keywords::Until),
                "do" => keywords.push(Keywords::Do),
                "done" => keywords.push(Keywords::Done),
                "case" => keywords.push(Keywords::Case),
                "esac" => keywords.push(Keywords::Esac),
                "in" => keywords.push(Keywords::In),
                _ => {}
            }
        }
        keywords
    }
    pub fn any() -> Vec<Keywords> {
        vec![
            Keywords::If,
            Keywords::Then,
            Keywords::Else,
            Keywords::Elif,
            Keywords::Fi,
            Keywords::For,
            Keywords::Select,
            Keywords::While,
            Keywords::Until,
            Keywords::Do,
            Keywords::Done,
            Keywords::Case,
            Keywords::Esac,
            Keywords::In,
        ]
    }
    pub fn openers() -> Vec<String> {
        vec![
            "if".into(),
            "while".into(),
            "for".into(),
            "until".into(),
            "select".into(),
            "case".into(),
        ]
    }

    pub fn separators() -> Vec<String> {
        vec![
            "then".into(),
            "in".into(),
            "do".into(),
            "elif".into(),
            "else".into(),
        ]
    }

    pub fn closers() -> Vec<Keywords> {
        vec![Keywords::Fi, Keywords::Esac, Keywords::Done]
    }
    pub fn check(string: &String) -> bool {
        Keywords::from(vec![&string])
            .iter()
            .find(|&kw| Keywords::any().contains(kw))
            .is_some()
    }
}

fn check_flag(flag_name: &str, flags: u32) -> bool {
    match flag_name {
        "keyword" => (flags & 1 << 1) != 0,
        "builtin" => (flags & 1 << 2) != 0,
        _ => unreachable!(),
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct WordDesc {
    text: String,
    abs_pos: usize, // Where the word starts in the input string
    span: (usize, usize),
    flags: u32,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RshParseError {
    abs_pos: usize,
    span: (usize, usize),
    msg: String,
    input: String,
}

impl RshParseError {
    fn calc_error_data(&self) -> (usize, usize, usize, usize, VecDeque<char>) {
        // Not sorry for this

        let mut chars = VecDeque::from(self.input.chars().collect::<Vec<char>>());
        let mut index = 0;
        let mut column = 1;
        let mut line = 1;
        let mut window = VecDeque::new();
        let mut this_line = String::new();
        let mut prev_line = String::new();
        let mut the_line_before_that = String::new();
        let mut target_column = 1;
        let error_length = self.span.1 - self.span.0;

        let mut found = false;
        while let Some(next_char) = chars.pop_front() {
            if index == self.abs_pos {
                target_column = column - 1;
            }
            if index > self.abs_pos && next_char == '\n' {
                debug!("breaking at newline");
                let both_lines = prev_line.clone() + &this_line;
                let all_the_lines = the_line_before_that.clone() + &both_lines;
                debug!("concatenating these lines: {}", all_the_lines);
                window = VecDeque::from(all_the_lines.chars().collect::<Vec<char>>());
                found = true;
                break;
            }
            if next_char == '\n' {
                column = 1;
                line += 1;
                the_line_before_that = prev_line;
                prev_line = this_line;
                this_line = String::new();
            } else {
                column += 1;
                window = VecDeque::new();
            }
            if window.len() == 50 {
                window.pop_front();
            }
            this_line.push(next_char);
            index += 1;
        }
        if !found {
            debug!("breaking at newline");
            let both_lines = prev_line + &this_line;
            let all_the_lines = the_line_before_that + &both_lines;
            debug!("concatenating these lines: {}", all_the_lines);
            window = VecDeque::from(all_the_lines.chars().collect::<Vec<char>>());
        }

        debug!("Chars after calculation: {:?}", chars);
        if window.len() == 50 {
            for _ in 0..3 {
                window.push_front('.');
            }
        }
        (line, column, target_column, error_length, window)
    }
}

impl fmt::Display for RshParseError {
    /// This display function returns a window of the offending code. Under the specific part of
    /// the line where the error occured, a line is drawn blaming the specific token which caused
    /// the error
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (line, column, ref mut target_column, error_length, window) = self.calc_error_data();

        let window_slice: String = window.clone().into_iter().collect();
        let error_line = window_slice.split("\n").last().unwrap();
        debug!(
            "target_column, error_length: {},{}",
            target_column, error_length
        );
        debug!("error_line: {}", error_line);
        writeln!(f, "\t")?;
        writeln!(f, "{};{} - Parse Error: {}", line, column, self.msg)?;
        writeln!(f, "-----------------------------------------------")?;
        writeln!(f, "\t")?;
        writeln!(f, "{}", window_slice)?;

        let mut error_pointer = String::new();
        if *target_column != 0 {
            *target_column -= 1
        }
        for i in 0..error_line.len() {
            if i == *target_column {
                error_pointer.push('^'); // Start of the span
                break;
            } else {
                error_pointer.push(' ');
            }
        }
        for i in 0..error_length {
            if i > error_line.len() {
                break;
            }
            if i == error_length - 1 {
                error_pointer.push('^');
            } else {
                error_pointer.push('~');
            }
        }

        writeln!(f, "{}", error_pointer)?;
        writeln!(f, "-----------------------------------------------")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ASTNode {
    Command {
        argv: VecDeque<Token>,
    },
    Builtin {
        argv: VecDeque<Token>,
    },
    Assignment {
        var: String,
        val: String
    },
    Function {
        argv: VecDeque<Token>,
    },
    Chain {
        left: Box<ASTNode>,
        right: Box<ASTNode>,
        operator: ChainOp,
    },
    Pipeline {
        left: Box<ASTNode>,
        right: Box<ASTNode>
    },
    IfThen {
        if_block: Conditional,
        elif_blocks: VecDeque<Conditional>,
        else_block: Option<Conditional>,
    },
    WhileDo {
        body: Conditional,
    },
    UntilDo {
        body: Conditional,
    },
    ForDo {
        vars: VecDeque<Token>,
        array: VecDeque<Token>,
        body: Conditional,
    },
    CaseIn {
        var: WordDesc,
        body: Vec<Conditional>,
    },
    FuncDef {
        func_name: WordDesc,
        func_body: Vec<ASTNode>,
    },
    Cmdsep,
    And,
    Or,
    Pipe,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Conditional {
    condition: VecDeque<ASTNode>,
    body: VecDeque<ASTNode>,
}

impl Conditional {
    pub fn get_cond(&self) -> VecDeque<ASTNode> {
        self.condition.clone()
    }
    pub fn get_body(&self) -> VecDeque<ASTNode> {
        self.body.clone()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub word_desc: WordDesc,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    Shebang,
    ProcessSub,
    CommandSub,
    Arithmetic,
    Subshell,
    Test,
    String {
        single_quote: bool,
    },
    VarSub,
    Assignment {
        var: String,
        val: String,
    },
    Path {
        relative: bool,
    },
    And,
    Or,
    Pipe,
    Redir {
        redir_type: RedirType,
        fd_out: i32,
        fd_target: Option<i32>,
        file_target: Option<PathBuf>
    },
    Keyword,
    Ident,
    Cmdsep,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RedirType {
    Output,
    Append,
    Input,
    Heredoc,
    Herestring,
}

impl Token {
    pub fn new(token_type: TokenType, word_desc: WordDesc) -> Self {
        Self {
            token_type,
            word_desc,
        }
    }
    // The function that creates a Token from a WordDesc
    pub fn from(word_desc: WordDesc, parser: &mut RshParser) -> Result<Token, RshParseError> {
        let string = word_desc.text.clone();
        debug!("ASTNodeuating token type for: {}", string);
        match string.as_str() {
            _ if REGEX["path"].is_match(&string) => {
                let relative = !string.starts_with('/');
                Ok(Token::new(TokenType::Path { relative }, word_desc))
            }
            _ if REGEX["assignment"].is_match(&string) => {
                let parts: Vec<&str> = string.split('=').collect();
                let (var, val) = (parts[0].into(), parts[1].to_string());
                Ok(Token::new(TokenType::Assignment { var, val }, word_desc))
            }
            _ if REGEX["redirection"].is_match(&string) => {
                if let Some(captures) = REGEX["redirection"].captures(&string) {
                    let operator = captures.get(2).map_or("none", |m| m.as_str()).to_string();
                    let fd_target = captures
                        .get(3)
                        .and_then(|m| m.as_str().to_string().parse::<i32>().ok());

                    debug!("redir operator: {}", operator);

                    if string.matches('<').count() > 3 || string.matches('>').count() > 2 {
                        return Err(RshParseError {
                            abs_pos: word_desc.abs_pos,
                            span: word_desc.span,
                            msg: "Invalid redirection operator".into(),
                            input: parser.input.clone(),
                        });
                    }

                    let mut file_target = None;
                    if !string.contains('&') {
                        // Fetch the filename from parser.words
                        if let Some(word) = parser.words.pop_front() {
                            file_target = Some(PathBuf::from(word.text.clone()));
                        }
                    }


                    let redir_type = match operator.as_str() {
                        ">" => RedirType::Output,
                        ">>" => RedirType::Append,
                        "<" => RedirType::Input,
                        "<<" => RedirType::Heredoc,
                        "<<<" => RedirType::Herestring,
                        _ => {
                            return Err(RshParseError {
                                abs_pos: word_desc.abs_pos,
                                span: word_desc.span,
                                msg: "Invalid redirection operator".into(),
                                input: parser.input.clone(),
                            })
                        }
                    };

                    // Determine the default fd_out based on redir_type
                    let fd_out = match redir_type {
                        RedirType::Output | RedirType::Append => 1,
                        RedirType::Input | RedirType::Heredoc | RedirType::Herestring => 0,
                    };

                    // Override the default if an fd is specified
                    let fd_out = captures
                        .get(1)
                        .and_then(|m| m.as_str().parse::<i32>().ok())
                        .unwrap_or(fd_out);

                    if file_target.is_none() && fd_target.is_none() {
                        return Err(RshParseError {
                            abs_pos: word_desc.abs_pos,
                            span: word_desc.span,
                            msg: "Couldn't find an output for this redirection".into(),
                            input: parser.input.clone(),
                        });
                    }

                    Ok(Token::new(
                        TokenType::Redir {
                            redir_type,
                            fd_out,
                            fd_target,
                            file_target
                        },
                        word_desc,
                    ))
                } else {
                    panic!("Redirection somehow succeeded and then failed on the same regex check?");
                }
            }
            _ if REGEX["rsh_shebang"].is_match(&string) => {
                Ok(Token::new(TokenType::Shebang, word_desc))
            }
            _ if REGEX["process_sub"].is_match(&string) => {
                Ok(Token::new(TokenType::ProcessSub, word_desc))
            }
            _ if REGEX["command_sub"].is_match(&string) => {
                Ok(Token::new(TokenType::CommandSub, word_desc))
            }
            _ if REGEX["arithmetic"].is_match(&string) => {
                Ok(Token::new(TokenType::Arithmetic, word_desc))
            }
            _ if REGEX["subshell"].is_match(&string) => {
                Ok(Token::new(TokenType::Subshell, word_desc))
            }
            _ if REGEX["test"].is_match(&string) => Ok(Token::new(TokenType::Test, word_desc)),
            _ if REGEX["string"].is_match(&string) => Ok(Token::new(
                TokenType::String {
                    single_quote: string.starts_with('\''),
                },
                word_desc,
            )),
            _ if REGEX["var_sub"].is_match(&string) => Ok(Token::new(TokenType::VarSub, word_desc)),
            _ if REGEX["cmdsep"].is_match(&string) => Ok(Token::new(TokenType::Cmdsep, word_desc)),
            _ if REGEX["operator"].is_match(&string) => match string.as_str() {
                "||" => Ok(Token::new(TokenType::Or, word_desc)),
                "&&" => Ok(Token::new(TokenType::And, word_desc)),
                "|" => Ok(Token::new(TokenType::Pipe, word_desc)),
                _ => Err(RshParseError {
                    abs_pos: word_desc.abs_pos,
                    span: word_desc.span,
                    msg: "Invalid operator".into(),
                    input: parser.input.clone(),
                }),
            },
            // TODO: implement some method for this or something
            _ if Keywords::check(&string) => Ok(Token::new(TokenType::Keyword, word_desc)),
            _ if REGEX["ident"].is_match(&string) => Ok(Token::new(TokenType::Ident, word_desc)),
            _ => Err(RshParseError {
                abs_pos: word_desc.abs_pos,
                span: word_desc.span,
                msg: "Failed to classify word".into(),
                input: parser.input.clone(),
            }),
        }
    }
    pub fn substitute(&mut self,new_text: String) {
        self.word_desc.text = new_text;
    }
    pub fn text(&self) -> &str {
        self.word_desc.text.as_str()
    }
    pub fn class(&self) -> &TokenType {
        &self.token_type
    }
    pub fn pos(&self) -> usize {
        self.word_desc.abs_pos
    }
    pub fn span(&self) -> (usize, usize) {
        self.word_desc.span
    }
    pub fn start_span(&self) -> usize {
        self.word_desc.span.0
    }
    pub fn end_span(&self) -> usize {
        self.word_desc.span.1
    }
    pub fn flags(&self) -> u32 {
        self.word_desc.flags
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ChainOp {
    And,
    Or,
}

#[derive(Clone, Debug)]
pub struct RshParser {
    input: String,
    chars: VecDeque<char>,
    words: VecDeque<WordDesc>,
    tokens: VecDeque<Token>,
    buffer: VecDeque<Token>, // Temp storage for tokens being processed
    pub ast: VecDeque<ASTNode>,
    delim_stack: VecDeque<(usize, char)>,
    pos: usize,
    window: VecDeque<char>,
    window_start_col: usize,
}

impl RshParser {
    pub fn new(input: &str) -> Self {
        let parser = Self {
            input: input.into(),
            chars: VecDeque::from(input.chars().collect::<Vec<char>>()),
            words: VecDeque::new(),
            tokens: VecDeque::new(),
            buffer: VecDeque::new(),
            ast: VecDeque::new(),
            delim_stack: VecDeque::new(),
            pos: 0,
            window: VecDeque::new(),
            window_start_col: 1,
        };
        parser
    }
    pub fn get_ast(&self) -> VecDeque<ASTNode> {
        self.ast.clone()
    }

    pub fn print_tokens(&self) {
        println!("blocks:");
        for word in self.words.clone() {
            println!("{:?}", word);
        }
    }
    pub fn advance(&mut self) -> Option<char> {
        if let Some(next_char) = self.chars.pop_front() {
            self.pos += 1;
            if self.window.len() > 50 {
                self.window.pop_front();
                self.window_start_col += 1;
            }
            self.window.push_back(next_char);
            Some(next_char)
        } else {
            None
        }
    }
    pub fn push_word(&mut self, span_start: &mut usize, cur_wd: &mut String, pos: usize) {
        let get_flags = |word: &String| -> u32 {
            let mut keyword_flag: u32 = 0;
            let mut builtin_flag: u32 = 0;
            let mut function_flag: u32 = 0;
            let mut alias_flag: u32 = 0;
            if Keywords::check(word) {
                keyword_flag += 1
            }
            if BUILTINS.contains(&word.as_str()) {
                builtin_flag += 1
            }
            if FUNCTIONS.contains(&word.as_str()) {
                function_flag += 1
            }
            if ALIASES.contains(&word.as_str()) {
                alias_flag += 1
            }
            // Shift values to proper bit position
            builtin_flag <<= 1;
            function_flag <<= 2;
            alias_flag <<= 3;
            // Squash flags together
            keyword_flag | builtin_flag | function_flag | alias_flag
        };
        if !cur_wd.is_empty() {
            self.words.push_back(WordDesc {
                text: cur_wd.clone(),
                abs_pos: *span_start,
                span: (*span_start, pos),
                flags: get_flags(cur_wd),
            });
            *span_start = pos + 1;
            cur_wd.clear();
        }
    }
    pub fn tokenize(&mut self) -> Result<(), RshParseError> {
        let mut current_word = String::new();
        let mut current_block: VecDeque<WordDesc> = VecDeque::new();
        let mut span_start = self.pos;
        while let Some(c) = self.advance() {
            debug!("checking char: {}", c);
            debug!("current_word: {}", current_word);
            if matches!(c, '(' | '[' | '{') {
                debug!("Pushing delimiter: {}", c);
                self.delim_stack.push_back((self.pos, c));
            }
            if matches!(c, ')' | ']' | '}') {
                match c {
                    ')' => {
                        let open_delim = self.delim_stack.pop_back();
                        if open_delim.is_some() && open_delim.unwrap().1 != '(' {
                            let delim = open_delim.unwrap();
                            return Err(RshParseError {
                                abs_pos: delim.0,
                                span: (delim.0, self.pos),
                                msg: "Mismatched open bracket".into(),
                                input: self.input.clone(),
                            });
                        } else if self.delim_stack.is_empty() {
                            current_word.push(c);
                            self.push_word(&mut span_start, &mut current_word, self.pos);
                            continue;
                        }
                    }
                    ']' => {
                        let open_delim = self.delim_stack.pop_back();
                        if open_delim.is_some() && open_delim.unwrap().1 != '[' {
                            let delim = open_delim.unwrap();
                            return Err(RshParseError {
                                abs_pos: delim.0,
                                span: (delim.0, self.pos),
                                msg: "Mismatched open bracket".into(),
                                input: self.input.clone(),
                            });
                        } else if self.delim_stack.is_empty() {
                            current_word.push(c);
                            debug!("Pushing word to block: {}", current_word);
                            self.push_word(&mut span_start, &mut current_word, self.pos);
                            span_start = self.pos + 1;
                            current_word.clear();
                            continue;
                        }
                    }
                    '}' => {
                        // Guard condition for brace expansion
                        let open_delim = self.delim_stack.pop_back();
                        if open_delim.is_some() && open_delim.unwrap().1 != '{' {
                            let delim = open_delim.unwrap();
                            return Err(RshParseError {
                                abs_pos: delim.0,
                                span: (delim.0, self.pos),
                                msg: "Mismatched open bracket".into(),
                                input: self.input.clone(),
                            });
                        } else if self.delim_stack.is_empty() {
                            current_word.push(c);
                            if !matches!(self.chars.front(), Some(' ') | Some(';') | Some('\n')) {
                                continue
                            }
                            debug!("Pushing word to block: {}", current_word);
                            self.push_word(&mut span_start, &mut current_word, self.pos);
                            span_start = self.pos + 1;
                            current_word.clear();
                            continue;
                        }
                    }
                    _ => unreachable!(),
                }
            }
            match c {
                '\\' => {
                    if let Some(&next_char) = self.chars.front() {
                        // Skip escaped linebreaks
                        if !matches!(next_char, '\n') {
                            current_word.push(next_char);
                        }
                    }
                    self.advance(); // Skip the next character after escaping
                }
                '#' if self.chars.front() != Some(&'!') => {
                    // Comment
                    while let Some(&next_char) = self.chars.front() {
                        if next_char == '\n' {
                            self.advance();
                            break;
                        } else {
                            self.advance();
                        }
                    }
                }
                ';' | '\n' => {
                    debug!("found block delimiter: {}", c);
                    if self.delim_stack.is_empty() {
                        debug!("pushing new block");
                        if !current_word.is_empty() {
                            self.push_word(&mut span_start, &mut current_word, self.pos);
                        }
                        if self.words.front().is_some()
                            && !matches!(self.words.front().unwrap().text.as_str(), ";" | "\n")
                        {
                            // Only push a command separator if one is not already there
                            self.push_word(&mut span_start, &mut c.into(), self.pos);
                        }
                        debug!("current_word: {}", current_word);
                        debug!("current block: {:?}", current_block);
                        span_start = self.pos + 1;
                        current_word.clear();
                    } else {
                        current_word.push(c);
                    }
                    current_block = VecDeque::new();
                }
                ' ' | '\t' if self.delim_stack.is_empty() => {
                    if !current_word.is_empty() {
                        debug!("Pushing word to block: {}", current_word);
                        self.push_word(&mut span_start, &mut current_word, self.pos);
                        span_start = self.pos + 1;
                        current_word.clear();
                    }
                }
                '"' | '\'' => {
                    debug!("found quotation mark: {}", c);
                    self.delim_stack.push_back((self.pos, c)); // Push opening delimiter onto the stack

                    while let Some(char) = self.advance() {
                        // If we encounter the same delimiter, check the stack
                        if char == c {
                            // Only pop if the stack has the corresponding opening delimiter
                            if let Some(&(pos, stack_char)) = self.delim_stack.back() {
                                if stack_char == c {
                                    self.delim_stack.pop_back(); // Pop the matching delimiter
                                    break; // Exit the loop once the closing delimiter is found
                                }
                            }
                        }
                        current_word.push(char); // Continue adding characters inside the delimiter
                    }

                    // Check if the delimiter stack is mismatched (extra unmatched opening delimiter)
                    if !self.delim_stack.is_empty() {
                        if let Some(&(pos, stack_char)) = self.delim_stack.front() {
                            if stack_char == c {
                                return Err(RshParseError {
                                    abs_pos: self.pos,
                                    span: (pos, self.pos),
                                    msg: "Mismatched quotation mark".to_string(),
                                    input: self.input.clone(),
                                });
                            }
                        }
                    } else if self.delim_stack.is_empty() {
                        debug!("Pushing word to block: {}", current_word);
                        self.push_word(&mut span_start, &mut current_word, self.pos);
                        span_start = self.pos + 1;
                        current_word.clear();
                    }
                }
                '|' => {
                    debug!("found pipe");
                    // Pipe handling (|)
                    if !current_word.is_empty() {
                        debug!("Pushing word to block: {}", current_word);
                        self.push_word(&mut span_start, &mut current_word, self.pos);
                        span_start = self.pos + 1;
                        current_word.clear();
                    }

                    current_word.push(c);
                    debug!(
                        "Pushed {} into current_word, current_word = {}",
                        c, current_word
                    );

                    if let Some(&next_char) = self.chars.front() {
                        match next_char {
                            '|' => {
                                // Handle '||'
                                current_word.push(self.advance().unwrap());
                                debug!(
                                    "Found other pipe, pushing onto current word, it's now: {}",
                                    current_word
                                );
                                debug!("Pushing word to block: {}", current_word);
                                self.push_word(&mut span_start, &mut current_word, self.pos);
                                debug!("Current blocks: {:#?}", current_block);
                                current_word.clear();
                                span_start = self.pos + 1;
                            }
                            _ => {
                                debug!("Returning single pipe: {}", current_word);
                                // Handle single pipe '|'
                                debug!("Pushing word to block: {}", current_word);
                                self.push_word(&mut span_start, &mut current_word, self.pos);
                                span_start = self.pos + 1;
                                current_word.clear();
                                self.advance();
                            }
                        }
                    }
                }
                '&' if matches!(*self.chars.front().unwrap(), '&') => {
                    debug!("found && operator");
                    // Handle '&' for background execution or '&&' for logical AND
                    if !current_word.is_empty() {
                        debug!("Pushing word to block: {}", current_word);
                        self.push_word(&mut span_start, &mut current_word, self.pos);
                        span_start = self.pos + 1;
                        current_word.clear();
                    }
                    current_word = "&&".into();
                    self.advance();
                    self.push_word(&mut span_start, &mut current_word, self.pos);
                    span_start = self.pos + 1;
                }
                '&' | '0'..='9' | '>' | '<' => {
                    debug!("Maybe found redirection?");
                    current_word.push(c);
                    let mut redir_check_stack = self.chars.clone();
                    let mut redir_test_string: String = "".into();
                    let mut count = 0;

                    // Look ahead to see if we are in a redirection
                    while let Some(check_char) = redir_check_stack.pop_front() {
                        debug!("Checking character for redir test: {}", check_char);
                        if matches!(check_char, '&' | '0'..='9' | '>' | '<') {
                            debug!("Adding checked character: {}", check_char);
                            count += 1;
                            redir_test_string.push(check_char);
                        } else {
                            break;
                        }
                    }
                    if REGEX["redirection"].is_match(redir_test_string.as_str()) {
                        debug!("redirection found, constructing...");
                        while count != 0 {
                            // Unwrap is safe because we know whats ahead
                            let redir_char = self.advance().unwrap();
                            debug!("count = {}, adding \'{}\'", count, redir_char);
                            current_word.push(redir_char);
                            count -= 1
                        }
                        self.push_word(&mut span_start, &mut current_word, self.pos);
                        span_start = self.pos + 1;
                        current_word.clear();
                    }
                }
                _ => {
                    // Add the character to the current word
                    debug!("pushing char: {}", c);
                    current_word.push(c);
                }
            }
        }
        if !self.delim_stack.is_empty() {
            debug!("Delimiter stack not empty! {:?}", self.delim_stack);
            let open_delim = self.delim_stack.pop_back();
            if let Some(delim) = open_delim {
                debug!("Returning parse error");
                return Err(RshParseError {
                    abs_pos: delim.0,
                    span: (delim.0, self.pos),
                    msg: "Mismatched delimiter".into(),
                    input: self.input.clone(),
                });
            }
        }

        if !current_word.is_empty() {
            debug!("Catching orphaned word: {}", current_word);
            self.push_word(&mut span_start, &mut current_word, self.pos);
        }
        while let Some(word) = self.words.pop_front() {
            let string = word.clone();
            let token = Token::from(word, self)?;
            debug!("Produced token: {:?} from word {:?}", token, string);
            self.tokens.push_back(token);
            // Move to the next node
        }
        self.expand()?;
        self.begin_descent()
    }

    pub fn expand(&mut self) -> Result<(),RshParseError> {
        let mut arg = false;

        while let Some(token) = self.tokens.pop_front() {
            match token.class() {
                TokenType::Cmdsep => {
                    arg = false;
                    self.buffer.push_back(token); // Push Cmdsep tokens directly
                }
                _ if token.text().contains('{') && token.text().contains('}') && arg => {
                    // Perform brace expansion
                    let expanded = self.expand_braces(token.text().to_string());
                    for expanded_token in expanded {
                        self.buffer.push_back(Token::new(
                            TokenType::String { single_quote: true },
                            WordDesc {
                                text: expanded_token,
                                abs_pos: token.pos(),
                                span: token.span(),
                                flags: token.flags(),
                            },
                        ));
                    }
                }
                TokenType::VarSub => {
                    // Perform variable expansion
                    let expanded = self.expand_variables(token.text().to_string());
                    for expanded_token in expanded {
                        self.buffer.push_back(Token::new(
                            TokenType::String { single_quote: false },
                            WordDesc {
                                text: expanded_token,
                                abs_pos: token.pos(),
                                span: token.span(),
                                flags: token.flags(),
                            },
                        ));
                    }
                }
                TokenType::CommandSub => {
                    // Perform command substitution
                    let expanded = self.expand_command_substitution(token.text().to_string());
                    for expanded_token in expanded {
                        self.buffer.push_back(Token::new(
                            TokenType::String { single_quote: false },
                            WordDesc {
                                text: expanded_token,
                                abs_pos: token.pos(),
                                span: token.span(),
                                flags: token.flags(),
                            },
                        ));
                    }
                }
                _ => {
                    // Push non-expandable tokens directly
                    self.buffer.push_back(token);
                }
            }

            // Mark that subsequent tokens are part of an argument
            arg = true;
        }

        // Drain the buffer back into tokens for further processing
        self.tokens.extend(self.buffer.drain(..));
        Ok(())
    }

    pub fn expand_variables(&self, block: String) -> Vec<String> {
        todo!()
    }

    pub fn expand_command_substitution(&self, block: String) -> Vec<String> {
        todo!()
    }

    pub fn expand_braces(&self, word: String) -> Vec<String> {
        let mut results = Vec::new();
        let mut buffer = VecDeque::from([word]);

        while let Some(current) = buffer.pop_front() {
            if let Some((prefix, amble, postfix)) = self.parse_first_brace(&current) {
                let expanded = self.expand_amble(amble);
                for part in expanded {
                    buffer.push_back(format!("{}{}{}", prefix, part, postfix));
                }
            } else {
                // No braces left to expand
                results.push(current);
            }
        }

        results
    }

    fn parse_first_brace(&self, word: &str) -> Option<(String, String, String)> {
        let mut prefix = String::new();
        let mut amble = String::new();
        let mut postfix = String::new();
        let mut char_iter = word.chars().peekable();
        let mut brace_stack = VecDeque::new();

        // Parse prefix
        while let Some(&c) = char_iter.peek() {
            if c == '{' {
                brace_stack.push_back(c);
                char_iter.next();
                break;
            } else {
                prefix.push(c);
                char_iter.next();
            }
        }

        // Parse amble
        while let Some(&c) = char_iter.peek() {
            match c {
                '{' => {
                    brace_stack.push_back(c);
                    amble.push(c);
                }
                '}' => {
                    brace_stack.pop_back();
                    if brace_stack.is_empty() {
                        char_iter.next(); // Consume closing brace
                        break;
                    } else {
                        amble.push(c);
                    }
                }
                _ => amble.push(c),
            }
            char_iter.next();
        }

        // Parse postfix
        postfix.extend(char_iter);

        if !brace_stack.is_empty() {
            None // Unmatched braces
        } else if !amble.is_empty() {
            Some((prefix, amble, postfix))
        } else {
            None // No braces found
        }
    }

    fn expand_amble(&self, amble: String) -> Vec<String> {
        if amble.contains("..") {
            // Handle range expansion
            if let Some(expanded) = self.expand_range(&amble) {
                return expanded;
            }
        } else if amble.contains(',') {
            // Handle comma-separated values
            return amble
                .split(',')
                .map(|s| s.trim().to_string())
                .collect::<Vec<String>>();
        }

        vec![amble] // If no expansion is needed, return as-is
    }

    fn expand_range(&self, range: &str) -> Option<Vec<String>> {
        let parts: Vec<&str> = range.trim_matches('{').trim_matches('}').split("..").collect();
        if let [start, end] = parts.as_slice() {
            if let (Ok(start_num), Ok(end_num)) = (start.parse::<i32>(), end.parse::<i32>()) {
                // Numeric range
                return Some((start_num..=end_num).map(|n| n.to_string()).collect());
            } else if start.len() == 1 && end.len() == 1 {
                // Alphabetic range
                let start_char = start.chars().next().unwrap();
                let end_char = end.chars().next().unwrap();
                return Some(
                    (start_char..=end_char)
                        .map(|c| c.to_string())
                        .collect(),
                );
            }
        }

        None // Invalid range
    }

    pub fn begin_descent(&mut self) -> Result<(),RshParseError> {
        let mut tokens = self.tokens.clone();
        let mut node;
        while !tokens.is_empty() {
            node = self.descend(&mut tokens);
            self.ast.push_back(node);
        }
        Ok(())
    }

    pub fn process_tokens<F>(&mut self, tokens: &mut VecDeque<Token>, mut f: F) -> Option<Token>
        where F: FnMut(&Token) -> bool,
    {
        while let Some(token) = tokens.pop_front() {
            // If we find a cmdsep, we have exhausted the scope of this invocation
            // Put everything back in the same order it was taken out
            if let TokenType::Cmdsep = token.class() {
                tokens.push_front(token);
                while let Some(token) = self.buffer.pop_back() {
                    tokens.push_front(token);
                }
                break
            }
            // If the condition returns true, then break this operation and return the token
            // This leaves both the buffer and the original deque in their current state, easily
            // showing both sides of an operation such as pipelines or chains while returning the
            // operator which it broke on
            match f(&token) {
                true => return Some(token),
                false => self.buffer.push_back(token),
            }
        }
        tokens.extend(self.buffer.drain(..));
        None
    }

    pub fn descend(&mut self, tokens: &mut VecDeque<Token>) -> ASTNode {
        debug!("Starting descent with tokens: {:?}", tokens);
        // Handle leading and trailing command separators
        if let Some(token) = tokens.pop_front() {
            if !matches!(token.class(), TokenType::Cmdsep) {
                tokens.push_front(token)
            }
        }
        if let Some(token) = tokens.pop_back() {
            if !matches!(token.class(), TokenType::Cmdsep) {
                tokens.push_back(token)
            }
        }

        if let Some(token) = self.process_tokens(tokens, |t| *t.class() == TokenType::Keyword) {
            info!("Found keyword, checking if it's an opener");
            if Keywords::openers().contains(&token.text().into()) {
                trace!("Keyword is an opener, building structure");
                trace!("tokens: {:?}",tokens);
                tokens.push_front(token);
                return self.build_structure(tokens);
            } else {
                panic!(
                    "Expected an opening keyword but got this: {}",
                    token.text()
                );
            }
        }

        // No structure keywords have been found, now check for chains
        if let Some(token) = self.process_tokens(tokens, |t| matches!(*t.class(), TokenType::And | TokenType::Or)) {
            let operator = if *token.class() == TokenType::And {
                ChainOp::And
            } else {
                ChainOp::Or
            };
            info!( "Found chaining operator: {:?}, descending into buffer and right sides", operator);

            return ASTNode::Chain {
                left: Box::new(self.descend(tokens)),
                right: Box::new(self.descend(tokens)),
                operator,
            };
        }


        // No chains have been found, now check for pipelines
        if let Some(_token) = self.process_tokens(tokens, |t| *t.class() == TokenType::Pipe) {
            return ASTNode::Pipeline {
                left: Box::new(self.descend(tokens)),
                right: Box::new(self.descend(tokens)),
            };
        }

        if let Some(token) = self.process_tokens(
            tokens,
            |t| matches!(*t.class(), TokenType::Ident | TokenType::String { .. } | TokenType::Assignment { .. })) {
                info!("Found identifier, building invocation");
                tokens.push_front(token);
                self.build_invocation(tokens)
        } else {
            panic!("Reached the bottom of the descent function with these tokens: {:#?}",tokens);
        }
    }

    pub fn build_pipeline(&mut self, tokens: &mut VecDeque<Token>) -> VecDeque<ASTNode> {
        let mut commands: VecDeque<ASTNode> = VecDeque::new();
        let mut buffer: VecDeque<Token> = VecDeque::new();
        while let Some(token) = tokens.pop_front() {
            match token.class() {
                TokenType::Pipe => {
                    commands.push_back(self.descend(tokens));
                    buffer.clear();
                }
                TokenType::Cmdsep => break,
                _ => {
                    buffer.push_back(token);
                }
            }
        }
        commands
    }

    pub fn build_invocation(&mut self, tokens: &mut VecDeque<Token>) -> ASTNode {
        let mut args: VecDeque<Token> = VecDeque::new();
        debug!("Starting build_invocation with tokens: {:?}", tokens);

        while let Some(token) = tokens.pop_front() {
            debug!("Processing token: {:?}", token);

            if matches!(token.class(), TokenType::Cmdsep) {
                debug!("Found command separator: {:?}", token);
                break
            }

            if matches!(token.class(), TokenType::Redir {..}) {
                args.push_back(token);
                // Keep grabbing redirs until there are no more left
                while let Some(token) = tokens.front() {
                    if matches!(token.class(), TokenType::Redir {..}) {
                        args.push_back(tokens.pop_front().unwrap());
                    } else {
                        break
                    }
                }
                break
            }

            trace!("Pushing token onto args: {:?}", token);
            args.push_back(token);
        }

        if BUILTINS.contains(&args[0].text()) {
            info!("Identified as a builtin command: {:?}, returning ASTNode::Builtin",args[0].text());
            ASTNode::Builtin { argv: args }

        } else if let TokenType::Assignment { var, val } = args[0].class() {
            ASTNode::Assignment { var: var.into(), val: val.into() }
        } else {
            info!("Identified as a regular command: {:?}, returning ASTNode::Command", args[0].text());
            ASTNode::Command { argv: args }
        }
    }

    fn new_expectation(input: String) -> VecDeque<String> {
        trace!("Generating new expectations based on input: {}", input);

        let mut expectations = VecDeque::new();

        match input.as_str() {
            "if" | "elif" => expectations.push_back("then".into()),
            "then" => {
                expectations.push_back("elif".into());
                expectations.push_back("else".into());
                expectations.push_back("fi".into());
            },
            "else" => expectations.push_back("fi".into()),
            "in" | "while" | "until" => expectations.push_back("do".into()),
            "for" | "select" | "case" => expectations.push_back("in".into()),
            "do" => expectations.push_back("done".into()),
            _ => {},
        }

        expectations
    }

    pub fn build_structure(&mut self, tokens: &mut VecDeque<Token>) -> ASTNode {
        let mut closer: &str = "";
        let mut block_type: String = String::new();

        if let Some(token) = tokens.front() {
            block_type = token.text().into();
            closer = match token.text() {
                "if" => "fi",
                "for" | "while" | "until" | "select" => "done",
                "case" => "esac",
                _ => unreachable!("Unexpected block type: {}", token.text()),
            };
            info!("Starting to build structure for block type: {}", block_type);
        }

        let mut body: VecDeque<ASTNode> = VecDeque::new();
        let mut condition: VecDeque<ASTNode> = VecDeque::new();
        let mut components: VecDeque<Conditional> = VecDeque::new(); // Conditional = condition + body
        let mut first_run = true;
        let mut current_context: String = String::new();
        let mut expecting = VecDeque::new();
        let mut next_tokens: VecDeque<Token> = VecDeque::new();

        debug!(
            "Initialized variables. Closer: '{}', Block type: '{}'",
            closer, block_type
        );

        while let Some(token) = tokens.pop_front() {
            debug!("Processing token: {:?}", token);

            if first_run {
                current_context = token.text().into();
                expecting = Self::new_expectation(current_context.clone());
                first_run = false;
                debug!(
                    "First run: Set current_context to '{}', expecting: {:?}",
                    current_context, expecting
                );
            }

            match token.class() {
                TokenType::Keyword => {
                    debug!("Found keyword: {}", token.text());

                    if token.text() == closer && expecting.contains(&token.text().into()) {
                        info!("Found closer keyword: '{}'", closer);
                        match block_type.as_str() {
                            "if" => {
                                info!("Zipping if statement");
                                debug!("components: {:?}",components);
                                let if_block = components.pop_front().unwrap();
                                let mut else_block: Option<Conditional> = None;
                                if let Some(last_block) = components.pop_back() {
                                    if last_block.condition.is_empty() {
                                        else_block = Some(last_block)
                                    } else {
                                        components.push_back(last_block)
                                    }
                                }
                                let elif_blocks = components;
                                debug!(
                                    "Returning IfThen: if_block: {:?}, elif_blocks: {:?}, else_block: {:?}",
                                    if_block, elif_blocks, else_block
                                );
                                return ASTNode::IfThen {
                                    if_block,
                                    elif_blocks,
                                    else_block,
                                };
                            }
                            "while" => {
                                return ASTNode::WhileDo { body: components.pop_back().unwrap() }
                            }
                            "until" => {
                                return ASTNode::UntilDo { body: components.pop_back().unwrap() }
                            }
                            _ => panic!(
                                "Unexpected block type during structure creation: {}",
                                block_type
                            ),
                        }
                    }

                    if expecting.contains(&token.text().into()) {
                        debug!(
                            "Keyword '{}' matches expectation. Updating current_context.",
                            token.text()
                        );
                        current_context = token.text().into();
                        expecting = Self::new_expectation(current_context.clone());
                    } else if !expecting.contains(&token.text().into()) && current_context != token.text() {
                        debug!(
                            "Keyword '{}' does not match expectation: {:?}",
                            token.text(),
                            expecting
                        );
                        next_tokens.push_back(token.clone());
                        debug!("next_tokens: {:?}",next_tokens);
                    }
                }
                TokenType::Cmdsep => {
                    trace!("found cmdsep in structure");
                    if let Some(next_token) = tokens.front() {
                        if !expecting.contains(&next_token.text().into()) {
                            trace!("not expecting this: {:?}",next_token);
                                // If the next token is expected, consume the command separator
                            // else, push the command separator to the next_tokens buffer
                            next_tokens.push_back(token);
                            continue
                        }
                        trace!("found expectation: {:?}",next_token);
                    }
                    debug!("Processing tokens '{:?}' under context: '{}'", next_tokens,current_context);

                    match current_context.as_str() {
                        "if" | "else" | "for" | "elif" | "while" | "until" => {
                            if matches!(current_context.as_str(), "else" | "for") {
                                let node = self.descend(&mut next_tokens);
                                components.push_back(Conditional {
                                    condition: VecDeque::new(),
                                    body: VecDeque::from(vec![node]),
                                });
                            } else {
                                let node = self.descend(&mut next_tokens);
                                debug!("Pushing to condition: {:?}", node);
                                condition.push_back(node);
                            }
                        }
                        _ => {
                            let node = self.descend(&mut next_tokens);
                            debug!("Pushing to body: {:?}", node);
                            body.push_back(node);
                            debug!(
                                "Finalizing component for context '{}'. Adding to components.",
                                current_context
                            );
                            components.push_back(Conditional {
                                condition: condition.clone(),
                                body: body.clone(),
                            });
                            condition.clear();
                            body.clear();
                        }
                    }
                }
                _ => {
                    trace!("defaulting to pushing to next_tokens for token: {:?}",token);
                    next_tokens.push_back(token.clone())
                },
            }

            if token.text() == closer {
                info!("Encountered closing keyword '{}'. Breaking loop.", closer);
                break;
            }
        }

        debug!(
            "Finished parsing structure for block type '{}'. Returning todo!",
            block_type
        );
        todo!()
    }
}
