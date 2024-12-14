use once_cell::sync::Lazy;
use log::{debug, error, info, trace};
use regex::Regex;
use std::collections::{HashMap, VecDeque};
use std::fmt;
use std::path::PathBuf;
use glob::glob;

use crate::event::{ShellError, ShellEvent};
use crate::builtin::BUILTINS;
use crate::shellenv::ShellEnv;
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

pub trait RshType {} // Type system

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

pub fn is_brace_expansion(token: &Token) -> bool {
    REGEX["brace_expansion"].is_match(token.text())
    && REGEX["brace_expansion"].captures(token.text()).unwrap()[1].is_empty()
}

pub fn check_globs(string: String) -> bool {
    string.chars().any(|t| matches!(t, '?' | '*' | '[' | ']' | '!' | '-'))
}

pub fn expand(shellenv: &mut ShellEnv, token: Token) -> VecDeque<Token> {
    debug!("expand(): Starting expansion with token: {:?}", token);
    let mut working_buffer: VecDeque<Token> = VecDeque::new();
    let mut product_buffer: VecDeque<Token> = VecDeque::new();

    working_buffer.push_back(token.clone());
    while let Some(token) = working_buffer.pop_front() {
        let is_glob = check_globs(token.text().into());
        let is_brace_expansion = is_brace_expansion(&token);
        if !is_glob && !is_brace_expansion {
            // Expand variables - returns original string if none are found
            let cleaned_token = expand_variable(shellenv, token.text().into());
            product_buffer.push_back(
                Token {
                    token_type: TokenType::String { single_quote: true },
                    word_desc: WordDesc {
                        text: cleaned_token,
                        abs_pos: token.pos(),
                        span: token.span(),
                    }
                }
            );
        } else if is_brace_expansion {
            trace!("expand(): Beginning brace expansion on {}", token.text());
            // Perform brace expansion
            let expanded = expand_braces(shellenv, token.text().to_string());
            for expanded_token in expanded {
                working_buffer.push_back(
                    Token {
                        token_type: TokenType::String { single_quote: true },
                        word_desc: WordDesc {
                            text: expanded_token,
                            abs_pos: token.pos(),
                            span: token.span(),
                        }
                    }
                );
            };
        } else if is_glob {
            // Expand glob patterns
            for path in glob(token.text()).unwrap().flatten() {
                working_buffer.push_back(
                    Token {
                        token_type: TokenType::String { single_quote: true },
                        word_desc: WordDesc {
                            text: path.to_str().unwrap().to_string(),
                            abs_pos: token.pos(),
                            span: token.span(),
                        }
                    }
                );
            }
        }
    }
    product_buffer
}

pub fn expand_variable(shellenv: &mut ShellEnv, string: String) -> String {
    trace!("expand_variable(): attempting to expand variable from {}", string);
    trace!("expand_variable(): match: {}", REGEX["var_sub"].is_match(&string));
    REGEX["var_sub"]
        .replace_all(&string, |caps: &regex::Captures| {
            trace!("expand_variable(): match found: {}", &caps[0]);
            let empty = &"".to_string();
            let var_name = caps.get(1).or(caps.get(2)).map_or("", |m| m.as_str());

            trace!("expand_variable(): variable name: {}", var_name);
            let val = shellenv.get_variable(var_name).unwrap_or(empty);
            trace!("expand_variable(): val found: {}", val);

            val.to_string()
        }).to_string()
}

pub fn expand_braces(shellenv: &mut ShellEnv, word: String) -> Vec<String> {
    let mut results = Vec::new();
    let mut buffer = VecDeque::from([word]);

    while let Some(current) = buffer.pop_front() {
        if let Some((prefix, amble, postfix)) = parse_first_brace(shellenv, &current) {
            let expanded = expand_amble(shellenv, amble);
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

fn parse_first_brace(shellenv: &mut ShellEnv, word: &str) -> Option<(String, String, String)> {
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

fn expand_amble(shellenv: &mut ShellEnv, amble: String) -> Vec<String> {
    if amble.contains("..") {
        // Handle range expansion
        if let Some(expanded) = expand_range(shellenv, &amble) {
            return expanded;
        }
    } else if amble.contains(',') {
        // Handle comma-separated values
        return amble
            .split(',')
            // expand_variable is used here to catch the case of {$var1,$var2,$var3}
            .map(|s| expand_variable(shellenv, s.trim().to_string()))
            .collect::<Vec<String>>();
    }

    vec![amble] // If no expansion is needed, return as-is
}

fn expand_range(shellenv: &mut ShellEnv, range: &str) -> Option<Vec<String>> {
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

pub struct StructureBuilderState {
    pub components: VecDeque<Conditional>,
    pub var_list: VecDeque<Token>,
    pub value_array: VecDeque<Token>,
    pub token_buffer: VecDeque<Token>,
    pub body: VecDeque<ASTNode>,
    pub condition: VecDeque<ASTNode>,
    pub current_context: String,
    pub expecting: VecDeque<String>,
}

impl StructureBuilderState {
    pub fn new() -> Self {
        Self {
            components: VecDeque::new(),
            var_list: VecDeque::new(),
            value_array: VecDeque::new(),
            token_buffer: VecDeque::new(),
            body: VecDeque::new(),
            condition: VecDeque::new(),
            current_context: String::new(),
            expecting: VecDeque::new(),
        }
    }
}

impl Default for StructureBuilderState {
    fn default() -> Self {
        Self::new()
    }
}

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
            .any(|kw| Keywords::any().contains(kw))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct WordDesc {
    pub text: String,
    pub abs_pos: usize, // Where the word starts in the input string
    pub span: (usize, usize),
    pub is_cmd: bool
}

#[derive(Debug, Clone, PartialEq)]
pub struct RshParseError {
    pub abs_pos: usize,
    pub span: (usize, usize),
    pub msg: String,
    pub input: String,
}

impl RshParseError {
    fn calc_error_data(&self) -> (usize, usize, usize, usize, VecDeque<char>) {
        debug!("calc_error_data(): Calculating error data");

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
                debug!("calc_error_data(): breaking at newline");
                let both_lines = prev_line.clone() + &this_line;
                let all_the_lines = the_line_before_that.clone() + &both_lines;
                debug!("calc_error_data(): concatenating these lines: {}", all_the_lines);
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
            debug!("calc_error_data(): breaking at newline");
            let both_lines = prev_line + &this_line;
            let all_the_lines = the_line_before_that + &both_lines;
            debug!("calc_error_data(): concatenating these lines: {}", all_the_lines);
            window = VecDeque::from(all_the_lines.chars().collect::<Vec<char>>());
        }

        debug!("calc_error_data(): Chars after calculation: {:?}", chars);
        if window.len() == 50 {
            for _ in 0..3 {
                window.push_front('.');
            }
        }
        (line, column, target_column, error_length, window)
    }
}

impl fmt::Display for RshParseError {
    /// This display function returns a window of the offending code. Under the specific part
    /// of the line where the error occured, a line is drawn blaming the specific token which caused
    /// the error
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (line, column, ref mut target_column, error_length, window) = self.calc_error_data();

        let window_slice: String = window.clone().into_iter().collect();
        let error_line = window_slice.split("\n").last().unwrap();
        debug!(
            "fmt(): target_column, error_length: {},{}",
            target_column, error_length
        );
        debug!("fmt(): error_line: {}", error_line);
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
    If {
        cond_blocks: VecDeque<Conditional>,
        else_block: Option<Conditional>,
    },
    Loop {
        condition: bool,
        body: Conditional,
    },
    For {
        vars: VecDeque<Token>,
        array: VecDeque<Token>,
        body: Conditional,
    },
    Case {
        var: WordDesc,
        cases: Vec<(String,ASTNode)>,
    },
    Select {
        var: WordDesc,
        options: Vec<(String,ASTNode)>,
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
    pub fn from(word_desc: WordDesc, parser: &mut Rsh) -> Result<Token, RshParseError> {
        let string = word_desc.text.clone();
        debug!("Token::from(): ASTNodeuating token type for: {}", string);
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

                    debug!("Token::from(): redir operator: {}", operator);

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
                    panic!("Token::from(): Redirection somehow succeeded and then failed on the same regex check?");
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
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ChainOp {
    And,
    Or,
}
#[derive(Clone, Debug, PartialEq)]
pub enum Structure {
    If {
        cond_blocks: VecDeque<Conditional>,
        else_block: Option<VecDeque<ASTNode>>
    },
    For {
        loop_vars: VecDeque<String>,
        loop_vals: VecDeque<String>
    },
    Loop {
        condition: bool,
        logic: Conditional
    },
    Case {
        check_string: String,
        cases: VecDeque<(String,ASTNode)>,
    },
    Select {
        target_var: String,
        options: VecDeque<(String,ASTNode)>
    },
}

/// block_type = 'for', 'if', 'while', etc.
/// buffer = intermediate token stack before being pushed into token_decks
/// token_decks = stacks of tokens split at keywords like elif/else
/// splitters = vector of expected keywords for the next split
/// closer = the keyword that closes the block; 'fi', 'esac', etc.
/// After the closer is found, evaluate each word deck, construct ast nodes, and build Structures
#[derive(Debug)]
pub struct LexerState {
    chars: VecDeque<char>,
    pos: usize,
    window: VecDeque<char>,
    window_start_col: usize,
}
impl LexerState {
    pub fn new(chars: VecDeque<char>) -> Self {
        Self {
            chars,
            pos: 0,
            window: VecDeque::new(),
            window_start_col: 0
        }
    }
}

#[derive(Debug)]
pub struct TokenizerState {
    tokens: VecDeque<Token>,
    buffer: VecDeque<Token>, // Temp storage for tokens being processed
}
impl TokenizerState {
    pub fn new() -> Self {
        Self {
            tokens: VecDeque::new(),
            buffer: VecDeque::new(),
        }
    }
}

impl Default for TokenizerState {
    fn default() -> Self {
        Self::new()
    }
}
#[derive(Debug)]
pub struct ParserState {
    pub ast: VecDeque<ASTNode>,
    delim_stack: VecDeque<(usize, char)>,
}
impl ParserState {
    pub fn new() -> Self {
        Self {
            ast: VecDeque::new(),
            delim_stack: VecDeque::new(),
        }
    }
}

impl Default for ParserState {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct Rsh<'a> {
    shellenv: &'a mut ShellEnv,
    input: String,
    words: VecDeque<WordDesc>,
    lexer: LexerState,
    parser: ParserState,
    tokenizer: TokenizerState
}

impl<'a> Rsh<'a> {
    pub fn new(input: &str, shellenv: &'a mut ShellEnv) -> Self {
        Self {
            shellenv,
            input: input.into(),
            words: VecDeque::new(),
            lexer: LexerState::new(input.chars().collect::<VecDeque<char>>()),
            parser: ParserState::new(),
            tokenizer: TokenizerState::new()
        }
    }
    pub fn get_ast(&self) -> VecDeque<ASTNode> {
        self.parser.ast.clone()
    }

    pub fn print_tokens(&self) {
        println!("blocks:");
        for word in self.words.clone() {
            println!("{:?}", word);
        }
    }
    pub fn advance(&mut self) -> Option<char> {
        if let Some(next_char) = self.lexer.chars.pop_front() {
            trace!("advance(): advancing character: {}",next_char);
            self.lexer.pos += 1;
            if self.lexer.window.len() > 50 {
                self.lexer.window.pop_front();
                self.lexer.window_start_col += 1;
            }
            self.lexer.window.push_back(next_char);
            Some(next_char)
        } else {
            None
        }
    }
    pub fn push_word(&mut self, span_start: &mut usize, cur_wd: &mut String, pos: usize) {
        if !cur_wd.is_empty() {
            self.words.push_back(WordDesc {
                text: cur_wd.clone(),
                abs_pos: *span_start,
                span: (*span_start, pos),
            });
            *span_start = pos + 1;
            cur_wd.clear();
        }
    }
    pub fn tokenize(&mut self) -> Result<(), RshParseError> {
        trace!("tokenize(): Beginning tokenize()!");
        let mut current_word = String::new();
        let mut current_block: VecDeque<WordDesc> = VecDeque::new();
        let mut span_start = self.lexer.pos;
        while let Some(c) = self.advance() {
            debug!("tokenize(): checking char: {}", c);
            debug!("tokenize(): current_word: {}", current_word);
            if matches!(c, '(' | '[' | '{') {
                debug!("tokenize(): Pushing delimiter: {}", c);
                self.parser.delim_stack.push_back((self.lexer.pos, c));
            }
            if matches!(c, ')' | ']' | '}') {
                match c {
                    ')' => {
                        let open_delim = self.parser.delim_stack.pop_back();
                        if open_delim.is_some() && open_delim.unwrap().1 != '(' {
                            let delim = open_delim.unwrap();
                            return Err(RshParseError {
                                abs_pos: delim.0,
                                span: (delim.0, self.lexer.pos),
                                msg: "Mismatched open bracket".into(),
                                input: self.input.clone(),
                            });
                        } else if self.parser.delim_stack.is_empty() {
                            current_word.push(c);
                            self.push_word(&mut span_start, &mut current_word, self.lexer.pos);
                            continue;
                        }
                    }
                    ']' => {
                        let open_delim = self.parser.delim_stack.pop_back();
                        if open_delim.is_some() && open_delim.unwrap().1 != '[' {
                            let delim = open_delim.unwrap();
                            return Err(RshParseError {
                                abs_pos: delim.0,
                                span: (delim.0, self.lexer.pos),
                                msg: "Mismatched open bracket".into(),
                                input: self.input.clone(),
                            });
                        } else if self.parser.delim_stack.is_empty() {
                            current_word.push(c);
                            debug!("tokenize(): Pushing word to block: {}", current_word);
                            self.push_word(&mut span_start, &mut current_word, self.lexer.pos);
                            span_start = self.lexer.pos + 1;
                            current_word.clear();
                            continue;
                        }
                    }
                    '}' => {
                        // Guard condition for brace expansion
                        let open_delim = self.parser.delim_stack.pop_back();
                        if open_delim.is_some() && open_delim.unwrap().1 != '{' {
                            let delim = open_delim.unwrap();
                            return Err(RshParseError {
                                abs_pos: delim.0,
                                span: (delim.0, self.lexer.pos),
                                msg: "Mismatched open bracket".into(),
                                input: self.input.clone(),
                            });
                        } else if self.parser.delim_stack.is_empty() {
                            current_word.push(c);
                            if !matches!(self.lexer.chars.front(), Some(' ') | Some(';') | Some('\n')) {
                                continue
                            }
                            debug!("tokenize(): Pushing word to block: {}", current_word);
                            self.push_word(&mut span_start, &mut current_word, self.lexer.pos);
                            span_start = self.lexer.pos + 1;
                            current_word.clear();
                            continue;
                        }
                    }
                    _ => unreachable!(),
                }
            }
            match c {
                '\\' => {
                    if let Some(&next_char) = self.lexer.chars.front() {
                        // Skip escaped linebreaks
                        if !matches!(next_char, '\n') {
                            current_word.push(next_char);
                        }
                    }
                    self.advance(); // Skip the next character after escaping
                }
                '#' if self.lexer.chars.front() != Some(&'!') => {
                    // Comment
                    while let Some(&next_char) = self.lexer.chars.front() {
                        if next_char == '\n' {
                            self.advance();
                            break;
                        } else {
                            self.advance();
                        }
                    }
                }
                ';' | '\n' => {
                    debug!("tokenize(): found block delimiter: {}", c);
                    if self.parser.delim_stack.is_empty() {
                        debug!("tokenize(): pushing new block");
                        if !current_word.is_empty() {
                            self.push_word(&mut span_start, &mut current_word, self.lexer.pos);
                        }
                        if self.words.front().is_some()
                            && !matches!(self.words.front().unwrap().text.as_str(), ";" | "\n")
                        {
                            // Only push a command separator if one is not already there
                            self.push_word(&mut span_start, &mut c.into(), self.lexer.pos);
                        }
                        debug!("tokenize(): current_word: {}", current_word);
                        debug!("tokenize(): current block: {:?}", current_block);
                        span_start = self.lexer.pos + 1;
                        current_word.clear();
                    } else {
                        current_word.push(c);
                    }
                    current_block = VecDeque::new();
                }
                ' ' | '\t' if self.parser.delim_stack.is_empty() => {
                    if !current_word.is_empty() {
                        debug!("tokenize(): Pushing word to block: {}", current_word);
                        self.push_word(&mut span_start, &mut current_word, self.lexer.pos);
                        span_start = self.lexer.pos + 1;
                        current_word.clear();
                    }
                }
                '"' | '\'' => {
                    debug!("tokenize(): found quotation mark: {}", c);
                    self.parser.delim_stack.push_back((self.lexer.pos, c)); // Push opening delimiter onto the stack

                    while let Some(char) = self.advance() {
                        // If we encounter the same delimiter, check the stack
                        if char == c {
                            // Only pop if the stack has the corresponding opening delimiter
                            if let Some(&(_pos, stack_char)) = self.parser.delim_stack.back() {
                                if stack_char == c {
                                    self.parser.delim_stack.pop_back(); // Pop the matching delimiter
                                    break; // Exit the loop once the closing delimiter is found
                                }
                            }
                        }
                        current_word.push(char); // Continue adding characters inside the delimiter
                    }

                    // Check if the delimiter stack is mismatched (extra unmatched opening delimiter)
                    if !self.parser.delim_stack.is_empty() {
                        if let Some(&(pos, stack_char)) = self.parser.delim_stack.front() {
                            if stack_char == c {
                                return Err(RshParseError {
                                    abs_pos: self.lexer.pos,
                                    span: (pos, self.lexer.pos),
                                    msg: "Mismatched quotation mark".to_string(),
                                    input: self.input.clone(),
                                });
                            }
                        }
                    } else if self.parser.delim_stack.is_empty() {
                        debug!("tokenize(): Pushing word to block: {}", current_word);
                        self.push_word(&mut span_start, &mut current_word, self.lexer.pos);
                        span_start = self.lexer.pos + 1;
                        current_word.clear();
                    }
                }
                '|' => {
                    debug!("tokenize(): found pipe");
                    // Pipe handling (|)
                    if !current_word.is_empty() {
                        debug!("tokenize(): Pushing word to block: {}", current_word);
                        self.push_word(&mut span_start, &mut current_word, self.lexer.pos);
                        span_start = self.lexer.pos + 1;
                        current_word.clear();
                    }

                    current_word.push(c);
                    debug!("tokenize(): Pushed {} into current_word, current_word = {}", c, current_word);

                    if let Some(&next_char) = self.lexer.chars.front() {
                        match next_char {
                            '|' => {
                                // Handle '||'
                                current_word.push(self.advance().unwrap());
                                debug!("tokenize(): Found other pipe, pushing onto current word, it's now: {}", current_word);
                                debug!("tokenize(): Pushing word to block: {}", current_word);
                                self.push_word(&mut span_start, &mut current_word, self.lexer.pos);
                                debug!("tokenize(): Current blocks: {:#?}", current_block);
                                current_word.clear();
                                span_start = self.lexer.pos + 1;
                            }
                            _ => {
                                debug!("tokenize(): Returning single pipe: {}", current_word);
                                // Handle single pipe '|'
                                debug!("tokenize(): Pushing word to block: {}", current_word);
                                self.push_word(&mut span_start, &mut current_word, self.lexer.pos);
                                span_start = self.lexer.pos + 1;
                                current_word.clear();
                                self.advance();
                            }
                        }
                    }
                }
                '&' if matches!(*self.lexer.chars.front().unwrap(), '&') => {
                    debug!("tokenize(): found && operator");
                    // Handle '&' for background execution or '&&' for logical AND
                    if !current_word.is_empty() {
                        debug!("tokenize(): Pushing word to block: {}", current_word);
                        self.push_word(&mut span_start, &mut current_word, self.lexer.pos);
                        span_start = self.lexer.pos + 1;
                        current_word.clear();
                    }
                    current_word = "&&".into();
                    self.advance();
                    self.push_word(&mut span_start, &mut current_word, self.lexer.pos);
                    span_start = self.lexer.pos + 1;
                }
                '&' | '0'..='9' | '>' | '<' => {
                    debug!("tokenize(): Maybe found redirection?");
                    current_word.push(c);
                    let mut redir_check_stack = self.lexer.chars.clone();
                    let mut redir_test_string: String = "".into();
                    let mut count = 0;

                    // Look ahead to see if we are in a redirection
                    while let Some(check_char) = redir_check_stack.pop_front() {
                        debug!("tokenize(): Checking character for redir test: {}", check_char);
                        if matches!(check_char, '&' | '0'..='9' | '>' | '<') {
                            debug!("tokenize(): Adding checked character: {}", check_char);
                            count += 1;
                            redir_test_string.push(check_char);
                        } else {
                            break;
                        }
                    }
                    if REGEX["redirection"].is_match(redir_test_string.as_str()) {
                        debug!("tokenize(): redirection found, constructing...");
                        while count != 0 {
                            // Unwrap is safe because we know whats ahead
                            let redir_char = self.advance().unwrap();
                            debug!("tokenize(): count = {}, adding \'{}\'", count, redir_char);
                            current_word.push(redir_char);
                            count -= 1
                        }
                        self.push_word(&mut span_start, &mut current_word, self.lexer.pos);
                        span_start = self.lexer.pos + 1;
                        current_word.clear();
                    }
                }
                _ => {
                    // Add the character to the current word
                    debug!("tokenize(): pushing char: {}", c);
                    current_word.push(c);
                }
            }
        }
        if !self.parser.delim_stack.is_empty() {
            debug!("tokenize(): Delimiter stack not empty! {:?}", self.parser.delim_stack);
            let open_delim = self.parser.delim_stack.pop_back();
            if let Some(delim) = open_delim {
                debug!("tokenize(): Returning parse error");
                return Err(RshParseError {
                    abs_pos: delim.0,
                    span: (delim.0, self.lexer.pos),
                    msg: "Mismatched delimiter".into(),
                    input: self.input.clone(),
                });
            }
        }

        if !current_word.is_empty() {
            debug!("tokenize(): Catching orphaned word: {}", current_word);
            self.push_word(&mut span_start, &mut current_word, self.lexer.pos);
        }
        while let Some(word) = self.words.pop_front() {
            let string = word.clone();
            let token = Token::from(word, self)?;
            debug!("tokenize(): Produced token: {:?} from word {:?}", token, string);
            self.tokenizer.tokens.push_back(token);
            // Move to the next node
        }
        self.expand()?;
        self.begin_descent()
    }

    // Going to deprecate this soon and move all of the logic to extract_args in execute.rs
    pub fn expand(&mut self) -> Result<(),RshParseError> {
        let mut arg = false;

        while let Some(token) = self.tokenizer.tokens.pop_front() {
            match token.class() {
                TokenType::Cmdsep => {
                    arg = false;
                    self.tokenizer.buffer.push_back(token); // Push Cmdsep tokens directly
                }
                TokenType::CommandSub => {
                    // Perform command substitution
                    let expanded = self.expand_command_substitution(token.text().to_string());
                    for expanded_token in expanded {
                        self.tokenizer.buffer.push_back(Token::new(
                            TokenType::String { single_quote: false },
                            WordDesc {
                                text: expanded_token,
                                abs_pos: token.pos(),
                                span: token.span(),
                            },
                        ));
                    }
                }
                _ => {
                    // Push non-expandable tokens directly
                    self.tokenizer.buffer.push_back(token);
                }
            }

            // Mark that subsequent tokens are part of an argument
            arg = true;
        }

        // Drain the buffer back into tokens for further processing
        self.tokenizer.tokens.extend(self.tokenizer.buffer.drain(..));
        Ok(())
    }


    pub fn expand_command_substitution(&self, block: String) -> Vec<String> {
        todo!()
    }


    pub fn begin_descent(&mut self) -> Result<(),RshParseError> {
        let mut tokens = self.tokenizer.tokens.clone();
        let mut node;
        while !tokens.is_empty() {
            node = self.descend(&mut tokens)?;
            self.parser.ast.push_back(node);
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
                while let Some(token) = self.tokenizer.buffer.pop_back() {
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
                false => self.tokenizer.buffer.push_back(token),
            }
        }
        tokens.extend(self.tokenizer.buffer.drain(..));
        None
    }

    pub fn descend(&mut self, tokens: &mut VecDeque<Token>) -> Result<ASTNode,RshParseError> {
        debug!("descend(): Starting descent with tokens: {:?}", tokens);
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
            info!("descend(): Found keyword, checking if it's an opener");
            if Keywords::openers().contains(&token.text().into()) {
                trace!("descend(): Keyword is an opener, building structure");
                trace!("descend(): tokens: {:?}",tokens);
                tokens.push_front(token);
                return self.build_structure(tokens);
            } else {
                panic!("descend(): Expected an opening keyword but got this: {}", token.text());
            }
        }

        // No structure keywords have been found, now check for chains
        if let Some(token) = self.process_tokens(tokens, |t| matches!(*t.class(), TokenType::And | TokenType::Or)) {
            let operator = if *token.class() == TokenType::And {
                ChainOp::And
            } else {
                ChainOp::Or
            };
            info!( "descend(): Found chaining operator: {:?}, descending into buffer and right sides", operator);

            return Ok(ASTNode::Chain {
                left: Box::new(self.descend(tokens)?),
                right: Box::new(self.descend(tokens)?),
                operator,
            });
        }


        // No chains have been found, now check for pipelines
        if let Some(_token) = self.process_tokens(tokens, |t| *t.class() == TokenType::Pipe) {
            return Ok(ASTNode::Pipeline {
                left: Box::new(self.descend(tokens)?),
                right: Box::new(self.descend(tokens)?),
            });
        }

        if let Some(token) = self.process_tokens(
            tokens,
            |t| matches!(*t.class(), TokenType::Ident | TokenType::String { .. } | TokenType::Assignment { .. })) {
                info!("descend(): Found identifier, building invocation");
                tokens.push_front(token);
                self.build_invocation(tokens)
        } else {
            panic!("descend(): Reached the bottom of the descent function with these tokens: {:#?}",tokens);
        }
    }

    pub fn build_pipeline(&mut self, tokens: &mut VecDeque<Token>) -> Result<VecDeque<ASTNode>,RshParseError> {
        let mut commands: VecDeque<ASTNode> = VecDeque::new();
        let mut buffer: VecDeque<Token> = VecDeque::new();
        while let Some(token) = tokens.pop_front() {
            match token.class() {
                TokenType::Pipe => {
                    commands.push_back(self.descend(tokens)?);
                    buffer.clear();
                }
                TokenType::Cmdsep => break,
                _ => {
                    buffer.push_back(token);
                }
            }
        }
        Ok(commands)
    }

    pub fn build_invocation(&mut self, tokens: &mut VecDeque<Token>) -> Result<ASTNode, RshParseError> {
        let mut args = VecDeque::new();
        debug!("build_invocation(): Starting build_invocation with tokens: {:?}", tokens);

        while let Some(token) = tokens.pop_front() {
            debug!("build_invocation(): Processing token: {:?}", token);

            match token.class() {
                TokenType::Cmdsep => {
                    debug!("build_invocation(): Found command separator: {:?}", token);
                    break;
                }
                TokenType::Redir {..} => {
                    args.push_back(token);
                    while let Some(next_token) = tokens.front() {
                        args.push_back(tokens.pop_front().unwrap());
                    }
                    break;
                }
                _ => {
                    trace!("build_invocation(): Pushing token onto args: {:?}", token);
                    args.push_back(token);
                }
            }
        }

        if let Some(first_token) = args.front() {
            if let TokenType::Assignment { var, val } = first_token.class() {
                Ok(ASTNode::Assignment { var: var.into(), val: val.into() })

            } else {
                let command = first_token.text();
                match command {
                    _ if BUILTINS.contains(&command) => {
                        info!("build_invocation(): Identified as a builtin command: {:?}, returning ASTNode::Builtin", command);
                        Ok(ASTNode::Builtin { argv: args })
                    }
                    _ if FUNCTIONS.contains(&command) => {
                        Ok(ASTNode::Function { argv: args })
                    }
                    _ => {
                        info!("build_invocation(): Identified as a regular command: {:?}, returning ASTNode::Command", command);
                        Ok(ASTNode::Command { argv: args })
                    }
                }
            }
        } else { unreachable!() }
    }

    fn new_expectation(input: &String) -> VecDeque<String> {
        trace!("new_expectation(): Generating new expectations based on input: {}", input);

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

    fn is_closing_keyword(&self, token: &Token, closer: &str) -> Result<bool, RshParseError> {
        if token.text() == closer {
            info!("new_expectation(): Encountered closing keyword '{}'.", closer);
            return Ok(true);
        }
        Ok(false)
    }

    pub fn build_structure(&mut self, tokens: &mut VecDeque<Token>) -> Result<ASTNode, RshParseError> {
        let mut state = StructureBuilderState::new();
        let (block_type, closer) = self.initialize_structure(tokens, &mut state)?;

        while let Some(token) = tokens.pop_front() {
            debug!("build_structure(): Processing token: {:?}", token);

            if self.is_closing_keyword(&token, &closer)? {
                return self.finalize_structure(
                    block_type.as_str(),
                    state,
                    token,
                );
            }

            self.process_token(
                token,
                tokens,
                &block_type,
                &mut state,
            )?;
        }
        unreachable!()
    }

    // Step 1: Initialize structure (block type and closer keyword)
    fn initialize_structure(&mut self, tokens: &mut VecDeque<Token>, state: &mut StructureBuilderState) -> Result<(String, String), RshParseError> {
        if let Some(token) = tokens.front() {
            let block_type = token.text().to_string();
            let closer = match token.text() {
                "if" => "fi".to_string(),
                "for" | "while" | "until" | "select" => "done".to_string(),
                "case" => "esac".to_string(),
                _ => return Err(RshParseError {
                    abs_pos: token.pos(),
                    span: token.span(),
                    msg: format!("Unexpected token: {}",token.text()),
                    input: self.input.clone()
                }),
            };
            info!("initialize_structure(): Starting to build structure for block type: {}", block_type);
            state.current_context = token.text().into();
            state.expecting = Self::new_expectation(&state.current_context);
            Ok((block_type, closer))
        } else {
            panic!("initialize_structure(): unexpected eof");
        }
    }

    fn finalize_structure(
        &mut self,
        block_type: &str,
        mut state: StructureBuilderState,
        token: Token,
    ) -> Result<ASTNode, RshParseError> {
        match block_type {
            "if" => {
                let mut else_block = None;
                if let Some(last_block) = state.components.back() {
                    if last_block.condition.is_empty() {
                        else_block = Some(state.components.pop_back().unwrap());
                    }
                }
                Ok(ASTNode::If {
                    cond_blocks: state.components,
                    else_block,
                })
            }
            "while" => Ok(ASTNode::Loop {
                condition: true,
                body: state.components.pop_back().unwrap(),
            }),
            "until" => Ok(ASTNode::Loop {
                condition: false,
                body: state.components.pop_back().unwrap(),
            }),
            "for" => {
                if state.var_list.is_empty() || state.value_array.is_empty() {
                    return Err(RshParseError {
                        abs_pos: token.pos(),
                        span: token.span(),
                        msg: "Undefined variables or array in for loop".into(),
                        input: self.input.clone(),
                    });
                }
                Ok(ASTNode::For {
                    vars: state.var_list,
                    array: state.value_array,
                    body: state.components.pop_back().unwrap(),
                })
            }
            _ => Err(RshParseError {
                abs_pos: token.pos(),
                span: token.span(),
                msg: format!("Unexpected block type: {}", block_type),
                input: self.input.clone(),
            }),
        }
    }

    // Step 4: Process each token
    fn process_token(
        &mut self,
        token: Token,
        tokens: &mut VecDeque<Token>,
        block_type: &str,
        state: &mut StructureBuilderState,
    ) -> Result<(), RshParseError> {
        match token.class() {
            TokenType::Keyword => self.process_keyword(token, state)?,
            TokenType::Cmdsep => self.process_command_separator(token, tokens, state)?,
            _ => self.process_default(token, block_type, state),
        }
        Ok(())
    }

    // Step 5: Handle keywords
    fn process_keyword(
        &mut self,
        token: Token,
        state: &mut StructureBuilderState,
    ) -> Result<(), RshParseError> {
        debug!("process_keyword(): Found keyword: {}", token.text());
        if state.expecting.contains(&token.text().into()) {
            debug!("process_keyword(): Keyword '{}' matches expectation. Updating current_context.", token.text()
            );
            state.current_context = token.text().into();
            state.expecting = Self::new_expectation(&state.current_context);
        } else if !state.expecting.contains(&token.text().into()) && state.current_context != *token.text() {
            debug!("process_keyword(): Keyword '{}' does not match expectation: {:?}, with current_context {}",
                token.text(),
                state.expecting,
                state.current_context
            );
            if state.current_context == "for" && matches!(token.class(), TokenType::String { .. } | TokenType::Ident) {
                state.var_list.push_back(token.clone());
            } else if state.current_context == "in" && matches!(token.class(), TokenType::String { .. } | TokenType::Ident) {
                let mut expanded = expand(self.shellenv, token.clone());
                while let Some(expanded_token) = expanded.pop_front() {
                    state.value_array.push_back(expanded_token.clone());
                }
            } else {
                state.token_buffer.push_back(token.clone());
            }

            debug!("process_keyword(): token_buffer: {:?}", state.token_buffer);
        }
        Ok(())
    }

    // Step 6: Handle command separators
    fn process_command_separator(
        &mut self,
        token: Token,
        tokens: &mut VecDeque<Token>,
        state: &mut StructureBuilderState,
    ) -> Result<(), RshParseError> {
        trace!("process_command_separator(): Found cmdsep: {:?}", token);
        if let Some(next_token) = tokens.front() {
            if !state.expecting.contains(&next_token.text().into()) {
                trace!("process_command_separator(): not expecting this: {:?}", next_token);
                state.token_buffer.push_back(token);
                return Ok(());
            }
            trace!("process_command_separator(): found expectation: {:?}", next_token);
        }
        debug!("process_command_separator(): Processing tokens '{:?}' under context: '{}'", state.token_buffer, state.current_context);

        match state.current_context.as_str() {
            "if" | "else" | "for" | "elif" | "while" | "until" => {
                if matches!(state.current_context.as_str(), "else" | "for") {
                    let node = self.descend(&mut state.token_buffer)?;
                    state.components.push_back(Conditional {
                        condition: VecDeque::new(),
                        body: VecDeque::from(vec![node]),
                    });
                } else {
                    let node = self.descend(&mut state.token_buffer)?;
                    debug!("process_command_separator(): Pushing to condition: {:?}", node);
                    state.condition.push_back(node);
                }
            }
            "in" => {
                return Ok(());
            }
            _ => {
                while state.token_buffer.front().is_some() {
                    let node = self.descend(&mut state.token_buffer)?;
                    debug!("process_command_separator(): Pushing to body: {:?}", node);
                    state.body.push_back(node);
                }
                debug!("process_command_separator(): Finalizing component for context '{}'. Adding to components.",
                    state.current_context
                );
                state.components.push_back(Conditional {
                    condition: state.condition.clone(),
                    body: state.body.clone(),
                });
                state.condition.clear();
                state.body.clear();
            }
        }
        Ok(())
    }

    // Step 7: Default token processing
    fn process_default(
        &mut self,
        token: Token,
        block_type: &str,
        state: &mut StructureBuilderState,
    ) {
        if block_type == "for" {
            trace!("process_default(): Processing 'for' block token: {:?}", token);
            state.var_list.push_back(token);
        } else if block_type == "in" {
            let mut expanded = expand(self.shellenv, token);
            while let Some(expanded_token) = expanded.pop_front() {
                state.value_array.push_back(expanded_token.clone());
            }
        } else {
            trace!("process_default(): Default processing for token: {:?}", token);
            state.token_buffer.push_back(token);
        }
    }
}
