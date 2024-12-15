use log::{debug, error, info, trace};
use once_cell::sync::Lazy;
use regex::Regex;
use std::collections::{HashMap, VecDeque};
use std::fmt;
use std::path::PathBuf;

use nix::NixPath;

use crate::shellenv::ShellEnv;

type TokenDeck = VecDeque<Token>;
type NodeDeck = VecDeque<ASTNode>;

const BUILTINS: [&str; 14] = [
    "echo", "set", "shift", "export", "cd", "readonly", "declare", "local", "unset", "trap", "node",
    "exec", "source", "wait",
];

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

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ChainOp {
    And,
    Or,
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
                debug!(
                    "calc_error_data(): concatenating these lines: {}",
                    all_the_lines
                );
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
            debug!(
                "calc_error_data(): concatenating these lines: {}",
                all_the_lines
            );
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
        argv: TokenDeck,
    },
    Builtin {
        argv: TokenDeck,
    },
    Function {
        argv: TokenDeck,
    },
    Assignment {
        var: String,
        val: String,
    },
    Chain {
        left: Box<ASTNode>,
        right: Box<ASTNode>,
        operator: ChainOp,
    },
    Pipeline {
        left: Box<ASTNode>,
        right: Box<ASTNode>,
    },
    If {
        cond_blocks: VecDeque<ASTConditional>,
        else_block: Option<NodeDeck>,
    },
    Loop {
        condition: bool,
        body: ASTConditional,
    },
    For {
        vars: TokenDeck,
        array: TokenDeck,
        body: NodeDeck,
    },
    Case {
        var: Token,
        cases: VecDeque<(Token, NodeDeck)>,
    },
    Select {
        var: Token,
        options: TokenDeck,
        body: NodeDeck,
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

#[derive(Debug)]
pub enum Context {
    Subshell,
    Substitution,
    CmdGroup,
    Test,
    DoubleString,
    SingleString,
    If,
    For,
    While,
    Until,
    Case,
    Select,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WordDesc {
    pub text: String,
    pub abs_pos: usize, // Where the word starts in the input string
    pub span: (usize, usize),
    pub is_cmd: bool,
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
    If {
        structure: Structure,
    },
    For {
        structure: Structure,
    },
    Loop {
        structure: Structure,
    },
    Case {
        structure: Structure,
    },
    Select {
        structure: Structure,
    },
    Test {
        tokens: TokenDeck,
    },
    Subshell {
        tokens: TokenDeck,
    },
    CmdGroup {
        tokens: TokenDeck,
    },
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
        file_target: Option<PathBuf>,
    },
    Keyword,
    Ident,
    Cmdsep,
}

impl Token {
    pub fn new(token_type: TokenType, word_desc: WordDesc) -> Self {
        Self {
            token_type,
            word_desc,
        }
    }

    pub fn from(
        mut word_desc: WordDesc,
        input_man: &mut RshInputManager,
    ) -> Result<Token, RshParseError> {
        let string = word_desc.text.clone();
        debug!("Token::from(): Evaluating token type for: {}", string);

        match string.as_str() {
            _ if REGEX["assignment"].is_match(&string) => {
                Self::create_assignment_token(string, word_desc)
            }
            _ if REGEX["redirection"].is_match(&string) => {
                Self::create_redirection_token(string, word_desc, input_man)
            }
            _ if REGEX["rsh_shebang"].is_match(&string) => {
                word_desc.text = Self::clean_string(string);
                Ok(Token::new(TokenType::Shebang, word_desc))
            }
            _ if REGEX["process_sub"].is_match(&string) => {
                word_desc.text = Self::clean_string(string);
                Ok(Token::new(TokenType::ProcessSub, word_desc))
            }
            _ if REGEX["command_sub"].is_match(&string) => {
                word_desc.text = Self::clean_string(string);
                Ok(Token::new(TokenType::CommandSub, word_desc))
            }
            _ if REGEX["arithmetic"].is_match(&string) => {
                word_desc.text = Self::clean_string(string);
                Ok(Token::new(TokenType::Arithmetic, word_desc))
            }
            _ if REGEX["string"].is_match(&string) => {
                Self::create_string_token(string, word_desc)
            }
            _ if REGEX["operator"].is_match(&string) => {
                Self::create_operator_token(string, word_desc, input_man)
            }
            _ if REGEX["ident"].is_match(&string) => {
                Self::create_path_or_ident(string, word_desc)
            }
            _ => Err(input_man.parse_error("Failed to classify word".into())),
        }
    }

    // Remove escape backslashes
    fn clean_string(string: String) -> String {
        let mut chars = string.chars();
        let mut cleaned = String::new();
        while let Some(char) = chars.next() {
            match char {
                '\\' => {
                    if let Some(escaped_char) = chars.next() {
                        cleaned.push(escaped_char)
                    }
                },
                _ => cleaned.push(char)
            }
        }
        cleaned
    }

    fn create_path_or_ident(string: String, mut word_desc: WordDesc) -> Result<Token, RshParseError> {
        let mut chars = string.chars();
        let mut path: bool = false;
        debug!("checking string for path or ident: {}",string);
        while let Some(char) = chars.next() {
            match char {
                '\\' => { chars.next(); },
                '/' => path = true,
                _ => continue
            }
        }
        if path {
            // Make sure path tokens are only created if they are commands
            // Arguments should be treated as idents
            path = word_desc.is_cmd;
        }
        match path {
            true => Self::create_path_token(string,word_desc),
            false => {
                word_desc.text = Self::clean_string(string);
                Ok(Token::new(TokenType::Ident, word_desc))
            }
        }
    }

    fn create_path_token(string: String, mut word_desc: WordDesc) -> Result<Token, RshParseError> {
        let relative = !string.starts_with('/');
        word_desc.text = Self::clean_string(string);
        Ok(Token::new(TokenType::Path { relative }, word_desc))
    }

    fn create_assignment_token(string: String, mut word_desc: WordDesc) -> Result<Token, RshParseError> {
        let parts: Vec<&str> = string.split('=').collect();
        let (var, val) = (parts[0].into(), parts[1].to_string());
        word_desc.text = Self::clean_string(string);
        Ok(Token::new(TokenType::Assignment { var, val }, word_desc))
    }

    fn create_redirection_token(
        string: String,
        mut word_desc: WordDesc,
        input_man: &mut RshInputManager,
    ) -> Result<Token, RshParseError> {
        if let Some(captures) = REGEX["redirection"].captures(&string) {
            let operator = captures.get(2).map_or("none", |m| m.as_str()).to_string();
            let fd_target = captures
                .get(3)
                .and_then(|m| m.as_str().to_string().parse::<i32>().ok());

            debug!("Token::from(): redir operator: {}", operator);

            if string.matches('<').count() > 3 || string.matches('>').count() > 2 {
                return Err(input_man.parse_error("Invalid redirection operator".into()))
            }

            let mut file_target = None;
            if !string.contains('&') {
                if let Some(word) = input_man.tk.tokens.pop_front() {
                    file_target = Some(PathBuf::from(word.text()));
                }
            }

            let redir_type = match operator.as_str() {
                ">" => RedirType::Output,
                ">>" => RedirType::Append,
                "<" => RedirType::Input,
                "<<" => RedirType::Heredoc,
                "<<<" => RedirType::Herestring,
                _ => return Err(input_man.parse_error("Invalid redirection operator".into()))
            };

            let fd_out = match redir_type {
                RedirType::Output | RedirType::Append => 1,
                RedirType::Input | RedirType::Heredoc | RedirType::Herestring => 0,
            };

            let fd_out = captures
                .get(1)
                .and_then(|m| m.as_str().parse::<i32>().ok())
                .unwrap_or(fd_out);

            if file_target.is_none() && fd_target.is_none() {
                return Err(input_man.parse_error("Couldn't find an output for this redirection".into()))
            }

            word_desc.text = Self::clean_string(string);
            Ok(Token::new(
                TokenType::Redir {
                    redir_type,
                    fd_out,
                    fd_target,
                    file_target,
                },
                word_desc,
            ))
        } else {
            panic!("Token::from(): Redirection somehow succeeded and then failed on the same regex check?");
        }
    }

    fn create_string_token(string: String, mut word_desc: WordDesc) -> Result<Token, RshParseError> {
        word_desc.text = Self::clean_string(string.clone());
        Ok(Token::new(
            TokenType::String {
                single_quote: string.starts_with('\''),
            },
            word_desc,
        ))
    }

    fn create_operator_token(string: String, mut word_desc: WordDesc, input_man: &mut RshInputManager) -> Result<Token, RshParseError> {
        word_desc.text = Self::clean_string(string.clone());
        match string.as_str() {
            "||" => Ok(Token::new(TokenType::Or, word_desc)),
            "&&" => Ok(Token::new(TokenType::And, word_desc)),
            "|" => Ok(Token::new(TokenType::Pipe, word_desc)),
            _ => Err(input_man.parse_error("Invalid operator".into()))
        }
    }

    pub fn substitute(&mut self, new_text: String) {
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

    pub fn is_cmd(&self) -> bool {
        self.word_desc.is_cmd
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Helper function to get the name of the token type as a string.
        fn token_type_name(token_type: &TokenType) -> &'static str {
            match token_type {
                TokenType::Shebang => "Shebang",
                TokenType::ProcessSub => "ProcessSub",
                TokenType::CommandSub => "CommandSub",
                TokenType::Arithmetic => "Arithmetic",
                TokenType::If { .. } => "If",
                TokenType::For { .. } => "For",
                TokenType::Loop { .. } => "Loop",
                TokenType::Case { .. } => "Case",
                TokenType::Select { .. } => "Select",
                TokenType::Test { .. } => "Test",
                TokenType::Subshell { .. } => "Subshell",
                TokenType::CmdGroup { .. } => "CmdGroup",
                TokenType::String { .. } => "String",
                TokenType::Assignment { .. } => "Assignment",
                TokenType::Path { .. } => "Path",
                TokenType::And => "And",
                TokenType::Or => "Or",
                TokenType::Pipe => "Pipe",
                TokenType::Redir { .. } => "Redir",
                TokenType::Keyword => "Keyword",
                TokenType::Ident => "Ident",
                TokenType::Cmdsep => "Cmdsep",
            }
        }

        // Recursive function to format a token and its nested tokens.
        fn fmt_token(
            token: &Token,
            f: &mut fmt::Formatter<'_>,
            prefix: &str,
            is_last: bool,
        ) -> fmt::Result {
            // Determine the connector based on whether this is the last token.
            let connector = if is_last { "└── " } else { "├── " };

            // Get the token name, handling the special case for Loop tokens.
            let token_name = if let TokenType::Loop { structure } = &token.token_type {
                if let Structure::Loop { condition, logic: _ } = structure {
                    match condition {
                        true => "While",
                        false => "Until"
                    }
                } else { unreachable!() }
            } else {
                token_type_name(&token.token_type)
            };

            // Write the token name and its text.
            writeln!(
                f,
                "{}{}{} (Text: '{}')",
                prefix,
                connector,
                token_name,
                token.word_desc.text
            )?;

            // Prepare the new prefix for nested tokens.
            let new_prefix = if is_last {
                format!("{}    ", prefix)
            } else {
                format!("{}│   ", prefix)
            };

            // Handle nested tokens based on the token type.
            match &token.token_type {
                TokenType::Subshell { tokens }
                | TokenType::CmdGroup { tokens }
                | TokenType::Test { tokens } => {
                    for (i, nested_token) in tokens.iter().enumerate() {
                        fmt_token(nested_token, f, &new_prefix, i == tokens.len() - 1)?;
                    }
                }
                TokenType::If { structure }
                | TokenType::For { structure }
                | TokenType::Loop { structure, .. }
                | TokenType::Case { structure }
                | TokenType::Select { structure } => {
                    fmt_structure(structure, f, &new_prefix, true)?;
                }
                _ => {}
            }
            Ok(())
        }

        // Recursive function to format a structure and its nested tokens.
        fn fmt_structure(
            structure: &Structure,
            f: &mut fmt::Formatter<'_>,
            prefix: &str,
            is_last: bool,
        ) -> fmt::Result {
            // Determine the connector based on whether this is the last structure.
            let connector = if is_last { "└── " } else { "├── " };
            writeln!(f, "{}{}Structure:", prefix, connector)?;

            // Prepare the new prefix for nested structures.
            let new_prefix = if is_last {
                format!("{}    ", prefix)
            } else {
                format!("{}│   ", prefix)
            };

            // Handle different types of structures.
            match structure {
                Structure::If { cond_blocks, else_block } => {
                    for (i, cond) in cond_blocks.iter().enumerate() {
                        let is_last_cond = i == cond_blocks.len() - 1 && else_block.is_none();
                        fmt_conditional(cond, f, &new_prefix, is_last_cond)?;
                    }
                    if let Some(else_block) = else_block {
                        writeln!(f, "{}Else Block:", new_prefix)?;
                        for (i, token) in else_block.iter().enumerate() {
                            fmt_token(token, f, &new_prefix, i == else_block.len() - 1)?;
                        }
                    }
                }
                Structure::For { loop_vars, loop_arr, body } => {
                    writeln!(f, "{}Loop Vars:", new_prefix)?;
                    for (i, token) in loop_vars.iter().enumerate() {
                        fmt_token(token, f, &new_prefix, i == loop_vars.len() - 1)?;
                    }
                    writeln!(f, "{}Loop Array:", new_prefix)?;
                    for (i, token) in loop_arr.iter().enumerate() {
                        fmt_token(token, f, &new_prefix, i == loop_arr.len() - 1)?;
                    }
                    writeln!(f, "{}Body:", new_prefix)?;
                    for (i, token) in body.iter().enumerate() {
                        fmt_token(token, f, &new_prefix, i == body.len() - 1)?;
                    }
                }
                Structure::Loop { condition: _, logic } => {
                    fmt_conditional(logic, f, &new_prefix, true)?;
                }
                Structure::Select { target_var, options, body } => {
                    writeln!(f, "{}Target Var: {}",new_prefix,target_var.text())?;
                    writeln!(f, "{}Option Array:", new_prefix)?;
                    for (i,token) in options.iter().enumerate() {
                        fmt_token(token, f, &new_prefix, i == options.len() - 1)?;
                    }
                    writeln!(f, "{}Body:", new_prefix)?;
                    for (i, token) in body.iter().enumerate() {
                        fmt_token(token, f, &new_prefix, i == body.len() - 1)?;
                    }
                }
                Structure::Case { check_string, cases } => {
                    writeln!(f, "{}Case Expression:", new_prefix)?;
                    writeln!(f, "{}{}Case Variable: '{}'", new_prefix, connector, check_string.text())?;

                    for (i, (case_pattern, case_body)) in cases.iter().enumerate() {
                        let is_last_case = i == cases.len() - 1;
                        let case_prefix = if is_last_case {
                            format!("{}    ", new_prefix)
                        } else {
                            format!("{}│   ", new_prefix)
                        };

                        writeln!(f, "{}{}Case Pattern: '{}'", new_prefix, connector, case_pattern.text())?;
                        writeln!(f, "{}Case Body:", case_prefix)?;
                        for (j, token) in case_body.iter().enumerate() {
                            fmt_token(token, f, &case_prefix, j == case_body.len() - 1)?;
                        }
                    }
                }
                _ => {}
            }

            Ok(())
        }

        // Recursive function to format a conditional and its nested tokens.
        fn fmt_conditional(
            conditional: &Conditional,
            f: &mut fmt::Formatter<'_>,
            prefix: &str,
            is_last: bool,
        ) -> fmt::Result {
            // Determine the connector based on whether this is the last conditional.
            let connector = if is_last { "└── " } else { "├── " };
            writeln!(f, "{}{}Conditional:", prefix, connector)?;

            // Prepare the new prefix for nested conditionals.
            let new_prefix = if is_last {
                format!("{}    ", prefix)
            } else {
                format!("{}│   ", prefix)
            };

            // Write the condition and body of the conditional.
            writeln!(f, "{}Condition:", new_prefix)?;
            for (i, token) in conditional.condition.iter().enumerate() {
                fmt_token(token, f, &new_prefix, i == conditional.condition.len() - 1)?;
            }

            writeln!(f, "{}Body:", new_prefix)?;
            for (i, token) in conditional.body.iter().enumerate() {
                fmt_token(token, f, &new_prefix, i == conditional.body.len() - 1)?;
            }

            Ok(())
        }

        // Start the formatting process for the current token.
        fmt_token(self, f, "", true)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RedirType {
    Output,
    Append,
    Input,
    Heredoc,
    Herestring,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Conditional {
    condition: TokenDeck,
    body: TokenDeck,
}

impl Conditional {
    pub fn get_cond(&self) -> TokenDeck {
        self.condition.clone()
    }
    pub fn get_body(&self) -> TokenDeck {
        self.body.clone()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ASTConditional {
    condition: NodeDeck,
    body: NodeDeck,
}

impl ASTConditional {
    pub fn get_cond(&self) -> NodeDeck {
        self.condition.clone()
    }
    pub fn get_body(&self) -> NodeDeck {
        self.body.clone()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Structure {
    If {
        cond_blocks: VecDeque<Conditional>,
        else_block: Option<TokenDeck>,
    },
    For {
        loop_vars: TokenDeck,
        loop_arr: TokenDeck,
        body: TokenDeck,
    },
    Loop {
        condition: bool,
        logic: Conditional,
    },
    Case {
        check_string: Box<Token>,
        cases: VecDeque<(Token, TokenDeck)>,
    },
    Select {
        target_var: Box<Token>,
        options: TokenDeck,
        body: TokenDeck,
    },
}

pub struct Lexer {
    chars: VecDeque<char>,
    abs_pos: usize,         // Character index
    coords: (usize, usize), // Line and Column
    span_start: usize,
}
impl Lexer {
    pub fn new(chars: VecDeque<char>) -> Self {
        Self {
            chars,
            abs_pos: 0,
            coords: (0, 0),
            span_start: 0,
        }
    }
    pub fn advance(&mut self) -> Option<char> {
        if let Some(char) = self.chars.pop_front() {
            self.abs_pos += 1;
            match char {
                '\n' => self.next_line(),
                _ => self.next_col(),
            }
            Some(char)
        } else {
            None
        }
    }
    fn next_line(&mut self) {
        let (mut line, _col) = self.coords;
        line += 1;
        self.coords = (line, 0);
    }
    fn next_col(&mut self) {
        let (line, mut col) = self.coords;
        col += 1;
        self.coords = (line, col);
    }
}
pub struct Tokenizer {
    tokens: TokenDeck,
    buffer: TokenDeck,
}
impl Tokenizer {
    pub fn new() -> Self {
        Self {
            tokens: VecDeque::new(),
            buffer: VecDeque::new(),
        }
    }
}
pub struct Parser {
    ast: VecDeque<ASTNode>,
}
impl Parser {
    pub fn new() -> Self {
        Self {
            ast: VecDeque::new(),
        }
    }
}

// This time I will handle structure logic differently.
// Subshells, Command Substitution, Logic Structures, and Subscopes will all be treated as "subcontexts"
// Regular delimiters like "{" and "[", as well as logical delimiters like "if" and "while" will trigger a recursion
// Words between these delimiters will be collected into a buffer. Words are treated literally here.
// Once the closing delimiter is found, these words will be used as the input for a new RshInputManager.
// The tokens returned by the recursive RshInputManager will go into the corresponding delimited token.
// With this, the prsser will have all of the information necessary to quickly and easily construct logical structures.

pub struct RshInputManager<'a> {
    shellenv: &'a mut ShellEnv,
    input: String,
    lx: Lexer,
    prs: Parser,
    tk: Tokenizer,
}

impl<'a> RshInputManager<'a> {
    pub fn new(input: &str, shellenv: &'a mut ShellEnv) -> Self {
        Self {
            shellenv,
            input: input.into(),
            lx: Lexer::new(input.chars().collect::<VecDeque<char>>()),
            prs: Parser::new(),
            tk: Tokenizer::new(),
        }
    }

    fn get_input_slice(&self, span: (usize, usize)) -> String {
        let (start, mut end) = span;
        if end > self.input.len() {
            end = self.input.len();
        }
        self.input[start..end].into()
    }

    fn build_if(&mut self) -> Result<Structure, RshParseError> {
        let breakers = vec!["elif".to_string(), "else".to_string(), "fi".to_string()];
        let mut cond_blocks = VecDeque::new();
        let mut else_block = None;
        let mut breaker: Token;

        loop {
            let mut condition = self.interpret(Some(vec!["then".into()]),true)?;
            debug!("condition before popping then: {:#?}", condition);
            let then = condition.pop_back().unwrap();
            debug!("condition after popping then: {:#?}", condition);
            debug!("Checking then: {:#?}", then);

            if !then.is_cmd() {
                return Err(self.parse_error("`Then` not properly separated".into()));
            }

            let mut body = self.interpret(Some(breakers.clone()),true)?;
            breaker = body.pop_back().unwrap();
            if !breaker.is_cmd() {
                return Err(self.parse_error(format!("{} not properly separated", breaker.text())));
            }
            debug!("found breaker: {}", breaker.text());

            let block = Conditional { condition, body };
            cond_blocks.push_back(block);

            match breaker.text() {
                "elif" => continue,
                "else" => {
                    let mut tokens = self.interpret(Some(vec!["fi".into()]),true)?;
                    let fi = tokens.pop_back().unwrap(); // pop the fi token
                    if fi.is_cmd() {
                        else_block = Some(tokens);
                    } else {
                        return Err(self.parse_error("Fi not properly separated".into()));
                    }
                    break;
                }
                "fi" => {
                    debug!("found fi, breaking out of loop(?)");
                    break;
                }
                _ => {
                    return Err(self.parse_error(format!(
                        "Expected one of these: {:?}, found this: {:?}",
                        breakers, breaker
                    )));
                }
            }
        }

        let structure = Structure::If {
            cond_blocks,
            else_block,
        };
        debug!("returning structure from build_if: {:?}", structure);
        Ok(structure)
    }

    fn build_for(&mut self) -> Result<Structure, RshParseError> {
        let loop_vars: TokenDeck = self.clean_interpret(Some(vec!["in".into()]), false)?;
        let loop_arr: TokenDeck = self.clean_interpret(Some(vec!["do".into()]), false)?;

        // Safely get the last token and check if it ends with a valid separator
        if let Some(last_token) = loop_arr.back() {
            let text = last_token.text();
            if !text.ends_with(';') && !text.ends_with('\n') {
                error!("Broke on this loop array: {:?}", loop_arr);
                return Err(self.parse_error("`do` not properly separated".into()));
            }
        } else {
            return Err(self.parse_error("Empty loop array".into()));
        }

        let body = self.interpret(Some(vec!["done".into()]), true)?;
        if !body.back().unwrap().is_cmd() {
            error!("Broke on this loop body: {:?}", body);
            return Err(self.parse_error("`done` not properly separated".into()));
        }

        Ok(Structure::For {
            loop_vars,
            loop_arr,
            body,
        })
    }

    fn build_loop(&mut self, loop_condition: bool) -> Result<Structure, RshParseError> {
        let condition = self.interpret(Some(vec!["do".into()]),true)?;
        let body = self.interpret(Some(vec!["done".into()]),true)?;
        let logic = Conditional { condition, body };
        Ok(Structure::Loop {
            condition: loop_condition,
            logic,
        })
    }

    fn build_case(&mut self) -> Result<Structure, RshParseError> {
        let mut var = self.clean_interpret(Some(vec!["in".into()]), false)?;
        let mut cases: VecDeque<(Token, TokenDeck)> = VecDeque::new();
        if var.len() != 1 {
            return Err(self.parse_error(format!("Expected one variable in case statement, got {}",var.len())));
        }
        loop {
            let mut condition = self.interpret(Some(vec!["esac".into(), ")".into()]), false)?;
            debug!("got condition: {:?}", condition);
            if condition.back().is_some() && condition.back().unwrap().text() == "esac" {
                break;
            } else if condition.back().is_none() {
                return Err(self.parse_error("No case blocks found in case statement".into()));
            }
            let body = self.interpret(Some(vec![";;".into()]), true)?;
            debug!("got body: {:?}", body);
            cases.push_back((condition.pop_back().unwrap(), body));
        }
        Ok(Structure::Case {
            check_string: Box::new(var.pop_back().unwrap()),
            cases,
        })
    }

    fn build_select(&mut self) -> Result<Structure,RshParseError> {
        let mut target = self.clean_interpret(Some(vec!["in".into()]), false)?;
        let target_var;
        if target.len() != 1 {
            return Err(self.parse_error(format!("Expected one variable in case statement, got {}",target.len())));
        } else {
            target_var = Box::new(target.pop_back().unwrap());
        };
        let mut options: TokenDeck = self.clean_interpret(Some(vec!["do".into()]), false)?;
        if let Some(last_token) = options.back() {
            let text = last_token.text();
            if !text.ends_with(';') && !text.ends_with('\n') {
                error!("Broke on this option array: {:?}", options);
                return Err(self.parse_error("`do` not properly separated".into()));
            }
            options.pop_back(); // Consume command separator
        } else {
            return Err(self.parse_error("Empty option array".into()));
        }

        let body = self.interpret(Some(vec!["done".into()]), true)?;
        if !body.back().unwrap().is_cmd() {
            error!("Broke on this loop body: {:?}", body);
            return Err(self.parse_error("`done` not properly separated".into()));
        }
        Ok(Structure::Select { target_var, options, body })
    }

    fn check_context(&self, string: &str) -> Option<Context> {
        match string {
            "(" => Some(Context::Subshell),
            "$" => Some(Context::Substitution),
            "{" => Some(Context::CmdGroup),
            "[" => Some(Context::Test),
            "\"" => Some(Context::DoubleString),
            "'" => Some(Context::SingleString),
            "if" => Some(Context::If),
            "for" => Some(Context::For),
            "while" => Some(Context::While),
            "until" => Some(Context::Until),
            "case" => Some(Context::Case),
            "select" => Some(Context::Select),
            _ => None,
        }
    }

    fn handle_string(&mut self, double: bool) -> Result<Token, RshParseError> {
        let string_start = self.lx.abs_pos;
        let mut string = String::new();
        loop {
            let mut c = self.lx.advance();
            debug!("popped char in string: {}", c.unwrap());
            debug!("self.lx.chars: {:?}", self.lx.chars);
            match c {
                Some('\\') => {
                    c = self.lx.advance();
                    string.push(c.unwrap());
                }
                Some('\'') if !double => break,
                Some('"') if double => break,
                Some(_) => string.push(c.unwrap()),
                None => {
                    return Err(self.parse_error("Unclosed string".into()));
                }
            }
        }
        Ok(Token {
            token_type: TokenType::String {
                single_quote: double,
            },
            word_desc: WordDesc {
                text: string,
                abs_pos: string_start,
                span: (string_start, self.lx.abs_pos),
                is_cmd: false,
            },
        })
    }

    fn span_start(&self) -> usize {
        self.lx.span_start
    }

    fn create_token_for_context(
        &self,
        context: Context,
        is_cmd: bool,
        span_start: usize,
        span_end: usize,
        structure: Structure,
    ) -> Result<Token, RshParseError> {
        let span = (span_start, span_end);
        let token_type = match context {
            Context::If => TokenType::If { structure },
            Context::For => TokenType::For { structure },
            Context::While => TokenType::Loop {
                structure,
            },
            Context::Until => TokenType::Loop {
                structure,
            },
            Context::Case => TokenType::Case { structure },
            Context::Select => TokenType::Select { structure },
            _ => unreachable!(),
        };

        Ok(Token {
            token_type,
            word_desc: WordDesc {
                text: self.get_input_slice(span),
                abs_pos: span_start,
                span,
                is_cmd,
            },
        })
    }

    fn handle_simple_structures(&mut self, context: Context, is_cmd: bool) -> Result<Token,RshParseError> {
        let span_start = self.span_start();
        let tokens: TokenDeck;
        let token_type: TokenType;

        if let Context::DoubleString | Context::SingleString = context {
            return self.handle_string(matches!(context, Context::DoubleString));
        }

        if let Context::Substitution = context {
            let token = self.interpret(Some(vec![" ".into(), "\n".into(), "\t".into(), ";".into()]),true)?.pop_front();
            if token.is_some() {
                if let Some(key) = token {
                    let span = (span_start, self.pos());
                    let replacement = self.shellenv.get_variable(key.text()).map_or("",|s| s).to_string();
                    let word_desc = WordDesc {
                        text: replacement,
                        abs_pos: span_start,
                        span,
                        is_cmd,
                    };
                    return Ok(Token { token_type: TokenType::String { single_quote: true }, word_desc })
                }
            }
        }

        match context {
            Context::Subshell => {
                tokens = self.interpret(Some(vec![")".into()]),true)?;
                token_type = TokenType::Subshell { tokens };
            }
            Context::CmdGroup => {
                tokens = self.interpret(Some(vec!["}".into()]),true)?;
                token_type = TokenType::CmdGroup { tokens };
            }
            Context::Test => {
                tokens = self.interpret(Some(vec!["]".into()]),true)?;
                token_type = TokenType::Test { tokens };
            }
            _ => unreachable!()
        }
        let span = (span_start, self.pos());
        let word_desc = WordDesc {
            text: self.get_input_slice(span),
            abs_pos: span_start,
            span,
            is_cmd,
        };
        Ok(Token {
            token_type,
            word_desc
        })
    }

    fn dispatch_context(&mut self, context: Context, is_cmd: bool) -> Result<Token, RshParseError> {
        let span_start = self.span_start();
        if matches!(context,Context::Subshell | Context::CmdGroup | Context::Test | Context::DoubleString | Context::SingleString | Context::Substitution) {
            return self.handle_simple_structures(context,is_cmd);
        }
        let (structure, span_end) = match context {
            Context::If => {
                let structure = self.build_if()?;
                (structure, self.lx.abs_pos)
            }
            Context::For => {
                let structure = self.build_for()?;
                (structure, self.lx.abs_pos)
            }
            Context::While => {
                let structure = self.build_loop(true)?;
                (structure, self.lx.abs_pos)
            }
            Context::Until => {
                let structure = self.build_loop(false)?;
                (structure, self.lx.abs_pos)
            }
            Context::Case => {
                let structure = self.build_case()?;
                (structure, self.lx.abs_pos)
            }
            Context::Select => {
                let structure = self.build_select()?;
                (structure, self.lx.abs_pos)
            }
            _ => unreachable!()
        };
        self.create_token_for_context(context, is_cmd, span_start, span_end, structure)
    }

    fn parse_error(&self, msg: String) -> RshParseError {
        RshParseError {
            abs_pos: self.pos(),
            span: self.span(),
            msg,
            input: self.input.clone(),
        }
    }

    fn get_word_desc(&self, string: &String, is_cmd: bool) -> WordDesc {
        WordDesc {
            text: string.into(),
            abs_pos: self.span_start(),
            span: (self.span_start(), self.lx.abs_pos),
            is_cmd,
        }
    }

    fn create_token(&mut self, string: &String, is_cmd: bool) -> Result<Token, RshParseError> {
        let word_desc = self.get_word_desc(string, is_cmd);
        Token::from(word_desc, self)
    }

    fn get_separator(&mut self) -> Token {
        Token {
            token_type: TokenType::Cmdsep,
            word_desc: WordDesc {
                text: ";".into(),
                abs_pos: self.pos(),
                span: (self.pos(), self.pos()),
                is_cmd: false,
            },
        }
    }

    pub fn pos(&self) -> usize {
        self.lx.abs_pos
    }

    pub fn span(&self) -> (usize, usize) {
        (self.span_start(), self.pos())
    }

    pub fn clean_deck(&self, deck: &mut TokenDeck) {
        let mut buffer = VecDeque::new();
        while let Some(token) = deck.pop_front() {
            if !token.text().is_empty() {
                buffer.push_back(token);
            }
        }
        deck.extend(buffer.drain(..));
    }

    pub fn clean_interpret(
        &mut self,
        break_on: Option<Vec<String>>,
        recurse: bool
    ) -> Result<TokenDeck, RshParseError> {
        let mut tokens = self.interpret(break_on,recurse)?;
        tokens.pop_back();
        Ok(tokens)
    }

    fn create_and_push_token(&mut self, current_word: &String, is_cmd: bool, current_deck: &mut TokenDeck) -> Result<(), RshParseError> {
        let token = self.create_token(current_word, is_cmd)?;
        current_deck.push_back(token);
        self.lx.span_start = self.lx.abs_pos;
        Ok(())
    }

    fn handle_context(&mut self, context: Context, is_cmd: bool, current_deck: &mut TokenDeck) -> Result<(), RshParseError> {
        let ctx_token = self.dispatch_context(context, is_cmd)?;
        current_deck.push_back(ctx_token);
        Ok(())
    }

    pub fn build_ast(&mut self) -> Result<NodeDeck,RshParseError> {
        let tokens = self.interpret(None, true)?;
        let ast = self.parse(tokens)?;
        Ok(ast)
    }

    fn break_interpretation(&self, current_word: &String, break_patterns: &Vec<String>) -> bool {
        break_patterns.contains(current_word)
    }

    // `break_on` defines a list of patterns to break the interpretation upon finding
    // `recurse` tells the interpreter whether or not it is allowed to recurse upon finding a
    // structural keyword like `if` and `for`, used for contexts where these words are treated literally
    pub fn interpret(&mut self, break_on: Option<Vec<String>>, recurse: bool) -> Result<TokenDeck, RshParseError> {
        let break_patterns = break_on.unwrap_or_default();
        let mut is_cmd = true;
        let mut current_word = String::new();
        let mut current_deck = VecDeque::new();
        while let Some(c) = self.lx.advance() {
            trace!("found char: {}", c);

            // Check for simple context indicators like '(', '$', etc
            if self.check_context(&c.to_string()).is_some() && recurse {
                let context = self.check_context(&c.to_string()).unwrap();
                debug!("Dispatching context: {:?}", context);
                self.lx.span_start = self.lx.abs_pos;
                self.handle_context(context, is_cmd, &mut current_deck)?;
                continue
            }

            // Check for simple break patterns
            if break_patterns.contains(&c.to_string()) {
                debug!("breaking on '{}'!",c);
                self.create_and_push_token(&current_word, is_cmd, &mut current_deck)?;
                // Do not consume command separators
                if matches!(c, ';' | '\n') {
                    self.lx.chars.push_front(';');
                }
                current_word.clear();
                self.clean_deck(&mut current_deck);
                return Ok(current_deck);
            };
            match c {
                '\\' => { // Escaping
                    if let Some(&next_char) = self.lx.chars.front() {

                        // We will clean up the left-over escape slashes later
                        current_word.push(c);
                        if !matches!(next_char, '\n') {
                            current_word.push(next_char);
                        }
                    }
                    self.lx.advance();
                }
                '#' if self.lx.chars.front() != Some(&'!') => { // Comments

                    while let Some(&next_char) = self.lx.chars.front() {
                        if next_char == '\n' {
                            self.lx.advance();
                            break;
                        } else {
                            self.lx.advance();
                        }
                    }
                }
                ';' | '\n' => { // Command separators

                    // Exit early if a case statement separator is found in a case block
                    if let Some(';') = self.lx.chars.front() {
                        if break_patterns.contains(&";;".into()) {
                            debug!("breaking case statement parse");
                            self.create_and_push_token(&current_word, is_cmd, &mut current_deck)?;
                            debug!("pushing last token: {:?}",current_word);
                            current_deck.push_back(self.get_separator());
                            current_word.clear();
                            self.lx.advance();
                            self.clean_deck(&mut current_deck);
                            return Ok(current_deck);
                        }
                    }
                    if !current_word.is_empty() {
                        debug!("break_patterns.contains word: {}", break_patterns.contains(&current_word));

                        // Check for complex context indicators like "if", "for", etc.
                        if self.check_context(&current_word).is_some() && recurse {
                            let context = self.check_context(&current_word).unwrap();
                            debug!("Dispatching context: {:?}", context);
                            self.handle_context(context, is_cmd, &mut current_deck)?;
                        } else {

                            // Create generic token
                            self.create_and_push_token(&current_word, is_cmd, &mut current_deck)?;
                        }
                        debug!(
                            "break_patterns: {:?}, current_word: {}",
                            break_patterns, current_word
                        );
                        if self.break_interpretation(&current_word, &break_patterns) {
                            debug!("breaking on '{}'!",current_word);
                            current_word.clear();
                            self.clean_deck(&mut current_deck);
                            return Ok(current_deck);
                        }
                        current_word.clear();
                    }
                    is_cmd = true;
                    let token = self.get_separator();
                    debug!("pushing token: {:?}", token);
                    current_deck.push_back(token);
                    self.lx.span_start = self.lx.abs_pos;
                    current_word.clear();
                }
                ' ' | '\t' => {
                    if !current_word.is_empty() {
                        debug!(
                            "break_patterns.contains word: {}",
                            break_patterns.contains(&current_word)
                        );
                        if self.check_context(&current_word).is_some() && recurse {
                            let context = self.check_context(&current_word).unwrap();
                            debug!("Dispatching context: {:?}", context);
                            self.handle_context(context, is_cmd, &mut current_deck)?;
                            is_cmd = true;
                        } else {
                            self.create_and_push_token(&current_word, is_cmd, &mut current_deck)?;
                            is_cmd = false;
                        }
                        if self.break_interpretation(&current_word, &break_patterns) {
                            self.clean_deck(&mut current_deck);
                            return Ok(current_deck);
                        }
                        current_word.clear();
                        self.lx.span_start = self.lx.abs_pos;
                    }
                }
                '|' => {
                    debug!("tokenize(): found pipe");

                    if !current_word.is_empty() {
                        self.create_and_push_token(&current_word, is_cmd, &mut current_deck)?;
                        self.lx.span_start = self.lx.abs_pos;
                        current_word.clear();
                    }

                    current_word.push(c);
                    debug!(
                        "tokenize(): Pushed {} into current_word, current_word = {}",
                        c, current_word
                    );

                    if let Some(&next_char) = self.lx.chars.front() {
                        match next_char {
                            '|' => {

                                current_word.push(self.lx.advance().unwrap());
                                self.create_and_push_token(&current_word, is_cmd, &mut current_deck)?;
                                self.lx.span_start = self.lx.abs_pos;
                                current_word.clear();
                            }
                            _ => {
                                debug!("tokenize(): Returning single pipe: {}", current_word);

                                debug!("tokenize(): Pushing word to block: {}", current_word);
                                self.create_and_push_token(&current_word, is_cmd, &mut current_deck)?;
                                self.lx.span_start = self.lx.abs_pos;
                                current_word.clear();
                                self.lx.advance();
                            }
                        }
                        is_cmd = true;
                    }
                }
                '&' if matches!(*self.lx.chars.front().unwrap(), '&') => {
                    debug!("tokenize(): found && operator");

                    if !current_word.is_empty() {
                        debug!("tokenize(): Pushing word to block: {}", current_word);
                        self.create_and_push_token(&current_word, is_cmd, &mut current_deck)?;
                        self.lx.span_start = self.lx.abs_pos;
                        current_word.clear();
                    }
                    current_word = "&&".into();
                    self.lx.advance();
                    self.create_and_push_token(&current_word, is_cmd, &mut current_deck)?;
                    self.lx.span_start = self.lx.abs_pos;
                    current_word.clear();
                    is_cmd = true;
                }
                '&' | '0'..='9' | '>' | '<' => {
                    debug!("tokenize(): Maybe found redirection?");
                    current_word.push(c);
                    let mut redir_check_stack = self.lx.chars.clone();
                    let mut redir_test_string: String = "".into();
                    let mut count = 0;

                    while let Some(check_char) = redir_check_stack.pop_front() {
                        debug!(
                            "tokenize(): Checking character for redir test: {}",
                            check_char
                        );
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

                            let redir_char = self.lx.advance().unwrap();
                            debug!("tokenize(): count = {}, adding \'{}\'", count, redir_char);
                            current_word.push(redir_char);
                            count -= 1
                        }
                        self.create_and_push_token(&current_word, is_cmd, &mut current_deck)?;
                        self.lx.span_start = self.lx.abs_pos;
                        current_word.clear();
                    }
                }
                _ => current_word.push(c),
            }
        }
        if !current_word.is_empty() {
            if !break_patterns.is_empty() && !self.break_interpretation(&current_word, &break_patterns) {

                return Err(self.parse_error(format!("Expected one of {:?}, found this: {}", break_patterns, current_word)));
            } else {
                self.create_and_push_token(&current_word, is_cmd, &mut current_deck)?;
                self.lx.span_start = self.lx.abs_pos;
                current_word.clear();
            }
        }
        self.clean_deck(&mut current_deck);
        Ok(current_deck)
    }

    pub fn parse(&mut self,mut tokens: TokenDeck) -> Result<NodeDeck,RshParseError> {
        let mut node_deck: NodeDeck = VecDeque::new();
        while let Some(token) = tokens.pop_front() {
            match token.class() {
                TokenType::Ident | TokenType::String { .. } | TokenType::Path { .. } => {
                    tokens.push_front(token);
                    let node = self.parse_invocation(&mut tokens)?;
                    node_deck.push_back(node);
                }
                TokenType::If { structure } => {
                    let mut node_cond_blocks = VecDeque::new();
                    let mut node_else_block = None;
                    if let Structure::If { cond_blocks, else_block } = structure {
                        for block in cond_blocks {
                            let node_conditional = ASTConditional {
                                condition: self.parse(block.get_cond())?,
                                body: self.parse(block.get_body())?
                            };
                            node_cond_blocks.push_back(node_conditional);
                        }
                        if let Some(block) = else_block {
                            node_else_block = Some(self.parse(block.clone())?);
                        }
                    } else { unreachable!() }
                    let node = ASTNode::If { cond_blocks: node_cond_blocks, else_block: node_else_block };
                    node_deck.push_back(node);
                }
                TokenType::For { structure } => {
                    let vars: TokenDeck;
                    let array: TokenDeck;
                    let node_body: NodeDeck;
                    if let Structure::For { loop_vars, loop_arr, body } = structure {
                        vars = loop_vars.clone();
                        array = loop_arr.clone();
                        node_body = self.parse(body.clone())?;
                    } else { unreachable!() }
                    let node = ASTNode::For { vars, array, body: node_body };
                    node_deck.push_back(node);
                }
                TokenType::Loop { structure } => {
                    let loop_type;
                    let node_body;
                    if let Structure::Loop { condition, logic } = structure {
                        let token_condition = logic.get_cond();
                        let token_body = logic.get_body();
                        loop_type = condition;
                        node_body = ASTConditional {
                            condition: self.parse(token_condition)?,
                            body: self.parse(token_body)?
                        };
                    } else { unreachable!() }
                    let node = ASTNode::Loop { condition: *loop_type, body: node_body };
                    node_deck.push_back(node);
                }
                TokenType::Case { structure } => {
                    let var: Token;
                    let mut node_cases: VecDeque<(Token, NodeDeck)> = VecDeque::new();
                    if let Structure::Case { check_string, cases } = structure {
                        var = *check_string.clone();
                        for case in cases {
                            let pattern = case.0.clone();
                            let node_deck = self.parse(case.1.clone())?;
                            let new_case = (pattern,node_deck);
                            node_cases.push_back(new_case);
                        }
                    } else { unreachable!() }
                    let node = ASTNode::Case { var, cases: node_cases };
                    node_deck.push_back(node);
                }
                TokenType::Select { structure } => {
                    let node_var;
                    let node_options;
                    let node_body;
                    if let Structure::Select { target_var, options, body } = structure {
                        node_var = *target_var.clone();
                        node_options = options;
                        node_body = self.parse(body.clone())?;
                    } else { unreachable!() }
                    let node = ASTNode::Select { var: node_var, options: node_options.clone(), body: node_body };
                    node_deck.push_back(node);
                }
                _ => unimplemented!("Logic not yet implemented for this token: {}",token)
            }
        }
        todo!()
    }
    fn parse_invocation(&mut self, tokens: &mut TokenDeck) -> Result<ASTNode,RshParseError> {
        // This function uses two double-ended queues to parse the tokens efficiently.
        // Nodes are popped from tokens, checked for a condition, then popped into the buffer
        // If the condition is met, the loop breaks and the two resulting 'sides' are operated on.
        let mut buffer: TokenDeck = VecDeque::new();
        while let Some(token) = tokens.pop_front() {
            match token.class() {
                TokenType::And => {
                    return Ok(ASTNode::Chain {
                        left: Box::new(self.parse_invocation(&mut buffer)?),
                        right: Box::new(self.parse_invocation(tokens)?),
                        operator: ChainOp::And
                    })
                },
                TokenType::Or => {
                    return Ok(ASTNode::Chain {
                        left: Box::new(self.parse_invocation(&mut buffer)?),
                        right: Box::new(self.parse_invocation(tokens)?),
                        operator: ChainOp::Or
                    })
                },
                TokenType::Cmdsep => break,
                _ => continue
            }
        }
        buffer.extend(tokens.drain(..)); // Reset the token deck for the next check
        tokens.extend(buffer.drain(..)); // Do it like this to keep the tokens in the right order

        while let Some(token) = tokens.pop_front() {
            match token.class() {
                TokenType::Pipe => {
                    return Ok(ASTNode::Pipeline {
                        left: Box::new(self.parse_invocation(&mut buffer)?),
                        right: Box::new(self.parse_invocation(tokens)?)
                    })
                },
                TokenType::Cmdsep => break,
                _ => continue
            }
        }
        buffer.extend(tokens.drain(..));
        tokens.extend(buffer.drain(..));

        let mut argv: TokenDeck = VecDeque::new();
        while let Some(token) = tokens.pop_front() {
            match token.class() {
                TokenType::Cmdsep => {
                    let cmd_name = argv.front().unwrap().text();
                    match cmd_name {
                        _ if BUILTINS.contains(&cmd_name) => {
                            return Ok(ASTNode::Builtin { argv })
                        }
                        _ if FUNCTIONS.contains(&cmd_name) => {
                            return Ok(ASTNode::Function { argv })
                        }
                        _ => {
                            return Ok(ASTNode::Command { argv })
                        }
                    }
                }
                TokenType::Ident | TokenType::String {..} | TokenType::Path {..} => {
                    argv.push_back(token);
                }
                _ => return Err(self.parse_error(format!("Found this while looking for arguments: {}", token.text())))
            }
        }
        Err(self.parse_error("Reached bottom of invocation parsing without discovering a valid pattern".into()))
    }
}
