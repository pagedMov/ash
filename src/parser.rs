use log::{info,trace,error,debug};
use std::mem;
use std::collections::{HashSet,VecDeque};
use tokio::sync::mpsc;
use tokio::task::JoinHandle;
use regex::Regex;

use crate::event::{self,ShellEvent};
use crate::event::ShellError;

static BUILTINS: [&str; 6] = ["cd", "echo", "exit", "export", "alias", "unset"];

// TODO: organize the functions and stuff in this file, stuff is kind of just thrown all over the place
// TODO: verify syntax before executing nodes


#[derive(Debug,Clone,PartialEq)]
pub enum ASTNode {
    Builtin {
        name: String,
        args: Vec<String>,
        redirs: Vec<Redirection>,
    },
    CmdChain {
        left: Box<ASTNode>,
        right: Box<ASTNode>,
        operator: ChainOp
    },
    ShCommand {
        name: String,
        args: Vec<String>,
        redirs: Vec<Redirection>,
    },
    Pipeline {
        commands: Vec<ASTNode>,
    },
    Conditional {
        paths: Vec<ConditionalPath>,
    },
    Loop {
        condition: Box<ASTNode>,
        loopvar_identifier: String,
        loopvar_value: String,
        body: Vec<ASTNode>,
    },
    Eof {}
}

#[derive(Debug,Clone,PartialEq)]
pub enum ChainOp {
    And,
    Or
}

#[derive(Debug,Clone,PartialEq)]
pub struct ConditionalPath {
    condition: Option<Box<ASTNode>>,
    body: Box<ASTNode>,
}

impl ConditionalPath {
    pub fn new(condition_optional: Option<ASTNode>, body: ASTNode) -> Self {
        let condition = condition_optional.map(Box::new);
        Self {
            condition,
            body: Box::new(body),
        }
    }

    pub fn get_cond(&self) -> &Option<Box<ASTNode>> {
        &self.condition
    }

    pub fn get_body(&self) -> &ASTNode {
        &self.body
    }
}

#[derive(Debug,Clone,PartialEq)]
pub struct Redirection {
    direction: RedirDestination,
    flags: Vec<RedirFlag>
}

impl Redirection {

    pub fn new(direction: RedirDestination, flags: Vec<RedirFlag>) -> Self {
        Redirection { direction, flags }
    }
    pub fn get_direction(&self) -> RedirDestination {
        self.direction.clone()
    }
    pub fn get_flags(&self) -> Vec<RedirFlag> {
        self.flags.clone()
    }
    pub fn set_flag(&mut self, flag: RedirFlag) {
        self.flags.push(flag);
    }
}

#[derive(Debug,Clone,PartialEq)]
pub enum RedirFlag {
    File(String),
    Append,
    Both,
    CloseFd,
    FdOut(u32),
    ToFd(u32),
}

#[derive(Debug, Clone, PartialEq)]
pub enum RedirDestination {
    Input,          // < file
    Output,         // > file
}

#[derive(Clone,Debug,PartialEq,Hash,Eq)]
pub enum Token {
    Word(String),
    AnyWord,
    Redir(String,String,String),
    AnyRedir,
    Fd(String),
    AnyFd,
    RedirOperator(String),
    RedirTarget(String),
    Macro(String),
    AnyMacro,
    Shebang,
    Semicolon,
    Newline,
    Pipe,
    And,
    Or,
    If,
    Then,
    Elif,
    Else,
    Fi,
    For,
    While,
    Until,
    Do,
    Done,
    Eof,
    Case,
    In,
    Esac,
    Select,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Break,
    Continue,
    Asterisk,
    QuestionMark,
    Null
}

impl Token {
    pub fn from_char(ch: char) -> Token {
        match ch {
            ' ' | '\t' => Token::Null,
            '|' => Token::Pipe,
            '\n' => Token::Newline,
            ';' => Token::Semicolon,
            _ => Token::Null,
        }
    }
    pub fn grouping_family() -> Vec<Token> {
        vec![
            Token::LeftParen,    // `(`
            Token::RightParen,   // `)`
            Token::LeftBrace,    // `{`
            Token::RightBrace,   // `}`
        ]
    }
    pub fn block_prefixes() -> Vec<Token> {
        vec![
            Token::If,
            Token::For,
            Token::While,
            Token::Until,
            Token::Case,
            Token::Select,
        ]
    }
    pub fn control_flow_family() -> Vec<Token> {
        vec![
            Token::Break,
            Token::Continue
        ]
    }
    pub fn glob_family() -> Vec<Token> {
        vec![
            Token::Asterisk,
            Token::QuestionMark
        ]
    }
    pub fn delimiter_family() -> Vec<Token> {
        vec![
            Token::Newline,
            Token::Semicolon,
            Token::Eof
        ]
    }
    pub fn operator_family() -> Vec<Token> {
        vec![
            Token::AnyRedir,
            Token::AnyFd,
            Token::Pipe,
            Token::And,
            Token::Or,
        ]
    }
    pub fn if_family() -> Vec<Token> {
        // `If` is in block_prefixes
        vec![
            Token::Then,
            Token::Elif,
            Token::Else,
            Token::Fi,
        ]
    }
    pub fn case_family() -> Vec<Token> {
        vec![
            Token::In,
            Token::Esac
        ]
    }
    pub fn loop_family() -> Vec<Token> {
        vec![
            Token::Do,
            Token::Done
        ]
    }
    pub fn all_variants() -> Vec<Token> {
        vec![
            Token::AnyWord,
            Token::AnyRedir,
            Token::AnyFd,
            Token::Semicolon,
            Token::Newline,
            Token::Pipe,
            Token::And,
            Token::Or,
            Token::If,
            Token::Then,
            Token::Elif,
            Token::Else,
            Token::Fi,
            Token::For,
            Token::While,
            Token::Until,
            Token::Do,
            Token::Done,
            Token::Eof,
            Token::Null
        ]
    }
}



pub struct Tokenizer {
    chars: VecDeque<char>,
    outbox: mpsc::Sender<Token>,
    redir_regex: Regex,
    expecting: HashSet<String>,         // Tracks unclosed delimiters
    expecting_immediate: Option<HashSet<Token>>, // Tracks immediate expectations
    col: i32,
    row: i32
}

impl Tokenizer {
    /// Creates a new Tokenizer instance
    pub fn new(input: &str, outbox: mpsc::Sender<Token>) -> Self {
        Self {
            chars: VecDeque::from(input.chars().collect::<Vec<char>>()),
            outbox,
            redir_regex: Regex::new(r"([\d&]*)(>>?|<|<&|>&)\s*([^\s;]*)").unwrap(),

            // Keeps track of delimiters
            expecting: HashSet::new(),
            // Predicts which tokens are valid next
            expecting_immediate: None,

            // Parsing position for detailed parse error output
            col: 1,
            row: 1
        }
    }

    fn get_pos(&self) -> String {
        format!("{};{}",self.row,self.col)
    }

    fn step(&mut self) {
        if let Some(char) = self.chars.pop_front() {
            trace!("Popped character: {}",char);
            match char {
                '\n' => {
                    self.row += 1;
                    self.col = 1;
                }
                _ => self.col += 1,
            }
        }
    }

    fn step_count(&mut self,steps: usize) {
        for _ in 0..steps {
            self.step()
        }
    }



    /// Tokenizes the input and sends tokens to the parser
    pub async fn tokenize(&mut self) -> Result<(), ShellError> {
        let mut tokens: VecDeque<Token> = VecDeque::new();
        loop {
            let ch: char = if self.chars.front().is_none() {
                tokens.push_back(Token::Eof);
                break;
            } else {
                *self.chars.front().unwrap()
            };
            debug!("tokenizer: checking character: {:?}", ch);

            // Skip whitespace
            if matches!(ch, ' ' | '\t') {
                self.step();
                continue;
            }

            let mut token: Token = Token::Null;

            // Build a word if the character isn't a single-character token
            if matches!(ch, '|' | '\n' | ';') {
                // Single-character tokens
                match ch {
                    '|' => {
                        if self.chars[1] != '|' {
                            self.step();
                            token = Token::Pipe;
                        } else {
                            self.step_count(2);
                            token = Token::Or;
                        }
                    }
                    '\n' => {
                        self.step();
                        token = Token::Newline;
                    }
                    ';' => {
                        self.step();
                        token = Token::Semicolon;
                    }
                    _ => unreachable!(),
                }
            } else {
                // Handle words
                let word = self.build_word().map_err(|err| {
                    error!("Error building word at {}: {:?}", self.get_pos(), err);
                    err
                })?;

                // Check if the word is a reserved keyword
                if let Some(keyword_token) = self.handle_reserved_word(&word)? {
                    token = keyword_token;
                } else {
                    // TODO: handle unwrap (important!)
                    token = self.handle_non_reserved_word(word).unwrap();
                    debug!("tokenize: Received redirection: {:?}",token);
                }
            }

            // Perform the expectation check on the token
            let checked_token = match &token {
                Token::Word(..) => Token::AnyWord,
                Token::Redir(..) => Token::AnyRedir,
                _ => token.clone(),
            };

            if self.expecting_immediate.is_some() && !self.is_expected_token(&checked_token) {
                let expected_tokens = self.expecting_immediate.clone().unwrap();
                return Err(ShellError::InvalidSyntax(format!(
                    "Syntax error at {}: Expected one of {:?}, found {:?}",
                    self.get_pos(),
                    expected_tokens,
                    token
                )));
            }

            // Handle expectations for the current token
            self.handle_expectations(&checked_token);

            // Push the token to the tokens queue
            tokens.push_back(token);
        }
        if !tokens.is_empty() {
            self.send_tokens(tokens).await?;
        } else {
            error!("Tokens emptied before Eof");
            return Err(ShellError::InvalidSyntax(format!("Syntax error at {}: Unexpected end of input",self.get_pos())));
        }
        // Final check for unclosed delimiters
        if !self.expecting.is_empty() {
            return Err(ShellError::InvalidSyntax(format!(
                "Syntax error at {}: Finished parsing with unclosed delimiters: {:?}",
                self.get_pos(),self.expecting
            )));
        }

        // Send the Eof token once the input is exhausted
        Ok(())
    }

    fn expect_next(&mut self, whitelist: bool, expected_tokens: &[Token]) {
        let new_set: HashSet<Token> = if whitelist {
            expected_tokens.iter().cloned().collect()
        } else { // Invert
            let all_variants: HashSet<Token> = Token::all_variants().iter().cloned().collect();
            let not_expected_set: HashSet<Token> = expected_tokens.iter().cloned().collect();

            all_variants.difference(&not_expected_set).cloned().collect()
        };
        self.expecting_immediate = Some(new_set);
    }

    fn is_expected_token(&mut self, mut token: &Token) -> bool {
        debug!("Checking expectations with token: {:?}",token);
        match token {
            Token::Word(..) => {
                token = &Token::AnyWord;
            }
            Token::Redir(..) => {
                token = &Token::AnyRedir
            }
            _ => {}
        }
        if let Some(expected_tokens) = &self.expecting_immediate {
            debug!("Current expectation: {:?}",expected_tokens);
            expected_tokens.contains(token)
        } else {
            debug!("No current expectations");
            false
        }
    }

    fn handle_expectations(&mut self, current_token: &Token) {
        match current_token {
            Token::If | Token::Elif | Token::Then => {
                self.expect_next(true,&[Token::AnyWord]);
            }
            Token::Fi | Token::Done | Token::Esac => {
                self.expect_next(true,&Token::delimiter_family());
            }
            Token::Pipe => {
                self.expect_next(true,&[Token::AnyWord]);
            }
            Token::AnyWord => {
                let mut tokens: Vec<Token> = vec![Token::AnyWord];
                tokens.extend(Token::operator_family());
                tokens.extend(Token::delimiter_family());
                self.expect_next(true,&tokens);
            }
            Token::Semicolon => {
                let mut tokens: Vec<Token> = vec![];
                tokens.extend(Token::operator_family());
                tokens.extend(vec![Token::Semicolon]);
                self.expect_next(false,&tokens);
            }
            Token::And | Token::Or  => {
                let mut tokens: Vec<Token> = vec![Token::AnyWord];
                tokens.extend(Token::block_prefixes());
                self.expect_next(true, &tokens);
            }
            _ => self.expecting_immediate = None,
        }
    }

    fn handle_reserved_word(&mut self, word: &str) -> Result<Option<Token>, ShellError> {
        let mut token: Option<Token> = None;
        debug!("Checking for reserved word with: {}",word);
        match word.trim() {
            "if" => {
                token = Some(Token::If);
                self.expecting.insert("fi".to_string());
                Ok(token)
            }
            "then" => {
                token = Some(Token::Then);
                Ok(token)
            }
            "fi" => {
                if !self.expecting.remove("fi") {
                    return Err(ShellError::InvalidSyntax(
                        format!("Syntax error at {}: Unexpected 'fi' without matching 'if'", self.get_pos()),
                    ));
                }
                token = Some(Token::Fi);
                Ok(token)
            }
            "elif" => {
                if !self.expecting.contains("fi") {
                    return Err(ShellError::InvalidSyntax(
                        format!("Syntax error at {}: Unexpected 'elif' without matching 'if'", self.get_pos()),
                    ));
                }
                token = Some(Token::Elif);
                Ok(token)
            }
            "else" => {
                if !self.expecting.contains("fi") {
                    return Err(ShellError::InvalidSyntax(
                        format!("Syntax error at {}: Unexpected 'else' without matching 'if'", self.get_pos()),
                    ));
                }
                token = Some(Token::Else);
                Ok(token)
            }
            "||" => {
                token = Some(Token::Or);
                Ok(token)
            }
            "&&" => {
                token = Some(Token::And);
                Ok(token)
            }
            "for" | "while" | "until" => {
                token = Some(Token::For); // Use appropriate token
                Ok(token)
            }
            "do" => {
                token = Some(Token::Do);
                Ok(token)
            }
            "done" => {
                token = Some(Token::Done);
                Ok(token)
            }
            _ => Ok(token), // Not a reserved word
        }
    }

    fn handle_non_reserved_word(
        &mut self,
        word: String
    ) -> Option<Token> {
        debug!("Checking word: {}", word);

        if word.contains(['<', '>', '&']) || word.parse::<u32>().is_ok() {
            if let Some(captures) = self.redir_regex.captures(word.as_str()) {
                debug!("Found redirection, parsing...");

                let fd = if let Some(m) = captures.get(1) {
                    let val = m.as_str();
                    if val.is_empty() { "1" } else { val }
                } else { "1" };
                let operator = captures.get(2).map_or("", |m| m.as_str());
                let mut target: String = captures.get(3).map_or("".to_string(), |m| m.as_str().to_string());

                if target.is_empty() {
                    self.skip_space();
                    target = self.build_word().unwrap();
                    self.step_count(target.chars().count());
                }

                let redir = Some(Token::Redir(operator.into(), fd.into(), target));
                debug!("Returning redirection: {:?}",redir);
                redir
            } else { unreachable!() }
        } else if !word.is_empty() {
            Some(Token::Word(word))
        } else { None }
    }

    fn in_double_quotes(&self) -> bool {
        self.expecting.contains("\"")
    }

    fn in_single_quotes(&self) -> bool {
        self.expecting.contains("'")
    }

    fn in_any_quotes(&self) -> bool {
        self.in_single_quotes() || self.in_double_quotes()
    }

    fn build_word(&mut self) -> Result<String, ShellError> {
        let mut word = String::new();

        while let Some(&c) = self.chars.front() {
            match c {
                '"' if !self.in_single_quotes() => {
                    if self.in_double_quotes() {
                        self.expecting.remove("\"");
                    } else {
                        self.expecting.insert("\"".into());
                    }
                    self.step()
                }
                '\'' if !self.in_double_quotes() => {
                    if self.in_single_quotes() {
                        self.expecting.remove("'");
                    } else {
                        self.expecting.insert("'".into());
                    }
                    self.step()
                }
                ';' | '|' | '\n' | ' ' | '\t' if !self.in_any_quotes() => {
                    break;
                }
                _ => {
                    word.push(c);
                    self.step()
                }
            }
        }

        Ok(word)
    }

    fn skip_space(&mut self) {
        while let Some(&ch) = self.chars.front() {
            match ch {
                ' ' | '\t' => {
                    self.step();
                }
                _ => return,
            }
        }
    }

    async fn send_tokens(&self, mut tokens: VecDeque<Token>) -> Result<(), ShellError> {
        while let Some(token) = tokens.pop_front() {
            info!("tokenizer: Sending token: {:?}", token);
            self.outbox.send(token).await.map_err(|err| {
                error!("Failed to send token: {:?}", err);
                ShellError::InvalidSyntax(format!("Syntax error at {}: {}",self.get_pos(),err))
            })?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct Parser {
    // Token currently being parsed
    token: Token,

    // Use a VecDeque so I can easily send nodes in the order they were built
    // VecDeque contains JoinHandles given from tokio::spawn() calls; better than awaiting each spawn() immediately
    // and then pushing which might cause a race condition

    // Communication between tokenizer and parser
    token_sender: mpsc::Sender<Token>,
    token_receiver: mpsc::Receiver<Token>,

    // Channel for internally communicating with sub-parsers
    // Sub-parsers are spawned to handle individual ASTNodes concurrently
    // external_node_sender is sent to sub-parsers to get nodes back from them
    internal_node_receiver: Option<mpsc::Receiver<JoinHandle<Option<ASTNode>>>>,
    internal_node_sender: mpsc::Sender<JoinHandle<Option<ASTNode>>>,
    // Channel to send ASTNodeskback to the main event loop
    node_outbox: mpsc::Sender<event::ShellEvent>,

}

impl Parser {
    pub fn new(output: mpsc::Sender<ShellEvent>, super_parser_channel: Option<mpsc::Sender<JoinHandle<Option<ASTNode>>>>) -> Self {
        let (token_sender, token_receiver) = mpsc::channel(100);

        // If a Sender is given, don't make a receiver
        let (internal_node_receiver, internal_node_sender) = if let Some(sender) = super_parser_channel {
            (None, sender)
        } else {
            let (new_sender, new_receiver) = mpsc::channel(100);
            (Some(new_receiver), new_sender)
        };

        Self {
            // Initialize token as Token::Null instead of making it an Option type
            // just so I don't have to write 'Some(token)' everywhere
            // dont @ me
            token: Token::Null,
            token_sender,
            token_receiver,
            internal_node_receiver,
            internal_node_sender,
            node_outbox: output,
        }
    }

    /// Abbreviation function
    ///
    /// Just returns a clone of the node_outbox field
    pub fn outbox(&self) -> mpsc::Sender<ShellEvent> {
        self.node_outbox.clone()
    }

    /// Wrapper function
    ///
    /// Spawns an instance of the tokenizer, and an instance of parse_input()
    /// The tokenizer sends tokens to parse_input() via the token_sender/receiver fields
    pub async fn handle_input(&mut self, input: String) {
        // Clone required fields
        let input = input.clone();
        let token_sender = self.token_sender.clone();

        // Spawn the tokenizer
        let tokenizer_result = tokio::spawn(async move {
            let mut tokenizer = Tokenizer::new(&input, token_sender);
            tokenizer.tokenize().await
        })
        .await;

        // Handle results
        if let Err(join_error) = tokenizer_result {
            self.report_error(ShellError::IoError(join_error.to_string())).await;
            return;
        }

        match tokenizer_result.unwrap() {
            Ok(_) => {
                if let Err(parse_error) = self.parse_input().await {
                    self.report_error(ShellError::ParsingError(parse_error.to_string())).await;
                }
            }
            Err(tokenize_error) => {
                self.report_error(ShellError::InvalidSyntax(tokenize_error.to_string())).await;
            }
        }
        let _ = self.node_outbox.send(ShellEvent::Prompt).await;
    }

    /// Helper method to send errors to the node outbox
    async fn report_error(&self, error: ShellError) {
        if let Err(send_error) = self.node_outbox.send(ShellEvent::CatchError(error)).await {
            error!("Failed to report error: {:?}", send_error);
        }
    }

    /// The main parsing logic
    ///
    /// Gets tokens incrementally from the tokenizer, and then does stuff with them
    /// Spawns a NodeDispatcher in a subroutine, NodeDispatcher exists to send AST nodes
    /// asynchronously. This allows for tokenizing, parsing, and returning to happen incrementally
    /// and in parallel. Input is essentially evaluated as it is read, which flattens the entire
    /// parsing process.
    pub async fn parse_input(&mut self) -> Result<(), ShellError> {
        debug!("parse_input: Starting parse_input");

        loop {
            debug!("parse_input: Current token: {:?}", self.token);

            match self.token {
                Token::Null => debug!("parse_input: Skipping Null token"),
                Token::Eof => {
                    // Send Eof ASTNode to NodeDispatcher
                    break;
                }
                _ => match self.token {
                    Token::Semicolon | Token::Newline => {
                        debug!("Skipping orphaned delimiter: {:?}",self.token);
                        self.next_token().await;
                        continue; // Doesn't mean anything out here
                    }
                    // CONDITIONALS
                    Token::If => {
                        debug!("parse_input: Detected 'If' token. Parsing conditional...");
                        let mut conditional_tokens: VecDeque<Token> = VecDeque::new();
                        loop {
                            if self.token == Token::Eof {
                                // TODO: make sure these errors are actually being handled
                                // whereever they are being sent to
                                return Err(ShellError::InvalidSyntax("'fi' token not found in conditional".into()));
                            }
                            conditional_tokens.push_back(self.token.clone());
                            if self.token == Token::Fi {
                                break;
                            } else {
                                self.next_token().await;
                            }
                        }

                        self.delegate_conditional(conditional_tokens).await;
                    }

                    // PIPELINES
                    _ => {
                        let mut pipeline_tokens: VecDeque<Token> = VecDeque::new();
                        let mut chains: VecDeque<VecDeque<Token>> = VecDeque::new();
                        loop {
                            match self.token {
                                Token::Eof | Token::Semicolon | Token::Newline => {
                                    debug!("parse_input: Reached delimiter in pipeline token parsing");
                                    break;
                                }
                                Token::And | Token::Or => {
                                    debug!("Chain token found, moving tokens: {:?}",pipeline_tokens);

                                    // Push current pipeline, and then push the operator
                                    // produces output like [[pipeline1],[And],[pipeline2]]
                                    chains.push_back(mem::take(&mut pipeline_tokens));
                                    chains.push_back(VecDeque::from(vec![self.token.clone()]));
                                    self.next_token().await;
                                }
                                _ => {
                                    pipeline_tokens.push_back(self.token.clone());
                                    self.next_token().await;
                                }
                            }
                        }
                        if chains.is_empty() { // Parse single pipeline
                            debug!("parse_input: Delegating to parse_pipeline for token: {:?}", self.token);
                            self.delegate_pipeline(pipeline_tokens).await;
                        } else {
                            debug!("parse_input: Delegating to parse_pipeline for token: {:?}", self.token);
                            chains.push_back(mem::take(&mut pipeline_tokens)); // Push final pipeline
                            self.delegate_chains(chains).await;
                        }
                    }
                }
            }

            // TODO: remember why this even needs to be here
            if self.token == Token::Eof {
                break;
            }
            self.next_token().await;
        }

        // TODO: verify syntax before doing this
        // TODO: handle errors instead of using `let _ =`
        let outbox = self.outbox();
        if let Some(ref mut rx) = self.internal_node_receiver {
            while let Ok(task) = rx.try_recv() {
                let node = task.await;
                if let Err(e) = node {
                    panic!("parse_input: Error while awaiting JoinHandle: {}",e)
                } else {
                    let _ = outbox.send(ShellEvent::NewASTNode(node.unwrap().unwrap())).await;
                }
            }
        }
        Ok(())
    }

    /// Helper method for popping tokens sent from the tokenizer
    async fn next_token(&mut self) {
        if self.token != Token::Eof {
            debug!("next_token: Fetching next token...");
            self.token = self.token_receiver.recv().await.unwrap_or(Token::Eof);
            info!("next_token: Token received: {:?}", self.token);
        } else {
            debug!("next_token: Reached Eof signal");
        }
    }

    // These three functions create sub-parsers that send ASTNodes back to this parser.
    // Any sub-parsers created by these functions also send directly to this one
    async fn delegate_pipeline(&mut self, tokens: VecDeque<Token>) {
        let super_parser_channel = self.internal_node_sender.clone();
        let super_parser_outbox = self.outbox();
        let _ = self.internal_node_sender.send(
            tokio::spawn(async move {
                let mut sub_parser = Parser::new(super_parser_outbox,Some(super_parser_channel));
                sub_parser.parse_pipeline(tokens).await
            })
        ).await;
    }

    async fn delegate_conditional(&mut self, tokens: VecDeque<Token>) {
        let super_parser_channel = self.internal_node_sender.clone();
        let super_parser_outbox = self.outbox();
        let _ = self.internal_node_sender.send(
            tokio::spawn(async move {
                let mut sub_parser = Parser::new(super_parser_outbox,Some(super_parser_channel));
                sub_parser.parse_conditional(tokens).await
            })
        ).await;
    }

    async fn delegate_chains(&mut self, tokens: VecDeque<VecDeque<Token>>) {
        let super_parser_channel = self.internal_node_sender.clone();
        let super_parser_outbox = self.outbox();
        let _ = self.internal_node_sender.send(
            tokio::spawn(async move {
                let mut sub_parser = Parser::new(super_parser_outbox,Some(super_parser_channel));
                sub_parser.parse_chains(tokens).await
            })
        ).await;
    }

    async fn parse_conditional(&mut self, mut tokens: VecDeque<Token>) -> Option<ASTNode> {
        debug!("parse_conditional: Starting parsing with tokens: {:?}", tokens);

        let mut paths = Vec::new();

        while let Some(token) = tokens.front() {
            debug!("parse_conditional: Processing token: {:?}", token);

            match token {
                Token::If | Token::Elif => {
                    debug!("parse_conditional: Detected conditional start: {:?}", token);

                    // Parse condition
                    let condition = self.conditional_helper(&mut tokens, "condition").await?;
                    debug!("parse_conditional: Parsed condition: {:?}", condition);

                    // Parse path for the condition
                    let path = self.conditional_helper(&mut tokens, "path").await?;
                    debug!("parse_conditional: Parsed path: {:?}", path);

                    paths.push(ConditionalPath::new(Some(condition), path));
                    debug!("parse_conditional: Added conditional path to paths: {:?}", paths);
                }
                Token::Else => {
                    debug!("parse_conditional: Detected 'Else' token. Parsing else path...");

                    // Parse else path
                    let else_path = self.conditional_helper(&mut tokens, "path").await?;
                    debug!("parse_conditional: Parsed else path: {:?}", else_path);

                    paths.push(ConditionalPath::new(None, else_path));
                    debug!("parse_conditional: Added else path to paths: {:?}", paths);
                }
                Token::Fi => {
                    debug!("parse_conditional: Detected 'Fi' token. Ending conditional parsing.");
                    break;
                }
                _ => {
                    error!("parse_conditional: Unexpected token in conditional: {:?}", token);
                    return None;
                }
            }
        }

        debug!("parse_conditional: Completed parsing. Paths: {:?}", paths);
        let node = Some(ASTNode::Conditional { paths });
        trace!("parse_conditional: Returning node: {:#?}",node);
        node
    }

    async fn conditional_helper(&mut self, tokens: &mut VecDeque<Token>, body_type: &str) -> Option<ASTNode> {
        let mut pipeline_tokens = VecDeque::new();
        let mut chains: VecDeque<VecDeque<Token>> = VecDeque::new();
        debug!("parse_condition: Starting condition parsing with tokens: {:?}", tokens);

        match body_type {
            "condition" => {
                if let Some(keyword) = tokens.pop_front() {
                    if !matches!(keyword, Token::If | Token::Elif) {
                        error!("Tried to parse if/elif block with unexpected keyword: {:?}",keyword);
                        return None;
                    }
                }
            }
            "path" => {
                if let Some(keyword) = tokens.pop_front() {
                    if !matches!(keyword, Token::Then | Token::Else) {
                        error!("Reached parse_path with an unexpected keyword: {:?}",keyword);
                        return None;
                    }
                }
            }
            _ => unreachable!()
        }

        while let Some(token) = tokens.front() {
            debug!("parse_condition: Inspecting token: {:?}", token);

            match token {
                Token::Semicolon | Token::Newline | Token::Eof => {
                    debug!("parse_condition: Detected delimiter token: {:?}", token);
                    tokens.pop_front();
                    break;
                }
                Token::And | Token::Or => {
                    debug!("parse_condition: Detected chain token: {:?}", token);
                    chains.push_back(std::mem::take(&mut pipeline_tokens));
                    chains.push_back(VecDeque::from(vec![token.clone()]));
                    tokens.pop_front();
                    debug!("parse_condition: Updated chains: {:?}", chains);
                }
                _ => {
                    debug!("parse_condition: Adding token to pipeline tokens: {:?}", token);
                    pipeline_tokens.push_back(token.clone());
                    tokens.pop_front();
                }
            }
        }

        if chains.is_empty() {
            debug!("parse_condition: Parsing single pipeline: {:?}", pipeline_tokens);
            self.parse_pipeline(pipeline_tokens).await
        } else {
            debug!("parse_condition: Parsing chained pipelines: {:?}", chains);
            chains.push_back(std::mem::take(&mut pipeline_tokens));
            self.parse_chains(chains).await
        }
    }

    /// Parses command chaining like cmd1 && cmd2 || cmd3
    ///
    /// the `chains` vector looks like this:
    /// [[cmd1,cmd2],[And],[cmd1,cmd2,cmd3],[Or],[cmd1]]
    ///
    /// The function steps through the chains vector grabbing elements two at a time
    /// With each iteration, the previous CmdChain is attached to the left side of the new one
    async fn parse_chains(&mut self, chains: VecDeque<VecDeque<Token>>) -> Option<ASTNode> {
        debug!("Parsing chains: {:?}", chains);

        let mut iter = chains.into_iter();
        let mut left = self.parse_pipeline(iter.next()?).await?;

        while let Some(operator_tokens) = iter.next() {
            let operator = match operator_tokens.front()? {
                Token::And => ChainOp::And,
                Token::Or => ChainOp::Or,
                _ => unreachable!(),
            };

            let right = self.parse_pipeline(iter.next()?).await?;
            left = ASTNode::CmdChain {
                left: Box::new(left),
                right: Box::new(right),
                operator,
            };
        }

        Some(left)
    }

    async fn parse_one() {
        // TODO: implement this
        // This function will be used to quickly parse a single line and return the output
        // Will be used for $(substitution) and subshells in general
        todo!()
    }

    /// Parses commands
    ///
    /// If only one command is found, it just returns the ShCommand node,
    /// If several are found, it returns a Pipeline node containing ShCommands
    ///
    /// Called from parse_input()
    async fn parse_pipeline(&mut self, mut tokens: VecDeque<Token>) -> Option<ASTNode> {
        debug!("parse_pipeline: Parsing iteratively");

        let mut stack: Vec<ASTNode> = Vec::new();

        let mut args = Vec::new();
        let mut redirs = Vec::new();

        while let Some(token) = tokens.pop_front() {
            match token {
                Token::Word(name) => {
                    debug!("parse_pipeline: Found command: {}", name);

                    while let Some(token) = tokens.front() {
                        match token {
                            Token::Word(arg) => {
                                debug!("parse_pipeline: Found argument: {}", arg);
                                args.push(arg.clone());
                                tokens.pop_front();
                            }
                            Token::Redir(operator, fd, target) => {
                                debug!("parse_pipeline: Found redirection: {:?}", token);
                                // Build redirection (similar logic to before)
                                let mut redir = match operator.as_str() {
                                    "<" => Redirection::new(RedirDestination::Input, vec![]),
                                    ">" => Redirection::new(RedirDestination::Output, vec![]),
                                    ">>" => {
                                        let mut r = Redirection::new(RedirDestination::Output, vec![]);
                                        r.set_flag(RedirFlag::Append);
                                        r
                                    }
                                    _ => unreachable!(),
                                };

                                if fd != "&" {
                                    redir.set_flag(RedirFlag::FdOut(fd.parse().unwrap()));
                                }

                                if target.starts_with('&') {
                                    redir.set_flag(RedirFlag::ToFd(target.strip_prefix('&')?.parse().unwrap()));
                                } else {
                                    redir.set_flag(RedirFlag::File(target.into()));
                                }

                                redirs.push(redir);
                                tokens.pop_front();
                            }
                            _ => break,
                        }
                    }

                    debug!("parse_pipeline: Finished parsing command: {}", name);

                    if BUILTINS.contains(&name.as_str()) {
                        stack.push(ASTNode::Builtin {
                            name,
                            args: std::mem::take(&mut args),
                            redirs: std::mem::take(&mut redirs),
                        });
                    } else {
                        stack.push(ASTNode::ShCommand {
                            name,
                            args: std::mem::take(&mut args),
                            redirs: std::mem::take(&mut redirs),
                        });
                    }
                }
                Token::Pipe => {
                    debug!("parse_pipeline: Found pipe '|', starting new pipeline");
                }
                Token::Semicolon | Token::Newline | Token::Eof => {
                    debug!("parse_pipeline: End of pipeline detected");
                    break;
                }
                _ => {
                    debug!("parse_pipeline: Unexpected token: {:?}", token);
                    return None;
                }
            }
        }

        if stack.len() == 1 {
            Some(stack.pop().unwrap())
        } else if !stack.is_empty() {
            Some(ASTNode::Pipeline { commands: stack })
        } else {
            debug!("parse_pipeline: No commands found.");
            None
        }
    }
}
