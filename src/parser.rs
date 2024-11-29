use log::{info,trace,error,debug};
use std::mem;
use std::collections::VecDeque;
use tokio::sync::mpsc;
use tokio::task::JoinHandle;
use regex::Regex;

use crate::event::{self,ShellEvent};
use crate::event::ShellError;

static _BUILTINS: [&str; 6] = ["cd", "echo", "exit", "export", "alias", "unset"];


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
    Or,
    Null // Used for the first pipeline in a chain
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

#[derive(Clone,Debug,PartialEq)]
pub enum Token {
    Word(String),
    Redir(String,String,String),
    Fd(String),
    RedirOperator(String),
    RedirTarget(String),
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
    Null
}


fn build_word(
    chars: &mut VecDeque<char>
) -> String {
    let mut singlequote = false;
    let mut doublequote = false;
    let mut word = String::new();

    while let Some(&c) = chars.front() {
        trace!(
            "Processing character: '{}' | Singlequote: {} | Doublequote: {} | Current word: '{}'",
            c, singlequote, doublequote, word
        );

        match c {
            '"' if !singlequote => {
                doublequote = !doublequote;
                trace!(
                    "Toggled doublequote to {}. Skipping character: '\"'",
                    doublequote
                );
                chars.pop_front();
            }
            '\'' if !doublequote => {
                singlequote = !singlequote;
                trace!(
                    "Toggled singlequote to {}. Skipping character: '\''",
                    singlequote
                );
                chars.pop_front();
            }
            ';' | '|' | '\n' | ' ' | '\t' if !singlequote && !doublequote => {
                trace!(
                    "Encountered delimiter '{}' while not inside quotes. Returning word: '{}'",
                    c, word
                );
                break;

            }
            _ => {
                word.push(c);
                trace!("Added '{}' to word. Current word: '{}'", c, word);
                chars.pop_front();
            }
        }
    }

    word
}

pub fn skip_space(chars: &mut VecDeque<char>) {
    while let Some(&ch) = chars.front() {
        match ch {
            ' ' | '\t' => { chars.pop_front(); },
            _ => { return; }
        }
    }
}

/// Reads input one character at a time, creating tokens from words and symbols
///
/// Tokens are then used by the Parser to create ASTNodes to send back to the
/// shell's main event loop
pub async fn tokenize(input: &str, outbox: mpsc::Sender<Token>) -> Result<(), ShellError> {

    // TODO: handle escaping with backslashes

    let mut chars = VecDeque::from(input.chars().collect::<Vec<char>>());
    let redirection_re = Regex::new(r"([\d&]*)(>>?|<|<&|>&)\s*([^\s;]*)").unwrap();

    while let Some(&ch) = chars.front() {
        debug!("tokenizer: checking character: {:?}",ch);
        let mut word = String::new();
        let mut tokens: VecDeque<Token> = VecDeque::new();
        match ch {
            ' ' | '\t' => {
                chars.pop_front();
                continue;
            }
            '|' => { // Handles both pipes and '||' chain operators
                if chars[1] != '|' {
                    tokens.push_back(Token::Pipe);
                    chars.pop_front();
                } else {
                    tokens.push_back(Token::Or);
                    chars.pop_front();
                }
            }
            '\n' => {
                tokens.push_back(Token::Newline);
                chars.pop_front();
            }
            ';' => {
                tokens.push_back(Token::Semicolon);
                chars.pop_front();
            }
            _ => {
                word = build_word(&mut chars);

                // Reserved words
                match word.as_str() {
                    "if" => { tokens.push_back(Token::If); }
                    "then" => { tokens.push_back(Token::Then); }
                    "elif" => { tokens.push_back(Token::Elif); }
                    "else" => { tokens.push_back(Token::Else); }
                    "fi" => { tokens.push_back(Token::Fi); }
                    "&&" => { tokens.push_back(Token::And); }
                    "for" => { tokens.push_back(Token::For); }
                    "while" => { tokens.push_back(Token::While); }
                    "until" => { tokens.push_back(Token::Until); }
                    "do" => { tokens.push_back(Token::Do); }
                    "done" => { tokens.push_back(Token::Done); }
                    _ => {
                        debug!("Checking word: {}",word.as_str());
                        // Only run regex on stuff that looks like a redirection
                        if word.contains(['<', '>', '&']) || word.parse::<u32>().is_ok() {
                            if let Some(captures) = redirection_re.captures(word.as_str()) {
                                debug!("Found redirection, parsing...");
                                let mut fd = captures.get(1).map_or("1", |m| m.as_str());
                                let operator = captures.get(2).map_or("", |m| m.as_str());
                                let mut target: String = captures.get(3).map_or("".to_string(), |m| m.as_str().to_string());

                                // TODO: handle unwrap
                                if fd.is_empty() {
                                    fd = "1";
                                }

                                // TODO: implement differentiation between >&1 and > &1
                                if target.is_empty() {
                                    skip_space(&mut chars);
                                    target = build_word(&mut chars);
                                    // TODO: handle this
                                    if target.is_empty() { panic!("No target for redirection"); }
                                }

                                tokens.push_back(Token::Redir(operator.into(),fd.into(),target));

                            }
                        } else {
                            debug!("No redirection regex match, pushing_back word");
                            if word.is_empty() { break; }
                            tokens.push_back(Token::Word(word.clone()));
                        }
                    }
                }
            }
        }
        if !tokens.is_empty() {
            // send token from tokenizer to parser
            while let Some(token) = tokens.pop_front() {
                info!("tokenizer: Sending token: {:?}",token);
                // TODO: handle errors here
                outbox.send(token).await.unwrap();
            }
        } else {
            error!("Tokens emptied before Eof");
            return Err(ShellError::InvalidSyntax(word));
        }
    }

    // Send the Eof token once the input is exhausted
    info!("tokenizer: Sending Eof token");
    outbox.send(Token::Eof).await.unwrap();
    Ok(())
}

// These have to be helper functions instead of Parser methods, in order for async to work
// This is because they can't be referenced inside of the tokio::spawn() context
// If they are tied to the Parser struct

/// Lord forgive me for what I am about to do
async fn parse_conditional(tokens: Vec<Token>) -> Option<ASTNode> {
    let mut tokens = VecDeque::from(tokens);
    debug!("parse_conditional: Starting parsing with tokens: {:?}", tokens);

    let mut paths = Vec::new();

    while let Some(token) = tokens.front() {
        debug!("parse_conditional: Processing token: {:?}", token);

        match token {
            Token::If | Token::Elif => {
                debug!("parse_conditional: Detected conditional start: {:?}", token);

                // Parse condition
                let condition = conditional_helper(&mut tokens, "condition").await?;
                debug!("parse_conditional: Parsed condition: {:?}", condition);

                // Parse path for the condition
                let path = conditional_helper(&mut tokens, "path").await?;
                debug!("parse_conditional: Parsed path: {:?}", path);

                paths.push(ConditionalPath::new(Some(condition), path));
                debug!("parse_conditional: Added conditional path to paths: {:?}", paths);
            }
            Token::Else => {
                debug!("parse_conditional: Detected 'Else' token. Parsing else path...");

                // Parse else path
                let else_path = conditional_helper(&mut tokens, "path").await?;
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
    Some(ASTNode::Conditional { paths })
}

async fn conditional_helper(tokens: &mut VecDeque<Token>, body_type: &str) -> Option<ASTNode> {
    let mut pipeline_tokens = Vec::new();
    let mut chains = Vec::new();
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
                chains.push(std::mem::take(&mut pipeline_tokens));
                chains.push(vec![token.clone()]);
                tokens.pop_front();
                debug!("parse_condition: Updated chains: {:?}", chains);
            }
            _ => {
                debug!("parse_condition: Adding token to pipeline tokens: {:?}", token);
                pipeline_tokens.push(token.clone());
                tokens.pop_front();
            }
        }
    }

    if chains.is_empty() {
        debug!("parse_condition: Parsing single pipeline: {:?}", pipeline_tokens);
        parse_pipeline(pipeline_tokens).await
    } else {
        debug!("parse_condition: Parsing chained pipelines: {:?}", chains);
        chains.push(std::mem::take(&mut pipeline_tokens));
        parse_chains(chains).await
    }
}

/// Parses command chaining like cmd1 && cmd2 || cmd3
///
/// the `chains` vector looks like this:
/// [[cmd1,cmd2],[And],[cmd1,cmd2,cmd3],[Or],[cmd1]]
///
/// The function steps through the chains vector grabbing elements two at a time
/// With each iteration, the previous CmdChain is attached to the left side of the new one
async fn parse_chains(chains: Vec<Vec<Token>>) -> Option<ASTNode> {
    debug!("Parsing chains: {:?}",chains);
    let mut left = parse_pipeline(chains[0].clone()).await?;
    for i in (1..chains.len()).step_by(2) {
        let operator = match chains[i].as_slice() {
            [Token::And] => ChainOp::And,
            [Token::Or] => ChainOp::Or,
            _ => unreachable!()
        };

        let right = parse_pipeline(chains[i+1].clone()).await?;
        left = ASTNode::CmdChain { left: Box::new(left), right: Box::new(right), operator }
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
async fn parse_pipeline(tokens: Vec<Token>) -> Option<ASTNode> {
    debug!("parse_pipeline: Parsing iteratively");

    let mut tokens = VecDeque::from(tokens);
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
                stack.push(ASTNode::ShCommand {
                    name,
                    args: std::mem::take(&mut args),
                    redirs: std::mem::take(&mut redirs),
                });
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

#[derive(Debug)]
pub struct NodeDispatcher {
    inbox: mpsc::Receiver<JoinHandle<Option<ASTNode>>>,
    outbox: mpsc::Sender<ShellEvent>,
}

impl NodeDispatcher {
    /// Create a new NodeDispatcher.
    pub fn new(inbox: mpsc::Receiver<JoinHandle<Option<ASTNode>>>, outbox: mpsc::Sender<ShellEvent>) -> Self {
        Self {
            inbox,
            outbox,
        }
    }

    /// Start processing tasks in the correct order.
    pub async fn start(&mut self) {
        debug!("node_dispatcher.start: Starting Node Dispatcher event loop");
        loop {
            if let Some(handle) = self.inbox.recv().await {
                debug!("Awaiting handle: {:?}",handle);
                let handle = handle.await;
                debug!("Received from handle: '{:?}'",handle);
                match handle {
                    Ok(Some(node)) => {
                        match node {
                            ASTNode::Eof {} => {
                                // Return to prompt
                                let _ = self.outbox.send(ShellEvent::Prompt).await;
                                break;
                            }
                            _ => {
                                // Handle successful node creation
                                debug!("node_dispatcher.start: Sending built AST node: {:?}",node);
                                if let Err(err) = self
                                    .outbox
                                    .send(ShellEvent::NewASTNode(node)) // Send the created node
                                    .await
                                {
                                    error!("node_dispatcher.start: Failed to send AST node: {:?}", err);
                                }
                            }
                        }
                    }
                    Ok(None) => {
                        // TODO: properly handle case where parser returns None
                        error!("node_dispatcher.start: Node task failed");
                    }
                    Err(join_error) => {
                        // Handle task panics
                        error!("node_dispatcher.start: Task panicked: {:?}", join_error);
                    }
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct Parser {
    // Token currently being parsed
    token: Token,

    // Stdin
    input: String,

    // Communication between tokenizer and parser
    token_sender: mpsc::Sender<Token>,
    token_receiver: mpsc::Receiver<Token>,

    // Channel to send ASTNodes back to the main event loop
    node_outbox: mpsc::Sender<event::ShellEvent>,

}

impl Parser {
    pub fn new(input: String, output: mpsc::Sender<ShellEvent>) -> Self {
        let (token_sender, token_receiver) = mpsc::channel(100);
        Self {
            // Initialize token as Token::Null instead of making it an Option type
            // just so I don't have to write 'Some(token)' everywhere
            // dont @ me
            token: Token::Null,
            input,
            token_sender,
            token_receiver,
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
    pub async fn handle_input(&mut self) -> Result<(), ShellError> {
        let input = self.input.clone();
        let token_sender = self.token_sender.clone();

        // Spawn async process for the tokenizer
        // Sends tokens to parser via the token_sender channel
        tokio::spawn(async move {
            if let Err(e) = tokenize(&input, token_sender).await {
                error!("Tokenizer error: {:?}", e);
            }
        });

        // Parse tokens incrementally as they arrive from the tokenizer
        self.parse_input().await
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

        // Channel for sending nodes to the dispatcher
        let (node_sender,node_receiver) = mpsc::channel(100);
        let mut node_dispatcher = NodeDispatcher::new(node_receiver,self.outbox());
        tokio::spawn(async move {
            node_dispatcher.start().await;
        });

        loop {
            debug!("parse_input: Current token: {:?}", self.token);

            match self.token {
                Token::Null => debug!("parse_input: Skipping Null token"),
                Token::Eof => {
                    // Send Eof ASTNode to NodeDispatcher
                    self.stop_dispatcher(node_sender.clone()).await;
                    break;
                }
                _ => match self.token {

                    // CONDITIONALS
                    Token::If => {
                        debug!("parse_input: Detected 'If' token. Parsing conditional...");
                        let mut conditional_tokens: Vec<Token> = vec![];
                        while self.token != Token::Fi {
                            if self.token == Token::Eof {
                                // TODO: make sure these errors are actually being handled
                                // whereever they are being sent to
                                return Err(ShellError::InvalidSyntax("'fi' token not found in conditional".into()));
                            }
                            conditional_tokens.push(self.token.clone());
                            self.next_token().await;
                        }

                        self.dispatch_handle(
                            node_sender.clone(),
                            tokio::spawn(async move {
                                parse_conditional(conditional_tokens).await
                            })
                        ).await;
                    }

                    // PIPELINES
                    _ => {
                        debug!("parse_input: Delegating to parse_pipeline for token: {:?}", self.token);
                        let mut pipeline_tokens: Vec<Token> = vec![];
                        let mut chains: Vec<Vec<Token>> = vec![];
                        loop {
                            match self.token {
                                Token::Eof | Token::Semicolon | Token::Newline => {
                                    debug!("parse_input: Reached delimiter in pipeline token parsing");
                                    break;
                                }
                                Token::And | Token::Or => {
                                    debug!("Chain token found, moving tokens: {:?}",pipeline_tokens);
                                    chains.push(mem::take(&mut pipeline_tokens));
                                    chains.push(vec![self.token.clone()]);
                                    self.next_token().await;
                                }
                                _ => {
                                    pipeline_tokens.push(self.token.clone());
                                    self.next_token().await;
                                }
                            }
                        }
                        if chains.is_empty() { // Parse single pipeline
                            self.dispatch_handle(
                                node_sender.clone(),
                                tokio::spawn(async move {
                                    parse_pipeline(pipeline_tokens).await
                                })
                            ).await;
                        } else {
                            chains.push(mem::take(&mut pipeline_tokens)); // Push final pipeline
                            self.dispatch_handle(
                                node_sender.clone(),
                                tokio::spawn(async move {
                                    parse_chains(chains).await
                                })
                            ).await;
                        }
                    }
                }
            }

            // TODO: remember why this even needs to be here
            if self.token == Token::Eof {
                self.stop_dispatcher(node_sender.clone()).await;
                break;
            }
            self.next_token().await;
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

    /// Helper method for emitting nodes to the NodeDispatcher in parse_input()
    async fn dispatch_handle(&mut self, sender: mpsc::Sender<JoinHandle<Option<ASTNode>>>, handle: JoinHandle<Option<ASTNode>>) {
        debug!("dispatch_handle: Sending node builder handle: {:?}", handle);
        let _ = sender.send(handle).await;
    }

    /// Helper method that breaks the event loop in NodeDispatcher.start()
    async fn stop_dispatcher(&mut self, sender: mpsc::Sender<JoinHandle<Option<ASTNode>>>) {
        // TODO: figure out if there's a better way to stop the dispatcher remotely
        let _ = sender.send(
            tokio::spawn( async move {
                Some(ASTNode::Eof {  })
            })
        ).await;
    }
}
