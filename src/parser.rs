use log::{info,trace,error,debug};
use std::collections::VecDeque;
use tokio::sync::mpsc;
use tokio::task::JoinHandle;

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
    ShCommand {
        name: String,
        args: Vec<String>,
        redirs: Vec<Redirection>,
    },
    Pipeline {
        commands: Vec<ASTNode>,
    },
    Conditional {
        condition: Box<ASTNode>,
        body1: Option<Box<ASTNode>>,
        body2: Option<Box<ASTNode>>
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
pub struct Redirection {
    direction: RedirDestination,
    file: Option<String>
}

impl Redirection {

    pub fn new(direction: RedirDestination, file: Option<String>) -> Self {
        Redirection { direction, file }
    }
    pub fn get_direction(&self) -> RedirDestination {
        self.direction.clone()
    }
    pub fn get_filepath(&self) -> Option<String> {
        self.file.clone()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RedirDestination {
    Input,          // < file
    Output,         // > file
    Error,          // 2> file
    Append(u32),         // >> file, 2>> file
    AppendBoth,
    Both,           // &> file
    FdOut(u32),     // n> file
    FdAppend(u32),     // n>> file
    FdToFd(u32,u32),   // n>&m
    OutToFd(u32),   // >&fd
    DupToAll,         // >&
    CloseFd(u32),   // n>&-
}

#[derive(Clone,Debug,PartialEq)]
pub enum Token {
    Word(String),
    Redir(Redirection),
    Semicolon,
    Newline,
    Pipe,
    If,
    Then,
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
            ';' | '|' | '\n' | '<' | '>' | ' ' | '\t' if !singlequote && !doublequote => {
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

/// Not even jesus christ would forgive me for this shit
pub fn check_for_redirections(old_chars: &VecDeque<char>) -> Option<(VecDeque<char>, Token)> {
    let mut chars: VecDeque<char>;

    // Only clone the VecDeque if it makes sense to do so
    if matches!(old_chars.front()?, '<' | '>' | '&' | '2'..='9') {
        chars = old_chars.clone();
    } else { return None; }
    let ch = chars.pop_front()?;
    match ch {
        '<' => {
            skip_space(&mut chars);
            let file = build_word(&mut chars);
            let token = Token::Redir(Redirection::new(
                    RedirDestination::Input,
                    Some(file)
            ));
            Some((chars,token))
        }
        '>' => match chars.front() {
            Some('>') => { // Append (>>)
                chars.pop_front();
                skip_space(&mut chars);
                let file = build_word(&mut chars);
                let token = Token::Redir(Redirection::new(
                        RedirDestination::Append(1),
                        Some(file)
                ));
                Some((chars,token))
            }
            Some('&') => { // Duplicate descriptor (>&fd)
                chars.pop_front();
                if let Some(digit) = chars.front().and_then(|c| c.to_digit(10)) {
                    chars.pop_front();
                    let token = Token::Redir(Redirection::new(
                            RedirDestination::OutToFd(digit),
                            None
                    ));
                    Some((chars,token))
                } else {
                    let token = Token::Redir(Redirection::new(
                            RedirDestination::DupToAll,
                            None
                    ));
                    Some((chars,token)) // Redirect all (>&)
                }
            }
            _ => { // Simple output (>)
                skip_space(&mut chars);
                let file = build_word(&mut chars);
                let token = Token::Redir(Redirection::new(
                        RedirDestination::Output,
                        Some(file)
                ));
                Some((chars,token))
            }
        },
        '&' => match chars.front() {
            Some('>') => { // &>
                chars.pop_front();
                match chars.front()? {
                    '>' => {
                        chars.pop_front();
                        skip_space(&mut chars);
                        let file = build_word(&mut chars);
                        let token = Token::Redir(Redirection::new(
                                RedirDestination::AppendBoth,
                                Some(file)
                        ));
                        Some((chars,token))
                    }
                    _ => {
                        chars.pop_front();
                        skip_space(&mut chars);
                        let file = build_word(&mut chars);
                        let token = Token::Redir(Redirection::new(
                                RedirDestination::Both,
                                Some(file)
                        ));
                        Some((chars,token))
                    }
                }
            }
            _ => None, // Invalid case
        },
        '2'..='9' => {
            let fd = ch.to_digit(10).unwrap(); // Safe unwrap since we matched on '2'..='9'
            if let Some('>') = chars.front() {
                chars.pop_front();
                match chars.front() {
                    Some('&') => { // Duplicate to another fd (n>&m)
                        chars.pop_front();
                        if let Some(digit) = chars.front().and_then(|c| c.to_digit(10)) {
                            chars.pop_front();
                            let token = Token::Redir(Redirection::new(
                                    RedirDestination::FdToFd(fd, digit),
                                    None
                            ));
                            Some((chars,token))
                        } else {
                            let token = Token::Redir(Redirection::new(
                                    RedirDestination::CloseFd(fd),
                                    None
                            ));
                            Some((chars,token)) // n>&-
                        }
                    }
                    Some('>') => {
                        chars.pop_front();
                        match chars.front() {
                            Some('&') => { // Duplicate to another fd (n>&m)
                                chars.pop_front();
                                if let Some(digit) = chars.front().and_then(|c| c.to_digit(10)) {
                                    chars.pop_front();
                                    let token = Token::Redir(Redirection::new(
                                            RedirDestination::FdToFd(fd, digit),
                                            None
                                    ));
                                    Some((chars,token))
                                } else {
                                    let token = Token::Redir(Redirection::new(
                                            RedirDestination::CloseFd(fd),
                                            None
                                    ));
                                    Some((chars,token)) // n>&-
                                }
                            }
                            _ => { // n> file
                                skip_space(&mut chars);
                                let file = build_word(&mut chars);
                                let token = Token::Redir(Redirection::new(
                                        RedirDestination::FdAppend(fd),
                                        Some(file)
                                ));
                                Some((chars,token))
                            }
                        }
                    }
                    _ => { // n> file
                        skip_space(&mut chars);
                        let file = build_word(&mut chars);
                        let token = Token::Redir(Redirection::new(
                                RedirDestination::FdOut(fd),
                                Some(file)
                        ));
                        Some((chars,token))
                    }
                }
            } else {
                None // Invalid syntax
            }
        }
        _ => None, // No redirection found
    }
}

/// Reads input one character at a time, creating tokens from words and symbols
///
/// Tokens are then used by the Parser to create ASTNodes to send back to the
/// shell's main event loop
pub async fn tokenize(input: &str, outbox: mpsc::Sender<Token>) -> Result<(), ShellError> {

    // TODO: implement more redirections, '<<<' etc.
    // TODO: handle escaping with backslashes
    // TODO:
    // TODO:

    let mut chars = VecDeque::from(input.chars().collect::<Vec<char>>());

    while let Some(&ch) = chars.front() {
        debug!("tokenizer: checking character: {:?}",ch);
        let mut word = String::new();
        let mut token: Option<Token> = None;
        match ch {
            ' ' | '\t' => {
                chars.pop_front();
                continue;
            }
            '|' => {
                token = Some(Token::Pipe);
                chars.pop_front();
            }
            '\n' => {
                token = Some(Token::Newline);
                chars.pop_front();
            }
            ';' => {
                token = Some(Token::Semicolon);
                chars.pop_front();
            }
            _ => {
                if let Some((new_chars,redir_token)) = check_for_redirections(&chars) {
                    token = Some(redir_token);
                    chars = new_chars;
                } else {
                    word = build_word(&mut chars);

                    // Reserved words
                    match word.as_str() {
                        "if" => { token = Some(Token::If); }
                        "then" => { token = Some(Token::Then); }
                        "else" => { token = Some(Token::Else); }
                        "fi" => { token = Some(Token::Fi); }
                        "for" => { token = Some(Token::For); }
                        "while" => { token = Some(Token::While); }
                        "until" => { token = Some(Token::Until); }
                        "do" => { token = Some(Token::Do); }
                        "done" => { token = Some(Token::Done); }
                        _ => { token = Some(Token::Word(word.clone())); }
                    }
                }
            }
        }
        if let Some(ref token) = token {
            // send token from tokenizer to parser
            info!("tokenizer: Sending token: {:?}",token);
            // TODO: handle errors here
            outbox.send(token.clone()).await.unwrap();
        } else {
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

/// Parses conditional statements via recursive descent.
///
/// Called from parse_input()
async fn parse_conditional(tokens: Vec<Token>) -> Option<ASTNode> {
    todo!("Implement conditional parsing")
}

/// Parses commands
///
/// If only one command is found, it just returns the ShCommand node,
/// If several are found, it returns a Pipeline node containing ShCommands
///
/// Called from parse_input()
async fn parse_pipeline(tokens: Vec<Token>) -> Option<ASTNode> {
    debug!("parse_pipeline: Parsing pipeline");

    // VecDeques are easier to work with for this
    // TODO: just make it take a VecDeque instead of converting it here
    let mut tokens = VecDeque::from(tokens);

    let mut commands = Vec::new();
    let mut args = Vec::new();
    let mut redirs = Vec::new();

    // All of this is pretty self-explanatory
    while let Some(token) = tokens.pop_front() {
        debug!("parse_pipeline: Current token in pipeline: {:?}", token);

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
                        Token::Redir(redir) => {
                            redirs.push(redir.clone());
                            tokens.pop_front();
                        }
                        _ => break,
                    }
                }

                debug!("parse_pipeline: Finished parsing command: {}", name);
                commands.push(ASTNode::ShCommand {
                    name,
                    args: std::mem::take(&mut args),
                    redirs: std::mem::take(&mut redirs),
                });
            }
            Token::Pipe => {
                debug!("parse_pipeline: Found pipe '|', continuing to next command");
            }
            Token::Semicolon | Token::Newline | Token::Eof => {
                debug!("parse_pipeline: End of pipeline detected: {:?}", token);
                break;
            }
            _ => {
                debug!("parse_pipeline: Unexpected token in pipeline: {:?}", token);
                return None;
            }
        }
    }

    if commands.len() == 1 {
        let command = commands.remove(0);
        debug!("parse_pipeline: Returning single command: {:?}", command);
        Some(command)
    } else if !commands.is_empty() {
        let pipeline = ASTNode::Pipeline { commands };
        debug!("parse_pipeline: Returning pipeline: {:?}", pipeline);
        Some(pipeline)
    } else {
        debug!("parse_pipeline: No commands found in pipeline.");
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
                    _ => {
                        debug!("parse_input: Delegating to parse_pipeline for token: {:?}", self.token);
                        let mut pipeline_tokens: Vec<Token> = vec![];
                        loop {
                            match self.token {
                                Token::Eof | Token::Semicolon | Token::Newline => {
                                    debug!("parse_input: Reached delimiter in pipeline token parsing");
                                    break;
                                }
                                _ => {
                                    pipeline_tokens.push(self.token.clone());
                                    self.next_token().await;
                                }
                            }
                        }

                        self.dispatch_handle(
                            node_sender.clone(),
                            tokio::spawn(async move {
                                parse_pipeline(pipeline_tokens).await
                            })
                        ).await;
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
