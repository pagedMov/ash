use log::{info,trace,error,debug};
use std::collections::VecDeque;
use std::iter::Peekable;
use std::str::Chars;
use std::sync::Arc;
use tokio::sync::{Notify,mpsc};
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
    }
}

#[derive(Debug,Clone,PartialEq)]
pub struct Redirection {
    direction: RedirectionType,
    file: String
}

impl Redirection {

    pub fn new(direction: RedirectionType, file: String) -> Self {
        Redirection { direction, file }
    }
    pub fn get_direction(&self) -> RedirectionType {
        self.direction.clone()
    }
    pub fn get_filepath(&self) -> String {
        self.file.clone()
    }
}

#[derive(Debug,Clone,PartialEq)]
pub enum RedirectionType {
    Input, // <
    Output,// >
    Error // 2>
}

#[derive(Clone,Debug,PartialEq)]
pub enum Token {
    Word(String),
    RedirectIn(String),
    RedirectOut(String),
    RedirectErr(String),
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

pub fn to_next_alphanumeric(chars: &mut VecDeque<char>) {
    chars.pop_front();
    while let Some(&ch) = chars.front() {
        match ch {
            ' ' | '\t' => { chars.pop_front(); },
            _ => { return; }
        }
    }
}

pub async fn tokenize(input: &str, outbox: mpsc::Sender<Token>) -> Result<(), ShellError> {

    let mut chars = VecDeque::from(input.chars().collect::<Vec<char>>());

    while let Some(&ch) = chars.front() {
        debug!("checking character: {:?}",ch);
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
            '2' => {
                if let Some('>') = chars.get(1) {
                    chars.pop_front(); to_next_alphanumeric(&mut chars); // Need to skip the '>' first
                    let file = build_word(&mut chars);
                    token = Some(Token::RedirectErr(file));
                }
            }
            '<' => {
                to_next_alphanumeric(&mut chars);
                let file = build_word(&mut chars);
                token = Some(Token::RedirectIn(file));
            }
            '>' => {
                to_next_alphanumeric(&mut chars);
                let file = build_word(&mut chars);
                token = Some(Token::RedirectOut(file));
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
                word = build_word(&mut chars);

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
        if let Some(ref token) = token {
            // send token from tokenizer to parser
            info!("Sending token: {:?}",token);
            outbox.send(token.clone()).await.map_err(|e| ShellError::InvalidSyntax(e.to_string()))?;
        } else {
            return Err(ShellError::InvalidSyntax(word));
        }
    }
    info!("Sending Eof token");
    outbox.send(Token::Eof).await.unwrap();
    Ok(())
}

async fn parse_conditional(tokens: Vec<Token>,outbox: mpsc::Sender<ShellEvent>) -> Option<ASTNode> {
    todo!("Implement conditional parsing")
}

async fn parse_pipeline(tokens: Vec<Token>, outbox: mpsc::Sender<ShellEvent>) -> Option<ASTNode> {
    debug!("Parsing pipeline");
    let mut tokens = VecDeque::from(tokens);
    let mut commands = Vec::new();
    let mut args = Vec::new();
    let mut redirs = Vec::new();

    while let Some(token) = tokens.pop_front() {
        debug!("Current token in pipeline: {:?}", token);

        match token {
            Token::Word(name) => {
                debug!("Found command: {}", name);

                while let Some(token) = tokens.front() {
                    match token {
                        Token::Word(arg) => {
                            debug!("Found argument: {}", arg);
                            args.push(arg.clone());
                            tokens.pop_front();
                        }
                        Token::RedirectIn(file) => {
                            debug!("Found input redirection '<'");
                            redirs.push(Redirection {
                                direction: RedirectionType::Input,
                                file: file.clone(),
                            });
                            tokens.pop_front();
                        }
                        Token::RedirectOut(file) => {
                            debug!("Found output redirection '>'");
                            redirs.push(Redirection {
                                direction: RedirectionType::Output,
                                file: file.clone(),
                            });
                            tokens.pop_front();
                        }
                        Token::RedirectErr(file) => {
                            debug!("Found error redirection '2>'");
                            redirs.push(Redirection {
                                direction: RedirectionType::Error,
                                file: file.clone(),
                            });
                            tokens.pop_front();
                        }
                        _ => break, // Stop processing arguments and redirections
                    }
                }

                debug!("Finished parsing command: {}", name);
                commands.push(ASTNode::ShCommand {
                    name,
                    args: std::mem::take(&mut args),
                    redirs: std::mem::take(&mut redirs),
                });
            }
            Token::Pipe => {
                debug!("Found pipe '|', continuing to next command");
            }
            Token::Semicolon | Token::Newline | Token::Eof => {
                debug!("End of pipeline detected: {:?}", token);
                break;
            }
            _ => {
                debug!("Unexpected token in pipeline: {:?}", token);
                return None;
            }
        }
    }

    if commands.len() == 1 {
        let command = commands.remove(0);
        debug!("Returning single command: {:?}", command);
        Some(command)
    } else if !commands.is_empty() {
        let pipeline = ASTNode::Pipeline { commands };
        debug!("Returning pipeline: {:?}", pipeline);
        Some(pipeline)
    } else {
        debug!("No commands found in pipeline.");
        None
    }
}

#[derive(Debug)]
pub struct NodeDispatcher {
    outbox: mpsc::Sender<ShellEvent>,
    node_queue: Vec<JoinHandle<Option<ASTNode>>>, // Handles return a result
    notify: Arc<Notify>,
}

impl NodeDispatcher {
    /// Create a new NodeDispatcher.
    pub fn new(outbox: mpsc::Sender<ShellEvent>) -> Self {
        Self {
            outbox,
            node_queue: Vec::new(),
            notify: Arc::new(Notify::new()),
        }
    }

    /// Add a new task handle to the queue.
    pub fn push_handle(&mut self, handle: JoinHandle<Option<ASTNode>>) {
        self.node_queue.push(handle);
        self.notify.notify_one();
    }

    /// Start processing tasks in the correct order.
    pub async fn start(&mut self) {
        loop {
            if let Some(handle) = self.node_queue.first_mut() {
                match handle.await {
                    Ok(Some(node)) => {
                        // Handle successful node creation
                        if let Err(err) = self
                            .outbox
                            .send(ShellEvent::NewASTNode(node)) // Send the created node
                            .await
                        {
                            error!("Failed to send AST node: {:?}", err);
                        }
                    }
                    Ok(None) => {
                        // TODO: properly handle case where parser returns None
                        error!("Node task failed");
                    }
                    Err(join_error) => {
                        // Handle task panics
                        error!("Task panicked: {:?}", join_error);
                    }
                }

                // Remove the completed task from the queue
                self.node_queue.remove(0);
            } else {
                // Wait for new tasks to be added if the queue is empty
                self.notify.notified().await;
            }
        }
    }
}

#[derive(Debug)]
pub struct Parser {
    token: Token,
    input: String,
    token_sender: mpsc::Sender<Token>,
    token_receiver: mpsc::Receiver<Token>,
    node_outbox: mpsc::Sender<event::ShellEvent>,
    node_dispatcher: NodeDispatcher

}

impl Parser {
    pub fn new(input: String, output: mpsc::Sender<ShellEvent>) -> Self {
        let (token_sender, token_receiver) = mpsc::channel(100);
        Self {
            token: Token::Null,
            input,
            token_sender,
            token_receiver,
            node_outbox: output.clone(),
            node_dispatcher: NodeDispatcher::new(output),
        }
    }

    pub fn outbox(&self) -> mpsc::Sender<ShellEvent> {
        self.node_outbox.clone()
    }

    pub async fn handle_input(&mut self) -> Result<(), ShellError> {
        let input = self.input.clone();
        let token_sender = self.token_sender.clone();

        tokio::spawn(async move {
            if let Err(e) = tokenize(&input, token_sender).await {
                error!("Tokenizer error: {:?}", e);
            }
        });

        self.parse_input().await
    }

    pub async fn parse_input(&mut self) -> Result<(), ShellError> {
        debug!("Starting parse_input");

        loop {
            debug!("Current token: {:?}", self.token);

            match self.token {
                Token::Null => debug!("Skipping Null token"),
                Token::Eof => {
                    debug!("Reached Eof token. Exiting parse_input loop.");
                    break;
                }
                _ => match self.token {
                    Token::If => {
                        debug!("Detected 'If' token. Parsing conditional...");
                        let mut conditional_tokens: Vec<Token> = vec![];
                        let outbox = self.outbox();
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
                            tokio::spawn(async move {
                                parse_conditional(conditional_tokens,outbox).await
                            })
                        );
                    }
                    _ => {
                        debug!("Delegating to parse_pipeline for token: {:?}", self.token);
                        let mut pipeline_tokens: Vec<Token> = vec![];
                        let outbox = self.outbox();
                        loop {
                            match self.token {
                                Token::Eof | Token::Semicolon | Token::Newline => {
                                    debug!("Reached delimiter in pipeline token parsing");
                                    break;
                                }
                                _ => {
                                    pipeline_tokens.push(self.token.clone());
                                    self.next_token().await;
                                }
                            }
                        }

                        self.dispatch_handle(
                            tokio::spawn(async move {
                                parse_pipeline(pipeline_tokens,outbox).await
                            })
                        );
                    }
                }
            }

            if self.token == Token::Eof {
                break;
            }
            self.next_token().await;
        }

        debug!("Finished parsing input. Sending prompt event.");
        self.dispatch_prompt().await?;
        Ok(())
    }

    async fn next_token(&mut self) {
        if self.token != Token::Eof {
            debug!("Fetching next token...");
            self.token = self.token_receiver.recv().await.unwrap_or(Token::Eof);
            info!("Token received: {:?}", self.token);
        } else {
            debug!("Reached Eof signal");
        }
    }



    fn dispatch_handle(&mut self, handle: JoinHandle<Option<ASTNode>>) {
        debug!("Sending AST node: {:?}", handle);
        self.node_dispatcher.push_handle(handle);
    }

    async fn dispatch_prompt(&self) -> Result<(), ShellError> {
        debug!("Sending prompt event");
        self.outbox()
            .send(ShellEvent::Prompt)
            .await
            .map_err(|_| ShellError::IoError("Failed to send prompt event".to_string()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tokio::sync::mpsc;

    fn setup_parser(input: &str) -> (Parser, mpsc::Receiver<ShellEvent>) {
        let (node_outbox, node_receiver) = mpsc::channel(100);
        let parser = Parser::new(input.to_string(), node_outbox);
        (parser, node_receiver)
    }

    async fn collect_nodes(receiver: &mut mpsc::Receiver<ShellEvent>) -> Vec<ShellEvent> {
        let mut nodes = Vec::new();
        while let Some(event) = receiver.recv().await {
            nodes.push(event);
        }
        nodes
    }

    #[tokio::test]
    async fn test_simple_commands() {
        let input = "a; b; c";
        let (mut parser, mut receiver) = setup_parser(input);

        tokio::spawn(async move { parser.handle_input().await.unwrap() });

        let nodes = collect_nodes(&mut receiver).await;

        assert_eq!(
            nodes,
            vec![
                ShellEvent::NewASTNode(ASTNode::ShCommand {
                    name: "a".to_string(),
                    args: vec![],
                    redirs: vec![]
                }),
                ShellEvent::NewASTNode(ASTNode::ShCommand {
                    name: "b".to_string(),
                    args: vec![],
                    redirs: vec![]
                }),
                ShellEvent::NewASTNode(ASTNode::ShCommand {
                    name: "c".to_string(),
                    args: vec![],
                    redirs: vec![]
                }),
                ShellEvent::Prompt
            ]
        );
    }
    #[tokio::test]
    async fn test_pipelines() {
        let input = "a | b | c; d | e";
        let (mut parser, mut receiver) = setup_parser(input);

        tokio::spawn(async move { parser.handle_input().await.unwrap() });

        let nodes = collect_nodes(&mut receiver).await;

        assert_eq!(
            nodes,
            vec![
                ShellEvent::NewASTNode(ASTNode::Pipeline {
                    commands: vec![
                        ASTNode::ShCommand {
                            name: "a".to_string(),
                            args: vec![],
                            redirs: vec![]
                        },
                        ASTNode::ShCommand {
                            name: "b".to_string(),
                            args: vec![],
                            redirs: vec![]
                        },
                        ASTNode::ShCommand {
                            name: "c".to_string(),
                            args: vec![],
                            redirs: vec![]
                        }
                    ]
                }),
                ShellEvent::NewASTNode(ASTNode::Pipeline {
                    commands: vec![
                        ASTNode::ShCommand {
                            name: "d".to_string(),
                            args: vec![],
                            redirs: vec![]
                        },
                        ASTNode::ShCommand {
                            name: "e".to_string(),
                            args: vec![],
                            redirs: vec![]
                        }
                    ]
                }),
                ShellEvent::Prompt
            ]
        );
    }
    #[tokio::test]
    async fn test_commands_with_arguments() {
        let input = "a b c; d e f g";
        let (mut parser, mut receiver) = setup_parser(input);

        tokio::spawn(async move { parser.handle_input().await.unwrap() });

        let nodes = collect_nodes(&mut receiver).await;

        assert_eq!(
            nodes,
            vec![
                ShellEvent::NewASTNode(ASTNode::ShCommand {
                    name: "a".to_string(),
                    args: vec!["b".to_string(), "c".to_string()],
                    redirs: vec![]
                }),
                ShellEvent::NewASTNode(ASTNode::ShCommand {
                    name: "d".to_string(),
                    args: vec!["e".to_string(), "f".to_string(), "g".to_string()],
                    redirs: vec![]
                }),
                ShellEvent::Prompt
            ]
        );
    }
    #[tokio::test]
    async fn test_commands_with_redirections() {
        let input = "a < input.txt > output.txt; b < in > out";
        let (mut parser, mut receiver) = setup_parser(input);

        tokio::spawn(async move { parser.handle_input().await.unwrap() });

        let nodes = collect_nodes(&mut receiver).await;

        assert_eq!(
            nodes,
            vec![
                ShellEvent::NewASTNode(ASTNode::ShCommand {
                    name: "a".to_string(),
                    args: vec![],
                    redirs: vec![
                        Redirection {
                            direction: RedirectionType::Input,
                            file: "input.txt".to_string()
                        },
                        Redirection {
                            direction: RedirectionType::Output,
                            file: "output.txt".to_string()
                        }
                    ]
                }),
                ShellEvent::NewASTNode(ASTNode::ShCommand {
                    name: "b".to_string(),
                    args: vec![],
                    redirs: vec![
                        Redirection {
                            direction: RedirectionType::Input,
                            file: "in".to_string()
                        },
                        Redirection {
                            direction: RedirectionType::Output,
                            file: "out".to_string()
                        }
                    ]
                }),
                ShellEvent::Prompt
            ]
        );
    }
    #[tokio::test]
    async fn test_complex_input() {
        let input = "a | b; c d e f g | h | i; j k l > out | m < in; n";
        let (mut parser, mut receiver) = setup_parser(input);

        tokio::spawn(async move { parser.handle_input().await.unwrap() });

        let nodes = collect_nodes(&mut receiver).await;

        assert!(nodes.is_empty()); // Ensure nodes are parsed
    }
}
