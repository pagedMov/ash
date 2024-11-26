use log::{info,trace,error,debug,};

use crate::helper;
use crate::environment::Environment;

static BUILTINS: [&str; 6] = ["cd", "echo", "exit", "export", "alias", "unset"];

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
    Var((String,String)),
    Semicolon,
    Newline,
    Pipe,
    RedirectIn,
    RedirectOut,
    RedirectErr,
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
}

pub fn tokenize(input: &str, environment: &Environment) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(&ch) = chars.peek() {
        match ch {
            ' ' | '\t' => { chars.next(); }
            '|' => {
                tokens.push(Token::Pipe);
                chars.next();
            }
            '<' => {
                tokens.push(Token::RedirectIn);
                chars.next();
            }
            '>' => {
                tokens.push(Token::RedirectOut);
                chars.next();
            }
            ';' => {
                tokens.push(Token::Semicolon);
                chars.next();
            }
            '\n' => {
                tokens.push(Token::Newline);
                chars.next();
            }
            '$' => {
                chars.next();
                let mut var = String::new();
                while let Some(&c) = chars.peek() {
                    if ";|\n<> \t".contains(c) { // Whitespace, semicolons, newlines, operators
                        break;
                    }
                    var.push(c);
                    chars.next();
                }
                if let Some(variable) = environment.get_var(&var) {
                    tokens.push(Token::Word(variable.clone()));
                }
            }
            _ => {
                let mut word = String::new();
                let double_quote = false;
                let single_quote = false;

                word = helper::build_word(&mut chars,single_quote,double_quote,word);

                if let Some(alias) = environment.get_alias(&word) {
                    let mut alias_tokens = tokenize(alias,environment);
                    let _eof_token = alias_tokens.pop(); // Removes Eof token
                    tokens.extend(alias_tokens)

                } else if helper::is_var_declaration(word.clone()) {
                    let (key,value) = helper::extract_var(word).unwrap();
                    tokens.push(Token::Var((key,value)));

                } else {
                    match word.as_str() {
                        "if" => { tokens.push(Token::If); }
                        "then" => { tokens.push(Token::Then); }
                        "else" => { tokens.push(Token::Else); }
                        "fi" => { tokens.push(Token::Fi); }
                        "for" => { tokens.push(Token::For); }
                        "while" => { tokens.push(Token::While); }
                        "until" => { tokens.push(Token::Until); }
                        "do" => { tokens.push(Token::Do); }
                        "done" => { tokens.push(Token::Done); }
                        "alias" => {
                            // alias needs special treatment since the syntax 'key=value' is reserved for
                            // variable assignment, so its arg gets consumed as a variable otherwise
                            // so let's just push the next word here instead of looping again
                            tokens.push(Token::Word(word));
                            if let Some(&' ') = chars.peek() {
                                chars.next(); // Skip space
                                let mut alias_def = String::new();
                                alias_def = helper::build_word(&mut chars, false, false, alias_def);
                                tokens.push(Token::Word(alias_def))
                            }
                        }
                        _ => { tokens.push(Token::Word(word)); }
                    }
                }
            }
        }
    }

    tokens.push(Token::Eof);
    tokens
}

#[derive(Clone, Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl<'a> Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        debug!("Initializing parser with tokens: {:?}", tokens);
        Self { tokens, pos: 0 }
    }

    fn current_token(&self) -> &Token {
        let token = self.tokens.get(self.pos).unwrap_or(&Token::Eof);
        trace!("Getting current token: {:?}", token);
        token
    }

    fn advance(&mut self) {
        trace!("Advancing from token at position {}: {:?}", self.pos, self.current_token());
        self.pos += 1;
    }

    fn parse_command(&mut self) -> Result<ASTNode, &'a str> {
        info!("Starting to parse command...");
        if let Token::Word(name) = self.current_token() {
            let name = name.clone();
            self.advance();

            let mut args = Vec::new();
            let mut redirs = Vec::new();

            loop {
                let token = self.current_token().clone();
                trace!("Processing token in command: {:?}", token);
                match token {
                    Token::Word(arg) => {
                        args.push(arg);
                        self.advance();
                    }
                    Token::RedirectIn => {
                        self.advance();
                        if let Token::Word(file) = self.current_token() {
                            trace!("Adding input redirection: {}", file);
                            redirs.push(Redirection {
                                direction: RedirectionType::Input,
                                file: file.clone(),
                            });
                            self.advance();
                        } else {
                            error!("Expected file after '<', found: {:?}", self.current_token());
                            return Err("Expected file after '<'");
                        }
                    }
                    Token::RedirectOut => {
                        self.advance();
                        if let Token::Word(file) = self.current_token() {
                            trace!("Adding output redirection: {}", file);
                            redirs.push(Redirection {
                                direction: RedirectionType::Output,
                                file: file.clone(),
                            });
                            self.advance();
                        } else {
                            error!("Expected file after '>', found: {:?}", self.current_token());
                            return Err("Expected file after '>'");
                        }
                    }
                    _ => break,
                }
            }

            if BUILTINS.contains(&name.as_str()) {
                let builtin = ASTNode::Builtin { name, args, redirs };
                debug!("Parsed builtin command: {:?}", builtin);
                return Ok(builtin);
            }

            let command = ASTNode::ShCommand { name, args, redirs };
            debug!("Parsed shell command: {:?}", command);
            Ok(command)
        } else {
            error!("Unexpected token when parsing command: {:?}", self.current_token());
            Err("Unexpected token found")
        }
    }

    fn parse_pipeline(&mut self) -> Result<ASTNode, &'a str> {
        info!("Starting to parse pipeline...");
        let mut commands = vec![self.parse_command()?];
        debug!("First command in pipeline: {:?}", commands[0]);

        loop {
            let token = self.current_token();
            match *token {
                Token::Pipe => {
                    trace!("Found pipe, adding next command to pipeline...");
                    self.advance();
                    commands.push(self.parse_command()?);
                }
                Token::Semicolon | Token::Newline => {
                    trace!("End of pipeline detected");
                    self.advance();
                    break;
                }
                _ => break,
            }
        }

        if commands.len() == 1 {
            let command = commands.remove(0);
            trace!("Pipeline contains a single command: {:?}", command);
            Ok(command)
        } else {
            debug!("Parsed pipeline with commands: {:?}", commands);
            Ok(ASTNode::Pipeline { commands })
        }
    }

    fn parse_conditional(&mut self) -> Result<ASTNode, &'a str> {
        info!("Starting to parse conditional...");
        self.advance();

        let condition = Box::new(self.parse_pipeline()?);
        debug!("Parsed condition of conditional: {:?}", condition);

        let mut body1: Option<Box<ASTNode>> = None;
        let mut body2: Option<Box<ASTNode>> = None;

        loop {
            let token = self.current_token();
            trace!("Processing token in conditional: {:?}", token);

            match *token {
                Token::Semicolon => {
                    trace!("Skipping semicolon in conditional");
                    self.advance();
                    continue;
                }
                Token::Then => {
                    trace!("Found 'then', parsing body1...");
                    self.advance();

                    loop {
                        let token = self.current_token();
                        match *token {
                            Token::Semicolon => {
                                trace!("Skipping semicolon in body1");
                                self.advance();
                                continue;
                            }
                            Token::Else => {
                                trace!("Found 'else', parsing body2...");
                                self.advance();
                                match self.current_token() {
                                    Token::If => {
                                        trace!("Found 'else if', parsing nested conditional...");
                                        body2 = Some(Box::new(self.parse_conditional()?));
                                    }
                                    _ => {
                                        body2 = Some(Box::new(self.parse_pipeline()?));
                                    }
                                }
                                continue;
                            }
                            Token::Fi => {
                                trace!("Found 'fi', completing conditional parsing...");
                                self.advance();
                                return Ok(ASTNode::Conditional { condition, body1, body2 });
                            }
                            _ => {
                                trace!("Parsing body1 pipeline...");
                                body1 = Some(Box::new(self.parse_pipeline()?));
                            }
                        }
                    }
                }
                _ => {
                    error!("Unexpected token in conditional: {:?}", token);
                    return Err("Unexpected token");
                }
            }
        }
    }

    pub fn parse_input(&mut self, environment: &mut Environment) -> Result<Vec<ASTNode>, String> {
        let mut statements = Vec::new();
        info!("Starting to parse input...");

        loop {
            let token = self.current_token();
            trace!("Current token in input parsing: {:?}", token);

            match token {
                Token::Eof => {
                    trace!("End of input detected");
                    break;
                }
                Token::Var((key, value)) => {
                    trace!("Setting variable: {}={}", key, value);
                    environment.set_var(key.as_str(), value.as_str());
                    self.advance();
                }
                Token::If => {
                    trace!("Found 'if', parsing conditional...");
                    match self.parse_conditional() {
                        Ok(conditional) => {
                            statements.push(conditional);
                            self.advance();
                        }
                        Err(e) => {
                            error!("Failed to parse conditional: {}", e);
                            return Err(format!("Failed to parse conditional: {}", e));
                        }
                    }
                }
                _ => {
                    info!("Parsing pipeline...");
                    match self.parse_pipeline() {
                        Ok(pipeline) => {
                            statements.push(pipeline);
                        }
                        Err(e) => {
                            error!("Failed to parse pipeline: {}", e);
                            return Err(format!("Failed to parse pipeline: {}", e));
                        }
                    }
                }
            }
        }

        debug!("Parsed input into statements: {:?}", statements);
        Ok(statements)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::environment::Environment;

    #[test]
    fn test_tokenize_simple_command() {
        let env = Environment::new();
        let input = "echo hello";
        let tokens = tokenize(input, &env);

        assert_eq!(
            tokens,
            vec![
            Token::Word("echo".to_string()),
            Token::Word("hello".to_string()),
            Token::Eof
            ]
        );
    }

    #[test]
    fn test_tokenize_redirections() {
        let env = Environment::new();
        let input = "cat < input.txt > output.txt";
        let tokens = tokenize(input, &env);

        assert_eq!(
            tokens,
            vec![
            Token::Word("cat".to_string()),
            Token::RedirectIn,
            Token::Word("input.txt".to_string()),
            Token::RedirectOut,
            Token::Word("output.txt".to_string()),
            Token::Eof
            ]
        );
    }

    #[test]
    fn test_tokenize_with_variables() {
        let mut env = Environment::new();
        env.set_var("VAR", "value");
        let input = "echo $VAR";
        let tokens = tokenize(input, &env);

        assert_eq!(
            tokens,
            vec![
            Token::Word("echo".to_string()),
            Token::Word("value".to_string()),
            Token::Eof
            ]
        );
    }

    #[test]
    fn test_tokenize_alias() {
        let mut env = Environment::new();
        env.set_alias("ls", "echo alias_ls");
        let input = "ls";
        let tokens = tokenize(input, &env);

        assert_eq!(
            tokens,
            vec![
            Token::Word("echo".to_string()),
            Token::Word("alias_ls".to_string()),
            Token::Eof
            ]
        );
    }

    #[test]
    fn test_parse_builtin() {
        let tokens = vec![
            Token::Word("echo".to_string()),
            Token::Word("hello".to_string()),
            Token::Eof,
        ];
        let mut parser = Parser::new(tokens);
        let command = parser.parse_command().unwrap();

        if let ASTNode::Builtin { name, args, redirs } = command {
            assert_eq!(name, "echo");
            assert_eq!(args, vec!["hello"]);
            assert!(redirs.is_empty());
        } else {
            panic!("Expected ASTNode::Builtin");
        }
    }

    #[test]
    fn test_parse_command() {
        let tokens = vec![
            Token::Word("command".to_string()),
            Token::Word("arg1".to_string()),
            Token::Word("arg2".to_string()),
            Token::Eof,
        ];
        let mut parser = Parser::new(tokens);
        let command = parser.parse_command().unwrap();

        if let ASTNode::ShCommand { name, args, redirs } = command {
            assert_eq!(name, "command");
            assert_eq!(args, vec!["arg1", "arg2"]);
            assert!(redirs.is_empty());
        } else {
            panic!("Expected ASTNode::ShCommand");
        }
    }

    #[test]
    fn test_parse_pipeline() {
        let tokens = vec![
            Token::Word("echo".to_string()),
            Token::Word("hello".to_string()),
            Token::Pipe,
            Token::Word("wc".to_string()),
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let pipeline = parser.parse_pipeline().unwrap();

        // Ensure it's a Pipeline node and unwrap the commands
        if let ASTNode::Pipeline { commands } = pipeline {
            // Check the number of commands in the pipeline (2)
            assert_eq!(commands.len(), 2);

            // Check the first command in the pipeline, should be a Builtin (echo)
            if let ASTNode::Builtin { name, args, redirs } = &commands[0] {
                assert_eq!(name, "echo");
                assert_eq!(args, &vec!["hello".to_string()]);
                assert_eq!(redirs, &Vec::<Redirection>::new());  // Use Vec::new() for an empty vector
            } else {
                panic!("Expected ASTNode::Builtin for the first command");
            }

            // Check the second command in the pipeline, should be a Builtin (wc)
            if let ASTNode::ShCommand { name, args, redirs } = &commands[1] {
                assert_eq!(name, "wc");
                assert_eq!(args.len(), 0); // "wc" should have no arguments in this case
                assert_eq!(redirs, &Vec::<Redirection>::new());
            } else {
                panic!("Expected ASTNode::ShCommand for the second command");
            }
        } else {
            panic!("Expected ASTNode::Pipeline");
        }
    }

    #[test]
    fn test_parse_conditional() {
        let tokens = vec![
            Token::If,
            Token::Word("true".to_string()),
            Token::Then,
            Token::Word("echo".to_string()),
            Token::Word("yes".to_string()),
            Token::Fi,
            Token::Eof,
        ];
        let mut parser = Parser::new(tokens);
        let conditional = parser.parse_conditional().unwrap();

        if let ASTNode::Conditional { condition: _, body1, body2 } = conditional {
            assert!(body2.is_none());
            if let Some(body1) = body1 {
                if let ASTNode::Pipeline { commands } = *body1 {
                    assert_eq!(commands.len(), 1);
                }
            }
        } else {
            panic!("Expected ASTNode::Conditional");
        }
    }

    #[test]
    fn test_redirection_methods() {
        let redirection = Redirection {
            direction: RedirectionType::Input,
            file: "input.txt".to_string(),
        };

        assert_eq!(redirection.get_direction(), RedirectionType::Input);
        assert_eq!(redirection.get_filepath(), "input.txt".to_string());
    }
}
