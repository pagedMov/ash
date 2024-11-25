use log::debug;
use crate::helper;
use crate::environment::Environment;

static BUILTINS: [&str; 6] = ["cd", "echo", "exit", "export", "alias", "unset"];

#[derive(Debug,Clone)]
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
    Output // >
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
    debug!("Tokenizing input: {}",input);

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
    debug!("Returning tokens: {:?}",tokens);
    tokens
}

#[derive(Clone,Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl<'a> Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        debug!("Initializing parser");
        Self { tokens, pos: 0 }
    }
    fn current_token(&self) -> &Token {
        let token = self.tokens.get(self.pos).unwrap_or(&Token::Eof);
        debug!("Getting current token: {:?}",token);
        token
    }
    fn advance(&mut self) {
        self.pos += 1;
    }
    fn parse_command(&mut self) -> Result<ASTNode,&'a str> {
        debug!("Parsing command...");
        if let Token::Word(name) = self.current_token() {
            let name = name.clone();
            self.advance();

            let mut args = Vec::new();
            let mut redirs = Vec::new();

            loop {
                let token = self.current_token().clone();
                match token {
                    Token::Word(arg) => {
                        args.push(arg);
                        self.advance();
                    }
                    Token::RedirectIn => {
                        self.advance();
                        if let Token::Word(file) = self.current_token() {
                            redirs.push(Redirection {
                                direction: RedirectionType::Input,
                                file: file.clone()
                            });
                            self.advance();
                        } else {
                            return Err("Expecteed file after '<'");
                        }
                    }
                    Token::RedirectOut => {
                        self.advance();
                        if let Token::Word(file) = self.current_token() {
                            redirs.push(Redirection {
                                direction: RedirectionType::Output,
                                file: file.clone()
                            });
                            self.advance();
                        } else {
                            return Err("Expected file after '>'");
                        }
                    }
                    _ => break
                }
            }

            if BUILTINS.contains(&name.as_str()) {
                let builtin = ASTNode::Builtin { name, args, redirs };
                debug!("Returning shell builtin: {:?}", builtin);
                return Ok(builtin)
            }

            let command = ASTNode::ShCommand {
                name,
                args,
                redirs
            };
            debug!("Returning command: {:?}",command);
            Ok(command)
        } else {
            Err("Unexpected token found")
        }
    }
    fn parse_pipeline(&mut self) -> Result<ASTNode,&'a str> {
        let mut commands = vec![self.parse_command()?];
        debug!("Parsing pipeline: {:?}",commands);

        loop {
            let token = self.current_token();
            match *token {
                Token::Pipe => {
                    self.advance();
                    commands.push(self.parse_command()?);
                }
                Token::Semicolon | Token::Newline => {
                    self.advance();
                    break
                }
                _ => break
            }
        }

        if commands.len() == 1 {
            let command = commands.remove(0);
            debug!("Returning command: {:?}", command);
            Ok(command)
        } else {
            debug!("Returning pipeline: {:?}",commands);
            Ok(ASTNode::Pipeline { commands })
        }
    }
    fn parse_conditional(&mut self) -> Result<ASTNode,&'a str> {
        self.advance();

        let condition = Box::new(self.parse_pipeline()?);
        debug!("Parsing new conditional with condition: {:?}",condition);

        loop {
            let token = self.current_token();
            match *token {
                Token::Semicolon => {
                    self.advance();
                    continue;
                },
                Token::Then => {
                    self.advance();
                    let mut body1: Option<Box<ASTNode>> = None;
                    let mut body2: Option<Box<ASTNode>> = None;
                    loop {
                        let token = self.current_token();
                        match *token {
                            Token::Semicolon => {
                                self.advance();
                                continue;
                            },
                            Token::Eof => { return Err("Unexpected End of File"); }
                            Token::Else => {
                                self.advance();
                                match self.current_token() {
                                    Token::If => body2 = Some(Box::new(self.parse_conditional()?)),
                                    _ => body2 = Some(Box::new(self.parse_pipeline()?)),
                                }
                            }
                            Token::Fi => {
                                let conditional = ASTNode::Conditional { condition, body1, body2};
                                debug!("Returning conditional: {:?}", conditional);
                                return Ok(conditional);
                            }
                            _ => {
                                body1 = Some(Box::new(self.parse_pipeline()?));
                            }
                        }
                    }
                }
                _ => { return Err("Unexpected token"); }
            }
        }
    }
    pub fn parse_input(&mut self, environment: &mut Environment) -> Result<Vec<ASTNode>,String> {
        let mut statements = Vec::new();
        debug!("Parsing new input!");

        loop {
            let token = self.current_token();
            match token {
                Token::Eof => break,
                Token::Var((key,value)) => {
                    environment.set_var(key.as_str(), value.as_str());
                    self.advance();

                }
                Token::If => {
                    let conditional = self.parse_conditional();
                    match conditional {
                        Ok(conditional) => {
                            statements.push(conditional);
                            self.advance();
                        }
                        Err(e) => { return Err(format!("Failed to parse conditional: {}",e)); }
                    }
                }
                _ => {
                    let pipeline = self.parse_pipeline();
                    match pipeline {
                        Ok(pipeline) => {
                            statements.push(pipeline);
                        }
                        Err(e) => { return Err(format!("Failed to parse pipeline: {}",e)); }
                    }
                }
            }
        }
        debug!("Constructed statements: {:?}",statements);
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
