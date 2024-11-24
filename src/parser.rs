use log::debug;
use crate::environment::Environment;
#[derive(Debug,Clone)]
pub enum ASTNode {
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

#[derive(Debug,Clone)]
pub struct Redirection {
  direction: RedirectionType,
  file: String
}

impl Redirection {
    pub fn get_direction(&self) -> RedirectionType {
        self.direction.clone()
    }
    pub fn get_filepath(&self) -> String {
        self.file.clone()
    }
}

#[derive(Debug,Clone)]
pub enum RedirectionType {
  Input, // <
  Output // >
}

#[derive(Clone,Debug)]
pub enum Token {
  Word(String),
  Var(String),
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
        while let Some(&c) = chars.peek() {
            if ";|\n<> \t".contains(c) { // Whitespace, semicolons, newlines, operators
                break;
            }
          word.push(c);
          chars.next();
        }
        if let Some(alias) = environment.get_alias(&word) {
            let mut alias_tokens = tokenize(alias,environment);
            let _ = alias_tokens.pop(); // Removes Eof token
            tokens.extend(alias_tokens)
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
  pub fn parse_input(&mut self) -> Result<Vec<ASTNode>,String> {
    let mut statements = Vec::new();
    debug!("Parsing new input!");

    loop {
      let token = self.current_token();
      match *token {
        Token::Eof => break,
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
