use regex::Regex;
use lazy_static::lazy_static;
use tokio::sync::mpsc::{Sender,Receiver,channel};
use std::collections::{HashMap,VecDeque};
use std::cmp::Ordering;
use std::fmt;
use log::debug;

use crate::event::ShellEvent;
use crate::rsh::parse::ast::ASTNode;

lazy_static! {
    pub static ref REGEX: HashMap<String, Regex> = {
        let mut m = HashMap::new();
        m.insert("redirection".into(), Regex::new(r"^([0-9]+)?(>{1,2}|<{1,3})([&]?[0-9]+)?$").unwrap());
        m.insert("path".into(), Regex::new(r"^(\/(?:[^\/]+\/)*[^\/]*|(?:\.[\/\w\-]+\/?))$").unwrap());
        m.insert("range_num".into(), Regex::new(r"^\{\d+\.\.\d+\}$").unwrap());
        m.insert("range_alpha".into(), Regex::new(r"^\{[a-zA-Z]\.\.[a-zA-Z]\}$").unwrap());
        m.insert("process_sub".into(), Regex::new(r"^>\(.*\)$").unwrap());
        m.insert("command_sub".into(), Regex::new(r"^\$\([^\)]+\)$").unwrap());
        m.insert("arithmetic".into(), Regex::new(r"^\$\(\([^\)]+\)\)$").unwrap());
        m.insert("subshell".into(), Regex::new(r"^\([^\)]+\)$").unwrap());
        m.insert("test".into(), Regex::new(r"^\[\s*(.*?)\s*\]$").unwrap());
        m.insert("string".into(), Regex::new(r#"^\"([^\"]*)\"$"#).unwrap());
        m.insert("var_sub".into(), Regex::new(r"^\$([A-Za-z_][A-Za-z0-9_]*)$").unwrap());
        m.insert("block".into(), Regex::new(r"^\{[^{}]*\}$").unwrap());
        m.insert("assignment".to_string(), Regex::new(r"^[A-Za-z_][A-Za-z0-9_]*=.*$").unwrap());
        m.insert("operator".into(), Regex::new(r"(?:&&|\|\||[><]=?|[|;&])").unwrap());
        m.insert("ident".into(), Regex::new(r"^[A-Za-z0-9_\-/\.]*$").unwrap());
        m
    };
}

const BUILTINS: [&str;12] =
    [
        "set",
        "shift",
        "export",
        "readonly",
        "declare",
        "local",
        "unset",
        "trap",
        "eval",
        "exec",
        "source",
        "wait",
    ];

const KEYWORDS: [&str; 18] =
    [
        "if",
        "then",
        "else",
        "elif",
        "fi",
        "for",
        "select",
        "while",
        "until",
        "do",
        "done",
        "case",
        "esac",
        "in",
        "function",
        "return",
        "continue",
        "break",
    ];

#[derive(Debug,Clone)]
pub struct WordDesc {
    text: String,
    abs_pos: usize, // Where the word starts in the input string
    span: (usize,usize)
}

#[derive(Debug,Clone)]
pub struct ParseError {
    abs_pos: usize,
    span: (usize,usize),
    msg: String,
    input: String,
}

impl ParseError {
    fn calc_error_data(&self) -> (usize,usize,usize, usize, VecDeque<char>, usize) {
        // Not sorry for this
        let mut chars = VecDeque::from(self.input.chars().collect::<Vec<char>>());
        let mut index = 0;
        let mut column = 1;
        let mut line = 1;
        let mut window = VecDeque::new();
        let mut window_start_col = 1;
        let mut this_line = String::new();
        let mut prev_line = String::new();
        let mut the_line_before_that = String::new();
        let mut target_column = 1;
        let mut error_length = self.span.1 - self.span.0;

        while let Some(next_char) = chars.pop_front() {
            if index == self.abs_pos {
                target_column = column - 1;
            }
            if index > self.abs_pos && next_char == '\n' {
                debug!("breaking at newline");
                let both_lines = prev_line + &this_line;
                let all_the_lines = the_line_before_that + &both_lines;
                debug!("concatenating these lines: {}",all_the_lines);
                window = VecDeque::from(all_the_lines.chars().collect::<Vec<char>>());
                break
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
                window_start_col = column - 48;
            }
            this_line.push(next_char);
            index += 1;
        }

        debug!("Chars after calculation: {:?}", chars);
        if window.len() == 50 {
            for _ in 0..3 {
                window.push_front('.');
            }
        }
        (line,column,target_column, error_length, window, window_start_col)
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {

        let (line,column,target_column, error_length, window, window_start_col) = self.calc_error_data();
        let window_end_col = window_start_col + window.len();

        let mut window_slice: String = window.clone().into_iter().collect();
        let error_line = window_slice.split("\n").last().unwrap();
        debug!("target_column, error_length: {},{}",target_column,error_length);
        debug!("error_line: {}",error_line);
        writeln!(f, "\t")?;
        writeln!(f, "{}:{} - Parse Error: {}", line, column, self.msg)?;
        writeln!(f, "-----------------------------------------------")?;
        writeln!(f, "\t{}", window_slice)?;

        let mut caret_pos_start = self.span.0.saturating_sub(window_start_col).saturating_sub(0) + 1;
        let mut caret_pos_end = self.span.1.saturating_sub(window_start_col).saturating_sub(0) + 1;

        let mut error_pointer = String::new();
        for i in 0..error_line.len() {
            if i == target_column - 1 {
                error_pointer.push('^'); // Start of the span
                break;
            } else {
                error_pointer.push(' ');
            }
        }
        for i in 0..error_length {
            if i > error_line.len() {
                break
            }
            if i == error_length - 1 {
                error_pointer.push('^');
            } else {
                error_pointer.push('~');
            }
        }

        writeln!(f, "{}", error_pointer)?;
        writeln!(f, "\t")?;
        writeln!(f, "-----------------------------------------------")
    }
}


#[derive(Clone,Debug)]
pub enum Eval {
    Command {
        words: VecDeque<WordDesc>,
    },
    Function {
        words: VecDeque<WordDesc>,
    },
    Chain {
        left: Box<Eval>,
        right: Box<Eval>,
        operator: ChainOp
    },
    Pipeline {
        left: Box<Eval>,
    },
    IfThen {
        if_block: Conditional,
        elif_blocks: Vec<Conditional>,
        else_block: Option<Conditional>
    },
    WhileDo {
        body: Conditional
    },
    UntilDo {
        body: Conditional
    },
    ForDo {
        array: Vec<WordDesc>,
        body: Box<Eval>,
    },
    CaseIn {
        var: WordDesc,
        body: Vec<Conditional>
    },
    FuncDev {
        func_name: WordDesc,
        func_body: Vec<Eval>
    }
}

#[derive(Clone, Debug)]
pub enum Unit {
    RangeNum {
        word_desc: WordDesc,
    },
    RangeAlpha {
        word_desc: WordDesc,
    },
    ProcessSub {
        word_desc: WordDesc,
    },
    CommandSub {
        word_desc: WordDesc,
    },
    Arithmetic {
        word_desc: WordDesc,
    },
    Subshell {
        word_desc: WordDesc,
    },
    Test {
        word_desc: WordDesc,
    },
    Block {
        word_desc: WordDesc,
    },
    String {
        single_quote: bool,
        word_desc: WordDesc,
    },
    VarSub {
        word_desc: WordDesc,
    },
    Assignment {
        var: String,
        value: String,
    },
    Path {
        word_desc: WordDesc,
        relative: bool,
    },
    Keyword {
        word_desc: WordDesc,
    },
    And {
        word_desc: WordDesc,
    },
    Or {
        word_desc: WordDesc,
    },
    Pipe {
        word_desc: WordDesc,
    },
    Redir {
        redir_type: RedirType,
        word_desc: WordDesc,
        fd_out: i32,
        fd_target: i32,
    },
    Ident {
        word_desc: WordDesc,
    },
}

#[derive(Debug,Clone)]
pub enum RedirType {
    Output,
    Append,
    Input,
    Heredoc,
    Herestring
}

impl Unit {
    // The function that creates a Unit from a WordDesc
    pub fn from(word_desc: WordDesc, parser: &RshParser) -> Result<Unit,ParseError> {
        let string = word_desc.text.clone();
        debug!("Evaluating unit type for: {}",string);
        match string.as_str() {
            _ if REGEX["path"].is_match(&string) => {
                let relative = !string.starts_with('/');
                Ok(Unit::Path { word_desc, relative })
            }
            _ if REGEX["assignment"].is_match(&string) => {
                let parts: Vec<&str> = string.split('=').collect();
                let (var, value) = (parts[0].into(), parts[1].to_string());
                Ok(Unit::Assignment { var, value })
            }
            _ if REGEX["redirection"].is_match(&string) => {
                // REGEX: Any digits immediately followed by 1-3 < or 1-2 >, optional '&' followed by digits

                if let Some(captures) = REGEX["redirection"].captures(&string) {
                    // Unwrap is safe here
                    let fd_out = captures.get(1).map_or("1", |m| m.as_str()).to_string().parse::<i32>().unwrap_or(1);
                    let operator = captures.get(2).map_or("none", |m| m.as_str()).to_string();
                    let fd_target = captures.get(4).map_or("1", |m| m.as_str()).to_string().parse::<i32>().unwrap_or(1);
                    debug!("redir operator: {}",operator);
                    if string.matches('<').count() > 3 || string.matches('>').count() > 2 {
                        return Err(ParseError {
                            abs_pos: word_desc.abs_pos,
                            span: word_desc.span,
                            msg: "Invalid redirection operator".into(),
                            input: parser.input.clone()
                        });
                    }
                    let redir_type = match operator.as_str() {
                        ">" => RedirType::Output,
                        ">>" => RedirType::Append,
                        "<" => RedirType::Input,
                        "<<" => RedirType::Heredoc,
                        "<<<" => RedirType::Herestring,
                        _ => return Err(ParseError {
                            abs_pos: word_desc.abs_pos,
                            span: word_desc.span,
                            msg: "Invalid redirection operator".into(),
                            input: parser.input.clone()
                        }),
                    };
                    Ok(Unit::Redir { redir_type, word_desc, fd_out, fd_target, })
                } else { panic!("Redirection somehow succeeded and then failed on the same regex check?") }
            }
            _ if REGEX["range_num"].is_match(&string) => Ok(Unit::RangeNum { word_desc }),
            _ if REGEX["range_alpha"].is_match(&string) => Ok(Unit::RangeAlpha { word_desc }),
            _ if REGEX["process_sub"].is_match(&string) => Ok(Unit::ProcessSub { word_desc }),
            _ if REGEX["command_sub"].is_match(&string) => Ok(Unit::CommandSub { word_desc }),
            _ if REGEX["arithmetic"].is_match(&string) => Ok(Unit::Arithmetic { word_desc }),
            _ if REGEX["subshell"].is_match(&string) => Ok(Unit::Subshell { word_desc }),
            _ if REGEX["test"].is_match(&string) => Ok(Unit::Test { word_desc }),
            _ if REGEX["block"].is_match(&string) => Ok(Unit::Block { word_desc }),
            _ if REGEX["string"].is_match(&string) => Ok(Unit::String { single_quote: string.starts_with('\''), word_desc }),
            _ if REGEX["var_sub"].is_match(&string) => Ok(Unit::VarSub { word_desc }),
            _ if REGEX["operator"].is_match(&string) => {
                match string.as_str() {
                    "||" => Ok(Unit::Or { word_desc }),
                    "&&" => Ok(Unit::And { word_desc }),
                    "|" => Ok(Unit::Pipe { word_desc }),
                    _ => Err(ParseError {
                            abs_pos: word_desc.abs_pos,
                            span: word_desc.span,
                            msg: "Invalid operator".into(),
                            input: parser.input.clone()
                        }),
                }
            }
            _ if REGEX["ident"].is_match(&string) => Ok(Unit::Ident { word_desc }),
            _ => Err(ParseError {
                    abs_pos: word_desc.abs_pos,
                    span: word_desc.span,
                    msg: "Failed to classify unit".into(),
                    input: parser.input.clone()
                }),
        }
    }
}

#[derive(Clone,Debug)]
pub struct Conditional {
    condition: Box<Eval>,
    body: Vec<Eval>,
}

#[derive(Clone,Debug)]
pub enum ChainOp {
    And,
    Or
}

#[derive(Clone,Debug)]
pub struct RshParser {
    input: String,
    chars: VecDeque<char>,
    blocks: VecDeque<VecDeque<WordDesc>>,
    units: VecDeque<VecDeque<Unit>>,
    evals: VecDeque<Eval>,
    delim_stack: VecDeque<(usize,char)>,
    pos: usize,
    window: VecDeque<char>,
    window_start_col: usize
}

impl RshParser {
    pub fn new(input: &str) -> Self {
        let parser = Self {
            input: input.into(),
            chars: VecDeque::from(input.chars().collect::<Vec<char>>()),
            blocks: VecDeque::new(),
            units: VecDeque::new(),
            evals: VecDeque::new(),
            delim_stack: VecDeque::new(),
            pos: 0,
            window: VecDeque::new(),
            window_start_col: 1,
        };
        parser
    }

    pub fn print_tokens(&self) {
        println!("blocks:");
        for block in self.blocks.clone() {
            println!("{:?}",block);
        }
    }
    pub fn print_units(&self) {
        println!("units:");
        for unit in self.units.clone() {
            println!("{:#?}",unit);
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
        } else { None }
    }
    pub fn tokenize(&mut self) -> Result<(),ParseError> {
        let push_word = | // Closure for pushing words
            cur_blk: &mut VecDeque<WordDesc>,
            span_start: &mut usize,
            cur_wd: &mut String,
            pos: usize|
            {
                if !cur_wd.is_empty() {
                    cur_blk.push_back(WordDesc { text: cur_wd.clone(), abs_pos: *span_start, span: (*span_start, pos) });
                    *span_start = pos + 1;
                    cur_wd.clear();
                }
            };

        let mut current_word = String::new();
        let mut current_block: VecDeque<WordDesc> = VecDeque::new();
        let mut span_start = self.pos;
        while let Some(c) = self.advance() {
            debug!("checking char: {}",c);
            debug!("current_word: {}",current_word);
            if matches!(c, '(' | '[' | '{') { debug!("Pushing delimiter: {}",c); self.delim_stack.push_back((self.pos,c)); }
            if matches!(c, ')' | ']' | '}') {
                match c {
                    ')' => {
                        let open_delim = self.delim_stack.pop_back();
                        if open_delim.is_some() && open_delim.unwrap().1 != '(' {
                            let delim = open_delim.unwrap();
                            return Err(ParseError {
                                abs_pos: delim.0,
                                span: (delim.0,self.pos),
                                msg: "Mismatched open bracket".into(),
                                input: self.input.clone()
                            });
                        } else if self.delim_stack.is_empty() {
                            current_word.push(c);
                            push_word(&mut current_block,&mut span_start,&mut current_word,self.pos);
                            continue;
                        }
                    }
                    ']'=> {
                        let open_delim = self.delim_stack.pop_back();
                        if open_delim.is_some() && open_delim.unwrap().1 != '[' {
                            let delim = open_delim.unwrap();
                            return Err(ParseError {
                                abs_pos: delim.0,
                                span: (delim.0,self.pos),
                                msg: "Mismatched open bracket".into(),
                                input: self.input.clone()
                            });
                        } else if self.delim_stack.is_empty() {
                            current_word.push(c);
                            debug!("Pushing word to block: {}",current_word);
                            push_word(&mut current_block,&mut span_start,&mut current_word,self.pos);
                            span_start = self.pos + 1;
                            current_word.clear();
                            continue;
                        }
                    }
                    '}'=> {
                        let open_delim = self.delim_stack.pop_back();
                        if open_delim.is_some() && open_delim.unwrap().1 != '{' {
                            let delim = open_delim.unwrap();
                            return Err(ParseError {
                                abs_pos: delim.0,
                                span: (delim.0,self.pos),
                                msg: "Mismatched open bracket".into(),
                                input: self.input.clone()
                            });
                        } else if self.delim_stack.is_empty() {
                            current_word.push(c);
                            debug!("Pushing word to block: {}",current_word);
                            push_word(&mut current_block,&mut span_start,&mut current_word,self.pos);
                            span_start = self.pos + 1;
                            current_word.clear();
                            continue;
                        }
                    }
                    _ => unreachable!()

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
                '#' => { // Comment
                    while let Some(&next_char) = self.chars.front() {
                        if next_char == '\'' {
                            break;
                        } else {
                            self.advance();
                        }
                    }
                }
                ';' | '\n' => {
                    debug!("found block delimiter: {}",c);
                    if self.delim_stack.is_empty() {
                        debug!("pushing new block");
                        push_word(&mut current_block,&mut span_start,&mut current_word,self.pos);
                        debug!("current_word: {}",current_word);
                        debug!("current block: {:?}",current_block);
                        span_start = self.pos + 1;
                        current_word.clear();
                        self.blocks.push_back(current_block);
                    }
                    current_block = VecDeque::new();
                }
                ' ' | '\t' if self.delim_stack.is_empty() => {
                    if !current_word.is_empty() {
                        debug!("Pushing word to block: {}",current_word);
                        push_word(&mut current_block,&mut span_start,&mut current_word,self.pos);
                        span_start = self.pos + 1;
                        current_word.clear();
                    }
                }
                '"' | '\'' => {
                    debug!("found quotation mark: {}",c);
                    self.delim_stack.push_back((self.pos,c));  // Push opening delimiter onto the stack
                    current_word.push(c);           // Add the delimiter to the word

                    while let Some(char) = self.advance() {

                        // If we encounter the same delimiter, check the stack
                        if char == c {
                            // Only pop if the stack has the corresponding opening delimiter
                            if let Some(&(pos,stack_char)) = self.delim_stack.back() {
                                if stack_char == c {
                                    self.delim_stack.pop_back(); // Pop the matching delimiter
                                    break;  // Exit the loop once the closing delimiter is found
                                }
                            }
                        }
                        current_word.push(char);  // Continue adding characters inside the delimiter
                    }

                    // Check if the delimiter stack is mismatched (extra unmatched opening delimiter)
                    if !self.delim_stack.is_empty() {
                        if let Some(&(pos, stack_char)) = self.delim_stack.front() {
                            if stack_char == c {
                                return Err(ParseError {
                                    abs_pos: self.pos,
                                    span: (pos,self.pos),
                                    msg: "Mismatched quotation mark".to_string(),
                                    input: self.input.clone()
                                })
                            }
                        }
                    } else if self.delim_stack.is_empty() {
                            current_word.push(c);
                            debug!("Pushing word to block: {}",current_word);
                            push_word(&mut current_block,&mut span_start,&mut current_word,self.pos);
                            span_start = self.pos + 1;
                            current_word.clear();
                    }
                }
                '|' => {
                    debug!("found pipe");
                    // Pipe handling (|)
                    if !current_word.is_empty() {
                        debug!("Pushing word to block: {}",current_word);
                        push_word(&mut current_block,&mut span_start,&mut current_word,self.pos);
                        span_start = self.pos + 1;
                        current_word.clear();
                    }

                    current_word.push(c);
                    debug!("Pushed {} into current_word, current_word = {}",c,current_word);

                    if let Some(&next_char) = self.chars.front() {
                        match next_char {
                            '|' => {
                                // Handle '||'
                                current_word.push(self.advance().unwrap());
                                debug!("Found other pipe, pushing onto current word, it's now: {}",current_word);
                                debug!("Pushing word to block: {}",current_word);
                                push_word(&mut current_block,&mut span_start,&mut current_word,self.pos);
                                debug!("Current blocks: {:#?}",current_block);
                                current_word.clear();
                                span_start = self.pos + 1;
                            }
                            _ => {
                                debug!("Returning single pipe: {}",current_word);
                                // Handle single pipe '|'
                                debug!("Pushing word to block: {}",current_word);
                                push_word(&mut current_block,&mut span_start,&mut current_word,self.pos);
                                span_start = self.pos + 1;
                                current_word.clear();
                                self.advance();
                            }
                        }
                    }
                }
                '&' if matches!(*self.chars.front().unwrap(),'&') => {
                    // Handle '&' for background execution or '&&' for logical AND
                    if !current_word.is_empty() {
                        debug!("Pushing word to block: {}",current_word);
                        push_word(&mut current_block,&mut span_start,&mut current_word,self.pos);
                        span_start = self.pos + 1;
                        current_word.clear();
                    }

                    if let Some(next_char) = self.advance() {
                        match next_char {
                            '&' => {
                                // Handle '&&'
                                push_word(&mut current_block,&mut span_start,&mut current_word,self.pos);
                                span_start = self.pos + 1;
                                self.advance(); // Consume the next '&'
                            }
                            _ => {
                                // Handle single '&'
                                push_word(&mut current_block,&mut span_start,&mut current_word,self.pos);
                                span_start = self.pos + 1;
                            }
                        }
                    }
                }
                '&' | '0'..='9' | '>' | '<' => {
                    debug!("Maybe found redirection?");
                    current_word.push(c);
                    let mut redir_check_stack = self.chars.clone();
                    let mut redir_test_string: String = "".into();
                    let mut count = 0;

                    // Look ahead to see if we are in a redirection
                    while let Some(check_char) = redir_check_stack.pop_front() {
                        debug!("Checking character for redir test: {}",check_char);
                        if matches!(check_char, '&' | '0'..='9' | '>' | '<') {
                            debug!("Adding checked character: {}",check_char);
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
                            debug!("count = {}, adding \'{}\'",count,redir_char);
                            current_word.push(redir_char);
                            count -= 1
                        }
                        push_word(&mut current_block,&mut span_start,&mut current_word,self.pos);
                        span_start = self.pos + 1;
                        current_word.clear();
                    }
                }
                _ => {
                    // Add the character to the current word
                    debug!("pushing char: {}",c);
                    current_word.push(c);
                }
            }
        }
        if !self.delim_stack.is_empty() {
            debug!("Delimiter stack not empty! {:?}",self.delim_stack);
            let open_delim = self.delim_stack.pop_back();
            if let Some(delim) = open_delim {
                debug!("Returning parse error");
                return Err(ParseError {
                    abs_pos: delim.0,
                    span: (delim.0,self.pos),
                    msg: "Mismatched delimiter".into(),
                    input: self.input.clone()
                })
            }
        }

        if !current_word.is_empty() {
            debug!("Catching orphaned word: {}",current_word);
            push_word(&mut current_block,&mut span_start,&mut current_word,self.pos);
        }
        debug!("pushing block: {:?}",current_block);
        self.blocks.push_back(current_block.clone());
        Ok(())
    }


    pub fn parse_blocks(&mut self) -> Result<(),ParseError> {
        let mut unit_block = VecDeque::new();
        while let Some(mut block) = self.blocks.pop_front() {  // Take the current node and move it
            // Push the Unit created from the current node's word into `self.units`
            while let Some(word) = block.pop_front() {
                let string = word.clone();
                let unit = Unit::from(word,&self)?;
                debug!("Produced unit: {:?} from word {:?}",unit,string);
                unit_block.push_back(unit);
                // Move to the next node
            }
            self.units.push_back(unit_block);
            unit_block = VecDeque::new();
        }
        Ok(())
    }

    fn parse_unitlist(&mut self, list: Vec<Unit>) -> Vec<Eval> {
        // Go through the Unit vector
        // Match the contained structure to a member of the Eval enum
        todo!()
    }
}
