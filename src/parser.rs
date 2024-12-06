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
        m.insert("rsh_shebang".into(), Regex::new(r"^#!((?:/[^\s]+)+)((?:\s+arg:[a-zA-Z][a-zA-Z0-9_\-]*)*)$").unwrap());
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

const FUNCTIONS: [&str;1] =
    [
        // Will replace this with an actual functions implementation later
        // Used for now for word flags
        "PLACEHOLDER_TEXT"
    ];

const ALIASES: [&str;1] =
    [
        // Will replace this with an actual aliases implementation later
        // Used for now for word flags
        "PLACEHOLDER_TEXT"
    ];

const BUILTINS: [&str;13] =
    [
        "echo",
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

const KEYWORDS: [&str; 14] =
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
    ];

fn check_flag(flag_name: &str, flags: u32) -> bool {
    match flag_name {
        "keyword" => (flags & 1 << 1) != 0,
        "builtin" => (flags & 1 << 2) != 0,
        _ => unreachable!()
    }
}

#[derive(Debug,Clone)]
pub struct WordDesc {
    text: String,
    abs_pos: usize, // Where the word starts in the input string
    span: (usize,usize),
    flags: u32
}

#[derive(Debug,Clone)]
pub struct RshParseError {
    abs_pos: usize,
    span: (usize,usize),
    msg: String,
    input: String,
}

impl RshParseError {
    fn calc_error_data(&self) -> (usize,usize,usize,usize,VecDeque<char>) {

        // Not sorry for this

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
                debug!("breaking at newline");
                let both_lines = prev_line.clone() + &this_line;
                let all_the_lines = the_line_before_that.clone() + &both_lines;
                debug!("concatenating these lines: {}",all_the_lines);
                window = VecDeque::from(all_the_lines.chars().collect::<Vec<char>>());
                found = true;
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
            }
            this_line.push(next_char);
            index += 1;
        }
        if !found {
            debug!("breaking at newline");
            let both_lines = prev_line + &this_line;
            let all_the_lines = the_line_before_that + &both_lines;
            debug!("concatenating these lines: {}",all_the_lines);
            window = VecDeque::from(all_the_lines.chars().collect::<Vec<char>>());
        }

        debug!("Chars after calculation: {:?}", chars);
        if window.len() == 50 {
            for _ in 0..3 {
                window.push_front('.');
            }
        }
        (line,column,target_column, error_length, window)
    }
}

impl fmt::Display for RshParseError {

    /// This display function returns a window of the offending code. Under the specific part of
    /// the line where the error occured, a line is drawn blaming the specific token which caused
    /// the error
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {

        let (line,column,ref mut target_column, error_length, window) = self.calc_error_data();

        let window_slice: String = window.clone().into_iter().collect();
        let error_line = window_slice.split("\n").last().unwrap();
        debug!("target_column, error_length: {},{}",target_column,error_length);
        debug!("error_line: {}",error_line);
        writeln!(f, "\t")?;
        writeln!(f, "{};{} - Parse Error: {}", line, column, self.msg)?;
        writeln!(f, "-----------------------------------------------")?;
        writeln!(f, "\t")?;
        writeln!(f, "{}", window_slice)?;

        let mut error_pointer = String::new();
        if *target_column != 0 { *target_column -= 1 }
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
                break
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


#[derive(Clone,Debug)]
pub enum Eval {
    Command {
        words: VecDeque<Unit>,
    },
    Builtin {
        words: VecDeque<Unit>,
    },
    Function {
        words: VecDeque<Unit>,
    },
    Chain {
        left: Box<Eval>,
        right: Box<Eval>,
        operator: ChainOp
    },
    Pipeline {
        commands: VecDeque<Eval>,
    },
    IfThen {
        if_block: Conditional,
        elif_blocks: VecDeque<Conditional>,
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
    FuncDef {
        func_name: WordDesc,
        func_body: Vec<Eval>
    },
    NoOp
}

#[derive(Clone,Debug)]
pub struct Conditional {
    condition: VecDeque<Eval>,
    body: VecDeque<Eval>,
}

impl Eval {

    /// This function creates evaluable abstract syntax tree nodes from a 2-dimenional vector of Unit structs.
    /// Simple commands are constructed using a helper function
    /// Logical structures are constructed using an interative, stack-based approach
    /// The keyword_stack variable maintains context by stacking keywords.
    /// Keywords are popped from the stack when their associated delimiter is found. (e.g. if -> fi)
    /// Conditions and bodies are also kept in a stack. When the delimiter such as `fi` or `done` is found,
    /// the function "unwinds" until it finds the first keyword in the structure. Along the way, it
    /// populates each component of the structure with the relevant bodies and conditions.
    pub fn infer_from_blocks(
        mut blocks: VecDeque<VecDeque<Unit>>,
        parser: &RshParser,
    ) -> Result<VecDeque<Eval>, RshParseError> {
        debug!("Beginning inference from blocks: {:?}", blocks);

        let mut evals: VecDeque<Eval> = VecDeque::new();
        let mut expecting: VecDeque<VecDeque<String>> = VecDeque::new();
        let mut keyword_stack: VecDeque<String> = VecDeque::new();
        let mut body_stack: VecDeque<VecDeque<Eval>> = VecDeque::new();
        let mut condition_stack: VecDeque<VecDeque<Eval>> = VecDeque::new();

        // Intermediate values for building logical constructs like "if then" and "while do"
        let mut process_conditionals: VecDeque<Conditional> = VecDeque::new();

        while let Some(mut block) = blocks.pop_front() {
            debug!("Popped block: {:?}", block);
            while let Some(unit) = block.front() {
                debug!("Processing unit: {:?}", unit);

                match unit.utype() {
                    UnitType::Keyword => {
                        debug!("Detected keyword: {}", unit.text());

                        // Check if the keyword matches expectations
                        if let Some(keyword_list) = expecting.back() {
                            debug!("Current expectations: {:?}", keyword_list);
                            if !keyword_list.contains(&unit.text().to_string()) {
                                panic!("unexpected token: {}",unit.text());
                            }
                        }

                        match unit.text() {
                            "while" | "until" | "if" => {
                                debug!("Starting new {} block",unit.text());
                                keyword_stack.push_back(unit.text().into());
                                expecting.push_back(Eval::handle_expectation(unit.text()).into());
                                condition_stack.push_back(VecDeque::new());
                                debug!("pushing new condition onto stack, current stack: {:?}",condition_stack);
                                block.pop_front();
                            }
                            "then" | "do" => {
                                debug!("Processing '{}' block",unit.text());
                                if let Some(keyword) = keyword_stack.back() {
                                    match unit.text() {
                                        "then" => if !matches!(keyword.as_str(), "if" | "elif") {
                                            panic!("then matched with incorrect keyword: {}",keyword)
                                        }
                                        "do" => if !matches!(keyword.as_str(), "while" | "until" | "for") {
                                            panic!("do matched with incorrect keyword: {}",keyword)
                                        }
                                        _ => unreachable!()
                                    }
                                }
                                expecting.push_back(Eval::handle_expectation(unit.text()).into());
                                body_stack.push_back(VecDeque::new()); // Start a new body
                                keyword_stack.push_back(unit.text().into());
                                block.pop_front();
                            }
                            "elif" => {
                                keyword_stack.push_back(unit.text().into());
                                expecting.push_back(Eval::handle_expectation(unit.text()).into());
                                debug!("checking for body_stack");
                                if process_conditionals.is_empty() && body_stack.back().is_some() && condition_stack.back().is_some() {
                                    let body = body_stack.pop_back().unwrap();
                                    let condition = condition_stack.pop_back().unwrap();
                                    debug!("body and condition found: {:?},{:?}",body,condition);
                                    process_conditionals.push_back(Conditional {
                                        condition,
                                        body
                                    });
                                }
                                condition_stack.push_back(VecDeque::new());
                                block.pop_front();
                            }
                            "else" => {
                                debug!("Processing 'else' block");
                                expecting.push_back(Eval::handle_expectation("else").into());
                                body_stack.push_back(VecDeque::new());
                                keyword_stack.push_back(unit.text().into());
                                block.pop_front();
                            }
                            "fi" | "done" | "esac" => {
                                let block_delim = unit.text();
                                let mut block_type = String::new();
                                debug!("Closing 'if' block");
                                while let Some(keyword) = keyword_stack.back() {
                                    debug!("Keyword stack before processing: {:?}", keyword_stack);
                                    match keyword.as_str() {
                                        "else" if block_delim == "fi" => {
                                            debug!("Processing 'else' block during 'fi' closure");
                                            if body_stack.back().is_some() {
                                                process_conditionals.push_back(Conditional {
                                                    condition: VecDeque::from(vec![Eval::NoOp]),
                                                    body: body_stack.pop_back().unwrap(),
                                                });
                                                keyword_stack.pop_back();
                                            }
                                        }
                                        "elif" if block_delim == "fi" => {
                                            debug!("Processing 'elif' block during 'fi' closure");
                                            if body_stack.back().is_some() && condition_stack.back().is_some() {
                                                process_conditionals.push_back(Conditional {
                                                    condition: condition_stack.pop_back().unwrap(),
                                                    body: body_stack.pop_back().unwrap(),
                                                });
                                            }
                                            keyword_stack.pop_back();
                                        }
                                        "if" | "while" | "until" => {
                                            debug!("Processing '{}' block during '{}' closure",keyword,block_delim);
                                            if body_stack.back().is_some() && condition_stack.back().is_some() {
                                                process_conditionals.push_back(Conditional {
                                                    condition: condition_stack.pop_back().unwrap(),
                                                    body: body_stack.pop_back().unwrap(),
                                                });
                                            }
                                            block_type = keyword.into();
                                            keyword_stack.pop_back();
                                            break; // Exit loop once the matching 'if' is processed
                                        }
                                        "then" | "do" => {
                                            debug!("Encountered '{}', popping from keyword stack",keyword);
                                            keyword_stack.pop_back();
                                        }
                                        _ => panic!("Unexpected keyword while unwinding conditional: {}", keyword),
                                    }
                                }

                                debug!("After unwinding, keyword stack: {:?}", keyword_stack);

                                if !process_conditionals.is_empty() {
                                    debug!("Finalizing block");
                                    if keyword_stack.is_empty() {
                                        debug!("Keyword stack is empty, pushing completed block to evals");
                                        match block_type.as_str() {
                                            "if" => {
                                                let if_block = process_conditionals.pop_front().unwrap();
                                                let else_block = process_conditionals.pop_back();
                                                let elif_blocks = process_conditionals.clone();
                                                evals.push_back(Eval::IfThen {
                                                    if_block,
                                                    elif_blocks,
                                                    else_block,
                                                });
                                            },
                                            "while" => evals.push_back(Eval::WhileDo {
                                                body: process_conditionals.pop_front().unwrap(),
                                            }),
                                            "until" => evals.push_back(Eval::UntilDo {
                                                body: process_conditionals.pop_front().unwrap(),
                                            }),
                                            _ => unreachable!()
                                        }
                                    } else {
                                        let keyword = keyword_stack.back().unwrap();
                                        debug!("Keyword stack is not empty, handling context for '{:?}'", keyword);
                                        debug!("Keyword stack: {:?}",keyword_stack);
                                        match keyword.as_str() {
                                            "do" | "then" | "else" => {
                                                debug!("closure found outer keyword: {}",keyword);
                                                debug!("Pushing block to body_stack");
                                                match block_type.as_str() {
                                                    "if" => {
                                                        let if_block = process_conditionals.pop_front().unwrap();
                                                        let else_block = process_conditionals.pop_back();
                                                        let elif_blocks = process_conditionals.clone();
                                                        body_stack.push_back(VecDeque::from(vec![Eval::IfThen {
                                                            if_block,
                                                            elif_blocks,
                                                            else_block,
                                                        }]));
                                                    },
                                                    "while" => body_stack.push_back(VecDeque::from(vec![Eval::WhileDo {
                                                        body: process_conditionals.pop_front().unwrap(),
                                                    }])),
                                                    "until" => body_stack.push_back(VecDeque::from(vec![Eval::UntilDo {
                                                        body: process_conditionals.pop_front().unwrap(),
                                                    }])),
                                                    _ => unreachable!()
                                                }
                                            }
                                            "if" | "while" | "until" | "elif" => {
                                                debug!("closure found outer keyword: {}",keyword);
                                                match block_type.as_str() {
                                                    "if" => {
                                                        debug!("Pushing if block to body_stack");
                                                        let if_block = process_conditionals.pop_front().unwrap();
                                                        let else_block = process_conditionals.pop_back();
                                                        let elif_blocks = process_conditionals.clone();
                                                        condition_stack.push_back(VecDeque::from(vec![Eval::IfThen {
                                                            if_block,
                                                            elif_blocks,
                                                            else_block,
                                                        }]));
                                                    },
                                                    "while" => {
                                                        debug!("Pushing while block to body_stack");
                                                        condition_stack.push_back(VecDeque::from(vec![Eval::WhileDo {
                                                            body: process_conditionals.pop_front().unwrap(),
                                                        }]));
                                                    }
                                                    "until" => condition_stack.push_back(VecDeque::from(vec![Eval::UntilDo {
                                                        body: process_conditionals.pop_front().unwrap(),
                                                    }])),
                                                    _ => unreachable!()
                                                }
                                            }
                                            _ => panic!("Unexpected keyword context while finalizing 'if': {}", keyword),
                                        }
                                    }
                                }

                                debug!("Cleaning up intermediate values");
                                if let Some(keyword) = keyword_stack.back() {
                                    debug!("Setting expectations for keyword: {}",keyword);
                                    expecting.push_back(Self::handle_expectation(keyword).into());
                                }
                                process_conditionals.clear();
                                debug!("Popping '{}' block from front of the current block",block_delim);
                                debug!("Current body_stack state: {:#?}",body_stack);
                                debug!("Current condition_stack state: {:#?}",condition_stack);
                                block.pop_front();
                            }
                            _ => {
                                let keyword = unit.text().to_string();
                                debug!("Unhandled keyword: {}", keyword);
                                keyword_stack.push_back(keyword.clone());
                                expecting.push_back(Eval::handle_expectation(keyword.as_str()).into());
                                block.pop_front();
                            }
                        }
                    }
                    UnitType::Ident => {
                        debug!("Processing identifier: {}", unit.text());
                        if let Some(keyword) = keyword_stack.back() {
                            match keyword.as_str() {
                                "then" | "else" | "do" => {
                                    debug!("Adding to body under context '{}'", keyword);
                                    if let Some(body) = body_stack.back_mut() {
                                        body.push_back(Eval::infer_command(block.clone(), parser)?);
                                        debug!("New body state: {:?}",body);
                                        debug!("Body stack state: {:?}",body_stack);
                                    }
                                }
                                "if" | "elif" | "while" | "until" => {
                                    debug!("Adding to condition under context '{}'", keyword);
                                    if let Some(condition) = condition_stack.back_mut() {
                                        condition.push_back(Eval::infer_command(block.clone(), parser)?);
                                        debug!("New condition state: {:?}",condition);
                                        debug!("Condition stack state: {:?}",condition_stack);
                                    }
                                }
                                _ => {
                                    debug!("Unexpected keyword context: {}", keyword);
                                }
                            }
                        } else {
                            debug!("No keyword context, treating as standalone command");
                            evals.push_back(Eval::infer_command(block.clone(), parser)?);
                        }

                        block.clear();
                    }
                    _ => {
                        debug!("Encountered unexpected unit type: {:?}", unit.utype());
                        return Err(RshParseError {
                            abs_pos: unit.pos(),
                            span: unit.span(),
                            msg: "Failed to infer evaluation from unit".into(),
                            input: parser.input.clone(),
                        });
                    }
                }
            }
        }

        debug!("Final evals: {:?}", evals);
        Ok(evals)
    }

    fn handle_expectation(input: &str) -> Vec<String> {
        let openers = ["if", "while", "for", "until", "select", "case"];
        let mut expecting: Vec<&str> = match input {
            "if" | "elif" => vec!["then"],
            "then" => vec!["elif", "else", "fi"],
            "else" => vec!["fi"],
            "in" | "while" | "until" => vec!["do"],
            "for" | "select" | "case" => vec!["in"],
            "do" => vec!["done"],
            _ => vec![], // Handle unrecognized keywords gracefully
        };

        if matches!(input, "if" | "elif" | "then" | "while" | "until") {
            expecting.extend(&openers);
        }

        expecting.into_iter().map(|s| s.to_string()).collect()
    }

    fn match_expectation(left: &UnitType, right: &UnitType) -> bool {
        match left {
            UnitType::And | UnitType::Or | UnitType::Pipe | UnitType::Redir {..} => {
                [
                    UnitType::Ident
                ].contains(right)
            },
            _ => { panic!("Unhandled unit type passed to expect_next_unit") }
        }
    }

    fn infer_command(mut block: VecDeque<Unit>, parser: &RshParser) -> Result<Eval, RshParseError> {

        let mut cur_part = VecDeque::new();

        while let Some(unit) = block.pop_front() {

            match unit.utype() {
                UnitType::And => {
                    if block.front().is_none() {
                        return Err(RshParseError {
                            abs_pos: unit.pos(),
                            span: unit.span(),
                            msg: "Unexpected token following `&&` operator".into(),
                            input: parser.input.clone(),
                        });
                    }
                    let next_unit_type = block.front().unwrap().utype();
                    if !Eval::match_expectation(unit.utype(), next_unit_type) {
                        return Err(RshParseError {
                            abs_pos: unit.pos(),
                            span: unit.span(),
                            msg: "Expected identifier after `&&`".into(),
                            input: parser.input.clone(),
                        });
                    }
                    return Ok(Eval::Chain {
                        left: Box::new(Eval::infer_command(cur_part, parser)?),
                        right: Box::new(Eval::infer_command(block, parser)?),
                        operator: ChainOp::And,
                    });
                }
                UnitType::Or => {
                    if block.front().is_none() {
                        return Err(RshParseError {
                            abs_pos: unit.pos(),
                            span: unit.span(),
                            msg: "Unexpected token following `||` operator".into(),
                            input: parser.input.clone(),
                        });
                    }
                    let next_unit_type = block.front().unwrap().utype();
                    if !Eval::match_expectation(unit.utype(), next_unit_type) {
                        return Err(RshParseError {
                            abs_pos: unit.pos(),
                            span: unit.span(),
                            msg: "Expected identifier after `||`".into(),
                            input: parser.input.clone(),
                        });
                    }
                    return Ok(Eval::Chain {
                        left: Box::new(Eval::infer_command(cur_part, parser)?),
                        right: Box::new(Eval::infer_command(block, parser)?),
                        operator: ChainOp::Or,
                    });
                }
                _ => {
                    cur_part.push_back(unit);
                }
            }
        }

        block.extend(cur_part.drain(..));

        let mut pipeline_cmds: VecDeque<Eval> = VecDeque::new();
        while let Some(unit) = block.pop_front() {

            match unit.utype() {
                UnitType::Pipe => {
                    if block.front().is_none() {
                        return Err(RshParseError {
                            abs_pos: unit.pos(),
                            span: unit.span(),
                            msg: "Unexpected token following `|` operator".into(),
                            input: parser.input.clone(),
                        });
                    }
                    let next_unit_type = block.front().unwrap().utype();
                    if !Eval::match_expectation(unit.utype(), next_unit_type) {
                        return Err(RshParseError {
                            abs_pos: unit.pos(),
                            span: unit.span(),
                            msg: "Expected identifier after `|`".into(),
                            input: parser.input.clone(),
                        });
                    }
                    pipeline_cmds.push_back(Eval::infer_command(cur_part.clone(), parser)?);
                    pipeline_cmds.push_back(Eval::infer_command(block.clone(), parser)?);
                    cur_part.clear();
                }
                _ => {
                    cur_part.push_back(unit);
                }
            }
        }

        if !pipeline_cmds.is_empty() {
            return Ok(Eval::Pipeline { commands: pipeline_cmds });
        }

        block.extend(cur_part.drain(..));

        let mut args: VecDeque<Unit> = VecDeque::new();
        let cmd_name = block
            .front()
            .expect("Expected at least one unit in block")
            .clone()
            .word_desc
            .text;

        for unit in block {
            args.push_back(unit);
        }

        if BUILTINS.contains(&cmd_name.as_str()) {
            Ok(Eval::Builtin { words: args })
        } else {
            Ok(Eval::Command { words: args })
        }
    }
}

#[derive(Debug,Clone)]
pub struct Unit {
    pub unit_type: UnitType,
    pub word_desc: WordDesc,
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnitType {
    Shebang,
    RangeNum,
    RangeAlpha,
    ProcessSub,
    CommandSub,
    Arithmetic,
    Subshell,
    Test,
    Block,
    String {
        single_quote: bool,
    },
    VarSub,
    Assignment {
        var: String,
        value: String,
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
        fd_target: i32,
    },
    Keyword,
    Ident,
}

#[derive(Debug,Clone,PartialEq)]
pub enum RedirType {
    Output,
    Append,
    Input,
    Heredoc,
    Herestring
}

impl Unit {
    pub fn new(unit_type: UnitType, word_desc: WordDesc) -> Self {
        Self { unit_type, word_desc }
    }
    // The function that creates a Unit from a WordDesc
    pub fn from(word_desc: WordDesc, parser: &RshParser) -> Result<Unit,RshParseError> {
        let string = word_desc.text.clone();
        debug!("Evaluating unit type for: {}",string);
        match string.as_str() {
            _ if REGEX["path"].is_match(&string) => {
                let relative = !string.starts_with('/');
                Ok(Unit::new(UnitType::Path { relative }, word_desc))
            }
            _ if REGEX["assignment"].is_match(&string) => {
                let parts: Vec<&str> = string.split('=').collect();
                let (var, value) = (parts[0].into(), parts[1].to_string());
                Ok(Unit::new(UnitType::Assignment { var, value }, word_desc))
            }
            _ if REGEX["redirection"].is_match(&string) => {
                if let Some(captures) = REGEX["redirection"].captures(&string) {
                    let fd_out = captures.get(1).map_or("1", |m| m.as_str()).to_string().parse::<i32>().unwrap_or(1);
                    let operator = captures.get(2).map_or("none", |m| m.as_str()).to_string();
                    let fd_target = captures.get(4).map_or("1", |m| m.as_str()).to_string().parse::<i32>().unwrap_or(1);
                    debug!("redir operator: {}",operator);
                    if string.matches('<').count() > 3 || string.matches('>').count() > 2 {
                        return Err(RshParseError {
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
                        _ => return Err(RshParseError {
                            abs_pos: word_desc.abs_pos,
                            span: word_desc.span,
                            msg: "Invalid redirection operator".into(),
                            input: parser.input.clone()
                        }),
                    };
                    Ok(Unit::new(UnitType::Redir { redir_type, fd_out, fd_target, }, word_desc))
                } else { panic!("Redirection somehow succeeded and then failed on the same regex check?") }
            }
            _ if REGEX["rsh_shebang"].is_match(&string) => Ok(Unit::new(UnitType::Shebang, word_desc)),
            _ if REGEX["range_num"].is_match(&string) => Ok(Unit::new(UnitType::RangeNum, word_desc)),
            _ if REGEX["range_alpha"].is_match(&string) => Ok(Unit::new(UnitType::RangeAlpha, word_desc)),
            _ if REGEX["process_sub"].is_match(&string) => Ok(Unit::new(UnitType::ProcessSub, word_desc)),
            _ if REGEX["command_sub"].is_match(&string) => Ok(Unit::new(UnitType::CommandSub, word_desc)),
            _ if REGEX["arithmetic"].is_match(&string) => Ok(Unit::new(UnitType::Arithmetic, word_desc)),
            _ if REGEX["subshell"].is_match(&string) => Ok(Unit::new(UnitType::Subshell, word_desc)),
            _ if REGEX["test"].is_match(&string) => Ok(Unit::new(UnitType::Test, word_desc)),
            _ if REGEX["block"].is_match(&string) => Ok(Unit::new(UnitType::Block, word_desc)),
            _ if REGEX["string"].is_match(&string) => Ok(Unit::new(UnitType::String { single_quote: string.starts_with('\'') }, word_desc)),
            _ if REGEX["var_sub"].is_match(&string) => Ok(Unit::new(UnitType::VarSub, word_desc)),
            _ if REGEX["operator"].is_match(&string) => {
                match string.as_str() {
                    "||" => Ok(Unit::new(UnitType::Or, word_desc)),
                    "&&" => Ok(Unit::new(UnitType::And, word_desc)),
                    "|" => Ok(Unit::new(UnitType::Pipe, word_desc)),
                    _ => Err(RshParseError {
                            abs_pos: word_desc.abs_pos,
                            span: word_desc.span,
                            msg: "Invalid operator".into(),
                            input: parser.input.clone()
                        }),
                }
            }
            _ if KEYWORDS.contains(&string.as_str()) => Ok(Unit::new(UnitType::Keyword, word_desc)),
            _ if REGEX["ident"].is_match(&string) => Ok(Unit::new(UnitType::Ident, word_desc)),
            _ => Err(RshParseError {
                    abs_pos: word_desc.abs_pos,
                    span: word_desc.span,
                    msg: "Failed to classify word".into(),
                    input: parser.input.clone()
                }),
        }
    }
    pub fn text(&self) -> &str {
        self.word_desc.text.as_str()
    }
    pub fn utype(&self) -> &UnitType {
        &self.unit_type
    }
    pub fn pos(&self) -> usize {
        self.word_desc.abs_pos
    }
    pub fn span(&self) -> (usize,usize) {
        self.word_desc.span
    }
    pub fn start_span(&self) -> usize {
        self.word_desc.span.0
    }
    pub fn end_span(&self) -> usize {
        self.word_desc.span.1
    }
    pub fn flags(&self) -> u32 {
        self.word_desc.flags
    }
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
    pub fn push_word(
        cur_blk: &mut VecDeque<WordDesc>,
        span_start: &mut usize,
        cur_wd: &mut String,
        pos: usize)
        {
            let get_flags = |word: &String| -> u32 {
                let mut keyword_flag: u32 = 0;
                let mut builtin_flag: u32 = 0;
                let mut function_flag: u32 = 0;
                let mut alias_flag: u32 = 0;
                if KEYWORDS.contains(&word.as_str()) { keyword_flag += 1 }
                if BUILTINS.contains(&word.as_str()) { builtin_flag += 1 }
                if FUNCTIONS.contains(&word.as_str()) { function_flag += 1 }
                if ALIASES.contains(&word.as_str()) { alias_flag += 1 }
                // Shift values to proper bit position
                builtin_flag  <<= 1;
                function_flag <<= 2;
                alias_flag    <<= 3;
                // Squash flags together
                keyword_flag | builtin_flag | function_flag | alias_flag
            };
            if !cur_wd.is_empty() {
                cur_blk.push_back(WordDesc { text: cur_wd.clone(), abs_pos: *span_start, span: (*span_start, pos), flags: get_flags(cur_wd) });
                *span_start = pos + 1;
                cur_wd.clear();
            }

        }
    pub fn tokenize(&mut self) -> Result<(),RshParseError> {
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
                            return Err(RshParseError {
                                abs_pos: delim.0,
                                span: (delim.0,self.pos),
                                msg: "Mismatched open bracket".into(),
                                input: self.input.clone()
                            });
                        } else if self.delim_stack.is_empty() {
                            current_word.push(c);
                            Self::push_word(&mut current_block,&mut span_start,&mut current_word,self.pos);
                            continue;
                        }
                    }
                    ']'=> {
                        let open_delim = self.delim_stack.pop_back();
                        if open_delim.is_some() && open_delim.unwrap().1 != '[' {
                            let delim = open_delim.unwrap();
                            return Err(RshParseError {
                                abs_pos: delim.0,
                                span: (delim.0,self.pos),
                                msg: "Mismatched open bracket".into(),
                                input: self.input.clone()
                            });
                        } else if self.delim_stack.is_empty() {
                            current_word.push(c);
                            debug!("Pushing word to block: {}",current_word);
                            Self::push_word(&mut current_block,&mut span_start,&mut current_word,self.pos);
                            span_start = self.pos + 1;
                            current_word.clear();
                            continue;
                        }
                    }
                    '}'=> {
                        let open_delim = self.delim_stack.pop_back();
                        if open_delim.is_some() && open_delim.unwrap().1 != '{' {
                            let delim = open_delim.unwrap();
                            return Err(RshParseError {
                                abs_pos: delim.0,
                                span: (delim.0,self.pos),
                                msg: "Mismatched open bracket".into(),
                                input: self.input.clone()
                            });
                        } else if self.delim_stack.is_empty() {
                            current_word.push(c);
                            debug!("Pushing word to block: {}",current_word);
                            Self::push_word(&mut current_block,&mut span_start,&mut current_word,self.pos);
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
                '#' if self.chars.front() != Some(&'!') => { // Comment
                    while let Some(&next_char) = self.chars.front() {
                        if next_char == '\n' {
                            self.advance();
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
                        Self::push_word(&mut current_block,&mut span_start,&mut current_word,self.pos);
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
                        Self::push_word(&mut current_block,&mut span_start,&mut current_word,self.pos);
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
                                return Err(RshParseError {
                                    abs_pos: self.pos,
                                    span: (pos,self.pos),
                                    msg: "Mismatched quotation mark".to_string(),
                                    input: self.input.clone()
                                })
                            }
                            current_word.push(c);
                        }
                    } else if self.delim_stack.is_empty() {
                            current_word.push(c);
                            debug!("Pushing word to block: {}",current_word);
                            Self::push_word(&mut current_block,&mut span_start,&mut current_word,self.pos);
                            span_start = self.pos + 1;
                            current_word.clear();
                    }
                }
                '|' => {
                    debug!("found pipe");
                    // Pipe handling (|)
                    if !current_word.is_empty() {
                        debug!("Pushing word to block: {}",current_word);
                        Self::push_word(&mut current_block,&mut span_start,&mut current_word,self.pos);
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
                                Self::push_word(&mut current_block,&mut span_start,&mut current_word,self.pos);
                                debug!("Current blocks: {:#?}",current_block);
                                current_word.clear();
                                span_start = self.pos + 1;
                            }
                            _ => {
                                debug!("Returning single pipe: {}",current_word);
                                // Handle single pipe '|'
                                debug!("Pushing word to block: {}",current_word);
                                Self::push_word(&mut current_block,&mut span_start,&mut current_word,self.pos);
                                span_start = self.pos + 1;
                                current_word.clear();
                                self.advance();
                            }
                        }
                    }
                }
                '&' if matches!(*self.chars.front().unwrap(),'&') => {
                    debug!("found && operator");
                    // Handle '&' for background execution or '&&' for logical AND
                    if !current_word.is_empty() {
                        debug!("Pushing word to block: {}",current_word);
                        Self::push_word(&mut current_block,&mut span_start,&mut current_word,self.pos);
                        span_start = self.pos + 1;
                        current_word.clear();
                    }
                    current_word = "&&".into();
                    self.advance();
                    Self::push_word(&mut current_block,&mut span_start,&mut current_word,self.pos);
                    span_start = self.pos + 1;
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
                        Self::push_word(&mut current_block,&mut span_start,&mut current_word,self.pos);
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
                return Err(RshParseError {
                    abs_pos: delim.0,
                    span: (delim.0,self.pos),
                    msg: "Mismatched delimiter".into(),
                    input: self.input.clone()
                })
            }
        }

        if !current_word.is_empty() {
            debug!("Catching orphaned word: {}",current_word);
            Self::push_word(&mut current_block,&mut span_start,&mut current_word,self.pos);
        }
        debug!("pushing block: {:?}",current_block);
        self.blocks.push_back(current_block.clone());
        Ok(())
    }


    pub fn parse_blocks(&mut self) -> Result<(),RshParseError> {
        let mut unit_block = VecDeque::new();
        while let Some(mut block) = self.blocks.pop_front() {  // Take the current node and move it
            // Push the Unit created from the current node's word into `self.units`
            while let Some(word) = block.pop_front() {
                let string = word.clone();
                let unit = Unit::from(word,self)?;
                debug!("Produced unit: {:?} from word {:?}",unit,string);
                unit_block.push_back(unit);
                // Move to the next node
            }
            if !unit_block.is_empty() {
                self.units.push_back(unit_block);
            }
            unit_block = VecDeque::new();
        }
        Ok(())
    }

    pub fn parse_unitlist(&mut self) -> Result<VecDeque<Eval>,RshParseError> {
        Eval::infer_from_blocks(self.units.clone(), self)
    }
}
