use lazy_static::lazy_static;
use log::{debug, error, info, trace};
use regex::Regex;
use std::cmp::Ordering;
use std::collections::{HashMap, VecDeque};
use std::fmt;
use tokio::sync::mpsc::{channel, Receiver, Sender};

use crate::event::ShellEvent;
use crate::rsh::parse::ast::ASTNode;

lazy_static! {
    pub static ref REGEX: HashMap<String, Regex> = {
        let mut m = HashMap::new();
        m.insert(
            "redirection".into(),
            Regex::new(r"^([0-9]+)?(>{1,2}|<{1,3})([&]?[0-9]+)?$").unwrap(),
        );
        m.insert(
            "path".into(),
            Regex::new(r"^(\/(?:[^\/]+\/)*[^\/]*|(?:\.[\/\w\-]+\/?))$").unwrap(),
        );
        m.insert("range_num".into(), Regex::new(r"^\{\d+\.\.\d+\}$").unwrap());
        m.insert(
            "rsh_shebang".into(),
            Regex::new(r"^#!((?:/[^\s]+)+)((?:\s+arg:[a-zA-Z][a-zA-Z0-9_\-]*)*)$").unwrap(),
        );
        m.insert(
            "range_alpha".into(),
            Regex::new(r"^\{[a-zA-Z]\.\.[a-zA-Z]\}$").unwrap(),
        );
        m.insert("process_sub".into(), Regex::new(r"^>\(.*\)$").unwrap());
        m.insert("command_sub".into(), Regex::new(r"^\$\([^\)]+\)$").unwrap());
        m.insert(
            "arithmetic".into(),
            Regex::new(r"^\$\(\([^\)]+\)\)$").unwrap(),
        );
        m.insert("subshell".into(), Regex::new(r"^\([^\)]+\)$").unwrap());
        m.insert("test".into(), Regex::new(r"^\[\s*(.*?)\s*\]$").unwrap());
        m.insert("string".into(), Regex::new(r#"^\"([^\"]*)\"$"#).unwrap());
        m.insert(
            "var_sub".into(),
            Regex::new(r"^\$([A-Za-z_][A-Za-z0-9_]*)$").unwrap(),
        );
        m.insert("block".into(), Regex::new(r"^\{[^{}]*\}$").unwrap());
        m.insert(
            "assignment".to_string(),
            Regex::new(r"^[A-Za-z_][A-Za-z0-9_]*=.*$").unwrap(),
        );
        m.insert(
            "operator".into(),
            Regex::new(r"(?:&&|\|\||[><]=?|[|;&])").unwrap(),
        );
        m.insert("ident".into(), Regex::new(r"^[A-Za-z0-9_\-/\.]*$").unwrap());
        m.insert("cmdsep".into(), Regex::new(r"^(?:\n|;)$").unwrap());
        m
    };
}

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

const BUILTINS: [&str; 13] = [
    "echo", "set", "shift", "export", "readonly", "declare", "local", "unset", "trap", "eval",
    "exec", "source", "wait",
];

#[derive(Debug, PartialEq)]
pub enum Keywords {
    If,
    Then,
    Else,
    Elif,
    Fi,
    For,
    Select,
    While,
    Until,
    Do,
    Done,
    Case,
    Esac,
    In,
}

impl Keywords {
    pub fn new(string: &str) -> Self {
        match string {
            "if" => Keywords::If,
            "then" => Keywords::Then,
            "else" => Keywords::Else,
            "elif" => Keywords::Elif,
            "fi" => Keywords::Fi,
            "for" => Keywords::For,
            "select" => Keywords::Select,
            "while" => Keywords::While,
            "until" => Keywords::Until,
            "do" => Keywords::Do,
            "done" => Keywords::Done,
            "case" => Keywords::Case,
            "esac" => Keywords::Esac,
            "in" => Keywords::In,
            _ => unreachable!(),
        }
    }
    pub fn from(strings: Vec<&str>) -> Vec<Self> {
        let mut keywords: Vec<Self> = vec![];
        for string in strings {
            match string {
                "if" => keywords.push(Keywords::If),
                "then" => keywords.push(Keywords::Then),
                "else" => keywords.push(Keywords::Else),
                "elif" => keywords.push(Keywords::Elif),
                "fi" => keywords.push(Keywords::Fi),
                "for" => keywords.push(Keywords::For),
                "select" => keywords.push(Keywords::Select),
                "while" => keywords.push(Keywords::While),
                "until" => keywords.push(Keywords::Until),
                "do" => keywords.push(Keywords::Do),
                "done" => keywords.push(Keywords::Done),
                "case" => keywords.push(Keywords::Case),
                "esac" => keywords.push(Keywords::Esac),
                "in" => keywords.push(Keywords::In),
                _ => {}
            }
        }
        keywords
    }
    pub fn any() -> Vec<Keywords> {
        vec![
            Keywords::If,
            Keywords::Then,
            Keywords::Else,
            Keywords::Elif,
            Keywords::Fi,
            Keywords::For,
            Keywords::Select,
            Keywords::While,
            Keywords::Until,
            Keywords::Do,
            Keywords::Done,
            Keywords::Case,
            Keywords::Esac,
            Keywords::In,
        ]
    }
    pub fn openers() -> Vec<String> {
        vec![
            "if".into(),
            "while".into(),
            "for".into(),
            "until".into(),
            "select".into(),
            "case".into(),
        ]
    }

    pub fn separators() -> Vec<String> {
        vec![
            "then".into(),
            "in".into(),
            "do".into(),
            "elif".into(),
            "else".into(),
        ]
    }

    pub fn closers() -> Vec<Keywords> {
        vec![Keywords::Fi, Keywords::Esac, Keywords::Done]
    }
    pub fn check(string: &String) -> bool {
        Keywords::from(vec![&string])
            .iter()
            .find(|&kw| Keywords::any().contains(kw))
            .is_some()
    }
}

fn check_flag(flag_name: &str, flags: u32) -> bool {
    match flag_name {
        "keyword" => (flags & 1 << 1) != 0,
        "builtin" => (flags & 1 << 2) != 0,
        _ => unreachable!(),
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct WordDesc {
    text: String,
    abs_pos: usize, // Where the word starts in the input string
    span: (usize, usize),
    flags: u32,
}

#[derive(Debug, Clone)]
pub struct RshParseError {
    abs_pos: usize,
    span: (usize, usize),
    msg: String,
    input: String,
}

impl RshParseError {
    fn calc_error_data(&self) -> (usize, usize, usize, usize, VecDeque<char>) {
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
                debug!("concatenating these lines: {}", all_the_lines);
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
            debug!("breaking at newline");
            let both_lines = prev_line + &this_line;
            let all_the_lines = the_line_before_that + &both_lines;
            debug!("concatenating these lines: {}", all_the_lines);
            window = VecDeque::from(all_the_lines.chars().collect::<Vec<char>>());
        }

        debug!("Chars after calculation: {:?}", chars);
        if window.len() == 50 {
            for _ in 0..3 {
                window.push_front('.');
            }
        }
        (line, column, target_column, error_length, window)
    }
}

impl fmt::Display for RshParseError {
    /// This display function returns a window of the offending code. Under the specific part of
    /// the line where the error occured, a line is drawn blaming the specific token which caused
    /// the error
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (line, column, ref mut target_column, error_length, window) = self.calc_error_data();

        let window_slice: String = window.clone().into_iter().collect();
        let error_line = window_slice.split("\n").last().unwrap();
        debug!(
            "target_column, error_length: {},{}",
            target_column, error_length
        );
        debug!("error_line: {}", error_line);
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
        operator: ChainOp,
    },
    Pipeline {
        left: Box<Eval>,
        right: Box<Eval>
    },
    IfThen {
        if_block: Conditional,
        elif_blocks: VecDeque<Conditional>,
        else_block: Option<Conditional>,
    },
    WhileDo {
        body: Conditional,
    },
    UntilDo {
        body: Conditional,
    },
    ForDo {
        vars: VecDeque<Unit>,
        array: VecDeque<Unit>,
        body: Conditional,
    },
    CaseIn {
        var: WordDesc,
        body: Vec<Conditional>,
    },
    FuncDef {
        func_name: WordDesc,
        func_body: Vec<Eval>,
    },
    Cmdsep,
    And,
    Or,
    Pipe,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Conditional {
    condition: VecDeque<Eval>,
    body: VecDeque<Eval>,
}

/// This struct is responsible for handling the logic present in the `infer_from_blocks` Eval method.
/// One of these represents a layer of context. A stack of these is kept, and a new ParserContext
/// is pushed to the stack each time a new nested structure is descended into. All of the code in
/// `infer_from_blocks` executes on the topmost context, except for when it propagates upward after
/// completing a logical structure
impl Eval {
    pub fn begin_descent(units: &mut VecDeque<Unit>, parser: &mut RshParser) {
        let mut evals = VecDeque::new();
        while !units.is_empty() {
            evals.push_back(Self::descend(units, parser));
        }
        parser.evals.extend(evals.drain(..));
    }
    pub fn descend(units: &mut VecDeque<Unit>, parser: &mut RshParser) -> Eval {
        let mut unit_buffer = VecDeque::new();
        debug!("Starting descent with units: {:?}", units);
        // Handle leading and trailing command separators
        if let Some(unit) = units.pop_front() {
            if !matches!(unit.utype(), UnitType::Cmdsep) {
                units.push_front(unit)
            }
        }
        if let Some(unit) = units.pop_back() {
            if !matches!(unit.utype(), UnitType::Cmdsep) {
                units.push_back(unit)
            }
        }

        while let Some(unit) = units.pop_front() {
            match unit.utype() {
                UnitType::Keyword => {
                    info!("Found keyword, checking if it's an opener");
                    if Keywords::openers().contains(&unit.text().into()) {
                        trace!("Keyword is an opener, building structure");
                        units.push_front(unit);
                        return Self::build_structure(units, parser);
                    } else {
                        panic!(
                            "Expected an opening keyword but got this: {}",
                            unit.text()
                        );
                    }
                },
                _ => unit_buffer.push_back(unit)
            }
        }
        // No structure keywords have been found, now check for chains
        debug!("No chains found, draining unit_buffer back into units");
        units.extend(unit_buffer.drain(..));

        while let Some(unit) = units.pop_front() {
            debug!("Processing unit: {:?}", unit);
            match unit.utype() {
                UnitType::And | UnitType::Or => {
                    let operator = if matches!(unit.utype(), UnitType::And) {
                        ChainOp::And
                    } else {
                        ChainOp::Or
                    };
                    info!(
                        "Found chaining operator: {:?}, descending into unit_buffer and right sides",
                        operator
                    );

                    return Eval::Chain {
                        left: Box::new(Self::descend(&mut unit_buffer, parser)),
                        right: Box::new(Self::descend(units, parser)),
                        operator,
                    };
                }
                _ => {
                    trace!("Pushing unit onto unit_buffer stack: {:?}", unit);
                    unit_buffer.push_back(unit);
                }
            }
        }

        // No chains have been found, now check for pipelines
        debug!("No chains found, draining unit_buffer back into units");
        units.extend(unit_buffer.drain(..));
        while let Some(unit) = units.pop_front() {
            debug!("Processing unit for pipeline: {:?}", unit);
            match unit.utype() {
                UnitType::Pipe => {
                    info!("Found pipeline operator, building pipeline");

                    return Eval::Pipeline {
                        left: Box::new(Self::descend(&mut unit_buffer, parser)),
                        right: Box::new(Self::descend(units, parser)),
                    };
                }
                _ => {
                    trace!("Pushing unit onto unit_buffer stack for pipeline: {:?}", unit);
                    unit_buffer.push_back(unit);
                }
            }
        }

        // No pipelines have been found, check for commands
        debug!("No pipelines found, draining unit_buffer back into units");
        units.extend(unit_buffer.drain(..));

        if let Some(unit) = units.pop_front() {
            debug!("Processing final unit: {:?}", unit);
            match unit.utype() {
                UnitType::Ident => {
                    info!("Found identifier, building invocation");
                    units.push_front(unit);
                    Self::build_invocation(units, parser)
                }
                _ => panic!(
                    "Reached lowest depth and couldn't find logic for this unit: {:?}",
                    unit
                ),
            }
        } else {
            panic!("Reached max depth with an empty units deque");
        }
    }

    pub fn build_pipeline(units: &mut VecDeque<Unit>, parser: &mut RshParser) -> VecDeque<Eval> {
        let mut commands: VecDeque<Eval> = VecDeque::new();
        let mut unit_buffer: VecDeque<Unit> = VecDeque::new();
        while let Some(unit) = units.pop_front() {
            match unit.utype() {
                UnitType::Pipe => {
                    commands.push_back(Self::descend(&mut unit_buffer, parser));
                    unit_buffer.clear();
                }
                UnitType::Cmdsep => break,
                _ => {
                    unit_buffer.push_back(unit);
                }
            }
        }
        commands
    }

    pub fn build_invocation(units: &mut VecDeque<Unit>, parser: &mut RshParser) -> Eval {
        let mut args: VecDeque<Unit> = VecDeque::new();
        debug!("Starting build_invocation with units: {:?}", units);

        while let Some(unit) = units.pop_front() {
            debug!("Processing unit: {:?}", unit);

            if matches!(unit.utype(), UnitType::Cmdsep) {
                debug!("Found command separator: {:?}", unit);
                break
            }

            trace!("Pushing unit onto args: {:?}", unit);
            args.push_back(unit);
        }
        if BUILTINS.contains(&args[0].text()) {
            info!(
                "Identified as a builtin command: {:?}, returning Eval::Builtin",
                args[0].text()
            );
            Eval::Builtin { words: args }
        } else {
            info!(
                "Identified as a regular command: {:?}, returning Eval::Command",
                args[0].text()
            );
            Eval::Command { words: args }
        }

    }

    fn new_expectation(input: String) -> VecDeque<String> {
        trace!("Generating new expectations based on input: {}", input);
        let expecting: Vec<String> = match input.as_str() {
            "if" | "elif" => vec!["then".into()],
            "then" => vec!["elif".into(), "else".into(), "fi".into()],
            "else" => vec!["fi".into()],
            "in" | "while" | "until" => vec!["do".into()],
            "for" | "select" | "case" => vec!["in".into()],
            "do" => vec!["done".into()],
            _ => vec![],
        };

        expecting.into()
    }

    pub fn build_structure(units: &mut VecDeque<Unit>, parser: &mut RshParser) -> Eval {
        let mut closer: &str = "";
        let mut block_type: String = String::new();

        if let Some(unit) = units.front() {
            block_type = unit.text().into();
            closer = match unit.text() {
                "if" => "fi",
                "for" | "while" | "until" | "select" => "done",
                "case" => "esac",
                _ => unreachable!("Unexpected block type: {}", unit.text()),
            };
            info!("Starting to build structure for block type: {}", block_type);
        }

        let mut body: VecDeque<Eval> = VecDeque::new();
        let mut condition: VecDeque<Eval> = VecDeque::new();
        let mut components: VecDeque<Conditional> = VecDeque::new(); // Conditional = condition + body
        let mut first_run = true;
        let mut current_context: String = String::new();
        let mut expecting = VecDeque::new();
        let mut next_units: VecDeque<Unit> = VecDeque::new();

        debug!(
            "Initialized variables. Closer: '{}', Block type: '{}'",
            closer, block_type
        );

        while let Some(unit) = units.pop_front() {
            debug!("Processing unit: {:?}", unit);

            if first_run {
                current_context = unit.text().into();
                expecting = Self::new_expectation(current_context.clone());
                first_run = false;
                debug!(
                    "First run: Set current_context to '{}', expecting: {:?}",
                    current_context, expecting
                );
            }

            match unit.utype() {
                UnitType::Keyword => {
                    debug!("Found keyword: {}", unit.text());

                    if unit.text() == closer && expecting.contains(&unit.text().into()) {
                        info!("Found closer keyword: '{}'", closer);
                        match block_type.as_str() {
                            "if" => {
                                info!("Zipping if statement");
                                debug!("components: {:?}",components);
                                let if_block = components.pop_front().unwrap();
                                let mut else_block: Option<Conditional> = None;
                                if let Some(last_block) = components.pop_back() {
                                    if last_block.condition.is_empty() {
                                        else_block = Some(last_block)
                                    } else {
                                        components.push_back(last_block)
                                    }
                                }
                                let elif_blocks = components;
                                debug!(
                                    "Returning IfThen: if_block: {:?}, elif_blocks: {:?}, else_block: {:?}",
                                    if_block, elif_blocks, else_block
                                );
                                return Eval::IfThen {
                                    if_block,
                                    elif_blocks,
                                    else_block,
                                };
                            }
                            "while" => {
                                return Eval::WhileDo { body: components.pop_back().unwrap() }
                            }
                            "until" => {
                                return Eval::UntilDo { body: components.pop_back().unwrap() }
                            }
                            _ => panic!(
                                "Unexpected block type during structure creation: {}",
                                block_type
                            ),
                        }
                    }

                    if expecting.contains(&unit.text().into()) {
                        debug!(
                            "Keyword '{}' matches expectation. Updating current_context.",
                            unit.text()
                        );
                        current_context = unit.text().into();
                        expecting = Self::new_expectation(current_context.clone());
                    } else if !expecting.contains(&unit.text().into()) && current_context != unit.text() {
                        debug!(
                            "Keyword '{}' does not match expectation: {:?}",
                            unit.text(),
                            expecting
                        );
                        next_units.push_back(unit.clone());
                        debug!("next_units: {:?}",next_units);
                    }
                }
                UnitType::Cmdsep => {
                    if let Some(next_unit) = units.front() {
                        if !expecting.contains(&next_unit.text().into()) {
                            // If the next unit is expected, consume the command separator
                            // else, push the command separator to the next_units buffer
                            next_units.push_back(unit);
                            continue
                        }
                    }
                    debug!("Processing units '{:?}' under context: '{}'", next_units,current_context);

                    match current_context.as_str() {
                        "if" | "else" | "for" | "elif" | "while" | "until" => {
                            if matches!(current_context.as_str(), "else" | "for") {
                                let eval = Self::descend(&mut next_units, parser);
                                components.push_back(Conditional {
                                    condition: VecDeque::new(),
                                    body: VecDeque::from(vec![eval]),
                                });
                            } else {
                                let eval = Self::descend(&mut next_units, parser);
                                debug!("Pushing to condition: {:?}", eval);
                                condition.push_back(eval);
                            }
                        }
                        _ => {
                            let eval = Self::descend(&mut next_units, parser);
                            debug!("Pushing to body: {:?}", eval);
                            body.push_back(eval);
                            debug!(
                                "Finalizing component for context '{}'. Adding to components.",
                                current_context
                            );
                            components.push_back(Conditional {
                                condition: condition.clone(),
                                body: body.clone(),
                            });
                            condition.clear();
                            body.clear();
                        }
                    }
                }
                _ => {
                    trace!("defaulting to pushing to next_units for unit: {:?}",unit);
                    next_units.push_back(unit.clone())
                },
            }

            if unit.text() == closer {
                info!("Encountered closing keyword '{}'. Breaking loop.", closer);
                break;
            }
        }

        debug!(
            "Finished parsing structure for block type '{}'. Returning todo!",
            block_type
        );
        todo!()
    }
}


#[derive(Debug, Clone, PartialEq)]
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
    Cmdsep,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RedirType {
    Output,
    Append,
    Input,
    Heredoc,
    Herestring,
}

impl Unit {
    pub fn new(unit_type: UnitType, word_desc: WordDesc) -> Self {
        Self {
            unit_type,
            word_desc,
        }
    }
    // The function that creates a Unit from a WordDesc
    pub fn from(word_desc: WordDesc, parser: &RshParser) -> Result<Unit, RshParseError> {
        let string = word_desc.text.clone();
        debug!("Evaluating unit type for: {}", string);
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
                    let fd_out = captures
                        .get(1)
                        .map_or("1", |m| m.as_str())
                        .to_string()
                        .parse::<i32>()
                        .unwrap_or(1);
                    let operator = captures.get(2).map_or("none", |m| m.as_str()).to_string();
                    let fd_target = captures
                        .get(4)
                        .map_or("1", |m| m.as_str())
                        .to_string()
                        .parse::<i32>()
                        .unwrap_or(1);
                    debug!("redir operator: {}", operator);
                    if string.matches('<').count() > 3 || string.matches('>').count() > 2 {
                        return Err(RshParseError {
                            abs_pos: word_desc.abs_pos,
                            span: word_desc.span,
                            msg: "Invalid redirection operator".into(),
                            input: parser.input.clone(),
                        });
                    }
                    let redir_type = match operator.as_str() {
                        ">" => RedirType::Output,
                        ">>" => RedirType::Append,
                        "<" => RedirType::Input,
                        "<<" => RedirType::Heredoc,
                        "<<<" => RedirType::Herestring,
                        _ => {
                            return Err(RshParseError {
                                abs_pos: word_desc.abs_pos,
                                span: word_desc.span,
                                msg: "Invalid redirection operator".into(),
                                input: parser.input.clone(),
                            })
                        }
                    };
                    Ok(Unit::new(
                        UnitType::Redir {
                            redir_type,
                            fd_out,
                            fd_target,
                        },
                        word_desc,
                    ))
                } else {
                    panic!("Redirection somehow succeeded and then failed on the same regex check?")
                }
            }
            _ if REGEX["rsh_shebang"].is_match(&string) => {
                Ok(Unit::new(UnitType::Shebang, word_desc))
            }
            _ if REGEX["range_num"].is_match(&string) => {
                Ok(Unit::new(UnitType::RangeNum, word_desc))
            }
            _ if REGEX["range_alpha"].is_match(&string) => {
                Ok(Unit::new(UnitType::RangeAlpha, word_desc))
            }
            _ if REGEX["process_sub"].is_match(&string) => {
                Ok(Unit::new(UnitType::ProcessSub, word_desc))
            }
            _ if REGEX["command_sub"].is_match(&string) => {
                Ok(Unit::new(UnitType::CommandSub, word_desc))
            }
            _ if REGEX["arithmetic"].is_match(&string) => {
                Ok(Unit::new(UnitType::Arithmetic, word_desc))
            }
            _ if REGEX["subshell"].is_match(&string) => {
                Ok(Unit::new(UnitType::Subshell, word_desc))
            }
            _ if REGEX["test"].is_match(&string) => Ok(Unit::new(UnitType::Test, word_desc)),
            _ if REGEX["block"].is_match(&string) => Ok(Unit::new(UnitType::Block, word_desc)),
            _ if REGEX["string"].is_match(&string) => Ok(Unit::new(
                UnitType::String {
                    single_quote: string.starts_with('\''),
                },
                word_desc,
            )),
            _ if REGEX["var_sub"].is_match(&string) => Ok(Unit::new(UnitType::VarSub, word_desc)),
            _ if REGEX["cmdsep"].is_match(&string) => Ok(Unit::new(UnitType::Cmdsep, word_desc)),
            _ if REGEX["operator"].is_match(&string) => match string.as_str() {
                "||" => Ok(Unit::new(UnitType::Or, word_desc)),
                "&&" => Ok(Unit::new(UnitType::And, word_desc)),
                "|" => Ok(Unit::new(UnitType::Pipe, word_desc)),
                _ => Err(RshParseError {
                    abs_pos: word_desc.abs_pos,
                    span: word_desc.span,
                    msg: "Invalid operator".into(),
                    input: parser.input.clone(),
                }),
            },
            // TODO: implement some method for this or something
            _ if Keywords::check(&string) => Ok(Unit::new(UnitType::Keyword, word_desc)),
            _ if REGEX["ident"].is_match(&string) => Ok(Unit::new(UnitType::Ident, word_desc)),
            _ => Err(RshParseError {
                abs_pos: word_desc.abs_pos,
                span: word_desc.span,
                msg: "Failed to classify word".into(),
                input: parser.input.clone(),
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
    pub fn span(&self) -> (usize, usize) {
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

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ChainOp {
    And,
    Or,
}

#[derive(Clone, Debug)]
pub struct RshParser {
    input: String,
    chars: VecDeque<char>,
    words: VecDeque<WordDesc>,
    units: VecDeque<Unit>,
    pub evals: VecDeque<Eval>,
    delim_stack: VecDeque<(usize, char)>,
    pos: usize,
    window: VecDeque<char>,
    window_start_col: usize,
}

impl RshParser {
    pub fn new(input: &str) -> Self {
        let parser = Self {
            input: input.into(),
            chars: VecDeque::from(input.chars().collect::<Vec<char>>()),
            words: VecDeque::new(),
            units: VecDeque::new(),
            evals: VecDeque::new(),
            delim_stack: VecDeque::new(),
            pos: 0,
            window: VecDeque::new(),
            window_start_col: 1,
        };
        parser
    }
    pub fn get_evals(&self) -> VecDeque<Eval> {
        self.evals.clone()
    }

    pub fn print_tokens(&self) {
        println!("blocks:");
        for word in self.words.clone() {
            println!("{:?}", word);
        }
    }
    pub fn print_units(&self) {
        println!("units:");
        for unit in self.units.clone() {
            println!("{:#?}", unit);
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
        } else {
            None
        }
    }
    pub fn push_word(&mut self, span_start: &mut usize, cur_wd: &mut String, pos: usize) {
        let get_flags = |word: &String| -> u32 {
            let mut keyword_flag: u32 = 0;
            let mut builtin_flag: u32 = 0;
            let mut function_flag: u32 = 0;
            let mut alias_flag: u32 = 0;
            if Keywords::check(word) {
                keyword_flag += 1
            }
            if BUILTINS.contains(&word.as_str()) {
                builtin_flag += 1
            }
            if FUNCTIONS.contains(&word.as_str()) {
                function_flag += 1
            }
            if ALIASES.contains(&word.as_str()) {
                alias_flag += 1
            }
            // Shift values to proper bit position
            builtin_flag <<= 1;
            function_flag <<= 2;
            alias_flag <<= 3;
            // Squash flags together
            keyword_flag | builtin_flag | function_flag | alias_flag
        };
        if !cur_wd.is_empty() {
            self.words.push_back(WordDesc {
                text: cur_wd.clone(),
                abs_pos: *span_start,
                span: (*span_start, pos),
                flags: get_flags(cur_wd),
            });
            *span_start = pos + 1;
            cur_wd.clear();
        }
    }
    pub fn tokenize(&mut self) -> Result<(), RshParseError> {
        let mut current_word = String::new();
        let mut current_block: VecDeque<WordDesc> = VecDeque::new();
        let mut span_start = self.pos;
        while let Some(c) = self.advance() {
            debug!("checking char: {}", c);
            debug!("current_word: {}", current_word);
            if matches!(c, '(' | '[' | '{') {
                debug!("Pushing delimiter: {}", c);
                self.delim_stack.push_back((self.pos, c));
            }
            if matches!(c, ')' | ']' | '}') {
                match c {
                    ')' => {
                        let open_delim = self.delim_stack.pop_back();
                        if open_delim.is_some() && open_delim.unwrap().1 != '(' {
                            let delim = open_delim.unwrap();
                            return Err(RshParseError {
                                abs_pos: delim.0,
                                span: (delim.0, self.pos),
                                msg: "Mismatched open bracket".into(),
                                input: self.input.clone(),
                            });
                        } else if self.delim_stack.is_empty() {
                            current_word.push(c);
                            self.push_word(&mut span_start, &mut current_word, self.pos);
                            continue;
                        }
                    }
                    ']' => {
                        let open_delim = self.delim_stack.pop_back();
                        if open_delim.is_some() && open_delim.unwrap().1 != '[' {
                            let delim = open_delim.unwrap();
                            return Err(RshParseError {
                                abs_pos: delim.0,
                                span: (delim.0, self.pos),
                                msg: "Mismatched open bracket".into(),
                                input: self.input.clone(),
                            });
                        } else if self.delim_stack.is_empty() {
                            current_word.push(c);
                            debug!("Pushing word to block: {}", current_word);
                            self.push_word(&mut span_start, &mut current_word, self.pos);
                            span_start = self.pos + 1;
                            current_word.clear();
                            continue;
                        }
                    }
                    '}' => {
                        let open_delim = self.delim_stack.pop_back();
                        if open_delim.is_some() && open_delim.unwrap().1 != '{' {
                            let delim = open_delim.unwrap();
                            return Err(RshParseError {
                                abs_pos: delim.0,
                                span: (delim.0, self.pos),
                                msg: "Mismatched open bracket".into(),
                                input: self.input.clone(),
                            });
                        } else if self.delim_stack.is_empty() {
                            current_word.push(c);
                            debug!("Pushing word to block: {}", current_word);
                            self.push_word(&mut span_start, &mut current_word, self.pos);
                            span_start = self.pos + 1;
                            current_word.clear();
                            continue;
                        }
                    }
                    _ => unreachable!(),
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
                '#' if self.chars.front() != Some(&'!') => {
                    // Comment
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
                    debug!("found block delimiter: {}", c);
                    if self.delim_stack.is_empty() {
                        debug!("pushing new block");
                        if !current_word.is_empty() {
                            self.push_word(&mut span_start, &mut current_word, self.pos);
                        }
                        if self.words.front().is_some()
                            && !matches!(self.words.front().unwrap().text.as_str(), ";" | "\n")
                        {
                            // Only push a command separator if one is not already there
                            self.push_word(&mut span_start, &mut c.into(), self.pos);
                        }
                        debug!("current_word: {}", current_word);
                        debug!("current block: {:?}", current_block);
                        span_start = self.pos + 1;
                        current_word.clear();
                    }
                    current_block = VecDeque::new();
                }
                ' ' | '\t' if self.delim_stack.is_empty() => {
                    if !current_word.is_empty() {
                        debug!("Pushing word to block: {}", current_word);
                        self.push_word(&mut span_start, &mut current_word, self.pos);
                        span_start = self.pos + 1;
                        current_word.clear();
                    }
                }
                '"' | '\'' => {
                    debug!("found quotation mark: {}", c);
                    self.delim_stack.push_back((self.pos, c)); // Push opening delimiter onto the stack
                    current_word.push(c); // Add the delimiter to the word

                    while let Some(char) = self.advance() {
                        // If we encounter the same delimiter, check the stack
                        if char == c {
                            // Only pop if the stack has the corresponding opening delimiter
                            if let Some(&(pos, stack_char)) = self.delim_stack.back() {
                                if stack_char == c {
                                    self.delim_stack.pop_back(); // Pop the matching delimiter
                                    break; // Exit the loop once the closing delimiter is found
                                }
                            }
                        }
                        current_word.push(char); // Continue adding characters inside the delimiter
                    }

                    // Check if the delimiter stack is mismatched (extra unmatched opening delimiter)
                    if !self.delim_stack.is_empty() {
                        if let Some(&(pos, stack_char)) = self.delim_stack.front() {
                            if stack_char == c {
                                return Err(RshParseError {
                                    abs_pos: self.pos,
                                    span: (pos, self.pos),
                                    msg: "Mismatched quotation mark".to_string(),
                                    input: self.input.clone(),
                                });
                            }
                            current_word.push(c);
                        }
                    } else if self.delim_stack.is_empty() {
                        current_word.push(c);
                        debug!("Pushing word to block: {}", current_word);
                        self.push_word(&mut span_start, &mut current_word, self.pos);
                        span_start = self.pos + 1;
                        current_word.clear();
                    }
                }
                '|' => {
                    debug!("found pipe");
                    // Pipe handling (|)
                    if !current_word.is_empty() {
                        debug!("Pushing word to block: {}", current_word);
                        self.push_word(&mut span_start, &mut current_word, self.pos);
                        span_start = self.pos + 1;
                        current_word.clear();
                    }

                    current_word.push(c);
                    debug!(
                        "Pushed {} into current_word, current_word = {}",
                        c, current_word
                    );

                    if let Some(&next_char) = self.chars.front() {
                        match next_char {
                            '|' => {
                                // Handle '||'
                                current_word.push(self.advance().unwrap());
                                debug!(
                                    "Found other pipe, pushing onto current word, it's now: {}",
                                    current_word
                                );
                                debug!("Pushing word to block: {}", current_word);
                                self.push_word(&mut span_start, &mut current_word, self.pos);
                                debug!("Current blocks: {:#?}", current_block);
                                current_word.clear();
                                span_start = self.pos + 1;
                            }
                            _ => {
                                debug!("Returning single pipe: {}", current_word);
                                // Handle single pipe '|'
                                debug!("Pushing word to block: {}", current_word);
                                self.push_word(&mut span_start, &mut current_word, self.pos);
                                span_start = self.pos + 1;
                                current_word.clear();
                                self.advance();
                            }
                        }
                    }
                }
                '&' if matches!(*self.chars.front().unwrap(), '&') => {
                    debug!("found && operator");
                    // Handle '&' for background execution or '&&' for logical AND
                    if !current_word.is_empty() {
                        debug!("Pushing word to block: {}", current_word);
                        self.push_word(&mut span_start, &mut current_word, self.pos);
                        span_start = self.pos + 1;
                        current_word.clear();
                    }
                    current_word = "&&".into();
                    self.advance();
                    self.push_word(&mut span_start, &mut current_word, self.pos);
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
                        debug!("Checking character for redir test: {}", check_char);
                        if matches!(check_char, '&' | '0'..='9' | '>' | '<') {
                            debug!("Adding checked character: {}", check_char);
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
                            debug!("count = {}, adding \'{}\'", count, redir_char);
                            current_word.push(redir_char);
                            count -= 1
                        }
                        self.push_word(&mut span_start, &mut current_word, self.pos);
                        span_start = self.pos + 1;
                        current_word.clear();
                    }
                }
                _ => {
                    // Add the character to the current word
                    debug!("pushing char: {}", c);
                    current_word.push(c);
                }
            }
        }
        if !self.delim_stack.is_empty() {
            debug!("Delimiter stack not empty! {:?}", self.delim_stack);
            let open_delim = self.delim_stack.pop_back();
            if let Some(delim) = open_delim {
                debug!("Returning parse error");
                return Err(RshParseError {
                    abs_pos: delim.0,
                    span: (delim.0, self.pos),
                    msg: "Mismatched delimiter".into(),
                    input: self.input.clone(),
                });
            }
        }

        if !current_word.is_empty() {
            debug!("Catching orphaned word: {}", current_word);
            self.push_word(&mut span_start, &mut current_word, self.pos);
        }
        Ok(())
    }

    pub fn parse_blocks(&mut self) -> Result<(), RshParseError> {
        while let Some(word) = self.words.pop_front() {
            let string = word.clone();
            let unit = Unit::from(word, self)?;
            debug!("Produced unit: {:?} from word {:?}", unit, string);
            self.units.push_back(unit);
            // Move to the next node
        }
        Ok(())
    }

    pub fn parse_unitlist(&mut self) -> Result<VecDeque<Eval>, RshParseError> {
        let mut units = self.units.clone();
        Eval::begin_descent(&mut units, self);
        Ok(self.evals.clone())
    }
}
