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
    pub fn openers() -> Vec<Keywords> {
        vec![
            Keywords::If,
            Keywords::While,
            Keywords::For,
            Keywords::Until,
            Keywords::Select,
            Keywords::Case,
        ]
    }

    pub fn separators() -> Vec<Keywords> {
        vec![
            Keywords::Then,
            Keywords::In,
            Keywords::Do,
            Keywords::Elif,
            Keywords::Else,
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
        commands: VecDeque<Eval>,
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

impl Conditional {
    pub fn new() -> Self {
        Self {
            condition: VecDeque::new(),
            body: VecDeque::new(),
        }
    }

    pub fn push_cond_eval(&mut self, eval: Eval) {
        self.condition.push_back(eval);
    }

    pub fn pop_cond_eval(&mut self) -> Option<Eval> {
        self.condition.pop_back()
    }

    pub fn peek_condition_eval(&mut self) -> Option<Eval> {
        if self.condition.back().is_some() {
            let eval = self.condition.pop_back().unwrap();
            self.condition.push_back(eval.clone());
            return Some(eval);
        } else {
            None
        }
    }

    pub fn push_body_eval(&mut self, eval: Eval) {
        self.body.push_back(eval);
    }

    pub fn pop_body_eval(&mut self) -> Option<Eval> {
        self.body.pop_back()
    }

    pub fn peek_body_eval(&mut self) -> Option<Eval> {
        if self.body.back().is_some() {
            let eval = self.body.pop_back().unwrap();
            self.body.push_back(eval.clone());
            return Some(eval);
        } else {
            None
        }
    }
}

/// This struct is responsible for handling the logic present in the `infer_from_blocks` Eval method.
/// One of these represents a layer of context. A stack of these is kept, and a new ParserContext
/// is pushed to the stack each time a new nested structure is descended into. All of the code in
/// `infer_from_blocks` executes on the topmost context, except for when it propagates upward after
/// completing a logical structure
#[derive(Debug)]
struct ParserContext<'a> {
    expecting: VecDeque<Keywords>,
    keyword_stack: VecDeque<String>,
    logic_components: VecDeque<Conditional>,
    loop_vars: VecDeque<Unit>,
    loop_array: VecDeque<Unit>,
    parser: &'a RshParser,
}

impl<'a> ParserContext<'a> {
    pub fn new(parser: &'a RshParser) -> Self {
        info!("Creating new ParserContext");
        Self {
            expecting: VecDeque::from(Keywords::any()),
            keyword_stack: VecDeque::new(),
            logic_components: VecDeque::new(),
            loop_vars: VecDeque::new(),
            loop_array: VecDeque::new(),
            parser,
        }
    }

    pub fn check_expectations(&self, unit: &Unit) -> Result<(), RshParseError> {
        debug!("Checking expectations for unit: {:?}", unit);
        debug!("Current expectations: {:?}", self.expecting);
        debug!("Current keyword stack: {:?}", self.keyword_stack);
        // TODO: clean this up
        let keyword = Keywords::new(unit.text());
        if !self.expecting.contains(&keyword) {
            error!(
                "Unexpected token: {}. Expected one of: {:?}",
                unit.text(),
                self.expecting
            );
            return Err(RshParseError {
                abs_pos: unit.pos(),
                span: unit.span(),
                msg: format!("unexpected token: {}", unit.text()),
                input: self.parser.input.clone(),
            });
        }

        trace!("Expectations met for token: {}", unit.text());
        Ok(())
    }

    fn new_expectation(&self, input: &str) -> VecDeque<Keywords> {
        trace!("Generating new expectations based on input: {}", input);
        let mut expecting: Vec<Keywords> = match input {
            "if" | "elif" => Keywords::from(vec!["then"]),
            "then" => Keywords::from(vec!["elif", "else", "fi"]),
            "else" => Keywords::from(vec!["fi"]),
            "in" | "while" | "until" => Keywords::from(vec!["do"]),
            "for" | "select" | "case" => Keywords::from(vec!["in"]),
            "do" => Keywords::from(vec!["done"]),
            _ => Keywords::from(vec![]),
        };

        if matches!(
            input,
            "if" | "do" | "else" | "elif" | "then" | "while" | "until"
        ) {
            expecting.extend(Keywords::openers());
        }

        trace!("New expectations: {:?}", expecting);
        VecDeque::from(expecting)
    }

    fn zip_if_block(&mut self) -> Result<Eval, RshParseError> {
        debug!("Zipping if block from logic components");
        trace!("Components: {:#?}", self.logic_components);
        let if_block = self.logic_components.pop_front().unwrap();
        let mut else_block: Option<Conditional> = self.logic_components.pop_back();

        if else_block.is_none() {
            debug!("Found a simple if statement with no else block");
            return Ok(Eval::IfThen {
                if_block,
                elif_blocks: VecDeque::new(),
                else_block: None,
            });
        }

        if !else_block.clone().unwrap().condition.is_empty() {
            debug!("Found elif block, pushing back to logic components");
            self.logic_components.push_back(else_block.clone().unwrap());
            else_block = None;
        }

        let mut elif_blocks: VecDeque<Conditional> = VecDeque::new();
        elif_blocks.extend(self.logic_components.drain(..));
        debug!(
            "Completed zipping if block with elif blocks: {:?} and else block: {:?}",
            elif_blocks, else_block
        );

        Ok(Eval::IfThen {
            if_block,
            elif_blocks,
            else_block,
        })
    }

    pub fn push_keyword(&mut self, unit: &Unit) {
        debug!("Pushing keyword: {}", unit.text());
        self.keyword_stack.push_back(unit.text().into());
        self.expecting = self.new_expectation(unit.text());
        trace!(
            "Keyword stack updated: {:?}, New expectations: {:?}",
            self.keyword_stack,
            self.expecting
        );
    }

    pub fn pop_keyword(&mut self) -> Option<String> {
        debug!("Popping keyword from stack");
        let keyword = self.keyword_stack.pop_back();
        if let Some(keyword) = &keyword {
            trace!("Popped keyword: {}", keyword);
        }
        if let Some(next) = self.next_keyword() {
            self.expecting = self.new_expectation(next);
        }
        trace!("Updated expectations after pop: {:?}", self.expecting);
        keyword
    }

    pub fn next_keyword(&self) -> Option<&str> {
        trace!("Fetching next keyword from stack");
        self.keyword_stack.back().map(|x| x.as_str())
    }

    pub fn first_keyword(&self) -> Option<&str> {
        trace!("Fetching first keyword from stack");
        self.keyword_stack.front().map(|x| x.as_str())
    }

    pub fn new_component(&mut self, unit: &Unit) -> Result<(), RshParseError> {
        debug!("Adding new component for unit: {:?}", unit);
        self.check_expectations(unit)?;
        self.logic_components.push_back(Conditional::new());
        trace!("Updated logic components: {:?}", self.logic_components);
        Ok(())
    }

    pub fn pop_component(&mut self) -> Option<Conditional> {
        debug!("Popping component from logic components");
        let component = self.logic_components.pop_back();
        trace!("Popped component: {:?}", component);
        component
    }

    pub fn push_condition(&mut self, eval: Eval) {
        debug!("Pushing condition eval: {:?}", eval);
        if let Some(mut component) = self.logic_components.pop_back() {
            component.push_cond_eval(eval);
            self.logic_components.push_back(component);
        }
        trace!("Updated logic components: {:?}", self.logic_components);
    }

    pub fn pop_condition(&mut self) -> Option<Eval> {
        if let Some(mut component) = self.logic_components.pop_back() {
            component.pop_cond_eval()
        } else {
            None
        }
    }

    pub fn peek_condition(&mut self) -> Option<Eval> {
        if let Some(mut component) = self.logic_components.pop_back() {
            component.peek_condition_eval()
        } else {
            None
        }
    }

    pub fn push_body(&mut self, eval: Eval) {
        debug!("Pushing body eval: {:?}", eval);
        if let Some(mut component) = self.logic_components.pop_back() {
            component.push_body_eval(eval);
            self.logic_components.push_back(component);
        }
        trace!("Updated logic components: {:?}", self.logic_components);
    }

    pub fn peek_body(&mut self) -> Option<Eval> {
        if let Some(mut component) = self.logic_components.pop_back() {
            component.peek_body_eval()
        } else {
            None
        }
    }

    pub fn pop_body(&mut self) -> Option<Eval> {
        if let Some(mut component) = self.logic_components.pop_back() {
            component.pop_body_eval()
        } else {
            None
        }
    }

    pub fn push_loop_var(&mut self, var: Unit) {
        debug!("Pushing loop variable: {:?}", var);
        self.loop_vars.push_back(var.clone());
        trace!("Updated loop variables: {:?}", self.loop_vars);
    }

    pub fn push_loop_element(&mut self, var: Unit) {
        debug!("Pushing loop element: {:?}", var);
        self.loop_array.push_back(var.clone());
        trace!("Updated loop array: {:?}", self.loop_array);
    }
}

impl Eval {
    pub fn infer_from_units(
        mut units: VecDeque<Unit>,
        parser: &RshParser,
    ) -> Result<VecDeque<Eval>, RshParseError> {
        let mut evals: VecDeque<Eval> = VecDeque::new();

        let mut ctx_layers: VecDeque<ParserContext> = VecDeque::from(vec![]);
        let mut ctx = ParserContext::new(parser);
        let mut first_run = true;

        // This switch flips after the first word is checked or if a keyword is found in a block
        // Only one control structure keyword per block, and it must be the first identifier.
        while let Some(unit) = units.pop_front() {
            trace!("Checking unit: {:?}", unit);

            //let mut cur_part = VecDeque::new();

            //while let Some(unit) = block.pop_front() {

            //match unit.utype() {
            //UnitType::Cmdsep => break,
            //UnitType::And => {
            //if block.front().is_none() {
            //return Err(RshParseError {
            //abs_pos: unit.pos(),
            //span: unit.span(),
            //msg: "Unexpected token following `&&` operator".into(),
            //input: parser.input.clone(),
            //});
            //}
            //let next_unit= block.front().unwrap();
            //Eval::match_expectation(unit.utype(), next_unit,parser)?;
            //return Ok(Eval::Chain {
            //left: Box::new(Eval::infer_command(cur_part, parser)?),
            //right: Box::new(Eval::infer_command(block, parser)?),
            //operator: ChainOp::Or,
            //});
            //}
            //UnitType::Or => {
            //if block.front().is_none() {
            //return Err(RshParseError {
            //abs_pos: unit.pos(),
            //span: unit.span(),
            //msg: "Unexpected token following `||` operator".into(),
            //input: parser.input.clone(),
            //});
            //}
            //let next_unit = block.front().unwrap();
            //Eval::match_expectation(unit.utype(), next_unit,parser)?;
            //return Ok(Eval::Chain {
            //left: Box::new(Eval::infer_command(cur_part, parser)?),
            //right: Box::new(Eval::infer_command(block, parser)?),
            //operator: ChainOp::Or,
            //});
            //}
            //_ => {
            //cur_part.push_back(unit);
            //}
            //}
            //}
            //
            //block.extend(cur_part.drain(..));
            //
            //let mut pipeline_cmds: VecDeque<Eval> = VecDeque::new();
            //while let Some(unit) = block.pop_front() {
            //
            //match unit.utype() {
            //UnitType::Pipe => {
            //if block.front().is_none() {
            //return Err(RshParseError {
            //abs_pos: unit.pos(),
            //span: unit.span(),
            //msg: "Unexpected token following `|` operator".into(),
            //input: parser.input.clone(),
            //});
            //}
            //let next_unit = block.front().unwrap();
            //Eval::match_expectation(unit.utype(), next_unit,parser)?;
            //pipeline_cmds.push_back(Eval::infer_command(cur_part.clone(), parser)?);
            //pipeline_cmds.push_back(Eval::infer_command(block.clone(), parser)?);
            //cur_part.clear();
            //}
            //_ => {
            //cur_part.push_back(unit);
            //}
            //}
            //}

            //if !pipeline_cmds.is_empty() {
            //return Ok(Eval::Pipeline { commands: pipeline_cmds });
            //}
            match unit.utype() {
                UnitType::Cmdsep => {
                    if let Some(keyword) = ctx.next_keyword() {
                        let mut local_context: VecDeque<Eval> = VecDeque::new();
                        let mut target: &str;
                        match keyword {
                            "then" | "else" | "do" => {
                                while let Some(eval) = ctx.pop_body() {
                                    local_context.push_back(eval);
                                }
                                target = "ctx_body";
                            }
                            "if" | "elif" | "while" | "until" => {
                                while let Some(eval) = ctx.pop_condition() {
                                    local_context.push_back(eval);
                                }
                                target = "ctx_condition";
                            }
                            _ => {
                                local_context.extend(evals.drain(..));
                                target = "evals";
                            }
                        }
                        debug!("Handling pipelines");
                        let mut work_stack: VecDeque<Eval> = VecDeque::new();
                        let mut pipeline_evals: VecDeque<Eval> = VecDeque::new();
                        while let Some(eval) = local_context.pop_back() {
                            trace!("Popped eval in search of pipes: {:?}", eval);
                            match eval {
                                Eval::Pipe => {
                                    if pipeline_evals.is_empty() {
                                        // TODO: handle unwrap
                                        let first_cmd = work_stack.pop_back().unwrap();
                                        pipeline_evals.push_back(first_cmd);
                                    }
                                    if let Some(right) = local_context.pop_back() {
                                        trace!("pushed pipeline eval: {:?}", right);
                                        pipeline_evals.push_back(right);
                                    }
                                }
                                _ => {
                                    if !pipeline_evals.is_empty() {
                                        trace!("pushed pipeline with evals: {:?}", pipeline_evals);
                                        work_stack.push_back(Eval::Pipeline {
                                            commands: pipeline_evals.clone(),
                                        });
                                        pipeline_evals.clear()
                                    }
                                    work_stack.push_back(eval);
                                }
                            }
                        }
                        // Pipelines constructed, drain back into body
                        while let Some(eval) = work_stack.pop_front() {
                            ctx.push_body(eval);
                        }
                        let mut chain_stack: VecDeque<(Eval, ChainOp, Option<Eval>)> =
                            VecDeque::new();
                        debug!("handling chain operators");

                        while let Some(eval) = local_context.pop_back() {
                            trace!("popped eval in search of chain operator: {:?}", eval);
                            match eval {
                                Eval::And | Eval::Or => {
                                    let operator = if matches!(eval, Eval::And) {
                                        ChainOp::And
                                    } else {
                                        ChainOp::Or
                                    };
                                    trace!("operator found: {:?}", operator);

                                    if chain_stack.is_empty() {
                                        // Start a new chain with the last command on the work stack
                                        if let Some(first_cmd) = work_stack.pop_back() {
                                            trace!("pushing initial value: {:?}", first_cmd);
                                            chain_stack.push_back((first_cmd, operator, None));
                                        } else {
                                            panic!(
                                                "Syntax error: Missing left-hand side for operator"
                                            );
                                        }
                                    }

                                    // Look for the next command (right-hand side of the operator)
                                    if let Some(right) = local_context.pop_back() {
                                        if let Some((lhs, op, rhs)) = chain_stack.back_mut() {
                                            // If the current chain is incomplete, add the right-hand side
                                            *op = operator;
                                            *rhs = Some(right);
                                        } else {
                                            // Start a new chain segment
                                            chain_stack.push_back((right, operator, None));
                                        }
                                    } else {
                                        panic!(
                                            "Syntax error: Missing right-hand side for operator"
                                        );
                                    }
                                }

                                // Handle regular commands
                                _ => {
                                    work_stack.push_back(eval);
                                }
                            }
                        }
                        // Once all operators are processed, combine the chain stack
                        while chain_stack.len() > 1 {
                            let (rhs, _, _) = chain_stack.pop_back().unwrap();
                            let (lhs, op, mid) = chain_stack.pop_back().unwrap();
                            chain_stack.push_back((
                                Eval::Chain {
                                    left: Box::new(lhs),
                                    right: Box::new(mid.unwrap()),
                                    operator: op,
                                },
                                op,
                                Some(rhs),
                            ));
                        }

                        // Push the final chain back to the work stack
                        if let Some((lhs, op, Some(rhs))) = chain_stack.pop_back() {
                            work_stack.push_back(Eval::Chain {
                                left: Box::new(lhs),
                                right: Box::new(rhs),
                                operator: op,
                            });
                        }
                        // Drain back into body
                        while let Some(eval) = work_stack.pop_front() {
                            ctx.push_body(eval);
                        }
                    }
                }
                UnitType::And | UnitType::Or | UnitType::Pipe => {
                    debug!("found an operator: {}", unit.text());
                    if let Some(keyword) = ctx.next_keyword() {
                        match keyword {
                            "then" | "else" | "do" => match unit.utype() {
                                UnitType::And => ctx.push_body(Eval::And),
                                UnitType::Or => ctx.push_body(Eval::Or),
                                UnitType::Pipe => ctx.push_body(Eval::Pipe),
                                _ => unreachable!(),
                            },
                            "if" | "elif" | "while" | "until" => match unit.utype() {
                                UnitType::And => ctx.push_condition(Eval::And),
                                UnitType::Or => ctx.push_condition(Eval::Or),
                                UnitType::Pipe => ctx.push_condition(Eval::Pipe),
                                _ => unreachable!(),
                            },
                            _ => {
                                debug!("Unexpected keyword context: {}", keyword);
                            }
                        }
                    } else {
                        debug!("No keyword context, treating as standalone operator");
                        match unit.utype() {
                            UnitType::And => evals.push_back(Eval::And),
                            UnitType::Or => evals.push_back(Eval::Or),
                            UnitType::Pipe => evals.push_back(Eval::Pipe),
                            _ => unreachable!(),
                        }
                    }
                }
                UnitType::Keyword => {
                    // Check if the keyword matches expectations
                    ctx.check_expectations(&unit)?;
                    match unit.text() {
                        "while" | "until" | "if" | "for" => {
                            if unit.text() == "if" {
                                if let Some(keyword) = ctx.next_keyword() {
                                    if matches!(keyword, "else") {
                                        return Err(RshParseError {
                                            abs_pos: unit.pos(),
                                            span: unit.span(),
                                            msg: "Found 'if' after 'else', use 'elif' instead."
                                                .into(),
                                            input: parser.input.clone(),
                                        });
                                    }
                                }
                            }
                            if first_run {
                                debug!("First run...");
                                first_run = false
                            } else {
                                debug!("Found opening keyword: {}", unit.text());
                                debug!("Descending...");
                                ctx_layers.push_back(ctx);
                                trace!("ctx_layers after pushing previous context:");
                                trace!("{:?}", ctx_layers);
                                ctx = ParserContext::new(parser);
                            }
                            ctx.new_component(&unit)?;
                            ctx.push_keyword(&unit);
                            if unit.text() == "for" {
                                debug!("Descending into for loop block");
                                // For loops are weird so lets get the vars and stuff here
                                while let Some(inner_unit) = units.pop_front() {
                                    match inner_unit.utype() {
                                        UnitType::Ident | UnitType::String { .. } => {
                                            match ctx.next_keyword() {
                                                Some("for") => {
                                                    info!(
                                                        "pushing loop variable: {}",
                                                        inner_unit.text()
                                                    );
                                                    ctx.push_loop_var(inner_unit.clone());
                                                }
                                                Some("in") => {
                                                    info!(
                                                        "pushing array element: {}",
                                                        inner_unit.text()
                                                    );
                                                    ctx.push_loop_element(inner_unit.clone());
                                                }
                                                _ => return Err(RshParseError {
                                                    abs_pos: unit.pos(),
                                                    span: unit.span(),
                                                    msg: format!(
                                                        "Unexpected keyword found in for loop: {}",
                                                        inner_unit.text()
                                                    ),
                                                    input: parser.input.clone(),
                                                }),
                                            }
                                        }
                                        UnitType::Keyword => {
                                            match inner_unit.text() {
                                                "in" if ctx.next_keyword() == Some("for") => {
                                                    info!("found keyword: {}", inner_unit.text());
                                                    ctx.push_keyword(&inner_unit);
                                                }
                                                "do" if ctx.next_keyword() == Some("in") => {
                                                    info!("found keyword: {}", inner_unit.text());
                                                    ctx.push_keyword(&inner_unit);
                                                    break;
                                                }
                                                _ => return Err(RshParseError {
                                                    abs_pos: unit.pos(),
                                                    span: unit.span(),
                                                    msg: format!(
                                                        "Unexpected keyword found in for loop: {}",
                                                        inner_unit.text()
                                                    ),
                                                    input: parser.input.clone(),
                                                }),
                                            }
                                        }
                                        _ => {
                                            return Err(RshParseError {
                                                abs_pos: unit.pos(),
                                                span: unit.span(),
                                                msg: format!(
                                                    "Unexpected unit type found in for loop: {}",
                                                    inner_unit.text()
                                                ),
                                                input: parser.input.clone(),
                                            })
                                        }
                                    }
                                }
                            }
                        }
                        "then" | "do" => {
                            match unit.text() {
                                "then" => {
                                    if !matches!(ctx.next_keyword(), Some("if") | Some("elif")) {
                                        return Err(RshParseError {
                                            abs_pos: unit.pos(),
                                            span: unit.span(),
                                            msg: format!(
                                                "then matched with incorrect keyword: {}",
                                                ctx.next_keyword().unwrap()
                                            ),
                                            input: parser.input.clone(),
                                        });
                                    }
                                }
                                "do" => {
                                    if !matches!(
                                        ctx.next_keyword(),
                                        Some("while") | Some("until") | Some("for") | Some("in")
                                    ) {
                                        return Err(RshParseError {
                                            abs_pos: unit.pos(),
                                            span: unit.span(),
                                            msg: format!(
                                                "do matched with incorrect keyword: {}",
                                                ctx.next_keyword().unwrap()
                                            ),
                                            input: parser.input.clone(),
                                        });
                                    }
                                }
                                _ => unreachable!(),
                            }
                            ctx.push_keyword(&unit);
                        }
                        "elif" => {
                            ctx.new_component(&unit)?;
                            ctx.push_keyword(&unit);
                        }
                        "else" => {
                            ctx.new_component(&unit)?;
                            ctx.push_keyword(&unit);
                        }
                        "fi" | "done" | "esac" => {
                            info!("Reached closing delimiter");
                            debug!(
                                "ctx_layers.is_empty: {}, ctx.first_keyword().is_some(): {}",
                                ctx_layers.is_empty(),
                                ctx.first_keyword().is_some()
                            );
                            trace!("ctx_layers before zipping structure: {:?}", ctx_layers);
                            if ctx_layers.is_empty() && ctx.first_keyword().is_some() {
                                match ctx.first_keyword().unwrap() {
                                    "if" => {
                                        let if_eval = ctx.zip_if_block()?;
                                        evals.push_back(if_eval);
                                    }
                                    "while" => evals.push_back(Eval::WhileDo {
                                        body: ctx.pop_component().unwrap(),
                                    }),
                                    "until" => evals.push_back(Eval::UntilDo {
                                        body: ctx.pop_component().unwrap(),
                                    }),
                                    "for" => evals.push_back(Eval::ForDo {
                                        vars: ctx.loop_vars.clone(),
                                        array: ctx.loop_array.clone(),
                                        body: ctx.pop_component().unwrap(),
                                    }),
                                    _ => {
                                        return Err(RshParseError {
                                            abs_pos: unit.pos(),
                                            span: unit.span(),
                                            msg: format!(
                                                "Unexpected unit type {}!",
                                                ctx.first_keyword().unwrap()
                                            ),
                                            input: parser.input.clone(),
                                        })
                                    }
                                }
                                ctx = ParserContext::new(parser);
                            } else if !ctx.keyword_stack.is_empty() && ctx.first_keyword().is_some()
                            {
                                let mut outer_ctx = ctx_layers.pop_front().unwrap();
                                let keyword = ctx.next_keyword().unwrap();
                                match keyword {
                                    "do" | "then" | "else" => {
                                        match ctx.first_keyword().unwrap() {
                                            "if" => {
                                                // Make a vecdeque here because
                                                // bodystack is two dimensional
                                                // This allows a single body to have
                                                // many nested constructs
                                                let if_eval = ctx.zip_if_block()?;
                                                outer_ctx.push_body(if_eval);
                                            }
                                            "while" => {
                                                let body = ctx.pop_component().unwrap();
                                                outer_ctx.push_body(Eval::WhileDo { body })
                                            }
                                            "until" => {
                                                let body = ctx.pop_component().unwrap();
                                                outer_ctx.push_body(Eval::UntilDo { body })
                                            }
                                            "for" => {
                                                let vars = ctx.loop_vars.clone();
                                                let array = ctx.loop_array.clone();
                                                let body = ctx.pop_component().unwrap();
                                                outer_ctx.push_body(Eval::ForDo {
                                                    vars,
                                                    array,
                                                    body,
                                                });
                                            }
                                            _ => unreachable!(),
                                        }
                                    }
                                    "if" | "while" | "until" | "elif" => {
                                        match ctx.first_keyword().unwrap() {
                                            "if" => {
                                                let if_eval = ctx.zip_if_block()?;
                                                outer_ctx.push_condition(if_eval);
                                            }
                                            "while" => {
                                                let body = ctx.pop_component().unwrap();
                                                outer_ctx.push_condition(Eval::WhileDo { body })
                                            }
                                            "until" => {
                                                let body = ctx.pop_component().unwrap();
                                                outer_ctx.push_condition(Eval::UntilDo { body })
                                            }
                                            "for" => {
                                                let vars = ctx.loop_vars.clone();
                                                let array = ctx.loop_array.clone();
                                                let body = ctx.pop_component().unwrap();
                                                outer_ctx.push_condition(Eval::ForDo {
                                                    vars,
                                                    array,
                                                    body,
                                                });
                                            }
                                            _ => unreachable!(),
                                        }
                                    }
                                    _ => return Err(RshParseError {
                                        abs_pos: unit.pos(),
                                        span: unit.span(),
                                        msg: format!(
                                            "Unexpected keyword context while finalizing 'if': {}",
                                            keyword
                                        ),
                                        input: parser.input.clone(),
                                    }),
                                }
                                ctx = outer_ctx;
                                debug!("Moving down a layer of context...");
                            }
                        }
                        _ => {
                            ctx.push_keyword(&unit);
                        }
                    }
                }
                UnitType::Ident => {
                    let mut args: VecDeque<Unit> = VecDeque::from(vec![unit]);

                    while let Some(unit) = units.pop_front() {
                        if !matches!(
                            unit.utype(),
                            UnitType::Cmdsep | UnitType::And | UnitType::Or | UnitType::Pipe
                        ) {
                            args.push_back(unit);
                        } else {
                            // Put the operator back
                            units.push_front(unit);
                            break;
                        }
                    }
                    let command: Eval;
                    if BUILTINS.contains(&args.front().unwrap().text()) {
                        command = Eval::Builtin { words: args };
                    } else {
                        command = Eval::Command { words: args };
                    }
                    // Replace the identifier
                    if let Some(keyword) = ctx.next_keyword() {
                        match keyword {
                            "then" | "else" | "do" => {
                                ctx.push_body(command);
                            }
                            "if" | "elif" | "while" | "until" => {
                                debug!("Adding to condition under context '{}'", keyword);
                                ctx.push_condition(command);
                            }
                            _ => {
                                debug!("Unexpected keyword context: {}", keyword);
                            }
                        }
                    } else {
                        debug!("No keyword context, treating as standalone command");
                        evals.push_back(command);
                    }
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

        debug!("Final evals: {:?}", evals);
        Ok(evals)
    }

    fn match_expectation(
        left: &UnitType,
        right: &Unit,
        parser: &RshParser,
    ) -> Result<bool, RshParseError> {
        match left {
            UnitType::And | UnitType::Or | UnitType::Pipe | UnitType::Redir { .. } => {
                Ok([UnitType::Ident].contains(right.utype()))
            }
            _ => Err(RshParseError {
                abs_pos: right.pos(),
                span: right.span(),
                msg: "unhandled unit type passed to expect_next_unit".into(),
                input: parser.input.clone(),
            }),
        }
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
    evals: VecDeque<Eval>,
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
        Eval::infer_from_units(self.units.clone(), self)
    }
}
