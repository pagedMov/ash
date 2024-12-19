use std::collections::{HashMap, VecDeque};
use once_cell::sync::Lazy;
use log::{error,debug,info,trace,warn};
use thiserror::Error;

use crate::shellenv::ShellEnv;
use crate::interp::token::{tokenize, Tk, TkType};
use crate::interp::expand::expand;

pub static EXPECT: Lazy<HashMap<TkType, Vec<TkType>>> = Lazy::new(|| {
    let mut m = HashMap::new();
    m.insert(TkType::If,     vec![TkType::Then]);
    m.insert(TkType::Elif,   vec![TkType::Then]);
    m.insert(TkType::Else,   vec![TkType::Fi]);
    m.insert(TkType::Then,   vec![TkType::Fi, TkType::Elif, TkType::Else]);
    m.insert(TkType::Do,     vec![TkType::Done]); // `Do` expects `Done`
    m.insert(TkType::Case,   vec![TkType::Esac]); // `Case` expects `Esac`
    m.insert(TkType::Select, vec![TkType::Do]); // `Select` expects `Do`
    m.insert(TkType::While,  vec![TkType::Do]); // `While` expects `Do`
    m.insert(TkType::Until,  vec![TkType::Do]); // `Until` expects `Do`
    m.insert(TkType::For,    vec![TkType::Do]); // `Until` expects `Do`
    m.insert(TkType::CaseSep,    vec![TkType::CaseDelim]); // `Until` expects `Do`
    m
});

pub const OPENERS: [TkType;7] = [
    TkType::If,
    TkType::For,
    TkType::Until,
    TkType::While,
    TkType::Case,
    TkType::Select,
    TkType::CaseSep
];

/// Used in contexts that dont have direct access to the original input.
/// Propagated upwards
#[derive(Error,Debug)]
pub enum RshErr {
    #[error("Parse error: {msg} at span {span:?}")]
    Parse {
        msg: String,
        span: (usize,usize)
    }
}

impl RshErr {
    pub fn from_parse(msg: String, span: (usize,usize)) -> Self {
        RshErr::Parse { msg, span }
    }
}

/// Used in contexts which do have direct access to the original input.
/// ParseErrs are propagated upward until reaching a context which contains the ParseState, at
/// which point they are converted into a ParseErrFull
#[derive(Debug,Error)]
#[error("Parse error: {msg}\n{pos_display}\n{window}\n{pointer}")]
pub struct ParseErrFull {
    pub msg: String,
    pub line: usize,
    pub col: usize,
    pub pos_display: String,
    pub window: String,
    pub pointer: String
}

impl ParseErrFull {
    pub fn from(err: RshErr, input: &str) -> Self {
        match err {
            RshErr::Parse { msg, span } => {
                let (line, col) = Self::get_line_col(input, span.0);
                let (window, window_offset) = Self::generate_window(input, line, col);
                let span_diff = span.1 - span.0;
                let pointer = Self::get_pointer(span_diff, window_offset);
                let pos_display = format!("{};{}:", line + 1, col + 1);
                Self {
                    msg,
                    line,
                    col,
                    pos_display,
                    window,
                    pointer,
                }
            }
            _ => unreachable!(),
        }
    }

    fn get_pointer(span_diff: usize, offset: usize) -> String {
        let padding = " ".repeat(offset); // Adjust padding to align with the start of the window
        let visible_span = span_diff.min(40 - offset); // Ensure the pointer doesn't exceed the window

        let mut pointer = String::new();
        pointer.push('^'); // Start with the initial pointer
        if visible_span > 1 {
            pointer.push_str(&"~".repeat(visible_span - 2));
            pointer.push('^'); // End with the final pointer
        }

        format!("{}{}", padding, pointer)
    }

    /// Calculate the line and column from a byte offset.
    fn get_line_col(input: &str, offset: usize) -> (usize, usize) {
        let mut line = 0;
        let mut col = 0;

        for (i, ch) in input.chars().enumerate() {
            if i == offset {
                break;
            }
            if ch == '\n' {
                line += 1;
                col = 0;
            } else {
                col += 1;
            }
        }

        (line, col)
    }

    /// Generate a sliding window of up to 20 characters around the error column.
    fn generate_window(input: &str, error_line: usize, error_col: usize) -> (String, usize) {
        let window_width = 40; // Maximum characters in the sliding window
        let lines: Vec<&str> = input.lines().collect();

        if lines.len() <= error_line {
            return ("Error line out of range".into(), 0); // Handle out-of-range error_line
        }

        let offending_line = lines[error_line];
        let line_len = offending_line.len();

        // Determine the sliding window start and end positions
        let start = if error_col > 10 {
            error_col.saturating_sub(10)
        } else {
            0
        };
        let end = (start + window_width).min(line_len);

        // Extract the sliding window and return its start position
        (offending_line[start..end].to_string(), error_col - start)
    }
}

#[derive(Debug,Clone)]
pub struct Conditional {
    pub condition: Box<Node>,
    pub body: Box<Node>
}

#[derive(Debug,Clone)]
pub struct CaseBlock {
    pub pattern: String,
    pub logic: Node
}

#[derive(Debug,Clone)]
pub struct Node {
    pub nd_type: NdType,
    pub span: (usize,usize)
}

impl Node {
    pub fn span(&self) -> (usize,usize) {
        self.span
    }
    pub fn node_type(&self) -> &NdType {
        &self.nd_type
    }
    pub fn set_span(&self,span: (usize,usize)) -> Self {
        Self {
            nd_type: self.nd_type.clone(),
            span
        }
    }
}
#[derive(Debug,Clone)]
pub enum NdType {
    Root { deck: VecDeque<Node> },
    If { cond_blocks: VecDeque<Conditional>, else_block: Option<Box<Node>> },
    For { loop_vars: VecDeque<Tk>, loop_arr: VecDeque<Tk>, loop_body: Box<Node> },
    Loop { condition: bool, logic: Conditional },
    Case { input_var: Tk, cases: VecDeque<CaseBlock> },
    Select { select_var: Tk, opts: VecDeque<Tk>, body: Box<Node> },
    Pipeline { left: Box<Node>, right: Box<Node> },
    Chain { left: Box<Node>, right: Box<Node>, op: Box<Node> },
    BraceGroup { body: Box<Node> },
    Subshell { body: String }, // It's a string because we're going to parse it in a subshell later
    FuncDef { name: String, body: Box<Node> },
    Assignment {name: String, value: Option<String> },
    Command { argv: VecDeque<Tk>, redirs: VecDeque<Tk> },
    And,
    Or,
    Pipe,
    Cmdsep
}

#[derive(Debug,Clone)]
pub struct ParseState<'a> {
    pub input: &'a str,
    pub shellenv: &'a ShellEnv,
    pub tokens: VecDeque<Tk>,
    pub ast: Node
}

pub fn descend<'a>(input: &'a str, shellenv: &'a ShellEnv) -> Result<ParseState<'a>,ParseErrFull> {
    info!("Starting descent into parsing with input: {:?}", input);
    let state = ParseState {
        input,
        shellenv,
        tokens: VecDeque::new(),
        ast: Node {
            nd_type: NdType::Root { deck: VecDeque::new() },
            span: (0,input.len())
        }
    };

    let state = tokenize(state);

    let state = expand(state);

    let state = parse(state);

    state
}

pub fn parse(state: ParseState) -> Result<ParseState,ParseErrFull> {
    info!("Parsing tokens into AST...");
    let tokens = state.tokens.clone();
    trace!("Tokens before parsing: {:?}", tokens);

    trace!("Tokens collected into Vec: {:?}", tokens);

    match get_tree(tokens) {
        Ok(ast) => {
            debug!("Generated AST: {:#?}", ast);

            Ok(ParseState {
                input: state.input,
                shellenv: state.shellenv,
                tokens: state.tokens,
                ast
            })
        }
        Err(e) => {
            Err(ParseErrFull::from(e, state.input))
        }
    }
}

pub fn get_tree(tokens: VecDeque<Tk>) -> Result<Node, RshErr> {
    trace!("Building AST from tokens: {:?}",tokens);
    let span = compute_span(tokens.clone());
    let nodes = parse_linear(tokens)?;
    trace!("Linear nodes parsed: {:?}", nodes);

    let deck = join_at_operators(nodes)?;
    trace!("AST deck after joining operators: {:?}", deck);

    Ok(
        Node {
            nd_type: NdType::Root { deck },
            span
        }
    )
}

pub fn get_tk_texts(tokens: &VecDeque<Tk>) -> Vec<String> {
    let mut texts = vec![];
    for tk in tokens {
        texts.push(tk.text().into())
    }
    texts
}

pub fn parse_linear(mut tokens: VecDeque<Tk>) -> Result<VecDeque<Node>, RshErr> {
    // First pass just makes nodes without joining at operators
    info!("Starting linear parsing of tokens...");
    let mut nodes = VecDeque::new();
    while let Some(tk) = tokens.front() {
        trace!("Current tokens: {:?}", tokens);
        use crate::interp::token::TkType::*;
        match tk.class() {
            If => {
                info!("Found 'if' token, processing...");
                info!("tokens: {:?}",get_tk_texts(&tokens));
                let (node, remaining_tokens) = build_if(tokens)?;
                nodes.push_back(node);
                tokens = remaining_tokens;
                continue
            }
            While => {
                info!("Found 'while' token, processing...");
                let (node, remaining_tokens) = build_loop(true, tokens)?;
                nodes.push_back(node);
                tokens = remaining_tokens;
                continue
            }
            Until => {
                info!("Found 'until' token, processing...");
                let (node, remaining_tokens) = build_loop(false, tokens)?;
                nodes.push_back(node);
                tokens = remaining_tokens;
                continue
            }
            For => {
                info!("Found 'for' token, processing...");
                let (node, remaining_tokens) = build_for(tokens)?;
                nodes.push_back(node);
                tokens = remaining_tokens;
                continue
            }
            Case => {
                info!("Found 'case' token, processing...");
                let (node, remaining_tokens) = build_case(tokens)?;
                nodes.push_back(node);
                tokens = remaining_tokens;
                continue
            }
            Select => {
                info!("Found 'select' token, processing...");
                let (node, remaining_tokens) = build_select(tokens)?;
                nodes.push_back(node);
                tokens = remaining_tokens;
                continue
            }
            Ident | String => {
                info!("Found command or string token, processing...");
                let (command_node, remaining_tokens) = build_command(tokens)?;
                nodes.push_back(command_node);
                tokens = remaining_tokens;
                // Fall through
            }
            Elif | Do | Then | Else => {
                tokens.pop_front();
                continue
            }
            Esac | Done | Fi => {
                tokens.pop_front();
                // Fall through
            }
            CaseSep | CaseDelim => {
                return Err(RshErr::from_parse(
                    format!("Found this outside of a case statement: {}", tokens[0].text()),
                    tokens[0].span(),
                ));
            }
            SOI => {
                trace!("Skipping Start of Input token");
                tokens.pop_front();
                continue
            }
            EOI => {
                info!("End of Input token encountered, stopping parsing");
                break;
            }
            Cmdsep | LogicAnd | LogicOr | Pipe => {
                info!("Found operator token: {:?}, preserving as node", tokens[0].class());
                match tk.class() {
                    Cmdsep => nodes.push_back(Node { nd_type: NdType::Cmdsep, span: tokens[0].span() }),
                    LogicAnd => nodes.push_back(Node { nd_type: NdType::And, span: tokens[0].span() }),
                    LogicOr => nodes.push_back(Node { nd_type: NdType::Or, span: tokens[0].span() }),
                    Pipe => nodes.push_back(Node { nd_type: NdType::Pipe, span: tokens[0].span() }),
                    _ => unreachable!(),
                }
                tokens.pop_front();
            }
            _ => {
                unimplemented!(
                    "Support for token type `{:?}` is not implemented yet",
                    tokens[0].class()
                );
            }
        }
        trace!("Current nodes: {:?}", nodes);
    }

    trace!("Completed linear parsing, nodes: {:?}", nodes);
    Ok(nodes)
}

pub fn join_at_operators(mut deck: VecDeque<Node>) -> Result<VecDeque<Node>, RshErr> {
    // Second pass will collect nodes at operators and construct pipelines and chains
    let mut buffer: VecDeque<Node> = VecDeque::new();
    let mut left: Option<Node> = None;

    trace!("Starting join_at_operators with deck: {:?}", deck);

    while let Some(node) = deck.pop_front() {
        trace!("Processing node: {:?}", node);
        match node.nd_type {
            NdType::And | NdType::Or => {
                trace!("Found logical operator: {:?}", node);
                if let Some(ref l_node) = left {
                    if let Some(r_node) = deck.pop_front() {
                        trace!(
                            "Creating Chain node with left: {:?}, operator: {:?}, right: {:?}",
                            l_node,
                            node,
                            r_node
                        );
                        let span = (l_node.span.0, r_node.span.1);
                        let chain = Node {
                            nd_type: NdType::Chain {
                                left: Box::new(l_node.clone()),
                                right: Box::new(r_node),
                                op: Box::new(node),
                            },
                            span,
                        };
                        left = Some(chain);
                    } else {
                        panic!("Operator {:?} is missing a right operand", node)
                    }
                } else {
                    panic!("Operator {:?} is missing a left operand", node)
                }
            }
            NdType::Pipe => {
                debug!("Found Pipe operator");
                if let Some(ref l_node) = left {
                    if let Some(r_node) = deck.pop_front() {
                        trace!(
                            "Creating Pipeline node with left: {:?}, right: {:?}",
                            l_node,
                            r_node
                        );
                        let span = (l_node.span.0, r_node.span.1);
                        let pipeline = Node {
                            nd_type: NdType::Pipeline {
                                left: Box::new(l_node.clone()),
                                right: Box::new(r_node),
                            },
                            span,
                        };
                        left = Some(pipeline);
                    } else {
                        panic!("Pipe operator is missing a right operand")
                    }
                } else {
                    panic!("Pipe operator is missing a left operand")
                }
            }
            NdType::Cmdsep => {
                debug!("Found Cmdsep");
                if let Some(node) = left.take() {
                    trace!("Pushing node to buffer: {:?}", node);
                    buffer.push_back(node);
                }
                // Create a standalone Cmdsep node with its span
                let span = node.span;
                let cmdsep_node = Node {
                    nd_type: NdType::Cmdsep,
                    span,
                };
                buffer.push_back(cmdsep_node);
            }
            _ => {
                trace!("Found general node: {:?}", node);
                if let Some(node) = left.take() {
                    trace!("Pushing node to buffer: {:?}", node);
                    buffer.push_back(node);
                }
                left = Some(node);
            }
        }
    }

    if let Some(node) = left {
        trace!("Pushing final node to buffer: {:?}", node);
        buffer.push_back(node);
    }

    trace!("Finished join_at_operators. Resulting buffer: {:?}", buffer);

    Ok(buffer)
}

fn extract_until(
    mut tokens: VecDeque<Tk>,
    stop_classes: &[TkType],
) -> (VecDeque<Tk>, VecDeque<Tk>) {
    let mut product = VecDeque::new();

    while let Some(tk) = tokens.pop_front() {
        debug!("extract_until: found token {:?}", tk.text());

        match tk.class() {
            // Handle openers by recursively descending into nested constructs
            _ if OPENERS.contains(&tk.class()) => {
                debug!("found opener `{:?}`, descending...", tk.class());
                let closer = &EXPECT[&tk.class()];

                // Clone tokens for nested parsing
                let nested_tokens = tokens.clone();
                let (nested, _) = extract_until(nested_tokens, closer);

                // Consume the tokens used by the nested construct
                for _ in 0..nested.len() {
                    tokens.pop_front();
                }

                // Add the opener and nested tokens to the product
                product.push_back(tk); // Add the opener
                product.extend(nested);

                // Ensure the closer is consumed
                if let Some(tk) = tokens.front() {
                    if closer.contains(&tk.class()) {
                        product.push_back(tokens.pop_front().unwrap());
                    } else {
                        warn!(
                            "Expected closer `{:?}`, but found `{:?}`",
                            closer, tk.class()
                        );
                        break;
                    }
                } else {
                    break;
                }
            }

            // Handle stop classes
            _ if stop_classes.contains(&tk.class()) => {
                debug!("found stop class `{:?}`, heading upward", tk.class());
                return (product, tokens);
            }

            // Default case: consume the token
            _ => {
                debug!(
                    "extract_until: token {:?} did not match any conditions, continuing",
                    tk.text()
                );
                product.push_back(tokens.pop_front().unwrap());
            }
        }
    }

    // Return the result or handle unmatched tokens
    if tokens.is_empty() && !stop_classes.is_empty() {
        warn!("Unmatched construct: expected one of {:?}", stop_classes);
    }

    (product, tokens)
}

fn extract_until_text(
    mut tokens: VecDeque<Tk>,
    stop_pattern: &str,
) -> (VecDeque<Tk>, VecDeque<Tk>) {
    let mut product = VecDeque::new();
    while let Some(tk) = tokens.pop_front() {
        if tk.text() == stop_pattern {
            break
        } else {
            product.push_back(tk);
        }
    }
    if tokens.is_empty() {
        let tokens = std::mem::take(&mut product);
        return (VecDeque::new(),tokens)
    }
    (product, tokens)
}

fn compute_span(tokens: VecDeque<Tk>) -> (usize, usize) {
    if tokens.is_empty() {
        (0, 0) // Default span for empty tokens
    } else {
        (tokens.front().unwrap().span().0, tokens.back().unwrap().span().1)
    }
}

pub fn build_if(mut tokens: VecDeque<Tk>) -> Result<(Node, VecDeque<Tk>), RshErr> {
    let mut cond_blocks = VecDeque::new();
    let mut else_block = None;
    tokens.pop_front(); // ignore 'if/elif'

    while !tokens.is_empty() {
        // Extract condition tokens until `Then`
        let (cond_tokens, remaining) = extract_until(tokens, &[TkType::Then]);
        debug!("cond_tokens: {:?}",get_tk_texts(&cond_tokens));
        debug!("remaining: {:?}",get_tk_texts(&remaining));
        tokens = remaining;
        if cond_tokens.is_empty() {
            return Err(RshErr::from_parse(
                "'Then' keyword not found in If statement".into(),
                tokens.front().unwrap().span(),
            ));
        }

        // Extract body tokens until `Fi`, `Else`, or `Elif`
        let (body_tokens, remaining) = extract_until(tokens, &[TkType::Fi, TkType::Else, TkType::Elif]);
        tokens = remaining;
        if body_tokens.is_empty() {
            return Err(RshErr::from_parse(
                "No valid body found for If statement".into(),
                tokens.front().unwrap().span(),
            ));
        }

        // Add condition and body to `cond_blocks`
        let condition = Box::new(get_tree(cond_tokens)?);
        let body = Box::new(get_tree(body_tokens)?);
        cond_blocks.push_back(Conditional { condition, body });

        // Handle `Else` or terminate on `Fi`
        if !tokens.is_empty() {
            match tokens.front().unwrap().class() {
                TkType::Else => {
                    let else_tk = tokens.pop_front().unwrap();
                    let (body_tokens, remaining) = extract_until(tokens, &[TkType::Fi]);
                    if body_tokens.is_empty() {
                        return Err(RshErr::from_parse(
                            "No valid body found for Else block".into(),
                            else_tk.span(),
                        ));
                    }
                    tokens = remaining;
                    else_block = Some(Box::new(get_tree(body_tokens)?));
                    break;
                }
                TkType::Fi => {
                    break;
                }
                _ => {} // Continue for `Elif`
            }
        }
    }

    let span = compute_span(tokens.clone());
    debug!("returning from build_if with tokens: {:?}",tokens);
    Ok((
        Node {
            nd_type: NdType::If { cond_blocks, else_block },
            span,
        },
        tokens,
    ))
}

pub fn build_for(mut tokens: VecDeque<Tk>) -> Result<(Node, VecDeque<Tk>), RshErr> {
    let span_start = tokens.front().unwrap().span().0;
    tokens.pop_front();
    let (loop_vars, remaining) = extract_until_text(tokens, "in");
    tokens = remaining;
    if loop_vars.is_empty() {
        return Err(RshErr::from_parse(
            "'in' keyword not found in For statement".into(),
                tokens.front().unwrap().span(),
        ));
    }

    let (mut loop_arr, remaining) = extract_until(tokens, &[TkType::Do]);
    tokens = remaining;
    if loop_arr.is_empty() {
        return Err(RshErr::from_parse(
            "'do' keyword not found in For statement".into(),
                tokens.front().unwrap().span(),
        ));
    }
    if loop_arr.back().is_some_and(|t| t.text() == ";") {
        loop_arr.pop_back();
    }

    let (loop_body, remaining) = extract_until(tokens, &[TkType::Done]);
    tokens = remaining;
    if loop_body.is_empty() {
        return Err(RshErr::from_parse(
            "'done' keyword not found in For statement".into(),
                tokens.front().unwrap().span(),
        ));
    }

    let loop_body = Box::new(get_tree(loop_body)?);

    let span_end = compute_span(tokens.clone()).1;
    let span = (span_start,span_end);
    Ok((
        Node {
            nd_type: NdType::For {
                loop_vars,
                loop_arr,
                loop_body,
            },
            span
        },
        tokens
    ))
}

pub fn build_loop(condition: bool, tokens: VecDeque<Tk>) -> Result<(Node, VecDeque<Tk>), RshErr> {
    let loop_condition = condition;

    // Extract condition tokens
    let (cond_tokens, mut remaining) = extract_until(tokens.clone(), &[TkType::Do]);
    debug!("returned to build_loop with cond_tokens {:?}", get_tk_texts(&cond_tokens));
    if cond_tokens.is_empty() {
        return Err(RshErr::from_parse(
            "'do' keyword not found in Loop statement".into(),
            tokens.front().unwrap().span(),
        ));
    }

    // Extract body tokens
    let (body_tokens, remaining) = extract_until(remaining, &[TkType::Done]);
    if body_tokens.is_empty() {
        return Err(RshErr::from_parse(
            "'done' keyword not found in Loop statement".into(),
            remaining.front().unwrap().span(),
        ));
    }

    // Parse condition and body
    let condition = Box::new(get_tree(cond_tokens)?);
    let body = Box::new(get_tree(body_tokens)?);
    let logic = Conditional { condition, body };

    let span = compute_span(tokens);
    Ok((
        Node {
            nd_type: NdType::Loop { condition: loop_condition, logic },
            span,
        },
        remaining,
    ))
}

pub fn build_case(mut tokens: VecDeque<Tk>) -> Result<(Node, VecDeque<Tk>), RshErr> {
    // TODO: rewrite this to not use so much mutation
    let span_start = tokens.front().unwrap().span().0;
    tokens.pop_front();
    debug!("build_case: tokens before getting var {:?}",get_tk_texts(&tokens));
    let (input_var, remaining) = extract_until_text(tokens, "in");
    debug!("build_case: got input_var {:?}",get_tk_texts(&input_var));
    debug!("build_case: remaining tokens {:?}",get_tk_texts(&remaining));
    tokens = remaining;
    if tokens.front().is_some_and(|t| t.class() == TkType::Cmdsep) {
        debug!("build_case: cutting separator");
        tokens.pop_front(); // cut separator
    }

    if input_var.len() != 1 {
        return Err(RshErr::from_parse(
            "Expected exactly one variable in case statement".to_string(),
            tokens.front().unwrap().span(),
        ));
    }

    info!("getting raw cases");
    let (raw_cases, remaining) = extract_until(tokens, &[TkType::Esac]);
    debug!("build_cases: got raw_cases {:?}",get_tk_texts(&raw_cases));
    tokens = remaining;
    let mut raw_cases = raw_cases;
    let mut cases = VecDeque::new();
    if tokens.front().is_some_and(|t| t.class() == TkType::Cmdsep) {
        tokens.pop_front(); // cut separator
    }

    while !raw_cases.is_empty() {
        let pattern = raw_cases.pop_front().unwrap();
        if !matches!(pattern.class(), TkType::String | TkType::Ident) {
            return Err(RshErr::from_parse(
                format!("Expected case pattern, got this: {:?}", pattern.class()),
                pattern.span(),
            ));
        }
        if matches!(pattern.class(), TkType::Esac) {
            break
        }
        let pattern = pattern.text().to_string();
        if raw_cases.front().is_some_and(|tk| tk.class() != TkType::CaseSep) {
            return Err(RshErr::from_parse(
                format!(
                    "Expected case separator, got this: {:?}",
                    raw_cases.front().unwrap().class()
                ),
                raw_cases.front().unwrap().span(),
            ));
        } else {
            raw_cases.pop_front();
        }
        let (block,remaining) = extract_until(raw_cases, &[TkType::CaseDelim]);
        raw_cases = remaining;
        if raw_cases.front().is_some_and(|tk| tk.class() == TkType::CaseDelim) {
            raw_cases.pop_front();
        }
        debug!("returned from case block extraction with tokens {:?}",get_tk_texts(&block));
        //while let Some(tk) = raw_cases.pop_front() {
            //debug!("case_delim - found: {:?}", tk.class());
//
            //match tk.class() {
                //TkType::CaseDelim => {
                    //// Found the case delimiter; break out of the loop
                    //break;
                //}
                //TkType::CaseSep => {
                    //// Found a case separator where it shouldn't be; return an error
                    //return Err(RshErr::from_parse(
                        //"expected ';;' after case block".into(),
                        //tk.span(),
                    //));
                //}
                //_ => {
                    //// Otherwise, add the token to the block
                    //block.push_back(tk);
                //}
            //}
        //}
        let logic = get_tree(block)?;
        let case = CaseBlock { pattern, logic };
        trace!("pushing case: {:?}", case);
        cases.push_back(case);
    }
    let span_end = compute_span(tokens.clone()).1;
    let span = (span_start,span_end);
    if cases.is_empty() {
        return Err(RshErr::from_parse("Found empty case block".into(), span))
    }
    Ok((
        Node {
            nd_type: NdType::Case { input_var: input_var.front().unwrap().clone(), cases },
            span
        },
        tokens
    ))
}

pub fn build_select(mut tokens: VecDeque<Tk>) -> Result<(Node, VecDeque<Tk>), RshErr> {
    // TODO: figure out a way to get 'in' to actually be a keyword
    // Fix the logic in general so this code doesn't have to use awkward work arounds
    let span_start = tokens.front().unwrap().span().0;
    tokens.pop_front();
    let (mut select_var, remaining) = extract_until_text(tokens, "in");
    tokens = remaining;
    if select_var.len() != 1 {
        return Err(RshErr::from_parse(
            "Expected exactly one variable in select statement".to_string(),
            tokens.front().unwrap().span()
        ));
    }
    let select_var = select_var.pop_front().unwrap();

    let (mut opts, remaining) = extract_until(tokens, &[TkType::Do]);
    tokens = remaining;
    if opts.back().is_some_and(|t| t.class() == TkType::Cmdsep) {
        opts.pop_back();
    }

    let (body, remaining) = extract_until(tokens, &[TkType::Done]);
    tokens = remaining;

    let body = Box::new(get_tree(body)?);

    let span_end = compute_span(tokens.clone()).1;
    let span = (span_start,span_end);
    Ok((
        Node {
            nd_type: NdType::Select {
                select_var,
                opts: opts.into(),
                body,
            },
            span
        },
        tokens
    ))
}

pub fn build_subshell(token: Tk) -> Result<Node, RshErr> {
    let span = token.span();
    Ok(
        Node {
            nd_type: NdType::Subshell { body: token.text().into() },
            span
        }
    )
}

pub fn build_func_def(tokens: VecDeque<Tk>) -> Result<(Node, VecDeque<Tk>), RshErr> {
    todo!("Implement build_func_def")
}

pub fn build_assignment(token: Tk) -> Result<Node, RshErr> {
    let (var, val) = token.text().split_once('=').unwrap();
    let span = token.span();
    Ok(
        Node {
            nd_type: NdType::Assignment {
                name: var.to_string(),
                value: Some(val.to_string()),
            },
            span
        }
    )
}

pub fn build_brace_group(tokens: VecDeque<Tk>) -> Result<(Node, VecDeque<Tk>), RshErr> {
    todo!("Implement build_brace_group")
}

pub fn build_pipeline<'a>(
    l_buffer: &'a [Tk],
    r_buffer: &'a [Tk],
) -> Result<(Node, &'a [Tk]), RshErr> {
    todo!()
}

pub fn build_chain<'a>(
    l_buffer: &'a [Tk],
    r_buffer: &'a [Tk],
    op: TkType,
) -> Result<(Node, &'a [Tk]), RshErr> {
    todo!()
}

pub fn build_command(mut tokens: VecDeque<Tk>) -> Result<(Node, VecDeque<Tk>), RshErr> {
    let mut argv = VecDeque::new(); // Temporary mutable Vec to collect arguments
    let mut redirs = VecDeque::new(); // Temporary mutable Vec to collect redirections

    // Compute the start span from the first token
    let start_span = tokens.front().map_or(0, |tk| tk.span().0);

    while let Some(tk) = tokens.pop_front() {

        match tk.class() {
            TkType::Cmdsep | TkType::LogicAnd | TkType::LogicOr | TkType::Pipe => {
                debug!("build_command breaking on: {:?}", tk);
                break;
            }
            TkType::Ident | TkType::String => {
                // Add to argv
                argv.push_back(tk.clone());
            }
            TkType::Redirection { ref redir } => {
                // Handle redirection
                let mut new_redir = redir.clone();
                if redir.fd_target.is_none() {
                    if let Some(file_target) = tokens.get(1) { // Look ahead for file target
                        match file_target.class() {
                            TkType::String | TkType::Ident => {
                                new_redir.file_target = Some(Box::new(file_target.clone()));
                                redirs.push_back(Tk {
                                    tk_type: TkType::Redirection { redir: new_redir },
                                    wd: tk.wd.clone(),
                                });
                            }
                            _ => {
                                return Err(RshErr::from_parse(
                                    "Expected identifier after redirection operator".to_string(),
                                    file_target.span(),
                                ));
                            }
                        }
                    } else {
                        return Err(RshErr::from_parse(
                            "Unexpected end of tokens after redirection operator".to_string(),
                            tk.span(),
                        ));
                    }
                } else {
                    redirs.push_back(tk.clone());
                }
            }
            TkType::EOI | TkType::SOI => {}
            _ => {
                error!("ran into EOI while building command, with these tokens: {:?}",get_tk_texts(&argv));
                return Err(RshErr::from_parse(
                    format!("Unexpected token: {:?}", tk),
                    tk.span(),
                ));
            }
        }
    }

    // Compute the end span from the last processed token
    let end_span = if let Some(last_arg) = argv.back() {
        last_arg.span().1
    } else if let Some(last_redir) = redirs.back() {
        last_redir.span().1
    } else {
        start_span // Default to the start span if no tokens were processed
    };

    debug!("returning from build_command with tokens: {:?}", tokens);

    Ok((
        Node {
            nd_type: NdType::Command { argv, redirs },
            span: (start_span, end_span),
        },
        tokens,
    ))
}
