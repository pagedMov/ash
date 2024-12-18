use im::Vector;
use log::{error,debug,info,trace};

use crate::shellenv::ShellEnv;
use crate::interp::token::{tokenize, Tk, TkType};
use crate::interp::expand::expand;

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
pub enum Node {
    Root { deck: Vector<Node> },
    If { cond_blocks: Vector<Conditional>, else_block: Option<Box<Node>> },
    For { loop_vars: Vector<Tk>, loop_arr: Vector<Tk>, loop_body: Box<Node> },
    Loop { condition: bool, logic: Conditional },
    Case { input_var: Tk, cases: Vector<CaseBlock> },
    Select { select_var: Tk, opts: Vector<Tk>, body: Box<Node> },
    Pipeline { left: Box<Node>, right: Box<Node> },
    Chain { left: Box<Node>, right: Box<Node>, op: Box<Node> },
    BraceGroup { body: Box<Node> },
    Subshell { body: String }, // It's a string because we're going to parse it in a subshell later
    FuncDef { name: String, body: Box<Node> },
    Assignment {name: String, value: Option<String> },
    Command { argv: Vector<Tk>, redirs: Vector<Tk> },
    And,
    Or,
    Pipe,
    Cmdsep
}

#[derive(Debug, Clone)]
pub enum NdType {
    Root,
    If,
    For,
    Loop,
    Case,
    Select,
    Pipeline,
    Chain,
    Redir,
    BraceGroup,
    Subshell,
    FuncDef,
    Assignment,
    Command,
}

#[derive(Debug,Clone)]
pub struct ParseState<'a> {
    pub input: &'a str,
    pub shellenv: &'a ShellEnv,
    pub tokens: Vector<Tk>,
    pub ast: Node
}

pub fn descend<'a>(input: &'a str, shellenv: &'a ShellEnv) -> ParseState<'a> {
    info!("Starting descent into parsing with input: {:?}", input);
    let state = ParseState {
        input,
        shellenv,
        tokens: Vector::new(),
        ast: Node::Root { deck: Vector::new() },
    };

    let state = tokenize(state);

    let state = expand(state);

    let state = parse(state);

    state
}

pub fn parse(state: ParseState) -> ParseState {
    info!("Parsing tokens into AST...");
    let tokens = state.tokens.clone();
    debug!("Tokens before parsing: {:?}", tokens);

    let tokens = tokens.into_iter().collect::<Vec<Tk>>();
    trace!("Tokens collected into Vec: {:?}", tokens);

    let ast = get_tree(tokens.as_slice());
    debug!("Generated AST: {:#?}", ast);

    ParseState {
        input: state.input,
        shellenv: state.shellenv,
        tokens: state.tokens,
        ast
    }
}

pub fn get_tree(tokens: &[Tk]) -> Node {
    info!("Building AST from tokens...");
    let nodes = parse_linear(tokens);
    debug!("Linear nodes parsed: {:?}", nodes);

    let deck = join_at_operators(&nodes);
    debug!("AST deck after joining operators: {:?}", deck);

    Node::Root { deck }
}

pub fn parse_linear(tokens: &[Tk]) -> Vec<Node> {
    // First pass just makes nodes without joining at operators
    info!("Starting linear parsing of tokens...");
    let mut nodes = Vec::new();

    let mut tokens = tokens;
    while !tokens.is_empty() {
        info!("Current tokens: {:?}",tokens);
        use crate::interp::token::TkType::*;
        match tokens[0].class() {
            If => {
                info!("Found 'if' token, processing...");
                let (node, remaining_tokens) = build_if(tokens);
                nodes.push(node);
                tokens = remaining_tokens;
            }
            While => {
                info!("Found 'while' token, processing...");
                let (node, remaining_tokens) = build_loop(true, tokens);
                nodes.push(node);
                tokens = remaining_tokens;
            }
            Until => {
                info!("Found 'until' token, processing...");
                let (node, remaining_tokens) = build_loop(false, tokens);
                nodes.push(node);
                tokens = remaining_tokens;
            }
            For => {
                info!("Found 'for' token, processing...");
                let (node, remaining_tokens) = build_for(tokens);
                nodes.push(node);
                tokens = remaining_tokens;
            }
            Case => {
                info!("Found 'case' token, processing...");
                let (node, remaining_tokens) = build_case(tokens);
                nodes.push(node);
                tokens = remaining_tokens;
            }
            Select => {
                info!("Found 'select' token, processing...");
                let (node, remaining_tokens) = build_select(tokens);
                nodes.push(node);
                tokens = remaining_tokens;
            }
            Ident | String => {
                info!("Found command or string token, processing...");
                let (command_node, remaining_tokens) = build_command(tokens);
                nodes.push(command_node);
                tokens = remaining_tokens;
            }
            Cmdsep | LogicAnd | LogicOr | Pipe => {
                info!("Found operator token: {:?}, preserving as node", tokens[0].class());
                match tokens[0].class() {
                    Cmdsep => nodes.push(Node::Cmdsep),
                    LogicAnd => nodes.push(Node::And),
                    LogicOr => nodes.push(Node::Or),
                    Pipe => nodes.push(Node::Pipe),
                    _ => unreachable!()
                }
                tokens = &tokens[1..];
            }
            SOI => {
                trace!("Skipping Start of Input token");
                tokens = &tokens[1..];
            }
            EOI => {
                info!("End of Input token encountered, stopping parsing");
                break;
            }
            Elif | Do | Then | Else | Done | Fi => {
                tokens = &tokens[1..];
            }
            CaseSep | CaseDelim => {
                panic!("Found this outside of a case statement: {:?}",tokens[0]);
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

    debug!("Completed linear parsing, nodes: {:?}", nodes);
    nodes
}

pub fn join_at_operators(deck: &[Node]) -> Vector<Node> {
    // Second pass will collect nodes at operators and construct pipelines and chains
    let mut buffer: Vector<Node> = Vector::new();
    let mut deck = Vector::from(deck);
    let mut left: Option<Node> = None;

    debug!("Starting join_at_operators with deck: {:?}", deck);

    while let Some(node) = deck.pop_front() {
        trace!("Processing node: {:?}", node);
        match node {
            Node::And | Node::Or => {
                debug!("Found logical operator: {:?}", node);
                if let Some(ref l_node) = left {
                    if let Some(r_node) = deck.pop_front() {
                        trace!("Creating Chain node with left: {:?}, operator: {:?}, right: {:?}", l_node, node, r_node);
                        let chain = Node::Chain {
                            left: Box::new(l_node.clone()),
                            right: Box::new(r_node),
                            op: Box::new(node),
                        };
                        left = Some(chain);
                    } else {
                        error!("Operator {:?} is missing a right operand", node);
                        panic!("Operator {:?} is missing a right operand", node);
                    }
                } else {
                    error!("Operator {:?} is missing a left operand", node);
                    panic!("Operator {:?} is missing a left operand", node);
                }
            }
            Node::Pipe => {
                debug!("Found Pipe operator");
                if let Some(ref l_node) = left {
                    if let Some(r_node) = deck.pop_front() {
                        trace!("Creating Pipeline node with left: {:?}, right: {:?}", l_node, r_node);
                        let pipeline = Node::Pipeline {
                            left: Box::new(l_node.clone()),
                            right: Box::new(r_node),
                        };
                        left = Some(pipeline);
                    } else {
                        error!("Pipe operator is missing a right operand");
                        panic!("Pipe operator is missing a right operand");
                    }
                } else {
                    error!("Pipe operator is missing a left operand");
                    panic!("Pipe operator is missing a left operand");
                }
            }
            Node::Cmdsep => {
                debug!("Found Cmdsep");
                if let Some(node) = left.take() {
                    trace!("Pushing node to buffer: {:?}", node);
                    buffer.push_back(node);
                }
            }
            _ => {
                debug!("Found general node: {:?}", node);
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

    debug!("Finished join_at_operators. Resulting buffer: {:?}", buffer);

    buffer
}

pub fn build_if(tokens: &[Tk]) -> (Node, &[Tk]) {
    let mut cond_blocks = Vector::new();
    let mut else_block = None;
    let mut index = 1; // Skip `if`

    while index < tokens.len() {
        // Extract condition tokens until `Then`
        let (cond_tokens, next_index) = extract_until(tokens, index, &[TkType::Then]);
        index = next_index + 1; // Skip `Then`

        // Extract body tokens until `Fi`, `Else`, or `Elif`
        let (body_tokens, next_index) = extract_until(tokens, index, &[TkType::Fi, TkType::Else, TkType::Elif]);
        index = next_index;

        // Add condition and body to `cond_blocks`
        let condition = Box::new(get_tree(cond_tokens));
        let body = Box::new(get_tree(body_tokens));
        cond_blocks.push_back(Conditional { condition, body });

        // Handle `Else` or terminate on `Fi`
        match tokens.get(index).map(|tk| tk.class()) {
            Some(TkType::Else) => {
                index += 1;
                let (body_tokens, next_index) = extract_until(tokens, index, &[TkType::Fi]);
                index = next_index + 1;
                else_block = Some(Box::new(get_tree(body_tokens)));
                break;
            }
            Some(TkType::Fi) => {
                index += 1;
                break;
            }
            _ => {} // Continue for `Elif`
        }
    }

    (
        Node::If { cond_blocks, else_block },
        &tokens[index..],
    )
}

fn extract_until<'a>(tokens: &'a [Tk], start: usize, stop_classes: &'a [TkType]) -> (&'a [Tk], usize) {
    let mut index = start;
    while index < tokens.len() && !stop_classes.contains(&tokens[index].class()) {
        index += 1;
    }
    (&tokens[start..index], index)
}
fn extract_until_text<'a>(tokens: &'a [Tk], start: usize, stop_pattern: &'a str) -> (&'a [Tk], usize) {
    let mut index = start;
    while index < tokens.len() && tokens[index].text() != stop_pattern {
        index += 1;
    }
    (&tokens[start..index], index)
}

pub fn build_for(tokens: &[Tk]) -> (Node,&[Tk]) {
    let mut index = 1;

    // TODO: figure out a way to get 'in' to actually be a keyword
    // Fix the logic in general so this code doesn't have to use awkward work arounds
    let (loop_vars,next_index) = extract_until_text(tokens, index, "in");
    index = next_index + 1;

    let (mut loop_arr,next_index) = extract_until(tokens, index, &[TkType::Do]);
    if loop_arr[loop_arr.len() - 1].text() == ";" {
        loop_arr = &loop_arr[..loop_arr.len() - 1]; // Cut the semicolon
    }
    index = next_index + 1;

    let (loop_body,next_index) = extract_until(tokens, index, &[TkType::Done]);
    index = next_index + 1;

    let loop_body = Box::new(get_tree(loop_body));

    (
        Node::For { loop_vars: loop_vars.into(), loop_arr: loop_arr.into(), loop_body },
        &tokens[index..]
    )
}

pub fn build_loop(condition: bool, tokens: &[Tk]) -> (Node, &[Tk]) {
    let loop_condition = condition;

    // Parse condition tokens until 'do'
    let mut index = 1; // Skip the loop keyword (e.g., 'while', 'until')
    let (cond_tokens,next_index) = extract_until(tokens, index, &[TkType::Do]);
    index = next_index + 1;

    let (body_tokens,next_index) = extract_until(tokens, index, &[TkType::Done]);
    index = next_index + 1;

    let condition = Box::new(get_tree(cond_tokens));
    let body = Box::new(get_tree(body_tokens));
    let logic = Conditional { condition, body };

    (
        Node::Loop { condition: loop_condition, logic },
        &tokens[index..]
    )
}

pub fn build_case(tokens: &[Tk]) -> (Node,&[Tk]) {
    // TODO: rewrite this to not use so much mutation
    let mut index = 1;
    let (input_var,next_index) = extract_until_text(tokens, index, "in");
    index = next_index + 1;
    if tokens[index].class() == TkType::Cmdsep {
        index += 1;
    }

    if input_var.len() != 1 {
        panic!("expected exactly one variable in case statement")
    }

    let (raw_cases,next_index) = extract_until(tokens,index,&[TkType::Esac]);
    let mut raw_cases = Vector::from(raw_cases);
    let mut cases = Vector::new();
    index = next_index + 1;
    if raw_cases[0].class() == TkType::Cmdsep {
        index += 1;
    }

    while !raw_cases.is_empty() {
        let pattern = raw_cases.pop_front().unwrap();
        if !matches!(pattern.class(),TkType::String | TkType::Ident) {
            panic!("Expected case pattern, got this: {:?} from {:?}",pattern.class(),raw_cases)
        }
        let pattern = pattern.text().to_string();
        if raw_cases.front().is_some_and(|tk| tk.class() != TkType::CaseSep) {
            panic!("Expected case separator, got this: {:?}",raw_cases.front().unwrap().class())
        } else {
            raw_cases.pop_front();
        }
        let mut block = Vec::new();
        while let Some(tk) = raw_cases.pop_front() {
            if tk.class() != TkType::CaseDelim {
                block.push(tk);
            } else { break }
        }
        let logic = get_tree(&block);
        let case = CaseBlock { pattern, logic };
        trace!("pushing case: {:?}",case);
        cases.push_back(case);
    }
    (Node::Case { input_var: input_var[0].clone(), cases }, &tokens[index..])
}

pub fn build_select(tokens: &[Tk]) -> (Node,&[Tk]) {
    let mut index = 1;

    // TODO: figure out a way to get 'in' to actually be a keyword
    // Fix the logic in general so this code doesn't have to use awkward work arounds
    let (select_var,next_index) = extract_until_text(tokens, index, "in");
    index = next_index + 1;
    if select_var.len() != 1 {
        panic!("expected exactly one variable in select statement")
    }
    let select_var = select_var[0].clone();

    let (mut opts,next_index) = extract_until(tokens, index, &[TkType::Do]);
    if opts[opts.len() - 1].class() == TkType::Cmdsep {
        opts = &opts[..opts.len() - 1]; // Cut the separator
    }
    index = next_index + 1;

    let (body,next_index) = extract_until(tokens, index, &[TkType::Done]);
    index = next_index + 1;

    let body = Box::new(get_tree(body));

    (
        Node::Select { select_var, opts: opts.into(), body },
        &tokens[index..]
    )

}

pub fn build_subshell(token: Tk) -> Node {
    Node::Subshell { body: token.text().into() }
}

pub fn build_func_def(tokens: &[Tk]) -> (Node,&[Tk]) {
    todo!("Implement build_func_def")
}

pub fn build_assignment(token: Tk) -> Node {
    let (var,val) = token.text().split_once('=').unwrap();
    Node::Assignment { name: var.to_string(), value: Some(val.to_string()) }
}

pub fn build_brace_group(tokens: &[Tk]) -> (Node,&[Tk]) {
    todo!("Implement build_brace_group")
}

pub fn build_pipeline<'a>(l_buffer: &'a [Tk], r_buffer: &'a [Tk]) -> (Node,&'a [Tk]) {
    todo!()
}

pub fn build_chain<'a>(l_buffer: &'a [Tk], r_buffer: &'a [Tk], op: TkType) -> (Node,&'a [Tk]) {
    todo!()
}

pub fn build_command(mut tokens: &[Tk]) -> (Node,&[Tk]) {
    let mut argv = Vector::new(); // Temporary mutable Vec to collect arguments
    let mut redirs = Vector::new(); // Temporary mutable Vec to collect redirections

    while !tokens.is_empty() {
        let tk = &tokens[0];

        match tk.class() {
            TkType::Cmdsep | TkType::LogicAnd | TkType::LogicOr | TkType::Pipe => {
                debug!("build_command breaking on: {:?}",tk);
                break
            }
            TkType::Ident | TkType::String => {
                // Add to argv
                argv.push_back(tk.clone());
            }
            TkType::Redirection { ref redir } => {
                // Handle redirection
                let mut new_redir = redir.clone();
                if redir.fd_target.is_none() {
                    if let Some(file_target) = tokens.first() {
                        match file_target.class() {
                            TkType::String | TkType::Ident => {
                                new_redir.file_target = Some(Box::new(file_target.clone()));
                                redirs.push_back(Tk {
                                    tk_type: TkType::Redirection { redir: new_redir },
                                    wd: tk.wd.clone(),
                                });
                                tokens = &tokens[1..]; // Advance the slice past file_target
                            }
                            _ => panic!("Expected identifier after redirection operator"),
                        }
                    } else {
                        panic!("Unexpected end of tokens after redirection operator");
                    }
                } else {
                    redirs.push_back(tk.clone());
                }
            }
            TkType::EOI | TkType::SOI => {},
            _ => {
                panic!("Unexpected token: {:?}", tk);
            }
        }
        tokens = &tokens[1..]; // Advance the slice
    }

    debug!("returning from build_command with tokens: {:?}",tokens);

    (Node::Command { argv, redirs, }, tokens)
}
