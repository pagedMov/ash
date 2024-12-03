use crate::rsh::parse::ast::{self, ASTNode, ChainLogic, CondPath};
use crate::event::ShellEvent;

use pest::{iterators::{Pairs, Pair}, Parser};
use std::collections::VecDeque;
use tokio::sync::mpsc::{self,Sender,Receiver,channel};
use pest_derive::Parser;
use log::{debug,info};

#[derive(Parser)]
#[grammar = "rsh/rsh.pest"]
pub struct RshParser;

pub async fn interpret(input: String,outbox: Sender<ShellEvent>) {
    if let Ok(pairs) = RshParser::parse(Rule::main, &input) {
        // Seed the stack with the initial pairs
        for pair in pairs {
            let _ = outbox.send(ShellEvent::NewASTNode(pair_director(pair))).await;
        }
    } else { panic!("AAAAHHHHHHHH"); }
    let _ = outbox.send(ShellEvent::Prompt).await;
}

pub fn pair_director(pair: Pair<'_, Rule>) -> ASTNode {
    debug!("Checking rule: {:?}",pair.as_rule());
    match pair.as_rule() {
        Rule::r#loop => build_loop(pair),
        Rule::conditional => build_conditional(pair),
        Rule::pipeline => build_pipeline(pair),
        Rule::chain => build_chain(pair),
        Rule::cmd => build_command(pair),
        Rule::EOI => ASTNode::Null,
        _ => unimplemented!("No implementation for {:?} yet",pair.as_rule())
    }
}

		//| case_in
		//| substitution
		//| subshell
		//| test
		//| range
    //| pipeline
    //| assignment
		//| function

pub fn build_loop(pair: Pair<'_, Rule>) -> ASTNode {
    let block = pair.into_inner().next().unwrap();
    match block.as_rule() {
        Rule::while_block => build_while_or_until(block, true),
        Rule::until_block => build_while_or_until(block, false),
        Rule::for_block => build_for(block),
        _ => panic!("Unexpected loop type: {:?}", block.as_rule()),
    }
}

fn build_while_or_until(pair: Pair<'_, Rule>, is_while: bool) -> ASTNode {
    let parts = pair.into_inner();
    let mut condition: Box<ASTNode> = Box::new(ASTNode::Null);
    let mut body: Vec<ASTNode> = vec![];

    for part in parts {
        match part.as_rule() {
            Rule::r#while | Rule::until | Rule::r#do | Rule::done => continue,
            Rule::condition => {
                condition = Box::new(pair_director(part.into_inner().next().unwrap()));
            }
            Rule::body => {
                for body_part in part.into_inner() {
                    body.push(pair_director(body_part));
                }
            }
            _ => panic!("Unexpected part in loop: {:?}", part.as_rule()),
        }
    }

    if is_while {
        ASTNode::WhileLoop { condition, body }
    } else {
        ASTNode::UntilLoop { condition, body }
    }
}

fn build_for(pair: Pair<'_, Rule>) -> ASTNode {
    let mut parts = pair.into_inner();
    let loop_var = parts
        .find(|p| p.as_rule() == Rule::loop_var)
        .expect("Expected loop_var in for loop")
        .as_str()
        .to_string();

    // TODO: please god find a better way
    let loop_array = parts
        .find(|p| p.as_rule() == Rule::loop_array)
        .expect("Expected loop_array in for loop")
        .into_inner()
        .next()
        .unwrap()
        .as_str()
        .to_string();

    let mut body: Vec<ASTNode> = vec![];
    if let Some(body_part) = parts.find(|p| p.as_rule() == Rule::body) {
        for part in body_part.into_inner() {
            body.push(pair_director(part));
        }
    }

    ASTNode::ForLoop {
        loop_var,
        loop_array,
        body,
    }
}

pub fn build_conditional(pair: Pair<'_,Rule>) -> ASTNode {
    debug!("building conditional from: {:?}",pair);
    let parts = pair.into_inner();
    // TODO: Fix this extremely unfortunate initialization mess
    let mut main_block: Box<CondPath> = Box::new(CondPath::Null);
    let mut elif_blocks: Vec<CondPath> = vec![];
    let mut else_block: Option<Box<CondPath>> = None;
    for part in parts {
        match part.as_rule() {
            Rule::if_block => {
                main_block = build_conditional_path(part.into_inner());
            }
            Rule::elif_block => {
                elif_blocks.push(*build_conditional_path(part.into_inner()));
            }
            Rule::else_block => {
                else_block = Some(build_conditional_path(part.into_inner()));
            }
            Rule::fi => continue,
            _ => panic!("Unexpected part in loop: {:?}",part.as_rule()),
        }
    }
    ASTNode::Conditional { main_block, elif_blocks, else_block }
}

pub fn build_conditional_path(mut parts: Pairs<'_,Rule>) -> Box<CondPath> {
    debug!("Building cond path from: {:?}",parts);
    let mut condition: ASTNode = ASTNode::Null;
    // TODO: handle unwrap
    let kind = parts.next().unwrap();
    let mut body: Vec<ASTNode> = vec![];
    for part in parts {
        match part.as_rule() {
            Rule::then => continue,
            Rule::condition => {
                condition = pair_director(part.into_inner().next().unwrap());
            }
            Rule::body => {
                let body_parts = part.into_inner();
                for body_part in body_parts {
                    body.push(pair_director(body_part));
                }
            }
            _ => panic!("Unexpected rule: {:?}",part.as_rule()),
        }
    }
    match kind.as_rule() {
        Rule::r#if => {
            Box::new(CondPath::If { condition, body })
        }
        Rule::elif => {
            Box::new(CondPath::Elif { condition, body })
        }
        Rule::r#else => {
            Box::new(CondPath::Else { body })
        }
        _ => panic!("¯\\_(ツ)_/¯"),
    }
}

pub fn build_pipeline(pair: Pair<'_, Rule>) -> ASTNode {
    let mut parts = pair.into_inner(); // Convert pair into inner pairs
    let mut pipeline = pair_director(parts.next().expect("Expected a command"));

    while let Some(pipe_or_cmd) = parts.next() {
        match pipe_or_cmd.as_rule() {
            Rule::pipe => {
                // Next element must be a command after the pipe
                if let Some(next_cmd) = parts.next() {
                    pipeline = ASTNode::Pipeline {
                        left: Box::new(pipeline),
                        right: Box::new(pair_director(next_cmd)),
                    };
                } else {
                    panic!("Expected a command after pipe, found none");
                }
            }
            _ => panic!("Unexpected rule in pipeline: {:?}", pipe_or_cmd.as_rule()),
        }
    }

    pipeline
}

pub fn build_chain(pair: Pair<'_,Rule>) -> ASTNode {
    let parts = pair.into_inner();
    let mut left: Box<ASTNode> = Box::new(ASTNode::Null);
    let mut operator: ChainLogic = ChainLogic::And; // Default
    let mut right: Box<ASTNode> = Box::new(ASTNode::Null);
    for part in parts {
        match part.as_rule() {
            Rule::left => {
                left = Box::new(pair_director(part.into_inner().next().unwrap()));
            }
            Rule::and => {
                operator = ChainLogic::And;
            }
            Rule::or => {
                operator = ChainLogic::Or;
            }
            Rule::right => {
                right = Box::new(pair_director(part.into_inner().next().unwrap()));
            }
            _ => panic!("¯\\_(ツ)_/¯"),
        }
    }
    ASTNode::Chain { left, right, operator }
}

pub fn build_command(pair: Pair<'_,Rule>) -> ASTNode {
    let parts = pair.into_inner();
    let mut name: String = String::new();
    let mut args: Vec<String> = vec![];
    for part in parts {
        match part.as_rule() {
            Rule::ident => {
                name = part.as_str().into();
            }
            Rule::arg => {
                args.push(part.as_str().into());
            }
            _ => panic!("¯\\_(ツ)_/¯"),
        }
    }
    ASTNode::Command { name, args }
}
