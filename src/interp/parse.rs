use std::collections::{HashMap, VecDeque};
use bitflags::bitflags;
use once_cell::sync::Lazy;
use std::mem::take;

use crate::event::ShError;
use crate::interp::token::{RedirType, RshTokenizer, Tk, TkType};
use crate::shellenv::read_logic;
use crate::{builtin, RshResult};

use super::helper;
use super::token::{Redir, TkizerCtx, WdFlags};

bitflags! {
	#[derive(Debug,Copy,Clone,PartialEq)]
	pub struct NdFlags: u32 {
		// General Contexts
		const VALID_OPERAND      = 0b00000000000000000000000000000001; // Can be a target for redirection, chains, or pipes
		const IS_OP              = 0b00000000000000000000000000000010; // Is an operator
		const COMBINE_OUT        = 0b00000000000000000000000000000100;
		const BACKGROUND         = 0b00000000000000000000000000001000;
		const IN_PIPE            = 0b00000000000000000000000000010000;
		const FUNCTION           = 0b00000000000000000000000000100000;
		const IN_CMD_SUB         = 0b00000000000000000000000001000000;
	}
}

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
	m
});

pub const OPENERS: [TkType;6] = [
	TkType::If,
	TkType::For,
	TkType::Until,
	TkType::While,
	TkType::Case,
	TkType::Select,
];

#[derive(PartialEq,Debug,Clone)]
enum Phase {
	Condition,
	Body,
	Vars,
	Array,
}

enum CmdType {
	Builtin,
	Subshell,
	Function,
	Command
}

#[derive(PartialEq,Clone,Copy,Debug,Eq,Hash)]
pub struct Span {
	pub start: usize,
	pub end: usize
}

impl Default for Span {
	fn default() -> Self {
		Span::new()
	}
}

impl Span {
	pub fn new() -> Self {
		Self { start: 0, end: 0 }
	}
	pub fn from(start: usize, end: usize) -> Self {
		Self { start, end }
	}

	pub fn set_start(&mut self, start: usize) {
		self.start = start;
	}

	pub fn set_end(&mut self, end: usize) {
		self.end = end;
	}
}

#[derive(Debug,Clone,PartialEq)]
pub struct Conditional {
	pub condition: Box<Node>,
	pub body: Box<Node>
}

#[derive(Debug,Clone,PartialEq)]
pub struct Node {
	pub command: Option<Tk>,
	pub nd_type: NdType,
	pub span: Span,
	pub flags: NdFlags,
	pub redirs: VecDeque<Node>
}

impl Node {
	pub fn new() -> Self {
		Self {
			command: None,
			nd_type: NdType::NullNode,
			span: Span::new(),
			flags: NdFlags::empty(),
			redirs: VecDeque::new()
		}
	}
	pub fn from(deck: VecDeque<Node>,span: Span) -> Self {
		Self {
			command: None,
			nd_type: NdType::Root { deck },
			span,
			flags: NdFlags::empty(),
			redirs: VecDeque::new()
		}
	}

	fn boxed(self) -> Box<Self> {
		Box::new(self)
	}
	fn with_flags(self,flags: NdFlags) -> Self {
		Self {
			command: self.command,
			nd_type: self.nd_type,
			span: self.span,
			flags,
			redirs: self.redirs
		}
	}
	pub fn span(&self) -> Span {
		self.span
	}
	pub fn node_type(&self) -> &NdType {
		&self.nd_type
	}
	pub fn set_span(&mut self,span: Span) {
		self.span = span
	}
	pub fn get_argv(&self) -> RshResult<Vec<Tk>> {
		let mut arg_vec = vec![];
		match &self.nd_type {
			NdType::Command { argv } | NdType::Builtin { argv } | NdType::Subshell { body: _, argv } => {
				for arg in argv {
					arg_vec.push(arg.clone());
				}
				Ok(arg_vec)
			}
			_ => Err(ShError::from_internal("Attempt to call `get_argv()` on a non-command node")),
		}
	}
	pub fn get_redirs(&self) -> RshResult<Vec<Node>> {
		if !self.flags.contains(NdFlags::VALID_OPERAND) {
			return Err(ShError::from_internal("Called get_redirs with an invalid operand"))
		}
		let mut redir_vec = vec![];
		for redir in &self.redirs {
			redir_vec.push(redir.clone());
		}
		Ok(redir_vec)
	}
}

impl Default for Node {
	fn default() -> Self {
		Self::new()
	}
}
#[derive(Debug,Clone,PartialEq)]
pub enum NdType {
	Root { deck: VecDeque<Node> },
	If { cond_blocks: VecDeque<Conditional>, else_block: Option<Box<Node>> },
	For { loop_vars: VecDeque<Tk>, loop_arr: VecDeque<Tk>, loop_body: Box<Node> },
	Loop { condition: bool, logic: Conditional },
	Case { input_var: Tk, cases: HashMap<String,Node> },
	Select { select_var: Tk, opts: VecDeque<Tk>, body: Box<Node> },
	PipelineBranch { left: Box<Node>, right: Box<Node>, both: bool }, // Intermediate value
	Pipeline { commands: VecDeque<Node>, both: bool }, // After being flattened
	Chain { left: Box<Node>, right: Box<Node>, op: Box<Node> },
	BraceGroup { body: Box<Node> },
	Subshell { body: String, argv: VecDeque<Tk> }, // It's a string because we're going to parse it in a subshell later
	FuncDef { name: String, body: Box<Node> },
	Assignment {name: String, value: Option<String> },
	Command { argv: VecDeque<Tk> },
	Builtin { argv: VecDeque<Tk> },
	Function { body: Box<Node>, argv: VecDeque<Tk> },
	Redirection { redir: Redir },
	And,
	Or,
	Pipe,
	PipeBoth,
	Cmdsep,
	NullNode
}

#[derive(Debug,PartialEq,Clone)]
pub struct ParseState {
	pub input: String,
	pub tokens: VecDeque<Tk>,
	pub ast: Node
}

#[derive(Debug,Clone)]
pub struct DescentContext {
	tokens: VecDeque<Tk>,
	root: VecDeque<Node>,
	start: usize,
	end: usize,
}

impl DescentContext {
	pub fn new(tokens: VecDeque<Tk>) -> Self {
		Self {
			tokens,
			root: VecDeque::new(),
			start: 0,
			end: 0
		}
	}

	pub fn mark_start(&self) -> usize {
		self.start
	}

	pub fn mark_end(&self) -> usize {
		self.end
	}

	pub fn next_tk(&mut self) -> Option<Tk> {
		let tk = self.tokens.pop_front();
		if let Some(ref tk) = tk {
			self.start = tk.span().start;
			self.end = tk.span().end
		}
		tk
	}

	pub fn last_tk(&mut self) -> Option<Tk> {
		self.tokens.pop_back()
	}

	pub fn front_tk(&mut self) -> Option<&Tk> {
		self.tokens.front()
	}

	pub fn back_tk(&mut self) -> Option<&Tk> {
		self.tokens.back()
	}

	pub fn next_node(&mut self) -> Option<Node> {
		self.root.pop_front()
	}

	pub fn last_node(&mut self) -> Option<Node> {
		self.root.pop_back()
	}

	pub fn front_node(&mut self) -> Option<&Node> {
		self.root.front()
	}

	pub fn back_node(&mut self) -> Option<&Node> {
		self.root.back()
	}

	pub fn attach_node(&mut self, node: Node) {
		self.root.push_back(node);
	}

	pub fn get_tk_texts(&self) -> Vec<String> {
		let mut texts = vec![];
		for tk in &self.tokens {
			texts.push(tk.text().into())
		}
		texts
	}
}

pub fn descend(tokenizer: &mut RshTokenizer) -> RshResult<Option<ParseState>> {
	let input = tokenizer.input();
	let mut state = ParseState {
		input: input.clone(),
		tokens: VecDeque::new(),
		ast: Node {
			command: None,
			nd_type: NdType::Root { deck: VecDeque::new() },
			span: Span::from(0,input.len()),
			flags: NdFlags::empty(),
			redirs: VecDeque::new()
		}
	};

	let deck = tokenizer.tokenize_one(TkizerCtx::new())?;
	state.tokens = deck.into();

	state = parse(state)?;

	Ok(Some(state))
}

/// The purpose of this function is mainly just to be an entry point for the parsing logic
/// It is the only part of this logic that has access to the full input context. ShError's are
/// propagated up here and then converted to a complete ShErrorFull using the context of
/// ParseState. This is done because propagating errors upwards is probably
/// cheaper (and definitely easier) than propagating the raw input text downwards.
pub fn parse(state: ParseState) -> RshResult<ParseState> {
	let ctx = DescentContext::new(state.tokens.clone());

	get_tree(ctx).map(|ast| {
		ParseState {
			input: state.input,
			tokens: state.tokens,
			ast
		}
	})
}

pub fn get_tree(ctx: DescentContext) -> RshResult<Node> {
	let span = compute_span(&ctx.tokens.clone());
	let ctx = parse_linear(ctx,false)?;
	let tree = Node {
		command: None,
		nd_type: NdType::Root { deck: ctx.root },
		span,
		flags: NdFlags::empty(),
		redirs: VecDeque::new()
	};
	let tree = propagate_redirections(tree)?;

	Ok(tree)
}

pub fn parse_linear(mut ctx: DescentContext, once: bool) -> RshResult<DescentContext> {
	// First pass just makes nodes without joining at operators
	while let Some(tk) = ctx.next_tk() {
		use crate::interp::token::TkType::*;
		match tk.class() {
			If => {
				ctx = build_if(ctx)?;
				if once {
					break
				} else {
					continue
				}
			}
			While => {
				ctx = build_loop(true,ctx)?;
				if once {
					break
				} else {
					continue
				}
			}
			Until => {
				ctx = build_loop(false,ctx)?;
				if once {
					break
				} else {
					continue
				}
			}
			For => {
				ctx = build_for(ctx)?;
				if once {
					break
				} else {
					continue
				}
			}
			Case => {
				ctx = build_case(ctx)?;
				if once {
					break
				} else {
					continue
				}
			}
			Select => {
				ctx = build_select(ctx)?;
				if once {
					break
				} else {
					continue
				}
			}
			Ident | String => {
				ctx.tokens.push_front(tk);
				ctx = build_command(ctx)?;
				// Fall through
			}
			Subshell => {
				ctx.tokens.push_front(tk);
				ctx = build_command(ctx)?;
			}
			FuncDef {..} => {
				ctx.tokens.push_front(tk);
				ctx = build_func_def(ctx)?;
			}
			Assignment => {
				ctx.tokens.push_front(tk);
				ctx = build_assignment(ctx)?;
			}
			SOI => {
				continue
			}
			EOI => {
				break;
			}
			Do | Done => {
				return Err(ShError::from_parse(format!("Found `{}` outside of loop context",tk.text()).as_str(), tk.span()))
			}
			Else | Elif | Then | Fi => {
				return Err(ShError::from_parse(format!("Found `{}` outside of `if` context",tk.text()).as_str(), tk.span()))
			}
			Esac => {
				return Err(ShError::from_parse("Found `esac` outside of `case` context", tk.span()))
			}
			Redirection { .. } => {
				ctx.tokens.push_front(tk);
				ctx = build_redirection(ctx)?;
			}
			Cmdsep => ctx.attach_node(
				Node {
					command: None,
					nd_type: NdType::Cmdsep,
					span: tk.span(),
					flags: NdFlags::empty(),
					redirs: VecDeque::new()
				}
			),
			LogicAnd => ctx.attach_node(
				Node {
					command: None,
					nd_type: NdType::And,
					span: tk.span(),
					flags: NdFlags::IS_OP,
					redirs: VecDeque::new()
				}
			),
			LogicOr => ctx.attach_node(
				Node {
					command: None,
					nd_type: NdType::Or,
					span: tk.span(),
					flags: NdFlags::IS_OP,
					redirs: VecDeque::new()
				}
			),
			Pipe => ctx.attach_node(
				Node {
					command: None,
					nd_type: NdType::Pipe,
					span: tk.span(),
					flags: NdFlags::IS_OP,
					redirs: VecDeque::new()
				}
			),
			PipeBoth => ctx.attach_node(
				Node {
					command: None,
					nd_type: NdType::PipeBoth,
					span: tk.span(),
					flags: NdFlags::IS_OP,
					redirs: VecDeque::new()
				}.with_flags(NdFlags::COMBINE_OUT)),
			_ => {
				unimplemented!(
					"Support for token type `{:?}` is not implemented yet",
					tk.class()
				);
			}
		}
	}

	ctx = join_at_operators(ctx)?;
	Ok(ctx)
}

pub fn check_valid_operand(node: &Node) -> bool {
	use crate::interp::parse::NdType::*;
	matches!(node.nd_type, PipelineBranch {..} | Pipeline {..} | Subshell {..} | Chain {..} | If {..} | For {..} | Loop {..} | Case {..} | Select {..} | Function {..} | Command {..} | Builtin {..})
}

pub fn join_at_operators(mut ctx: DescentContext) -> RshResult<DescentContext> {
	let mut buffer: VecDeque<Node> = VecDeque::new();

	// First pass: Redirection operators
	while let Some(node) = ctx.next_node() {
		match node.nd_type {
			NdType::Redirection { .. } => {
				if let Some(mut target_node) = buffer.pop_back() {
					target_node.redirs.push_back(node);
					buffer.push_back(target_node);
				} else {
					return Err(ShError::from_parse("Found this orphaned redirection operator", node.span()))
				}
			}
			_ => buffer.push_back(node),
		}
	}
	ctx.root.extend(buffer.drain(..));

	// Second pass: Pipeline operators
	while let Some(node) = ctx.next_node() {
		match node.nd_type {
			NdType::Pipe | NdType::PipeBoth => {
				let both = match node.nd_type {
					NdType::PipeBoth => true,
					NdType::Pipe => false,
					_ => unreachable!()
				};
				if let Some(mut left) = buffer.pop_back() {
					if let Some(mut right) = ctx.next_node() {
						if !check_valid_operand(&left) {
							return Err(ShError::from_parse("The left side of this pipeline is invalid", node.span))
						}
						if !check_valid_operand(&right) {
							dbg!(&right);
							return Err(ShError::from_parse("The right side of this pipeline is invalid", node.span))
						}
						left.flags |= NdFlags::IN_PIPE;
						right.flags |= NdFlags::IN_PIPE;
						let left = left.boxed();
						let right = right.boxed();
						let pipeline = Node {
							command: None,
							nd_type: NdType::PipelineBranch { left, right, both },
							span: Span::from(0,0),
							flags: NdFlags::empty(),
							redirs: VecDeque::new()
						};
						buffer.push_back(pipeline);
					} else {
						return Err(ShError::from_parse("This pipeline is missing a right operand", node.span))
					}
				} else {
					return Err(ShError::from_parse("This pipeline is missing a left operand", node.span))
				}
			}
			NdType::Cmdsep => {
				continue
			}
			_ => buffer.push_back(node)
		}
	}
	// Now we will flatten the pipelines from a tree structure into a straight line sequence
	take(&mut buffer).into_iter().for_each(|node| {
		let flags = node.flags;
		let redirs = node.redirs.clone();
		let span = node.span();
		if let NdType::PipelineBranch { left, right, both } = node.nd_type {
			let commands = helper::flatten_pipeline(*left, *right, VecDeque::new());
			let flattened_pipeline = Node {
				command: None,
				nd_type: NdType::Pipeline { commands, both },
				span,
				flags,
				redirs
			};
			buffer.push_back(flattened_pipeline);
		} else {
			buffer.push_back(node);
		}
	});
	ctx.root.extend(buffer.drain(..));

	// Third pass: Chain operators
	while let Some(node) = ctx.next_node() {
		match node.nd_type {
			NdType::And | NdType::Or => {
				if let Some(left) = buffer.pop_back() {
					if let Some(right) = ctx.next_node() {
						if !check_valid_operand(&left) {
							return Err(ShError::from_parse("The left side of this chain is invalid", node.span))
						}
						if !check_valid_operand(&right) {
							return Err(ShError::from_parse("The right side of this chain is invalid", node.span))
						}
						let left = left.boxed();
						let right = right.boxed();
						let op = node.boxed();
						let chain = Node {
							command: None,
							nd_type: NdType::Chain { left, right, op },
							span: Span::from(0,0),
							flags: NdFlags::empty(),
							redirs: VecDeque::new()
						};
						buffer.push_back(chain);
					} else {
						return Err(ShError::from_parse("This chain is missing a right operand", node.span))
					}
				} else {
					return Err(ShError::from_parse("This chain is missing a left operand", node.span))
				}
			}
			NdType::Cmdsep => {
				continue
			}
			_ => buffer.push_back(node)
		}
	}

	ctx.root.extend(buffer.drain(..));
	Ok(ctx)
}
pub fn propagate_redirections(mut node: Node) -> RshResult<Node> {
	// This function allows for redirections for higher order control flow structures
	// e.g. `while true; do echo hello world; done > file.txt`
	// The entire AST is rebuilt in-place, while carrying redirections out to the leaf nodes
	let mut nd_type = node.nd_type.clone();
	match nd_type {
		NdType::Root { ref mut deck } => {
			// Iterate through the deck and map all root node redirections to children
			let mut new_deck = VecDeque::new();
			while let Some(redir) = node.redirs.pop_back() {
				while let Some(mut deck_node) = deck.pop_front() {
					deck_node.redirs.push_front(redir.clone());
					new_deck.push_back(deck_node);
				}
				deck.extend(take(&mut new_deck));
			}
			while let Some(mut deck_node) = deck.pop_front() {
				deck_node = propagate_redirections(deck_node)?;
				new_deck.push_back(deck_node);
			}
			node = Node::from(new_deck, node.span)
		}
		NdType::If { cond_blocks, mut else_block } => {
			// Iterate through cond_blocks and map redirections accordingly
			// Input redirections go to cond, output redirections go to body
			let (cond_redirs,body_redirs) = get_flow_ctl_redirections(&node)?;
			let mut new_cond_blocks = VecDeque::new();
			for block in cond_blocks {
				let mut cond = *block.condition;
				let mut body = *block.body;

				for redir in &cond_redirs {
					cond.redirs.push_back(redir.clone());
				}
				let cond = Box::new(propagate_redirections(cond)?);

				for redir in &body_redirs {
					body.redirs.push_back(redir.clone());
				}
				let body = Box::new(propagate_redirections(body)?);
				new_cond_blocks.push_back(Conditional { condition: cond, body });
			}
			if let Some(mut else_body) = else_block {
				for redir in &body_redirs {
					else_body.redirs.push_back(redir.clone());
				}
				else_block = Some(Box::new(propagate_redirections(*else_body)?));
			}
			node = Node {
				command: None,
				nd_type: NdType::If { cond_blocks: new_cond_blocks, else_block },
				flags: node.flags,
				redirs: VecDeque::new(),
				span: node.span
			}
		}
		NdType::Loop { condition, logic } => {
			// Same as the logic for propagating in If blocks, just performed once
			let mut cond = logic.condition;
			let mut body = logic.body;
			let (cond_redirs,body_redirs) = get_flow_ctl_redirections(&node)?;

			for redir in &cond_redirs {
				cond.redirs.push_back(redir.clone());
			}
			cond = Box::new(propagate_redirections(*cond)?);

			for redir in &body_redirs {
				body.redirs.push_back(redir.clone());
			}
			body = Box::new(propagate_redirections(*body)?);
			let logic = Conditional { condition: cond, body };
			node = Node {
				command: None,
				nd_type: NdType::Loop { condition, logic },
				flags: node.flags,
				redirs: VecDeque::new(),
				span: node.span
			}
		}
		NdType::For { loop_vars, loop_arr, mut loop_body } => {
			// Simple, loop_body is just a Root node so we just need to map redirs to it
			// and then call propagate_redirections()
			for redir in &node.redirs {
				loop_body.redirs.push_back(redir.clone());
			}

			let loop_body = Box::new(propagate_redirections(*loop_body)?);
			node = Node {
				command: None,
				nd_type: NdType::For { loop_vars, loop_arr, loop_body },
				flags: node.flags,
				redirs: VecDeque::new(),
				span: node.span
			}
		}
		NdType::Case { input_var, mut cases } => {
			// This one gets a little bit messy
			// Iterate through keys and map redirections to each case body
			// And then iterate through the keys again and call propagate_redirections() on each
			let keys = cases.keys().cloned().collect::<Vec<String>>();
			let mut new_cases = HashMap::new();
			for redir in &node.redirs {
				for key in keys.iter() {
					cases.get_mut(key).unwrap().redirs.push_back(redir.clone())
				}
			}
			for key in keys.iter() {
				if let Some(mut case_node) = cases.remove(key) {
					case_node = propagate_redirections(case_node)?;
					new_cases.insert(key.clone(),case_node);
				}
			}
			let cases = new_cases;
			node = Node {
				command: None,
				nd_type: NdType::Case { input_var, cases },
				flags: node.flags,
				redirs: VecDeque::new(),
				span: node.span
			}

		}
		NdType::Select { select_var, opts, mut body } => {
			// Same as For node logic
			for redir in &node.redirs {
				body.redirs.push_back(redir.clone());
			}

			body = Box::new(propagate_redirections(*body)?);
			node = Node {
				command: None,
				nd_type: NdType::Select { select_var, opts, body },
				flags: node.flags,
				redirs: VecDeque::new(),
				span: node.span
			}
		}
		_ => {
			// Fall-through
			// This is for bottom-level nodes like commands and subshells
			// If we have reached one of these, propagation is complete
			// so we can just return the node now
		}
	}
	Ok(node)
}

fn get_flow_ctl_redirections(node: &Node) -> RshResult<(Vec<Node>, Vec<Node>)> {
	// Separates redirections into two baskets; one for conditions and one for bodies
	// Input redirections like `while read -r line; do echo $line; done < lines.txt` go to the condition
	// Output redirections like `while true; do echo hello world; done >> hello.txt` go to the body
	let redirs = node.get_redirs()?;
	let (cond_redirs, body_redirs): (Vec<Node>, Vec<Node>) = redirs.into_iter().partition(|redir_nd| {
		if let NdType::Redirection { ref redir } = redir_nd.nd_type {
			matches!(redir.op, RedirType::Input)
		} else {
			false
		}
	});
	Ok((cond_redirs,body_redirs))
}

fn compute_span(tokens: &VecDeque<Tk>) -> Span {
	if tokens.is_empty() {
		Span::from(0, 0) // Default span for empty tokens
	} else {
		Span::from(tokens.front().unwrap().span().start, tokens.back().unwrap().span().end)
	}
}

fn parse_and_attach(mut tokens: VecDeque<Tk>, mut root: VecDeque<Node>) -> RshResult<VecDeque<Node>> {
	let mut sub_ctx = DescentContext::new(take(&mut tokens));
	sub_ctx = parse_linear(sub_ctx,true)?;
	while let Some(node) = sub_ctx.root.pop_back() {
		root.push_front(node);
	}
	Ok(root)
}

fn get_conditional(cond_root: VecDeque<Node>, cond_span: Span, body_root: VecDeque<Node>, body_span: Span) -> Conditional {
	let condition = Node {
		command: None,
		nd_type: NdType::Root { deck: cond_root },
		span: cond_span,
		flags: NdFlags::empty(),
		redirs: VecDeque::new()
	}.boxed();
	let body = Node {
		command: None,
		nd_type: NdType::Root { deck: body_root },
		span: body_span,
		flags: NdFlags::empty(),
		redirs: VecDeque::new()
	}.boxed();
	Conditional { condition, body }
}

pub fn build_redirection(mut ctx: DescentContext) -> RshResult<DescentContext> {
	let redir_tk = ctx.next_tk()
		.ok_or_else(|| ShError::from_internal("Called build_redirection with an empty token queue"))?;

		let span = redir_tk.span();

		let mut redir = if let TkType::Redirection { redir } = redir_tk.class() {
			redir
		} else {
			return Err(ShError::from_internal(format!("Called build_redirection() with a non-redirection token: {:?}",redir_tk).as_str()))
		};

		if redir.fd_target.is_none() && redir.file_target.is_none() {
			let target = ctx.next_tk()
				.ok_or_else(|| ShError::from_parse("Did not find an output for this redirection operator", span))?;

				if !matches!(target.class(), TkType::Ident | TkType::String) {
					return Err(ShError::from_parse(format!("Expected identifier after redirection operator, found this: {}",target.text()).as_str(), span))
				}

				redir.file_target = Some(Box::new(target));
		}

		let node = Node {
			command: None,
			nd_type: NdType::Redirection { redir },
			span,
			flags: NdFlags::IS_OP,
			redirs: VecDeque::new()
		};
		ctx.attach_node(node);

		Ok(ctx)
}

pub fn build_if(mut ctx: DescentContext) -> RshResult<DescentContext> {
	let mut cond_tokens = VecDeque::new();
	let mut cond_root = VecDeque::new();
	let mut body_tokens = VecDeque::new();
	let mut body_root = VecDeque::new();

	let mut if_context = TkType::If;
	let mut logic_blocks = VecDeque::new();
	let mut else_block = None;
	let mut phase = Phase::Condition;
	let mut closed = false;

	let span_start = ctx.mark_start();

	while let Some(tk) = ctx.next_tk() {
		let err_span = ctx.mark_start();

		match tk.class() {
			_ if OPENERS.contains(&tk.class()) => {
				ctx.tokens.push_front(tk);
				match phase {
					Phase::Condition => {
						if !cond_tokens.is_empty() {
							cond_root = parse_and_attach(take(&mut cond_tokens), cond_root)?;
						}
						ctx = parse_linear(ctx, true)?;
						if let Some(node) = ctx.root.pop_back() {
							cond_root.push_back(node);
						}
					},
					Phase::Body => {
						if !body_tokens.is_empty() {
							body_root = parse_and_attach(take(&mut body_tokens), body_root)?;
						}
						ctx = parse_linear(ctx, true)?;
						if let Some(node) = ctx.root.pop_back() {
							body_root.push_back(node);
						}
					},
					_ => unreachable!()
				}
			}
			TkType::Elif if if_context != TkType::Else => {
				if_context = TkType::Elif;
				let cond_span = compute_span(&cond_tokens);
				cond_root = parse_and_attach(take(&mut cond_tokens), cond_root)?;

				let body_span = compute_span(&body_tokens);
				body_root = parse_and_attach(take(&mut body_tokens), body_root)?;

				let logic = get_conditional(take(&mut cond_root), cond_span, take(&mut body_root), body_span);
				logic_blocks.push_back(logic);
				phase = Phase::Condition;
			}
			TkType::Then => {
				if if_context == TkType::Then {
					return Err(ShError::from_parse(
							"Did not find a condition for this `then` block",
							Span::from(err_span,ctx.mark_end()))
					)
				}
				if if_context == TkType::Else {
					return Err(ShError::from_parse(
							"Else blocks do not get a `then` statement; give the body directly after the else keyword",
							Span::from(err_span,ctx.mark_end()))
					)
				}
				if_context = TkType::Then;
				phase = Phase::Body;
			}
			TkType::Else => {
				if if_context != TkType::Then {
					return Err(ShError::from_parse("Was expecting a `then` block, get an else block instead", Span::from(err_span,ctx.mark_end())))
				}
				if_context = TkType::Else;
				let cond_span = compute_span(&cond_tokens);
				cond_root = parse_and_attach(take(&mut cond_tokens), cond_root)?;

				let body_span = compute_span(&body_tokens);
				body_root = parse_and_attach(take(&mut body_tokens), body_root)?;

				let logic = get_conditional(take(&mut cond_root), cond_span, take(&mut body_root), body_span);
				logic_blocks.push_back(logic);
				phase = Phase::Body;
			}
			TkType::Fi => {
				closed = true;
				if !matches!(if_context,TkType::Then | TkType::Else) {
					return Err(ShError::from_parse("Was expecting a `then` block, get an else block instead", Span::from(err_span,ctx.mark_end())))
				}
				if if_context == TkType::Else {
					let else_ctx = DescentContext::new(take(&mut body_tokens));
					let else_node = get_tree(else_ctx)?.boxed();
					else_block = Some(else_node);
				}
				if !body_tokens.is_empty() && !cond_tokens.is_empty() {
					let cond_span = compute_span(&cond_tokens);
					cond_root = parse_and_attach(take(&mut cond_tokens), cond_root)?;

					let body_span = compute_span(&body_tokens);
					body_root = parse_and_attach(take(&mut body_tokens), body_root)?;

					let logic = get_conditional(take(&mut cond_root), cond_span, take(&mut body_root), body_span);
					logic_blocks.push_back(logic);
				}
				break;
			}
			_ if phase == Phase::Condition => {
				cond_tokens.push_back(tk);
			}
			_ if phase == Phase::Body => {
				body_tokens.push_back(tk);
			}
			_ => unreachable!("Unexpected token in build_if: {:?}", tk),
		}
	}

	let span_end = ctx.mark_end();
	let span = Span::from(span_start, span_end);

	if !closed {
		return Err(ShError::from_parse("This if statement didn't get an `fi`", span))
	}

	let node = Node {
		command: None,
		nd_type: NdType::If { cond_blocks: logic_blocks, else_block },
		span,
		flags: NdFlags::VALID_OPERAND,
		redirs: VecDeque::new()
	};
	ctx.attach_node(node);

	Ok(ctx)
}

pub fn build_for(mut ctx: DescentContext) -> RshResult<DescentContext> {
	let mut phase = Phase::Vars;

	let mut loop_vars: VecDeque<Tk> = VecDeque::new();
	let mut loop_arr: VecDeque<Tk> = VecDeque::new();
	let mut body_tokens: VecDeque<Tk> = VecDeque::new();
	let mut body_root: VecDeque<Node> = VecDeque::new();
	let span_start = ctx.mark_start();
	let mut body_start = 0;
	let mut closed = false;

	while let Some(tk) = ctx.next_tk() {
		match tk.class() {
			TkType::In => {
				if loop_vars.is_empty() {
					return Err(ShError::from_parse(
							"This for loop didn't get any loop variables",
							Span::from(span_start,ctx.mark_end()))
					)
				}
				phase = Phase::Array
			}
			TkType::Do => {
				if loop_arr.back().is_some_and(|tk| tk.class() == TkType::Cmdsep) {
					loop_arr.pop_back();
				}
				if loop_arr.is_empty() {
					return Err(ShError::from_parse(
							"This for loop got an empty array",
							Span::from(span_start,ctx.mark_end()))
					)
				}
				body_start = ctx.mark_start();
				phase = Phase::Body
			}
			TkType::Done => {
				if phase == Phase::Vars {
					return Err(ShError::from_parse(
							"This for loop has an unterminated variable definition",
							Span::from(span_start,ctx.mark_end()))
					)
				}
				if phase == Phase::Array {
					return Err(ShError::from_parse(
							"This for loop has an unterminated array definition",
							Span::from(span_start,ctx.mark_end()))
					)
				}
				closed = true;
				break;
			}
			_ => match phase {
				Phase::Vars => {
					loop_vars.push_back(tk);
				}
				Phase::Array => {
					loop_arr.push_back(tk);
				}
				Phase::Body => {
					match tk.class() {
						_ if OPENERS.contains(&tk.class()) => {
							ctx.tokens.push_front(tk);
							if !body_tokens.is_empty() {
								body_root = parse_and_attach(take(&mut body_tokens), body_root)?;
							}
							ctx = parse_linear(ctx, true)?;
							if let Some(node) = ctx.root.pop_back() {
								body_root.push_back(node);
							}
						},
						_ => body_tokens.push_back(tk),
					}
				}
				_ => unreachable!()
			}
		}
	}

	let span_end = ctx.mark_end();
	let span = Span::from(span_start,span_end);

	if !closed {
		return Err(ShError::from_parse(
				"This loop is missing a `done`.",
				span)
		)
	}

	if !body_tokens.is_empty() {
		body_root = parse_and_attach(take(&mut body_tokens), body_root)?;
	}
	let body_end = ctx.mark_end();
	let body_span = Span::from(body_start,body_end);
	let loop_body = Node::from(body_root,body_span).boxed();
	let node = Node {
		command: None,
		nd_type: NdType::For { loop_vars, loop_arr, loop_body },
		span,
		flags: NdFlags::VALID_OPERAND,
		redirs: VecDeque::new()
	};
	ctx.attach_node(node);
	Ok(ctx)
}

pub fn build_loop(condition: bool, mut ctx: DescentContext) -> RshResult<DescentContext> {
	let loop_condition = condition;

	let mut phase = Phase::Condition;
	let mut cond_tokens = VecDeque::new();
	let mut cond_root = VecDeque::new();
	let mut body_tokens = VecDeque::new();
	let mut body_root = VecDeque::new();
	let mut closed = false;
	let span_start = ctx.mark_start();

	while let Some(tk) = ctx.next_tk() {
		match tk.class() {
			TkType::Do => {
				if cond_tokens.is_empty() {
					return Err(ShError::from_parse("Did not find a condition for this loop", tk.span()))
				}
				phase = Phase::Body
			}
			TkType::Done => {
				if body_tokens.is_empty() {
					return Err(ShError::from_parse("Did not find a body for this loop", tk.span()))
				}
				closed = true;
				break
			}
			_ if OPENERS.contains(&tk.class()) => {
				ctx.tokens.push_front(tk);
				match phase {
					Phase::Condition => {
						if !cond_tokens.is_empty() {
							cond_root = parse_and_attach(take(&mut cond_tokens), cond_root)?;
						}
						ctx = parse_linear(ctx, true)?;
						if let Some(node) = ctx.root.pop_back() {
							cond_root.push_back(node);
						}
					},
					Phase::Body => {
						if !body_tokens.is_empty() {
							body_root = parse_and_attach(take(&mut body_tokens), body_root)?;
						}
						ctx = parse_linear(ctx, true)?;
						if let Some(node) = ctx.root.pop_back() {
							body_root.push_back(node);
						}
					},
					_ => unreachable!()
				}
			}
			_ if phase == Phase::Condition => {
				cond_tokens.push_back(tk);
			}
			_ if phase == Phase::Body => {
				body_tokens.push_back(tk);
			}
			_ => unreachable!()
		}
	}


	let span_end = ctx.mark_end();
	let span = Span::from(span_start,span_end);

	if !closed {
		return Err(ShError::from_parse(
				"This loop is missing a `done`",
				span)
		)
	}

	let mut cond_span = Span::from(0,0);
	let mut body_span = Span::from(0,0);
	if !cond_tokens.is_empty() && !body_tokens.is_empty() {
		cond_span = compute_span(&cond_tokens);
		body_span = compute_span(&body_tokens);
		cond_root = parse_and_attach(cond_tokens,cond_root)?;
		body_root = parse_and_attach(body_tokens,body_root)?;
	}
	let logic = get_conditional(cond_root, cond_span, body_root, body_span);

	let node = Node {
		command: None,
		nd_type: NdType::Loop { condition: loop_condition, logic },
		span,
		flags: NdFlags::VALID_OPERAND,
		redirs: VecDeque::new()
	};
	ctx.attach_node(node);
	Ok(ctx)
}

pub fn build_case(mut ctx: DescentContext) -> RshResult<DescentContext> {
	let mut cases = HashMap::new();
	let mut block_string = String::new();
	let mut block_tokens = VecDeque::new();
	let mut block_root = VecDeque::new();
	let mut input_var: Option<Tk> = None;
	let mut phase = Phase::Vars;
	let mut closed = false;
	if ctx.front_tk().is_some_and(|tk| tk.tk_type == TkType::Ident) {
		input_var = Some(ctx.next_tk().unwrap());
	}

	let span_start = ctx.mark_start();

	while let Some(tk) = ctx.next_tk() {
		match tk.class() {
			TkType::In => {
				if input_var.is_some() {
					if ctx.front_tk().is_some_and(|tk| tk.class() == TkType::Cmdsep) {
						ctx.next_tk();
					}
					phase = Phase::Condition;
				} else {
					return Err(ShError::from_parse(
							"Did not find a variable for this case statement",
							tk.span(),
					));
				}
			}
			TkType::Esac => {
				// Final block handling
				if !block_string.is_empty() {
					let block_span = compute_span(&block_tokens);
					block_root = parse_and_attach(take(&mut block_tokens), block_root)?;
					let block_node = Node::from(take(&mut block_root), block_span);
					cases.insert(block_string.clone(), block_node);
				}
				if cases.is_empty() {
					return Err(ShError::from_parse(
							"Did not find any cases for this case statement",
							tk.span(),
					));
				}
				closed = true;
				break;
			}
			TkType::CasePat if phase == Phase::Condition => {
				phase = Phase::Body;
				if block_string.is_empty() {
					block_string = tk.text().trim().to_string();
				} else {
					return Err(ShError::from_parse(
							"Expected only one variable in case statement",
							tk.span(),
					));
				}
			}
			_ if phase == Phase::Body && ctx.front_tk().is_some_and(|f_tk| f_tk.tk_type == TkType::CasePat) => {
				let block_span = compute_span(&block_tokens);
				block_root = parse_and_attach(take(&mut block_tokens), block_root)?;
				let block_node = Node::from(take(&mut block_root), block_span);
				cases.insert(take(&mut block_string), block_node);
				phase = Phase::Condition;
			}
			_ if phase == Phase::Body => {
				if block_string.is_empty() {
					return Err(ShError::from_parse(
							format!("Did not find a pattern for this case block: {}",tk.text()).as_str(),
							tk.span(),
					));
				}
				match tk.class() {
					_ if OPENERS.contains(&tk.class()) => {
						ctx.tokens.push_front(tk);
						if !block_tokens.is_empty() {
							block_root = parse_and_attach(take(&mut block_tokens), block_root)?;
						}
						ctx = parse_linear(ctx, true)?;
						if let Some(node) = ctx.root.pop_back() {
							block_root.push_back(node);
						}
					},
					_ => block_tokens.push_back(tk),
				}
			}
			_ => {
				return Err(ShError::from_parse("Something weird happened in this case statement", tk.span()))
			}
		}
	}

	let span_end = ctx.mark_end();
	let span = Span::from(span_start, span_end);

	if !closed {
		return Err(ShError::from_parse(
				"This case statement is missing an `esac`",
				span)
		)
	}

	if input_var.is_none() {
		return Err(ShError::from_parse(
				"Did not find a variable for this case statement",
				span,
		));
	}

	let input_var = input_var.unwrap();
	let node = Node {
		command: None,
		nd_type: NdType::Case { input_var, cases },
		span,
		flags: NdFlags::empty(),
		redirs: VecDeque::new()
	};
	ctx.attach_node(node);
	Ok(ctx)
}

pub fn build_select(mut ctx: DescentContext) -> RshResult<DescentContext> {
	// TODO: figure out a way to get 'in' to actually be a keyword
	// Fix the logic in general so this code doesn't have to use awkward work arounds
	let mut phase = Phase::Condition;

	let mut select_var: Option<Tk> = None;
	let mut opts: VecDeque<Tk> = VecDeque::new();
	let mut body_tokens: VecDeque<Tk> = VecDeque::new();
	let mut body_root: VecDeque<Node> = VecDeque::new();
	let mut closed = false;
	let span_start = ctx.mark_start();
	let body_start = 0;

	while let Some(tk) = ctx.next_tk() {
		match tk.class() {
			TkType::In => {
				phase = Phase::Vars
			}
			TkType::Do => {
				if opts.back().is_some_and(|tk| tk.class() == TkType::Cmdsep) {
					opts.pop_back();
				}
				phase = Phase::Body
			}
			TkType::Done => {
				if select_var.is_none() {
					return Err(ShError::from_parse("Did not find a variable for this select statement", tk.span()))
				}
				if opts.is_empty() {
					return Err(ShError::from_parse("Did not find any options for this select statement", tk.span()))
				}
				if body_tokens.is_empty() {
					return Err(ShError::from_parse("This select statement has an empty body", tk.span()))
				}
				body_root = parse_and_attach(take(&mut body_tokens), body_root)?;
				closed = true;
				break
			}
			_ => {
				match phase {
					Phase::Condition => {
						select_var = Some(tk);
					}
					Phase::Vars => {
						opts.push_back(tk);
					}
					Phase::Body => {
						match tk.class() {
							_ if OPENERS.contains(&tk.class()) => {
								ctx.tokens.push_front(tk);
								if !body_tokens.is_empty() {
									body_root = parse_and_attach(take(&mut body_tokens), body_root)?;
								}
								ctx = parse_linear(ctx, true)?;
								if let Some(node) = ctx.root.pop_back() {
									body_root.push_back(node);
								}
							},
							_ => body_tokens.push_back(tk),
						}
					}
					_ => unreachable!()
				}
			}
		}
	}
	let span_end = ctx.mark_end();
	let span = Span::from(span_start,span_end);

	if !closed {
		return Err(ShError::from_parse(
				"This select statement is missing a `done`",
				span)
		)
	}

	if !body_tokens.is_empty() {
		body_root = parse_and_attach(take(&mut body_tokens), body_root)?;
	}
	if select_var.is_none() {
		return Err(ShError::from_parse("Did not find a variable for this select statement", span))
	}
	let select_var = select_var.unwrap();
	let body_end = ctx.mark_end();
	let body_span = Span::from(body_start,body_end);
	let body = Node::from(body_root,body_span).boxed();
	let node = Node {
		command: None,
		nd_type: NdType::Select { select_var, opts, body },
		span,
		flags: NdFlags::VALID_OPERAND,
		redirs: VecDeque::new()
	};
	ctx.attach_node(node);
	Ok(ctx)
}

pub fn build_func_def(mut ctx: DescentContext) -> RshResult<DescentContext> {
	let def = ctx.next_tk().unwrap();
	if let TkType::FuncDef = def.tk_type {
		//TODO: initializing a new shellenv instead of cloning the current one here
		//could cause issues later, keep an eye on this
		//Might be fine to just build the AST since nothing is being executed or expanded
		let name = def.text();
		let body_tk = ctx.next_tk().unwrap(); // We can be reasonably sure that this exists
		let body = body_tk.text();
		let mut tokenizer = RshTokenizer::new(body);
		let mut state = ParseState {
			input: body.into(),
			tokens: VecDeque::new(),
			ast: Node::new()
		};

		loop {
			let mut deck = tokenizer.tokenize_one(TkizerCtx::new())?;
			if deck.is_empty() { break };
			state.tokens.extend(deck.drain(..));
		}
		let state = parse(state)?;
		let func_tree = state.ast.boxed();
		let node = Node {
			command: Some(def.clone()),
			nd_type: NdType::FuncDef { name: name.to_string(), body: func_tree },
			span: def.span(),
			flags: NdFlags::empty(),
			redirs: VecDeque::new()
		};
		ctx.attach_node(node);

		Ok(ctx)
	} else { unreachable!() }
}

pub fn build_assignment(mut ctx: DescentContext) -> RshResult<DescentContext> {
	let ass = ctx.next_tk().unwrap();
	if let TkType::Assignment = ass.tk_type {
		let (var, val) = ass.text().split_once('=').unwrap();
		let span = ass.span();
		let node = Node {
			command: None,
			nd_type: NdType::Assignment {
				name: var.to_string(),
				value: Some(val.to_string()),
			},
			span,
			flags: NdFlags::VALID_OPERAND,
			redirs: VecDeque::new()
		};
		ctx.attach_node(node);
		Ok(ctx)
	} else { unreachable!() }
}

pub fn build_brace_group(tokens: VecDeque<Tk>) -> RshResult<(Node, VecDeque<Tk>)> {
	todo!("Implement build_brace_group")
}

pub fn build_command(mut ctx: DescentContext) -> RshResult<DescentContext> {
	let mut argv = VecDeque::new();
	// We handle redirections in join_at_operators(), so hold them here and push them back onto the queue afterward
	let mut held_redirs = VecDeque::new();
	let mut background = false;

	let cmd = ctx.front_tk().unwrap().clone();
	let func_body = read_logic(|l| l.get_func(cmd.text()))?;
	let cmd_type = if builtin::BUILTINS.contains(&cmd.text()) {
		CmdType::Builtin
	} else if cmd.tk_type == TkType::Subshell {
		CmdType::Subshell
	} else if func_body.is_some() {
		CmdType::Function
	} else {
		CmdType::Command
	};

	while let Some(mut tk) = ctx.next_tk() {

		match tk.class() {
			TkType:: PipeBoth | TkType::Cmdsep | TkType::LogicAnd | TkType::LogicOr | TkType::Pipe => {
				ctx.tokens.push_front(tk);
				while let Some(redir) = held_redirs.pop_back() {
					// Push redirections back onto the queue, at the front
					// This has the effect of moving all redirections to the right of the command node
					// Which will be useful in join_at_operators()
					ctx.tokens.push_front(redir);
				}
				break;
			}
			TkType::Background => {
				background = true;
				break // Background operator '&' is always the last argument
			}
			TkType::Subshell => continue, // Don't include the subshell token in the args
			TkType::Ident | TkType::CommandSub | TkType::String | TkType::VariableSub | TkType::Assignment => {
				// Add to argv
				argv.push_back(tk);
			}
			TkType::Redirection { ref mut redir } => {
				// Handle redirection
				if redir.fd_target.is_none() {
					if let Some(target_tk) = ctx.next_tk() {
						if matches!(target_tk.class(), TkType::Ident | TkType::String) {
							redir.file_target = Some(Box::new(target_tk));
						}
					}
				}
				tk.tk_type = TkType::Redirection { redir: redir.clone() };
				held_redirs.push_back(tk)
			}
			TkType::SOI => continue,
			TkType::EOI => {
				while let Some(redir) = held_redirs.pop_back() {
					// Push redirections back onto the queue, at the front
					// This has the effect of moving all redirections to the right of the command node
					// Which will be useful in join_at_operators()
					ctx.tokens.push_front(redir);
				}
				break
			}
			_ => {
				return Err(ShError::from_parse(
						format!("Unexpected token: {:?}", tk).as_str(),
						tk.span(),
				));
			}
		}
	}


	let command = argv.front().cloned();
	let span = compute_span(&argv);
	let mut node = match cmd_type {
		CmdType::Command => {
			Node {
				command,
				nd_type: NdType::Command { argv },
				span,
				flags: NdFlags::VALID_OPERAND,
				redirs: VecDeque::new()
			}
		}
		CmdType::Builtin => {
			Node {
				command,
				nd_type: NdType::Builtin { argv },
				span,
				flags: NdFlags::VALID_OPERAND,
				redirs: VecDeque::new()
			}
		}
		CmdType::Function => {
			Node {
				command,
				nd_type: NdType::Function { body: Box::new(func_body.unwrap()), argv },
				span,
				flags: NdFlags::VALID_OPERAND,
				redirs: VecDeque::new()
			}
		}
		CmdType::Subshell => {
			Node {
				command: None,
				nd_type: NdType::Subshell { body: cmd.text().into(), argv },
				span,
				flags: NdFlags::VALID_OPERAND,
				redirs: VecDeque::new()
			}
		}
	};
	if background {
		node.flags |= NdFlags::BACKGROUND
	}
	ctx.attach_node(node);
	Ok(ctx)
}
