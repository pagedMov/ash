use std::collections::{HashMap, VecDeque};
use bitflags::bitflags;
use once_cell::sync::Lazy;
use log::{error,debug,info,trace};
use std::mem::take;

use crate::event::ShellError;
use crate::shellenv::ShellEnv;
use crate::interp::token::{tokenize, Tk, TkType};

use super::token::WdFlags;

bitflags! {
	#[derive(Debug,Clone,PartialEq)]
	pub struct NdFlags: u32 {
		const COMBINE_OUT = 0b00000001;
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
	m.insert(TkType::CaseSep,vec![TkType::CaseDelim]); // `Until` expects `Do`
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

#[derive(PartialEq,Debug,Clone)]
enum Phase {
	Condition,
	Body,
	Vars,
	Array,
}

enum CmdType {
	Builtin,
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

/// Used in contexts that dont have direct access to the original input.
/// Propagated upwards
#[derive(PartialEq,Debug)]
pub struct ParseErr {
	pub msg: String,
	pub span: Span
}

impl ParseErr {
	pub fn from_parse(msg: String, span: Span) -> Self {
		Self { msg, span }
	}
}

/// Used in contexts which do have direct access to the original input.
/// ParseErrs are propagated upward until reaching a context which contains the ParseState, at
/// which point they are converted into a ParseErrFull

#[derive(Debug,Clone,PartialEq)]
pub struct Conditional {
	pub condition: Box<Node>,
	pub body: Box<Node>
}

#[derive(Debug,Clone,PartialEq)]
pub struct Node {
	pub nd_type: NdType,
	pub span: Span,
	pub flags: NdFlags
}

impl Node {
	pub fn from(deck: VecDeque<Node>,span: Span) -> Self {
		Self {
			nd_type: NdType::Root { deck },
			span,
			flags: NdFlags::empty()
		}
	}
	pub fn with_flags(&mut self,flags: NdFlags) -> Self {
		Self {
			nd_type: self.nd_type.clone(),
			span: self.span,
			flags
		}
	}
	pub fn span(&self) -> Span {
		self.span
	}
	pub fn node_type(&self) -> &NdType {
		&self.nd_type
	}
	pub fn set_span(&self,span: Span) -> Self {
		Self {
			nd_type: self.nd_type.clone(),
			span,
			flags: NdFlags::empty()
		}
	}
	pub fn get_argv(&self) -> Result<Vec<Tk>,ShellError> {
		let mut arg_vec = vec![];
		match &self.nd_type {
			NdType::Command { argv, redirs: _ } | NdType::Builtin { argv, redirs: _ } => {
				for arg in argv {
					arg_vec.push(arg.clone());
				}
				Ok(arg_vec)
			}
			_ => Err(ShellError::InternalError("Attempt to call `get_argv()` on a non-command node".into(), self.span)),
		}
	}
	pub fn get_redirs(&self) -> Result<Vec<Tk>,ShellError> {
		let mut redir_vec = vec![];
		match &self.nd_type {
			NdType::Command { argv: _, redirs } | NdType::Builtin { argv: _, redirs } => {
				for redir in redirs {
					redir_vec.push(redir.clone());
				}
				Ok(redir_vec)
			}
			_ => Err(ShellError::InternalError("Attempt to call `get_redirs()` on a non-command node".into(), self.span)),
		}
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
	Pipeline { left: Box<Node>, right: Box<Node>, both: bool },
	Chain { left: Box<Node>, right: Box<Node>, op: Box<Node> },
	BraceGroup { body: Box<Node> },
	Subshell { body: String }, // It's a string because we're going to parse it in a subshell later
	FuncDef { name: String, body: Box<Node> },
	Assignment {name: String, value: Option<String> },
	Command { argv: VecDeque<Tk>, redirs: VecDeque<Tk> },
	Builtin { argv: VecDeque<Tk>, redirs: VecDeque<Tk> },
	And,
	Or,
	Pipe,
	PipeBoth,
	Cmdsep
}

#[derive(Debug,PartialEq,Clone)]
pub struct ParseState<'a> {
	pub input: &'a str,
	pub shellenv: &'a ShellEnv,
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

pub fn descend<'a>(input: &'a str, shellenv: &'a ShellEnv) -> Result<ParseState<'a>,ParseErr> {
	info!("Starting descent into parsing with input: {:?}", input);
	let mut state = ParseState {
		input,
		shellenv,
		tokens: VecDeque::new(),
		ast: Node {
			nd_type: NdType::Root { deck: VecDeque::new() },
			span: Span::from(0,input.len()),
			flags: NdFlags::empty()
		}
	};

	state = tokenize(state)?;

	state = parse(state)?;

	Ok(state)
}

/// The purpose of this function is mainly just to be an entry point for the parsing logic
/// It is the only part of this logic that has access to the full input context. ParseErr's are
/// propagated up here and then converted to a complete ParseErrFull using the context of
/// ParseState. This is done because propagating errors upwards is probably
/// cheaper (and definitely easier) than propagating the raw input text downwards.
pub fn parse(state: ParseState) -> Result<ParseState,ParseErr> {
	let ctx = DescentContext::new(state.tokens.clone());

	get_tree(ctx).map(|ast| {
		debug!("Generated AST: {:#?}", ast);
		ParseState {
			input: state.input,
			shellenv: state.shellenv,
			tokens: state.tokens,
			ast
		}
	})
}

pub fn get_tree(ctx: DescentContext) -> Result<Node, ParseErr> {
	trace!("Building AST from tokens: {:?}",ctx.tokens);
	let span = compute_span(&ctx.tokens.clone());
	let ctx = parse_linear(ctx,false)?;

	Ok(
		Node {
			nd_type: NdType::Root { deck: ctx.root },
			span,
			flags: NdFlags::empty()
		}
	)
}

pub fn parse_linear(mut ctx: DescentContext, once: bool) -> Result<DescentContext, ParseErr> {
	// First pass just makes nodes without joining at operators
	info!("Starting linear parsing of tokens...");
	while let Some(tk) = ctx.next_tk() {
		trace!("Current tokens: {:?}", ctx.tokens);
		use crate::interp::token::TkType::*;
		match tk.class() {
			If => {
				info!("Found 'if' token, processing...");
				info!("tokens: {:?}",ctx.get_tk_texts());
				ctx = build_if(ctx)?;
				if once {
					break
				} else {
					continue
				}
			}
			While => {
				info!("Found 'while' token, processing...");
				ctx = build_loop(true,ctx)?;
				if once {
					break
				} else {
					continue
				}
			}
			Until => {
				info!("Found 'until' token, processing...");
				ctx = build_loop(false,ctx)?;
				if once {
					break
				} else {
					continue
				}
			}
			For => {
				info!("Found 'for' token, processing...");
				ctx = build_for(ctx)?;
				if once {
					break
				} else {
					continue
				}
			}
			Case => {
				info!("Found 'case' token, processing...");
				ctx = build_case(ctx)?;
				if once {
					break
				} else {
					continue
				}
			}
			Select => {
				info!("Found 'select' token, processing...");
				ctx = build_select(ctx)?;
				if once {
					break
				} else {
					continue
				}
			}
			Ident | String => {
				info!("Found command or string token, processing...");
				ctx.tokens.push_front(tk);
				ctx = build_command(ctx)?;
				// Fall through
			}
			Subshell => {
				info!("Found subshell");
				ctx.tokens.push_front(tk);
				ctx = build_subshell(ctx)?;
			}
			Assignment => {
				ctx.tokens.push_front(tk);
				ctx = build_assignment(ctx)?;
			}
			SOI => {
				trace!("Skipping Start of Input token");
				continue
			}
			EOI => {
				info!("End of Input token encountered, stopping parsing");
				break;
			}
			Do => {
				return Err(ParseErr::from_parse("Found `do` outside of loop context".into(), tk.span()))
			}
			Done => {
				return Err(ParseErr::from_parse("Found `done` outside of loop context".into(), tk.span()))
			}
			Else => {
				return Err(ParseErr::from_parse("Found `else` outside of `if` context".into(), tk.span()))
			}
			Elif => {
				return Err(ParseErr::from_parse("Found `elif` outside of `if` context".into(), tk.span()))
			}
			Then => {
				return Err(ParseErr::from_parse("Found `then` outside of `if` context".into(), tk.span()))
			}
			Fi => {
				return Err(ParseErr::from_parse("Found `fi` outside of `if` context".into(), tk.span()))
			}
			Esac => {
				return Err(ParseErr::from_parse("Found `esac` outside of `case` context".into(), tk.span()))
			}
			CaseDelim => {
				return Err(ParseErr::from_parse("Double semicolons (`;;`) are reserved for delimiting case statement conditions".into(), tk.span()))
			}
			CaseSep => {
				return Err(ParseErr::from_parse("Found this unmatched close parenthesis".into(), tk.span()))
			}
			Cmdsep | LogicAnd | LogicOr | Pipe | PipeBoth => {
				info!("Found operator token: {:?}, preserving as node", tk.class());
				match tk.class() {
					Cmdsep => ctx.attach_node(Node { nd_type: NdType::Cmdsep, span: tk.span(), flags: NdFlags::empty() }),
					LogicAnd => ctx.attach_node(Node { nd_type: NdType::And, span: tk.span(), flags: NdFlags::empty() }),
					LogicOr => ctx.attach_node(Node { nd_type: NdType::Or, span: tk.span(), flags: NdFlags::empty() }),
					Pipe => ctx.attach_node(Node { nd_type: NdType::Pipe, span: tk.span(), flags: NdFlags::empty() }),
					PipeBoth => ctx.attach_node(Node { nd_type: NdType::PipeBoth, span: tk.span(), flags: NdFlags::empty() }.with_flags(NdFlags::COMBINE_OUT)),
					_ => unreachable!(),
				}
			}
			_ => {
				unimplemented!(
					"Support for token type `{:?}` is not implemented yet",
					tk.class()
				);
			}
		}
		trace!("Current nodes: {:?}", &ctx.root);
	}

	ctx = join_at_operators(ctx)?;
	trace!("Completed linear parsing, nodes: {:?}", &ctx.root);
	Ok(ctx)
}

pub fn check_valid_operand(node: &Node) -> bool {
	use crate::interp::parse::NdType::*;
	matches!(node.nd_type, Pipeline {..} | Chain {..} | If {..} | For {..} | Loop {..} | Case {..} | Select {..} | Command {..} | Builtin {..})
}

pub fn join_at_operators(mut ctx: DescentContext) -> Result<DescentContext, ParseErr> {
	// Second pass will collect nodes at operators and construct pipelines and chains
	let mut buffer = VecDeque::new();

	while let Some(node) = ctx.next_node() {
		match node.nd_type {
			NdType::Pipe | NdType::PipeBoth => {
				let both = match node.nd_type {
					NdType::PipeBoth => true,
					NdType::Pipe => false,
					_ => unreachable!()
				};
				if let Some(left) = buffer.pop_back() {
					if let Some(right) = ctx.next_node() {
						if !check_valid_operand(&left) {
							return Err(ParseErr::from_parse("The left side of this pipeline is invalid".into(), node.span))
						}
						if !check_valid_operand(&right) {
							return Err(ParseErr::from_parse("The right side of this pipeline is invalid".into(), node.span))
						}
						let left = Box::new(left);
						let right = Box::new(right);
						let pipeline = Node {
							nd_type: NdType::Pipeline { left, right, both },
							span: Span::from(0,0),
							flags: NdFlags::empty()
						};
						buffer.push_back(pipeline);
					} else {
						return Err(ParseErr::from_parse("This pipeline is missing a right operand".into(), node.span))
					}
				} else {
					return Err(ParseErr::from_parse("This pipeline is missing a left operand".into(), node.span))
				}
			}
			NdType::Cmdsep => {
				while !buffer.is_empty() {
					ctx.root.push_front(buffer.pop_back().unwrap());
				}
				break
			}
			_ => buffer.push_back(node)
		}
	}
	ctx.root.extend(buffer.drain(..));
	while let Some(node) = ctx.next_node() {
		match node.nd_type {
			NdType::And | NdType::Or => {
				if let Some(left) = buffer.pop_back() {
					if let Some(right) = ctx.next_node() {
						if !check_valid_operand(&left) {
							return Err(ParseErr::from_parse("The left side of this chain is invalid".into(), node.span))
						}
						if !check_valid_operand(&right) {
							return Err(ParseErr::from_parse("The right side of this chain is invalid".into(), node.span))
						}
						let left = Box::new(left);
						let right = Box::new(right);
						let op = Box::new(node);
						let chain = Node {
							nd_type: NdType::Chain { left, right, op },
							span: Span::from(0,0),
							flags: NdFlags::empty()
						};
						buffer.push_back(chain);
					} else {
						return Err(ParseErr::from_parse("This chain is missing a right operand".into(), node.span))
					}
				} else {
					return Err(ParseErr::from_parse("This chain is missing a left operand".into(), node.span))
				}
			}
			NdType::Cmdsep => {
				while !buffer.is_empty() {
					ctx.root.push_front(buffer.pop_back().unwrap());
				}
				break
			}
			_ => buffer.push_back(node)
		}
	}

	ctx.root.extend(buffer.drain(..));
	Ok(ctx)
}

fn compute_span(tokens: &VecDeque<Tk>) -> Span {
	if tokens.is_empty() {
		Span::from(0, 0) // Default span for empty tokens
	} else {
		Span::from(tokens.front().unwrap().span().start, tokens.back().unwrap().span().end)
	}
}

fn parse_and_attach(mut tokens: VecDeque<Tk>, mut root: VecDeque<Node>) -> Result<VecDeque<Node>,ParseErr> {
	let mut sub_ctx = DescentContext::new(take(&mut tokens));
	sub_ctx = parse_linear(sub_ctx,true)?;
	while let Some(node) = sub_ctx.root.pop_back() {
		root.push_front(node);
	}
	Ok(root)
}

fn get_conditional(cond_root: VecDeque<Node>, cond_span: Span, body_root: VecDeque<Node>, body_span: Span) -> Conditional {
	let condition = Box::new(Node { nd_type: NdType::Root { deck: cond_root }, span: cond_span, flags: NdFlags::empty() });
	let body = Box::new(Node { nd_type: NdType::Root { deck: body_root }, span: body_span, flags: NdFlags::empty() });
	Conditional { condition, body }
}

pub fn build_if(mut ctx: DescentContext) -> Result<DescentContext, ParseErr> {
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
	debug!("Starting build_if, initial tokens: {:?}", ctx.get_tk_texts());

	while let Some(tk) = ctx.next_tk() {
		let err_span = ctx.mark_start();
		debug!(
			"build_if: processing token {:?}, current phase: {:?}, context: {:?}",
			tk.text(),
			phase,
			if_context
		);

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
				debug!("build_if: processing 'elif', switching context...");
				if_context = TkType::Elif;
				let cond_span = compute_span(&cond_tokens);
				cond_root = parse_and_attach(take(&mut cond_tokens), cond_root)?;

				let body_span = compute_span(&body_tokens);
				body_root = parse_and_attach(take(&mut body_tokens), body_root)?;

				let logic = get_conditional(take(&mut cond_root), cond_span, take(&mut body_root), body_span);
				logic_blocks.push_back(logic);
				phase = Phase::Condition;
				debug!(
					"build_if: added logic block, logic_blocks: {:?}, remaining tokens: {:?}",
					logic_blocks.len(),
					ctx.get_tk_texts()
				);
			}
			TkType::Then => {
				if if_context == TkType::Then {
					return Err(ParseErr::from_parse(
						"Did not find a condition for this `then` block".into(),
						Span::from(err_span,ctx.mark_end()))
					)
				}
				if if_context == TkType::Else {
					return Err(ParseErr::from_parse(
						"Else blocks do not get a `then` statement; give the body directly after the else keyword".into(),
						Span::from(err_span,ctx.mark_end()))
					)
				}
				if_context = TkType::Then;
				debug!("build_if: processing 'then', switching to Body phase");
				phase = Phase::Body;
			}
			TkType::Else => {
				debug!("build_if: processing 'else', switching context...");
				if if_context != TkType::Then {
					return Err(ParseErr::from_parse("Was expecting a `then` block, get an else block instead".into(), Span::from(err_span,ctx.mark_end())))
				}
				if_context = TkType::Else;
				let cond_span = compute_span(&cond_tokens);
				cond_root = parse_and_attach(take(&mut cond_tokens), cond_root)?;

				let body_span = compute_span(&body_tokens);
				body_root = parse_and_attach(take(&mut body_tokens), body_root)?;

				let logic = get_conditional(take(&mut cond_root), cond_span, take(&mut body_root), body_span);
				logic_blocks.push_back(logic);
				phase = Phase::Body;
				debug!(
					"build_if: added logic block, logic_blocks: {:?}, remaining tokens: {:?}",
					logic_blocks.len(),
					ctx.get_tk_texts()
				);
			}
			TkType::Fi => {
				closed = true;
				if !matches!(if_context,TkType::Then | TkType::Else) {
					return Err(ParseErr::from_parse("Was expecting a `then` block, get an else block instead".into(), Span::from(err_span,ctx.mark_end())))
				}
				debug!("build_if: processing 'fi', finalizing if block");
				debug!("cond tokens: {:?}",cond_tokens);
				debug!("body tokens: {:?}",body_tokens);
				if if_context == TkType::Else {
					debug!("build_if: processing else block...");
					let else_ctx = DescentContext::new(take(&mut body_tokens));
					let else_node = Box::new(get_tree(else_ctx)?);
					else_block = Some(else_node);
					debug!("build_if: else block node created");
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
				debug!("build_if: adding token {:?} to cond_tokens", tk.text());
				cond_tokens.push_back(tk);
			}
			_ if phase == Phase::Body => {
				debug!("build_if: adding token {:?} to body_tokens", tk.text());
				body_tokens.push_back(tk);
			}
			_ => unreachable!("Unexpected token in build_if: {:?}", tk),
		}
	}

	let span_end = ctx.mark_end();
	let span = Span::from(span_start, span_end);

	if !closed {
		return Err(ParseErr::from_parse("This if statement didn't get an `fi`".into(), span))
	}

	debug!("build_if: constructing final node...");
	let node = Node {
		nd_type: NdType::If { cond_blocks: logic_blocks, else_block },
		span,
		flags: NdFlags::empty()
	};
	debug!("created node: {:#?}",node);
	ctx.attach_node(node);
	debug!("build_if: node attached, final remaining tokens: {:?}", ctx.get_tk_texts());

	Ok(ctx)
}

pub fn build_for(mut ctx: DescentContext) -> Result<DescentContext, ParseErr> {
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
					return Err(ParseErr::from_parse(
						"This for loop didn't get any loop variables".into(),
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
					return Err(ParseErr::from_parse(
						"This for loop got an empty array".into(),
						Span::from(span_start,ctx.mark_end()))
					)
				}
				body_start = ctx.mark_start();
				phase = Phase::Body
			}
			TkType::Done => {
				if phase == Phase::Vars {
					return Err(ParseErr::from_parse(
						"This for loop has an unterminated variable definition".into(),
						Span::from(span_start,ctx.mark_end()))
					)
				}
				if phase == Phase::Array {
					return Err(ParseErr::from_parse(
						"This for loop has an unterminated array definition".into(),
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
		return Err(ParseErr::from_parse(
			"This loop is missing a `done`.".into(),
			span)
		)
	}

	if !body_tokens.is_empty() {
		body_root = parse_and_attach(take(&mut body_tokens), body_root)?;
	}
	let body_end = ctx.mark_end();
	let body_span = Span::from(body_start,body_end);
	let loop_body = Box::new(Node::from(body_root,body_span));
	let node = Node {
		nd_type: NdType::For { loop_vars, loop_arr, loop_body },
		span,
		flags: NdFlags::empty()
	};
	ctx.attach_node(node);
	Ok(ctx)
}

pub fn build_loop(condition: bool, mut ctx: DescentContext) -> Result<DescentContext, ParseErr> {
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
					return Err(ParseErr::from_parse("Did not find a condition for this loop".into(), tk.span()))
				}
				phase = Phase::Body
			}
			TkType::Done => {
				if body_tokens.is_empty() {
					return Err(ParseErr::from_parse("Did not find a body for this loop".into(), tk.span()))
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
		return Err(ParseErr::from_parse(
			"This loop is missing a `done`".into(),
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
		nd_type: NdType::Loop { condition: loop_condition, logic },
		span,
		flags: NdFlags::empty()
	};
	ctx.attach_node(node);
	Ok(ctx)
}

pub fn build_case(mut ctx: DescentContext) -> Result<DescentContext, ParseErr> {
	let mut cases = HashMap::new();
	let mut block_string = String::new();
	let mut block_tokens = VecDeque::new();
	let mut block_root = VecDeque::new();
	let mut input_var: Option<Tk> = None;
	let mut outer_phase = Phase::Vars;
	let mut inner_phase = Phase::Condition;
	let mut closed = false;

	let span_start = ctx.mark_start();

	while let Some(tk) = ctx.next_tk() {
		debug!("found token in build_case: {:?}",tk);
		match tk.class() {
			TkType::In => {
				if input_var.is_some() {
					if ctx.front_tk().is_some_and(|tk| tk.class() == TkType::Cmdsep) {
						ctx.next_tk();
					}
					outer_phase = Phase::Body;
				} else {
					return Err(ParseErr::from_parse(
							"Did not find a variable for this case statement".into(),
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
					return Err(ParseErr::from_parse(
							"Did not find any cases for this case statement".into(),
							tk.span(),
					));
				}
				closed = true;
				break;
			}
			TkType::CaseSep => {
				if inner_phase == Phase::Condition {
					inner_phase = Phase::Body;
				} else {
					return Err(ParseErr::from_parse(
							"Unexpected case separator".into(),
							tk.span(),
					));
				}
			}
			TkType::CaseDelim => {
				if inner_phase == Phase::Body {
					let block_span = compute_span(&block_tokens);
					block_root = parse_and_attach(take(&mut block_tokens), block_root)?;
					let block_node = Node::from(take(&mut block_root), block_span);
					cases.insert(take(&mut block_string), block_node);

					if ctx.front_tk().is_some_and(|tk| tk.class() == TkType::Cmdsep) {
						while let Some(tk) = ctx.next_tk() {
							if tk.class() != TkType::Cmdsep {
								ctx.tokens.push_front(tk); // Handle pesky newlines
								break
							}
						}
					}

					inner_phase = Phase::Condition;
				} else {
					return Err(ParseErr::from_parse(
							"Unexpected case delimiter".into(),
							tk.span(),
					));
				}
			}
			_ => {
				match outer_phase {
					Phase::Vars => {
						if input_var.is_none() && matches!(tk.class(), TkType::Ident) {
							input_var = Some(tk);
						} else {
							return Err(ParseErr::from_parse(
									"Expected only one variable in case statement".into(),
									tk.span(),
							));
						}
					}
					Phase::Body => match inner_phase {
						Phase::Condition => {
							if block_string.is_empty() {
								block_string = tk.text().into();
							} else {
								return Err(ParseErr::from_parse(
										format!("Found a second token for this case pattern: {}",tk.text()),
										tk.span(),
								));
							}
						}
						Phase::Body => {
							if block_string.is_empty() {
								return Err(ParseErr::from_parse(
										format!("Did not find a pattern for this case block: {}",tk.text()),
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
						_ => unreachable!("Unexpected phase in case statement parsing"),
					},
					_ => unreachable!("Unexpected outer phase in case statement parsing"),
				}
			}
		}
	}

	let span_end = ctx.mark_end();
	let span = Span::from(span_start, span_end);

	if !closed {
		return Err(ParseErr::from_parse(
			"This case statement is missing an `esac`".into(),
			span)
		)
	}

	if input_var.is_none() {
		return Err(ParseErr::from_parse(
				"Did not find a variable for this case statement".into(),
				span,
		));
	}

	let input_var = input_var.unwrap();
	let node = Node {
		nd_type: NdType::Case { input_var, cases },
		span,
		flags: NdFlags::empty()
	};
	ctx.attach_node(node);
	Ok(ctx)
}

pub fn build_select(mut ctx: DescentContext) -> Result<DescentContext, ParseErr> {
	// TODO: figure out a way to get 'in' to actually be a keyword
	// Fix the logic in general so this code doesn't have to use awkward work arounds
	let mut phase = Phase::Condition;
	trace!("entered build_select with these tokens: {:?}",ctx.tokens);

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
					return Err(ParseErr::from_parse("Did not find a variable for this select statement".into(), tk.span()))
				}
				if opts.is_empty() {
					return Err(ParseErr::from_parse("Did not find any options for this select statement".into(), tk.span()))
				}
				if body_tokens.is_empty() {
					return Err(ParseErr::from_parse("This select statement has an empty body".into(), tk.span()))
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
		return Err(ParseErr::from_parse(
			"This select statement is missing a `done`".into(),
			span)
		)
	}

	if !body_tokens.is_empty() {
		body_root = parse_and_attach(take(&mut body_tokens), body_root)?;
	}
	if select_var.is_none() {
		return Err(ParseErr::from_parse("Did not find a variable for this select statement".into(), span))
	}
	let select_var = select_var.unwrap();
	let body_end = ctx.mark_end();
	let body_span = Span::from(body_start,body_end);
	let body = Box::new(Node::from(body_root,body_span));
	let node = Node {
		nd_type: NdType::Select { select_var, opts, body },
		span,
		flags: NdFlags::empty()
	};
	ctx.attach_node(node);
	Ok(ctx)
}

pub fn build_subshell(mut ctx: DescentContext) -> Result<DescentContext, ParseErr> {
	let token = ctx.next_tk().unwrap();
	let span = token.span();
	let node = Node {
		nd_type: NdType::Subshell { body: token.text().into() },
		span,
		flags: NdFlags::empty()
	};
	ctx.attach_node(node);
	Ok(ctx)
}

pub fn build_func_def(tokens: VecDeque<Tk>) -> Result<(Node, VecDeque<Tk>), ParseErr> {
	todo!("Implement build_func_def")
}

pub fn build_assignment(mut ctx: DescentContext) -> Result<DescentContext, ParseErr> {
	let ass = ctx.next_tk().unwrap();
	if let TkType::Assignment = ass.tk_type {
		let (var, val) = ass.text().split_once('=').unwrap();
		let span = ass.span();
		let node = Node {
			nd_type: NdType::Assignment {
				name: var.to_string(),
				value: Some(val.to_string()),
			},
			span,
			flags: NdFlags::empty()
		};
		ctx.attach_node(node);
		Ok(ctx)
	} else { unreachable!() }
}

pub fn build_brace_group(tokens: VecDeque<Tk>) -> Result<(Node, VecDeque<Tk>), ParseErr> {
	todo!("Implement build_brace_group")
}

pub fn build_command(mut ctx: DescentContext) -> Result<DescentContext, ParseErr> {
	let mut argv = VecDeque::new(); // Temporary mutable Vec to collect arguments
	let mut redirs = VecDeque::new(); // Temporary mutable Vec to collect redirections

	let cmd_type = if ctx.front_tk().unwrap().flags().contains(WdFlags::BUILTIN) {
		CmdType::Builtin
	} else {
		CmdType::Command
	};

	while let Some(tk) = ctx.next_tk() {

		match tk.class() {
			TkType:: PipeBoth | TkType::Cmdsep | TkType::LogicAnd | TkType::LogicOr | TkType::Pipe => {
				debug!("build_command breaking on: {:?}", tk);
				ctx.tokens.push_front(tk);
				break;
			}
			TkType::Ident | TkType::String | TkType::VariableSub => {
				// Add to argv
				argv.push_back(tk.clone());
			}
			TkType::Redirection { ref redir } => {
				// Handle redirection
				let mut new_redir = redir.clone();
				if redir.fd_target.is_none() {
					if let Some(file_target) = ctx.front_tk() { // Look ahead for file target
						match file_target.class() {
							TkType::String | TkType::Ident => {
								new_redir.file_target = Some(Box::new(ctx.next_tk().unwrap()));
								redirs.push_back(Tk {
									tk_type: TkType::Redirection { redir: new_redir },
									wd: tk.wd.clone(),
								});
							}
							_ => {
								return Err(ParseErr::from_parse(
										"Expected identifier after redirection operator".to_string(),
										file_target.span(),
								));
							}
						}
					} else {
						return Err(ParseErr::from_parse(
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
				error!("ran into EOI while building command, with these tokens: {:?}",ctx.get_tk_texts());
				return Err(ParseErr::from_parse(
						format!("Unexpected token: {:?}", tk.text()),
						tk.span(),
				));
			}
		}
	}

	debug!("returning from build_command with tokens: {:?}", ctx.tokens);

	let span = compute_span(&argv);
	let node = match cmd_type {
		CmdType::Command => {
			Node {
				nd_type: NdType::Command { argv, redirs },
				span,
				flags: NdFlags::empty()
			}
		}
		CmdType::Builtin => {
			Node {
				nd_type: NdType::Builtin { argv, redirs },
				span,
				flags: NdFlags::empty()
			}
		}
	};
	debug!("attaching node: {:?}",node);
	ctx.attach_node(node);
	debug!("ast state: {:?}",ctx.root);
	Ok(ctx)
}
