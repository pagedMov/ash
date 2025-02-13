use rustyline::highlight::Highlighter;

use crate::{builtin::BUILTINS, prelude::*};

use super::prompt::LashHelper;

pub const RESET: &str = "\x1b[0m";
pub const BLACK: &str = "\x1b[30m";
pub const RED: &str = "\x1b[1;31m";
pub const GREEN: &str = "\x1b[32m";
pub const YELLOW: &str = "\x1b[33m";
pub const BLUE: &str = "\x1b[34m";
pub const MAGENTA: &str = "\x1b[35m";
pub const CYAN: &str = "\x1b[36m";
pub const WHITE: &str = "\x1b[37m";
pub const BRIGHT_BLACK: &str = "\x1b[90m";
pub const BRIGHT_RED: &str = "\x1b[91m";
pub const BRIGHT_GREEN: &str = "\x1b[92m";
pub const BRIGHT_YELLOW: &str = "\x1b[93m";
pub const BRIGHT_BLUE: &str = "\x1b[94m";
pub const BRIGHT_MAGENTA: &str = "\x1b[95m";
pub const BRIGHT_CYAN: &str = "\x1b[96m";
pub const BRIGHT_WHITE: &str = "\x1b[97m";

pub const ERROR: &str = RED;
pub const COMMAND: &str = GREEN;
pub const KEYWORD: &str = YELLOW;
pub const STRING: &str = BLUE;
pub const ESCAPED: &str = CYAN;
pub const OPERATOR: &str = CYAN;
pub const NUMBER: &str = BRIGHT_BLUE;
pub const PATH: &str = BRIGHT_CYAN;
pub const VARSUB: &str = MAGENTA;
pub const COMMENT: &str = BRIGHT_BLACK;
pub const FUNCNAME: &str = CYAN;

#[derive(Debug)]
struct LashHighlighter<'a> {
	expect: Vec<Vec<Rule>>,
	lash: &'a mut Lash
}

impl<'a> LashHighlighter<'a> {
	pub fn new(lash: &'a mut Lash) -> Self {
		Self { expect: vec![], lash }
	}

	pub fn then_expectation() -> Vec<Rule> {
		vec![Rule::elif,Rule::r#else,Rule::fi]
	}

	pub fn expecting(&self, rule: Rule) -> bool {
		self.expect.last().is_some_and(|expect| expect.contains(&rule))
	}

	pub fn validate_cmd(&self,target: &str, path: &str) -> bool {
		if target.is_empty() || path.is_empty() {
			return false
		}
		let logic = self.lash.logic().clone();
		let is_cmd = path.split(':')
			.map(Path::new)
			.any(|p| p.join(target).exists());
			let is_func = logic.get_func(target).is_some();
			let is_alias = logic.get_alias(target).is_some();
			let is_builtin = BUILTINS.contains(&target);
			let is_file = {
				let mut path_cand = target.to_string();
				if path_cand.starts_with("~/") {
					path_cand = path_cand.strip_prefix("~").unwrap().to_string();
					let home = env::var("HOME").unwrap();
					path_cand = format!("{home}{path_cand}");
				}
				let path = Path::new(&path_cand);
				path.exists() && path.is_file()
			};

			is_cmd | is_func | is_alias | is_builtin | is_file
	}

	fn style_text(&self,code: &str, text: &str) -> String {
		format!("{code}{text}{RESET}")
	}

	fn highlight_struct(&mut self,pair: Pair<'a,Rule>, mut buffer: String) -> String {
		let struct_pair = pair.into_inner().next().unwrap();
		let span = struct_pair.as_span();
		match struct_pair.as_rule() {
			Rule::r#match |
			Rule::select |
			Rule::r#for => {
				self.expect.push(vec![Rule::in_kw]);
				let kw = self.style_text(KEYWORD, struct_pair.as_str());
				buffer.replace_span(span,&kw);
				buffer
			}
			Rule::r#if | Rule::elif => {
				self.expect.push(vec![Rule::r#then]);
				let kw = self.style_text(KEYWORD, struct_pair.as_str());
				buffer.replace_span(span,&kw);
				buffer
			}
			Rule::then if self.expecting(Rule::then) => {
				self.expect.pop();
				self.expect.push(vec![Rule::elif,Rule::r#else,Rule::fi]);
				let kw = self.style_text(KEYWORD, struct_pair.as_str());
				buffer.replace_span(span,&kw);
				buffer
			}
			Rule::r#else if self.expecting(Rule::r#else) => {
				self.expect.pop();
				self.expect.push(vec![Rule::fi]);
				let kw = self.style_text(KEYWORD, struct_pair.as_str());
				buffer.replace_span(span,&kw);
				buffer
			}
			Rule::fi if self.expecting(Rule::fi) => {
				self.expect.pop();
				let kw = self.style_text(KEYWORD, struct_pair.as_str());
				buffer.replace_span(span,&kw);
				buffer
			}
			Rule::r#while |
			Rule::until => {
				self.expect.push(vec![Rule::r#do]);
				let kw = self.style_text(KEYWORD, struct_pair.as_str());
				buffer.replace_span(span,&kw);
				buffer
			}
			Rule::r#do if self.expecting(Rule::r#do) => {
				self.expect.pop();
				self.expect.push(vec![Rule::done]);
				let kw = self.style_text(KEYWORD, struct_pair.as_str());
				buffer.replace_span(span,&kw);
				buffer
			}
			Rule::done => {
				self.expect.pop();
				let kw = self.style_text(KEYWORD, struct_pair.as_str());
				buffer.replace_span(span,&kw);
				buffer
			}
			Rule::in_kw if self.expecting(Rule::in_kw) => {
				self.expect.pop();
				self.expect.push(vec![Rule::r#do]);
				let kw = self.style_text(KEYWORD, struct_pair.as_str());
				buffer.replace_span(span,&kw);
				buffer
			}
			Rule::hl_subshell => {
				let body = struct_pair.scry(Rule::subsh_body).unwrap();
				let highlighted = self.highlight_input(body.as_str()).fill_from(body.as_str());
				let sub_left = self.style_text(STRING,"(");
				let sub_right = self.style_text(STRING,")");
				let subsh = format!("{sub_left}{highlighted}{sub_right}");
				buffer.replace_span( span, &subsh);
				buffer
				}
			Rule::hl_assign => {
				let (var,val) = struct_pair.as_str().split_once('=').unwrap();
				let styled_var = self.style_text(VARSUB,var);
				let styled_val = self.style_text(STRING,val);
				let display = [styled_var,styled_val].join("=").to_string();
				buffer.replace_span(span,&display);
				buffer
				}
			Rule::for_with_in | Rule::for_with_vars | Rule::in_with_vars => {
				match struct_pair.as_rule() {
					Rule::for_with_in => self.expect.push(vec![Rule::r#do]),
					Rule::for_with_vars => self.expect.push(vec![Rule::r#in]),
					Rule::in_with_vars => self.expect.push(vec![Rule::r#do]),
					_ => unreachable!()
				}
				let mut inner = if let Rule::for_with_in = struct_pair.as_rule() {
					let for_pair = struct_pair.scry(&[Rule::for_with_vars,Rule::r#for][..]).unwrap();
					let in_pair = struct_pair.scry(&[Rule::in_with_vars,Rule::in_kw][..]).unwrap();
					let mut for_deque = for_pair.to_deque();
					let in_deque = in_pair.to_deque();
					for_deque.extend(in_deque);
					for_deque
				} else {
					struct_pair.to_deque()
				};
				while let Some(word) = inner.pop_back() {
					let span = word.as_span();
					let code = match word.as_rule() {
						Rule::var => VARSUB,
						_ => KEYWORD
					};
					let styled = self.style_text(code, word.as_str());
					buffer.replace_span( span, &styled);
				}
				buffer
			}
			Rule::func_name => {
				let stripped = struct_pair.as_str().strip_suffix("()").unwrap();
				let styled = self.style_text(FUNCNAME, stripped);
				let display = format!("{}()",styled);
				buffer.replace_span( span, &display);
				buffer
			}
			_ => buffer
		}
	}

	fn highlight_redir(&mut self, pair: Pair<'a,Rule>) -> String {
		debug_assert!(pair.as_rule() == Rule::hl_redir);
		let mut body = pair.as_str().to_string();

		let redir = LashParse::parse(Rule::hl_redir, pair.as_str()).unwrap().into_iter().next().unwrap();
		let mut inner = redir.into_inner().rev();
		while let Some(part) = inner.next() {
			let span = part.as_span();
			match part.as_rule() {
				Rule::file => { /* Don't highlight it */ }
				Rule::fd_out | Rule::fd_target => {
					// Validate fd
					let fd = part.as_str();
					let fd_path = format!("/proc/self/fd/{fd}");
					let exists = Path::new(&fd_path).exists();
					let styled = if exists {
						self.style_text(COMMAND, fd)
					} else {
						self.style_text(ERROR, fd)
					};
					body.replace_span(span, &styled)
				}
				_ => {
					let styled = self.style_text(OPERATOR, part.as_str());
					body.replace_span(span, &styled)
				}
			}
		}
		body
	}

	fn highlight_dquote(&mut self,pair: Pair<'a,Rule>) -> String {
		debug_assert!(pair.as_rule() == Rule::dquoted);

		let body = pair.scry(Rule::dquote_body).unwrap().as_str();
		let sub_parse = LashParse::parse(Rule::syntax_hl, body);
		if let Ok(parse) = sub_parse {
			let mut buffer = body.to_string();
			let mut words = parse.into_iter().next().unwrap().to_deque();
			while let Some(word) = words.pop_back() {
				if word.as_rule() == Rule::words {
					let mut inner_words = word.into_inner().collect::<VecDeque<_>>();
					while let Some(in_wd) = inner_words.pop_back() {
						let span = in_wd.as_span();
						if in_wd.clone().step(1).is_some() {
							let wd_type = in_wd.step(1).unwrap();
							match wd_type.as_rule() {
								Rule::cmd_sub => {
									let body = wd_type.as_str().trim_start_matches("$(").trim_end_matches(')');
									let highlighted = self.highlight_input(body).fill_from(body);
									let sub_left = self.style_text(STRING,"$(");
									let sub_right = format!("{}{}",STRING,")");
									let cmd_sub = format!("{sub_left}{highlighted}{sub_right}");
									buffer.replace_span(span, &cmd_sub);
								}
								Rule::var_sub | Rule::param_sub => {
									let word = wd_type.as_str();
									let styled = format!("{}{}{}",VARSUB,word,STRING);
									buffer.replace_span(span, &styled);
								}
								_ => { /* Do nothing */ }
							}
						}
					}
				}
			}
			format!("\"{}{}{}\"",STRING,buffer,RESET)
		} else {
			pair.as_str().to_string()
		}
	}

	fn highlight_words(&mut self,pair: Pair<'a,Rule>, mut buffer: String, path: &str) -> String {
		let mut is_cmd = true;
		let mut words = pair.to_deque();
		while let Some(word_pair) = words.pop_back() {
			if word_pair.as_rule() == Rule::hl_redir {
				let span = word_pair.as_span();
				let styled = self.highlight_redir(word_pair);
				buffer.replace_span(span,&styled);
				continue
			}

			let sub_type = if word_pair.as_rule() == Rule::hl_glob {
				Some(word_pair.clone())
			} else {
				word_pair.clone().step(1)
			};
			if sub_type.clone().is_some_and(|pr| pr.as_rule() != Rule::tilde_sub) {
				let sub_type = sub_type.unwrap();
				let span = sub_type.as_span();
				match sub_type.as_rule() {
					Rule::loud_ident => { /* Pass */ }
					Rule::dquoted => {
						let styled = self.highlight_dquote(sub_type);
						buffer.replace_span(span,&styled);
					}
					Rule::hl_redir => {
						let styled = self.highlight_redir(sub_type);
						buffer.replace_span(span,&styled);
					}
					Rule::squoted => {
						let body = sub_type.as_str().trim_matches('\'');
						let styled = self.style_text(STRING,body);
						let squoted = format!("{}{}{}",'\'',styled,'\'');
						buffer.replace_span(span,&squoted);
					}
					Rule::param_sub | Rule::var_sub => {
						let word = sub_type.as_str();
						let styled = self.style_text(VARSUB,word);
						buffer.replace_span(span,&styled);
					}
					Rule::arr_index => {
						// If it works, it works
						let (left,right) = sub_type.as_str().split_once('[').unwrap();
						let styled_name = self.style_text(VARSUB,&left);
						let styled = [styled_name,right.to_string()].join("[").to_string();
						buffer.replace_span(span,&styled);
					}
					Rule::cmd_sub => {
						let body = sub_type.as_str().trim_start_matches("$(").trim_end_matches(')');
						let highlighted = self.highlight_input(body).fill_from(body);
						let sub_left = self.style_text(STRING,"$(");
						let sub_right = self.style_text(STRING,")");
						let cmd_sub = format!("{sub_left}{highlighted}{sub_right}");
						buffer.replace_span(span, &cmd_sub);
					}
					Rule::proc_sub => {
						let body = sub_type.as_str().trim_start_matches(">(").trim_start_matches("<(").trim_end_matches(')');
						let highlighted = self.highlight_input(body).fill_from(body);
						let sub_left = if sub_type.as_str().starts_with("<(") {
							self.style_text(STRING,"<(")
						} else {
							self.style_text(STRING,">(")
						};
						let sub_right = self.style_text(STRING,")");
						let proc_sub = format!("{sub_left}{highlighted}{sub_right}");
						buffer.replace_span(span, &proc_sub);
					}
					Rule::hl_glob => {
						let mut globs = sub_type.filter(Rule::hl_globs);
						while let Some(hl_glob) = globs.pop_back() {
							let glob_span = hl_glob.as_span();
							let glob = hl_glob.step(1).unwrap();
							match glob.as_rule() {
								Rule::glob_opt | Rule::glob_wild => {
									let styled = self.style_text(KEYWORD,glob.as_str());
									buffer.replace_span(glob_span, &styled);
								}
								Rule::glob_brackets => {
									let body = glob.as_str().trim_matches(['[',']']);
									let left_brack = format!("{}{}{}",KEYWORD,'[',RESET);
									let right_brack = format!("{}{}{}",KEYWORD,']',RESET);
									let rebuilt = format!("{left_brack}{body}{right_brack}");
									buffer.replace_span(glob_span,&rebuilt);
								}
								_ => unreachable!("Unexpected rule: {:?}",sub_type.as_rule())
							}
						}
					}
					Rule::hl_brace_word => {
						let body = sub_type.scry(Rule::brace_expand).unwrap().as_str().trim_matches(['{','}']);
						let left_brace = format!("{}{}{}",KEYWORD,'{',RESET);
						let right_brace = format!("{}{}{}",KEYWORD,'}',RESET);
						let rebuilt = format!("{left_brace}{body}{right_brace}");
						buffer.replace_span(span,&rebuilt);
					}
					_ => unreachable!("Unexpected rule: {:?}",sub_type.as_rule())
				}
			} else {
				let word = word_pair.as_str();
				let span = word_pair.as_span();
				if words.is_empty() {
					let code = if self.validate_cmd(word, path) {
						COMMAND
					} else {
						ERROR
					};
					let styled_word = self.style_text(code, word);
					buffer.replace_span(span, &styled_word);
				} else {
					let code = RESET;
					let styled_word = self.style_text(code, word);
					buffer.replace_span(span, &styled_word);
				}

			}
		}
		buffer.to_string()
	}

	fn highlight_pair(&mut self,pair: Pair<'a,Rule>, mut buffer: String) -> String {
		let path = env::var("PATH").unwrap_or_default();
		let span = pair.as_span();
		match pair.as_rule() {
			Rule::loud_sep => {
				let hl = self.style_text(RESET, &pair.as_str());
				buffer.replace_span(span, &hl)
			}
			Rule::loud_operator => {
				let hl = self.style_text(OPERATOR, &pair.as_str());
				buffer.replace_span( span, &hl)
			}
			Rule::words => buffer = self.highlight_words(pair, buffer, &path),
			Rule::shell_struct => buffer = self.highlight_struct(pair, buffer),
			_ => unreachable!("Reached highlight pair with this unexpected rule: {:?}",pair.as_rule())
		}
		buffer
	}

	fn highlight_input(&mut self,input: &'a str) -> String {
		let parsed_input = LashParse::parse(Rule::syntax_hl, input);
		match parsed_input {
			Ok(parsed_input) => {
				let mut buffer = parsed_input.as_str().to_string().fill_from(input);
				let mut inner = parsed_input.into_iter().next().unwrap().to_vec();
				while let Some(pair) = inner.pop() {
					buffer = self.highlight_pair(pair, buffer);
				}
				buffer
			}
			Err(_) => {
				input.to_string()
			}
		}
	}
}

impl<'a> Highlighter for LashHelper<'a> {
	fn highlight<'l>(&self, line: &'l str, pos: usize) -> std::borrow::Cow<'l, str> {
		let _ = pos;
		let mut cloned = self.lash.clone();
		let mut highlighter = LashHighlighter { expect: vec![], lash: &mut cloned };
		std::borrow::Cow::Owned(highlighter.highlight_input(line))
	}

	fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
		&'s self,
		prompt: &'p str,
		default: bool,
	) -> std::borrow::Cow<'b, str> {
		let _ = default;
		std::borrow::Cow::Borrowed(prompt)
	}

	fn highlight_hint<'h>(&self, hint: &'h str) -> std::borrow::Cow<'h, str> {
		std::borrow::Cow::Borrowed(hint)
	}

	fn highlight_candidate<'c>(
		&self,
		candidate: &'c str, // FIXME should be Completer::Candidate
		completion: rustyline::CompletionType,
	) -> std::borrow::Cow<'c, str> {
		let _ = completion;
		std::borrow::Cow::Borrowed(candidate)
	}

	fn highlight_char(&self, line: &str, pos: usize, kind: rustyline::highlight::CmdKind) -> bool {
		let _ = (line, pos, kind);
		true
	}
}
