// Turn back now if you value your sanity

#[cfg(test)]
pub mod tokenizer_tests {
	use std::collections::VecDeque;
	use crate::{interp::token::*, shellenv::write_vars};

	#[test]
	fn simple() {
		let input = "echo Hello, World!";
		let mut tkizer = LashTokenizer::new(input);
		let mut tokens = vec![];
		while !tkizer.input().is_empty() {
			let mut new_tokens = tkizer.tokenize_one(true).unwrap();
			tokens.append(&mut new_tokens);
		}
		let debug_result = format!("{:?}",tokens);
		let expected = "[Tk { tk_type: SOI, wd: WordDesc { text: \"\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \"echo\", span: Span { start: 0, end: 4 }, flags: WdFlags(BUILTIN) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: String, wd: WordDesc { text: \"Hello,\", span: Span { start: 5, end: 11 }, flags: WdFlags(IS_ARG) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: String, wd: WordDesc { text: \"World!\", span: Span { start: 12, end: 18 }, flags: WdFlags(IS_ARG) } }]";
		assert_eq!(debug_result,expected)
	}

	#[test]
	fn multiple_args() {
		let input = "ls -l /home/user";
		let mut tokenizer = LashTokenizer::new(input);

		let tokens = tokenizer.tokenize_one(true).unwrap();
		let debug_result = format!("{:?}",tokens);
		let expected = "[Tk { tk_type: SOI, wd: WordDesc { text: \"\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \"ls\", span: Span { start: 0, end: 2 }, flags: WdFlags(0x0) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: String, wd: WordDesc { text: \"-l\", span: Span { start: 3, end: 5 }, flags: WdFlags(IS_ARG) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: String, wd: WordDesc { text: \"/home/user\", span: Span { start: 6, end: 16 }, flags: WdFlags(IS_ARG) } }]";
		assert_eq!(debug_result,expected)
	}

	#[test]
	fn quoted_arg() {
		let input = "echo \"Hello, world!\"";
		let mut tokenizer = LashTokenizer::new(input);

		let tokens = tokenizer.tokenize_one(true).unwrap();
		let debug_result = format!("{:?}",tokens);
		let expected = "[Tk { tk_type: SOI, wd: WordDesc { text: \"\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \"echo\", span: Span { start: 0, end: 4 }, flags: WdFlags(BUILTIN) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: DQuote, wd: WordDesc { text: \"\\\"\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: String, wd: WordDesc { text: \"Hello, world!\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: DQuote, wd: WordDesc { text: \"\\\"\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }]";
		assert_eq!(debug_result,expected)
	}

	#[test]
	fn single_quoted_arg() {
		let input = "echo 'single quoted arg'";
		let mut tokenizer = LashTokenizer::new(input);

		let tokens = tokenizer.tokenize_one(true).unwrap();
		let debug_result = format!("{:?}",tokens);
		let expected = "[Tk { tk_type: SOI, wd: WordDesc { text: \"\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \"echo\", span: Span { start: 0, end: 4 }, flags: WdFlags(BUILTIN) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: SQuote, wd: WordDesc { text: \"'\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: String, wd: WordDesc { text: \"single quoted arg\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: SQuote, wd: WordDesc { text: \"'\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }]";
		assert_eq!(debug_result,expected)
	}

	#[test]
	fn variable_expansion() {
		let input = "echo $USER";
		let mut tokenizer = LashTokenizer::new(input);

		let tokens = tokenizer.tokenize_one(true).unwrap();
		let debug_result = format!("{:?}",tokens);
		let expected = "[Tk { tk_type: SOI, wd: WordDesc { text: \"\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \"echo\", span: Span { start: 0, end: 4 }, flags: WdFlags(BUILTIN) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: String, wd: WordDesc { text: \"pagedmov\", span: Span { start: 5, end: 10 }, flags: WdFlags(IS_ARG) } }]";
		assert_eq!(debug_result,expected)
	}

	#[test]
	fn pipeline() {
		let input = "ls | grep file";
		let mut tokenizer = LashTokenizer::new(input);

		let tokens = tokenizer.tokenize_one(true).unwrap();
		let debug_result = format!("{:?}",tokens);
		let expected = "[Tk { tk_type: SOI, wd: WordDesc { text: \"\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \"ls\", span: Span { start: 0, end: 2 }, flags: WdFlags(0x0) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Pipe, wd: WordDesc { text: \"|\", span: Span { start: 0, end: 0 }, flags: WdFlags(IS_OP) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \"grep\", span: Span { start: 3, end: 4 }, flags: WdFlags(0x0) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: String, wd: WordDesc { text: \"file\", span: Span { start: 5, end: 9 }, flags: WdFlags(IS_ARG) } }]";
		assert_eq!(debug_result,expected)
	}


	#[test]
	fn redirection() {
		let input = "echo hello > output.txt";
		let mut tokenizer = LashTokenizer::new(input);

		let tokens = tokenizer.tokenize_one(true).unwrap();
		let debug_result = format!("{:?}",tokens);
		let expected = "[Tk { tk_type: SOI, wd: WordDesc { text: \"\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \"echo\", span: Span { start: 0, end: 4 }, flags: WdFlags(BUILTIN) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: String, wd: WordDesc { text: \"hello\", span: Span { start: 5, end: 10 }, flags: WdFlags(IS_ARG) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Redirection { redir: Redir { fd_source: 1, op: Output, fd_target: None, file_target: Some(Tk { tk_type: Ident, wd: WordDesc { text: \"\", span: Span { start: 11, end: 12 }, flags: WdFlags(0x0) } }) } }, wd: WordDesc { text: \">\", span: Span { start: 11, end: 12 }, flags: WdFlags(IS_ARG | IS_OP) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: String, wd: WordDesc { text: \"output.txt\", span: Span { start: 13, end: 23 }, flags: WdFlags(IS_ARG) } }]";
		assert_eq!(debug_result,expected)
	}

	#[test]
	fn subshell() {
		let input = "(echo hi)";
		let mut tokenizer = LashTokenizer::new(input);

		let tokens = tokenizer.tokenize_one(true).unwrap();
		let debug_result = format!("{:?}",tokens);
		let expected = "[Tk { tk_type: SOI, wd: WordDesc { text: \"\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Subshell, wd: WordDesc { text: \"echo hi\", span: Span { start: 0, end: 5 }, flags: WdFlags(0x0) } }]";
		assert_eq!(debug_result,expected)
	}

	#[test]
	fn subshell_multiline() {
		let input = "(#!python
print(\"hello world\")
)";
		let mut tokenizer = LashTokenizer::new(input);

		let tokens = tokenizer.tokenize_one(true).unwrap();
		let debug_result = format!("{:?}",tokens);
		let expected = "[Tk { tk_type: SOI, wd: WordDesc { text: \"\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Subshell, wd: WordDesc { text: \"#!python\\nprint(\\\"hello world\\\")\\n\", span: Span { start: 0, end: 9 }, flags: WdFlags(0x0) } }]";
		assert_eq!(debug_result,expected)
	}

	#[test]
	fn func_def() {
		let input = "func() {
			echo hi
			cat file.txt
		}";
		let mut tokenizer = LashTokenizer::new(input);

		let tokens = tokenizer.tokenize_one(true).unwrap();
		let debug_result = format!("{:?}",tokens);
		let expected = "[Tk { tk_type: SOI, wd: WordDesc { text: \"\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: FuncDef, wd: WordDesc { text: \"func\", span: Span { start: 0, end: 6 }, flags: WdFlags(0x0) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: FuncBody, wd: WordDesc { text: \"\\n\\t\\t\\techo hi\\n\\t\\t\\tcat file.txt\\n\\t\\t\", span: Span { start: 7, end: 8 }, flags: WdFlags(0x0) } }]";
		assert_eq!(debug_result,expected)
	}

	#[test]
	fn expand_params() {
		let input = "echo $@";
		let mut tokenizer = LashTokenizer::new(input);
		write_vars(|v| v.pos_param_pushback("one".into())).unwrap();
		write_vars(|v| v.pos_param_pushback("two".into())).unwrap();
		write_vars(|v| v.pos_param_pushback("three".into())).unwrap();
		write_vars(|v| v.pos_param_pushback("four".into())).unwrap();

		let tokens = tokenizer.tokenize_one(true).unwrap();
		let debug_result = format!("{:?}",tokens);
		let expected = "[Tk { tk_type: SOI, wd: WordDesc { text: \"\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \"echo\", span: Span { start: 0, end: 4 }, flags: WdFlags(BUILTIN) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: String, wd: WordDesc { text: \"one\", span: Span { start: 5, end: 7 }, flags: WdFlags(IS_ARG) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: String, wd: WordDesc { text: \"two\", span: Span { start: 0, end: 0 }, flags: WdFlags(IS_ARG) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: String, wd: WordDesc { text: \"three\", span: Span { start: 0, end: 0 }, flags: WdFlags(IS_ARG) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: String, wd: WordDesc { text: \"four\", span: Span { start: 0, end: 0 }, flags: WdFlags(IS_ARG) } }]";
		assert_eq!(debug_result,expected)
	}

	#[test]
	fn if_structure() {
		let input = "if true; then echo hi; elif false; then echo hello; else echo greetings; fi";
		let mut tokenizer = LashTokenizer::new(input);

		let tokens = tokenizer.tokenize_one(true).unwrap();
		let debug_result = format!("{:?}",tokens);
		let expected = "[Tk { tk_type: SOI, wd: WordDesc { text: \"\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: If, wd: WordDesc { text: \"if\", span: Span { start: 0, end: 2 }, flags: WdFlags(KEYWORD) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \"true\", span: Span { start: 3, end: 7 }, flags: WdFlags(0x0) } }, Tk { tk_type: Cmdsep, wd: WordDesc { text: \";\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Then, wd: WordDesc { text: \"then\", span: Span { start: 9, end: 13 }, flags: WdFlags(KEYWORD) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \"echo\", span: Span { start: 14, end: 18 }, flags: WdFlags(BUILTIN) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: String, wd: WordDesc { text: \"hi\", span: Span { start: 19, end: 21 }, flags: WdFlags(IS_ARG) } }, Tk { tk_type: Cmdsep, wd: WordDesc { text: \";\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Elif, wd: WordDesc { text: \"elif\", span: Span { start: 23, end: 27 }, flags: WdFlags(KEYWORD) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \"false\", span: Span { start: 28, end: 33 }, flags: WdFlags(0x0) } }, Tk { tk_type: Cmdsep, wd: WordDesc { text: \";\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Then, wd: WordDesc { text: \"then\", span: Span { start: 35, end: 39 }, flags: WdFlags(KEYWORD) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \"echo\", span: Span { start: 40, end: 44 }, flags: WdFlags(BUILTIN) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: String, wd: WordDesc { text: \"hello\", span: Span { start: 45, end: 50 }, flags: WdFlags(IS_ARG) } }, Tk { tk_type: Cmdsep, wd: WordDesc { text: \";\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Else, wd: WordDesc { text: \"else\", span: Span { start: 52, end: 56 }, flags: WdFlags(KEYWORD) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \"echo\", span: Span { start: 57, end: 61 }, flags: WdFlags(BUILTIN) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: String, wd: WordDesc { text: \"greetings\", span: Span { start: 62, end: 71 }, flags: WdFlags(IS_ARG) } }, Tk { tk_type: Cmdsep, wd: WordDesc { text: \";\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Fi, wd: WordDesc { text: \"fi\", span: Span { start: 73, end: 75 }, flags: WdFlags(KEYWORD) } }]";
		assert_eq!(debug_result,expected)
	}

	#[test]
	fn for_loop() {
		let input = "for i in 1 2 3; do echo $i; done";
		let mut tokenizer = LashTokenizer::new(input);

		let tokens = tokenizer.tokenize_one(true).unwrap();
		let debug_result = format!("{:?}",tokens);
		let expected = "[Tk { tk_type: SOI, wd: WordDesc { text: \"\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: For, wd: WordDesc { text: \"for\", span: Span { start: 0, end: 3 }, flags: WdFlags(KEYWORD) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \"i\", span: Span { start: 4, end: 5 }, flags: WdFlags(0x0) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: In, wd: WordDesc { text: \"in\", span: Span { start: 6, end: 8 }, flags: WdFlags(KEYWORD) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \"1\", span: Span { start: 9, end: 10 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \"2\", span: Span { start: 11, end: 12 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \"3\", span: Span { start: 13, end: 14 }, flags: WdFlags(0x0) } }, Tk { tk_type: Cmdsep, wd: WordDesc { text: \";\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Do, wd: WordDesc { text: \"do\", span: Span { start: 0, end: 0 }, flags: WdFlags(KEYWORD) } }, Tk { tk_type: LoopBody, wd: WordDesc { text: \" echo ; \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Done, wd: WordDesc { text: \"done\", span: Span { start: 0, end: 0 }, flags: WdFlags(KEYWORD) } }]";
		assert_eq!(debug_result,expected)
	}

	#[test]
	fn while_loop() {
		let input = "while true; do echo working; done";
		let mut tokenizer = LashTokenizer::new(input);

		let tokens = tokenizer.tokenize_one(true).unwrap();
		let debug_result = format!("{:?}",tokens);
		let expected = "[Tk { tk_type: SOI, wd: WordDesc { text: \"\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: While, wd: WordDesc { text: \"while\", span: Span { start: 0, end: 5 }, flags: WdFlags(KEYWORD) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: LoopCond, wd: WordDesc { text: \"true; \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Do, wd: WordDesc { text: \"do\", span: Span { start: 0, end: 0 }, flags: WdFlags(KEYWORD) } }, Tk { tk_type: LoopBody, wd: WordDesc { text: \" echo working; \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Done, wd: WordDesc { text: \"done\", span: Span { start: 0, end: 0 }, flags: WdFlags(KEYWORD) } }]";
		assert_eq!(debug_result,expected)
	}

	#[test]
	fn until_loop() {
		let input = "until true; do echo waiting; done";
		let mut tokenizer = LashTokenizer::new(input);

		let tokens = tokenizer.tokenize_one(true).unwrap();
		let debug_result = format!("{:?}",tokens);
		let expected = "[Tk { tk_type: SOI, wd: WordDesc { text: \"\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Until, wd: WordDesc { text: \"until\", span: Span { start: 0, end: 5 }, flags: WdFlags(KEYWORD) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: LoopCond, wd: WordDesc { text: \"true; \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Do, wd: WordDesc { text: \"do\", span: Span { start: 0, end: 0 }, flags: WdFlags(KEYWORD) } }, Tk { tk_type: LoopBody, wd: WordDesc { text: \" echo waiting; \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Done, wd: WordDesc { text: \"done\", span: Span { start: 0, end: 0 }, flags: WdFlags(KEYWORD) } }]";
		assert_eq!(debug_result,expected)
	}

	#[test]
	fn final_boss() {
		let input = "if while while while echo; do while echo; do while echo; do echo; done; done; done; do until until until echo; do echo; done; do for i in 1 2 3; do echo; done; done; do for i in 1 2 3; do if echo; then echo; fi; done; done; done; do until for i in 1 2 3; do if while echo; do echo; done; then until echo; do echo; done; fi; done; do if while for i in 1 2 3; do echo; done; do if echo; then echo; fi; done; then until while echo; do echo; done; do until echo; do echo; done; done; fi; done; done; then until for i in 1 2 3; do while for i in 1 2 3; do for i in 1 2 3; do echo; done; done; do if if echo; then echo; fi; then while echo; do echo; done; fi; done; done; do if until while until echo; do echo; done; do for i in 1 2 3; do echo; done; done; do until if echo; then echo; fi; do while echo; do echo; done; done; done; then for i in 1 2 3; do for i in 1 2 3; do until echo; do echo; done; done; done; fi; done; elif for i in 1 2 3; do if if if for i in 1 2 3; do echo; done; then if echo; then echo; fi; fi; then while while echo; do echo; done; do until echo; do echo; done; done; fi; then while until for i in 1 2 3; do echo; done; do if echo; then echo; fi; done; do for i in 1 2 3; do while echo; do echo; done; done; done; fi; done; then if while until if until echo; do echo; done; then for i in 1 2 3; do echo; done; fi; do while if echo; then echo; fi; do while echo; do echo; done; done; done; do for i in 1 2 3; do until until echo; do echo; done; do for i in 1 2 3; do echo; done; done; done; done; then until if for i in 1 2 3; do if echo; then echo; fi; done; then if while echo; do echo; done; then until echo; do echo; done; fi; fi; do while while for i in 1 2 3; do echo; done; do if echo; then echo; fi; done; do until while echo; do echo; done; do until echo; do echo; done; done; done; done; fi; else for i in 1 2 3; do until for i in 1 2 3; do for i in 1 2 3; do echo; done; done; do if if echo; then echo; fi; then while echo; do echo; done; fi; done; done; fi";
		let mut tokenizer = LashTokenizer::new(input);

		let tokens = tokenizer.tokenize_one(true).unwrap();
		let debug_result = format!("{:?}",tokens);
		let expected = "[Tk { tk_type: SOI, wd: WordDesc { text: \"\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: If, wd: WordDesc { text: \"if\", span: Span { start: 0, end: 2 }, flags: WdFlags(KEYWORD) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: While, wd: WordDesc { text: \"while\", span: Span { start: 3, end: 8 }, flags: WdFlags(KEYWORD) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: LoopCond, wd: WordDesc { text: \"while while echo; do while echo; do while echo; do echo; done; done; done; do until until until echo; \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Do, wd: WordDesc { text: \"do\", span: Span { start: 0, end: 0 }, flags: WdFlags(KEYWORD) } }, Tk { tk_type: LoopBody, wd: WordDesc { text: \" echo; \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Done, wd: WordDesc { text: \"done\", span: Span { start: 0, end: 0 }, flags: WdFlags(KEYWORD) } }, Tk { tk_type: Cmdsep, wd: WordDesc { text: \";\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Do, wd: WordDesc { text: \"do\", span: Span { start: 9, end: 14 }, flags: WdFlags(KEYWORD) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: For, wd: WordDesc { text: \"for\", span: Span { start: 15, end: 20 }, flags: WdFlags(KEYWORD) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \"i\", span: Span { start: 21, end: 25 }, flags: WdFlags(0x0) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: In, wd: WordDesc { text: \"in\", span: Span { start: 27, end: 29 }, flags: WdFlags(KEYWORD) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \"1\", span: Span { start: 30, end: 35 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \"2\", span: Span { start: 36, end: 40 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \"3\", span: Span { start: 42, end: 44 }, flags: WdFlags(0x0) } }, Tk { tk_type: Cmdsep, wd: WordDesc { text: \";\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Do, wd: WordDesc { text: \"do\", span: Span { start: 0, end: 0 }, flags: WdFlags(KEYWORD) } }, Tk { tk_type: LoopBody, wd: WordDesc { text: \" echo; done; \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Done, wd: WordDesc { text: \"done\", span: Span { start: 0, end: 0 }, flags: WdFlags(KEYWORD) } }, Tk { tk_type: Cmdsep, wd: WordDesc { text: \";\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Do, wd: WordDesc { text: \"do\", span: Span { start: 45, end: 50 }, flags: WdFlags(KEYWORD) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: For, wd: WordDesc { text: \"for\", span: Span { start: 51, end: 55 }, flags: WdFlags(KEYWORD) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \"i\", span: Span { start: 57, end: 59 }, flags: WdFlags(0x0) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: In, wd: WordDesc { text: \"in\", span: Span { start: 60, end: 64 }, flags: WdFlags(KEYWORD) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \"1\", span: Span { start: 66, end: 70 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \"2\", span: Span { start: 72, end: 76 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \"3\", span: Span { start: 78, end: 82 }, flags: WdFlags(0x0) } }, Tk { tk_type: Cmdsep, wd: WordDesc { text: \";\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Do, wd: WordDesc { text: \"do\", span: Span { start: 0, end: 0 }, flags: WdFlags(KEYWORD) } }, Tk { tk_type: LoopBody, wd: WordDesc { text: \" if echo; then echo; fi; done; \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Done, wd: WordDesc { text: \"done\", span: Span { start: 0, end: 0 }, flags: WdFlags(KEYWORD) } }, Tk { tk_type: Cmdsep, wd: WordDesc { text: \";\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Space, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Done, wd: WordDesc { text: \"done\", span: Span { start: 84, end: 86 }, flags: WdFlags(KEYWORD) } }, Tk { tk_type: Cmdsep, wd: WordDesc { text: \";\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }]";
		assert_eq!(debug_result,expected)
	}
}

#[cfg(test)]
pub mod parser_tests {
	use crate::interp::{parse::*, token::LashTokenizer};

	#[test]
	fn parser_simple() {
		let input = "echo hello world";
		let mut tokenizer = LashTokenizer::new(input);

		let mut flat_nodes = Vec::new(); // Collect nodes directly
		loop {
			let state = descend(&mut tokenizer).unwrap();
			if state.tokens.is_empty() {
				break;
			}

			// Extract the deck from the Root node while iterating
			if let NdType::Root { deck } = state.ast.nd_type {
				flat_nodes.extend(deck);
			} else {
				flat_nodes.push(state.ast);
			}
		}

		let expected = "[Node { command: Some(Tk { tk_type: Ident, wd: WordDesc { text: \"echo\", span: Span { start: 0, end: 4 }, flags: WdFlags(BUILTIN) } }), nd_type: Builtin { argv: [Tk { tk_type: Ident, wd: WordDesc { text: \"echo\", span: Span { start: 0, end: 4 }, flags: WdFlags(BUILTIN) } }, Tk { tk_type: String, wd: WordDesc { text: \"hello\", span: Span { start: 5, end: 10 }, flags: WdFlags(IS_ARG) } }, Tk { tk_type: String, wd: WordDesc { text: \"world\", span: Span { start: 11, end: 16 }, flags: WdFlags(IS_ARG) } }] }, span: Span { start: 0, end: 16 }, flags: NdFlags(VALID_OPERAND), redirs: [] }]";
		let debug_result = format!("{:?}",flat_nodes);
		assert_eq!(debug_result,expected)
	}

	#[test]
	fn parser_multiple_args() {
		let input = "ls -l /home/user";
		let mut tokenizer = LashTokenizer::new(input);

		let mut flat_nodes = Vec::new();
		loop {
			let state = descend(&mut tokenizer).unwrap();
			if state.tokens.is_empty() {
				break;
			}

			if let NdType::Root { deck } = state.ast.nd_type {
				flat_nodes.extend(deck);
			} else {
				flat_nodes.push(state.ast);
			}
		}

		let expected = "[Node { command: Some(Tk { tk_type: Ident, wd: WordDesc { text: \"ls\", span: Span { start: 0, end: 2 }, flags: WdFlags(0x0) } }), nd_type: Command { argv: [Tk { tk_type: Ident, wd: WordDesc { text: \"ls\", span: Span { start: 0, end: 2 }, flags: WdFlags(0x0) } }, Tk { tk_type: String, wd: WordDesc { text: \"-l\", span: Span { start: 3, end: 5 }, flags: WdFlags(IS_ARG) } }, Tk { tk_type: String, wd: WordDesc { text: \"/home/user\", span: Span { start: 6, end: 16 }, flags: WdFlags(IS_ARG) } }] }, span: Span { start: 0, end: 16 }, flags: NdFlags(VALID_OPERAND), redirs: [] }]";
		let debug_result = format!("{:?}",flat_nodes);
		assert_eq!(debug_result,expected)
	}

	#[test]
	fn parser_redirection_simple() {
		let input = "echo hello world > file.txt";
		let mut tokenizer = LashTokenizer::new(input);

		let mut flat_nodes = Vec::new();
		loop {
			let state = descend(&mut tokenizer).unwrap();
			if state.tokens.is_empty() {
				break;
			}

			if let NdType::Root { deck } = state.ast.nd_type {
				flat_nodes.extend(deck);
			} else {
				flat_nodes.push(state.ast);
			}
		}

		let expected = "[Node { command: Some(Tk { tk_type: Ident, wd: WordDesc { text: \"echo\", span: Span { start: 0, end: 4 }, flags: WdFlags(BUILTIN) } }), nd_type: Builtin { argv: [Tk { tk_type: Ident, wd: WordDesc { text: \"echo\", span: Span { start: 0, end: 4 }, flags: WdFlags(BUILTIN) } }, Tk { tk_type: String, wd: WordDesc { text: \"hello\", span: Span { start: 5, end: 10 }, flags: WdFlags(IS_ARG) } }, Tk { tk_type: String, wd: WordDesc { text: \"world\", span: Span { start: 11, end: 16 }, flags: WdFlags(IS_ARG) } }] }, span: Span { start: 0, end: 16 }, flags: NdFlags(VALID_OPERAND), redirs: [Node { command: None, nd_type: Redirection { redir: Redir { fd_source: 1, op: Output, fd_target: None, file_target: Some(Tk { tk_type: String, wd: WordDesc { text: \"file.txt\", span: Span { start: 19, end: 27 }, flags: WdFlags(IS_ARG) } }) } }, span: Span { start: 17, end: 18 }, flags: NdFlags(IS_OP), redirs: [] }] }]";
		let debug_result = format!("{:?}",flat_nodes);
		assert_eq!(debug_result,expected)
	}

	#[test]
	fn parser_redirection_complex() {
		let input = "cat nonexistantfile.txt 1>&2 2> file.txt";
		let mut tokenizer = LashTokenizer::new(input);

		let mut flat_nodes = Vec::new();
		loop {
			let state = descend(&mut tokenizer).unwrap();
			if state.tokens.is_empty() {
				break;
			}

			if let NdType::Root { deck } = state.ast.nd_type {
				flat_nodes.extend(deck);
			} else {
				flat_nodes.push(state.ast);
			}
		}

		let expected = "[Node { command: Some(Tk { tk_type: Ident, wd: WordDesc { text: \"cat\", span: Span { start: 0, end: 3 }, flags: WdFlags(0x0) } }), nd_type: Command { argv: [Tk { tk_type: Ident, wd: WordDesc { text: \"cat\", span: Span { start: 0, end: 3 }, flags: WdFlags(0x0) } }, Tk { tk_type: String, wd: WordDesc { text: \"nonexistantfile.txt\", span: Span { start: 4, end: 23 }, flags: WdFlags(IS_ARG) } }] }, span: Span { start: 0, end: 23 }, flags: NdFlags(VALID_OPERAND), redirs: [Node { command: None, nd_type: Redirection { redir: Redir { fd_source: 1, op: Output, fd_target: Some(2), file_target: None } }, span: Span { start: 24, end: 28 }, flags: NdFlags(IS_OP), redirs: [] }, Node { command: None, nd_type: Redirection { redir: Redir { fd_source: 2, op: Output, fd_target: None, file_target: Some(Tk { tk_type: String, wd: WordDesc { text: \"file.txt\", span: Span { start: 32, end: 40 }, flags: WdFlags(IS_ARG) } }) } }, span: Span { start: 29, end: 31 }, flags: NdFlags(IS_OP), redirs: [] }] }]";
		let debug_result = format!("{:?}",flat_nodes);
		assert_eq!(debug_result,expected)
	}

	#[test]
	fn parser_quoted_args() {
		let input = "echo \"hello world\"";
		let mut tokenizer = LashTokenizer::new(input);

		let mut flat_nodes = Vec::new();
		loop {
			let state = descend(&mut tokenizer).unwrap();
			if state.tokens.is_empty() {
				break;
			}

			if let NdType::Root { deck } = state.ast.nd_type {
				flat_nodes.extend(deck);
			} else {
				flat_nodes.push(state.ast);
			}
		}

		let expected = "[Node { command: Some(Tk { tk_type: Ident, wd: WordDesc { text: \"echo\", span: Span { start: 0, end: 4 }, flags: WdFlags(BUILTIN) } }), nd_type: Builtin { argv: [Tk { tk_type: Ident, wd: WordDesc { text: \"echo\", span: Span { start: 0, end: 4 }, flags: WdFlags(BUILTIN) } }, Tk { tk_type: String, wd: WordDesc { text: \"hello world\", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }] }, span: Span { start: 0, end: 0 }, flags: NdFlags(VALID_OPERAND), redirs: [] }]";
		let debug_result = format!("{:?}",flat_nodes);
		assert_eq!(debug_result,expected)
	}

	#[test]
	fn for_loop() {
		let input = "for i in 1 2 3; do echo $i; done";
		let mut tokenizer = LashTokenizer::new(input);

		let mut flat_nodes = Vec::new();
		loop {
			let state = descend(&mut tokenizer).unwrap();
			if state.tokens.is_empty() {
				break;
			}

			if let NdType::Root { deck } = state.ast.nd_type {
				flat_nodes.extend(deck);
			} else {
				flat_nodes.push(state.ast);
			}
		}

		let expected = "[Node { command: None, nd_type: For { loop_vars: [Tk { tk_type: Ident, wd: WordDesc { text: \"i\", span: Span { start: 4, end: 5 }, flags: WdFlags(0x0) } }], loop_arr: [Tk { tk_type: Ident, wd: WordDesc { text: \"1\", span: Span { start: 9, end: 10 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \"2\", span: Span { start: 11, end: 12 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \" \", span: Span { start: 0, end: 0 }, flags: WdFlags(0x0) } }, Tk { tk_type: Ident, wd: WordDesc { text: \"3\", span: Span { start: 13, end: 14 }, flags: WdFlags(0x0) } }], loop_body: Node { command: None, nd_type: LoopBody { body: \" echo ; \" }, span: Span { start: 0, end: 0 }, flags: NdFlags(0x0), redirs: [] } }, span: Span { start: 0, end: 0 }, flags: NdFlags(VALID_OPERAND | FOR_BODY), redirs: [] }]";
		let debug_result = format!("{:?}",flat_nodes);
		assert_eq!(debug_result,expected)
	}

	#[test]
	fn parser_while_loop() {
		let input = "while true; do echo hi; done";
		let mut tokenizer = LashTokenizer::new(input);

		let mut flat_nodes = Vec::new();
		loop {
			let state = descend(&mut tokenizer).unwrap();
			if state.tokens.is_empty() {
				break;
			}

			if let NdType::Root { deck } = state.ast.nd_type {
				flat_nodes.extend(deck);
			} else {
				flat_nodes.push(state.ast);
			}
		}

		let expected = "[Node { command: None, nd_type: Loop { condition: true, logic: Conditional { condition: Node { command: None, nd_type: LoopCond { cond: \"true; \" }, span: Span { start: 0, end: 0 }, flags: NdFlags(0x0), redirs: [] }, body: Node { command: None, nd_type: LoopBody { body: \" echo hi; \" }, span: Span { start: 0, end: 0 }, flags: NdFlags(0x0), redirs: [] } } }, span: Span { start: 0, end: 0 }, flags: NdFlags(VALID_OPERAND), redirs: [] }]";
		let debug_result = format!("{:?}",flat_nodes);
		assert_eq!(debug_result,expected)
	}

	#[test]
	fn parser_until_loop() {
		let input = "until true; do echo waiting; done";
		let mut tokenizer = LashTokenizer::new(input);

		let mut flat_nodes = Vec::new();
		loop {
			let state = descend(&mut tokenizer).unwrap();
			if state.tokens.is_empty() {
				break;
			}

			if let NdType::Root { deck } = state.ast.nd_type {
				flat_nodes.extend(deck);
			} else {
				flat_nodes.push(state.ast);
			}
		}

		let expected = "[Node { command: None, nd_type: Loop { condition: false, logic: Conditional { condition: Node { command: None, nd_type: LoopCond { cond: \"true; \" }, span: Span { start: 0, end: 0 }, flags: NdFlags(0x0), redirs: [] }, body: Node { command: None, nd_type: LoopBody { body: \" echo waiting; \" }, span: Span { start: 0, end: 0 }, flags: NdFlags(0x0), redirs: [] } } }, span: Span { start: 0, end: 0 }, flags: NdFlags(VALID_OPERAND), redirs: [] }]";
		let debug_result = format!("{:?}",flat_nodes);
		assert_eq!(debug_result,expected)
	}

	#[test]
	fn parser_if_statement() {
		let input = "if true; then echo hi; else echo bye; fi";
		let mut tokenizer = LashTokenizer::new(input);

		let mut flat_nodes = Vec::new();
		loop {
			let state = descend(&mut tokenizer).unwrap();
			if state.tokens.is_empty() {
				break;
			}

			if let NdType::Root { deck } = state.ast.nd_type {
				flat_nodes.extend(deck);
			} else {
				flat_nodes.push(state.ast);
			}
		}

		let expected = "[Node { command: None, nd_type: If { cond_blocks: [Conditional { condition: Node { command: None, nd_type: Root { deck: [Node { command: Some(Tk { tk_type: Ident, wd: WordDesc { text: \"true\", span: Span { start: 3, end: 7 }, flags: WdFlags(0x0) } }), nd_type: Command { argv: [Tk { tk_type: Ident, wd: WordDesc { text: \"true\", span: Span { start: 3, end: 7 }, flags: WdFlags(0x0) } }] }, span: Span { start: 3, end: 7 }, flags: NdFlags(VALID_OPERAND), redirs: [] }] }, span: Span { start: 3, end: 0 }, flags: NdFlags(0x0), redirs: [] }, body: Node { command: None, nd_type: Root { deck: [Node { command: Some(Tk { tk_type: Ident, wd: WordDesc { text: \"echo\", span: Span { start: 14, end: 18 }, flags: WdFlags(BUILTIN) } }), nd_type: Builtin { argv: [Tk { tk_type: Ident, wd: WordDesc { text: \"echo\", span: Span { start: 14, end: 18 }, flags: WdFlags(BUILTIN) } }, Tk { tk_type: String, wd: WordDesc { text: \"hi\", span: Span { start: 19, end: 21 }, flags: WdFlags(IS_ARG) } }] }, span: Span { start: 14, end: 21 }, flags: NdFlags(VALID_OPERAND), redirs: [] }] }, span: Span { start: 14, end: 0 }, flags: NdFlags(0x0), redirs: [] } }], else_block: Some(Node { command: None, nd_type: Root { deck: [Node { command: Some(Tk { tk_type: Ident, wd: WordDesc { text: \"echo\", span: Span { start: 28, end: 32 }, flags: WdFlags(BUILTIN) } }), nd_type: Builtin { argv: [Tk { tk_type: Ident, wd: WordDesc { text: \"echo\", span: Span { start: 28, end: 32 }, flags: WdFlags(BUILTIN) } }, Tk { tk_type: String, wd: WordDesc { text: \"bye\", span: Span { start: 33, end: 36 }, flags: WdFlags(IS_ARG) } }] }, span: Span { start: 28, end: 36 }, flags: NdFlags(VALID_OPERAND), redirs: [] }] }, span: Span { start: 28, end: 0 }, flags: NdFlags(0x0), redirs: [] }) }, span: Span { start: 0, end: 40 }, flags: NdFlags(VALID_OPERAND), redirs: [] }]";
		let debug_result = format!("{:?}",flat_nodes);
		assert_eq!(debug_result,expected)
	}
	/*
	#[test]
	fn parser_final_boss() {
		let input = "if while while while echo; do while echo; do while echo; do echo; done; done; done; do until until until echo; do echo; done; do for i in 1 2 3; do echo; done; done; do for i in 1 2 3; do if echo; then echo; fi; done; done; done; do until for i in 1 2 3; do if while echo; do echo; done; then until echo; do echo; done; fi; done; do if while for i in 1 2 3; do echo; done; do if echo; then echo; fi; done; then until while echo; do echo; done; do until echo; do echo; done; done; fi; done; done; then until for i in 1 2 3; do while for i in 1 2 3; do for i in 1 2 3; do echo; done; done; do if if echo; then echo; fi; then while echo; do echo; done; fi; done; done; do if until while until echo; do echo; done; do for i in 1 2 3; do echo; done; done; do until if echo; then echo; fi; do while echo; do echo; done; done; done; then for i in 1 2 3; do for i in 1 2 3; do until echo; do echo; done; done; done; fi; done; elif for i in 1 2 3; do if if if for i in 1 2 3; do echo; done; then if echo; then echo; fi; fi; then while while echo; do echo; done; do until echo; do echo; done; done; fi; then while until for i in 1 2 3; do echo; done; do if echo; then echo; fi; done; do for i in 1 2 3; do while echo; do echo; done; done; done; fi; done; then if while until if until echo; do echo; done; then for i in 1 2 3; do echo; done; fi; do while if echo; then echo; fi; do while echo; do echo; done; done; done; do for i in 1 2 3; do until until echo; do echo; done; do for i in 1 2 3; do echo; done; done; done; done; then until if for i in 1 2 3; do if echo; then echo; fi; done; then if while echo; do echo; done; then until echo; do echo; done; fi; fi; do while while for i in 1 2 3; do echo; done; do if echo; then echo; fi; done; do until while echo; do echo; done; do until echo; do echo; done; done; done; done; fi; else for i in 1 2 3; do until for i in 1 2 3; do for i in 1 2 3; do echo; done; done; do if if echo; then echo; fi; then while echo; do echo; done; fi; done; done; fi";
		let mut tokenizer = LashTokenizer::new(input);

		let mut flat_nodes = Vec::new();
		loop {
			let state = descend(&mut tokenizer).unwrap();
			if state.tokens.is_empty() {
				break;
			}

			if let NdType::Root { deck } = state.ast.nd_type {
				flat_nodes.extend(deck);
			} else {
				flat_nodes.push(state.ast);
			}
		}

		let expected = "";
		let debug_result = format!("{:?}",flat_nodes);
		assert_eq!(debug_result,expected)
	}
	*/
}

#[cfg(test)]
pub mod exec_tests {
}

#[cfg(test)]
pub mod expand_tests {
}

#[cfg(test)]
pub mod helper_tests {
}

#[cfg(test)]
pub mod shellenv_tests {
}

#[cfg(test)]
pub mod shopt_tests {
}

#[cfg(test)]
pub mod comp_tests {
}

#[cfg(test)]
pub mod signal_tests {
}

#[cfg(test)]
pub mod prompt_tests {
}
