use crate::{prelude::*, utils};

pub const ARG_RULES: [Rule; 2] = [Rule::arg_assign,Rule::word];

pub trait Rules {
	fn matches(&self, rule: Rule) -> bool;
}

impl Rules for Rule {
	fn matches(&self, rule: Rule) -> bool {
		rule == *self
	}
}

impl Rules for &[Rule] {
	fn matches(&self, rule: Rule) -> bool {
		self.contains(&rule)
	}
}

pub trait OptPairExt<'a> {
	fn unpack(self) -> SlashResult<Pair<'a,Rule>>;
}

impl<'a> OptPairExt<'a> for Option<Pair<'a,Rule>> {
	/// There are many places in the slash codebase where we can be reasonably certain that an Option<Pair> will be Some
	/// However, if we are wrong for whatever reason, it's probably better to not crash the program by calling unwrap()
	///
	/// This function is essentially a safe unwrap that returns our error type instead of panicking
	#[track_caller]
	fn unpack(self) -> SlashResult<Pair<'a,Rule>> {
		if let Some(pair) = self {
			Ok(pair)
		} else {
			dbg!(std::panic::Location::caller());
			Err(Low(SlashErrLow::InternalErr("Called unpack() on a None value".into())))
		}
	}
}

pub trait PairExt<'a> {
	fn to_vec(self) -> Vec<Pair<'a,Rule>>;
	fn to_deque(self) -> VecDeque<Pair<'a,Rule>>;
	fn contains_rules<R: Rules>(&self, rule: R) -> bool;
	fn process_args(&self, slash: &mut Slash) -> Vec<String>;
	fn filter<R: Rules>(&self, rules: R) -> VecDeque<Pair<'a,Rule>>;
	fn seek_all<R: Rules>(&self, rules: R) -> VecDeque<Pair<'a,Rule>>;
	fn scry<R: Rules>(&self, rules: R) -> Option<Pair<'a,Rule>>;
	fn step(self, count: usize) -> Option<Pair<'a,Rule>>;
}

impl<'a> PairExt<'a> for Pair<'a,Rule> {
	/// Goes straight down, a certain number of times.
	/// Useful for rules like `esc_sequence > esc_vis_grp > esc_sequence > esc_runtime: "\\D"` which have a long chain of single rules to descend through
	fn step(self, count: usize) -> Option<Pair<'a, Rule>> {
		let mut current = self;

		for _ in 0..count {
			current = current.into_inner().next()?; // Go one level deeper
		}

		Some(current)
	}
	/// Filter inner pairs by a rule type
	fn filter<R: Rules>(&self, rules: R) -> VecDeque<Pair<'a,Rule>> {
		self.clone().into_inner().filter(|pr| rules.matches(pr.as_rule())).collect::<VecDeque<_>>()
	}
	fn seek_all<R: Rules>(&self, rules: R) -> VecDeque<Pair<'a,Rule>> {
		let mut matches = VecDeque::new();
		let mut stack = self.clone().to_deque();
		while let Some(pair) = stack.pop_front() {
			if rules.matches(pair.as_rule()) {
				matches.push_front(pair);
			} else {
				let pair_inner = pair.to_deque();
				stack.extend(pair_inner);
			}
		}
		matches
	}
	/// Traverse the pair and return the first pair that contains the given rule
	fn scry<R: Rules>(&self, rules: R) -> Option<Pair<'a,Rule>> {
		let mut stack = self.clone().to_deque();
		while let Some(pair) = stack.pop_front() {
			if rules.matches(pair.as_rule()) {
				return Some(pair)
			} else {
				let pair_inner = pair.to_deque();
				stack.extend(pair_inner);
			}
		}
		None
	}
	fn to_vec(self) -> Vec<Pair<'a,Rule>> {
		self.into_inner().collect::<Vec<_>>()
	}
	fn to_deque(self) -> VecDeque<Pair<'a,Rule>> {
		self.into_inner().collect::<VecDeque<_>>()
	}
	/// Automatically process command arguments, sorting words and redirections
	fn process_args(&self, slash: &mut Slash) -> Vec<String> {
		let mut argv = vec![];
		if self.as_rule() != Rule::simple_cmd {
			return argv
		}
		let inner = self.clone().into_inner();
		for arg in inner {
			match arg.as_rule() {
				Rule::word | Rule::cmd_name | Rule::arg_assign => argv.push(arg.as_str().trim_quotes()),
				Rule::redir => slash.ctx_mut().push_redir(utils::Redir::from_pair(arg).unwrap()),
				_ => unreachable!("Unexpected rule: {:?}",arg.as_rule())
			}
		}
		argv
	}
	fn contains_rules<R: Rules>(&self, rules: R) -> bool {
	  let clone = self.clone();
		let mut stack = clone.to_deque();
		while let Some(pair) = stack.pop_front() {
			if rules.matches(pair.as_rule()) {
				return true;
			}
			let inner = pair.to_deque();
			if !inner.is_empty() {
				stack.extend(inner);
			}
		}
		false
	}
}

#[derive(pest_derive::Parser)]
#[grammar_inline = r##"
// Helper rules
WHITESPACE        = _{ " " | "\t" }
COMMENT           = _{ !"#!" ~ "#" ~ (!(NEWLINE | "#") ~ ANY)* }
number            =  { ASCII_DIGIT+ }
parameter         =  { "#" | ASCII_DIGIT+ | "@" | "*" | "?" | "$" | "!" | "_" | "-" }
glob_brackets     = @{ !"\\[" ~ "[" ~ (!("]" | "," | WHITESPACE | NEWLINE) ~ ANY)* ~ "]" }
glob_pat          =  { (!"\\?" ~ "?") | (!"\\*" ~ "*") | glob_brackets }
alpha_range_upper = @{ ASCII_ALPHA_UPPER ~ ".." ~ ASCII_ALPHA_UPPER }
alpha_range_lower = @{ ASCII_ALPHA_LOWER ~ ".." ~ ASCII_ALPHA_LOWER }
num_range         = @{ ASCII_DIGIT ~ ".." ~ ASCII_DIGIT }
brace_list        = @{ !"," ~ (brace_word | ident) ~ ("," ~ !"," ~ (brace_word | ident))* }
brace_expand      = @{ "{" ~ (alpha_range_upper | alpha_range_lower | num_range | brace_list) ~ "}" }
path_seg          = @{ path_root | path_rel }
path_root         =  { ("/" ~ ident)+ }
path_rel          =  { (ident ~ "/")+ }
reserved          =  @{ ("if" | "for" | "while" | "do" | "done" | "fi" | "in" | "select" | "match") ~ word_bound }

// in case you need to explicitly mark where a word ends
// necessary with shell constructs, for some reason
word_bound = _{ !ASCII_ALPHANUMERIC }

// Rules for the expansion phase of the parsing
operator = _{
    WHITESPACE* ~ ("|" | "&&" | "|&" | ">" | "<" | "<<" | "<<<" | ">>" | ">|" | ">>|" | "<>" | "&") ~ WHITESPACE*
}
// Just finds all of the words. Probably fails to certain edge cases, but works for now
find_expansions =  { (WHITESPACE | NEWLINE | assignment | word | operator)* }
glob_word       = @{ ident? ~ glob_pat+ ~ ident? }
tilde_sub       = @{ pwd | prev_pwd | (home ~ path_seg?) }
brace_word      = @{ ident? ~ brace_expand+ ~ ident? }
var_sub         = @{ !"\\$" ~ "$" ~ var_ident }
index           =  { ASCII_DIGIT+ }
slice           = ${ index ~ ".." ~ index }
key             =  { dquoted | squoted }
arr_index       = @{ !"\\$" ~ "$" ~ var_ident ~ ("[" ~ (key | slice | index) ~ "]")+ }
cmd_sub         = @{ !"\\$" ~ "$(" ~ subsh_body ~ ")" }
param_sub       = @{ !"\\$" ~ "$" ~ parameter }
expansion       =  {
    tilde_sub
  | brace_word
  | var_sub
  | arr_index
  | cmd_sub
  | param_sub
}

// A silent version for the first pass
// A loud version for extracting the exact expansion types
expand_word = @{ (ident? ~ expansion+ ~ ident?)+ }
expand_word_loud = { ((ident? ~ NEWLINE*) ~ expansion+ ~ (NEWLINE* ~ ident?))+ }

// Types of generic words
dqt      = _{ "\"" }
sqt      = _{ "'" }
home     =  { "~" }
pwd      =  { "~+" }
prev_pwd =  { "~-" }

dquote_body        =  { ("\\\"" | !"\"" ~ ANY)* }
squote_body        =  { ("\\'" | !"'" ~ ANY)* }
dquoted            =  { dqt ~ dquote_body ~ dqt }
squoted            =  { sqt ~ squote_body ~ sqt }
var_ident_plain    = @{ NEWLINE* ~ !parameter ~ ASCII_ALPHA ~ (ASCII_ALPHANUMERIC | "." | "_")* }
var_ident_brackets = @{ !"\\{" ~ "{" ~ var_ident_plain ~ !"\\}" ~ "}" }
var_ident          =  { var_ident_brackets | var_ident_plain }
ident              = _{
	"[" |
    "]" |
    (("\\" ~ ANY) | // 'out' and 'in' refer to redir operators '>' and '<'
  	(!out ~ !in ~ ASCII_ALPHANUMERIC | "\"" | "'" | "[" | "]" | "*" | "?" | "_" | "-" | "!" | "%" | "+" | "=" | "\\" | "/" | "," | "." | ":" | "@"))+
}
cmd_name           = @{ word }
word               = ${
    dquoted
  | squoted
  | expand_word
  | ident
}
array_elem         =  { array | (("\\," | "\\]" | "\\[") | !("[" | "]" | ",") ~ ANY)+ }
array              =  { "[" ~ (array_elem ~ ("," ~ array_elem)*)? ~ "]" }
word_list          =  { word ~ (NEWLINE* ~ word)* }
std_assign         =  { var_ident ~ "=" ~ word? ~ (!sep ~ cmd_list)? }
plus_assign        =  { var_ident ~ "+=" ~ word? ~ (!sep ~ cmd_list)? }
increment          =  @{ var_ident ~ "++" ~ (!sep ~ WHITESPACE+ ~ cmd_list)? }
decrement          =  @{ var_ident ~ "--" ~ (!sep ~ WHITESPACE+ ~ cmd_list)? }
minus_assign       =  { var_ident ~ "-=" ~ word? ~ (!sep ~ cmd_list)? }
assignment         =  { increment | decrement | std_assign | plus_assign | minus_assign }
arg_assign         =  { var_ident ~ "=" ~ (array|word)? }
sep                = _{ (";" | NEWLINE)+ }

// Types of commands
cmd_list   =  { (bg_cmd | expr) ~ (#op = op ~ (bg_cmd | expr))* }
simple_cmd =  { !reserved ~ (redir | cmd_name) ~ (arg_assign | word | redir)* }
bg_cmd     =  { expr ~ !"&&" ~ "&" ~ word_bound }
pipeline   =  { (shell_cmd | simple_cmd) ~ ("|" ~ (shell_cmd | simple_cmd))+ }
expr       = _{ pipeline | shell_cmd | assignment | simple_cmd }
shell_cmd  =  {
    (for_cmd | match_cmd | loop_cmd | if_cmd | subshell | brace_grp | assignment | func_def) ~ redir*
}


subshebang = @{ "#!" ~ (!NEWLINE ~ ANY)+ ~ NEWLINE }
subsh_body = @{ (nested | non_paren)+ }
nested     = _{ "(" ~ subsh_body* ~ ")"? }
non_paren  = _{ (!"(" ~ !")" ~ ANY)+ }
subshell   =  { "(" ~ subshebang? ~ subsh_body ~ ")" ~ (redir | (arg_assign | word | redir))* }
proc_sub   =  { (in | out) ~ "(" ~ subsh_body ~ ")" }

if_cond   = { cmd_list }
loop_cond = { cmd_list }
if_body   = { (!("fi" | "elif" | "else") ~ cmd_list ~ sep)+ }
loop_body = { (!"done" ~ cmd_list ~ sep)+ }

loop_kind = { "while" | "until" }
loop_cmd  = { loop_kind ~ NEWLINE* ~ loop_cond ~ sep ~ "do" ~ NEWLINE* ~ loop_body ~ "done" }

for_vars = { (!"in" ~ word ~ NEWLINE*)+ }
for_arr  = { (word ~ NEWLINE*)+ }
for_cmd  = { "for" ~ NEWLINE* ~ for_vars ~ in ~ NEWLINE* ~ for_arr+ ~ sep ~ "do" ~ NEWLINE* ~ loop_body ~ NEWLINE* ~ "done" ~ word_bound }

match_pat  = { (!"=>" ~ word)+ }
match_body = { (brace_grp ~ ","? | (!"," ~ ANY)+ ~ ",") }
match_arm  = { match_pat ~ "=>" ~ NEWLINE* ~ match_body }
match_cmd  = {
		"match" ~ NEWLINE* ~ word ~ NEWLINE* ~ in ~ NEWLINE* ~ match_arm ~ (NEWLINE* ~ match_arm)* ~ NEWLINE* ~ "done" ~ word_bound
}

if_cmd     = { "if" ~ NEWLINE* ~ if_cond ~ sep ~ "then" ~ NEWLINE* ~ if_body ~ elif_block* ~ else_block? ~ "fi" ~ word_bound }
elif_block = { "elif" ~ NEWLINE* ~ if_cond ~ sep ~ "then" ~ NEWLINE* ~ if_body }
else_block = { "else" ~ NEWLINE* ~ (!("fi") ~ #else_body = cmd_list ~ sep)+ }

// Operator stuff
and = { "&&" }
or  = { "||" }
op  = { (and | or) }

redir_list =  { redir ~ (redir)* }
fd_out     = @{ number }
fd_target  = @{ number }
file       = { proc_sub|word }
pipe       =  { "|" }
in         =  { "<" }
out        =  { ">" }
force_out  =  { ">|" }
in_out     =  { "<>" }
append     =  { ">>" }
heredoc    =  { "<<" }
herestring =  { "<<<" }
redir      =  {
    (out ~ file)
  | (in ~ file)
  | (fd_out ~ in ~ file)
  | (fd_out ~ out ~ file)
  | (append ~ file)
  | (fd_out ~ append ~ file)
  | (heredoc ~ file)
  | (herestring ~ file)
  | (fd_out ~ heredoc ~ file)
  | (in ~ "&" ~ fd_target)
  | (fd_out ~ in ~ "&" ~ fd_target)
  | (out ~ "&" ~ fd_target)
  | (fd_out ~ out ~ "&" ~ fd_target)
  | (out ~ "&" ~ "-")
  | (fd_out ~ out ~ "&" ~ "-")
  | (fd_out ~ out ~ "&" ~ "-")
  | ("&" ~ out ~ file)
  | (fd_out ~ in_out ~ file)
  | (in_out ~ file)
  | (force_out ~ file)
  | (fd_out ~ force_out ~ file)
}

// Prompt Parsing
prompt             =  { (raw_text | esc_sequence)* }
raw_text           = _{ WHITESPACE* ~ (!"\\" ~ ANY)+ ~ WHITESPACE* }
ascii_oct_seq      =  { ASCII_OCT_DIGIT ~ ASCII_OCT_DIGIT? ~ ASCII_OCT_DIGIT? }
ansi_params        =  { ASCII_DIGIT+ ~ (";" ~ ASCII_DIGIT+)* }
custom_esc_seg     =  { (!("." | "\\{" | "\\}") ~ ANY)+ }
custom_esc_path    = @{ custom_esc_seg ~ ("." ~ custom_esc_seg)* }
esc_sequence       =  {
    esc_bell
  | esc_vis_grp
  | esc_user_seq
  | esc_ansi_seq
  | esc_ascii_oct
  | esc_12hour_short
  | esc_24hour_short
  | esc_runtime
  | esc_12hour
  | esc_24hour
  | esc_weekday
  | esc_dquote
  | esc_squote
  | esc_return
  | esc_newline
  | esc_pwd
  | esc_pwd_short
  | esc_hostname
  | esc_hostname_short
  | esc_shellname
  | esc_username
  | esc_prompt_symbol
  | esc_exit_code
  | esc_success_symbol
  | esc_failure_symbol
}
esc_pwd            =  { "\\w" }
esc_pwd_short      =  { "\\W" }
esc_hostname       =  { "\\H" }
esc_hostname_short =  { "\\h" }
esc_shellname      =  { "\\s" }
esc_username       =  { "\\u" }
esc_prompt_symbol  = @{ "\\$" }
esc_exit_code      =  { "\\?" }
esc_success_symbol =  { "\\S" }
esc_failure_symbol =  { "\\F" }
esc_bell           =  { "\\a" }
esc_newline        =  { "\\n" }
esc_return         =  { "\\r" }
esc_backsslash      =  { "\\" }
esc_squote         =  { "'" }
esc_dquote         =  { "\"" }
esc_weekday        =  { "\\d" }
esc_24hour         =  { "\\t" }
esc_12hour         =  { "\\T" }
esc_runtime        =  { "\\D" }
esc_24hour_short   =  { "\\A" }
esc_12hour_short   =  { "\\@" }
esc_ascii_oct      = ${ "\\" ~ ascii_oct_seq }
esc_ansi_seq       =  { "\\e" ~ "[" ~ ansi_params ~ ASCII_ALPHA }
esc_user_seq       = ${ "\\{" ~ custom_esc_path ~ "\\}" }
esc_vis_grp        =  { "\\(" ~ (raw_text | esc_sequence)* ~ "\\)" }

// Syntax Highlighting

for           = @{ "for" ~ word_bound }
var           =  { !loud_operator ~ word }
for_with_vars =  { for ~ (!"in" ~ var)+ }
in_kw         =  { WHITESPACE* ~ "in" ~ WHITESPACE* }
in_with_vars  =  { in_kw ~ var+ }
for_with_in   =  { (for_with_vars | for) ~ (in_with_vars | in_kw) }
if            = @{ "if" ~ word_bound }
then          = @{ "then" ~ word_bound }
else          = @{ "else" ~ word_bound }
elif          = @{ "elif" ~ word_bound }
fi            = @{ "fi" ~ word_bound }
match         = @{ "match" ~ word_bound }
select        = @{ "select" ~ word_bound }
until         = @{ "until" ~ word_bound }
while         = @{ "while" ~ word_bound }
do            = @{ "do" ~ word_bound }
done          = @{ "done" ~ word_bound }
hl_assign     =  { var_ident ~ "=" ~ word? }
hl_subshell   =  { "(" ~ subshebang? ~ subsh_body ~ ")" }

loud_sep = { (";" | NEWLINE)+ }
// Loud because the rule is not silent

loud_ident = @{
    (("\\" ~ ANY) | // 'out' and 'in' refer to redir operators '>' and '<'
  (!out ~ !in ~ ASCII_ALPHANUMERIC | "\"" | "'" | "_" | "-" | "!" | "%" | "+" | "=" | "\\" | "/" | "," | "." | ":" | "@"))+
}

loud_operator = {
    WHITESPACE* ~ ("|" | "{" | // Brackets are here to delimit brace groups
  "}" | "=>" | "&&" | "|&" | ">" | "=" | "<" | "<<" | "<<<" | ">>" | ">|" | ">>|" | "<>" | "&") ~ WHITESPACE*
}

out_to_fd  = { out ~ "&" }
in_from_fd = { in ~ "&" }
combine    = { "&" ~ out }
close_fd   = { "&" ~ "-" }

hl_redir = {
    (out ~ file)
  | (in ~ file)
  | (fd_out ~ in ~ file)
  | (fd_out ~ out ~ file)
  | (append ~ file)
  | (fd_out ~ append ~ file)
  | (heredoc ~ file)
  | (herestring ~ file)
  | (fd_out ~ heredoc ~ file)
  | (in_from_fd ~ fd_target)
  | (fd_out ~ in_from_fd ~ fd_target)
  | (out_to_fd ~ fd_target)
  | (fd_out ~ out_to_fd ~ fd_target)
  | (out ~ close_fd)
  | (fd_out ~ out ~ close_fd)
  | (fd_out ~ in ~ close_fd)
  | (combine ~ file)
  | (fd_out ~ in_out ~ file)
  | (in_out ~ file)
  | (force_out ~ file)
  | (fd_out ~ force_out ~ file)
}

hl_word = ${
    dquoted
  | squoted
  | param_sub
  | arr_index
  | var_sub
  | cmd_sub
  | proc_sub
  | tilde_sub
  | ident
}

shell_struct = {
    for_with_in
  | for_with_vars
  | for
  | in_with_vars
  | in_kw
  | if
  | then
  | else
  | elif
  | fi
  | match
  | select
  | until
  | while
  | done
  | do
  | hl_subshell
  | hl_assign
  | func_name
}

glob_opt      = { !"\\?" ~ "?" }
glob_wild     = { !"\\*" ~ "*" }
hl_globs      = { glob_opt | glob_wild | glob_brackets }
hl_glob       = ${ (loud_ident? ~ hl_globs+ ~ loud_ident?)+ }
hl_brace_word = { loud_ident? ~ brace_expand+ ~ loud_ident? }

hl_brace_grp = { "{" ~ syntax_hl ~ "}" }

words = { (!shell_struct ~ (hl_redir | hl_brace_word | hl_glob | hl_word))+ }

syntax_hl = { (loud_sep | shell_struct | loud_operator | words)* }

// Misc rules
func_name = @{ word ~ "()" }
func_def  =  {
    (func_name ~ NEWLINE* ~ brace_grp)
  | ("fn" ~ (func_name | word) ~ NEWLINE* ~ brace_grp)
}

brace_grp = { "{" ~ sub_main ~ "}" }

// sub_main is used for brace groups, it essentially allows for holding a script in the brace group
// It's stored as a static string, but using sub_main makes sure that it actually parses first
sub_main = _{ NEWLINE* ~ cmd_list? ~ (sep ~ cmd_list?)* ~ NEWLINE* }
main     =  { SOI ~ NEWLINE* ~ cmd_list? ~ (sep ~ cmd_list?)* ~ NEWLINE* ~ EOI }
"##]
pub struct SlashParse;
