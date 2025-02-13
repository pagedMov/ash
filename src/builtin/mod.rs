pub mod alias;
pub mod assign;
pub mod cd;
pub mod echo;
pub mod export;
pub mod fg;
pub mod opts;
pub mod dir_stack;
pub mod pwd;
pub mod source;
pub mod test;
pub mod control;
pub mod job;
pub mod cmd_override;
pub mod exec;

pub const BUILTINS: [&str; 43] = [
	"try", "except", "return", "break", "continue", "exit", "command", "pushd", "popd", "setopt", "getopt", "type", "string", "int", "bool", "arr", "float", "dict", "expr", "echo", "jobs", "unset", "fg", "bg", "set", "builtin", "test", "[", "shift", "unalias", "alias", "export", "cd", "readonly", "declare", "local", "unset", "trap", "node", "exec", "source", "read_func", "wait",
];
