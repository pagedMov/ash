mod command;
mod parser;
mod prompt;

fn main() {
    env_logger::init();
    loop {
        let input = prompt::prompt();
        let words = parser::tokenize_input(input);
        let commands = parser::parse_tokens(words);
        for command in commands {
            command::exec_command(None, command);
        }
    }
}
