mod command;
mod parser;
mod prompt;
mod environment;
use log::debug;

fn main() {
    env_logger::init();
    let mut environ = environment::Environment::new();
    loop {
        let input = prompt::prompt();
        let tokens = parser::tokenize(input.as_str(),&environ);
        println!("Tokens: {:?}", tokens);

        let mut parser = parser::Parser::new(tokens);
        let ast = parser.parse_input();
        match ast {
            Ok(ast) => {
                debug!("AST: {:#?}", ast);
                command::node_walk(ast)
            }
            Err(e) => {
                println!("{}",e);
            }
        }
    }
}
