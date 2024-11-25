pub mod command;
pub mod parser;
pub mod prompt;
pub mod environment;
pub mod helper;
use log::{info, debug};

fn main() {
    env_logger::init();
    let mut environ = environment::Environment::new();
    loop {
        let input = prompt::prompt();
        let tokens = parser::tokenize(input.as_str(),&environ);
        info!("Tokens: {:?}", tokens);

        let mut parser = parser::Parser::new(tokens);
        let ast = parser.parse_input(&mut environ);
        match ast {
            Ok(ast) => {
                debug!("AST: {:#?}", ast);
                match command::node_walk(ast, &mut environ) {
                    Ok(_) => {
                        environ.set_var("?","0");
                    }
                    Err(e) => {
                        environ.set_var("?", &e.0.to_string());
                        println!("{}",e.1);
                    }
                }
            }
            Err(e) => {
                println!("{}",e);
            }
        }
    }
}
