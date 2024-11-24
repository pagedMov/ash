mod command;
mod parser;
mod prompt;

fn main() {
    env_logger::init();
    loop {
        let input = prompt::prompt();
        let tokens = parser::tokenize(input.as_str());
        println!("Tokens: {:?}", tokens);

        let mut parser = parser::Parser::new(tokens);
        let ast = parser.parse_input();
        match ast {
            Ok(ast) => {
                println!("AST: {:#?}", ast);
                command::node_walk(ast)
            }
            Err(e) => {
                println!("{}",e);
            }
        }
    }
}
