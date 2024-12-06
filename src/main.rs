//pub mod event;
//pub mod prompt;
//pub mod parser;
//mod rsh;
//
//use log::debug;
//
//use tokio::io::{self, AsyncBufReadExt, BufReader};
//use tokio_stream::{wrappers::LinesStream, StreamExt};
//
//use crate::event::EventLoop;
//
//#[tokio::main]
//async fn main() {
  //env_logger::init();
  //let mut event_loop = EventLoop::new();
//
  //debug!("Starting event loop");
   //TODO: use the value returned by this for something
  //let _ = event_loop.listen().await;
//}

pub mod parser;
pub mod event;
pub mod prompt;
mod rsh;

use std::fs;


fn main() {
    env_logger::init();
    let input = "if echo \"First condition\"; if echo \"Second condition\"; then echo \"Second then\"; elif echo \"Second elif\"; if echo \"Nested if in elif\"; then echo \"Nested then in elif\"; fi; else echo \"Second else\"; fi; then echo \"First then\"; elif echo \"First elif\"; then echo \"First elif then\"; elif echo \"Another elif\"; else echo \"First else\"; fi";
    let test_script = "test_script.sh";
    let script_input = &fs::read_to_string(test_script);
    let mut parser = parser::RshParser::new(input);
    match parser.tokenize() {
        Ok(_) => {
            parser.print_tokens();
            match parser.parse_blocks() {
                Ok(_) => {
                    parser.print_units();
                    match parser.parse_unitlist() {
                        Ok(evals) => println!("final evaluations: {:#?}",evals),
                        Err(e) => println!("{}",e)
                    }
                }
                Err(e) => println!("{}",e),
            }
        }
        Err(e) => println!("{}",e),
    }
}
