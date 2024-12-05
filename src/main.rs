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
    let input = "name=\"world\"\ncount=5\necho \"Hello, $name!\"\ncurrent_time=$(date +%H:%M:%S)\n\necho \"The current time is $current_time.\"\nresult=$((count * 2))\necho \"Twice the count is $result.\"\n";
    let test_script = "./test_script.sh";
    let test_string = "current_time=$(date +%H:%M:%S)";
    let script_input = &fs::read_to_string(test_script).unwrap();
    let mut parser = parser::RshParser::new(script_input);
    match parser.tokenize() {
        Ok(_) => {
            parser.print_tokens();
            match parser.parse_blocks() {
                Ok(_) => parser.print_units(),
                Err(e) => println!("{}",e),
            }
        }
        Err(e) => println!("{}",e),
    }
}
