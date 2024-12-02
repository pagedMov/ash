//pub mod event;
//pub mod parser;
//pub mod prompt;
//pub mod execute;

//use log::debug;

//use crate::event::EventLoop;

//#[tokio::main]
//async fn main() {
    //env_logger::init();
    //let mut event_loop = EventLoop::new();
//
    //debug!("Starting event loop");
    // TODO: use the value returned by this for something
    //let _ = event_loop.listen().await;
//}

use pest::{RuleType,Parser};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "rsh.pest"]
pub struct RshParser;

fn main() {

    let input = "command arg1 arg2 | command2 arg1";
    let result = RshParser::parse(Rule::lines, input).unwrap();

    for pair in result {
        println!("{}",pair)
    }


}
