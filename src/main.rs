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


pub mod parser2;
//pub mod prompt;
//pub mod event;
//pub mod execute;
pub mod shellenv;
//pub mod builtin;

use crate::parser2::RshInputManager;
//use crate::event::EventLoop;
use crate::shellenv::ShellEnv;


//#[tokio::main]
//async fn main() {
    //env_logger::init();
    //let mut shellenv = ShellEnv::new(false,true);
    //let mut event_loop = EventLoop::new(&mut shellenv);
    //let _ = event_loop.listen().await;
//}

fn main() {
    env_logger::init();
    let input = "for for in for; do for=for; echo $for; done";
    let mut shellenv = ShellEnv::new(false,false);
    let mut input_man = RshInputManager::new(input,&mut shellenv);
    let tokens = input_man.interpret(None,true);
    match tokens {
        Ok(tree) => for token in tree {
            println!("{}",token);
        },
        Err(e) => println!("{}",e)
    }
}
