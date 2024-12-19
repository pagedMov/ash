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


//pub mod prompt;
//pub mod event;
//pub mod execute;
pub mod shellenv;
pub mod interp;
//pub mod builtin;

use interp::parse::descend;
use interp::debug;

//use crate::event::EventLoop;
use crate::shellenv::ShellEnv;
use im::Vector;


//#[tokio::main]
//async fn main() {
    //env_logger::init();
    //let mut shellenv = ShellEnv::new(false,true);
    //let mut event_loop = EventLoop::new(&mut shellenv);
    //let _ = event_loop.listen().await;
//}

fn main() {
    env_logger::init();
    //let input = "for var in 1 2 3; do case var in;1) if until until until echo; do echo; done; do if echo; then echo; elif echo; then if echo; then echo; elif echo; then echo; fi; fi; done; do for i in 1 2 3; do echo; done; done; then until if until echo; do echo; done; then if echo; then echo; elif echo; then echo; fi; elif for i in 1 2 3; do echo; done; then until echo; do echo; done; fi; do if echo; then echo; elif echo; then echo; fi; done; fi;;2) until echo; do echo; done;;3) for i in 1 2 3; do echo; done ;;esac; done";
    let input = "while while echo; do echo; done; do echo; done";
    let shellenv = ShellEnv::new(false,false);
    let state = descend(input,&shellenv);
    println!("printing debug tree");
    println!("input: {}",input);
    println!();
    match state {
        Ok(parse_state) => println!("{}",parse_state.ast),
        Err(e) => println!("{}",e)
    }
}
