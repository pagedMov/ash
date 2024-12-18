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
    let input = "select opt in opt1 opt2 opt3; do case opt in; opt1) echo option 1;;opt2) echo option 2;;opt3) echo option 3;;esac; done";
    let shellenv = ShellEnv::new(false,false);
    let state = descend(input,&shellenv);
    println!("printing tree");
    println!("{}",state.ast);
}
