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
pub mod prompt;
pub mod event;
pub mod execute;
pub mod shellenv;

use crate::event::EventLoop;
use crate::shellenv::ShellEnv;


#[tokio::main]
async fn main() {
    env_logger::init();
    let mut shellenv = ShellEnv::new(true);
    let mut event_loop = EventLoop::new(shellenv);
    let _ = event_loop.listen().await;
}
