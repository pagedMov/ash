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


pub mod prompt;
pub mod event;
pub mod execute;
pub mod shellenv;
pub mod interp;
pub mod builtin;

use event::EventLoop;
use interp::parse::descend;
use interp::debug;

//use crate::event::EventLoop;
use crate::shellenv::ShellEnv;
use im::Vector;
use tokio::{io::{self, AsyncBufReadExt, AsyncWriteExt, BufReader}, sync::mpsc};
use log::{trace,debug};


#[tokio::main]
async fn main() {
    env_logger::init();
    let mut shellenv = ShellEnv::new(false,true);
    let mut event_loop = EventLoop::new(&mut shellenv);
    let _ = event_loop.listen().await;
}

//#[tokio::main]
//async fn main() {
    //env_logger::init();
    //loop {
        //let mut stdout = io::stdout();
        //stdout.write_all(b"> ").await.unwrap();
        //stdout.flush().await.unwrap();
//
        //let stdin = io::stdin();
        //let mut reader = BufReader::new(stdin);
//
        //let mut input = String::new();
        //reader.read_line(&mut input).await.unwrap();
//
        //let trimmed_input = input.trim().to_string();
        //trace!("Received input! {}",trimmed_input);
        //let shellenv = ShellEnv::new(false,false);
        //let state = descend(&input,&shellenv);
        //if let Err(e) = state {
            //println!("{}",e);
        //} else {
            //println!("debug tree:");
            //println!("{}",state.unwrap().ast);
        //}
    //}
//}
