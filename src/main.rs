pub mod event;
pub mod prompt;
pub mod interpret;
mod rsh;

use log::debug;

use tokio::io::{self, AsyncBufReadExt, BufReader};
use tokio_stream::{wrappers::LinesStream, StreamExt};

use crate::event::EventLoop;

#[tokio::main]
async fn main() {
  env_logger::init();
  let mut event_loop = EventLoop::new();

  debug!("Starting event loop");
   //TODO: use the value returned by this for something
  let _ = event_loop.listen().await;
}
