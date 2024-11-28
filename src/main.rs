pub mod event;
pub mod parser;
pub mod prompt;

use log::debug;

use crate::event::EventLoop;

#[tokio::main]
async fn main() {
    env_logger::init();
    let mut event_loop = EventLoop::new();

    debug!("Starting event loop");
    // TODO: use the value returned by this for something
    let _ = event_loop.listen().await;
}
