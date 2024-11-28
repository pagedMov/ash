use tokio::{io::{self, AsyncBufReadExt, AsyncWriteExt, BufReader}, sync::mpsc};
use log::debug;

use crate::event::ShellEvent;

pub async fn prompt(sender: mpsc::Sender<ShellEvent>) {
    debug!("Reached prompt");

    let mut stdout = io::stdout();
    stdout.write_all(b"> ").await.unwrap();
    stdout.flush().await.unwrap();

    let stdin = io::stdin();
    let mut reader = BufReader::new(stdin);

    let mut input = String::new();
    reader.read_line(&mut input).await.unwrap();

    let trimmed_input = input.trim().to_string();
    sender.send(ShellEvent::UserInput(trimmed_input)).await.unwrap();
}
