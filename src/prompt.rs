use tokio::{io::{self, AsyncBufReadExt, AsyncWriteExt, BufReader}, sync::mpsc};
use log::{trace,debug};

use crate::{event::{ShellError,ShellEvent}, shellenv::ShellEnv};
use crate::interp::parse::descend;

pub async fn prompt(sender: mpsc::Sender<ShellEvent>, shellenv: &mut ShellEnv) {
    debug!("Reached prompt");

    let mut stdout = io::stdout();
    stdout.write_all(b"> ").await.unwrap();
    stdout.flush().await.unwrap();

    let stdin = io::stdin();
    let mut reader = BufReader::new(stdin);

    let mut input = String::new();
    reader.read_line(&mut input).await.unwrap();

    let trimmed_input = input.trim().to_string();
    trace!("Received input! {}",trimmed_input);
    let shellenv = ShellEnv::new(false,false);
    let state = descend(&input,&shellenv);
    match state {
        Ok(parse_state) => {
            let _ = sender.send(ShellEvent::NewAST(parse_state.ast)).await;
            let _ = sender.send(ShellEvent::Prompt).await;
        }
        Err(e) => {
            let _ = sender.send(ShellEvent::CatchError(ShellError::ParsingError(e))).await;
            let _ = sender.send(ShellEvent::Prompt).await;
        }
    }
}
