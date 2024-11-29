use std::os::unix::process::ExitStatusExt;
use std::io::Write;
use std::process::{ExitStatus, Stdio};
use std::pin::Pin;
use std::future::Future;

use log::{debug, error, info, trace, warn};
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::process::{self, Command};
use tokio::sync::mpsc;

use crate::event::{ShellError, ShellEvent};
use crate::parser::{ASTNode, ChainOp, Redirection};

pub struct NodeWalker {
    event_outbox: mpsc::Sender<ShellEvent>,
    node: ASTNode,
}

impl NodeWalker {
    pub fn new(event_outbox: mpsc::Sender<ShellEvent>, node: ASTNode) -> Self {
        Self { event_outbox, node }
    }

    pub fn outbox(&self) -> mpsc::Sender<ShellEvent> {
        self.event_outbox.clone()
    }

    pub fn walk(self) -> Pin<Box<dyn Future<Output = Result<ExitStatus, ShellError>> + Send>> {
        let node = self.node.clone(); // Clone for use in async blocks
        let outbox = self.outbox().clone();

        Box::pin(async move {
            match node {
                ASTNode::ShCommand { .. } | ASTNode::Builtin { .. } => {
                    debug!("walk: Executing command:\n {:#?}", node);
                    exec(node.clone(), outbox).await
                }
                ASTNode::Pipeline { ref commands } => {
                    debug!("walk: Traversing Pipeline");
                    exec(node, outbox).await
                }
                ASTNode::CmdChain {
                    left,
                    right,
                    operator,
                } => {
                    debug!("walk: Processing CmdChain");

                    let left_walker = NodeWalker::new(outbox.clone(), *left);
                    let left_result = left_walker.walk().await?;

                    match operator {
                        ChainOp::And if left_result.success() => {
                            let right_walker = NodeWalker::new(outbox, *right);
                            right_walker.walk().await
                        }
                        ChainOp::Or if !left_result.success() => {
                            let right_walker = NodeWalker::new(outbox, *right);
                            right_walker.walk().await
                        }
                        _ => Ok(left_result),
                    }
                }
                ASTNode::Conditional { paths } => {
                    debug!("walk: Evaluating Conditional");

                    for path in paths {
                        if let Some(condition) = path.get_cond() {
                            let cond_walker = NodeWalker::new(outbox.clone(), *condition.clone());
                            let cond_result = cond_walker.walk().await?;

                            if cond_result.success() {
                                let body_walker = NodeWalker::new(outbox, path.get_body().clone());
                                return body_walker.walk().await;
                            }
                        } else {
                            let body_walker = NodeWalker::new(outbox, path.get_body().clone());
                            return body_walker.walk().await;
                        }
                    }
                    Ok(ExitStatus::from_raw(0))
                }
                ASTNode::Loop { .. } => {
                    debug!("walk: Starting Loop");
                    todo!("Implement loops")
                }
                _ => unreachable!(),
            }
        })
    }
}

async fn get_output(node: ASTNode, piped_input: Option<Vec<u8>>, outbox: mpsc::Sender<ShellEvent>) -> (Vec<u8>,Vec<u8>,ExitStatus) {
    // TODO: handle errors and redirection
    if let ASTNode::ShCommand { name, args, redirs } = node {
        let mut piped_stdout: Vec<u8> = vec![];
        let mut piped_stderr: Vec<u8> = vec![];
        let source: Stdio;
        if piped_input.is_some() {
            source = Stdio::piped();
        } else {
            source = Stdio::inherit();
        }
        let mut child = Command::new(name)
            .args(args)
            .stdin(source)
            .stdout(Stdio::piped())
            .spawn().unwrap();
        if piped_input.is_some() {
            if let Some(mut stdin) = child.stdin.take() {
                stdin.write_all(&piped_input.unwrap()).await;
            }
        }
        if let Some(mut stdout) = child.stdout.take() {
            stdout.read_to_end(&mut piped_stdout).await;
        }
        if let Some(mut stderr) = child.stderr.take() {
            stderr.read_to_end(&mut piped_stderr).await;
        }
        let status = child.wait().await.unwrap();
        (piped_stdout,piped_stderr,status)
    } else { unreachable!() }
}

async fn exec(node: ASTNode, outbox: mpsc::Sender<ShellEvent>) -> Result<ExitStatus, ShellError> {
    match node {
        ASTNode::Builtin { name, args, redirs } => {
            todo!("implement builtins")
        }
        ASTNode::ShCommand { name, args, redirs } => {

        }
        ASTNode::Pipeline { commands } => {

            let mut prev_stdout: Option<Vec<u8>> = None;
            for command in commands {
                let (stdout,stderr,status) = get_output(command,prev_stdout.clone(),outbox.clone()).await;
                prev_stdout = Some(stdout);
            }
            std::io::stdout().write_all(&prev_stdout.unwrap()).unwrap();
        }
        _ => unreachable!()
    }
    todo!()
}
