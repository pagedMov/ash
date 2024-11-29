use log::{debug, error, info, trace, warn};
use tokio::sync::mpsc;

use crate::event::ShellEvent;
use crate::parser::ASTNode;

pub struct NodeWalker {
    event_outbox: mpsc::Sender<ShellEvent>,
    node: ASTNode
}

impl NodeWalker {
    pub fn new(event_outbox: mpsc::Sender<ShellEvent>, node: ASTNode) -> Self {
        Self { event_outbox, node }
    }

    pub fn outbox(&self) -> mpsc::Sender<ShellEvent> {
        self.event_outbox.clone()
    }

    pub async fn walk(&self) {
        // Walk across the node from left to right basically
        match &self.node {
            ASTNode::ShCommand { name, args, redirs } => {
                debug!("walk: Executing ShCommand:\n {:#?}",self.node);
            }
            ASTNode::Builtin { name, args, redirs } => {
                debug!("walk: Executing Builtin:\n {:#?}",self.node);
            }
            ASTNode::Pipeline { commands } => {
                debug!("walk: Traversing Pipeline");
            }
            ASTNode::CmdChain { left, right, operator } => {
                debug!("walk: Processing CmdChain");
            }
            ASTNode::Conditional { paths } => {
                debug!("walk: Evaluating Conditional");
            }
            ASTNode::Loop { condition, loopvar_identifier, loopvar_value, body } => {
                debug!("walk: Starting Loop");
            }
            _ => unreachable!()
        }
    }

}
