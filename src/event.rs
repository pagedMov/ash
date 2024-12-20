use std::collections::VecDeque;

use tokio::sync::mpsc;
use log::{error,debug,info};
use tokio::signal::unix::{signal, Signal, SignalKind};
use thiserror::Error;

use crate::interp::parse::{ParseErrFull,Node};
use crate::{execute, prompt};
use crate::shellenv::ShellEnv;
//use crate::execute::NodeWalker;

#[derive(Debug,Error,PartialEq)]
pub enum ShellError {
    #[error("Command not found: {0}")]
    CommandNotFound(String),

    #[error("Invalid syntax: {0}")]
    InvalidSyntax(String),

    #[error("{0}")]
    ParsingError(ParseErrFull),

    #[error("Execution faiiled for command '{0}' with exit code {1}")]
    ExecFailed(String,i32),

    #[error("I/o error: {0}")]
    IoError(String),
}

impl ShellError {
    pub fn is_fatal(&self) -> bool {
        match self {
            ShellError::IoError(..) => true,
            ShellError::CommandNotFound(..) => false,
            ShellError::ExecFailed(..) => false,
            ShellError::ParsingError(..) => false,
            ShellError::InvalidSyntax(..) => false,
        }
    }
}

#[derive(Debug,PartialEq)]
pub enum ShellEvent {
    Prompt,
    Signal(Signals),
    SubprocessExited(u32,i32),
    NewAST(Node),
    CatchError(ShellError),
    Exit(i32)
}

#[derive(Debug,PartialEq)]
pub enum Signals {
    SIGINT,
    SIGIO,
    SIGPIPE,
    SIGTSTP,
    SIGQUIT,
    SIGTERM,
    SIGCHLD,
    SIGHUP,
    SIGWINCH,
    SIGUSR1,
    SIGUSR2
}

pub struct EventLoop<'a> {
    sender: mpsc::Sender<ShellEvent>,
    receiver: mpsc::Receiver<ShellEvent>,
    shellenv: &'a mut ShellEnv
}

impl<'a> EventLoop<'a> {
    /// Creates a new `EventLoop` instance with a message passing channel.
    ///
    /// # Returns
    /// A new instance of `EventLoop` with a sender and receiver for inter-task communication.
    pub fn new(shellenv: &'a mut ShellEnv) -> Self {
        let (sender, receiver) = mpsc::channel(100);
        Self {
            sender,
            receiver,
            shellenv
        }
    }

    /// Provides a clone of the `sender` channel to send events to the event loop.
    ///
    /// # Returns
    /// A clone of the `mpsc::Sender<ShellEvent>` for sending events.
    pub fn inbox(&self) -> mpsc::Sender<ShellEvent> {
        self.sender.clone()
    }

    /// Starts the event loop and listens for incoming events.
    ///
    /// This method spawns a separate task to listen for system signals using a `SignalListener`,
    /// and then begins processing events from the channel.
    ///
    /// # Returns
    /// A `Result` containing the exit code (`i32`) or a `ShellError` if an error occurs.
    pub async fn listen(&mut self) -> Result<i32, ShellError> {
        let mut signal_listener = SignalListener::new(self.inbox());
        tokio::spawn(async move {
            signal_listener.signal_listen().await
        });
        self.event_listen().await
    }

    /// Processes events from the event loop's receiver channel.
    ///
    /// This method handles different types of `ShellEvent` messages, such as prompting the user,
    /// handling exit signals, processing new AST nodes, and responding to subprocess exits or errors.
    ///
    /// # Returns
    /// A `Result` containing the exit code (`i32`) or a `ShellError` if an error occurs.
    pub async fn event_listen(&mut self) -> Result<i32, ShellError> {
        debug!("Event loop started.");
        let mut code: i32 = 0;

        // Send an initial prompt event to the loop.
        self.sender.send(ShellEvent::Prompt).await.unwrap();
        while let Some(event) = self.receiver.recv().await {
            match event {
                ShellEvent::Prompt => {
                    // Trigger the prompt logic.
                    prompt::prompt(self.inbox(),self.shellenv).await;
                }
                ShellEvent::Exit(exit_code) => {
                    // Handle exit events and set the exit code.
                    code = exit_code;
                }
                ShellEvent::NewAST(tree) => {
                    // Log and process a new AST node.
                    debug!("new tree:\n {:#?}", tree);
                    let mut walker = execute::NodeWalker::new(tree,&mut self.shellenv);
                    match walker.start_walk() {
                        Ok(code) => {
                            info!("Last exit status: {:?}",code);
                        },
                        Err(e) => self.inbox().send(ShellEvent::CatchError(e)).await.unwrap()
                    }
                }
                ShellEvent::SubprocessExited(pid, exit_code) => {
                    // Log the exit of a subprocess.
                    debug!("Process '{}' exited with code {}", pid, exit_code);
                }
                ShellEvent::Signal(signal) => {
                    // Handle received signals.
                    debug!("Received signal: {:?}", signal);
                }
                ShellEvent::CatchError(err) => {
                    // Handle errors, exiting if fatal.
                    if err.is_fatal() {
                        error!("Fatal: {}", err);
                        std::process::exit(1);
                    } else {
                        error!("Error: {}", err);
                    }
                }
            }
        }
        Ok(code)
    }
}

pub struct SignalListener {
    outbox: mpsc::Sender<ShellEvent>,
    //sigint: Signal,
    sigio: Signal,
    sigpipe: Signal,
    sigtstp: Signal,
    sigquit: Signal,
    sigterm: Signal,
    sigchild: Signal,
    sighup: Signal,
    sigwinch: Signal,
    sigusr1: Signal,
    sigusr2: Signal,
}

impl SignalListener {
    pub fn new(outbox: mpsc::Sender<ShellEvent>) -> Self {
        Self {
            // Signal listeners
            // TODO: figure out what to do instead of unwrapping
            outbox,
            //sigint: signal(SignalKind::interrupt()).unwrap(),
            sigio: signal(SignalKind::io()).unwrap(),
            sigpipe: signal(SignalKind::pipe()).unwrap(),
            sigtstp: signal(SignalKind::from_raw(20)).unwrap(),
            sigquit: signal(SignalKind::quit()).unwrap(),
            sigterm: signal(SignalKind::terminate()).unwrap(),
            sigchild: signal(SignalKind::child()).unwrap(),
            sighup: signal(SignalKind::hangup()).unwrap(),
            sigwinch: signal(SignalKind::window_change()).unwrap(),
            sigusr1: signal(SignalKind::user_defined1()).unwrap(),
            sigusr2: signal(SignalKind::user_defined2()).unwrap(),
        }
    }
    pub async fn signal_listen(&mut self) -> Result<i32, ShellError> {
        //let sigint = &mut self.sigint;
        let sigio = &mut self.sigio;
        let sigpipe = &mut self.sigpipe;
        let sigtstp = &mut self.sigtstp;
        let sigquit = &mut self.sigquit;
        let sigterm = &mut self.sigterm;
        let sigchild = &mut self.sigchild;
        let sighup = &mut self.sighup;
        let sigwinch = &mut self.sigwinch;
        let sigusr1 = &mut self.sigusr1;
        let sigusr2 = &mut self.sigusr2;

        loop {
            tokio::select! {
                //_ = sigint.recv() => {
                    //self.outbox.send(ShellEvent::Signal(Signals::SIGINT)).await.unwrap();
                    // Handle SIGINT
                //}
                _ = sigio.recv() => {
                    self.outbox.send(ShellEvent::Signal(Signals::SIGIO)).await.unwrap();
                    // Handle SIGIO
                }
                _ = sigpipe.recv() => {
                    self.outbox.send(ShellEvent::Signal(Signals::SIGPIPE)).await.unwrap();
                    // Handle SIGPIPE
                }
                _ = sigtstp.recv() => {
                    self.outbox.send(ShellEvent::Signal(Signals::SIGTSTP)).await.unwrap();
                    // Handle SIGPIPE
                }
                _ = sigquit.recv() => {
                    self.outbox.send(ShellEvent::Signal(Signals::SIGQUIT)).await.unwrap();
                    // Handle SIGQUIT
                }
                _ = sigterm.recv() => {
                    self.outbox.send(ShellEvent::Signal(Signals::SIGTERM)).await.unwrap();
                    // Handle SIGTERM
                }
                _ = sigchild.recv() => {
                    self.outbox.send(ShellEvent::Signal(Signals::SIGCHLD)).await.unwrap();
                    // Handle SIGCHLD
                }
                _ = sighup.recv() => {
                    self.outbox.send(ShellEvent::Signal(Signals::SIGHUP)).await.unwrap();
                    // Handle SIGHUP
                }
                _ = sigwinch.recv() => {
                    self.outbox.send(ShellEvent::Signal(Signals::SIGWINCH)).await.unwrap();
                    // Handle SIGWINCH
                }
                _ = sigusr1.recv() => {
                    self.outbox.send(ShellEvent::Signal(Signals::SIGUSR1)).await.unwrap();
                    // Handle SIGUSR1
                }
                _ = sigusr2.recv() => {
                    self.outbox.send(ShellEvent::Signal(Signals::SIGUSR2)).await.unwrap();
                    // Handle SIGUSR2
                }
            }
        }
    }
}
