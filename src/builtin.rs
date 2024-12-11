use std::collections::VecDeque;
use std::io::Write;
use std::os::fd::{AsFd, AsRawFd, BorrowedFd};
use std::{io, os::fd::RawFd};
use nix::unistd::write;


use crate::execute::RshExitStatus;
use crate::{event::ShellError, parser::Token};

pub const BUILTINS: [&str; 14] = [
    "echo", "set", "shift", "export", "readonly", "declare", "local", "unset", "trap", "node",
    "exec", "source", "wait",
];

/// Easter egg for `sudo rm -rf /*`
pub fn lecture() {
    println!("Whoa whoa whoa, hold on there. Do you even know what you just asked for?");
    let warnings = vec![
        "Are you sure? This command will delete your entire filesystem. Is that what you wanted?",
        "Maybe you didn't read that correctly. I said your ENTIRE filesystem. Everything. There will be no survivors. Continue?",
        "Listen. If you keep pressing 'y', things are going to get ugly. Are you sure that you're sure?",
        "Are you sure that you're sure that you're sure?",
        "Think about your poor, innocent filesystem. Did it really do anything to warrant this kind of treatment?",
        "Even then, surely you can find it in your heart to forgive these poor, defenseless files?",
        "You're a monster. Maybe I should run this command after all.",
        "I'm really going to do it. Keep pressing 'y' at your own peril.",
        "I'm really, SERIOUSLY about to do it. Do you really, SERIOUSLY want me to delete every file on your computer?",
        "Alright, it is apparent that you cannot be convinced. However, before I run this command, let's go over this one more time just to be sure you know what you're asking for.",
    ];
}

pub fn echo(mut argv: VecDeque<Token>, stdout: Option<RawFd>) -> Result<RshExitStatus,ShellError> {
    argv.pop_front(); // argv[0] is 'echo'
    let mut output = argv
        .into_iter()
        .map(|t| t.text().into())
        .collect::<Vec<String>>();
    output.push("\n".into());
    let output_str = output.join(" ");

    let output = unsafe { BorrowedFd::borrow_raw(stdout.unwrap_or(1)) };

    if let Err(e) = write(output.as_fd(), output_str.as_bytes()) {
        return Err(ShellError::ExecFailed(format!("Congrats on somehow managing to make echo return 1. Cause: {}",e), 1));
    }
    Ok(RshExitStatus::Success)
}
