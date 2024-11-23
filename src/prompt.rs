use std::io::{self, Write};

pub fn prompt() -> String {
    print!("> ");
    let _ = io::stdout().flush();

    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();

    input.trim().to_string()
}
