use std::fs::File;
use std::io::stdin;
use std::io::Write;
use std::io::stdout;
use std::collections::VecDeque;
use std::process::{ChildStdout, Command, Stdio};
use log::{debug, error, trace};
use std::path::PathBuf;

fn exec_command(pipein: Option<ChildStdout>,mut tokens: VecDeque<String>) -> Option<ChildStdout> {
    let mut curr_cmd: Vec<String> = vec![];

    let mut filepath: Option<PathBuf> = None;
    while let Some(token) = tokens.pop_front() {
        match token.as_str() {
            "|" => break,
            ">" => { filepath = Some(tokens.pop_front().unwrap().into()) },
            _ => {
                curr_cmd.push(token.to_string());
            }
        }
    }
    let cmd = curr_cmd.remove(0);
    let args = curr_cmd;
    let stdin = if let Some(input) = pipein {
        Stdio::from(input)
    } else {
        Stdio::inherit()
    };

    let stdout = if !tokens.is_empty() || filepath.is_some() {
        Stdio::piped()
    } else {
        Stdio::inherit()
    };
    let mut child = match Command::new(cmd)
        .args(args)
        .stdout(stdout)
        .stdin(stdin)
        .spawn() {
            Ok(child) => {
                trace!("Command spawned successfully");
                child
            },
            Err(e) => {
                error!("Error spawning command: {}", e);
                return None;
            }
        };
    trace!("Waiting for command to finish...");
    if let Err(e) = child.wait() {
        error!("Error waiting for command: {}", e);
    }
    if !tokens.is_empty() {
        exec_command(child.stdout, tokens)
    } else {
        // Handle output redirection to a file
        if let Some(path) = filepath {
            match File::create(path) {
                Ok(mut file) => {
                    // Unwrap the stdout and copy the data to the file
                    if let Some(mut stdout) = child.stdout.take() {
                        std::io::copy(&mut stdout, &mut file).expect("Failed to write to file");
                    }
                },
                Err(e) => {
                    error!("Error creating file: {}", e);
                }
            }
        }
        child.stdout
    }
}

fn tokenize_input(input: String) -> VecDeque<String> {
    // Group characters in quotation marks
    // Ignore special characters after backslashes
    // Split by whitespace everywhere else
    // Token groups separated by operators for command chaining/redirection
    // Token groups separated by semicolons or linebreaks for separate invokations

    let charstream = input.trim().chars();
    let mut single_quoted: bool = false;
    let mut double_quoted: bool = false;
    let mut cur_token = String::new();
    let mut tokens: VecDeque<String> = VecDeque::new();

    debug!("Tokenizing input: {}",input);
    trace!("charstream: {:?}",charstream);

    for item in charstream {
        trace!("Checking character: {}", item);
        match item {
            ' ' | '\n' | ';' => {
                cur_token.push(item);
                if ! cur_token.is_empty() && ! single_quoted && ! double_quoted {
                    debug!("Pushing token {}", cur_token);
                    tokens.push_back(cur_token);
                    cur_token = String::new();
                }
            },
            '\'' => {
                single_quoted = !single_quoted;
            }
            '\"' => {
                double_quoted = !double_quoted;
            }
            _ => {
                cur_token.push(item);
            }
        }
    }
    debug!("Pushing token {}", cur_token);
    tokens.push_back(cur_token); // If it's the last token
    tokens
}

fn parse_tokens(words: VecDeque<String>) -> VecDeque<VecDeque<String>> {
    debug!("Parsing words: {:?}",words);
    let mut paragraph: VecDeque<VecDeque<String>> = VecDeque::new();
    let mut sentence: VecDeque<String> = VecDeque::new();

    for word in words {
        trace!("Checking word: {}",word);
        let lastchar = word.chars().last();
        if lastchar.is_none() { return VecDeque::new(); } // Word is empty
        match lastchar.unwrap() {
            ';' | '\n' => { // Delimiters
                let word_trimmed = word.trim_end_matches([';','\n']).to_string(); // Cut off delimiters
                sentence.push_back(word_trimmed.trim().to_string());
                trace!("Pushing sentence: {:?}",sentence);
                paragraph.push_back(sentence);
                sentence = VecDeque::new();
            }
            _ => {
                match word.as_str() {
                    " " => { continue; }, // Last line of defense to prevent empty strings
                    _ => {
                        trace!("Pushing word: {}",word);
                        sentence.push_back(word.trim().to_string());
                    }

                }
            }
        }
    }
    paragraph.push_back(sentence);
    debug!("Returning paragraph: {:?}",paragraph);
    paragraph
}

fn prompt() -> String {
    print!("> ");
    let _ = stdout().flush();

    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();

    debug!("Received input: {}", input.trim());

    let commands = input.trim();

    commands.to_string()
}

fn main() {
    env_logger::init();
    loop {
        let input = prompt();
        let words = tokenize_input(input);
        let commands = parse_tokens(words);
        for command in commands {
            exec_command(None, command);
        }
    }
}
