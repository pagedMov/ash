use regex::Regex;
use std::str::Chars;
use std::iter::Peekable;
use std::os::unix::process::ExitStatusExt;
use std::process::ExitStatus;
use std::io::{stdout, stderr, Read, Write};

pub fn is_var_declaration(word: String) -> bool {
    let regex = Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_-]*=[^=]+$").unwrap();
    regex.is_match(&word)
}

pub fn fail(reason: &str) -> (ExitStatus,String) {
    (ExitStatus::from_raw(1),reason.to_string())
}
pub fn succeed() -> ExitStatus {
    ExitStatus::from_raw(0)
}

pub fn build_word(chars: &mut Peekable<Chars<'_>>,mut singlequote: bool, mut doublequote: bool, mut word: String) -> String {
    // Builds word recursively from chars iterator
    // Stops at standard shell delimiters
    if let Some(&c) = chars.peek() {
        match c {
            '"' if !singlequote => {
                doublequote = !doublequote;
                chars.next();
                return build_word(chars, singlequote, doublequote, word);
            },
            '\'' if !doublequote => {
                singlequote = !singlequote;
                chars.next();
                return build_word(chars, singlequote, doublequote, word);
            },
            _ => {}
        }
        if ";|\n<> \t".contains(c) && !doublequote && !singlequote { // Whitespace, semicolons, newlines, operators
            return word;
        }
        word.push(c);
        chars.next();
        return build_word(chars, singlequote, doublequote, word)
    }
    word
}

pub fn write_bytes<R>(read_from: &mut R) -> Result<Vec<u8>,String> where R: Read  {
    let mut buffer: Vec<u8> = vec![];
    match read_from.read_to_end(&mut buffer) {
        Ok(_) => { Ok(buffer) },
        Err(e) => { Err(e.to_string()) }
    }
}

pub fn read_bytes<W>(write_to: &mut W, read_from: &[u8]) -> Result<(),String> where W: Write  {
    match write_to.write_all(read_from) {
        Ok(_) => { Ok(()) },
        Err(e) => { Err(e.to_string()) }
    }
}

pub fn write_stdout(output: Vec<u8>) -> Result<(),(ExitStatus, String)> {
    stdout().write_all(&output).map_err(|e| {
        (ExitStatus::from_raw(1), format!("Echo failed: {}", e))
    })?;
    stdout().flush().map_err(|e| {
        (ExitStatus::from_raw(1), format!("Echo failed: {}", e))
    })?;
    Ok(())
}

pub fn write_stderr(output: Vec<u8>) -> Result<(),(ExitStatus, String)> {
    stderr().write_all(&output).map_err(|e| {
        (ExitStatus::from_raw(1), format!("Echo failed: {}", e))
    })?;
    stderr().flush().map_err(|e| {
        (ExitStatus::from_raw(1), format!("Echo failed: {}", e))
    })?;
    Ok(())
}

pub fn extract_var(word: String) -> Result<(String,String),String> {
    if let Some((key, value)) = word.split_once('=') {
        Ok((key.to_string(),value.to_string()))
    } else {
        Err("Error parsing key-value pair".to_string())
    }
}
