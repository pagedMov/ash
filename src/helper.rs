use regex::Regex;
use std::os::unix::process::ExitStatusExt;
use std::process::ExitStatus;
use std::io::{Write, stdout};

pub fn is_var_declaration(word: String) -> bool {
    let regex = Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_-]*=[^=]+$").unwrap();
    regex.is_match(&word)
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

pub fn extract_var(word: String) -> Result<(String,String),String> {
    if let Some((key, value)) = word.split_once('=') {
        Ok((key.to_string(),value.to_string()))
    } else {
        Err("Error parsing key-value pair".to_string())
    }
}
