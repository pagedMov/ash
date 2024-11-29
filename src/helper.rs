use regex::Regex;
use std::fs::OpenOptions;
use std::str::Chars;
use std::iter::Peekable;
use std::io::{stdout, stderr, Read, Write};

use crate::parser::Redirection;

pub fn increment_string(int_str: String) -> Result<String,InternalError> {
    if let Ok(mut int) = int_str.parse::<i32>() {
        int += 1;
        Ok(int.to_string())
    } else {
        Err(InternalError::new("Failed to parse integer string"))
    }
}

pub fn is_var_declaration(word: &str) -> bool {
    let regex = Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*=.*$").unwrap();
    regex.is_match(word)
}

pub fn extract_var(word: String) -> Result<(String, String), InternalError> {
    if let Some((key, value)) = word.split_once('=') {
        if !key.is_empty() {
            Ok((key.to_string(), value.to_string()))
        } else {
            Err(InternalError::new("Error parsing key-value pair: key is empty"))
        }
    } else {
        Err(InternalError::new("Error parsing key-value pair: no '=' found"))
    }
}

pub fn redirect_input(redir: Redirection) -> Result<Vec<u8>,InternalError> {
    let mut buffer: Vec<u8> = vec![];
    let mut file = OpenOptions::new()
        .read(true)
        .open(redir.get_filepath())
        .map_err(|e| InternalError::new(&e.to_string()))?;
    let file_content = read_bytes(&mut file)?;
    write_bytes(&mut buffer, &file_content)?;
    Ok(buffer)
}

pub fn redirect_output(buffer: Vec<u8>, redir: Redirection) -> Result<(),InternalError> {
    let mut file = OpenOptions::new()
        .create(true)
        .truncate(true)
        .write(true)
        .open(redir.get_filepath())
        .map_err(|e| InternalError::new(&e.to_string()))?;
    write_bytes(&mut file, &buffer)?;
    Ok(())
}

pub fn fail<'a>(reason: &'a str) -> (i32,&'a str) {
    (1,reason)
}
pub fn succeed() -> i32 {
    0
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

pub fn read_bytes<R>(writer: &mut R) -> Result<Vec<u8>,InternalError> where R: Read  {
    let mut buffer: Vec<u8> = vec![];
    match writer.read_to_end(&mut buffer) {
        Ok(_) => { Ok(buffer) },
        Err(e) => { Err(InternalError::new(&e.to_string())) }
    }
}

pub fn write_bytes<W>(reader: &mut W, writer: &[u8]) -> Result<(),InternalError> where W: Write  {
    match reader.write_all(writer) {
        Ok(_) => { Ok(()) },
        Err(e) => { Err(InternalError::new(&e.to_string())) }
    }
}

pub fn write_stdout(output: Vec<u8>) -> Result<(),InternalError> {
    stdout().write_all(&output)
        .map_err(|e| InternalError::new(&format!("Echo failed: {}",e)))?;
    stdout().flush()
        .map_err(|e| InternalError::new(&format!("Echo failed: {}",e)))?;
    Ok(())
}

pub fn write_stderr(output: Vec<u8>) -> Result<(),InternalError> {
    stderr().write_all(&output)
        .map_err(|e| InternalError::new(&format!("Echo failed: {}",e)))?;
    stderr().flush()
        .map_err(|e| InternalError::new(&format!("Echo failed: {}",e)))?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn test_is_var_declaration() {
        assert!(is_var_declaration("VAR=value"));
        assert!(is_var_declaration("a1_b2=value"));
        assert!(is_var_declaration("VAR1=value"));
        assert!(!is_var_declaration("=value"));
        assert!(!is_var_declaration("1VAR=value"));
        assert!(!is_var_declaration("VAR==value"));
    }

    #[test]
    fn test_extract_var() {
        assert_eq!(
            extract_var("VAR=value".to_string()).unwrap(),
            ("VAR".to_string(), "value".to_string())
        );
        assert_eq!(
            extract_var("KEY=12345".to_string()).unwrap(),
            ("KEY".to_string(), "12345".to_string())
        );
        assert!(extract_var("KEY=value=extra".to_string()).is_ok());
        assert!(extract_var("=value".to_string()).is_err());
    }

    #[test]
    fn test_fail_and_succeed() {
        let fail_result = fail("Some error");
        println!("error code: {:?}",fail_result.0);
        assert_eq!(fail_result.0, 1);
        assert_eq!(fail_result.1, "Some error");

        let success_status = succeed();
        assert_eq!(success_status, 0);
    }

    #[test]
    fn test_build_word() {
        let mut chars = "\"hello world\"; more".chars().peekable();
        assert_eq!(build_word(&mut chars, false, false, String::new()), "hello world");
        assert_eq!(chars.collect::<String>(), "; more");

        let mut chars = "'single quoted'; other".chars().peekable();
        assert_eq!(build_word(&mut chars, false, false, String::new()), "single quoted");
        assert_eq!(chars.collect::<String>(), "; other");

        let mut chars = "plainword<>next".chars().peekable();
        assert_eq!(build_word(&mut chars, false, false, String::new()), "plainword");
        assert_eq!(chars.collect::<String>(), "<>next");
    }

    #[test]
    fn test_read_bytes() {
        let input = b"test data";
        let mut cursor = Cursor::new(input);
        let result = read_bytes(&mut cursor).unwrap();
        assert_eq!(result, input.to_vec());
    }

    #[test]
    fn test_write_bytes() {
        let mut buffer = Vec::new();
        write_bytes(&mut buffer, b"test write").unwrap();
        assert_eq!(buffer, b"test write");
    }

    #[test]
    fn test_write_stdout() {
        let output = b"stdout test\n";
        assert!(write_stdout(output.to_vec()).is_ok());
    }

    #[test]
    fn test_write_stderr() {
        let output = b"stderr test\n";
        assert!(write_stderr(output.to_vec()).is_ok());
    }
}
