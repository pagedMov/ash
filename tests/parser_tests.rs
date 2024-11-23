use rush::parser;
use std::collections::VecDeque;

#[cfg(test)]

// Test for `tokenize_input`
#[test]
fn test_tokenize_input_basic() {
    let input = "echo Hello, world!";
    let expected = vec!["echo", "Hello,", "world!"];
    let tokens = parser::tokenize_input(input.to_string());

    let tokens_vec: Vec<String> = tokens.into();
    assert_eq!(tokens_vec, expected);
}

#[test]
fn test_tokenize_input_with_quotes() {
    let input = "echo \"Hello, world!\"";
    let expected = vec!["echo", "Hello, world!"];
    let tokens = parser::tokenize_input(input.to_string());

    let tokens_vec: Vec<String> = tokens.into();
    assert_eq!(tokens_vec, expected);
}

#[test]
fn test_tokenize_input_with_single_quotes() {
    let input = "echo 'Hello, world!'";
    let expected = vec!["echo", "Hello, world!"];
    let tokens = parser::tokenize_input(input.to_string());

    let tokens_vec: Vec<String> = tokens.into();
    assert_eq!(tokens_vec, expected);
}

#[test]
fn test_tokenize_input_with_special_characters() {
    let input = "echo Hello; world!";
    let expected = vec!["echo", "Hello;", "world!"];
    let tokens = parser::tokenize_input(input.to_string());

    let tokens_vec: Vec<String> = tokens.into();
    assert_eq!(tokens_vec, expected);
}

#[test]
fn test_tokenize_input_with_spaces() {
    let input = "echo  Hello,  world! ";
    let expected = vec!["echo", "Hello,", "world!"];
    let tokens = parser::tokenize_input(input.to_string());

    let tokens_vec: Vec<String> = tokens.into();
    assert_eq!(tokens_vec, expected);
}

#[test]
fn test_tokenize_input_empty() {
    let input = "";
    let expected: Vec<String> = Vec::new();
    let tokens = parser::tokenize_input(input.to_string());

    let tokens_vec: Vec<String> = tokens.into();
    assert_eq!(tokens_vec, expected);
}

// Test for `parse_tokens`
#[test]
fn test_parse_tokens_basic() {
    let input = vec!["echo".to_string(), "Hello,".to_string(), "world!".to_string()];
    let parsed = parser::parse_tokens(input.into());

    let expected: VecDeque<VecDeque<String>> = vec![
        vec!["echo", "Hello,", "world!"]
            .into_iter().map(|s| s.into()).collect()
    ].into_iter().collect();

    assert_eq!(parsed, expected);
}

#[test]
fn test_parse_tokens_with_delimiters() {
    let input = vec!["echo".to_string(), "Hello,".to_string(), "world;".to_string(), "ls".to_string(), "-l".to_string()];
    let parsed = parser::parse_tokens(input.into());

    let expected: VecDeque<VecDeque<String>> = vec![
        vec!["echo", "Hello,", "world"].into_iter().map(|s| s.into()).collect(),
        vec!["ls", "-l"].into_iter().map(|s| s.into()).collect(),
    ].into_iter().collect();

    assert_eq!(parsed, expected);
}

#[test]
fn test_parse_tokens_with_empty_input() {
    let input: VecDeque<String> = VecDeque::new();
    let parsed = parser::parse_tokens(input);

    let expected: VecDeque<VecDeque<String>> = VecDeque::new();
    assert_eq!(parsed, expected);
}

#[test]
fn test_parse_tokens_with_only_semicolons() {
    let input = vec!["echo".to_string(), "Hello;".to_string(), ";".to_string(), "ls".to_string()];
    let parsed = parser::parse_tokens(input.into());

    let expected: VecDeque<VecDeque<String>> = vec![
        vec!["echo", "Hello"].into_iter().map(|s| s.into()).collect(),
        vec!["ls"].into_iter().map(|s| s.into()).collect(),
    ].into_iter().collect();

    assert_eq!(parsed, expected);
}
