use regex::Regex;

pub fn is_var_declaration(word: String) -> bool {
    let regex = Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_-]*=[^=]+$").unwrap();
    regex.is_match(&word)
}

pub fn extract_var(word: String) -> Result<(String,String),String> {
    if let Some((key, value)) = word.split_once('=') {
        Ok((key.to_string(),value.to_string()))
    } else {
        Err("Error parsing key-value pair".to_string())
    }
}
