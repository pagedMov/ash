use glob::glob;
use log::{trace,debug};
use std::collections::VecDeque;
use crate::interp::token::{TkType,Tk,WordDesc};
use crate::interp::parse::ParseState;
use crate::interp::helper;

pub fn expand(state: ParseState) -> ParseState {
    let mut buffer = VecDeque::new();
    let mut tokens = state.tokens.clone();
    while let Some(tk) = tokens.pop_front() {
        for token in expand_token(tk) {
            buffer.push_back(token);
        }
    }
    tokens = std::mem::take(&mut buffer);
    ParseState {
        input: state.input,
        shellenv: state.shellenv,
        tokens,
        ast: state.ast
    }
}

pub fn check_globs(string: String) -> bool {
    string.chars().any(|t| matches!(t, '?' | '*' | '[' | ']' | '!' | '-'))
}

pub fn expand_token(token: Tk) -> VecDeque<Tk> {
    trace!("expand(): Starting expansion with token: {:?}", token);
    let mut working_buffer: VecDeque<Tk> = VecDeque::new();
    let mut product_buffer: VecDeque<Tk> = VecDeque::new();

    working_buffer.push_back(token.clone());
    while let Some(token) = working_buffer.pop_front() {
        let is_glob = check_globs(token.text().into());
        let is_brace_expansion = helper::is_brace_expansion(&token);
        if !is_glob && !is_brace_expansion {
            // Expand variables - returns original string if none are found
            product_buffer.push_back(token);
        } else if is_brace_expansion {
            trace!("expand(): Beginning brace expansion on {}", token.text());
            // Perform brace expansion
            let expanded = expand_braces(token.text().to_string());
            for expanded_token in expanded {
                working_buffer.push_back(
                    Tk {
                        tk_type: TkType::String,
                        wd: WordDesc {
                            text: expanded_token,
                            span: token.span(),
                            flags: token.flags()
                        }
                    }
                );
            };
        } else if is_glob {
            // Expand glob patterns
            for path in glob(token.text()).unwrap().flatten() {
                working_buffer.push_back(
                    Tk {
                        tk_type: TkType::String,
                        wd: WordDesc {
                            text: path.to_str().unwrap().to_string(),
                            span: token.span(),
                            flags: token.flags()
                        }
                    }
                );
            }
        }
    }
    product_buffer
}

pub fn expand_braces(word: String) -> VecDeque<String> {
    let mut results = VecDeque::new();
    let mut buffer = VecDeque::from(vec![word]);

    while let Some(current) = buffer.pop_front() {
        if let Some((prefix, amble, postfix)) = parse_first_brace(&current) {
            let expanded = expand_amble(amble);
            for part in expanded {
                buffer.push_back(format!("{}{}{}", prefix, part, postfix));
            }
        } else {
            // No braces left to expand
            results.push_back(current);
        }
    }

    results
}

fn parse_first_brace(word: &str) -> Option<(String, String, String)> {
    let mut prefix = String::new();
    let mut amble = String::new();
    let mut postfix = String::new();
    let mut char_iter = word.chars().peekable();
    let mut brace_stack = VecDeque::new();

    // Parse prefix
    while let Some(&c) = char_iter.peek() {
        if c == '{' {
            brace_stack.push_back(c);
            char_iter.next();
            break;
        } else {
            prefix.push(c);
            char_iter.next();
        }
    }

    // Parse amble
    while let Some(&c) = char_iter.peek() {
        match c {
            '{' => {
                brace_stack.push_back(c);
                amble.push(c);
            }
            '}' => {
                brace_stack.pop_back();
                if brace_stack.is_empty() {
                    char_iter.next(); // Consume closing brace
                    break;
                } else {
                    amble.push(c);
                }
            }
            _ => amble.push(c),
        }
        char_iter.next();
    }

    // Parse postfix
    postfix.extend(char_iter);

    if !brace_stack.is_empty() {
        None // Unmatched braces
    } else if !amble.is_empty() {
        Some((prefix, amble, postfix))
    } else {
        None // No braces found
    }
}

fn expand_amble(amble: String) -> VecDeque<String> {
    if amble.contains("..") {
        // Handle range expansion
        if let Some(expanded) = expand_range(&amble) {
            return expanded;
        }
    } else if amble.contains(',') {
        // Handle comma-separated values
        return amble.split(',').map(|s| s.to_string()).collect::<VecDeque<String>>();
    }

    VecDeque::from(vec![amble]) // If no expansion is needed, return as-is
}

fn expand_range(range: &str) -> Option<VecDeque<String>> {
    let parts: Vec<&str> = range.trim_matches('{').trim_matches('}').split("..").collect();
    if let [start, end] = parts.as_slice() {
        if let (Ok(start_num), Ok(end_num)) = (start.parse::<i32>(), end.parse::<i32>()) {
            // Numeric range
            return Some((start_num..=end_num).map(|n| n.to_string()).collect());
        } else if start.len() == 1 && end.len() == 1 {
            // Alphabetic range
            let start_char = start.chars().next().unwrap();
            let end_char = end.chars().next().unwrap();
            return Some(
                (start_char..=end_char)
                    .map(|c| c.to_string())
                    .collect(),
            );
        }
    }

    None // Invalid range
}
