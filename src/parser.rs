use std::collections::VecDeque;
use log::{debug, trace};

pub fn tokenize_input(input: String) -> VecDeque<String> {
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

pub fn parse_tokens(words: VecDeque<String>) -> VecDeque<VecDeque<String>> {
    debug!("Parsing words: {:?}", words);
    let mut paragraph: VecDeque<VecDeque<String>> = VecDeque::new();
    let mut sentence: VecDeque<String> = VecDeque::new();

    for word in words {
        trace!("Checking word: {}", word);
        let lastchar = word.chars().last();
        if lastchar.is_none() { return VecDeque::new(); } // Word is empty
        match lastchar.unwrap() {
            ';' | '\n' => { // Delimiters
                let word_trimmed = word.trim_end_matches([';','\n']).to_string(); // Cut off delimiters
                sentence.push_back(word_trimmed.trim().to_string());
                trace!("Pushing sentence: {:?}", sentence);
                paragraph.push_back(sentence);
                sentence = VecDeque::new();
            }
            _ => {
                match word.as_str() {
                    " " => { continue; }, // Last line of defense to prevent empty strings
                    _ => {
                        trace!("Pushing word: {}", word);
                        sentence.push_back(word.trim().to_string());
                    }

                }
            }
        }
    }
    paragraph.push_back(sentence);
    debug!("Returning paragraph: {:?}", paragraph);
    paragraph
}
