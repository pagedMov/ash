use crate::prelude::*;

pub fn expand_glob(pair: Pair<Rule>) -> String {
	let word = pair.as_str();
	let mut result = String::new();
	for entry in glob::glob(word).unwrap() {
		if let Ok(path) = entry {
			result = format!("{} {}",result,path.to_str().unwrap());
		}
	}
	result.trim().to_string()
}
