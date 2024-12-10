use std::env;
use std::collections::{HashSet,VecDeque,HashMap};
use std::ffi::CString;
use std::path::PathBuf;

use crate::parser::ASTNode;

#[derive(Debug, Clone)]
pub struct ShellEnv {
    interactive: bool,
    env_vars: HashMap<String, String>,
    variables: HashMap<String, String>,
    aliases: HashMap<String, String>,
    functions: HashMap<String, VecDeque<ASTNode>>,
    parameters: HashMap<String, String>,
    file_descriptors: HashSet<i32>
}

impl ShellEnv {
    // Constructor
    pub fn new(interactive: bool) -> Self {
        let mut file_descriptors = HashSet::new();
        file_descriptors.insert(0);
        file_descriptors.insert(1);
        file_descriptors.insert(2);
        Self {
            interactive,
            env_vars: std::env::vars().collect::<HashMap<String,String>>(),
            variables: HashMap::new(),
            aliases: HashMap::new(),
            functions: HashMap::new(),
            parameters: HashMap::new(),
            file_descriptors
        }
    }

    pub fn source_file(&self, path: PathBuf) {
        todo!("implement logic for sourcing files");
    }

    // Getters and Setters for `interactive`
    pub fn is_interactive(&self) -> bool {
        self.interactive
    }

    pub fn last_exit_status(&mut self, code: i32) {
        todo!("this will set $? to the exit code of the most recently exited process")
    }

    pub fn set_interactive(&mut self, interactive: bool) {
        self.interactive = interactive;
    }

    // Getters and Setters for `variables`
    pub fn get_variable(&self, key: &str) -> Option<&String> {
        self.variables.get(key)
    }

    /// For C FFI calls
    pub fn get_cvars(&self) -> Vec<CString> {
        self.env_vars
            .iter()
            .map(|(key, value)| {
                let env_pair = format!("{}={}", key, value);
                CString::new(env_pair).unwrap() })
            .collect::<Vec<CString>>()
    }

    pub fn set_variable(&mut self, key: String, value: String) {
        self.variables.insert(key, value);
    }

    pub fn remove_variable(&mut self, key: &str) -> Option<String> {
        self.variables.remove(key)
    }

    // Getters and Setters for `aliases`
    pub fn get_alias(&self, key: &str) -> Option<&String> {
        self.aliases.get(key)
    }

    pub fn set_alias(&mut self, key: String, value: String) {
        self.aliases.insert(key, value);
    }

    pub fn remove_alias(&mut self, key: &str) -> Option<String> {
        self.aliases.remove(key)
    }

    // Getters and Setters for `functions`
    pub fn get_function(&self, name: &str) -> Option<&VecDeque<ASTNode>> {
        self.functions.get(name)
    }

    pub fn set_function(&mut self, name: String, body: VecDeque<ASTNode>) {
        self.functions.insert(name, body);
    }

    pub fn remove_function(&mut self, name: &str) -> Option<VecDeque<ASTNode>> {
        self.functions.remove(name)
    }

    // Getters and Setters for `parameters`
    pub fn get_parameter(&self, key: &str) -> Option<&String> {
        self.parameters.get(key)
    }

    pub fn set_parameter(&mut self, key: String, value: String) {
        self.parameters.insert(key, value);
    }

    pub fn remove_parameter(&mut self, key: &str) -> Option<String> {
        self.parameters.remove(key)
    }

    // Utility method to clear the environment
    pub fn clear(&mut self) {
        self.variables.clear();
        self.aliases.clear();
        self.functions.clear();
        self.parameters.clear();
    }
}
