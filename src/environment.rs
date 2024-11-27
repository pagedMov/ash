use std::collections::{HashSet, HashMap};
use std::env::{self};

use crate::helper;

#[derive(Debug,PartialEq)]
pub struct InternalError {
    exit_code: i32,
    error_message: String
}

impl InternalError {
    pub fn new(error_message: &str) -> Self {
        let exit_code = 1;
        let error_message = error_message.to_string();
        InternalError { exit_code, error_message }
    }
    pub fn get_code(&self) -> i32 {
        self.exit_code
    }
    pub fn get_msg(&self) -> &str {
        &self.error_message
    }
}

#[derive(Default)]
pub struct Environment {
    interactive: bool,
    variables: HashMap<String,String>,
    aliases: HashMap<String,String>,
    functions: HashMap<String,String>,
    internals: HashMap<String,String>
}

impl Environment {
    pub fn new(interactive: bool) -> Self {
        let mut environment = Environment {
            interactive,
            variables: HashMap::new(),
            functions: HashMap::new(),
            aliases: HashMap::new(),
            internals: HashMap::new()
        };
        for (key,value) in env::vars() {
            environment.variables.insert(key,value);
        }

        environment
    }

    pub fn increment_params(&mut self) -> Result<(),InternalError> {
        if !self.internals.contains_key("num_params") {
            self.set_internal("num_params", "0");
        }
        let num_params = self.get_internal("num_params").map_or("0", |v| v);
        if let Ok(incremented) = helper::increment_string(num_params.to_string()) {
            self.set_internal("num_params", &incremented);
            Ok(())
        } else {
            Err(InternalError::new("Failed to increment number of positional parameters."))
        }

    }

    pub fn set_function(&mut self, key: &str, value: &str) {
        self.functions.insert(key.to_string(),value.to_string());
    }

    pub fn get_function(&self, key: &str) -> Option<&String> {
        self.functions.get(key)
    }

    pub fn set_internal(&mut self, key: &str, value: &str) {
        self.internals.insert(key.to_string(),value.to_string());
    }

    pub fn get_internal(&self, key: &str) -> Option<&String> {
        self.internals.get(key)
    }

    pub fn set_alias(&mut self, key: &str, value: &str) {
        self.aliases.insert(key.to_string(),value.to_string());
    }

    pub fn get_alias(&self, key: &str) -> Option<&String> {
        self.aliases.get(key)
    }

    pub fn unset_alias(&mut self,key: &str) -> Result<(),i32> {
        if self.aliases.contains_key(key) {
            let _ = self.aliases.remove(key);
            return Ok(());
        }
        Err(1)
    }

    pub fn set_var(&mut self, key: &str, value: &str) {
        self.variables.insert(key.to_string(),value.to_string());
    }

    pub fn get_var(&self, key: &str) -> Option<&String> {
        self.variables.get(key)
    }

    pub fn unset_var(&mut self,key: &str) -> Result<(),i32> {
        if self.variables.contains_key(key) {
            env::remove_var(key);
            let _ = self.variables.remove(key);
            return Ok(());
        }
        Err(1)
    }

    pub fn export_var(&mut self, key: &str, value: &str) {
        env::set_var(key, value);
        self.variables.insert(key.to_string(),value.to_string());
    }
}
