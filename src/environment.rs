use std::collections::{HashSet, HashMap};

#[derive(Default)]
pub struct Environment {
    exported_variables: HashSet<String>,
    variables: HashMap<String,String>,
    aliases: HashMap<String,String>
}

impl Environment {
    pub fn new() -> Self {
        let mut environment = Environment {
            exported_variables: HashSet::new(),
            variables: HashMap::new(),
            aliases: HashMap::new()
        };
        for (key,value) in std::env::vars() {
            environment.exported_variables.insert(key.clone());
            environment.variables.insert(key,value);
        }

        environment
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
            if self.exported_variables.contains(key) {
                let _ = self.exported_variables.remove(key);
            }
            let _ = self.variables.remove(key);
            return Ok(());
        }
        Err(1)
    }

    pub fn export_var(&mut self, key: &str, value: &str) {
        self.exported_variables.insert(key.to_string());
        self.variables.insert(key.to_string(),value.to_string());
    }
}
