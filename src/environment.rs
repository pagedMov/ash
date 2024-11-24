use std::collections::HashMap;

#[derive(Default)]
pub struct Environment {
    variables: HashMap<String,String>,
    aliases: HashMap<String,String>
}

impl Environment {
    pub fn new() -> Self {
        let mut environment = Environment {
            variables: HashMap::new(),
            aliases: HashMap::new()
        };
        for (key,value) in std::env::vars() {
            environment.variables.insert(key,value);
        }

        environment.aliases.insert("alias_check".to_string(),"alias_check1 alias_check2".to_string());
        environment.aliases.insert("alias_check1".to_string(),"echo hi;".to_string());
        environment.aliases.insert("alias_check2".to_string(),"echo hello".to_string());
        environment
    }

    pub fn set_alias(&mut self, key: &str, value: &str) {
        self.aliases.insert(key.to_string(),value.to_string());
    }

    pub fn get_alias(&self, key: &str) -> Option<&String> {
        self.aliases.get(key)
    }

    pub fn set_var(&mut self, key: &str, value: &str) {
        self.variables.insert(key.to_string(),value.to_string());
    }

    pub fn get_var(&self, key: &str) -> Option<&String> {
        self.variables.get(key)
    }
}
