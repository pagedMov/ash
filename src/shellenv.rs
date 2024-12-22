use std::env;
use std::collections::{HashSet,VecDeque,HashMap};
use std::ffi::CString;
use std::fs::File;
use std::io::Read;
use std::os::fd::{AsFd,BorrowedFd, RawFd};
use std::path::{Path, PathBuf};

use libc::STDERR_FILENO;
use log::{debug, info, trace};
use nix::unistd::write;

use crate::event::ShellError;
use crate::execute::{NodeWalker, RshExitStatus};
use crate::interp::expand::expand_var;
use crate::interp::parse::{descend, Node};

#[derive(Debug, Clone)]
pub struct ShellEnv {
	interactive: bool,
	login: bool,
	env_vars: HashMap<String, String>,
	variables: HashMap<String, String>,
	aliases: HashMap<String, String>,
	shopts: HashMap<String,usize>,
	functions: HashMap<String, VecDeque<Node>>,
	parameters: HashMap<String, String>,
	open_fds: HashSet<i32>
}

impl ShellEnv {
	// Constructor
	pub fn new(login: bool, interactive: bool) -> Result<Self,ShellError> {
		let mut open_fds = HashSet::new();
		let shopts = init_shopts();
		let mut env_vars = std::env::vars().collect::<HashMap<String,String>>();
		env_vars.insert("HIST_FILE".into(),"${HOME}/.rsh_hist".into());
		open_fds.insert(0);
		open_fds.insert(1);
		open_fds.insert(2);
		let mut shellenv = Self {
			interactive,
			login,
			env_vars,
			variables: HashMap::new(),
			aliases: HashMap::new(),
			shopts,
			functions: HashMap::new(),
			parameters: HashMap::new(),
			open_fds
		};
		let runtime_commands_path = &expand_var(&shellenv, "${HOME}/.rshrc".into());
		let runtime_commands_path = Path::new(runtime_commands_path);
		if runtime_commands_path.exists() {
				shellenv.source_file(runtime_commands_path.to_path_buf())?;
		} else {
				eprintln!("Warning: Runtime commands file '{}' not found.", runtime_commands_path.display());
		}
    if login {
        let profile_path = expand_var(&shellenv, "${HOME}/.profile".into());
        let profile_path = Path::new(&profile_path);
        if profile_path.exists() {
            shellenv.source_file(profile_path.to_path_buf())?;
        }
    }
		Ok(shellenv)
	}

	pub fn source_file(&mut self, path: PathBuf) -> Result<(),ShellError> {
		let mut file = File::open(path).map_err(|e| ShellError::IoError(e.to_string()))?;
		let mut buffer = String::new();
		let stderr = unsafe { BorrowedFd::borrow_raw(STDERR_FILENO) };
		file.read_to_string(&mut buffer).map_err(|e| ShellError::IoError(e.to_string()))?;


		let state = descend(&buffer, self);
		match state {
			Ok(parse_state) => {
				let mut walker = NodeWalker::new(parse_state.ast, self);
				match walker.start_walk() {
					Ok(code) => {
						info!("Last exit status: {:?}", code);
						if let RshExitStatus::Fail { code, cmd } = code {
							if code == 127 {
								if let Some(cmd) = cmd {
									write(stderr, format!("Command not found: {}\n", cmd).as_bytes()).unwrap();
								}
							}
						}
					}
					Err(e) => {
						write(stderr.as_fd(), format!("Execution error: {}\n", e).as_bytes()).unwrap();
					}
				}
			}
			Err(e) => {
				write(stderr.as_fd(), format!("Parsing error: {}\n", e).as_bytes()).unwrap();
			}
		}
		Ok(())
	}

	pub fn change_dir(&mut self, path: &Path) {
		self.export_variable("PWD".into(), path.to_str().unwrap().to_string());
		let _ = env::set_current_dir(path);
	}

	pub fn open_fd(&mut self, fd: RawFd) {
		self.open_fds.insert(fd);
	}

	pub fn close_fd(&mut self, fd: RawFd) {
		self.open_fds.remove(&fd);
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
	pub fn get_variable(&self, key: &str) -> Option<String> {
		if let Some(value) = self.variables.get(key) {
			Some(value.to_string())
		} else {
			self.env_vars.get(key).map(|value| value.to_string())
		}
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
		debug!("inserted var: {} with value: {}",key,value);
		self.variables.insert(key.clone(), value);
		trace!("testing variable get: {} = {}", key, self.get_variable(key.as_str()).unwrap())
	}

	pub fn get_shopt(&self, key: &str) -> usize {
		self.shopts[key]
	}

	pub fn set_shopt(&mut self, key: &str, value: usize) {
		self.shopts.insert(key.into(),value);
	}

	pub fn export_variable(&mut self, key: String, value: String) {
		self.variables.insert(key.clone(),value.clone());
		self.env_vars.insert(key,value);
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
	pub fn get_function(&self, name: &str) -> Option<&VecDeque<Node>> {
		self.functions.get(name)
	}

	pub fn set_function(&mut self, name: String, body: VecDeque<Node>) {
		self.functions.insert(name, body);
	}

	pub fn remove_function(&mut self, name: &str) -> Option<VecDeque<Node>> {
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

fn init_shopts() -> HashMap<String,usize> {
	let mut shopts = HashMap::new();
	shopts.insert("dotglob".into(),0);
	shopts.insert("int_comments".into(),1);
	shopts.insert("hist_ignore_dupes".into(),1);
	shopts.insert("max_hist".into(),1000);
	shopts.insert("edit_mode".into(),1);
	shopts.insert("comp_limit".into(),100);
	shopts.insert("auto_hist".into(),1);
	shopts.insert("prompt_highlight".into(),1);
	shopts.insert("tab_stop".into(),4);
	shopts.insert("bell_style".into(),1);
	shopts
}
