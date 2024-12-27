use std::env;
use std::collections::{HashSet,VecDeque,HashMap};
use std::ffi::CString;
use std::fs::File;
use std::io::Read;
use std::os::fd::{AsFd,BorrowedFd, RawFd};
use std::path::{Path, PathBuf};

use bitflags::bitflags;
use libc::STDERR_FILENO;
use log::{debug, info, trace};
use nix::unistd::write;

use crate::event::{ShellError, ShellErrorFull};
use crate::execute::{NodeWalker, RshWaitStatus};
use crate::interp::expand::expand_var;
use crate::interp::helper;
use crate::interp::parse::{descend, Node, Span};

bitflags! {
	#[derive(Debug,Copy,Clone,PartialEq)]
	pub struct EnvFlags: u32 {
		// Guard conditions against infinite alias/var/function recursion
		const NO_ALIAS    = 0b00000000000001;
		const NO_VAR      = 0b00000000000010;
		const NO_FUNC     = 0b00000000000100;

		// Context
		const IN_FUNC     = 0b00000000001000; // Enables the `return` builtin
		const LOGIN_SHELL = 0b00000000010000;
		const INTERACTIVE = 0b00000000100000;
	}
}

#[derive(Debug,PartialEq,Clone)]
pub struct ShellEnv {
	pub flags: EnvFlags,
	pub env_vars: HashMap<String, String>,
	pub variables: HashMap<String, String>,
	pub aliases: HashMap<String, String>,
	pub shopts: HashMap<String,usize>,
	pub functions: HashMap<String, Box<Node>>,
	pub parameters: HashMap<String, String>,
	pub open_fds: HashSet<i32>,
	pub last_input: Option<String>
}

impl ShellEnv {
	// Constructor
	pub fn new(login: bool, interactive: bool) -> Self {
		let mut open_fds = HashSet::new();
		let stderr = helper::get_stderr();
		let shopts = init_shopts();
		let mut flags = EnvFlags::empty();
		// TODO: probably need to find a way to initialize env vars that doesnt rely on a parent process
		let mut env_vars = std::env::vars().collect::<HashMap<String,String>>();
		env_vars.insert("HIST_FILE".into(),"${HOME}/.rsh_hist".into());

		if login { flags |= EnvFlags::LOGIN_SHELL; }
		if interactive { flags |= EnvFlags::INTERACTIVE; }
		open_fds.insert(0);
		open_fds.insert(1);
		open_fds.insert(2);
		let mut shellenv = Self {
			flags: EnvFlags::empty(),
			env_vars,
			variables: HashMap::new(),
			aliases: HashMap::new(),
			shopts,
			functions: HashMap::new(),
			parameters: HashMap::new(),
			open_fds,
			last_input: None
		};
		let runtime_commands_path = &expand_var(&shellenv, "${HOME}/.rshrc".into());
		let runtime_commands_path = Path::new(runtime_commands_path);
		if runtime_commands_path.exists() {
				if let Err(e) = shellenv.source_file(runtime_commands_path.to_path_buf(), Span::new()) {
					let err = ShellErrorFull::from(shellenv.get_last_input(),e);
					write(stderr,format!("Failed to source ~/.rshrc: {}",err).as_bytes()).unwrap();
				}
		} else {
				eprintln!("Warning: Runtime commands file '{}' not found.", runtime_commands_path.display());
		}
    if login {
			let profile_path = expand_var(&shellenv, "${HOME}/.rsh_profile".into());
			let profile_path = Path::new(&profile_path);
			if profile_path.exists() {
				if let Err(e) = shellenv.source_file(profile_path.to_path_buf(), Span::new()) {
					let err = ShellErrorFull::from(shellenv.get_last_input(),e);
					write(stderr,format!("Failed to source ~/.rshrc: {}",err).as_bytes()).unwrap();
				}
			}
    }
		shellenv
	}

	pub fn mod_flags<F>(&mut self, transform: F) where F: FnOnce(&mut EnvFlags) {
		transform(&mut self.flags)
	}

	pub fn is_interactive(&self) -> bool {
		self.flags.contains(EnvFlags::INTERACTIVE)
	}

	pub fn is_login(&self) -> bool {
		self.flags.contains(EnvFlags::LOGIN_SHELL)
	}

	pub fn set_last_input(&mut self, input: &str) {
		self.last_input = Some(input.to_string())
	}

	pub fn get_last_input(&mut self) -> String {
		self.last_input.clone().unwrap_or_default()
	}

	pub fn source_file(&mut self, path: PathBuf, span: Span) -> Result<(),ShellError> {
		let mut file = File::open(&path).map_err(|e| ShellError::from_io(&e.to_string(), span))?;
		let mut buffer = String::new();
		let stderr = unsafe { BorrowedFd::borrow_raw(STDERR_FILENO) };
		file.read_to_string(&mut buffer).map_err(|e| ShellError::from_io(&e.to_string(), span))?;
		self.last_input = Some(buffer.clone());


		let state = descend(&buffer, self);
		match state {
			Ok(parse_state) => {
				let mut walker = NodeWalker::new(parse_state.ast, self);
				match walker.start_walk() {
					Ok(code) => {
						info!("Last exit status: {:?}", code);
						if let RshWaitStatus::Fail { code, cmd, span } = code {
							if code == 127 {
								if let Some(cmd) = cmd {
									let err = ShellErrorFull::from(self.get_last_input(),ShellError::from_no_cmd(&format!("Command not found: {}",cmd), span));
									write(stderr, format!("{}", err).as_bytes()).unwrap();
								}
							}
						}
					}
					Err(e) => {
						let err = ShellErrorFull::from(self.get_last_input(), e);
						write(stderr.as_fd(), format!("{}", err).as_bytes()).unwrap();
					}
				}
			}
			Err(e) => {
				let err = ShellErrorFull::from(self.get_last_input(), e);
				write(stderr.as_fd(), format!("Encountered error while sourcing file: {}\n{}",path.display(), err).as_bytes()).unwrap();
			}
		}
		Ok(())
	}

	pub fn change_dir(&mut self, path: &Path, span: Span) -> Result<(), ShellError> {
		self.export_variable("PWD".into(), path.to_str().unwrap().to_string());
		match env::set_current_dir(path) {
			Ok(_) => Ok(()),
			Err(e) => Err(ShellError::from_execf(&e.to_string(), 1, span))
		}
	}

	pub fn open_fd(&mut self, fd: RawFd) {
		self.open_fds.insert(fd);
	}

	pub fn close_fd(&mut self, fd: RawFd) {
		self.open_fds.remove(&fd);
	}

	pub fn last_exit_status(&mut self, code: i32) {
		todo!("this will set $? to the exit code of the most recently exited process")
	}

	pub fn set_interactive(&mut self, interactive: bool) {
		if interactive {
			self.flags |= EnvFlags::INTERACTIVE
		} else {
			self.flags &= !EnvFlags::INTERACTIVE
		}
	}

	// Getters and Setters for `variables`
	pub fn get_variable(&self, key: &str) -> Option<String> {
		if let Some(value) = self.variables.get(key) {
			Some(value.to_string())
		} else if let Some(value) = self.env_vars.get(key) {
			Some(value.to_string())
		} else {
			self.get_parameter(key).map(|val| val.to_string())
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

	pub fn set_variable(&mut self, key: String, mut value: String) {
		debug!("inserted var: {} with value: {}",key,value);
		if value.starts_with('"') && value.ends_with('"') {
			value = value.strip_prefix('"').unwrap().into();
			value = value.strip_suffix('"').unwrap().into();
		}
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
		if self.flags.contains(EnvFlags::NO_ALIAS) {
			return None
		}
		self.aliases.get(key)
	}

	pub fn set_alias(&mut self, key: String, mut value: String) -> Result<(), String> {
		if self.get_function(key.as_str()).is_some() {
			return Err(format!("The name `{}` is already being used as a function",key))
		}
		if value.starts_with('"') && value.ends_with('"') {
			value = value.strip_prefix('"').unwrap().into();
			value = value.strip_suffix('"').unwrap().into();
		}
		self.aliases.insert(key, value);
		Ok(())
	}

	pub fn remove_alias(&mut self, key: &str) -> Option<String> {
		self.aliases.remove(key)
	}

	// Getters and Setters for `functions`
	pub fn get_function(&self, name: &str) -> Option<Box<Node>> {
		self.functions.get(name).cloned()
	}

	pub fn set_function(&mut self, name: String, body: Box<Node>) -> Result<(),ShellError> {
		if self.get_alias(name.as_str()).is_some() {
			return Err(ShellError::from_parse(format!("The name `{}` is already being used as an alias",name).as_str(), body.span()))
		}
		self.functions.insert(name, body);
		Ok(())
	}

	pub fn remove_function(&mut self, name: &str) -> Option<Box<Node>> {
		self.functions.remove(name)
	}

	// Getters and Setters for `parameters`
	pub fn get_parameter(&self, key: &str) -> Option<&String> {
		self.parameters.get(key)
	}

	pub fn set_parameter(&mut self, key: String, value: String) {
		self.parameters.insert(key, value);
	}

	pub fn clear_pos_parameters(&mut self) {
		let mut index = 1;
		while let Some(_value) = self.get_parameter(index.to_string().as_str()) {
			self.parameters.remove(index.to_string().as_str());
			index += 1;
		}
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
