use std::{collections::HashMap, env, io::Read, path::PathBuf, sync::{Arc, LazyLock}};

use bitflags::bitflags;
use nix::{sys::signal::{self, Signal}, unistd::{gethostname, Pid, User}};
use std::{fs::File, sync::RwLock};

use crate::{event::ShError, execute::{ExecUnit, RshWait}, interp::parse::{descend, Node}, RshResult};

pub static JOBS: LazyLock<Arc<RwLock<JobTable>>> = LazyLock::new(|| {
	Arc::new(
		RwLock::new(
			JobTable::new()
		)
	)
});

pub static VARS: LazyLock<Arc<RwLock<VarTable>>> = LazyLock::new(|| {
	Arc::new(
		RwLock::new(
			VarTable::new()
		)
	)
});

pub static LOGIC: LazyLock<Arc<RwLock<LogicTable>>> = LazyLock::new(|| {
	Arc::new(
		RwLock::new(
			LogicTable::new()
		)
	)
});

pub static META: LazyLock<Arc<RwLock<EnvMeta>>> = LazyLock::new(|| {
	Arc::new(
		RwLock::new(
			EnvMeta::new(EnvFlags::empty())
		)
	)
});



bitflags! {
	#[derive(Debug,Copy,Clone,PartialEq)]
	pub struct EnvFlags: u32 {
		// Guard conditions against infinite alias/var/function recursion
		const NO_ALIAS         = 0b00000000000000000000000000000001;
		const NO_VAR           = 0b00000000000000000000000000000010;
		const NO_FUNC          = 0b00000000000000000000000000000100;

		// Context
		const IN_FUNC          = 0b00000000000000000000000000001000; // Enables the `return` builtin
		const INTERACTIVE      = 0b00000000000000000000000000010000;
		const CLEAN            = 0b00000000000000000000000000100000; // Do not inherit env vars from parent
		const NO_RC            = 0b00000000000000000000000001000000;

		// Options set by 'set' command
		const EXPORT_ALL_VARS  = 0b00000000000000000000000010000000; // set -a
		const REPORT_JOBS_ASAP = 0b00000000000000000000000100000000; // set -b
		const EXIT_ON_ERROR    = 0b00000000000000000000001000000000; // set -e
		const NO_GLOB          = 0b00000000000000000000010000000000; // set -f
		const HASH_CMDS        = 0b00000000000000000000100000000000; // set -h
		const ASSIGN_ANYWHERE  = 0b00000000000000000001000000000000; // set -k
		const ENABLE_JOB_CTL   = 0b00000000000000000010000000000000; // set -m
		const NO_EXECUTE       = 0b00000000000000000100000000000000; // set -n
		const ENABLE_RSHELL    = 0b00000000000000001000000000000000; // set -r
		const EXIT_AFTER_EXEC  = 0b00000000000000010000000000000000; // set -t
		const UNSET_IS_ERROR   = 0b00000000000000100000000000000000; // set -u
		const PRINT_INPUT      = 0b00000000000001000000000000000000; // set -v
		const STACK_TRACE      = 0b00000000000010000000000000000000; // set -x
		const EXPAND_BRACES    = 0b00000000000100000000000000000000; // set -B
		const NO_OVERWRITE     = 0b00000000001000000000000000000000; // set -C
		const INHERIT_ERR      = 0b00000000010000000000000000000000; // set -E
		const HIST_SUB         = 0b00000000100000000000000000000000; // set -H
		const NO_CD_SYMLINKS   = 0b00000001000000000000000000000000; // set -P
		const INHERIT_RET      = 0b00000010000000000000000000000000; // set -T
	}
	#[derive(Debug,Copy,Clone)]
	pub struct JobFlags: i8 {
		const LONG      = 0b00000001;
		const PIDS      = 0b00000010;
		const NEW_ONLY  = 0b00000100;
		const RUNNING   = 0b00001000;
		const STOPPED   = 0b00010000;
		const INIT      = 0b00100000;
	}
}

trait RshType {}
trait RshScalar {}
trait RshComposite {}

impl RshType for String {}
impl RshScalar for String {}

impl RshType for i32 {}
impl RshScalar for i32 {}

impl RshType for f64 {}
impl RshScalar for f64 {}

impl RshType for bool {}
impl RshScalar for bool {}

impl<T: RshType> RshType for Vec<T> {}
impl<T: RshType> RshComposite for Vec<T> {}

impl<K: RshType, V: RshType> RshType for HashMap<K,V> {}
impl<K: RshType, V: RshType> RshComposite for HashMap<K,V> {}

#[derive(Debug,Clone)]
pub struct Job {
	job_id: i32,
	pids: Vec<Pid>,
	commands: Vec<String>,
	pgid: Pid,
	statuses: Vec<RshWait>,
	active: bool,
}

impl Job {
	pub fn new(job_id: i32, pids: Vec<Pid>, commands: Vec<String>, pgid: Pid) -> Self {
		let num_pids = pids.len();
		Self { job_id, pgid, pids, commands, statuses: vec![RshWait::Running;num_pids], active: true }
	}
	pub fn is_active(&self) -> bool {
		self.active
	}
	pub fn update_from_pid(&mut self, pid: Pid, new_stat: RshWait) {
		for (index,job_pid) in self.pids.iter().enumerate() {
			if pid == *job_pid {
				self.update_status(index, new_stat);
				break
			}
		}
	}
	pub fn update_status(&mut self, pid_index: usize, new_stat: RshWait) {
			if pid_index < self.statuses.len() {
					self.statuses[pid_index] = new_stat;
			} else {
					eprintln!("Error: Invalid pid_index {} for statuses", pid_index);
					// Alternatively, return a Result to signal the error.
			}
	}
	pub fn pids(&self) -> &[Pid] {
		&self.pids
	}
	pub fn pgid(&self) -> &Pid {
		&self.pgid
	}
	pub fn get_proc_statuses(&self) -> &[RshWait] {
		&self.statuses
	}
	pub fn id(&self) -> i32 {
		self.job_id
	}
	pub fn commands(&self) -> Vec<String> {
		self.commands.clone()
	}
	pub fn deactivate(&mut self) {
		self.active = false;
	}
	pub fn signal_proc(&self, sig: Signal) -> RshResult<()> {
		if self.pids().len() == 1 {
			let pid = *self.pids().first().unwrap();
				signal::kill(pid, sig).map_err(|_| ShError::from_io())
		} else {
			signal::killpg(self.pgid, sig).map_err(|_| ShError::from_io())
		}
	}
	pub fn print(&self, current: Option<i32>, flags: JobFlags) -> String {
		let long = flags.contains(JobFlags::LONG);
		let init = flags.contains(JobFlags::INIT);
		let pids = flags.contains(JobFlags::PIDS);
		let mut output = String::new();

		const GREEN: &str = "\x1b[32m";
		const RED: &str = "\x1b[31m";
		const CYAN: &str = "\x1b[35m";
		const RESET: &str = "\x1b[0m";

		// Add job ID and status
		let symbol = if current.is_some_and(|cur| cur == self.job_id) {
			"+"
		} else if current.is_some_and(|cur| cur == self.job_id + 1) {
			"-"
		} else {
			" "
		};
		output.push_str(&format!("[{}]{} ", self.job_id, symbol));
		let padding_num = symbol.len() + self.job_id.to_string().len() + 3;
		let padding: String = " ".repeat(padding_num);

		// Add commands and PIDs
		for (i, cmd) in self.commands.iter().enumerate() {
			let pid = if pids || init {
				let mut pid = self.pids.get(i).unwrap().to_string();
				pid.push(' ');
				pid
			} else {
				"".to_string()
			};
			let cmd = cmd.clone();
			let mut status0 = if init { "".into() } else { self.statuses.get(i).unwrap().to_string() };
			if status0.len() < 6 && !status0.is_empty() {
				// Pad out the length so that formatting is uniform
				let diff = 6 - status0.len();
				let pad = " ".repeat(diff);
				status0.push_str(&pad);
			}
			let status1 = format!("{}{}",pid,status0);
			let status2 = format!("{}\t{}",status1,cmd);
			let mut status_final = if status0.starts_with("done") {
				format!("{}{}{}",GREEN,status2,RESET)
			} else if status0.starts_with("exit") {
				format!("{}{}{}",RED,status2,RESET)
			} else {
				format!("{}{}{}",CYAN,status2,RESET)
			};
			if i != self.commands.len() - 1 {
				status_final.push_str(" |");
			}
			status_final.push('\n');
			let status_line = if long {
				// Long format includes PIDs
				format!(
						"{}{} {}",
						if i != 0 { padding.clone() } else { "".into() },
						self.pids().get(i).unwrap(),
						status_final
				)
			} else {
				format!(
						"{}{}",
						if i != 0 { padding.clone() } else { "".into() },
						status_final
				)
			};
			output.push_str(&status_line);
		}

		output
	}

}


#[derive(Clone,Debug)]
pub struct JobTable {
	fg: Option<Job>,
	jobs: HashMap<i32,Job>,
	curr_job: Option<i32>,
	updated_since_check: Vec<i32>,
}

impl JobTable {
	fn new() -> Self {
		Self {
			fg: None,
			jobs: HashMap::new(),
			curr_job: None,
			updated_since_check: Vec::new()
		}
	}
	pub fn curr_job(&self) -> Option<i32> {
		self.curr_job
	}
	pub fn mark_updated(&mut self, id: i32) {
		self.updated_since_check.push(id)
	}
	pub fn borrow_jobs(&self) -> &HashMap<i32,Job> {
		&self.jobs
	}
	pub fn get_job(&mut self, id: i32) -> Option<&mut Job> {
		self.jobs.get_mut(&id)
	}
	pub fn new_job(&mut self, pids: Vec<Pid>, commands: Vec<String>, pgid: Pid, fg: bool) {
		let job_id = if fg {
			0
		} else {
			self.jobs.len() + 1
		};
		let job = Job::new(job_id as i32,pids,commands,pgid);
		println!("{}",job.print(Some(job_id as i32), JobFlags::INIT));
		if job_id >= 1 {
			self.jobs.insert(job_id as i32, job);
		}
	}
	pub fn print_jobs(&mut self, flags: &JobFlags) {
		let mut jobs = if flags.contains(JobFlags::NEW_ONLY) {
			self.jobs
				.values()
				.filter(|job| self.updated_since_check.contains(&job.job_id))
				.collect::<Vec<&Job>>()
		} else {
			self.jobs.values().collect::<Vec<&Job>>()
		};
		self.updated_since_check.clear();
		jobs.sort_by_key(|job| job.job_id);
		for job in jobs {
			let id = job.job_id;
			// Filter jobs based on flags
			if flags.contains(JobFlags::RUNNING) && !matches!(job.statuses.get(id as usize).unwrap(), RshWait::Running) {
				continue;
			}
			if flags.contains(JobFlags::STOPPED) && !matches!(job.statuses.get(id as usize).unwrap(), RshWait::Stopped {..}) {
				continue;
			}
			// Print the job in the selected format
			println!("{}", job.print(self.curr_job, *flags));
		}
	}
}

#[derive(Debug,Clone)]
pub enum RshValue {
	String(String),
	Bool(bool),
	Int(i32),
	Float(f64),
	Array(Vec<RshValue>),
	Dict(HashMap<RshValue, RshValue>),
}

#[derive(Debug,Clone)]
pub struct VarTable {
	env: HashMap<String,String>,
	params: HashMap<String,String>,
	strings: HashMap<String,String>,
	bools: HashMap<String,bool>,
	ints: HashMap<String,i32>,
	floats: HashMap<String,f64>,
	arrays: HashMap<String,Vec<RshValue>>,
	dicts: HashMap<String,HashMap<RshValue,RshValue>>
}

impl VarTable {
	pub fn new() -> Self {
		let env = init_env_vars(false);
		Self {
			env,
			params: HashMap::new(),
			strings: HashMap::new(),
			bools: HashMap::new(),
			ints: HashMap::new(),
			floats: HashMap::new(),
			arrays: HashMap::new(),
			dicts: HashMap::new(),
		}
	}

	pub fn borrow_evars(&self) -> &HashMap<String,String> {
		&self.env
	}

	pub fn export_var(&mut self, key: &str, val: &str) {
		self.env.insert(key.into(),val.into());
		std::env::set_var(key, val);
	}

	// Getters and setters for `env`
	pub fn get_evar(&self, key: &str) -> Option<String> {
		self.env.get(key).cloned()
	}
	pub fn set_evar(&mut self, key: &str, value: &str) {
		self.env.insert(key.into(), value.into());
	}

	// Getters and setters for `params`
	pub fn get_param(&self, key: &str) -> Option<String> {
		self.params.get(key).cloned()
	}
	pub fn set_param(&mut self, key: String, value: String) {
		self.params.insert(key, value);
	}
	pub fn reset_params(&mut self) {
		self.params.clear();
	}

	// Getters and setters for `strings`
	pub fn get_string(&self, key: &str) -> Option<String> {
		self.strings.get(key).cloned()
	}
	pub fn set_string(&mut self, key: String, value: String) {
		self.strings.insert(key, value);
	}

	// Getters and setters for `bools`
	pub fn get_bool(&self, key: &str) -> Option<bool> {
		self.bools.get(key).cloned()
	}
	pub fn set_bool(&mut self, key: String, value: bool) {
		self.bools.insert(key, value);
	}

	// Getters and setters for `ints`
	pub fn get_int(&self, key: &str) -> Option<i32> {
		self.ints.get(key).cloned()
	}
	pub fn set_int(&mut self, key: String, value: i32) {
		self.ints.insert(key, value);
	}

	// Getters and setters for `floats`
	pub fn get_float(&self, key: &str) -> Option<f64> {
		self.floats.get(key).cloned()
	}
	pub fn set_float(&mut self, key: String, value: f64) {
		self.floats.insert(key, value);
	}

	// Getters and setters for `arrays`
	pub fn get_array(&self, key: &str) -> Option<Vec<RshValue>> {
		self.arrays.get(key).cloned()
	}
	pub fn set_array(&mut self, key: String, value: Vec<RshValue>) {
		self.arrays.insert(key, value);
	}

	// Getters and setters for `dicts`
	pub fn get_dict(&self, key: &str) -> Option<HashMap<RshValue, RshValue>> {
		self.dicts.get(key).cloned()
	}
	pub fn set_dict(&mut self, key: String, value: HashMap<RshValue, RshValue>) {
		self.dicts.insert(key, value);
	}
}

impl Default for VarTable {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug,Clone)]
pub struct LogicTable {
	functions: HashMap<String,Box<Node>>,
	aliases: HashMap<String,String>
}

impl LogicTable {
	pub fn new() -> Self {
		Self {
			functions: HashMap::new(),
			aliases: HashMap::new()
		}
	}
	pub fn new_alias(&mut self, name: &str, value: String) {
		self.aliases.insert(name.to_string(),value);
	}
	pub fn get_alias(&self, name: &str) -> Option<String> {
		self.aliases.get(name).cloned()
	}
	pub fn new_func(&mut self, name: &str, node: Node) {
		self.functions.insert(name.to_string(),Box::new(node));
	}
	pub fn get_func(&self, name: &str) -> Option<Node> {
		self.functions.get(name).map(|boxed_node| *boxed_node.clone())
	}
}

impl Default for LogicTable {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug,Clone)]
pub struct EnvMeta {
	last_input: String,
	shopts: HashMap<String,usize>,
	flags: EnvFlags,
	num_children: usize,
}

impl EnvMeta {
	pub fn new(flags: EnvFlags) -> Self {
		Self {
			last_input: String::new(),
			shopts: init_shopts(),
			flags,
			num_children: 0,
		}
	}
	pub fn set_last_input(&mut self,input: &str) {
		self.last_input = input.to_string()
	}
	pub fn add_child(&mut self) {
		self.num_children += 1;
	}
	pub fn reap_child(&mut self) {
		self.num_children = self.num_children.saturating_sub(1);
	}
	pub fn children(&self) -> usize {
		self.num_children
	}
	pub fn get_last_input(&self) -> String {
		self.last_input.clone()
	}
	pub fn set_shopt(&mut self, key: &str, val: usize) {
		self.shopts.insert(key.into(),val);
	}
	pub fn get_shopt(&self, key: &str) -> Option<usize> {
		self.shopts.get(&key.to_string()).map(|val| *val)
	}
	pub fn mod_flags<F>(&mut self, flag_mod: F)
	where F: FnOnce(&mut EnvFlags) {
		flag_mod(&mut self.flags)
	}
}

pub fn read_jobs<F,T>(f: F) -> RshResult<T>
where F: FnOnce(&JobTable) -> T {
	let lock = JOBS.read().map_err(|_| ShError::from_internal("Failed to obtain write lock; lock might be poisoned"))?;
	Ok(f(&lock))
}

pub fn write_jobs<F,T>(f: F) -> RshResult<T>
where F: FnOnce(&mut JobTable) -> T {
	let mut lock = JOBS.write().map_err(|_| ShError::from_internal("Failed to obtain write lock; lock might be poisoned"))?;
	Ok(f(&mut lock))
}

pub fn read_vars<F,T>(f: F) -> RshResult<T>
where F: FnOnce(&VarTable) -> T {
	let lock = VARS.read().map_err(|_| ShError::from_internal("Failed to obtain write lock; lock might be poisoned"))?;
	Ok(f(&lock))
}
pub fn write_vars<F,T>(f: F) -> RshResult<T>
where F: FnOnce(&mut VarTable) -> T {
	let mut lock = VARS.write().map_err(|_| ShError::from_internal("Failed to obtain write lock; lock might be poisoned"))?;
	Ok(f(&mut lock))
}
pub fn read_logic<F,T>(f: F) -> RshResult<T>
where F: FnOnce(&LogicTable) -> T {
	let lock = LOGIC.read().map_err(|_| ShError::from_internal("Failed to obtain write lock; lock might be poisoned"))?;
	Ok(f(&lock))
}
pub fn write_logic<F,T>(f: F) -> RshResult<T>
where F: FnOnce(&mut LogicTable) -> T {
	let mut lock = LOGIC.write().map_err(|_| ShError::from_internal("Failed to obtain write lock; lock might be poisoned"))?;
	Ok(f(&mut lock))
}
pub fn read_meta<F,T>(f: F) -> RshResult<T>
where F: FnOnce(&EnvMeta) -> T {
	let lock = META.read().map_err(|_| ShError::from_internal("Failed to obtain write lock; lock might be poisoned"))?;
	Ok(f(&lock))
}
pub fn write_meta<F,T>(f: F) -> RshResult<T>
where F: FnOnce(&mut EnvMeta) -> T {
	let mut lock = META.write().map_err(|_| ShError::from_internal("Failed to obtain write lock; lock might be poisoned"))?;
	Ok(f(&mut lock))
}

pub fn is_interactive() -> RshResult<bool> {
	read_meta(|m| m.flags.contains(EnvFlags::INTERACTIVE))
}

fn init_shopts() -> HashMap<String,usize> {
	let mut shopts = HashMap::new();
	shopts.insert("dotglob".into(),0);
	shopts.insert("trunc_prompt_path".into(),4);
	shopts.insert("int_comments".into(),1);
	shopts.insert("autocd".into(),1);
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

pub struct SavedEnv {
	vars: VarTable,
	logic: LogicTable,
	meta: EnvMeta
}

impl SavedEnv {
	pub fn get_snapshot() -> RshResult<Self> {
		let vars = read_vars(|vars| vars.clone())?;
		let logic = read_logic(|log| log.clone())?;
		let meta = read_meta(|meta| meta.clone())?;
		Ok(Self { vars, logic, meta })
	}
	pub fn restore_snapshot(self) -> RshResult<()> {
		write_vars(|vars| *vars = self.vars)?;
		write_logic(|log| *log = self.logic)?;
		write_meta(|meta| *meta = self.meta)?;
		Ok(())
	}
}

fn init_env_vars(clean: bool) -> HashMap<String,String> {
	let pathbuf_to_string = |pb: Result<PathBuf, std::io::Error>| pb.unwrap_or_default().to_string_lossy().to_string();
	// First, inherit any env vars from the parent process if clean bit not set
	let mut env_vars = HashMap::new();
	if !clean {
		env_vars = std::env::vars().collect::<HashMap<String,String>>();
	}
	let home;
	let username;
	let uid;
	if let Some(user) = User::from_uid(nix::unistd::Uid::current()).ok().flatten() {
		home = user.dir;
		username = user.name;
		uid = user.uid;
	} else {
		home = PathBuf::new();
		username = "unknown".into();
		uid = 0.into();
	}
	let home = pathbuf_to_string(Ok(home));
	let hostname = gethostname().map(|hname| hname.to_string_lossy().to_string()).unwrap_or_default();

	env_vars.insert("HOSTNAME".into(), hostname.clone());
	env::set_var("HOSTNAME", hostname);
	env_vars.insert("UID".into(), uid.to_string());
	env::set_var("UID", uid.to_string());
	env_vars.insert("TMPDIR".into(), "/tmp".into());
	env::set_var("TMPDIR", "/tmp");
	env_vars.insert("TERM".into(), "xterm-256color".into());
	env::set_var("TERM", "xterm-256color");
	env_vars.insert("LANG".into(), "en_US.UTF-8".into());
	env::set_var("LANG", "en_US.UTF-8");
	env_vars.insert("USER".into(), username.clone());
	env::set_var("USER", username.clone());
	env_vars.insert("LOGNAME".into(), username.clone());
	env::set_var("LOGNAME", username);
	env_vars.insert("PWD".into(), pathbuf_to_string(std::env::current_dir()));
	env::set_var("PWD", pathbuf_to_string(std::env::current_dir()));
	env_vars.insert("OLDPWD".into(), pathbuf_to_string(std::env::current_dir()));
	env::set_var("OLDPWD", pathbuf_to_string(std::env::current_dir()));
	env_vars.insert("HOME".into(), home.clone());
	env::set_var("HOME", home.clone());
	env_vars.insert("SHELL".into(), pathbuf_to_string(std::env::current_exe()));
	env::set_var("SHELL", pathbuf_to_string(std::env::current_exe()));
	env_vars.insert("HIST_FILE".into(),format!("{}/.rsh_hist",home));
	env::set_var("HIST_FILE",format!("{}/.rsh_hist",home));
	env_vars
}

pub fn source_file(path: PathBuf) -> RshResult<()> {
	let mut file = File::open(&path).map_err(|_| ShError::from_io())?;
	let mut buffer = String::new();
	file.read_to_string(&mut buffer).map_err(|_| ShError::from_io())?;
	write_meta(|meta| meta.set_last_input(&buffer.clone()))?;

	let state = descend(&buffer)?;
	todo!()
}
