use std::{collections::BTreeMap, env, ffi::{CString, OsStr}, fmt::{self, Debug, Display}, hash::Hash, io::Read, mem::take, os::fd::BorrowedFd, path::PathBuf, str::FromStr, sync::{Arc, Condvar, LazyLock, Mutex}};
use std::collections::HashMap;

use bitflags::bitflags;
use nix::{sys::signal::{self, kill, Signal}, unistd::{gethostname, getpgrp, isatty, tcgetpgrp, tcsetpgrp, Pid, User}};
use once_cell::sync::Lazy;
use std::{fs::File, sync::RwLock};

use crate::{event::{self, ShError}, execute::{self, RshWait}, interp::{helper, parse::{descend, Node, Span}, token::RshTokenizer}, RshResult};

pub static RSH_PGRP: Lazy<Pid> = Lazy::new(Pid::this);

pub static TERM_CTL: LazyLock<Arc<RwLock<TermCtl>>> = LazyLock::new(|| {
	Arc::new(
		RwLock::new(
			TermCtl::new()
		)
	)
});

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
		const IN_SUBSH         = 0b00000000000000000000000010000000; // In a subshell
																																 // Options set by 'set' command
		const EXPORT_ALL_VARS  = 0b00000000000000000000000100000000; // set -a
		const REPORT_JOBS_ASAP = 0b00000000000000000000001000000000; // set -b
		const EXIT_ON_ERROR    = 0b00000000000000000000010000000000; // set -e
		const NO_GLOB          = 0b00000000000000000000100000000000; // set -f
		const HASH_CMDS        = 0b00000000000000000001000000000000; // set -h
		const ASSIGN_ANYWHERE  = 0b00000000000000000010000000000000; // set -k
		const ENABLE_JOB_CTL   = 0b00000000000000000100000000000000; // set -m
		const NO_EXECUTE       = 0b00000000000000001000000000000000; // set -n
		const ENABLE_RSHELL    = 0b00000000000000010000000000000000; // set -r
		const EXIT_AFTER_EXEC  = 0b00000000000000100000000000000000; // set -t
		const UNSET_IS_ERROR   = 0b00000000000001000000000000000000; // set -u
		const PRINT_INPUT      = 0b00000000000010000000000000000000; // set -v
		const STACK_TRACE      = 0b00000000000100000000000000000000; // set -x
		const EXPAND_BRACES    = 0b00000000001000000000000000000000; // set -B
		const NO_OVERWRITE     = 0b00000000010000000000000000000000; // set -C
		const INHERIT_ERR      = 0b00000000100000000000000000000000; // set -E
		const HIST_SUB         = 0b00000001000000000000000000000000; // set -H
		const NO_CD_SYMLINKS   = 0b00000010000000000000000000000000; // set -P
		const INHERIT_RET      = 0b00000100000000000000000000000000; // set -T
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

#[derive(Debug, Clone)]
pub struct HashFloat(pub f64);

impl PartialEq for HashFloat {
	fn eq(&self, other: &Self) -> bool {
		self.0.to_bits() == other.0.to_bits()
	}
}

impl Eq for HashFloat {}

impl Hash for HashFloat {
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
		self.0.to_bits().hash(state)
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RVal {
	String(String),
	Int(i32),
	Float(HashFloat),
	Bool(bool),
	Array(Vec<RVal>),
	Dict(BTreeMap<RVal, RVal>),
}

impl RVal {
	pub fn parse(s: &str) -> Result<Self, String> {
		if let Ok(int) = s.parse::<i32>() {
			return Ok(RVal::Int(int));
		}
		if let Ok(float) = s.parse::<f64>() {
			return Ok(RVal::Float(HashFloat(float)));
		}
		if let Ok(boolean) = s.parse::<bool>() {
			return Ok(RVal::Bool(boolean));
		}
		Ok(RVal::String(s.to_string()))
	}

	pub fn as_os_str(&self) -> Option<&OsStr> {
		match self {
			RVal::String(s) => Some(OsStr::new(s)),
			_ => None, // Only strings can be converted to OsStr
		}
	}

	pub fn as_string(&self) -> Option<&String> {
		if let RVal::String(s) = self {
			Some(s)
		} else {
			None
		}
	}

	pub fn as_int(&self) -> Option<i32> {
		if let RVal::Int(i) = self {
			Some(*i)
		} else {
			None
		}
	}

	pub fn as_float(&self) -> Option<f64> {
		if let RVal::Float(f) = self {
			Some(f.0)
		} else {
			None
		}
	}

	pub fn as_bool(&self) -> Option<bool> {
		if let RVal::Bool(b) = self {
			Some(*b)
		} else {
			None
		}
	}

	pub fn as_array(&self) -> Option<&Vec<RVal>> {
		if let RVal::Array(arr) = self {
			Some(arr)
		} else {
			None
		}
	}

	pub fn as_dict(&self) -> Option<&BTreeMap<RVal, RVal>> {
		if let RVal::Dict(dict) = self {
			Some(dict)
		} else {
			None
		}
	}
}

impl Default for RVal {
	fn default() -> Self {
		RVal::String("".into())
	}
}

impl fmt::Display for RVal {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			RVal::Int(int) => write!(f, "{}", int),
			RVal::String(string) => write!(f, "{}", string),
			RVal::Float(float) => write!(f, "{}", float.0),
			RVal::Bool(bool) => write!(f, "{}", bool),
			RVal::Array(array) => {
				let formatted_array: Vec<String> = array.iter().map(|val| format!("{}", val)).collect();
				write!(f, "{}", formatted_array.join(" "))
			}
			RVal::Dict(dict) => {
				let formatted_dict: Vec<String> = dict
					.iter()
					.map(|(key, value)| format!("{}: {}", key, value))
					.collect();
					write!(f, "{{{}}}", formatted_dict.join(", "))
			}
		}
	}
}

#[derive(Debug,Clone)]
pub struct Job {
	job_id: i32,
	pids: Vec<Pid>,
	commands: Vec<String>,
	pgid: Pid,
	statuses: Vec<RshWait>,
	active: bool,
	saved_statuses: Vec<RshWait>
}

impl Job {
	pub fn new(job_id: i32, pids: Vec<Pid>, commands: Vec<String>, pgid: Pid) -> Self {
		let num_pids = pids.len();
		Self {
			job_id,
			pgid,
			pids,
			commands,
			statuses: vec![RshWait::Running;num_pids],
			active: true,
			saved_statuses: vec![]
		}
	}
	pub fn is_running(&self) -> bool {
		self.statuses.iter().any(|stat| matches!(stat,RshWait::Running))
	}
	pub fn is_stopped(&self) -> bool {
		self.statuses.iter().all(|stat| matches!(stat,RshWait::Stopped {..} | RshWait::Success | RshWait::Fail {..}))
	}
	pub fn stop(&mut self,signal: Signal) {
		self.saved_statuses = take(&mut self.statuses);
		for i in 0..self.saved_statuses.len() {
			if self.saved_statuses.get(i).is_some_and(|stat| matches!(stat, RshWait::Success | RshWait::Fail {..})) {
				self.statuses.push(self.saved_statuses.get(i).unwrap().clone());
			} else {
				self.statuses.push(RshWait::Stopped { sig: signal })
			}
		}
	}
	pub fn cont(&mut self) -> RshResult<()> {
		if self.saved_statuses.is_empty() {
			// Most likely just foregrounding a background process that's already running
			return Ok(())
		}
		if self.statuses.iter().any(|stat| !matches!(stat,RshWait::Stopped {..} | RshWait::Success | RshWait::Fail {..})) {
			return Err(ShError::from_internal("Attempted to continue a job that is already running"))
		}
		self.statuses = take(&mut self.saved_statuses);
		Ok(())
	}
	pub fn is_foreground(&self) -> bool {
		unsafe { tcgetpgrp(BorrowedFd::borrow_raw(0)).unwrap() == self.pgid }
	}
	pub fn update_status(&mut self, pid: Option<Pid>, new_stat: RshWait) {
		if let Some(pid) = pid {
			let child = self.pids().iter().position(|job_pid| *job_pid == pid).unwrap();
			self.statuses[child] = new_stat;
		} else {
			let pids: Vec<Pid> = self.pids().to_vec();
			for (index, _) in pids.into_iter().enumerate() {
				self.statuses[index] = new_stat.clone();
			}
		}
	}
	pub fn pids(&self) -> &[Pid] {
		&self.pids
	}
	pub fn pgid(&self) -> &Pid {
		&self.pgid
	}
	pub fn set_id(&mut self, id: i32) {
		self.job_id = id
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
	pub fn display(&self, job_order: &[i32], flags: JobFlags) -> String {
		let long = flags.contains(JobFlags::LONG);
		let init = flags.contains(JobFlags::INIT);
		let pids = flags.contains(JobFlags::PIDS);
		let current = job_order.last();
		let prev = if job_order.len() > 2 {
			job_order.get(job_order.len() - 2)
		} else { None };
		let mut output = String::new();

		const GREEN: &str = "\x1b[32m";
		const RED: &str = "\x1b[31m";
		const CYAN: &str = "\x1b[35m";
		const RESET: &str = "\x1b[0m";

		// Add job ID and status
		let symbol = if current.is_some_and(|cur| *cur == self.job_id) {
			"+"
		} else if prev.is_some_and(|prev| *prev == self.job_id) {
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
			} else if status0.starts_with("exit") || status0.starts_with("stopped") {
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
	jobs: HashMap<i32,Job>,
	job_order: Vec<i32>,
	updated_since_check: Vec<i32>,
}

impl JobTable {
	fn new() -> Self {
		Self {
			jobs: HashMap::new(),
			job_order: Vec::new(),
			updated_since_check: Vec::new(),

		}
	}
	pub fn update_fg_status(&mut self, pid: Option<Pid>, wait: RshWait) {
		if let Some(ref mut fg_job) = self.get_job(0) {
			fg_job.update_status(pid, wait);
		}
	}
	pub fn update_current(&mut self, id: i32) {
		if let Some(index) = self.job_order.iter().position(|job_id| *job_id == id) {
			self.job_order.remove(index);
		}
		self.job_order.push(id)
	}
	pub fn is_fg(&self,pid: Pid) -> bool {
		self.jobs.get(&0).is_some_and(|job| *job.pgid() == pid)
	}
	pub fn job_order(&self) -> &[i32] {
		&self.job_order
	}
	pub fn num_jobs(&self) -> usize {
		self.jobs.len()
	}
	pub fn update_by_pgid(&mut self, pgid: Pid, pid: Option<Pid>, wait: RshWait) {
		if let Some(job) = self.mut_by_pgid(pgid) {
			job.update_status(pid, wait);
		}
	}
	pub fn update_by_id(&mut self, id: i32, pid: Option<Pid>, wait: RshWait) {
		if let Some(job) = self.jobs.get_mut(&id) {
			job.update_status(pid, wait);
		}
	}
	pub fn num_running(&self) -> usize {
		let mut num_running = 0;
		for job in self.jobs.keys() {
			for status in &self.jobs.get(job).unwrap().statuses {
				if *status == RshWait::Running {
					num_running += 1;
				}
			}
		}
		num_running
	}
	pub fn new_fg(&mut self, job: Job) {
		self.jobs.insert(0,job);
	}
	pub fn to_background(&mut self) -> usize {
		let job_id = self.num_jobs();
		let job = self.jobs.remove(&0);
		if let Some(mut fg_job) = job {
			self.update_current(job_id as i32);
			fg_job.set_id(job_id as i32);
			println!("{}",fg_job.display(&self.job_order, JobFlags::PIDS));
			self.jobs.insert(job_id as i32,fg_job);
		}
		job_id
	}
	pub fn to_foreground(&mut self, job_id: i32) {
		if let Some(mut job) = self.jobs.remove(&job_id) {
			job.set_id(0);
			self.new_fg(job);
		}
	}
	pub fn mark_updated(&mut self, id: i32) {
		self.updated_since_check.push(id)
	}
	pub fn read_by_command(&self, string: &str) -> Vec<Job> {
		let mut matches = vec![];
		for id in self.jobs.keys() {
			let job = self.jobs.get(id).unwrap();
			for command in job.commands() {
				if command.to_lowercase().contains(&string.to_lowercase()) {
					matches.push(job.clone());
				}
			}
		}
		matches
	}
	pub fn borrow_jobs(&self) -> &HashMap<i32,Job> {
		&self.jobs
	}
	pub fn get_job(&mut self, id: i32) -> Option<&mut Job> {
		self.jobs.get_mut(&id)
	}
	pub fn read_job(&self, id: i32) -> Option<&Job> {
		self.jobs.get(&id)
	}
	pub fn get_by_pgid(&self, pgid: Pid) -> Option<&Job> {
		self.jobs.values()
			.find(|job| pgid == job.pgid)
	}
	pub fn get_by_pid(&self, pid: Pid) -> Option<&Job> {
		self.jobs.values().find(|job| {
			job.pids.contains(&pid)
		})
	}
	pub fn mut_by_pgid(&mut self, pgid: Pid) -> Option<&mut Job> {
		self.jobs.iter_mut()
			.map(|(_, job)| job)
			.find(|job| pgid == job.pgid)
	}
	pub fn is_finished(&self, pgid: Pid) -> bool {
		let job = self.get_by_pgid(pgid);
		if let Some(job) = job {
			for status in &job.statuses {
				if *status == RshWait::Running {
					return false
				}
			}
		}
		true
	}
	pub fn complete(&mut self, job_id: usize) {
		if let Some(job) = self.jobs.get_mut(&(job_id as i32)) {
			job.statuses.iter_mut().for_each(|stat| *stat = RshWait::Success);
		}
	}
	pub fn new_job(&mut self, pids: Vec<Pid>, commands: Vec<String>, pgid: Pid) -> usize {
		let job_id = self.jobs.len() + 1;
		let job = Job::new(job_id as i32,pids,commands,pgid);
		if job_id >= 1 {
			println!("{}",job.display(&self.job_order,JobFlags::INIT));
		}
		self.jobs.insert(job_id as i32, job);
		self.update_current(job_id as i32);
		job_id
	}
	pub fn reset_recents(&mut self) {
		self.updated_since_check.clear()
	}
	pub fn is_a_job(&self, pid: &Pid) -> bool {
		for job in &self.jobs {
			for job_pid in &job.1.pids {
				if pid == job_pid {
					return true
				}
			}
		}
		false
	}
	pub fn print_jobs(&self, flags: &JobFlags) {
		let mut jobs = if flags.contains(JobFlags::NEW_ONLY) {
			self.jobs
				.values()
				.filter(|job| self.updated_since_check.contains(&job.job_id))
				.collect::<Vec<&Job>>()
		} else {
			self.jobs.values().collect::<Vec<&Job>>()
		};
		jobs.sort_by_key(|job| job.job_id);
		for job in jobs {
			// Skip foreground job
			if job.id() == 0 {
				continue
			}
			let id = job.job_id;
			// Filter jobs based on flags
			if flags.contains(JobFlags::RUNNING) && !matches!(job.statuses.get(id as usize).unwrap(), RshWait::Running) {
				continue;
			}
			if flags.contains(JobFlags::STOPPED) && !matches!(job.statuses.get(id as usize).unwrap(), RshWait::Stopped {..}) {
				continue;
			}
			// Print the job in the selected format
			println!("{}", job.display(&self.job_order,*flags));
		}
	}
}


#[derive(Debug,Clone)]
pub struct VarTable {
	env: HashMap<String,String>,
	params: HashMap<String,String>,
	vars: HashMap<String,RVal>
}

impl VarTable {
	pub fn new() -> Self {
		let env = init_env_vars(false);
		Self {
			env,
			params: HashMap::new(),
			vars: HashMap::new()
		}
	}

	pub fn borrow_evars(&self) -> &HashMap<String, String> {
		&self.env
	}

	// Getters and setters for `env`
	pub fn get_evar(&self, key: &str) -> Option<String> {
		self.env.get(key).cloned().map(|evar| evar.to_string())
	}
	pub fn export_var(&mut self, key: &str, val: &str) {
		let value = val.trim_matches(['"', '\'']).to_string();
		self.env.insert(key.into(), value.clone());
		std::env::set_var(key, value);
	}
	pub fn unset_evar(&mut self, key: &str) {
		self.env.remove(key);
		std::env::remove_var(key);
	}

	// Getters, setters, and unsetters for `params`
	pub fn get_param(&self, key: &str) -> Option<String> {
		self.params.get(key).cloned().map(|param| param.to_string())
	}
	pub fn set_param(&mut self, key: String, value: String) {
		self.params.insert(key, value);
	}
	pub fn reset_params(&mut self) {
		self.params.clear();
	}
	pub fn unset_param(&mut self, key: &str) {
		self.params.remove(key);
	}

	pub fn set_var(&mut self, key: &str, val: RVal) {
		self.vars.insert(key.to_string(),val);
	}
	pub fn unset_var(&mut self, key: &str) {
		self.vars.remove(key);
	}
	pub fn get_var(&self, key: &str) -> Option<RVal> {
		self.vars.get(key).cloned()
	}

	pub fn index_arr(&self, key: &str, index: usize) -> RshResult<RVal> {
		if let Some(var) = self.vars.get(key) {
			if let RVal::Array(arr) = var {
				if let Some(value) = arr.get(index) {
					Ok(value.clone())
				} else {
					Err(ShError::from_execf(format!("Index `{}` out of range for array `{}`",index,key).as_str(), 1, Span::new()))
				}
			} else {
				Err(ShError::from_execf(format!("{} is not an array",key).as_str(), 1, Span::new()))
			}
		} else {
			Err(ShError::from_execf(format!("{} is not a variable",key).as_str(), 1, Span::new()))
		}
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
	pub fn remove_alias(&mut self, name: &str) {
		self.aliases.remove(name);
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
	in_prompt: bool
}

impl EnvMeta {
	pub fn new(flags: EnvFlags) -> Self {
		let in_prompt = flags.contains(EnvFlags::INTERACTIVE);
		Self {
			last_input: String::new(),
			shopts: init_shopts(),
			flags,
			in_prompt
		}
	}
	pub fn leave_prompt(&mut self) {
		self.in_prompt = false
	}
	pub fn enter_prompt(&mut self) {
		self.in_prompt = true
	}
	pub fn set_last_input(&mut self,input: &str) {
		self.last_input = input.to_string()
	}
	pub fn get_last_input(&self) -> String {
		self.last_input.clone()
	}
	pub fn set_shopt(&mut self, key: &str, val: usize) {
		self.shopts.insert(key.into(),val);
	}
	pub fn get_shopt(&self, key: &str) -> Option<usize> {
		self.shopts.get(key).copied()
	}
	pub fn mod_flags<F>(&mut self, flag_mod: F)
		where F: FnOnce(&mut EnvFlags) {
			flag_mod(&mut self.flags)
	}
	pub fn flags(&self) -> EnvFlags {
		self.flags
	}
}

pub struct TermCtl {
	rsh_pgrp: Pid,
	term_ctl_status: Arc<(Mutex<bool>, Condvar)>
}

impl TermCtl {
	pub fn new() -> Self {
		let rsh_pgrp = getpgrp();
		let term_ctl_status = Arc::new((Mutex::new(false), Condvar::new()));
		Self { rsh_pgrp, term_ctl_status }
	}

	pub fn wait_for_terminal_control(&self) {
		let (lock, cvar) = &*self.term_ctl_status;
		let mut has_control = lock.lock().unwrap();

		while !*has_control {
			if term_controller() == self.rsh_pgrp {
				*has_control = true;
				break;
			}
			// Pass the MutexGuard directly (remove the `&mut`)
			has_control = cvar.wait(has_control).unwrap();
		}
	}

	pub fn notify_terminal_control(&self) {
		let (lock, cvar) = &*self.term_ctl_status;
		let mut has_control = lock.lock().unwrap();
		*has_control = true;
		cvar.notify_all();
	}

	pub fn my_pgrp(&self) -> Pid {
		self.rsh_pgrp
	}
}

pub fn attach_tty(pgid: Pid) -> RshResult<()> {
	// Ensure stdin (fd 0) is a tty before proceeding
	if !isatty(0).unwrap_or(false) {
		return Ok(());
	}

	if pgid == my_pgrp()? {
		notify_term_ctl()?
	}

	// Attempt to set the process group for the terminal
	unsafe {
		tcsetpgrp(BorrowedFd::borrow_raw(0), pgid)
			.map_err(|e| ShError::from_internal(format!("Failed to attach tty to pgid {}: {}", pgid, e).as_str()))
	}
}

pub fn term_controller() -> Pid {
	unsafe { tcgetpgrp(BorrowedFd::borrow_raw(0)) }.unwrap()
}

pub fn try_prompt() -> RshResult<()> {
	let in_prompt = read_meta(|m| m.in_prompt)?;
	let is_interactive = read_meta(|m| m.flags.contains(EnvFlags::INTERACTIVE))?;
	if !is_interactive {
		return Ok(())
	}
	if !in_prompt {
		event::fire_prompt()?;
	} else { /* Do nothing */ }
	Ok(())
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
pub fn await_term_ctl() -> RshResult<()> {
	let term_ctl = TERM_CTL.read().map_err(|_| ShError::from_internal("Failed to obtain write lock; lock might be poisoned"))?;
	term_ctl.wait_for_terminal_control();
	Ok(())
}
pub fn notify_term_ctl() -> RshResult<()> {
	let term_ctl = TERM_CTL.read().map_err(|_| ShError::from_internal("Failed to obtain write lock; lock might be poisoned"))?;
	term_ctl.notify_terminal_control();
	Ok(())
}
pub fn my_pgrp() -> RshResult<Pid> {
	let term_ctl = TERM_CTL.read().map_err(|_| ShError::from_internal("Failed to obtain write lock; lock might be poisoned"))?;
	Ok(term_ctl.my_pgrp())
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
	shopts.insert("tab_stop".into(),8);
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

pub fn get_cstring_evars() -> RshResult<Vec<CString>> {
	let env = read_vars(|v| v.borrow_evars().clone())?;
	let env = env.iter().map(|(k,v)| CString::new(format!("{}={}",k,v).as_str()).unwrap()).collect::<Vec<CString>>();
	Ok(env)
}

pub fn source_file(path: PathBuf) -> RshResult<()> {
	let mut file = File::open(&path).map_err(|_| ShError::from_io())?;
	let mut buffer = String::new();
	file.read_to_string(&mut buffer).map_err(|_| ShError::from_io())?;
	write_meta(|meta| meta.set_last_input(&buffer.clone()))?;

	let mut tokenizer = RshTokenizer::new(&buffer);
	loop {
		let state = descend(&mut tokenizer)?;
		if state.is_none() { break }
		let result = execute::traverse_ast(state.unwrap().ast)?;
		if let RshWait::Fail { code, cmd } = result {
			if code == 127 {
				if let Some(cmd) = cmd {
					eprintln!("Command not found: {}",cmd);
				}
			}
		}
	}
	Ok(())
}
