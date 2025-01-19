use std::{collections::BTreeMap, env, ffi::{CString, OsStr}, fmt, hash::Hash, io::Read, mem::take, os::fd::BorrowedFd, path::PathBuf, sync::{Arc, LazyLock}};
use std::collections::HashMap;

use bitflags::bitflags;
use nix::{sys::{signal::{kill, killpg, signal, SigHandler, Signal}, wait::{waitpid, WaitPidFlag, WaitStatus}}, unistd::{gethostname, getpgid, isatty, setpgid, tcgetpgrp, tcsetpgrp, Pid, User}};
use once_cell::sync::Lazy;
use std::{fs::File, sync::RwLock};


use crate::{event::ShError, execute::{self, RshWait}, interp::{helper, parse::{descend, Node, Span}, token::RshTokenizer}, RshResult};

#[derive(Debug)]
pub struct DisplayWaitStatus(pub WaitStatus);

impl fmt::Display for DisplayWaitStatus {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match &self.0 {
			WaitStatus::Exited(_, code) => {
				match code {
					0 => write!(f, "done"),
					_ => write!(f, "failed: {}", code),
				}
			}
			WaitStatus::Signaled(_, signal, _) => {
				write!(f, "signaled: {:?}", signal)
			}
			WaitStatus::Stopped(_, signal) => {
				write!(f, "stopped: {:?}", signal)
			}
			WaitStatus::PtraceEvent(_, signal, _) => {
				write!(f, "ptrace event: {:?}", signal)
			}
			WaitStatus::PtraceSyscall(_) => {
				write!(f, "ptrace syscall")
			}
			WaitStatus::Continued(_) => {
				write!(f, "continued")
			}
			WaitStatus::StillAlive => {
				write!(f, "running")
			}
		}
	}
}

pub static RSH_PGRP: Lazy<Pid> = Lazy::new(Pid::this);
pub static RSH_PATH: Lazy<String> = Lazy::new(|| std::env::current_exe().unwrap().to_str().unwrap().to_string());

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
	pub struct JobCmdFlags: i8 { // Options for the jobs builtin
		const LONG      = 0b00000001;
		const PIDS      = 0b00000010;
		const NEW_ONLY  = 0b00000100;
		const RUNNING   = 0b00001000;
		const STOPPED   = 0b00010000;
		const INIT      = 0b00100000;
	}
}

#[derive(Debug,Clone)]
pub enum JobID { // Possible arguments for JobTable::query(), each of these can find a job somehow
	Pgid(Pid),
	Pid(Pid),
	TableID(usize),
	Command(String)
}

#[derive(Debug,PartialEq,Clone)]
pub struct ChildProc {
	pgid: Pid,
	pid: Pid,
	command: Option<String>,
	status: WaitStatus
}

impl ChildProc {
	pub fn new(pid: Pid, command: Option<&str>, pgid: Option<Pid>) -> RshResult<Self> {
		let command = command.map(|str| str.to_string());
    let status = if kill(pid, None).is_ok() {
        WaitStatus::StillAlive // The process is still running
    } else {
        WaitStatus::Exited(pid, 0) // Default to exited
    };
		let mut child = Self { pgid: pid, pid, command, status };
		if let Some(pgid) = pgid {
			child.setpgid(pgid).ok();
		} else {
			child.setpgid(pid).ok();
		}
		Ok(child)
	}
	pub fn pid(&self) -> Pid {
		self.pid
	}
	pub fn pgid(&self) -> Pid {
		self.pgid
	}
	pub fn command(&self) -> Option<String> {
		self.command.clone()
	}
	pub fn status(&self) -> WaitStatus {
		self.status
	}
	pub fn waitpid(&mut self, flags: Option<WaitPidFlag>) -> Result<WaitStatus, nix::errno::Errno>  {
		let result = waitpid(self.pid(), flags);
		if let Ok(status) = result {
			self.status = status
		}
		result
	}
	pub fn setpgid(&mut self, pgid: Pid) -> RshResult<()> {
		self.pgid = pgid;
		setpgid(self.pid, pgid).map_err(|_| ShError::from_io())
	}
	pub fn set_status(&mut self, status: WaitStatus) {
		self.status = status;
	}
	pub fn kill(&self, signal: Signal) -> RshResult<()> {
		kill(self.pid, Some(signal))
			.map_err(|_| ShError::from_io())
	}
	pub fn is_alive(&self) -> bool {
		matches!(self.status, WaitStatus::StillAlive)
	}
	pub fn is_stopped(&self) -> bool {
		matches!(self.status, WaitStatus::Stopped(_, _))
	}
	pub fn is_done(&self) -> bool {
		matches!(self.status, WaitStatus::Exited(_, _))
	}
}

pub struct JobBuilder {
	table_id: Option<usize>,
	pgid: Option<Pid>,
	children: Vec<ChildProc>
}

impl Default for JobBuilder {
	fn default() -> Self {
		Self::new()
	}
}

impl JobBuilder {
	pub fn new() -> Self {
		Self { table_id: None, pgid: None, children: vec![] }
	}
	pub fn with_id(self, id: usize) -> Self {
		Self {
			table_id: Some(id),
			pgid: self.pgid,
			children: self.children
		}
	}
	pub fn with_pgid(self, pgid: Pid) -> Self {
		Self {
			table_id: self.table_id,
			pgid: Some(pgid),
			children: self.children
		}
	}
	pub fn with_children(self, children: Vec<ChildProc>) -> Self {
		Self {
			table_id: self.table_id,
			pgid: self.pgid,
			children
		}
	}
	pub fn build(self) -> Job {
		Job {
			table_id: self.table_id,
			pgid: self.pgid.unwrap(),
			children: self.children
		}
	}
}

#[derive(Debug, Clone)]
pub struct Job {
	table_id: Option<usize>,
	pgid: Pid,
	children: Vec<ChildProc>,
}

impl Job {
	pub fn set_table_id(&mut self, id: usize) {
		self.table_id = Some(id)
	}
	pub fn is_alive(&self) -> bool {
		!self.children.iter().all(|chld| chld.is_done())
	}
	pub fn table_id(&self) -> Option<usize> {
		self.table_id
	}
	pub fn pgid(&self) -> Pid {
		self.pgid
	}
	pub fn get_commands(&self) -> Vec<String> {
		let mut cmds = vec![];
		for child in &self.children {
			cmds.push(child.command().clone().unwrap_or_default())
		}
		cmds
	}
	pub fn set_statuses(&mut self, status: WaitStatus) {
		for child in self.children.iter_mut() {
			child.set_status(status);
		}
	}
	pub fn get_pids(&self) -> Vec<Pid> {
		self.children.iter().map(|chld| chld.pid()).collect::<Vec<Pid>>()
	}
	pub fn update_by_id(&mut self, id: JobID, status: WaitStatus) -> RshResult<()> {
		match id {
    JobID::Pid(pid) => {
			let query_result = self.children.iter_mut().find(|chld| chld.pid() == pid);
			if let Some(child) = query_result {
				child.set_status(status);
			}
		}
    JobID::Command(cmd) => {
			let query_result = self.children
				.iter_mut()
				.find(|chld| chld.command().is_some_and(|chld_cmd| chld_cmd.contains(&cmd)));
			if let Some(child) = query_result {
				child.set_status(status);
			}
		}
    JobID::Pgid(pgid) => {
			if pgid == self.pgid {
				for child in self.children.iter_mut() {
					child.set_status(status);
				}
			}
		}
    JobID::TableID(id) => {
			if self.table_id.is_some_and(|tbl_id| tbl_id == id) {
				for child in self.children.iter_mut() {
					child.set_status(status);
				}
			}
		}
}
		Ok(())
	}
	pub fn poll_children(&mut self) -> RshResult<()> {
		for child in self.children.iter_mut() {
			let pid = child.pid();
			match waitpid(pid, Some(WaitPidFlag::WNOHANG | WaitPidFlag::WUNTRACED | WaitPidFlag::WCONTINUED)) {
				Ok(status) => {
					match status {
						WaitStatus::StillAlive => continue,
						_ => { /* Do nothing */ }
					};
					child.set_status(status);
				}
				Err(_) => {
					attach_tty(*RSH_PGRP)?
				}
			}
		}
		Ok(())
	}
	pub fn get_statuses(&self) -> Vec<WaitStatus> {
		self.children.iter().map(|chld| chld.status()).collect::<Vec<WaitStatus>>()
	}
	pub fn get_children(&self) -> &[ChildProc] {
		&self.children
	}
	pub fn get_children_mut(&mut self) -> &mut Vec<ChildProc> {
		&mut self.children
	}
	pub fn new_child(&mut self, child: ChildProc) {
		self.children.push(child);
	}
	pub fn to_fg(&self) -> RshResult<()> {
		attach_tty(self.pgid)
	}
	pub fn killpg(&mut self, signal: Signal) -> RshResult<()> {
		let status = match signal {
			Signal::SIGTSTP => WaitStatus::Stopped(self.pgid, Signal::SIGTSTP),
			Signal::SIGCONT => WaitStatus::Continued(self.pgid),
			_ => unimplemented!()
		};
		self.set_statuses(status);
		killpg(self.pgid, Some(signal)).map_err(|_| ShError::from_io())?;
		Ok(())
	}
	pub fn wait_pgrp(&mut self) -> RshResult<Vec<WaitStatus>> {
		let mut statuses = Vec::new();

		for child in self.children.iter_mut() {
			let result = child.waitpid(Some(WaitPidFlag::WUNTRACED));
			match result {
				Ok(status) => {
					statuses.push(status);
				}
				Err(nix::errno::Errno::ECHILD) => {
					// No more child processes in the group
					break;
				}
				Err(_) => {
					return Err(ShError::from_io());
				}
			}
		}
		Ok(statuses)
	}
	pub fn display(&self, job_order: &[usize], flags: JobCmdFlags) -> String {
		let long = flags.contains(JobCmdFlags::LONG);
		let init = flags.contains(JobCmdFlags::INIT);
		let pids = flags.contains(JobCmdFlags::PIDS);
		let current = job_order.last();
		let prev = if job_order.len() > 2 {
			job_order.get(job_order.len() - 2)
		} else {
			None
		};

		// TODO: Handle this unwrap more safely
		let id = self.table_id.unwrap();
		let symbol = helper::determine_job_symbol(id, current, prev);
		let padding_num = symbol.len() + id.to_string().len() + 3;
		let padding: String = " ".repeat(padding_num);

		let mut output = format!("[{}]{} ", id + 1, symbol);

		for (i, cmd) in self.get_commands().iter().enumerate() {
			let status_final = helper::format_command_status(i, cmd, self, init, pids);
			let status_line = helper::format_status_line(i, &status_final, self, long, &padding);
			output.push_str(&status_line);
		}

		output
	}
}

pub struct JobTable {
	fg: Option<Job>,
	jobs: Vec<Option<Job>>,
	order: Vec<usize>,
	new_updates: Vec<usize>
}

impl JobTable {
	pub fn new() -> Self {
		Self { fg: None, jobs: vec![], order: vec![], new_updates: vec![] }
	}
	pub fn reset_fg(&mut self) {
		std::mem::take(&mut self.fg);
	}
	fn next_open_pos(&self) -> usize {
		if let Some(open_position) = self.jobs.iter().position(|slot| slot.is_none()) {
			open_position
		} else {
			self.jobs.len()
		}
	}
	pub fn mut_jobs(&mut self) -> &mut Vec<Option<Job>> {
		&mut self.jobs
	}
	pub fn curr_job(&self) -> Option<usize> {
		self.order.last().copied()
	}
	pub fn prev_job(&self) -> Option<usize> {
		let len = self.order.len();
		if len > 1 {
			self.order.get(len - 2).copied()
		} else {
			None
		}
	}
	pub fn insert_job(&mut self, mut job: Job, silent: bool) -> RshResult<usize> {
		self.prune_jobs();
		let table_position = if let Some(id) = job.table_id() { id } else { self.next_open_pos() };
		job.set_table_id(table_position);
		self.order.push(table_position);

		if !silent {
			println!("{}", job.display(self.job_order(), JobCmdFlags::INIT));
		}
		if table_position == self.jobs.len() {
			self.jobs.push(Some(job)); // Append if no open position is found
		} else {
			self.jobs[table_position] = Some(job); // Insert job at the open position
		}
		Ok(table_position)
	}
	pub fn job_order(&self) -> &[usize] {
		&self.order
	}
	pub fn new_fg(&mut self, job: Job) -> RshResult<Vec<WaitStatus>> {
		let pgid = job.pgid();
		self.fg = Some(job);
		attach_tty(pgid)?;
		let statuses = self.fg.as_mut().unwrap().wait_pgrp()?;
		attach_tty(*RSH_PGRP)?;
		Ok(statuses)
	}

	pub fn fg_to_bg(&mut self, status: WaitStatus) -> RshResult<()> {
		attach_tty(*RSH_PGRP)?;
		if self.fg.is_none() {
			return Ok(())
		}
		let fg = take(&mut self.fg);
		if let Some(mut job) = fg {
			// Find the first open position (None)
			job.set_statuses(status);
			self.insert_job(job,false)?;
		}
		Ok(())
	}
	pub fn remove_job(&mut self, id: JobID) -> Option<Job> {
		let table_id = self.query(id).map(|job| job.table_id()).unwrap();
		if let Some(table_id) = table_id {
			self.jobs.get_mut(table_id).and_then(Option::take)
		} else {
			None
		}
	}
	pub fn bg_to_fg(&mut self, id: JobID) -> RshResult<()> {
		let job = self.remove_job(id);
		if let Some(job) = job {
			helper::handle_fg(job)?;
		}
		Ok(())
	}
	pub fn get_fg(&self) -> Option<&Job> {
		self.fg.as_ref()
	}
	pub fn get_fg_mut(&mut self) -> Option<&mut Job> {
		self.fg.as_mut()
	}

	/// Queries the job table to find a job that matches the specified identifier.
	///
	/// This function allows you to search for a job in the job table using various types of identifiers.
	/// It returns a reference to the job if a match is found, or `None` if no matching job exists.
	///
	/// # Parameters
	///
	/// - `identifier`: A `JobID` enum variant that specifies the type of identifier to use for the search.
	///   The `JobID` can be one of the following:
	///   - `JobID::Pgid(pgid)`: Matches a job by its process group ID.
	///   - `JobID::Pid(pid)`: Matches a job by the process ID of any of its child processes.
	///   - `JobID::TableID(id)`: Matches a job by its index in the job table.
	///   - `JobID::Command(cmd)`: Matches a job by a partial command name match.
	///
	/// # Returns
	///
	/// An `Option` containing a reference to the `Job` if a match is found, or `None` if no job matches
	/// the specified identifier.
	///
	/// # Examples
	///
	/// ```rust
	/// let job_table = JobTable::new(); // Assuming JobTable is defined elsewhere
	/// if let Some(job) = job_table.query(JobID::Pgid(1234)) {
	///     println!("Found job with PGID 1234: {:?}", job);
	/// } else {
	///     println!("No job found with PGID 1234");
	/// }
	///
	/// if let Some(job) = job_table.query(JobID::Command("bash".to_string())) {
	///     println!("Found job with command containing 'bash': {:?}", job);
	/// } else {
	///     println!("No job found with command containing 'bash'");
	/// }
	/// ```
	///
	/// In these examples, the function is used to search for jobs by process group ID and by partial
	/// command name match.
	///
	/// # Notes
	///
	/// - The function performs a partial match when searching by command name, meaning it will return
	///   a job if any of its child processes' command names contain the specified string.
	/// - The search is case-sensitive when matching command names.
	pub fn query(&self, identifier: JobID) -> Option<&Job> {
		match identifier {
			// Match by process group ID
			JobID::Pgid(pgid) => {
				self.jobs.iter().find_map(|job| {
					job.as_ref().filter(|j| j.pgid == pgid)
				})
			}
			// Match by process ID
			JobID::Pid(pid) => {
				self.jobs.iter().find_map(|job| {
					job.as_ref().filter(|j| j.children.iter().any(|child| child.pid == pid))
				})
			}
			// Match by table ID (index in the job table)
			JobID::TableID(id) => {
				self.jobs.get(id).and_then(|job| job.as_ref())
			}
			// Match by command name (partial match)
			JobID::Command(cmd) => {
				self.jobs.iter().find_map(|job| {
					job.as_ref().filter(|j| {
						j.children.iter().any(|child| {
							child.command.as_ref().is_some_and(|c| c.contains(&cmd))
						})
					})
				})
			}
		}
	}
	pub fn query_mut(&mut self, identifier: JobID) -> Option<&mut Job> {
		match identifier {
			// Match by process group ID
			JobID::Pgid(pgid) => {
				self.jobs.iter_mut().find_map(|job| {
					job.as_mut().filter(|j| j.pgid == pgid)
				})
			}
			// Match by process ID
			JobID::Pid(pid) => {
				self.jobs.iter_mut().find_map(|job| {
					job.as_mut().filter(|j| j.children.iter().any(|child| child.pid == pid))
				})
			}
			// Match by table ID (index in the job table)
			JobID::TableID(id) => {
				self.jobs.get_mut(id).and_then(|job| job.as_mut())
			}
			// Match by command name (partial match)
			JobID::Command(cmd) => {
				self.jobs.iter_mut().find_map(|job| {
					job.as_mut().filter(|j| {
						j.children.iter().any(|child| {
							child.command.as_ref().is_some_and(|c| c.contains(&cmd))
						})
					})
				})
			}
		}
	}
	pub fn prune_jobs(&mut self) {
		while let Some(job) = self.jobs.last() {
			if job.is_none() {
				self.jobs.pop();
			} else {
				break;
			}
		}
	}
	pub fn prune_finished(&mut self) {
		self.jobs.retain(|job| job.as_ref().is_some_and(|job| job.is_alive()));
	}
	pub fn reset_recents(&mut self) {
		self.new_updates.clear()
	}
	pub fn print_jobs(&self, flags: &JobCmdFlags) {
		let jobs = if flags.contains(JobCmdFlags::NEW_ONLY) {
			&self.jobs
				.iter()
				.filter(|job| job.as_ref().is_some_and(|job| self.new_updates.contains(&job.table_id().unwrap())))
				.map(|job| job.as_ref())
				.collect::<Vec<Option<&Job>>>()
		} else {
			&self.jobs
				.iter()
				.map(|job| job.as_ref())
				.collect::<Vec<Option<&Job>>>()
		};
		for job in jobs.iter().flatten() {
			// Skip foreground job
			let id = job.table_id().unwrap();
			// Filter jobs based on flags
			if flags.contains(JobCmdFlags::RUNNING) && !matches!(job.get_statuses().get(id).unwrap(), WaitStatus::StillAlive | WaitStatus::Continued(_)) {
				continue;
			}
			if flags.contains(JobCmdFlags::STOPPED) && !matches!(job.get_statuses().get(id).unwrap(), WaitStatus::Stopped(_,_)) {
				continue;
			}
			// Print the job in the selected format
			println!("{}", job.display(&self.order,*flags));
		}
	}
	pub fn update_job_statuses(&mut self) -> RshResult<()> {
		for job in self.jobs.iter_mut().flatten() {
			//job.poll_children()?;
		}
		Ok(())
	}
}

impl Default for JobTable {
    fn default() -> Self {
        Self::new()
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
		if let Ok(index) = key.parse::<usize>() {
			// Extract current positional parameters
			let mut pos_params: Vec<String> = self.params
				.get("@")
				.unwrap_or(&String::new())
				.split_whitespace()
				.map(|s| s.to_string())
				.collect();

				// Ensure pos_params is large enough to accommodate the index
				if index >= pos_params.len() {
					pos_params.resize(index + 1, String::new());
				}

				// Update or set the value at the specified index
				pos_params[index] = value.clone();

				// Update "$@" with the joined positional parameters
				self.params.insert("@".into(), pos_params.join(" "));
		}

		// Set the individual parameter as well
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
		if let Some(var) = self.vars.get(key).cloned() {
			Some(var)
		} else if let Some(var) = self.params.get(key).cloned() {
			Some(RVal::String(var))
		} else { self.env.get(key).cloned().map(RVal::String) }
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
	pub fn remove_func(&mut self, name: &str) {
		self.functions.remove(name);
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

pub fn disable_reaping() {
	unsafe { signal(Signal::SIGCHLD, SigHandler::Handler(crate::signal::ignore_sigchld)) }.unwrap();
}

pub fn enable_reaping() -> RshResult<()> {
	write_jobs(|j| j.update_job_statuses())??;
	unsafe { signal(Signal::SIGCHLD, SigHandler::Handler(crate::signal::handle_sigchld)) }.unwrap();
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
pub fn attach_tty(pgid: Pid) -> RshResult<()> {
	// Ensure stdin (fd 0) is a tty before proceeding
	if !isatty(0).unwrap_or(false) {
		return Ok(());
	}

	if unsafe { tcgetpgrp(BorrowedFd::borrow_raw(0)) == Ok(pgid) } {
		return Ok(())
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
		if state.clone().is_some_and(|ps| ps.tokens.is_empty()) { break }
		let result = execute::traverse_ast(state.unwrap().ast)?;
		if let RshWait::Fail { code, cmd } = result {
			if code == 127 {
				if let Some(cmd) = cmd {
					eprintln!("Command not found: {}",cmd);
					break
				}
			}
		}
	}
	Ok(())
}

#[cfg(test)]
mod tests {
	use rstest::{fixture, rstest};
	use pretty_assertions::assert_eq;
	use super::*;


	#[fixture]
	fn mock_jobs() -> JobTable {
		let mut job_table = JobTable::new();

		let children1 = vec![
			ChildProc::new(Pid::from_raw(100), Some("cmd1"),None).unwrap(),
			ChildProc::new(Pid::from_raw(101), Some("cmd2"),None).unwrap()
		];
		let job1 = JobBuilder::new()
			.with_id(1)
			.with_pgid(Pid::from_raw(100))
			.with_children(children1)
			.build();

			let children2 = vec![
				ChildProc::new(Pid::from_raw(200), Some("cmd3"),None).unwrap(),
			];
			let job2 = JobBuilder::new()
				.with_id(2)
				.with_pgid(Pid::from_raw(200))
				.with_children(children2)
				.build();

			job_table.insert_job(job1,false).unwrap();
			job_table.insert_job(job2,false).unwrap();

			job_table
	}

	#[fixture]
	fn mock_vars() -> VarTable {
		VarTable::new()
	}

	#[fixture]
	fn mock_logic() -> LogicTable {
		LogicTable::new()
	}


	// Assume RVal, HashFloat, and VarTable are defined elsewhere in your code

#[rstest]
	fn shellenv_define_string(mut mock_vars: VarTable) {
		let name = "string_var";
		let value = RVal::String("Hello, World!".to_string());
		mock_vars.set_var(name, value);
		let return_val = mock_vars.get_var(name).unwrap();
		let display = format!("{}", return_val);

		assert_eq!(display, "Hello, World!".to_string());
	}

#[rstest]
	fn shellenv_define_int(mut mock_vars: VarTable) {
		let name = "integer";
		let value = RVal::Int(5);
		mock_vars.set_var(name, value);
		let return_val = mock_vars.get_var(name).unwrap();
		let display = format!("{}", return_val);

		assert_eq!(display, "5".to_string());
	}

#[rstest]
	fn shellenv_define_float(mut mock_vars: VarTable) {
		let name = "float_var";
		let value = RVal::Float(HashFloat(3.15)); // Assuming `HashFloat` wraps an `f64`.
		mock_vars.set_var(name, value);
		let return_val = mock_vars.get_var(name).unwrap();
		let display = format!("{}", return_val);

		assert_eq!(display, "3.15".to_string()); // Adjust formatting if `HashFloat` differs.
	}

#[rstest]
	fn shellenv_define_bool(mut mock_vars: VarTable) {
		let name = "bool_var";
		let value = RVal::Bool(true);
		mock_vars.set_var(name, value);
		let return_val = mock_vars.get_var(name).unwrap();
		let display = format!("{}", return_val);

		assert_eq!(display, "true".to_string());
	}

	#[rstest]
	fn shellenv_define_array(mut mock_vars: VarTable) {
		let name = "array_var";
		let value = RVal::Array(vec![
			RVal::Int(1),
			RVal::Int(2),
			RVal::Int(3),
			RVal::String("four".to_string()),
		]);
		mock_vars.set_var(name, value);
		let return_val = mock_vars.get_var(name).unwrap();
		let display = format!("{}", return_val);

		// Assuming `RVal::Array` is formatted as `[1, 2, 3, "four"]`.
		assert_eq!(display, "1 2 3 four".to_string());
	}

#[rstest]
	fn logic_table_new_alias(mut mock_logic: LogicTable) {
		mock_logic.new_alias("alias1", "value1".to_string());
		let alias_value = mock_logic.get_alias("alias1");
		assert_eq!(alias_value, Some("value1".to_string()));
	}

#[rstest]
	fn logic_table_remove_alias(mut mock_logic: LogicTable) {
		mock_logic.new_alias("alias1", "value1".to_string());
		mock_logic.remove_alias("alias1");
		let alias_value = mock_logic.get_alias("alias1");
		assert_eq!(alias_value, None);
	}

#[rstest]
	fn logic_table_get_alias(mut mock_logic: LogicTable) {
		mock_logic.new_alias("alias1", "value1".to_string());
		let alias_value = mock_logic.get_alias("alias1");
		assert_eq!(alias_value, Some("value1".to_string()));
		assert_eq!(mock_logic.get_alias("nonexistent"), None);
	}

#[rstest]
	fn logic_table_new_func(mut mock_logic: LogicTable) {
		let node = Node::new(); // Assuming a `Node::new()` exists for creating a dummy node.
		mock_logic.new_func("func1", node.clone());
		let retrieved_func = mock_logic.get_func("func1");
		assert!(matches!(retrieved_func, Some(node)));
	}

#[rstest]
	fn logic_table_get_func(mut mock_logic: LogicTable) {
		let node = Node::new(); // Assuming a `Node::new()` exists for creating a dummy node.
		mock_logic.new_func("func1", node.clone());
		let retrieved_func = mock_logic.get_func("func1");
		assert!(matches!(retrieved_func, Some(node)));

		// Ensure retrieving a nonexistent function returns `None`
		assert!(mock_logic.get_func("nonexistent").is_none());
	}

	#[rstest]
	fn logic_table_remove_func(mut mock_logic: LogicTable) {
		let node = Node::new(); // Assuming a `Node::new()` exists for creating a dummy node.
		mock_logic.new_func("func1", node.clone());
		mock_logic.remove_func("func1");
		let retrieved_func = mock_logic.get_func("func1");
		assert!(retrieved_func.is_none());
	}


	#[rstest]
	fn vartable_index_array(mut mock_vars: VarTable) {
		// Create an array and add it to the variable table
		let key = "my_array";
		let array_value = RVal::Array(vec![
			RVal::Int(10),
			RVal::Int(20),
			RVal::String("thirty".to_string()),
		]);
		mock_vars.set_var(key, array_value);

		// Test valid indices
		let first_element = mock_vars.index_arr(key, 0).unwrap();
		assert_eq!(first_element, RVal::Int(10));

		let second_element = mock_vars.index_arr(key, 1).unwrap();
		assert_eq!(second_element, RVal::Int(20));

		let third_element = mock_vars.index_arr(key, 2).unwrap();
		assert_eq!(third_element, RVal::String("thirty".to_string()));

		// Test invalid index (out of bounds)
		let out_of_bounds = mock_vars.index_arr(key, 3);
		assert!(out_of_bounds.is_err());
		if let Err(err) = out_of_bounds {
			assert_eq!(
				err.to_string(),
				"Index `3` out of range for array `my_array`"
			);
		}

		// Test non-array variable
		mock_vars.set_var("not_array", RVal::Int(42));
		let not_an_array = mock_vars.index_arr("not_array", 0);
		assert!(not_an_array.is_err());
		if let Err(err) = not_an_array {
			assert_eq!(err.to_string(), "not_array is not an array");
		}

		// Test nonexistent variable
		let nonexistent = mock_vars.index_arr("does_not_exist", 0);
		assert!(nonexistent.is_err());
		if let Err(err) = nonexistent {
			assert_eq!(err.to_string(), "does_not_exist is not a variable");
		}
	}
}
