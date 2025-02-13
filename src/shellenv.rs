use std::{collections::{BTreeMap, VecDeque}, env, ffi::{CString, OsStr}, fmt, hash::Hash, io::{self, Read}, mem::take, os::fd::BorrowedFd, path::{Path, PathBuf}, sync::{Arc, LazyLock}, time::{Duration, Instant}};
use std::collections::HashMap;

use bitflags::bitflags;
use nix::{sys::{signal::{kill, killpg, signal, SigHandler, SigmaskHow, Signal::{self, SIGCHLD, SIGTSTP, SIGTTIN, SIGTTOU}}, wait::{waitpid, WaitPidFlag, WaitStatus}}, unistd::{gethostname, getpgrp, isatty, setpgid, tcgetpgrp, tcsetpgrp, Pid, User}};
use once_cell::sync::Lazy;
use std::sync::RwLock;

use crate::{execute::dispatch, prelude::*, utils::{self, Redir}};
use crate::{error::{LashErr::*, LashErrLow}, helper::{self, VecDequeExtension}, shopt::ShOpts, LashResult};


#[derive(Debug)]
pub struct DisplayWaitStatus(pub WaitStatus);

pub const PARAMS: [&str;8] = ["#", "@", "*", "$", "!", "?", "-", "_"];

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

pub static RSH_PATH: Lazy<String> = Lazy::new(|| std::env::current_exe().unwrap().to_str().unwrap().to_string());

pub static JOBS: LazyLock<Arc<RwLock<JobTable>>> = LazyLock::new(|| {
	Arc::new(
		RwLock::new(
			JobTable::new()
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
		const IN_SUB_PROC      = 0b00000000000000000000000000001000;
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
		const SOURCING         = 0b00001000000000000000000000000000;
		const INITIALIZED      = 0b00010000000000000000000000000000;
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
pub struct Lash {
	vars: VarTable,
	logic: LogicTable,
	meta: EnvMeta,
	ctx: ExecCtx
}

impl Lash {
	pub fn new() -> Self {
		let env = Self::init_env_vars(true);
		let vars = VarTable::new(env);
		let logic = LogicTable::new();
		let meta = EnvMeta::new(EnvFlags::empty());
		let ctx = ExecCtx::new();

		Self { vars, logic, meta, ctx }
	}
	pub fn vars(&self) -> &VarTable {
		&self.vars
	}
	pub fn vars_mut(&mut self) -> &mut VarTable {
		&mut self.vars
	}
	pub fn logic(&self) -> &LogicTable {
		&self.logic
	}
	pub fn logic_mut(&mut self) -> &mut LogicTable {
		&mut self.logic
	}
	pub fn meta(&self) -> &EnvMeta {
		&self.meta
	}
	pub fn meta_mut(&mut self) -> &mut EnvMeta {
		&mut self.meta
	}
	pub fn ctx(&self) -> &ExecCtx {
		&self.ctx
	}
	pub fn ctx_mut(&mut self) -> &mut ExecCtx {
		&mut self.ctx
	}
	pub fn get_status(&self) -> i32 {
		self.vars.get_param("?").map(|c| c.parse::<i32>().unwrap()).unwrap_or(0)
	}
	#[track_caller]
	pub fn set_code(&mut self, code: i32) {
		//dbg!(code);
		//let call_info = std::panic::Location::caller();
		//let file = call_info.file();
		//let line = call_info.line();
		//let col = call_info.column();
		//eprintln!("called set code from file: {file}, on line {line} col {col}");

		self.vars.set_param("?", &code.to_string())
	}
	pub fn in_pipe(&self) -> bool {
		self.meta.flags().contains(EnvFlags::IN_SUB_PROC)
	}
	pub fn push_state(&mut self) -> LashResult<()> {
		self.ctx.push_state()
	}
	pub fn pop_state(&mut self) -> LashResult<()> {
		self.ctx.pop_state()
	}
	pub fn consume_redirs(&mut self, redirs: VecDeque<Redir>) -> LashResult<()> {
		self.ctx_mut().extend_redirs(redirs);
		self.ctx_mut().activate_redirs()?;
		Ok(())
	}
	pub fn start_timer(&mut self) {
		self.meta.timer_start = Some(Instant::now())
	}
	pub fn stop_timer(&mut self) -> LashResult<()> {
		if let Some(start_time) = self.meta.timer_start {
			self.meta.cmd_duration = Some(start_time.elapsed());
			self.vars.export_var("OX_CMD_TIME", &self.meta.cmd_duration.unwrap().as_millis().to_string());
		}
		Ok(())
	}

	pub fn change_dir(&mut self, path: &Path) -> LashResult<()> {
		let cwd = env::var("PWD").map_err(|_| Low(LashErrLow::from_io()))?;
		self.vars.export_var("OLDPWD", &cwd);
		env::set_current_dir(path)?;
		let cwd = env::current_dir().map_err(|_| Low(LashErrLow::from_io()))?;
		self.vars.export_var("PWD", cwd.to_str().unwrap());
		Ok(())
	}

	pub fn source_rc(&mut self, path: Option<PathBuf>) -> LashResult<()> {
		let path = if let Some(path) = path {
			path
		} else {
			let home = env::var("HOME").unwrap();
			PathBuf::from(format!("{home}/.lashrc"))
		};
		if let Err(e) = self.source_file(path.to_str().unwrap()) {
			self.set_code(1);
			eprintln!("Failed to source lashrc: {}",e);
		}
		Ok(())
	}


	pub fn source_file<'a>(&mut self, path: &str) -> LashResult<()> {
		let mut file = utils::SmartFD::std_open(&path)?;
		let mut buffer = String::new();
		file.read_to_string(&mut buffer).map_err(|_| Low(LashErrLow::from_io()))?;
		file.close()?;

		dispatch::exec_input(buffer, self)
	}

	pub fn get_cstring_evars<'a>(&self) -> LashResult<Vec<CString>> {
		let env = self.vars.borrow_evars();
		let env = env.iter().map(|(k,v)| CString::new(format!("{}={}",k,v).as_str()).unwrap()).collect::<Vec<CString>>();
		Ok(env)
	}
	pub fn is_func(&self, name: &str) -> LashResult<bool> {
		let result = self.logic.get_func(name).is_some();
		Ok(result)
	}

	pub fn init_env_vars(clean: bool) -> HashMap<String,String> {
		let pathbuf_to_string = |pb: Result<PathBuf, std::io::Error>| pb.unwrap_or_default().to_string_lossy().to_string();
		// First, inherit any env vars from the parent process if clean bit not set
		let mut env_vars = HashMap::new();
		if !clean {
			env_vars = std::env::vars().collect::<HashMap<String,String>>();
		}
		let term = {
			if isatty(1).unwrap() {
				if let Ok(term) = std::env::var("TERM") {
					term
				} else {
					"linux".to_string()
				}
			} else {
				"xterm-256color".to_string()
			}
		};
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
		env_vars.insert("TERM".into(), term.clone());
		env::set_var("TERM", term);
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
		env_vars.insert("HIST_FILE".into(),format!("{}/.lash_hist",home));
		env::set_var("HIST_FILE",format!("{}/.lash_hist",home));

		env_vars
	}
	pub fn exec_as_cond(&mut self, input: &str) -> LashResult<i32> {
		let saved = self.ctx.clone();
		self.ctx = self.ctx.as_cond();
		dispatch::exec_input(input.to_string(), self)?;
		let status = self.get_status();
		self.ctx = saved;
		self.set_code(status);
		Ok(status)
	}
	pub fn exec_as_body(&mut self, input: &str) -> LashResult<i32> {
		let saved = self.ctx.clone();
		self.ctx = self.ctx.as_body();
		dispatch::exec_input(input.to_string(), self)?;
		let status = self.get_status();
		self.ctx = saved;
		self.set_code(status);
		Ok(status)
	}
}

#[derive(Debug,Clone)]
pub struct ExecCtx {
	redir_queue: VecDeque<utils::Redir>,
	flags: utils::ExecFlags,
	depth: usize,
	state_stack: Vec<Box<ExecCtx>>,
	max_recurse_depth: usize
}

impl ExecCtx {
	pub fn new() -> Self {
		Self {
			redir_queue: VecDeque::new(),
			flags: utils::ExecFlags::empty(),
			depth: 0,
			state_stack: vec![], // Each alteration is local to a single layer of recursion
			max_recurse_depth: 1000
		}
	}
	/// Creates a new instance of ExecCtx which retains only the standard input of the original
	/// Used in shell structures like `if` and `while` to direct input to the condition
	pub fn as_cond(&self) -> Self {
		let mut clone = self.clone();
		let (cond_redirs,_) = self.sort_redirs();
		clone.redir_queue = cond_redirs.into();
		clone
	}
	/// Creates a new instance of ExecCtx which retains only the stdout and stderr of the original
	/// Used in shell structures like `if` and `while` to direct output from the body
	pub fn as_body(&self) -> Self {
		let mut clone = self.clone();
		let (_,body_redirs) = self.sort_redirs();
		clone.redir_queue = body_redirs.into();
		clone
	}
	pub fn refresh(&mut self) -> LashResult<()> {
		*self = ExecCtx::new();
		Ok(())
	}
	/// returned as (Incoming Redirs, Outgoing Redirs)
	pub fn sort_redirs(&self) -> (Vec<utils::Redir>,Vec<utils::Redir>) {
		let mut in_redirs = vec![];
		let mut out_redirs = vec![];
		for redir in self.redir_queue.clone() {
			match redir.redir_type() {
				Rule::r#in => in_redirs.push(redir.clone()),
				Rule::out | Rule::append => out_redirs.push(redir.clone()),
				_ => unimplemented!()
			}
		}
		(in_redirs,out_redirs)
	}
	pub fn push_state(&mut self) -> LashResult<()> {
		let saved_state = Box::new(self.clone());
		self.state_stack.push(saved_state);
		Ok(())
	}
	pub fn pop_state(&mut self) -> LashResult<()> {
		if let Some(state) = self.state_stack.pop() {
			*self = *state;
		}
		Ok(())
	}
	pub fn ascend(&mut self) -> LashResult<()> {
		self.pop_state()?;
		self.depth = self.depth.saturating_sub(1);
		Ok(())
	}
	pub fn descend(&mut self) -> LashResult<()> {
		self.push_state()?;
		self.depth += 1;
		if self.depth >= self.max_recurse_depth {
			Err(Low(LashErrLow::InternalErr(format!("Hit maximum recursion depth during execution: {}",self.max_recurse_depth))))
		} else {
			Ok(())
		}
	}
	pub fn depth(&self) -> usize {
		self.depth
	}
	pub fn flags_mut(&mut self) -> &mut utils::ExecFlags {
		&mut self.flags
	}
	pub fn flags(&self) -> utils::ExecFlags {
		self.flags
	}
	pub fn set_redirs(&mut self, redirs: VecDeque<utils::Redir>) {
		self.redir_queue = redirs
	}
	pub fn extend_redirs(&mut self, redirs: VecDeque<utils::Redir>) {
		self.redir_queue.extend(redirs);
	}
	pub fn push_redir(&mut self,redir: utils::Redir) {
		self.redir_queue.push_back(redir)
	}
	pub fn pop_redir(&mut self) -> Option<utils::Redir> {
		self.redir_queue.pop_back()
	}
	pub fn take_redirs(&mut self) -> VecDeque<utils::Redir> {
		take(&mut self.redir_queue)
	}
	pub fn consume_redirs(&mut self) -> utils::CmdRedirs {
		utils::CmdRedirs::new(self.take_redirs())
	}
	pub fn activate_redirs(&mut self) -> LashResult<()> {
		let mut redirs = self.consume_redirs();
		redirs.activate()
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

impl<'a> ChildProc {
	pub fn new(pid: Pid, command: Option<&str>, pgid: Option<Pid>) -> LashResult<Self> {
		let command = command.map(|str| str.to_string());
    let status = if kill(pid, None).is_ok() {
        WaitStatus::StillAlive // The process is still running
    } else {
        WaitStatus::Exited(pid, 0) // Default to exited
    };
		let mut child = Self { pgid: pid, pid, command, status };
		if let Some(pgid) = pgid {
			child.setpgid(pgid);
		} else {
			child.setpgid(pid);
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
	pub fn setpgid(&mut self, pgid: Pid) {
		self.pgid = pgid;
		setpgid(self.pid, pgid);
	}
	pub fn set_status(&mut self, status: WaitStatus) {
		self.status = status;
	}
	pub fn kill(&self, signal: Signal) -> LashResult<()> {
		kill(self.pid, Some(signal))
			.map_err(|_| Low(LashErrLow::from_io()))
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
	pub fn update_by_id(&mut self, id: JobID, status: WaitStatus) -> LashResult<()> {
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
	pub fn poll_children(&mut self) -> LashResult<()> {
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
					attach_tty(getpgrp())?
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
	pub fn to_fg(&self) -> LashResult<()> {
		attach_tty(self.pgid)
	}
	pub fn killpg(&mut self, signal: Signal) -> LashResult<()> {
		let status = match signal {
			Signal::SIGTSTP => WaitStatus::Stopped(self.pgid, Signal::SIGTSTP),
			Signal::SIGCONT => WaitStatus::Continued(self.pgid),
			_ => unimplemented!()
		};
		self.set_statuses(status);
		killpg(self.pgid, Some(signal)).map_err(|_| Low(LashErrLow::from_io()))?;
		Ok(())
	}
	pub fn wait_pgrp<'a>(&mut self) -> LashResult<Vec<WaitStatus>> {
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
					return Err(Low(LashErrLow::from_io()));
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

#[derive(Debug,Clone)]
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
	pub fn insert_job(&mut self, mut job: Job, silent: bool) -> LashResult<usize> {
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
	pub fn new_fg<'a>(&mut self, job: Job) -> LashResult<Vec<WaitStatus>> {
		let pgid = job.pgid();
		self.fg = Some(job);
		attach_tty(pgid)?;
		let statuses = self.fg.as_mut().unwrap().wait_pgrp()?;
		attach_tty(getpgrp())?;
		Ok(statuses)
	}

	pub fn fg_to_bg(&mut self, status: WaitStatus) -> LashResult<()> {
		attach_tty(getpgrp())?;
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
	pub fn bg_to_fg(&mut self,lash: &mut Lash, id: JobID) -> LashResult<()> {
		let job = self.remove_job(id);
		if let Some(job) = job {
			helper::handle_fg(lash,job)?;
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
	pub fn hang_up(&mut self) {
		for job in self.jobs.iter_mut() {
			if let Some(ref mut job) = job {
				job.killpg(Signal::SIGHUP).ok();
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
	pub fn update_job_statuses<'a>(&mut self) -> LashResult<()> {
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

impl HashFloat {
	pub fn from_f64(float: f64) -> Self {
		Self(float)
	}
	pub fn to_f64(self) -> f64 {
		self.0
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LashVal {
	String(String),
	Int(i32),
	Float(HashFloat),
	Bool(bool),
	Array(Vec<LashVal>),
	Dict(BTreeMap<String, LashVal>),
}

impl LashVal {
	pub fn parse(mut s: &str) -> LashResult<Self> {
		if let Ok(int) = s.parse::<i32>() {
			return Ok(LashVal::Int(int));
		}
		if let Ok(float) = s.parse::<f64>() {
			return Ok(LashVal::Float(HashFloat(float)));
		}
		if let Ok(boolean) = s.parse::<bool>() {
			return Ok(LashVal::Bool(boolean));
		}
		if s.starts_with('"') && s.ends_with('"') {
			s = s.trim_matches('"');
			return Ok(LashVal::String(s.to_string()))
		} else if s.starts_with('\'') && s.ends_with('\'') {
			s = s.trim_matches('\'');
			return Ok(LashVal::String(s.to_string()))
		}
		if let Ok(array) = LashParse::parse(Rule::array, s) {
			let mut arr_inner = array.into_iter().next().unpack()?.into_inner();
			let mut elements = vec![];
			if arr_inner.as_str() == "[]" {
				return Ok(LashVal::Array(elements))
			} else {
				while let Some(element) = arr_inner.next() {
					let lash_val = LashVal::parse(element.as_str())?;
					elements.push(lash_val)
				}
				return Ok(LashVal::Array(elements))
			}
		}
		Ok(LashVal::String(s.to_string()))
	}

	pub fn as_os_str(&self) -> Option<&OsStr> {
		match self {
			LashVal::String(s) => Some(OsStr::new(s)),
			_ => None, // Only strings can be converted to OsStr
		}
	}

	pub fn fmt_type(&self) -> String {
		match self {
			LashVal::String(_) => String::from("string"),
			LashVal::Int(_) => String::from("int"),
			LashVal::Float(_) => String::from("float"),
			LashVal::Bool(_) => String::from("bool"),
			LashVal::Array(_) => String::from("array"),
			LashVal::Dict(_) => String::from("dict"),
		}
	}

	pub fn operate<F: FnOnce(&mut LashVal) -> LashVal>(&mut self, operation: F) -> LashResult<()> {
		*self = operation(self);
		Ok(())
	}

	pub fn increment(&mut self) -> LashResult<()> {
		match *self {
			Self::Int(i) => {
				self.operate(|_| Self::Int(i + 1))?
			}
			_ => return Err(Low(LashErrLow::InternalErr("Expected an integer in increment call".into()))),
		}
		Ok(())
	}

	pub fn decrement(&mut self) -> LashResult<()> {
		match *self {
			Self::Int(i) => {
				self.operate(|_| Self::Int(i - 1))?
			}
			_ => return Err(Low(LashErrLow::InternalErr("Expected an integer in decrement call".into()))),
		}
		Ok(())
	}

	pub fn concat(&mut self, val: LashVal) -> LashResult<()> {
		match self {
			LashVal::String(ref mut word) => {
				*word = format!("{}{}",word,val);
			}
			_ => return Err(Low(LashErrLow::InternalErr("Expected an array in append call".into()))),
		}
		Ok(())
	}

	pub fn push(&mut self, val: LashVal) -> LashResult<()> {
		match self {
			LashVal::Array(ref mut arr) => {
				arr.push(val);
			}
			_ => return Err(Low(LashErrLow::InternalErr("Expected an array in append call".into()))),
		}
		Ok(())
	}

	pub fn pop(&mut self) -> LashResult<Option<LashVal>> {
		match self {
			LashVal::Array(ref mut arr) => {
				Ok(arr.pop())
			}
			_ => return Err(Low(LashErrLow::InternalErr("Expected an array in pop call".into()))),
		}
	}

	pub fn as_string(&self) -> Option<&String> {
		if let LashVal::String(s) = self {
			Some(s)
		} else {
			None
		}
	}

	pub fn as_int(&self) -> Option<i32> {
		if let LashVal::Int(i) = self {
			Some(*i)
		} else {
			None
		}
	}

	pub fn as_float(&self) -> Option<f64> {
		if let LashVal::Float(f) = self {
			Some(f.0)
		} else {
			None
		}
	}

	pub fn as_bool(&self) -> Option<bool> {
		if let LashVal::Bool(b) = self {
			Some(*b)
		} else {
			None
		}
	}

	pub fn as_array(&self) -> Option<&Vec<LashVal>> {
		if let LashVal::Array(arr) = self {
			Some(arr)
		} else {
			None
		}
	}

	pub fn as_dict(&self) -> Option<&BTreeMap<String, LashVal>> {
		if let LashVal::Dict(dict) = self {
			Some(dict)
		} else {
			None
		}
	}

	pub fn try_insert(&mut self, key: String, val: LashVal) -> LashResult<()> {
		if let LashVal::Dict(map) = self {
			map.insert(key,val);
			Ok(())
		} else {
			Err(Low(LashErrLow::InternalErr("Called try_insert() on a non-dict LashVal".into())))
		}
	}

	pub fn try_get(&mut self, key: &str) -> LashResult<Option<&LashVal>> {
		if let LashVal::Dict(map) = self {
			Ok(map.get(key))
		} else {
			Err(Low(LashErrLow::InternalErr("Called try_get() on a non-dict LashVal".into())))
		}
	}

	pub fn try_get_mut<'a>(&mut self, key: &str) -> LashResult<Option<&mut LashVal>> {
		if let LashVal::Dict(map) = self {
			Ok(map.get_mut(key))
		} else {
			Err(Low(LashErrLow::InternalErr("Called try_get_mut() on a non-dict LashVal".into())))
		}
	}

	pub fn try_remove(&mut self, key: &str) -> LashResult<Option<LashVal>> {
		if let LashVal::Dict(map) = self {
			Ok(map.remove(key))
		} else {
			Err(Low(LashErrLow::InternalErr("Called try_remove() on a non-dict LashVal".into())))
		}
	}
}

impl Default for LashVal {
	fn default() -> Self {
		LashVal::String("".into())
	}
}

impl fmt::Display for LashVal {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			LashVal::Int(int) => write!(f, "{}", int),
			LashVal::String(string) => write!(f, "{}", string),
			LashVal::Float(float) => write!(f, "{}", float.0),
			LashVal::Bool(bool) => write!(f, "{}", bool),
			LashVal::Array(array) => {
				let formatted_array: Vec<String> = array.iter().map(|val| format!("{}", val)).collect();
				write!(f, "{}", formatted_array.join(" "))
			}
			LashVal::Dict(dict) => {
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
	pos_params: VecDeque<String>,
	vars: HashMap<String,LashVal>
}

impl VarTable {
	pub fn new(env: HashMap<String,String>) -> Self {
		Self {
			env,
			params: HashMap::new(),
			pos_params: VecDeque::new(),
			vars: HashMap::new()
		}
	}

	pub fn vars(&self) -> &HashMap<String, LashVal> {
		&self.vars
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
		if let Ok(index) = key.parse::<usize>() {
			self.pos_params.get(index).cloned().map(|param| param.to_string())
		} else {
			let result = self.params.get(key).cloned().map(|param| param.to_string());
			result
		}
	}
	pub fn borrow_pos_params(&self) -> &VecDeque<String> {
		&self.pos_params
	}
	pub fn pos_param_index(&self, key: usize) -> Option<String> {
		self.get_param(key.to_string().as_str())
	}
	pub fn pos_param_popfront(&mut self) -> Option<String> {
		let popped_param = self.pos_params.pop_front();
		self.set_param("@".into(), &self.pos_params.clone().to_vec().join(" "));
		self.set_param("#".into(), &self.pos_params.len().to_string());
		popped_param
	}
	pub fn pos_param_pushback(&mut self, param: &str) {
		self.pos_params.push_back(param.to_string());
		self.set_param("@".into(), &self.pos_params.clone().to_vec().join(" "));
		self.set_param("#".into(), &self.pos_params.len().to_string());
	}
	pub fn set_param(&mut self, key: &str, value: &str) {
		self.params.insert(key.into(), value.into());
	}
	pub fn reset_params(&mut self) {
		self.params.clear();
	}
	pub fn unset_param(&mut self, key: &str) {
		self.params.remove(key);
	}

	pub fn set_var(&mut self, key: &str, val: LashVal) {
		self.vars.insert(key.to_string(),val);
	}
	pub fn unset_var(&mut self, key: &str) {
		self.vars.remove(key);
	}
	pub fn get_var(&self, key: &str) -> Option<LashVal> {
		if let Some(var) = self.vars.get(key).cloned() {
			Some(var)
		} else if let Some(var) = self.params.get(key).cloned() {
			let val = LashVal::String(var);
			Some(val)
		} else {
			let var = self.env.get(key).cloned().map(LashVal::String);
			var
		}
	}
	pub fn get_var_mut(&mut self, key: &str) -> Option<&mut LashVal> {
		self.vars.get_mut(key)
	}

	pub fn index_arr(&self, key: &str, index: usize) -> LashResult<LashVal> {
		if let Some(var) = self.vars.get(key) {
			if let LashVal::Array(arr) = var {
				if let Some(value) = arr.get(index) {
					Ok(value.clone())
				} else {
					Err(Low(LashErrLow::ExecFailed(format!("Index `{}` out of range for array `{}`",index,key))))
				}
			} else {
				Err(Low(LashErrLow::ExecFailed(format!("{} is not an array",key))))
			}
		} else {
			Err(Low(LashErrLow::ExecFailed(format!("{} is not a variable",key))))
		}
	}
}

#[derive(Debug,Clone)]
pub struct LogicTable {
	functions: HashMap<String,String>,
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
	pub fn borrow_aliases(&self) -> &HashMap<String,String> {
		&self.aliases
	}
	pub fn get_alias(&self, name: &str) -> Option<String> {
		self.aliases.get(name).cloned()
	}
	pub fn new_func(&mut self, name: &str, instructions: &str) {
		self.functions.insert(name.to_string(),instructions.to_string());
	}
	pub fn get_func(&self, name: &str) -> Option<String> {
		self.functions.get(name).cloned()
	}
	pub fn borrow_functions(&self) -> &HashMap<String,String> {
		&self.functions
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
	last_command: Option<String>,
	timer_start: Option<Instant>,
	cmd_duration: Option<Duration>,
	dir_stack: Vec<PathBuf>,
	shopts: ShOpts,
	flags: EnvFlags,
	in_prompt: bool
}

impl EnvMeta {
	pub fn new(flags: EnvFlags) -> Self {
		let in_prompt = flags.contains(EnvFlags::INTERACTIVE);
		Self {
			last_input: String::new(),
			last_command: None,
			timer_start: None,
			cmd_duration: None,
			dir_stack: vec![std::env::current_dir().unwrap()],
			shopts: ShOpts::new(),
			flags,
			in_prompt,
		}
	}
	pub fn get_cmd_duration(&self) -> Option<Duration> {
		self.cmd_duration
	}
	pub fn reset_dir_stack(&mut self, path: PathBuf) {
		self.dir_stack = vec![path]
	}
	pub fn push_dir(&mut self, path: PathBuf) {
		self.dir_stack.push(path)
	}
	pub fn set_last_command(&mut self, cmd: &str) {
		self.last_command = Some(cmd.into())
	}
	pub fn get_last_command(&self) -> Option<String> {
		self.last_command.clone()
	}
	pub fn pop_dir(&mut self) -> Option<PathBuf> {
		if self.dir_stack.len() > 1 {
			self.dir_stack.pop()
		} else {
			None
		}
	}
	pub fn top_dir(&self) -> Option<&PathBuf> {
		self.dir_stack.last()
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
	pub fn borrow_shopts(&self) -> &ShOpts {
		&self.shopts
	}
	pub fn set_shopt(&mut self, key: &str, val: &str) -> LashResult<()> {
		let val = LashVal::parse(val)?;
		let query = key.split('.').map(|str| str.to_string()).collect::<VecDeque<String>>();
		self.shopts.set(query,val)
	}
	pub fn get_shopt<'a>(&self, key: &str) -> LashResult<String> {
		let result = &self.shopts.get(key)?;
		Ok(result.to_string().trim().to_string())
	}
	pub fn mod_flags<F>(&mut self, flag_mod: F)
		where F: FnOnce(&mut EnvFlags) {
			flag_mod(&mut self.flags)
	}
	pub fn flags(&self) -> EnvFlags {
		self.flags
	}
}


/// Override the default signal handler to manually wait on processes
pub fn disable_reaping() {
	unsafe { signal(Signal::SIGCHLD, SigHandler::Handler(crate::signal::ignore_sigchld)) }.unwrap();
}

/// Re-enable the sigchld handler
pub fn enable_reaping<'a>() -> LashResult<()> {
	write_jobs(|j| j.update_job_statuses())??;
	unsafe { signal(Signal::SIGCHLD, SigHandler::Handler(crate::signal::handle_sigchld)) }.unwrap();
	Ok(())
}

pub fn read_jobs<'a,F,T>(f: F) -> LashResult<T>
where F: FnOnce(&JobTable) -> T {
	let lock = JOBS.read().map_err(|_| Low(LashErrLow::InternalErr("Failed to obtain write lock; lock might be poisoned".into())))?;
	Ok(f(&lock))
}

pub fn write_jobs<'a,F,T>(f: F) -> LashResult<T>
where F: FnOnce(&mut JobTable) -> T {
	let mut lock = JOBS.write().map_err(|_| Low(LashErrLow::InternalErr("Failed to obtain write lock; lock might be poisoned".into())))?;
	Ok(f(&mut lock))
}

pub fn attach_tty<'a>(pgid: Pid) -> LashResult<()> {
	if !isatty(0).unwrap_or(false) || pgid == term_controller() {
		return Ok(())
	}

	if pgid == getpgrp() && term_controller() != getpgrp() {
		kill(term_controller(), Signal::SIGTTOU).ok();
	}

	let mut new_mask = nix::sys::signal::SigSet::empty();
	let mut mask_backup = nix::sys::signal::SigSet::empty();

	new_mask.add(SIGTSTP);
	new_mask.add(SIGTTIN);
	new_mask.add(SIGTTOU);
	new_mask.add(SIGCHLD);

	nix::sys::signal::pthread_sigmask(SigmaskHow::SIG_BLOCK, Some(&mut new_mask), Some(&mut mask_backup))
		.map_err(|_| io::Error::last_os_error())?;

	if unsafe { tcgetpgrp(BorrowedFd::borrow_raw(0)) == Ok(pgid) } {
		return Ok(())
	}

	// Attempt to set the process group for the terminal
	// FIXME: If this fails, it fails silently. Consider finding a more robust way to do this.
	let result = unsafe { tcsetpgrp(BorrowedFd::borrow_raw(0), pgid) };

	nix::sys::signal::pthread_sigmask(SigmaskHow::SIG_SETMASK, Some(&mut mask_backup), Some(&mut new_mask))
		.map_err(|_| io::Error::last_os_error())?;

	match result {
		Ok(_) => Ok(()),
		Err(_) => {
			// Something weird has probably happened - let's take back the terminal
			unsafe { tcsetpgrp(BorrowedFd::borrow_raw(0), getpgrp()).ok(); }
			Ok(())
		}
	}
}

pub fn term_controller() -> Pid {
	unsafe { tcgetpgrp(BorrowedFd::borrow_raw(0)) }.unwrap_or(getpgrp())
}
