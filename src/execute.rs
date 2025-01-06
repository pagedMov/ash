use std::{collections::{HashMap, VecDeque}, ffi::CString, fmt::{self, Display}, mem::take, os::fd::{AsFd, AsRawFd, BorrowedFd, FromRawFd, IntoRawFd, RawFd}, path::Path, sync::{mpsc::{Receiver, Sender}, Arc}};

use libc::{memfd_create, MFD_CLOEXEC};
use nix::{fcntl::{open, OFlag}, sys::{signal::Signal, stat::Mode}, unistd::{close, dup, dup2, execvpe, pipe, tcsetpgrp, Pid}, NixPath};
use std::sync::Mutex;

use crate::{event::{ShError, ShEvent}, interp::{expand, parse::{self, NdFlags, NdType, Node, Span}, token::{Redir, RedirType, Tk, WdFlags}}, shellenv::{read_logic, read_meta, read_vars}, RshResult};

macro_rules! fork_instruction {
	(
		$io:expr,
		$node:expr,
		child => $child_instr:block,
		parent => $parent_instr:block
	) => {{
		use nix::unistd::{fork, ForkResult, setpgid, tcsetpgrp};

		// Perform initial setup for I/O redirection and plumbing
		$io.backup_fildescs()?;
		$io.do_plumbing()?;

		// Handle background process flag
		if $node.flags.contains(NdFlags::BACKGROUND) {
			match unsafe { fork() } {
				Ok(ForkResult::Child) => {
					$child_instr;
				}
				Ok(ForkResult::Parent { child }) => {
					// Don't wait for background processes in the parent
					setpgid(child, child).map_err(|_| ShError::from_io())?;
					unsafe { tcsetpgrp(BorrowedFd::borrow_raw(0),child) }.unwrap();
				}
				Err(_) => Err(ShError::from_io())?,
			}
		} else {
			// Handle foreground process
			match unsafe { fork() } {
				Ok(ForkResult::Child) => {
					$child_instr;
				}
				Ok(ForkResult::Parent { child }) => {
					setpgid(child, child).map_err(|_| ShError::from_io())?;
					// Set terminal control to the new process group
					unsafe { nix::unistd::tcsetpgrp(BorrowedFd::borrow_raw(0), child.into()) }.map_err(|_| ShError::from_io())?;
					$parent_instr;
				}
				Err(_) => Err(ShError::from_io())?,
			}
		}

		// Restore file descriptors
		$io.restore_fildescs()?;
		Ok::<_, ShError>(RshWait::new())
	}};
}

bitflags::bitflags! {
	#[derive(Clone,Debug,Copy)]
	pub struct ExecFlags: u8 {
		const IN_PIPE    = 0b00000001;
		const BACKGROUND = 0b00000010;
	}
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct RustFd {
	fd: RawFd,
}

impl RustFd {
	pub fn new(fd: RawFd) -> RshResult<Self> {
		if fd < 0 {
			return Err(ShError::from_internal("Attempted to create a new RustFd from a negative FD"));
		}
		Ok(RustFd { fd })
	}

	/// Create a `RustFd` from a duplicate of `stdin` (FD 0)
	pub fn from_stdin() -> RshResult<Self> {
		let fd = dup(0).map_err(|_| ShError::from_io())?;
		Ok(Self { fd })
	}

	/// Create a `RustFd` from a duplicate of `stdout` (FD 1)
	pub fn from_stdout() -> RshResult<Self> {
		let fd = dup(1).map_err(|_| ShError::from_io())?;
		Ok(Self { fd })
	}

	/// Create a `RustFd` from a duplicate of `stderr` (FD 2)
	pub fn from_stderr() -> RshResult<Self> {
		let fd = dup(2).map_err(|_| ShError::from_io())?;
		Ok(Self { fd })
	}

	/// Create a `RustFd` from a type that provides an owned or borrowed FD
	pub fn from_fd<T: AsFd>(fd: T) -> RshResult<Self> {
		let raw_fd = fd.as_fd().as_raw_fd();
		if raw_fd < 0 {
			return Err(ShError::from_internal("Attempted to convert to RustFd from a negative FD"));
		}
		Ok(RustFd { fd: raw_fd })
	}

	/// Create a `RustFd` by consuming ownership of an FD
	pub fn from_owned_fd<T: IntoRawFd>(fd: T) -> RshResult<Self> {
		let raw_fd = fd.into_raw_fd(); // Consumes ownership
		if raw_fd < 0 {
			return Err(ShError::from_internal("Attempted to convert to RustFd from a negative FD"));
		}
		Ok(RustFd { fd: raw_fd })
	}

	/// Create a new `RustFd` that points to an in-memory file descriptor. In-memory file descriptors can be interacted with as though they were normal files.
	pub fn new_memfd(name: &str, executable: bool) -> RshResult<Self> {
		let c_name = CString::new(name).map_err(|_| ShError::from_internal("Invalid name for memfd"))?;
		let flags = if executable {
			0
		} else {
			MFD_CLOEXEC
		};
		let fd = unsafe { memfd_create(c_name.as_ptr(), flags) };
		Ok(RustFd { fd })
	}

	/// Write some bytes to the contained file descriptor
	pub fn write(&self, buffer: &[u8]) -> RshResult<()> {
		if !self.is_valid() {
			return Err(ShError::from_internal("Attempted to write to an invalid RustFd"));
		}
		let result = unsafe { libc::write(self.fd, buffer.as_ptr() as *const libc::c_void, buffer.len()) };
		if result < 0 {
			Err(ShError::from_io())
		} else {
			Ok(())
		}
	}

	/// Wrapper for nix::unistd::pipe(), simply produces two `RustFds` that point to a read and write pipe respectfully
	pub fn pipe() -> RshResult<(Self,Self)> {
		let (r_pipe,w_pipe) = pipe().map_err(|_| ShError::from_io())?;
		let r_fd = RustFd::from_owned_fd(r_pipe)?;
		let w_fd = RustFd::from_owned_fd(w_pipe)?;
		Ok((r_fd,w_fd))
	}

	/// Produce a `RustFd` that points to the same resource as the 'self' `RustFd`
	pub fn dup(&self) -> RshResult<Self> {
		if !self.is_valid() {
			return Err(ShError::from_internal("Attempted to dup an invalid fd"));
		}
		let new_fd = dup(self.fd).map_err(|_| ShError::from_io())?;
		Ok(RustFd { fd: new_fd })
	}

	/// A wrapper for nix::unistd::dup2(), 'self' is duplicated to the given target file descriptor.
	pub fn dup2<T: AsRawFd>(&self, target: &T) -> RshResult<()> {
		let target_fd = target.as_raw_fd();
		if self.fd == target_fd {
			// Nothing to do here
			return Ok(())
		}
		if !self.is_valid() || target_fd < 0 {
			return Err(ShError::from_io());
		}

		dup2(self.fd, target_fd).map_err(|_| ShError::from_io())?;
		Ok(())
	}

	/// Open a file using a file descriptor, with the given OFlags and Mode bits
	pub fn open(path: &Path, flags: OFlag, mode: Mode) -> RshResult<Self> {
		let file_fd: RawFd = open(path, flags, mode).map_err(|_| ShError::from_io())?;
		Ok(Self { fd: file_fd })
	}

	pub fn close(&mut self) -> RshResult<()> {
		if !self.is_valid() {
			return Err(ShError::from_internal("Attempted to close an invalid RustFd"));
		}
		close(self.fd).map_err(|_| ShError::from_io())?;
		self.fd = -1;
		Ok(())
	}

	pub fn mk_shared(self) -> Arc<Mutex<Self>> {
		Arc::new(Mutex::new(self))
	}

	pub fn is_valid(&self) -> bool {
		self.fd > 0
	}
}

impl Display for RustFd {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.fd)
	}
}

impl Drop for RustFd {
	fn drop(&mut self) {
		if self.fd >= 0 {
			self.close().ok();
		}
	}
}

impl AsRawFd for RustFd {
	fn as_raw_fd(&self) -> RawFd {
		self.fd
	}
}

impl IntoRawFd for RustFd {
	fn into_raw_fd(self) -> RawFd {
		let fd = self.fd;
		std::mem::forget(self);
		fd
	}
}

impl FromRawFd for RustFd {
	unsafe fn from_raw_fd(fd: RawFd) -> Self {
		RustFd { fd }
	}
}

#[derive(PartialEq,Debug,Clone)]
pub enum RshWait {
	Success,
	Fail { code: i32, cmd: Option<String> },
	Signaled { sig: Signal },
	Stopped { sig: Signal },
	Terminated { signal: i32 },
	Continued,
	Running,
	Killed { signal: i32 },
	TimeOut,

	// These wait statuses are returned by builtins like `return` and `break`
	SIGRETURN, // Return from a function
	SIGCONT, // Restart a loop from the beginning
	SIGBREAK, // Break a loop
	SIGRSHEXIT // Internal call to exit early
}

impl Display for RshWait {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			RshWait::Success { .. } => write!(f, "done"),
			RshWait::Fail { code, .. } => write!(f, "exit {}", code),
			RshWait::Signaled { sig } => write!(f, "exit {}", sig),
			RshWait::Stopped { sig } => write!(f, "stopped {}", sig),
			RshWait::Terminated { signal } => write!(f, "terminated {}", signal),
			RshWait::Continued => write!(f, "continued"),
			RshWait::Running => write!(f, "running"),
			RshWait::Killed { signal } => write!(f, "killed {}", signal),
			RshWait::TimeOut => write!(f, "time out"),
			_ => write!(f, "{:?}",self)
		}
	}
}

impl RshWait {
	pub fn new() -> Self {
		RshWait::Success
	}
	pub fn s(span: Span) -> Self {
		RshWait::Success
	}
	pub fn f(code: i32, cmd: Option<String>, span: Span) -> Self {
		RshWait::Fail { code, cmd, }
	}
}

impl Default for RshWait {
	fn default() -> Self {
		RshWait::new()
	}
}

#[derive(Debug)]
pub struct ProcIO {
	pub stdin: Option<Arc<Mutex<RustFd>>>,
	pub stdout: Option<Arc<Mutex<RustFd>>>,
	pub stderr: Option<Arc<Mutex<RustFd>>>,
	pub backup: HashMap<RawFd,RustFd>
}

impl ProcIO {
	pub fn new() -> Self {
		Self { stdin: None, stdout: None, stderr: None, backup: HashMap::new() }
	}
	pub fn from(stdin: Option<Arc<Mutex<RustFd>>>, stdout: Option<Arc<Mutex<RustFd>>>, stderr: Option<Arc<Mutex<RustFd>>>) -> Self {
		Self {
			stdin,
			stdout,
			stderr,
			backup: HashMap::new(),
		}
	}
	pub fn close_all(&mut self) -> RshResult<()> {
		if let Some(fd) = &self.stdin {
			fd.lock().unwrap().close()?;
		}
		if let Some(fd) = &self.stdout {
			fd.lock().unwrap().close()?;
		}
		if let Some(fd) = &self.stderr {
			fd.lock().unwrap().close()?;
		}
		Ok(())
	}
	pub fn backup_fildescs(&mut self) -> RshResult<()> {
		let mut backup = HashMap::new();
		// Get duped file descriptors
		let dup_in = RustFd::from_stdin()?;
		let dup_out = RustFd::from_stdout()?;
		let dup_err = RustFd::from_stderr()?;
		// Store them in a hashmap
		backup.insert(0,dup_in);
		backup.insert(1,dup_out);
		backup.insert(2,dup_err);
		self.backup = backup;
		Ok(())
	}
	pub fn restore_fildescs(&mut self) -> RshResult<()> {
		// Get duped file descriptors from hashmap
		if !self.backup.is_empty() {
			// Dup2 to restore file descriptors
			if let Some(mut saved_in) = self.backup.remove(&0) {
				saved_in.dup2(&0)?;
				saved_in.close()?;
			}
			if let Some(mut saved_out) = self.backup.remove(&1) {
				saved_out.dup2(&1)?;
				saved_out.close()?;
			}
			if let Some(mut saved_err) = self.backup.remove(&2) {
				saved_err.dup2(&2)?;
				saved_err.close()?;
			}
		}
		Ok(())
	}
	pub fn do_plumbing(&mut self) -> RshResult<()> {
		if let Some(ref mut err_pipe) = self.stderr {
			let mut pipe = err_pipe.lock().unwrap();
			pipe.dup2(&2)?;
			pipe.close()?;
		}
		// Redirect stdout
		if let Some(ref mut w_pipe) = self.stdout {
			let mut pipe = w_pipe.lock().unwrap();
			pipe.dup2(&1)?;
			pipe.close()?;
		}

		// Redirect stdin
		if let Some(ref mut r_pipe) = self.stdin {
			let mut pipe = r_pipe.lock().unwrap();
			pipe.dup2(&0)?;
			pipe.close()?;
		}
		Ok(())
	}
}

impl Clone for ProcIO {
	/// Use this sparingly- this was implemented to make ProcIO more wieldy when used in structs that implement Clone,
	/// but for all intents and purposes the ProcIO struct is meant to be a unique identifier for an open file descriptor.
	/// Use this if you have to, but know that it may cause unintended side effects.
	///
	/// Since ProcIO uses Arc<Mutex<RustFd>>, these clones will refer to the same data as the original. That means modifications will effect both instances.
	fn clone(&self) -> Self {
		ProcIO::from(self.stdin.clone(),self.stdout.clone(),self.stderr.clone())
	}
}

impl Default for ProcIO {
	fn default() -> Self {
		Self::new()
	}
}

#[derive(Debug,Clone)]
pub struct ExecUnit {
	name: Option<String>,
	tokens: Vec<Tk>,
	node: Box<Node>,
	io: Option<ProcIO>,
	env_overrides: Option<HashMap<String,String>>,
	flags: ExecFlags,
}

impl ExecUnit {
	pub fn new(node: Node, io: Option<ProcIO>, env_overrides: Option<HashMap<String,String>>) -> Self {
		let node = Box::new(node);
		let mut name = if node.command.is_some() {
			node.command.as_ref().unwrap().text().to_string()
		} else {
			String::new()
		};
		let tokens = if !name.is_empty() {
			if let NdType::Command { ref argv } | NdType::Builtin { ref argv } = node.nd_type {
				argv.iter().cloned().collect::<Vec<Tk>>()
			} else { unreachable!() }
		} else if let NdType::Subshell { body: _, ref argv } = node.nd_type {
			name = "Anonymous subshell".to_string();
			argv.iter().cloned().collect::<Vec<Tk>>()
		} else {
			vec![]
		};
		let mut flags = ExecFlags::empty();
		if node.flags.contains(NdFlags::BACKGROUND) { flags |= ExecFlags::BACKGROUND };
		if node.flags.contains(NdFlags::IN_PIPE) { flags |= ExecFlags::IN_PIPE };
		let name = if name.is_empty() { None } else { Some(take(&mut name)) };
		Self {
			name,
			tokens,
			node,
			io,
			env_overrides,
			flags
		}
	}
}

pub struct ExecDispatcher {
	inbox: Receiver<Node>,
	outbox: Sender<ShEvent>
}

impl ExecDispatcher {
	pub fn new(inbox: Receiver<Node>, outbox: Sender<ShEvent>) -> Self {
		Self { inbox, outbox }
	}
	pub fn run(&self) -> RshResult<RshWait> {
		let mut status = RshWait::new();
		for tree in self.inbox.iter() {
			status = traverse_ast(tree)?;
		}
		Ok(status)
	}
}

pub fn traverse_ast(ast: Node) -> RshResult<RshWait> {
	let saved_in = RustFd::from_stdin()?;
	let saved_out = RustFd::from_stdout()?;
	let saved_err = RustFd::from_stderr()?;
	let status = traverse_root(ast, None, ProcIO::new())?;
	saved_in.dup2(&0)?;
	saved_out.dup2(&1)?;
	saved_err.dup2(&2)?;
	Ok(status)
}


fn traverse(node: Node, io: ProcIO) -> RshResult<RshWait> {
	let last_status;
	match node.nd_type {
		NdType::Command { ref argv } | NdType::Builtin { ref argv } => {
			let mut node = node.clone();
			let command_name = argv.front().unwrap();
			let not_from_alias = !command_name.flags().contains(WdFlags::FROM_ALIAS);
			let is_not_command_builtin = command_name.text() != "command";
			if not_from_alias && is_not_command_builtin {
				node = expand::expand_alias(node.clone())?;
			}
			if let Some(_func) = read_logic(|log| log.get_func(command_name.text()))? {
				//last_status = handle_function(node, io)?;
				todo!()
			} else if !matches!(node.nd_type, NdType::Command {..} | NdType::Builtin {..}) {
				// If the resulting alias expansion returns a root node
				// then traverse the resulting sub-tree
				return traverse_root(node, None, io)
			} else {
				match node.nd_type {
					NdType::Command {..} => {
						last_status = handle_command(node, io)?;
					}
					NdType::Builtin {..} => {
						//last_status = handle_builtin(node, io)?;
						todo!()
					}
					_ => unreachable!()
				}
			}
		}
		NdType::Pipeline {..} => {
			//last_status = handle_pipeline(node, io)?;
			todo!()
		}
		NdType::Chain {..} => {
			//last_status = handle_chain(node)?;
			todo!()
		}
		NdType::If {..} => {
			//last_status = handle_if(node,io)?;
			todo!()
		}
		NdType::For {..} => {
			//last_status = handle_for(node,io)?;
			todo!()
		}
		NdType::Loop {..} => {
			//last_status = handle_loop(node,io)?;
			todo!()
		}
		NdType::Case {..} => {
			//last_status = handle_case(node,io)?;
			todo!()
		}
		NdType::Select {..} => {
			todo!("handle select")
		}
		NdType::Subshell {..} => {
			//last_status = handle_subshell(node,io)?;
			todo!()
		}
		NdType::FuncDef {..} => {
			//last_status = handle_func_def(node)?;
			todo!()
		}
		NdType::Assignment {..} => {
			//last_status = handle_assignment(node)?;
			todo!()
		}
		NdType::Cmdsep => {
			last_status = RshWait::new();
		}
		_ => unimplemented!("Support for node type `{:?}` is not yet implemented",node.nd_type)
	}
	Ok(last_status)
}

fn traverse_root(mut node: Node, break_condition: Option<bool>, io: ProcIO) -> RshResult<RshWait> {
	let mut last_status = RshWait::new();
	if !node.redirs.is_empty() {
		node = parse::propagate_redirections(node)?;
	}
	if let NdType::Root { deck } = node.nd_type {
		for node in &deck {
			last_status = traverse(node.clone(), io.clone())?;
			if let Some(condition) = break_condition {
				match condition {
					true => {
						if let RshWait::Fail {..} = last_status {
							break
						}
					}
					false => {
						if let RshWait::Success  = last_status {
							break
						}
					}
				}
			}
		}
	}
	Ok(last_status)
}

fn handle_command(mut node: Node, mut io: ProcIO) -> RshResult<RshWait> {
	let argv = expand::expand_arguments(&mut node)?;
	let argv = argv.iter().map(|arg| CString::new(arg.text()).unwrap()).collect::<Vec<CString>>();
	let redirs = node.get_redirs()?;

	if let NdType::Command { ref argv } = node.nd_type {
		if read_meta(|m| m.get_shopt("autocd").is_some_and(|opt| opt > 0))? && argv.len() == 1 {
			let path_cand = argv.front().unwrap();
			let is_relative = path_cand.text().starts_with('.');
			let contains_slash = path_cand.text().contains('/');
			let path_exists = Path::new(path_cand.text()).is_dir();

			if (is_relative || contains_slash) && path_exists {
				let argv = node.get_argv()?;
				return handle_autocd(node.clone(), argv, path_cand.flags(),io);
			}
		}
	}

	let (command,envp) = prepare_execvpe(&argv)?;

	fork_instruction!(io,node,
		child => {
			let mut open_fds = VecDeque::new();
			if !redirs.is_empty() {
				open_fds.extend(handle_redirs(redirs.clone().into())?);
			}
			let Err(_) = execvpe(&command,&argv,&envp);
			std::process::exit(127);
		},
		parent => { println!("parent process") }
	)
}

fn handle_redirs(mut redirs: VecDeque<Node>) -> RshResult<VecDeque<RustFd>> {
	let mut fd_queue: VecDeque<RustFd> = VecDeque::new();
	let mut fd_dupes: VecDeque<Redir> = VecDeque::new();

	while let Some(redir_tk) = redirs.pop_front() {
		if let NdType::Redirection { ref redir } = redir_tk.nd_type {
			let Redir { fd_source, op, fd_target, file_target } = &redir;
			if fd_target.is_some() {
				fd_dupes.push_back(redir.clone());
			} else if let Some(file_path) = file_target {
				let source_fd = RustFd::new(*fd_source)?;
				let flags = match op {
					RedirType::Input => OFlag::O_RDONLY,
					RedirType::Output => OFlag::O_WRONLY | OFlag::O_CREAT | OFlag::O_TRUNC,
					RedirType::Append => OFlag::O_WRONLY | OFlag::O_CREAT | OFlag::O_APPEND,
					_ => unimplemented!()
				};
				let mut file_fd = RustFd::open(Path::new(file_path.text()), flags, Mode::from_bits(0o644).unwrap())?;
				file_fd.dup2(&source_fd)?;
				file_fd.close()?;
				fd_queue.push_back(source_fd);
			}
		}
	}

	while let Some(dupe_redir) = fd_dupes.pop_front() {
		let Redir { fd_source, op: _, fd_target, file_target: _ } = dupe_redir;
		let mut target_fd = RustFd::new(fd_target.unwrap())?;
		let source_fd = RustFd::new(fd_source)?;
		target_fd.dup2(&source_fd)?;
		target_fd.close()?;
		fd_queue.push_back(source_fd);
	}

	Ok(fd_queue)
}

fn prepare_execvpe(argv: &[CString]) -> RshResult<(CString, Vec<CString>)> {
	let command = argv[0].clone();

	// Clone the environment variables into a temporary structure
	let env_vars: Vec<(String, String)> = read_vars(|vars| {
		vars.borrow_evars()
			.iter()
			.map(|(k, v)| (k.clone(), v.clone()))
			.collect()
	})?;

	// Convert the environment variables into CString
	let envp = env_vars
		.iter()
		.map(|(k, v)| {
			let env_pair = format!("{}={}", k, v);
			CString::new(env_pair).expect("Failed to create CString")
		})
	.collect::<Vec<CString>>();

		Ok((command, envp))
}

fn handle_autocd(node: Node, argv: Vec<Tk>,flags: WdFlags,io: ProcIO) -> RshResult<RshWait> {
	let cd_token = Tk::new("cd".into(), node.span(), flags);
	let mut autocd_argv = VecDeque::from(argv);
	autocd_argv.push_front(cd_token.clone());
	let autocd = Node {
		command: Some(cd_token),
		nd_type: NdType::Builtin { argv: autocd_argv },
		span: node.span(),
		flags: node.flags,
		redirs: node.redirs
	};
	traverse(autocd,io)
}
