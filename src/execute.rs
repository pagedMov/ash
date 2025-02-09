use core::fmt;
use std::{collections::{HashMap, HashSet, VecDeque}, env, ffi::{c_void, CString}, fmt::Display, io::{self, Write}, mem::take, os::fd::{AsFd, AsRawFd, BorrowedFd, FromRawFd, IntoRawFd, RawFd}, path::{Path, PathBuf}, sync::{Arc, Mutex, MutexGuard}};

use libc::{iovec, mode_t, MFD_CLOEXEC, O_APPEND, O_CREAT, O_RDONLY, O_RDWR, O_TRUNC, O_WRONLY};
use nix::{errno::Errno, fcntl::{fcntl, open, FcntlArg::F_GETFD, OFlag}, sys::{memfd::{memfd_create, MemFdCreateFlag}, signal::Signal, stat::{fstat, Mode}, wait::WaitStatus}, unistd::{close, dup, dup2, execve, execvpe, fork, pipe, ForkResult, Pid}};
use pest::{iterators::Pair, Parser};
use rayon::iter::IntoParallelRefIterator;

use crate::{builtin::{self, BUILTINS}, error::{LashErrExt, LashErrHigh}, exec_input, helper::{handle_fg, proc_res, StrExtension}, pair::OptPairExt, shellenv::{read_logic, read_meta, EnvFlags, SavedEnv}};
use crate::error::LashErrLow::*;
use crate::error::LashErr::*;
use crate::{error::{LashErr, LashErrLow}, expand, helper, shellenv::{self, read_vars, write_meta, write_vars, ChildProc, JobBuilder, LashVal}, LashParse, LashResult, pair::PairExt, Rule};

const SHELL_CMDS: [&str;6] = [
	"for",
	"while",
	"select",
	"match",
	"until",
	"if"
];

bitflags::bitflags! {
	#[derive(Debug,Clone,Copy)]
	pub struct ExecFlags: u32 {
		const NO_FORK    = 0b00000000000000000000000000000001;
		const BACKGROUND = 0b00000000000000000000000000000010;
		const IN_PIPE    = 0b00000000000000000000000000000100;
	}
}

#[derive(Debug,Clone)]
pub struct ExecCtx {
	redir_queue: VecDeque<Redir>,
	flags: ExecFlags,
	last_status: i32,
	depth: usize,
	state_stack: Vec<Box<ExecCtx>>,
	max_recurse_depth: usize
}

impl ExecCtx {
	pub fn new() -> Self {
		let last_status = shellenv::check_status().unwrap().parse::<i32>().unwrap();
		Self {
			redir_queue: VecDeque::new(),
			flags: ExecFlags::empty(),
			last_status,
			depth: 0,
			state_stack: vec![], // Each alteration is local to a single layer of recursion
			max_recurse_depth: read_meta(|m| m.get_shopt("core.max_recurse_depth")).unwrap().unwrap().parse::<usize>().unwrap()
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
	pub fn sort_redirs(&self) -> (Vec<Redir>,Vec<Redir>) {
		let mut in_redirs = vec![];
		let mut out_redirs = vec![];
		for redir in self.redir_queue.clone() {
			match redir.redir_type {
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
	pub fn flags_mut(&mut self) -> &mut ExecFlags {
		&mut self.flags
	}
	pub fn flags(&self) -> ExecFlags {
		self.flags
	}
	pub fn set_redirs(&mut self, redirs: VecDeque<Redir>) {
		self.redir_queue = redirs
	}
	pub fn extend_redirs(&mut self, redirs: VecDeque<Redir>) {
		self.redir_queue.extend(redirs);
	}
	pub fn push_redir(&mut self,redir: Redir) {
		self.redir_queue.push_back(redir)
	}
	pub fn pop_redir(&mut self) -> Option<Redir> {
		self.redir_queue.pop_back()
	}
	pub fn take_redirs(&mut self) -> VecDeque<Redir> {
		take(&mut self.redir_queue)
	}
	pub fn consume_redirs(&mut self) -> CmdRedirs {
		CmdRedirs::new(self.take_redirs())
	}
	pub fn activate_redirs(&mut self) -> LashResult<()> {
		let mut redirs = self.consume_redirs();
		redirs.activate()
	}
	pub fn update_status(&mut self) -> LashResult<()> {
		let status = shellenv::check_status()?;
		self.last_status = status.parse::<i32>().unwrap();
		Ok(())
	}
}

#[derive(Debug,Clone)]
pub struct Redir {
	redir_type: Rule,
	our_fd: i32,
	their_fd: Option<i32>,
	file_target: Option<PathBuf>
}

impl Redir {
	pub fn from_pair(pair: Pair<Rule>) -> LashResult<Self> {
		if let Rule::redir = pair.as_rule() {
			let mut inner = pair.into_inner();
			let mut redir_type = None;
			let mut our_fd = None;
			let mut their_fd = None;
			let mut file_target = None;
			while let Some(pair) = inner.next() {
				match pair.as_rule() {
					Rule::fd_out => {
						let fd = pair.as_str().parse::<i32>().unwrap();
						our_fd = Some(fd);
					}
					Rule::file => {
						let path = PathBuf::from(pair.as_str());
						file_target = Some(path);
					}
					Rule::fd_target => {
						let fd = pair.as_str().parse::<i32>().unwrap();
						their_fd = Some(fd);
					}
					Rule::r#in |
					Rule::out |
					Rule::force_out |
					Rule::in_out |
					Rule::append |
					Rule::heredoc |
					Rule::herestring => redir_type = Some(pair.as_rule()),
					_ => unreachable!()
				}
			}
			let our_fd = our_fd.unwrap_or(match redir_type.unwrap() {
				Rule::r#in |
				Rule::herestring |
				Rule::heredoc => 0,
				_ => 1
			});

			Ok(
				Self {
					redir_type: redir_type.unwrap(),
					our_fd,
					their_fd,
					file_target
				}
			)
		} else {
			Err(Low(LashErrLow::InternalErr(format!("Expected a redir rule in redir construction got this: {:?}", pair.as_rule()))))
		}
	}
	pub fn from_raw(our_fd: RawFd, their_fd: RawFd) -> Self {
		let redir_type = match our_fd {
			0 => Rule::r#in,
			_ => Rule::out
		};
		Self { redir_type, our_fd, their_fd: Some(their_fd), file_target: None }
	}
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct RustFd {
	fd: RawFd,
}

impl fmt::Write for RustFd {
	fn write_str(&mut self, s: &str) -> std::fmt::Result {
	  self.write(s.as_bytes()).map_err(|_| fmt::Error)?;
		Ok(())
	}
	fn write_char(&mut self, c: char) -> std::fmt::Result {
		let mut buffer = [0u8;4];
		let slice = c.encode_utf8(&mut buffer);
		self.write(slice.as_bytes()).map_err(|_| fmt::Error)?;
		Ok(())
	}
}

impl io::Write for RustFd {
	fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
		if !self.is_valid() {
			return Err(io::Error::new(io::ErrorKind::Other, "Invalid RustFd"))
		}

		let result = unsafe { libc::write(self.fd, buf.as_ptr() as *const libc::c_void, buf.len()) };

		if result < 0 {
			Err(io::Error::last_os_error())
		} else {
			Ok(result as usize)
		}
	}
	fn flush(&mut self) -> std::io::Result<()> {
		Ok(())
	}
	fn write_fmt(&mut self, fmt: std::fmt::Arguments<'_>) -> io::Result<()> {
		let mut formatted = String::new();
		fmt::write(&mut formatted, fmt).map_err(|_| io::Error::new(io::ErrorKind::Other, "Failed to write formatted string"))?;

		self.write(formatted.as_bytes())?;
		Ok(())
	}
}

impl io::Read for RustFd {
	fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
		if !self.is_valid() {
			return Err(io::Error::new(io::ErrorKind::Other, "Invalid RustFd"));
		}

		let result = unsafe { libc::read(self.as_raw_fd(), buf.as_ptr() as *mut c_void, buf.len()) };

		if result < 0 {
			let err = io::Error::last_os_error();
			let no = err.raw_os_error().unwrap();
			if no != 4 { // EINTR
				return Err(err);
			} else {
				return self.read(buf);
			}
		} else {
			return Ok(result as usize)
		}
	}
	fn read_to_end(&mut self, buf: &mut Vec<u8>) -> io::Result<usize> {
		if !self.is_valid() {
			return Err(io::Error::new(io::ErrorKind::Other, "Invalid RustFd"));
		}

		let mut temp_buf = [0u8; 4096];
		let mut total_read = 0;

		loop {
			let result = unsafe { libc::read(self.as_raw_fd(), temp_buf.as_mut_ptr() as *mut c_void, temp_buf.len()) };

			match result {
				0 => break,

				n if n > 0 => {
					let n = n as usize;
					buf.extend_from_slice(&temp_buf[..n]);
					dbg!(&buf);
					total_read += n;
				}

				n if n < 0 => {
					let err = io::Error::last_os_error();
					let no = err.raw_os_error().unwrap();
					if no != 4 { // EINTR
						return Err(err);
					} else {
						continue;
					}
				}
				_ => unreachable!(),
			}
		}

		Ok(total_read)
	}
	fn read_to_string(&mut self, buf: &mut String) -> io::Result<usize> {
		if !self.is_valid() {
			return Err(io::Error::new(io::ErrorKind::Other, "Invalid RustFd"));
		}

		let mut temp_buf = [0u8; 4096];
		let mut total_read = 0;

		loop {
			let result = unsafe { libc::read(self.as_raw_fd(), temp_buf.as_mut_ptr() as *mut c_void, temp_buf.len()) };

			match result {
				0 => break,

				n if n > 0 => {
					let n = n as usize;
					match std::str::from_utf8(&temp_buf[..n]) {
						Ok(valid) => buf.push_str(valid),
						Err(_) => return Err(io::Error::new(io::ErrorKind::InvalidData, "Invalid UTF-8"))
					}
					total_read += n;
				}

				n if n < 0 => {
					let err = io::Error::last_os_error();
					let no = err.raw_os_error().unwrap();
					if no != 4 { // EINTR
						return Err(err);
					} else {
						continue;
					}
				}
				_ => unreachable!(),
			}
		}

		Ok(total_read)
	}
	fn read_vectored(&mut self, bufs: &mut [io::IoSliceMut<'_>]) -> io::Result<usize> {
		if !self.is_valid() {
			return Err(io::Error::new(io::ErrorKind::Other, "Invalid RustFd"));
		}

		let iovcnt = bufs.len() as i32;
		let mut iovecs: Vec<iovec> = Vec::with_capacity(iovcnt as usize);

		for buf in bufs {
			iovecs.push(iovec {
				iov_base: buf.as_mut_ptr() as *mut c_void,
				iov_len: buf.len()
			});
		}

		let result = unsafe { libc::readv(self.as_raw_fd(), iovecs.as_mut_ptr(), iovcnt) };

		if result < 0 {
			return Err(io::Error::last_os_error())
		}

		Ok(result as usize)
	}
}

impl AsFd for RustFd {
	fn as_fd(&self) -> std::os::unix::prelude::BorrowedFd<'_> {
		unsafe { BorrowedFd::borrow_raw(self.as_raw_fd()) }
	}
}

impl<'a> RustFd {
	pub fn new(fd: RawFd) -> io::Result<Self> {
		if fd < 0 {
			panic!()
		}
		Ok(RustFd { fd })
	}

	/// Create a `RustFd` from a duplicate of `stdin` (FD 0)
	pub fn from_stdin() -> io::Result<Self> {
		let fd = unsafe { libc::dup(0) };
		if fd < 0 {
			Err(io::Error::last_os_error())
		} else {
			Ok(Self::new(fd)?)
		}
	}

	/// Create a `RustFd` from a duplicate of `stdout` (FD 1)
	pub fn from_stdout() -> io::Result<Self> {
		let fd = unsafe { libc::dup(1) };
		if fd < 0 {
			Err(io::Error::last_os_error())
		} else {
			Ok(Self::new(fd)?)
		}
	}

	/// Create a `RustFd` from a duplicate of `stderr` (FD 2)
	pub fn from_stderr() -> io::Result<Self> {
		let fd = unsafe { libc::dup(2) };
		if fd < 0 {
			Err(io::Error::last_os_error())
		} else {
			Ok(Self::new(fd)?)
		}
	}

	/// Create a `RustFd` from a type that provides an owned or borrowed FD
	pub fn from_fd<T: AsFd>(fd: T) -> io::Result<Self> {
		let raw_fd = fd.as_fd().as_raw_fd();
		if raw_fd < 0 {
			return Err(io::Error::new(io::ErrorKind::Other, "Invalid RustFd"));
		}
		Ok(RustFd::new(raw_fd)?)
	}

	/// Create a `RustFd` by consuming ownership of an FD
	pub fn from_owned_fd<T: IntoRawFd>(fd: T) -> io::Result<Self> {
		let raw_fd = fd.into_raw_fd(); // Consumes ownership
		if raw_fd < 0 {
			return Err(io::Error::new(io::ErrorKind::Other, "Invalid RustFd"));
		}
		Ok(RustFd::new(raw_fd)?)
	}

	/// Create a new `RustFd` that points to an in-memory file descriptor. In-memory file descriptors can be interacted with as though they were normal files.
	pub fn memfd_create(name: &str, flags: u32) -> io::Result<Self> {
		let c_name = CString::new(name).unwrap();
		let fd = unsafe { libc::memfd_create(c_name.as_ptr(), flags) };
		Ok(RustFd::new(fd)?)
	}

	/// Wrapper for nix::unistd::pipe(), simply produces two `RustFds` that point to a read and write pipe respectfully
	pub fn pipe() -> io::Result<(Self,Self)> {
		let mut fds = [0;2];
		let result = unsafe { libc::pipe(fds.as_mut_ptr()) };

		if result == -1 {
			return Err(io::Error::last_os_error())
		}
		let r_fd = RustFd::new(fds[0])?;
		let w_fd = RustFd::new(fds[1])?;
		Ok((r_fd,w_fd))
	}

	/// Produce a `RustFd` that points to the same reour_fd as the 'self' `RustFd`
	pub fn dup(&self) -> io::Result<Self> {
		if !self.is_valid() {
			return Err(io::Error::new(io::ErrorKind::Other, "Invalid RustFd"));
		}
		let duped = unsafe { libc::dup(self.as_raw_fd()) };
		Ok(RustFd::new(duped)?)
	}

	/// A wrapper for nix::unistd::dup2(), 'self' is duplicated to the given target file descriptor.
	pub fn dup2<T: AsRawFd>(&self, target: &T) -> io::Result<()> {
		let target_fd = target.as_raw_fd();
		if self.fd == target_fd {
			// Nothing to do here
			return Ok(())
		}
		if !self.is_valid() || target_fd < 0 {
			return Err(io::Error::new(io::ErrorKind::Other, "Invalid RustFd"));
		}

		unsafe { libc::dup2(self.as_raw_fd(), target_fd) };
		Ok(())
	}

	/// Open a file using a file descriptor, with the given OFlags and Mode bits
	pub fn open(path: &str, mode: mode_t) -> io::Result<Self> {
		let c_path = CString::new(path)
			.map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, format!("Invalid path: {}", e)))?;
		let file_fd = unsafe { libc::open(c_path.as_ptr(), mode as i32) };
		if file_fd < 0 {
			return Err(io::Error::last_os_error())
		}
		Ok(Self::new(file_fd)?)
	}

	pub fn std_open(path: &str) -> io::Result<Self> {
		let mode: u32 = 0o644 | O_RDWR as u32;
		Self::open(path, mode)
	}

	pub fn close(&mut self) -> io::Result<()> {
		if !self.is_valid() {
			return Ok(())
		}
		if matches!(self.as_raw_fd(), 0 | 1 | 2) {
			self.fd = -1;
			return Ok(())
		}

		let result = unsafe { libc::close(self.as_raw_fd()) };
		if result < 0 {
			self.fd = -1;
			return Err(io::Error::last_os_error())
		} else {
			self.fd = -1;
			Ok(())
		}
	}

	pub fn is_valid(&self) -> bool {
		self.fd >= 0
	}
}

impl Display for RustFd {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.fd)
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

pub struct SavedIO {
	in_fd: RustFd,
	out_fd: RustFd,
	err_fd: RustFd
}

impl SavedIO {
	pub fn new() -> LashResult<Self> {
		Ok(Self {
			in_fd: RustFd::from_stdin()?,
			out_fd: RustFd::from_stdout()?,
			err_fd: RustFd::from_stderr()?
		})
	}
	pub fn restore(self) -> LashResult<()> {
		self.in_fd.dup2(&0)?;
		self.out_fd.dup2(&1)?;
		self.err_fd.dup2(&2)?;
		std::mem::drop(self);
		Ok(())
	}
}

#[derive(Debug)]
pub struct ProcIO {
	pub stdin: Option<Arc<Mutex<RustFd>>>,
	pub stdout: Option<Arc<Mutex<RustFd>>>,
	pub stderr: Option<Arc<Mutex<RustFd>>>,
	pub backup: HashMap<RawFd,RustFd>,
}

impl<'a> ProcIO {
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
	pub fn close_all(&mut self) -> LashResult<()> {
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
	pub fn backup_fildescs(&mut self) -> LashResult<()> {
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
	pub fn restore_fildescs(&mut self) -> LashResult<()> {
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
	pub fn route_input(&mut self) -> LashResult<()> {
		if let Some(ref mut stdin_fd) = self.stdin {
			let mut fd = stdin_fd.lock().unwrap();
			fd.dup2(&0)?;
			fd.close()?;
		}
		Ok(())
	}
	pub fn route_output(&mut self) -> LashResult<()> {
		if let Some(ref mut stdout_fd) = self.stdout {
			let mut fd = stdout_fd.lock().unwrap();
			fd.dup2(&1)?;
			fd.close()?;
		}
		if let Some(ref mut stderr_fd) = self.stderr {
			let mut fd = stderr_fd.lock().unwrap();
			fd.dup2(&2)?;
			fd.close()?;
		}
		Ok(())
	}
	pub fn route_io(&mut self) -> LashResult<()> {
		self.route_input()?;
		self.route_output()?;
		Ok(())
	}
	pub fn take(&mut self) -> Self {
		std::mem::take(self)
	}
}

impl Clone for ProcIO {
	/// Use this sparingly- this was implemented to make ProcIO more wieldy when used in structs that implement Clone,
	/// but for all intents and purposes the ProcIO struct is meant to be a unique identifier for an open file descriptor.
	/// Use this if you have to, but know that it may cause unintended side effects.
	///
	/// Since ProcIO uses Arc<Mutex<RustFd>>, these clones will refer to the same data as the original. That means modifications will effect both instances.
	fn clone(&self) -> Self {
		let mut new = ProcIO::from(self.stdin.clone(),self.stdout.clone(),self.stderr.clone());
		new
	}
}

impl Default for ProcIO {
	fn default() -> Self {
		Self::new()
	}
}

fn dispatch_exec<'a>(node: Pair<'a,Rule>, ctx: &mut ExecCtx) -> LashResult<()> {
		match node.as_rule() {
			Rule::simple_cmd => {
				let command_name = node.clone().into_inner().find(|pair| pair.as_rule() == Rule::cmd_name).unpack()?.as_str();
				if BUILTINS.contains(&command_name) {
					exec_builtin(node,command_name,ctx)?;
				} else if shellenv::is_func(command_name)? {
					exec_func(node,ctx)?;
				} else {
					exec_cmd(node, ctx)?;
				}
			}
			Rule::shell_cmd => {
				let mut shell_cmd_inner = node.to_deque();
				let shell_cmd = shell_cmd_inner.pop_front().unpack()?;
				while shell_cmd_inner.front().is_some_and(|pair| pair.as_rule() == Rule::redir) {
					let redir = Redir::from_pair(shell_cmd_inner.pop_front().unpack()?)?;
					ctx.push_redir(redir);
				}
				match shell_cmd.as_rule() {
					Rule::for_cmd => exec_for_cmd(shell_cmd, ctx)?,
					Rule::match_cmd => exec_match_cmd(shell_cmd, ctx)?,
					Rule::loop_cmd => exec_loop_cmd(shell_cmd, ctx)?,
					Rule::if_cmd => exec_if_cmd(shell_cmd, ctx)?,
					Rule::subshell => exec_subshell(shell_cmd, ctx)?,
					Rule::brace_grp => todo!(),
					Rule::assignment => exec_assignment(shell_cmd, ctx)?,
					Rule::func_def => exec_func_def(shell_cmd, ctx)?,
					_ => unreachable!()
				};
			}
			Rule::pipeline => { exec_pipeline(node, ctx)?; },
			Rule::EOI => { /* Do nothing */ }
			_ => todo!("Support for rule '{:?}' is unimplemented",node.as_rule())
		}
		Ok(())
}

pub fn descend(mut node_stack: VecDeque<Pair<Rule>>, ctx: &mut ExecCtx) -> LashResult<()> {
	ctx.descend()?; // Increment depth counter
	while let Some(node) = node_stack.pop_front() {
		match node.as_rule() {
			Rule::main | Rule::cmd_list => {
				let inner = node.to_deque();
				node_stack.extend(inner);
			}
			Rule::op => {
				let option = node.step(1);
				if let Some(op) = option {
					match op.as_rule() {
						Rule::and => {
							let is_success = shellenv::check_status()? == "0";
							if !is_success {
								break
							}
						}
						Rule::or => {
							let is_success = shellenv::check_status()? == "0";
							if is_success {
								break
							}
						}
						_ => unreachable!()
					}
				}
			}
			Rule::bg_cmd => {
				if let Some(cmd) = node.step(1) {
					let flags = ctx.flags_mut();
					*flags |= ExecFlags::BACKGROUND;
					dispatch_exec(cmd, ctx)?
				}
			}
			_ => dispatch_exec(node, ctx)?
		}
	}
	ctx.ascend()?; // Decrement depth counter
	Ok(())
}

fn exec_func_def<'a>(func_def: Pair<'a,Rule>, ctx: &mut ExecCtx) -> LashResult<()> {
	let blame = func_def.clone();
	let func_name = func_def.scry(Rule::func_name).unpack()?;
	let body = func_def.scry(Rule::brace_grp).unpack()?;
	helper::write_func(func_name.as_str().trim_end_matches("()"), body.as_str().trim_matches(['{','}']).trim())?;
	shellenv::set_code(0)?;
	Ok(())
}

pub fn exec_subshell<'a>(subsh: Pair<'a,Rule>, ctx: &mut ExecCtx) -> LashResult<()> {
	let mut shebang = None;
	let body = subsh.scry(Rule::subsh_body).unpack()?.as_str();
	if let Some(subshebang) = subsh.scry(Rule::subshebang) {
		let raw_shebang = subshebang.as_str().to_string();
		shebang = Some(expand::expand_shebang(&raw_shebang));
	}

	let argv = helper::prepare_argv(subsh.clone());
	let redirs = helper::prepare_redirs(subsh)?;

	ctx.extend_redirs(redirs);
	if let Some(shebang) = shebang {
		let script = format!("{}{}",shebang,body);
		handle_external_subshell(script,argv,ctx)?;
	} else {
		handle_internal_subshell(body.to_string(),argv,ctx)?;
	}

	shellenv::set_code(0)?;
	Ok(())
}

fn handle_external_subshell(script: String, argv: VecDeque<String>, ctx: &mut ExecCtx) -> LashResult<()> {
	let argv = argv.into_iter().map(|arg| CString::new(arg).unwrap()).collect::<Vec<_>>();
	let envp = shellenv::get_cstring_evars()?;
	let mut memfd = RustFd::memfd_create("anonymous_subshell", 1)?;
	write!(memfd,"{}",script)?;

	let fd_path = CString::new(format!("/proc/self/fd/{memfd}")).unwrap();
	ctx.activate_redirs()?;

	if shellenv::in_pipe()? {
		execve(&fd_path, &argv, &envp).unwrap();
		panic!("execve() failed in subshell execution");
	}

	match unsafe { fork() } {
		Ok(ForkResult::Child) => {
			execve(&fd_path, &argv, &envp).unwrap();
			panic!("execve() failed in subshell execution");
		}
		Ok(ForkResult::Parent { child }) => {
			let children = vec![
				ChildProc::new(child, Some("anonymous_subshell"),None)?
			];
			let job = JobBuilder::new()
				.with_pgid(child)
				.with_children(children)
				.build();
			handle_fg(job)?;
		}
		Err(e) => panic!("Encountered fork error: {}",e)
	}
	memfd.close()?;
	Ok(())
}

fn handle_internal_subshell(body: String, argv: VecDeque<String>, ctx: &mut ExecCtx) -> LashResult<()> {
	let snapshot = SavedEnv::get_snapshot()?;
	ctx.activate_redirs()?;
	write_vars(|v| {
		v.reset_params();
		for arg in argv {
			v.pos_param_pushback(&arg);
		}
	})?;
	exec_input(body.consume_escapes(), ctx)?;
	snapshot.restore_snapshot()?;
	Ok(())
}

fn exec_pipeline<'a>(pipeline: Pair<'a,Rule>, ctx: &mut ExecCtx) -> LashResult<()> {
	let blame = pipeline.clone();
	let (in_redirs,out_redirs) = ctx.sort_redirs();
	let _ = ctx.take_redirs();

	let mut inner = pipeline.into_inner().peekable();
	let mut prev_read_pipe: Option<RustFd> = None;
	let mut pgid: Option<Pid> = None;
	let mut cmds: Vec<String> = vec![];
	let mut pids: Vec<Pid> = vec![];

	let mut first = true;
	while let Some(node) = inner.next() {
		let (r_pipe,w_pipe) = if inner.peek().is_some() {
			let (r_pipe,w_pipe) = RustFd::pipe()?;
			(Some(r_pipe),Some(w_pipe))
		} else {
			(None,None)
		};

		let cmd_check = node.clone();
		cmds.push(helper::get_pipeline_cmd(cmd_check)?);

		match unsafe { fork() } {
			Ok(ForkResult::Child) => {
				if let Some(mut pipe) = r_pipe {
					pipe.close()?
				}
				let _ = prev_read_pipe.as_ref()
					.map(|r| Redir::from_raw(0, r.as_raw_fd()))
					.and_then(|redir| Some(ctx.push_redir(redir)));
				let _ = w_pipe.as_ref()
					.map(|w| Redir::from_raw(1, w.as_raw_fd()))
					.and_then(|redir| Some(ctx.push_redir(redir)));
				*ctx.flags_mut() |= ExecFlags::NO_FORK;
				// These two if statements handle the case of existing i/o for the pipeline
				// Stuff like shell functions in the middle of pipelines
				if first {
					// If the pipeline started with input, redirect it here
					ctx.extend_redirs(in_redirs.into());
				}
				if inner.peek().is_none() {
					// If the pipeline ends with output, redirect it here
					ctx.extend_redirs(out_redirs.into());
				}

				dispatch_exec(node, ctx)?;
				std::process::exit(1)
			}
			Ok(ForkResult::Parent { child }) => {
				if let Some(mut pipe) = w_pipe {
					pipe.close()?
				}
				prev_read_pipe = r_pipe;
				pids.push(child);
				if pgid.is_none() {
					pgid = Some(child);
				}
				if inner.peek().is_none() {
					let mut children = vec![];
					let mut commands = cmds.iter();
					for pid in &pids {
						let cmd = commands.next().map(|cmd| cmd.as_str());
						let child = ChildProc::new(*pid,cmd,pgid)?;
						children.push(child);
					}
					let job = JobBuilder::new()
						.with_pgid(pgid.unwrap())
						.with_children(children)
						.build();

					handle_fg(job)?;
				}
			}
			Err(e) => return Err(High(LashErrHigh::exec_err("Command in pipeline failed", blame)))
		}
		if first {
			first = false;
		}
	}
	shellenv::set_code(0)?;
	Ok(())
}

fn exec_assignment<'a>(ass: Pair<'a,Rule>, ctx: &mut ExecCtx) -> LashResult<()> {
	let cmd = ass.scry(Rule::cmd_list);
	let blame = ass.clone();
	let var_name: String = ass.scry(Rule::var_ident).unpack()?.as_str().to_string();
	let assign_type = ass.scry(&[
		Rule::increment,
		Rule::decrement,
		Rule::plus_assign,
		Rule::minus_assign,
		Rule::std_assign][..]).unpack()?;
	let mut var_val: LashVal = LashVal::default();
	match assign_type.as_rule() {
		Rule::increment => {
			write_vars(|v| {
				if let Some(val) = v.get_var_mut(&var_name) {
					proc_res(val.increment(), blame).catch(); //proc_res and catch() together can throw an error in places like this
				}
			})?;
		}
		Rule::decrement => {
			write_vars(|v| {
				if let Some(val) = v.get_var_mut(&var_name) {
					proc_res(val.decrement(), blame).catch();
				}
			})?;
		}
		Rule::plus_assign => {
			let rhs = LashVal::parse(ass.scry(Rule::word).unpack()?.as_str())?;
			let var_val = read_vars(|v| v.get_var(&var_name))?;
			if var_val.clone().is_some_and(|val| &val.fmt_type() == "int") {
				if let LashVal::Int(lhs) = var_val.unwrap() {
					if let LashVal::Int(rhs) = rhs {
						let value = LashVal::Int(lhs + rhs);
						write_vars(|v| v.set_var(&var_name, LashVal::Int(lhs + rhs)))?;
					} else {
						let msg = "The right side of this assignment is invalid; expected an integer";
						return Err(High(LashErrHigh::syntax_err(msg, blame)))
					}
				} else {
					let msg = "The left side of this assignment is invalid; expected an integer";
					return Err(High(LashErrHigh::syntax_err(msg, blame)))
				}
			} else {
				let msg = "The variable in this assignment is unset";
				return Err(High(LashErrHigh::syntax_err(msg, blame)))
			}
		}
		Rule::minus_assign => {
			let rhs = LashVal::parse(ass.scry(Rule::word).unpack()?.as_str())?;
			let var_val = read_vars(|v| v.get_var(&var_name))?;
			if var_val.clone().is_some_and(|val| &val.fmt_type() == "int") {
				if let LashVal::Int(lhs) = var_val.unwrap() {
					if let LashVal::Int(rhs) = rhs {
						write_vars(|v| v.set_var(&var_name, LashVal::Int(lhs - rhs)))?;
					} else {
						let msg = "The right side of this assignment is invalid; expected an integer";
						return Err(High(LashErrHigh::syntax_err(msg, blame)))
					}
				} else {
					let msg = "The left side of this assignment is invalid; expected an integer";
					return Err(High(LashErrHigh::syntax_err(msg, blame)))
				}
			} else {
				let msg = "The variable in this assignment is unset";
				return Err(High(LashErrHigh::syntax_err(msg, blame)))
			}
		}
		Rule::std_assign => {
			var_val = LashVal::parse(ass.scry(Rule::word).unpack()?.as_str())?;
			write_vars(|v| {
				v.set_var(&var_name, var_val.clone());
			})?;
		}
		Rule::cmd_list => {}
		_ => unreachable!()
	}

	// TODO: cleanup this logic, it currently doesn't isolate the variable setting to the execution context
	if let Some(cmd) = cmd {
		// If there are commands attached, export the variables, then execute, then restore environment state
		let saved_vars = read_vars(|v| v.clone())?;
		write_vars(|v| v.export_var(&var_name, &var_val.to_string()))?;
		exec_input(cmd.as_str().to_string(), ctx)?;
		write_vars(|v| *v = saved_vars)?;
	}
	shellenv::set_code(0)?;
	Ok(())
}

fn exec_for_cmd<'a>(cmd: Pair<'a,Rule>,ctx: &mut ExecCtx) -> LashResult<()> {
	let ctx = &mut ctx.as_body();
	let mut saved_vars = HashMap::new();
	let loop_body_pair = cmd.scry(Rule::loop_body).unpack()?;
	let loop_vars = cmd.scry(Rule::for_vars)
		.unpack()?
		.into_inner()
		.into_iter()
		.map(|var| var.as_str())
		.collect::<Vec<&str>>();
	let loop_arr = cmd.scry(Rule::for_arr)
		.unpack()?
		.into_inner()
		.map(|elem| LashVal::parse(elem.as_str()).unwrap())
		.collect::<Vec<LashVal>>();

	let vars_len = loop_vars.len();
	for var in &loop_vars {
		let existing_val = read_vars(|v| v.get_var(var))?.unwrap_or_default();
		saved_vars.insert(var,existing_val);
	}

	for (i,element) in loop_arr.iter().enumerate() {
		let var_index = i % vars_len;
		write_vars(|v| v.set_var(loop_vars[var_index], element.clone()))?;
		let loop_body = expand::expand_list(loop_body_pair.clone())?;
		exec_input(loop_body, ctx)?;
	}
	for var in &loop_vars {
		let saved_val = saved_vars.remove(var).unwrap_or_default();
		write_vars(|v| v.set_var(var, saved_val))?;
	}
	shellenv::set_code(0)?;
	Ok(())
}

fn exec_match_cmd<'a>(cmd: Pair<'a,Rule>, ctx: &mut ExecCtx) -> LashResult<()> {
	let mut inner = cmd.into_inner();
	let match_pat = inner.next().unpack()?;
	let mut arms = VecDeque::new();

	while let Some(arm) = inner.next() {
		arms.push_back(arm);
	}

	while let Some(arm) = arms.pop_front() {
		let mut inner = arm.into_inner();
		let arm_pat = inner.next().unpack()?;
		let arm_body_pair = inner.next().unpack()?;

		if arm_pat.as_str().trim() == match_pat.as_str().trim() {
			let arm_body = expand::expand_list(arm_body_pair)?;
			exec_input(arm_body.trim_end_matches(',').to_string(), ctx)?;
			break
		}
	}
	shellenv::set_code(0)?;
	Ok(())
}

fn exec_if_cmd<'a>(cmd: Pair<'a,Rule>, ctx: &mut ExecCtx) -> LashResult<()> {
	let if_cond_pair = cmd.scry(Rule::if_cond).unpack()?;
	let if_body_pair = cmd.scry(Rule::if_body).unpack()?;
	let else_block = cmd.scry(Rule::else_block);
	let mut elif_blocks = cmd.into_inner().filter(|pr| pr.as_rule() == Rule::elif_block).collect::<VecDeque<_>>();

	let mut cond_ctx = ctx.as_cond();
	let mut body_ctx = ctx.as_body();

	let in_pipe = shellenv::in_pipe()?;
	if in_pipe {
		// We are going to temporarily remove this flag here, to make sure that cond/body executions fork the process
		// If we don't do this, the program will exit after executing the first condition
		write_meta(|m| m.mod_flags(|f| *f &= !EnvFlags::IN_SUB_PROC))?;
	}

	let if_cond = expand::expand_list(if_cond_pair)?;
	exec_input(if_cond, &mut cond_ctx)?;
	if &shellenv::check_status()? == "0" {
		let if_body = expand::expand_list(if_body_pair)?;
		exec_input(if_body, &mut body_ctx)?;
		return Ok(())
	}

	while let Some(elif_block) = elif_blocks.pop_front() {
		let mut inner = elif_block.into_inner();
		let elif_cond_pair = inner.next().unpack()?;
		let elif_body_pair = inner.next().unpack()?;

		let elif_cond = expand::expand_list(elif_cond_pair)?;
		exec_input(elif_cond, &mut cond_ctx)?;
		if &shellenv::check_status()? == "0" {
			let elif_body = expand::expand_list(elif_body_pair)?;
			exec_input(elif_body, &mut body_ctx)?;
			return Ok(())
		}
	}

	if let Some(else_block) = else_block {
		let else_body_pair = else_block.step(1).unpack()?;
		let else_body = expand::expand_list(else_body_pair)?;
		exec_input(else_body, &mut body_ctx)?;
	}

	if in_pipe {
		// We are going to temporarily remove this flag here, to make sure that cond/body executions fork the process
		// If we don't do this, the program will exit after executing the first condition
		write_meta(|m| m.mod_flags(|f| *f |= EnvFlags::IN_SUB_PROC))?;
	}
	shellenv::set_code(0)?;
	Ok(())
}

fn exec_loop_cmd<'a>(cmd: Pair<'a,Rule>, ctx: &mut ExecCtx) -> LashResult<()> {
	let loop_kind = cmd.scry(Rule::loop_kind).unpack()?;
	let loop_cond_pair = cmd.scry(Rule::loop_cond).unpack()?;
	let loop_body_pair = cmd.scry(Rule::loop_body).unpack()?;
	let mut cond_ctx = ctx.as_cond();
	let mut body_ctx = ctx.as_body();

	loop {
		let loop_cond = expand::expand_list(loop_cond_pair.clone())?;
		exec_input(loop_cond, &mut cond_ctx)?;
		let is_success = shellenv::check_status()? == "0";
		match loop_kind.as_str() {
			"while" => {
				if !is_success {
					write_vars(|v| v.set_param("?".into(), "0".into()))?;
					write_meta(|v| v.set_last_command("while"))?;
					break
				}
			}
			"until" => {
				if is_success {
					write_vars(|v| v.set_param("?".into(), "0".into()))?;
					write_meta(|v| v.set_last_command("until"))?;
					break
				}
			}
			_ => unreachable!()
		}
		let loop_body = expand::expand_list(loop_body_pair.clone())?;
		let result = exec_input(loop_body, &mut body_ctx);
		match result {
			Err(LashErr::High(err)) => {
				match err.get_err() {
					LashErrLow::LoopBreak(code) => {
						write_vars(|v| v.set_param("?".into(), code.to_string()))?;
						break
					}
					LashErrLow::LoopCont => continue,
					_ => return Err(LashErr::High(err))
				}
			}
			Err(e) => return Err(e),
			Ok(_) => continue,
		}
	}
	shellenv::set_code(0)?;
	Ok(())
}

fn exec_builtin(cmd: Pair<Rule>, name: &str, ctx: &mut ExecCtx) -> LashResult<()> {
	let blame = cmd.clone();
	match name {
		"test" | "[" => {
			let mut argv = cmd.to_deque();
			argv.pop_front(); // Ignore the command name
			let result = proc_res(builtin::test(&mut argv, ctx), blame)?;
			if result {
				return shellenv::set_code(0)
			} else {
				return shellenv::set_code(1)
			}
		}
		"string" | "float" | "int" | "arr" | "bool" => builtin::assign_builtin(cmd, ctx)?,
		"pushd" => builtin::pushd(cmd, ctx)?,
		"source" => builtin::source(cmd, ctx)?,
		"popd" => builtin::popd(cmd, ctx)?,
		"type" => builtin::var_type(cmd, ctx)?,
		"setopt" => builtin::setopt(cmd, ctx)?,
		"cd" => builtin::cd(cmd, ctx)?,
		"alias" => builtin::alias(cmd, ctx)?,
		"pwd" => builtin::pwd(cmd, ctx)?,
		"export" => builtin::export(cmd, ctx)?,
		"echo" => builtin::echo(cmd, ctx)?,
		_ => unimplemented!("Have not implemented support for builtin `{}` yet",name)
	};
	shellenv::set_code(0)?;
	Ok(())
}

fn exec_func(cmd: Pair<Rule>,ctx: &mut ExecCtx) -> LashResult<()> {
	let blame = cmd.clone();
	let mut argv = helper::prepare_argv(cmd);
	let func_name = argv.pop_front().unwrap();
	let body = read_logic(|l| l.get_func(&func_name).unwrap())?;
	let mut var_table = read_vars(|v| v.clone())?;
	let snapshot = SavedEnv::get_snapshot()?;

	var_table.reset_params();
	for arg in argv {
		var_table.pos_param_pushback(&arg);
	}
	write_vars(|v| *v = var_table)?;
	let result = exec_input(body, ctx);
	snapshot.restore_snapshot()?;

	// Let's check to see if the function was cut by the return command
	match &result {
		Ok(()) => {
			shellenv::set_code(0)?;
			Ok(())
		}
		Err(e) => {
			match e {
				High(high) => Err(High(high.clone())),
				Low(low) => {
					match low {
						FuncReturn(code) => {
							shellenv::set_code(*code as isize)?;
							Ok(())
						}
						_ => {
							proc_res(result,blame)
						}
					}
				}
			}
		}
	}
}

fn exec_cmd<'a>(cmd: Pair<Rule>, ctx: &mut ExecCtx) -> LashResult<()> {
	let blame = cmd.clone();
	let mut argv = helper::prepare_argv(cmd.clone());
	let mut redirs = helper::prepare_redirs(cmd)?;
	ctx.extend_redirs(redirs);
	argv.retain(|arg| !arg.is_empty() && arg != "\"\"" && arg != "''");

	if helper::validate_autocd(&argv)? {
		let arg = argv.pop_front().unwrap();
		let dir = PathBuf::from(&arg);
		return autocd(dir)
	}

	let argv = argv.into_iter().map(|arg| CString::new(arg).unwrap()).collect::<Vec<_>>();


	let command = argv.first().unwrap().clone();


	if SHELL_CMDS.contains(&command.to_str().unwrap()) {
		return Err(High(LashErrHigh::exec_err(format!("This shell command appears malformed"), blame)))
	}

	let env_vars = env::vars().into_iter().collect::<Vec<(String,String)>>();
	let envp = env_vars.iter().map(|var| CString::new(format!("{}={}",var.0,var.1)).unwrap()).collect::<Vec<_>>();

	ctx.activate_redirs()?;

	if ctx.flags().contains(ExecFlags::NO_FORK) {
		exec_external(command, argv, envp, blame);
	}

	match unsafe { fork() } {
		Ok(ForkResult::Child) => {
			exec_external(command, argv, envp, blame);
		}
		Ok(ForkResult::Parent { child }) => {
			handle_parent_process(child, command.to_str().unwrap().to_string())?;
		}
		Err(_) => todo!()
	}

	Ok(())
}

#[derive(Debug)]
pub struct CmdRedirs {
	open_fds: Vec<RustFd>,
	targets_fd: Vec<Redir>,
	targets_file: Vec<Redir>
}

impl CmdRedirs {
	pub fn new(mut redirs: VecDeque<Redir>) -> Self {
		let mut targets_fd = vec![];
		let mut targets_file = vec![];
		while let Some(redir) = redirs.pop_back() {
			let Redir { redir_type: _, our_fd: _, their_fd, file_target: _ } = &redir;
			if their_fd.is_some() {
				targets_fd.push(redir);
			} else {
				targets_file.push(redir);
			}
		}
		Self { open_fds: vec![], targets_fd, targets_file }
	}
	pub fn activate(&mut self) -> LashResult<()> {
		self.open_file_targets()?;
		self.open_their_fds()?;
		Ok(())
	}
	pub fn close_all(mut self) -> LashResult<()> {
		for fd in self.open_fds.iter_mut() {
			fd.close()?
		}
		Ok(())
	}
	pub fn open_file_targets(&mut self) -> LashResult<()> {
		for redir in &self.targets_file {
			let Redir { redir_type, our_fd, their_fd: _, file_target } = redir;
			let src_fd = RustFd::new(*our_fd)?;
			let path = file_target.as_ref().unwrap(); // We know that there's a file target so unwrap is safe
			let flags = match redir_type {
				Rule::r#in => O_RDONLY,
				Rule::out => O_WRONLY | O_CREAT | O_TRUNC,
				Rule::append => O_WRONLY | O_CREAT | O_APPEND,
				_ => unreachable!(),
			};
			let mut file_fd = RustFd::open(path.to_str().unwrap(), flags as u32)?;
			file_fd.dup2(&src_fd)?;
			file_fd.close()?;
			self.open_fds.push(src_fd);
		}
		Ok(())
	}
	pub fn open_their_fds(&mut self) -> LashResult<()> {
		for redir in &self.targets_fd {
			let Redir { redir_type: _, our_fd, their_fd, file_target: _ } = redir;
			let mut tgt_fd = RustFd::new(their_fd.unwrap())?;
			let src_fd = RustFd::new(*our_fd)?;
			tgt_fd.dup2(&src_fd)?;
			tgt_fd.close()?;
			self.open_fds.push(src_fd);
		}
		Ok(())
	}
}

fn autocd(dir: PathBuf) -> LashResult<()> {
	let cwd = env::var("PWD").map_err(|_| Low(LashErrLow::from_io()))?;
	write_vars(|v| v.export_var("OLDPWD", &cwd))?;
	env::set_current_dir(&dir).map_err(|_| Low(LashErrLow::from_io()))?;
	let cwd = env::current_dir().map_err(|_| Low(LashErrLow::from_io()))?;
	write_vars(|v| v.export_var("PWD", cwd.to_str().unwrap()))?;
	Ok(())
}

fn exec_external(command: CString, argv: Vec<CString>, envp: Vec<CString>,blame: Pair<Rule>) -> ! {
	let Err(e) = execvpe(&command, &argv, &envp);
	match e {
		Errno::ENOENT => {
			let error = High(LashErrHigh::cmd_not_found(command.to_str().unwrap(), blame));
			eprintln!("{}",error);
		}
		Errno::EACCES => {
			let error = High(LashErrHigh::no_permission(command.to_str().unwrap(), blame));
			eprintln!("{}",error);
		}
		_ => unimplemented!("Case for `{}` not implemented", e.to_string())
	}
	std::process::exit(e as i32)
}

fn handle_parent_process<'a>(child: Pid, command: String) -> LashResult<()> {
	let children = vec![
		ChildProc::new(child, Some(&command), None)?
	];
	let job = JobBuilder::new()
		.with_children(children)
		.with_pgid(child)
		.build();

	helper::handle_fg(job)?;
	Ok(())
}
