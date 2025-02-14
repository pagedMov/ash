use libc::{S_IRGRP, S_IROTH, S_IRUSR, S_IWUSR};
use once_cell::sync::Lazy;
use regex::Regex;

use crate::{helper, prelude::*, shellenv::{ChildProc, JobBuilder}};

pub const SIG_EXIT_OFFSET: i32 = 128;

pub static REGEX: Lazy<HashMap<&'static str, Regex>> = Lazy::new(|| {
	let mut regex = HashMap::new();
	regex.insert("var_index", Regex::new(r"(\w+)\[(\d+)\]").unwrap());
	regex.insert("redirection",Regex::new(r"^(?P<fd_out>[0-9]+)?(?P<operator>>{1,2}|<{1,3})(?:(?:[&]?(?P<fd_target>[0-9]+))|(?P<file_target>[a-zA-Z0-9-\.]*))?$").unwrap());
	regex.insert("rsh_shebang",Regex::new(r"^#!((?:/[^\s]+)+)((?:\s+arg:[a-zA-Z][a-zA-Z0-9_\-]*)*)$").unwrap());
	regex.insert("brace_expansion",Regex::new(r"(\$?)\{(?:[\x20-\x7e,]+|[0-9]+(?:\.\.[0-9+]){1,2}|[a-z]\.\.[a-z]|[A-Z]\.\.[A-Z])\}").unwrap());
	regex.insert("process_sub",Regex::new(r"^>\(.*\)$").unwrap());
	regex.insert("command_sub",Regex::new(r"^\$\([^\)]+\)$").unwrap());
	regex.insert("arithmetic",Regex::new(r"^\$\(\([^\)]+\)\)$").unwrap());
	regex.insert("subshell",Regex::new(r"^\([^\)]+\)$").unwrap());
	regex.insert("test",Regex::new(r"^\[\s*(.*?)\s*\]$").unwrap());
	regex.insert("sng_string",Regex::new(r#"^\'([^\']*)\'$"#).unwrap());
	regex.insert("dub_string",Regex::new(r#"^\"([^\"]*)\"$"#).unwrap());
	regex.insert("var_sub",Regex::new(r"\$(?:([A-Za-z_][A-Za-z0-9_]*)|\{([A-Za-z_][A-Za-z0-9_]*)\})").unwrap());
	regex.insert("assignment",Regex::new(r"^[A-Za-z_][A-Za-z0-9_]*=.*$").unwrap());
	regex.insert("funcdef",Regex::new(r"^[\x20-\x7E]*\(\)\s+\{[\s\S]*?\}").unwrap());
	regex.insert("operator",Regex::new(r"(?:&&|\|\||[><]=?|[|&])").unwrap());
	regex.insert("cmdsep",Regex::new(r"^(?:\n|;)$").unwrap());
	regex.insert("ident",Regex::new(r"^[\x20-\x7E]*$").unwrap());
	regex.insert("glob_braces",Regex::new(r".*\[[A-Za-z0-9-_]+\].*").unwrap());
	regex.insert("find_do",Regex::new(r"(?P<loop_cond>.*?)(?P<kw>[\n;]*\s*do\s+)$").unwrap());
	regex.insert("find_done",Regex::new(r"(?P<loop_body>.*?)(?P<kw>[\n;]*\s*done(?:[\s;]*|\z))$").unwrap());
	regex.insert("ansi",Regex::new(r"\x1B\[[0-9;]*m").unwrap());
	regex
});

pub const SHELL_CMDS: [&str;6] = [
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
		const NO_FORK       = 0b00000000000000000000000000000001;
		const BACKGROUND    = 0b00000000000000000000000000000010;
		const IN_PIPE       = 0b00000000000000000000000000000100;
		const IGN_FUNC      = 0b00000000000000000000000000001000;
		const NO_RESET_IN   = 0b00000000000000000000000000010000;
		const NO_RESET_OUT  = 0b00000000000000000000000000100000;
		const NO_RESET_ERR  = 0b00000000000000000000000001000000;
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
	pub fn from_pair(pair: Pair<Rule>) -> SlashResult<Self> {
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
			Err(Low(SlashErrLow::InternalErr(format!("Expected a redir rule in redir construction got this: {:?}", pair.as_rule()))))
		}
	}
	pub fn from_raw(our_fd: RawFd, their_fd: RawFd) -> Self {
		let redir_type = match our_fd {
			0 => Rule::r#in,
			_ => Rule::out
		};
		Self { redir_type, our_fd, their_fd: Some(their_fd), file_target: None }
	}
	pub fn redir_type(&self) -> Rule {
		self.redir_type
	}
}

#[derive(Debug)]
pub struct CmdRedirs {
	open_fds: Vec<SmartFD>,
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
	pub fn activate(&mut self) -> SlashResult<()> {
		self.open_file_targets()?;
		self.open_their_fds()?;
		Ok(())
	}
	pub fn close_all(mut self) -> SlashResult<()> {
		for fd in self.open_fds.iter_mut() {
			fd.close()?
		}
		Ok(())
	}
	pub fn open_file_targets(&mut self) -> SlashResult<()> {
		for redir in &self.targets_file {
			let Redir { redir_type, our_fd, their_fd: _, file_target } = redir;
			let src_fd = SmartFD::new(*our_fd)?;
			let path = file_target.as_ref().unwrap(); // We know that there's a file target so unwrap is safe
			let flags = match redir_type {
				Rule::r#in => OFlag::O_RDONLY,
				Rule::out => OFlag::O_WRONLY | OFlag::O_CREAT | OFlag::O_TRUNC,
				Rule::append => OFlag::O_WRONLY | OFlag::O_CREAT | OFlag::O_APPEND,
				_ => unreachable!(),
			};
			let mode = Mode::from_bits(0o644).unwrap();
			let mut file_fd = SmartFD::open(path, flags, mode)?;
			file_fd.dup2(&src_fd)?;
			file_fd.close()?;
			self.open_fds.push(src_fd);
		}
		Ok(())
	}
	pub fn open_their_fds(&mut self) -> SlashResult<()> {
		for redir in &self.targets_fd {
			let Redir { redir_type: _, our_fd, their_fd, file_target: _ } = redir;
			let mut tgt_fd = SmartFD::new(their_fd.unwrap())?;
			let src_fd = SmartFD::new(*our_fd)?;
			tgt_fd.dup2(&src_fd)?;
			tgt_fd.close()?;
			self.open_fds.push(src_fd);
		}
		Ok(())
	}
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct SmartFD {
	fd: RawFd,
}

impl fmt::Write for SmartFD {
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

impl io::Write for SmartFD {
	fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
		if !self.is_valid() {
			return Err(io::Error::new(io::ErrorKind::Other, "Invalid SmartFD"))
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
		fmt::write(&mut formatted, fmt).unwrap(); // FIXME: do not leave this unwrap unhandled for long

		self.write(formatted.as_bytes())?;
		Ok(())
	}
}

impl io::Read for SmartFD {
	fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
		if !self.is_valid() {
			return Err(io::Error::new(io::ErrorKind::Other, "Invalid SmartFD"));
		}

		match nix::unistd::read(self.as_raw_fd(), buf) {
			Ok(num_bytes) => Ok(num_bytes), // Return the number of bytes read
			Err(nix::errno::Errno::EINTR) => self.read(buf), // Retry if interrupted
			Err(_) => Err(io::Error::last_os_error()), // Convert other errors
		}
	}
	fn read_to_end(&mut self, buf: &mut Vec<u8>) -> io::Result<usize> {
		if !self.is_valid() {
			return Err(io::Error::new(io::ErrorKind::Other, "Invalid SmartFD"));
		}

		let mut temp_buf = [0u8; 4096];
		let mut total_read = 0;

		loop {
			match nix::unistd::read(self.as_raw_fd(), &mut temp_buf) {
				Ok(0) => break,
				Ok(n) => {
					buf.extend_from_slice(&temp_buf[..n]);
					total_read += n;
				}
				Err(nix::errno::Errno::EINTR) => continue,
				Err(_) => return Err(io::Error::last_os_error())
			}
		}
		Ok(total_read)
	}
	fn read_to_string(&mut self, buf: &mut String) -> io::Result<usize> {
		if !self.is_valid() {
			return Err(io::Error::new(io::ErrorKind::Other, "Invalid SmartFD"));
		}

		let mut temp_buf = [0u8; 4096];
		let mut total_read = 0;

		loop {
			match nix::unistd::read(self.as_raw_fd(), &mut temp_buf) {
				Ok(0) => break,
				Ok(n) => {
					match std::str::from_utf8(&temp_buf[..n]) {
						Ok(valid) => buf.push_str(valid),
						Err(_) => return Err(io::Error::new(io::ErrorKind::InvalidData, "Invalid UTF-8"))
					}
					total_read += n;
				}
				Err(nix::errno::Errno::EINTR) => continue,
				Err(_) => return Err(io::Error::last_os_error())
			}
		}
		Ok(total_read)
	}
	fn read_vectored(&mut self, bufs: &mut [io::IoSliceMut<'_>]) -> io::Result<usize> {
		if !self.is_valid() {
			return Err(io::Error::new(io::ErrorKind::Other, "Invalid SmartFD"));
		}

		match nix::sys::uio::readv(&mut *self, bufs) {
			Ok(n) => Ok(n),
			Err(nix::errno::Errno::EINTR) => self.read_vectored(bufs),
			Err(_) => Err(io::Error::last_os_error())
		}
	}
}

impl AsFd for SmartFD {
	fn as_fd(&self) -> std::os::unix::prelude::BorrowedFd<'_> {
		unsafe { BorrowedFd::borrow_raw(self.as_raw_fd()) }
	}
}

impl<'a> SmartFD {
	pub fn new(fd: RawFd) -> SlashResult<Self> {
		if fd < 0 {
			panic!()
		}
		Ok(SmartFD { fd })
	}

	/// Create a `SmartFD` from a duplicate of `stdin` (FD 0)
	pub fn from_stdin() -> SlashResult<Self> {
		let fd = dup(0).map_err(|_| Low(SlashErrLow::from_io()))?;
		Ok(Self { fd })
	}

	/// Create a `SmartFD` from a duplicate of `stdout` (FD 1)
	pub fn from_stdout() -> SlashResult<Self> {
		let fd = dup(1).map_err(|_| Low(SlashErrLow::from_io()))?;
		Ok(Self { fd })
	}

	/// Create a `SmartFD` from a duplicate of `stderr` (FD 2)
	pub fn from_stderr() -> SlashResult<Self> {
		let fd = dup(2).map_err(|_| Low(SlashErrLow::from_io()))?;
		Ok(Self { fd })
	}

	/// Create a `SmartFD` from a type that provides an owned or borrowed FD
	pub fn from_fd<T: AsFd>(fd: T) -> SlashResult<Self> {
		let raw_fd = fd.as_fd().as_raw_fd();
		if raw_fd < 0 {
			return Err(Low(SlashErrLow::BadFD("Attempted to create a SmartFD from a negative int".into())))
		}
		Ok(SmartFD { fd: raw_fd })
	}

	/// Create a `SmartFD` by consuming ownership of an FD
	pub fn from_owned_fd<T: IntoRawFd>(fd: T) -> SlashResult<Self> {
		let raw_fd = fd.into_raw_fd(); // Consumes ownership
		if raw_fd < 0 {
			return Err(Low(SlashErrLow::BadFD("Attempted to create a SmartFD from a negative int".into())))
		}
		Ok(SmartFD { fd: raw_fd })
	}

	/// Create a new `SmartFD` that points to an in-memory file descriptor. In-memory file descriptors can be interacted with as though they were normal files.
	pub fn new_memfd(name: &str, executable: bool) -> SlashResult<Self> {
		let c_name = CString::new(name).unwrap();
		let flags = if executable {
			MemFdCreateFlag::empty()
		} else {
			MemFdCreateFlag::MFD_CLOEXEC
		};
		let fd = memfd_create(&c_name, flags).map_err(|_| Low(SlashErrLow::from_io()))?;
		Ok(SmartFD { fd: fd.as_raw_fd() })
	}

	/// Wrapper for nix::unistd::pipe(), simply produces two `SmartFDs` that point to a read and write pipe respectfully
	pub fn pipe() -> SlashResult<(Self,Self)> {
		let (r_pipe,w_pipe) = pipe().map_err(|_| Low(SlashErrLow::from_io()))?;
		let r_fd = SmartFD::from_owned_fd(r_pipe)?;
		let w_fd = SmartFD::from_owned_fd(w_pipe)?;
		Ok((r_fd,w_fd))
	}

	/// Produce a `SmartFD` that points to the same resource as the 'self' `SmartFD`
	pub fn dup(&self) -> SlashResult<Self> {
		if !self.is_valid() {
			return Err(Low(SlashErrLow::BadFD("Attempted to call `dup()` on an invalid SmartFD".into())))
		}
		let new_fd = dup(self.fd).unwrap();
		Ok(SmartFD { fd: new_fd })
	}

	/// A wrapper for nix::unistd::dup2(), 'self' is duplicated to the given target file descriptor.
	pub fn dup2<T: AsRawFd>(&self, target: &T) -> SlashResult<()> {
		let target_fd = target.as_raw_fd();
		if self.fd == target_fd {
			// Nothing to do here
			return Ok(())
		}
		if !self.is_valid() || target_fd < 0 {
			return Err(Low(SlashErrLow::BadFD("Attempted to call `dup2()` on an invalid SmartFD".into())))
		}

		dup2(self.fd, target_fd).unwrap();
		Ok(())
	}

	/// Open a file using a file descriptor, with the given OFlags and Mode bits
	pub fn open(path: &Path, flags: OFlag, mode: Mode) -> SlashResult<Self> {
		let file_fd = open(path, flags, mode);
		if let Ok(file_fd) = file_fd {
			Ok(Self { fd: file_fd })
		} else {
			return Err(Low(SlashErrLow::BadFD(format!("Attempted to open non-existant file '{}'",path.to_str().unwrap()))))
		}
	}

	pub fn std_open(path: &Path) -> SlashResult<Self> {
		let flags = OFlag::O_RDWR;
		let mode = Mode::from_bits(0o644).unwrap();
		let fd = open(path, flags, mode);
		if let Ok(file) = fd {
			Ok(Self { fd: file})
		} else {
			return Err(Low(SlashErrLow::BadFD(format!("Attempted to open non-existant file '{}'",path.to_str().unwrap()))))
		}
	}

	pub fn close(&mut self) -> SlashResult<()> {
		if !self.is_valid() {
			return Ok(())
		}
		if matches!(self.as_raw_fd(), 0 | 1 | 2) {
			self.fd = -1;
			return Ok(())
		}

		close(self.fd).unwrap();
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

impl Display for SmartFD {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.fd)
	}
}

impl Drop for SmartFD {
	#[track_caller]
	fn drop(&mut self) {
		if self.fd >= 0 && self.close().is_err() {
		}
	}
}

impl AsRawFd for SmartFD {
	fn as_raw_fd(&self) -> RawFd {
		self.fd
	}
}

impl IntoRawFd for SmartFD {
	fn into_raw_fd(self) -> RawFd {
		let fd = self.fd;
		std::mem::forget(self);
		fd
	}
}

impl FromRawFd for SmartFD {
	unsafe fn from_raw_fd(fd: RawFd) -> Self {
		SmartFD { fd }
	}
}

pub fn exec_external(command: CString, argv: Vec<CString>, envp: Vec<CString>,blame: Pair<Rule>) -> ! {
	let Err(e) = execvpe(&command, &argv, &envp);
	match e {
		Errno::ENOENT => {
			let error = High(SlashErrHigh::cmd_not_found(command.to_str().unwrap(), blame));
			eprintln!("{}",error);
		}
		Errno::EACCES => {
			let error = High(SlashErrHigh::no_permission(command.to_str().unwrap(), blame));
			eprintln!("{}",error);
		}
		_ => unimplemented!("Case for `{}` not implemented", e.to_string())
	}
	std::process::exit(e as i32)
}

pub fn handle_parent_process<'a>(child: Pid, command: String, slash: &mut Slash) -> SlashResult<()> {
	let children = vec![
		ChildProc::new(child, Some(&command), None)?
	];
	let job = JobBuilder::new()
		.with_children(children)
		.with_pgid(child)
		.build();

	helper::handle_fg(slash,job)?;
	Ok(())
}

pub fn save_fds() -> SlashResult<(SmartFD,SmartFD,SmartFD)> {
	Ok((
		SmartFD::from_stdin()?,
		SmartFD::from_stdout()?,
		SmartFD::from_stderr()?
	))
}

pub fn restore_fds(mut stdio: (SmartFD,SmartFD,SmartFD), slash: &mut Slash) -> SlashResult<()> {
	let flags = slash.ctx().flags();
	if !flags.contains(ExecFlags::NO_RESET_IN) {
		stdio.0.dup2(&0)?;
		stdio.0.close()?;
	}
	if !flags.contains(ExecFlags::NO_RESET_OUT) {
		stdio.1.dup2(&1)?;
		stdio.1.close()?;
	}
	if !flags.contains(ExecFlags::NO_RESET_ERR) {
		stdio.2.dup2(&2)?;
		stdio.2.close()?;
	}
	Ok(())
}
