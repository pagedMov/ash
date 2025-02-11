use once_cell::sync::Lazy;
use regex::Regex;

use crate::{helper, prelude::*, shellenv::{ChildProc, JobBuilder}};

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
		const NO_FORK    = 0b00000000000000000000000000000001;
		const BACKGROUND = 0b00000000000000000000000000000010;
		const IN_PIPE    = 0b00000000000000000000000000000100;
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
	pub fn redir_type(&self) -> Rule {
		self.redir_type
	}
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

pub fn exec_external(command: CString, argv: Vec<CString>, envp: Vec<CString>,blame: Pair<Rule>) -> ! {
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

pub fn handle_parent_process<'a>(child: Pid, command: String) -> LashResult<()> {
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

pub fn save_fds() -> LashResult<(RustFd,RustFd,RustFd)> {
	Ok((
		RustFd::from_stdin()?,
		RustFd::from_stdout()?,
		RustFd::from_stderr()?
	))
}

pub fn restore_fds(mut stdio: (RustFd,RustFd,RustFd)) -> LashResult<()> {
	stdio.0.dup2(&0)?;
	stdio.0.close()?;
	stdio.1.dup2(&1)?;
	stdio.1.close()?;
	stdio.2.dup2(&2)?;
	stdio.2.close()?;
	Ok(())
}
