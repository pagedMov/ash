pub use std::{
	collections::{
		HashMap,
		HashSet,
		VecDeque
	},
	env,
	ffi::{
		c_void,
		CString
	},
	fmt::{
		self,
		Display
	},
	io::{
		self,
		Read,
		Write
	},
	mem::take,
	os::fd::{
		AsFd,
		AsRawFd,
		BorrowedFd,
		FromRawFd,
		IntoRawFd,
		RawFd
	}, path::{
		Path,
		PathBuf
	}, sync::{
		Arc,
		Mutex,
		MutexGuard
	}
};

pub use libc::{
	iovec,
	mode_t,
	STDIN_FILENO,
	STDOUT_FILENO,
	STDERR_FILENO,
	MFD_CLOEXEC,
	O_APPEND,
	O_CREAT,
	O_RDONLY,
	O_RDWR,
	O_TRUNC,
	O_WRONLY
};
pub use nix::{
	errno::Errno,
	fcntl::{fcntl,
	open,
	FcntlArg::F_GETFD,
	OFlag
	}, sys::{
		memfd::{memfd_create,
		MemFdCreateFlag
		},
		signal::Signal, stat::{
			fstat,
			Mode
		}, wait::WaitStatus
	}, unistd::{
		setpgid,
		isatty,
		close,
		dup,
		dup2,
		execve,
		execvpe,
		fork,
		pipe,
		ForkResult,
		Pid
	}
};
pub use pest::{
	iterators::Pair,
	Parser,
	Span
};
pub use bitflags::bitflags;
pub use crate::{
	pest_ext::{
		OptPairExt,
		PairExt,
		Rules,
		Rule,
		LashParse,
	},
	shellenv::Lash,
	helper::{
		StrExtension,
		StringExt,
		VecExtension,
		VecDequeExtension,
	},
	error::{
		LashResult,
		LashErr::*,
		LashErrLow,
		LashErrHigh
	},
};
