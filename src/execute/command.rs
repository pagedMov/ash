use crate::{helper, prelude::*};

use crate::utils;

pub fn exec_cmd<'a>(cmd: Pair<Rule>, lash: &mut Lash) -> LashResult<()> {
	let blame = cmd.clone();
	let mut argv = helper::prepare_argv(cmd.clone(),lash)?;
	let mut redirs = helper::prepare_redirs(cmd)?;
	lash.ctx_mut().extend_redirs(redirs);
	argv.retain(|arg| !arg.is_empty() && arg != "\"\"" && arg != "''");

	if helper::validate_autocd(lash,&argv)? {
		let arg = argv.pop_front().unwrap();
		let dir = PathBuf::from(&arg);
		return lash.change_dir(&dir)
	}

	let argv = argv.into_iter().map(|arg| CString::new(arg).unwrap()).collect::<Vec<_>>();


	let command = argv.first().unwrap().clone();


	if utils::SHELL_CMDS.contains(&command.to_str().unwrap()) {
		return Err(High(LashErrHigh::exec_err(format!("This shell command appears malformed"), blame)))
	}

	let env_vars = env::vars().into_iter().collect::<Vec<(String,String)>>();
	let envp = env_vars.iter().map(|var| CString::new(format!("{}={}",var.0,var.1)).unwrap()).collect::<Vec<_>>();

	lash.ctx_mut().activate_redirs()?;

	if lash.ctx_mut().flags().contains(utils::ExecFlags::NO_FORK) {
		utils::exec_external(command, argv, envp, blame);
	}

	match unsafe { fork() } {
		Ok(ForkResult::Child) => {
			utils::exec_external(command, argv, envp, blame);
		}
		Ok(ForkResult::Parent { child }) => {
			utils::handle_parent_process(child, command.to_str().unwrap().to_string(),lash)?;
		}
		Err(_) => todo!()
	}

	Ok(())
}
