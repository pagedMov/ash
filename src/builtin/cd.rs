use crate::prelude::*;

use crate::{helper::{self}, shellenv::Slash, SlashResult, pest_ext::Rule};

pub fn execute<'a>(cd_call: Pair<'a,Rule>, slash: &mut Slash) -> SlashResult<()> {
	let blame = cd_call.clone();
	let mut argv = helper::prepare_argv(cd_call,slash)?;
	argv.pop_front();
	let new_pwd;
	match argv.pop_front() {
		Some(arg) => {
			if arg.as_str() == "-" {
				new_pwd = slash.vars().get_evar("OLDPWD").unwrap_or("/".into());
			} else {
				new_pwd = arg.as_str().into();
			}
		}
		None => {
			new_pwd = env::var("HOME").unwrap_or("/".into());
		}
	}
	slash.vars_mut().export_var("OLDPWD", &env::var("PWD").unwrap_or_default());
	env::set_current_dir(new_pwd)?;
	slash.vars_mut().export_var("PWD", env::current_dir().unwrap().to_str().unwrap());
	Ok(())
}
