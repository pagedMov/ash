use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::{HashMap, VecDeque};

use crate::{event::ShError, interp::parse::Span, OxResult};

#[derive(Serialize, Clone, Deserialize, Debug)]
pub struct ShOpts {
	pub core: ShOptsCore,
	pub prompt: ShOptsPrompt,
	pub exec: ShOptsExec,
}

impl ShOpts {
	pub fn new() -> Self {
		let core = ShOptsCore {
			dotglob: true,
			autocd: true,
			hist_ignore_dupes: true,
			max_hist: 1000,
			int_comments: true,
			auto_hist: true,
			bell_style: 1,
		};
		let prompt = ShOptsPrompt {
			trunc_prompt_path: 4,
			edit_mode: "vi".into(),
			comp_limit: 100,
			prompt_highlight: false,
			tab_stop: 8,
			git_signs: PromptGitSigns {
				dirty_tree: "!".into(),
				clean_tree: "+".into(),
				untracked: "?".into(),
				ahead_of_remote: "⇡".into(),
				behind_remote: "⇣".into(),
				branch_icon: "".into()
			},
			exit_status: PromptStatus {
				success: " ".into(),
				failure: "✗".into(),
			}
		};
		let exec = ShOptsExec {
			exec_opts: HashMap::new(),
		};
		Self { core, prompt, exec }
	}

	pub fn get(&self, query: &str) -> OxResult<Value> {
		let mut query = query.split('.').map(|seg| seg.to_string()).collect::<VecDeque<String>>();
		let key = query.pop_front().unwrap();
		match key.as_str() {
			"core" => Ok(self.core.get(query)?),
			"prompt" => Ok(self.prompt.get(query)?),
			"exec" => Ok(self.exec.get(query)?),
			_ => Err(ShError::from_execf(format!("Invalid shopt key: {}",key).as_str(), 1, Span::new()))
		}
	}
	pub fn set(&mut self, mut query: VecDeque<String>, value: Value) -> OxResult<()> {
		let key = query.pop_front().unwrap();
		match key.as_str() {
			"core" => self.core.set(query, value),
			"prompt" => self.prompt.set(query, value),
			"exec" => self.exec.set(query, value),
			_ => Err(ShError::from_execf(
					format!("Invalid shopt key: {}", key).as_str(),
					1,
					Span::new(),
			)),
		}
	}
}

impl Default for ShOpts {
	fn default() -> Self {
		Self::new()
	}
}

#[derive(Serialize, Clone, Deserialize, Debug)]
pub struct ShOptsCore {
	pub dotglob: bool,
	pub autocd: bool,
	pub hist_ignore_dupes: bool,
	pub max_hist: usize,
	pub int_comments: bool,
	pub auto_hist: bool,
	pub bell_style: usize,
}

impl ShOptsCore {
	pub fn get(&self, mut query: VecDeque<String>) -> OxResult<Value> {
		let key = query.pop_front().unwrap();
		match key.as_str() {
			"dotglob" => Ok(Value::Bool(self.dotglob)),
			"autocd" => Ok(Value::Bool(self.autocd)),
			"hist_ignore_dupes" => Ok(Value::Bool(self.hist_ignore_dupes)),
			"max_hist" => Ok(Value::Number(self.max_hist.into())),
			"int_comments" => Ok(Value::Bool(self.int_comments)),
			"auto_hist" => Ok(Value::Bool(self.auto_hist)),
			"bell_style" => Ok(Value::Number(self.bell_style.into())),
			_ => Err(ShError::from_execf(format!("Invalid core opts key: {}",key).as_str(), 1, Span::new()))
		}
	}
	pub fn set(&mut self, mut query: VecDeque<String>, value: Value) -> OxResult<()> {
		let key = query.pop_front().unwrap();
		match key.as_str() {
			"dotglob" => {
				self.dotglob = value.as_bool().ok_or_else(|| {
					ShError::from_execf(
						format!("Invalid value for core.dotglob: {:?}", value).as_str(),
						1,
						Span::new(),
					)
				})?;
			}
			"autocd" => {
				self.autocd = value.as_bool().ok_or_else(|| {
					ShError::from_execf(
						format!("Invalid value for core.autocd: {:?}", value).as_str(),
						1,
						Span::new(),
					)
				})?;
			}
			"hist_ignore_dupes" => {
				self.hist_ignore_dupes = value.as_bool().ok_or_else(|| {
					ShError::from_execf(
						format!("Invalid value for core.hist_ignore_dupes: {:?}", value).as_str(),
						1,
						Span::new(),
					)
				})?;
			}
			"max_hist" => {
				self.max_hist = value.as_u64().ok_or_else(|| {
					ShError::from_execf(
						format!("Invalid value for core.max_hist: {:?}", value).as_str(),
						1,
						Span::new(),
					)
				})? as usize;
			}
			"int_comments" => {
				self.int_comments = value.as_bool().ok_or_else(|| {
					ShError::from_execf(
						format!("Invalid value for core.int_comments: {:?}", value).as_str(),
						1,
						Span::new(),
					)
				})?;
			}
			"auto_hist" => {
				self.auto_hist = value.as_bool().ok_or_else(|| {
					ShError::from_execf(
						format!("Invalid value for core.auto_hist: {:?}", value).as_str(),
						1,
						Span::new(),
					)
				})?;
			}
			"bell_style" => {
				self.bell_style = value.as_u64().ok_or_else(|| {
					ShError::from_execf(
						format!("Invalid value for core.bell_style: {:?}", value).as_str(),
						1,
						Span::new(),
					)
				})? as usize;
			}
			_ => {
				return Err(ShError::from_execf(
						format!("Invalid core opts key: {}", key).as_str(),
						1,
						Span::new(),
				))
			}
		}
		Ok(())
	}
}

#[derive(Serialize, Clone, Deserialize, Debug)]
pub struct ShOptsPrompt {
	pub trunc_prompt_path: usize,
	pub edit_mode: String,
	pub comp_limit: usize,
	pub prompt_highlight: bool,
	pub tab_stop: usize,
	pub git_signs: PromptGitSigns, // Sub-group for Git-specific options
	pub exit_status: PromptStatus, // Sub-group for exit status symbols
}

impl ShOptsPrompt {
	pub fn get(&self, mut query: VecDeque<String>) -> OxResult<Value> {
		let key = query.pop_front().unwrap();
		match key.as_str() {
			"trunc_prompt_path" => Ok(Value::Number(self.trunc_prompt_path.into())),
			"edit_mode" => Ok(Value::String(self.edit_mode.clone())),
			"comp_limit" => Ok(Value::Number(self.comp_limit.into())),
			"prompt_highlight" => Ok(Value::Bool(self.prompt_highlight)),
			"tab_stop" => Ok(Value::Number(self.tab_stop.into())),
			"git_signs" => Ok(self.git_signs.get(query)?),
			"exit_status" => Ok(self.exit_status.get(query)?),
			_ => Err(ShError::from_execf(format!("Invalid key for prompt opts: {}",key).as_str(), 1, Span::new()))
		}
	}

	pub fn set(&mut self, mut query: VecDeque<String>, value: Value) -> OxResult<()> {
		let key = query.pop_front().unwrap();
		match key.as_str() {
			"trunc_prompt_path" => {
				self.trunc_prompt_path = value.as_u64().ok_or_else(|| {
					ShError::from_execf(
						format!("Invalid value for prompt.trunc_prompt_path: {:?}", value).as_str(),
						1,
						Span::new(),
					)
				})? as usize;
			}
			"edit_mode" => {
				self.edit_mode = value.as_str().ok_or_else(|| {
					ShError::from_execf(
						format!("Invalid value for prompt.edit_mode: {:?}", value).as_str(),
						1,
						Span::new(),
					)
				})?
				.to_string();
					}
			"comp_limit" => {
				self.comp_limit = value.as_u64().ok_or_else(|| {
					ShError::from_execf(
						format!("Invalid value for prompt.comp_limit: {:?}", value).as_str(),
						1,
						Span::new(),
					)
				})? as usize;
			}
			"prompt_highlight" => {
				self.prompt_highlight = value.as_bool().ok_or_else(|| {
					ShError::from_execf(
						format!("Invalid value for prompt.prompt_highlight: {:?}", value).as_str(),
						1,
						Span::new(),
					)
				})?;
			}
			"tab_stop" => {
				self.tab_stop = value.as_u64().ok_or_else(|| {
					ShError::from_execf(
						format!("Invalid value for prompt.tab_stop: {:?}", value).as_str(),
						1,
						Span::new(),
					)
				})? as usize;
			}
			"git_signs" => self.git_signs.set(query, value)?,
			"exit_status" => self.exit_status.set(query, value)?,
			_ => {
				return Err(ShError::from_execf(
						format!("Invalid key for prompt opts: {}", key).as_str(),
						1,
						Span::new(),
				))
			}
		}
		Ok(())
	}
}

#[derive(Serialize, Clone, Deserialize, Debug)]
pub struct PromptStatus {
	pub success: String,
	pub failure: String,
}

impl PromptStatus {
	pub fn get(&self, mut query: VecDeque<String>) -> OxResult<Value> {
		let key = query.pop_front().unwrap();
		match key.as_str() {
			"success" => Ok(Value::String(self.success.clone())),
			"failure" => Ok(Value::String(self.failure.clone())),
			_ => Err(ShError::from_execf(format!("Invalid key for prompt exit status opts: {}",key).as_str(), 1, Span::new()))
		}
	}

	pub fn set(&mut self, mut query: VecDeque<String>, value: Value) -> OxResult<()> {
		let key = query.pop_front().unwrap();
		match key.as_str() {
			"success" => {
				self.success = value.as_str().ok_or_else(|| {
					ShError::from_execf(
						format!("Invalid value for prompt_status.success: {:?}", value).as_str(),
						1,
						Span::new(),
					)
				})?
				.to_string();
					}
			"failure" => {
				self.failure = value.as_str().ok_or_else(|| {
					ShError::from_execf(
						format!("Invalid value for prompt_status.failure: {:?}", value).as_str(),
						1,
						Span::new(),
					)
				})?
				.to_string();
					}
			_ => {
				return Err(ShError::from_execf(
						format!("Invalid key for prompt_status: {}", key).as_str(),
						1,
						Span::new(),
				))
			}
		}
		Ok(())
	}
}

#[derive(Serialize, Clone, Deserialize, Debug)]
pub struct PromptGitSigns {
	pub dirty_tree: String,
	pub clean_tree: String,
	pub untracked: String,
	pub ahead_of_remote: String,
	pub behind_remote: String,
	pub branch_icon: String
}

impl PromptGitSigns {
	pub fn get(&self, mut query: VecDeque<String>) -> OxResult<Value> {
		let key = query.pop_front().unwrap();
		match key.as_str() {
			"dirty_tree" => Ok(Value::String(self.dirty_tree.clone())),
			"clean_tree" => Ok(Value::String(self.clean_tree.clone())),
			"untracked" => Ok(Value::String(self.untracked.clone())),
			"ahead_of_remote" => Ok(Value::String(self.ahead_of_remote.clone())),
			"behind_remote" => Ok(Value::String(self.behind_remote.clone())),
			"branch_icon" => Ok(Value::String(self.branch_icon.clone())),
			_ => Err(ShError::from_execf(format!("Invalid key for prompt git sign opts: {}",key).as_str(), 1, Span::new()))
		}
	}

	pub fn set(&mut self, mut query: VecDeque<String>, value: Value) -> OxResult<()> {
		let key = query.pop_front().unwrap();
		match key.as_str() {
			"dirty_tree" => {
				self.dirty_tree = value.as_str().ok_or_else(|| {
					ShError::from_execf(
						format!("Invalid value for git_signs.dirty_tree: {:?}", value).as_str(),
						1,
						Span::new(),
					)
				})?
				.to_string();
					}
			"clean_tree" => {
				self.clean_tree = value.as_str().ok_or_else(|| {
					ShError::from_execf(
						format!("Invalid value for git_signs.clean_tree: {:?}", value).as_str(),
						1,
						Span::new(),
					)
				})?
				.to_string();
					}
			"untracked" => {
				self.untracked = value.as_str().ok_or_else(|| {
					ShError::from_execf(
						format!("Invalid value for git_signs.untracked: {:?}", value).as_str(),
						1,
						Span::new(),
					)
				})?
				.to_string();
					}
			"ahead_of_remote" => {
				self.ahead_of_remote = value.as_str().ok_or_else(|| {
					ShError::from_execf(
						format!("Invalid value for git_signs.ahead_of_remote: {:?}", value).as_str(),
						1,
						Span::new(),
					)
				})?
				.to_string();
					}
			"behind_remote" => {
				self.behind_remote = value.as_str().ok_or_else(|| {
					ShError::from_execf(
						format!("Invalid value for git_signs.behind_remote: {:?}", value).as_str(),
						1,
						Span::new(),
					)
				})?
				.to_string();
					}
			"branch_icon" => {
				self.branch_icon = value.as_str().ok_or_else(|| {
					ShError::from_execf(
						format!("Invalid value for git_signs.behind_remote: {:?}", value).as_str(),
						1,
						Span::new(),
					)
				})?
				.to_string();
					}
			_ => {
				return Err(ShError::from_execf(
						format!("Invalid key for git_signs: {}", key).as_str(),
						1,
						Span::new(),
				))
			}
		}
		Ok(())
	}
}

#[derive(Serialize, Clone, Deserialize, Debug)]
pub struct ShOptsExec {
	pub exec_opts: HashMap<String, String>, // Keeping this dynamic for extensibility
}

impl ShOptsExec {
	pub fn get(&self, query: VecDeque<String>) -> OxResult<Value> {
		todo!()
	}
	pub fn set(&self, mut query: VecDeque<String>, value: Value) -> OxResult<()> {
		todo!()
	}
}
