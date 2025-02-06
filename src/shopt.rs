use std::collections::{BTreeMap, VecDeque};

use crate::{error::{LashErr, LashErrLow}, shellenv::LashVal, LashResult};

#[derive(Clone, Debug)]
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
			max_recurse_depth: 500,
		};
		let prompt = ShOptsPrompt {
			trunc_prompt_path: 4,
			edit_mode: "vi".into(),
			comp_limit: 100,
			prompt_highlight: true,
			tab_stop: 8,
			exit_status: PromptStatus {
				success: " ".into(),
				failure: "✗".into(),
			},
			custom: PromptCustom {
				opts: LashVal::Dict(BTreeMap::new()),
			}
		};
		let exec = ShOptsExec {
			exec_opts: BTreeMap::new(),
		};
		Self { core, prompt, exec }
	}

	pub fn get<'a>(&self, query: &str) -> LashResult<LashVal> {
		let mut query = query.split('.').map(|seg| seg.to_string()).collect::<VecDeque<String>>();
		let key = query.pop_front().unwrap();
		match key.as_str() {
			"core" => Ok(self.core.get(query)?),
			"prompt" => Ok(self.prompt.get(query)?),
			"exec" => Ok(self.exec.get(query)?),
			_ => Err(LashErr::Low(LashErrLow::ExecFailed(format!("Invalid shopt key: {}",key))))
		}
	}
	pub fn set(&mut self, mut query: VecDeque<String>, value: LashVal) -> LashResult<()> {
		let key = query.pop_front().unwrap();
		match key.as_str() {
			"core" => self.core.set(query, value),
			"prompt" => self.prompt.set(query, value),
			"exec" => self.exec.set(query, value),
			_ => Err(LashErr::Low(LashErrLow::ExecFailed(format!("Invalid shopt key: {}", key))))
		}
	}
}

impl Default for ShOpts {
	fn default() -> Self {
		Self::new()
	}
}

#[derive(Clone, Debug)]
pub struct ShOptsCore {
	pub dotglob: bool,
	pub autocd: bool,
	pub hist_ignore_dupes: bool,
	pub max_hist: usize,
	pub int_comments: bool,
	pub auto_hist: bool,
	pub bell_style: usize,
	pub max_recurse_depth: usize,
}

impl ShOptsCore {
	pub fn get<'a>(&self, mut query: VecDeque<String>) -> LashResult<LashVal> {
		let key = query.pop_front().unwrap();
		match key.as_str() {
			"dotglob" => Ok(LashVal::Bool(self.dotglob)),
			"autocd" => Ok(LashVal::Bool(self.autocd)),
			"hist_ignore_dupes" => Ok(LashVal::Bool(self.hist_ignore_dupes)),
			"max_hist" => Ok(LashVal::Int(self.max_hist as i32)),
			"int_comments" => Ok(LashVal::Bool(self.int_comments)),
			"auto_hist" => Ok(LashVal::Bool(self.auto_hist)),
			"bell_style" => Ok(LashVal::Int(self.bell_style as i32)),
			"max_recurse_depth" => Ok(LashVal::Int(self.max_recurse_depth as i32)),
			_ => Err(LashErr::Low(LashErrLow::ExecFailed(format!("Invalid core opts key: {}",key))))
		}
	}
	pub fn set(&mut self, mut query: VecDeque<String>, value: LashVal) -> LashResult<()> {
		let key = query.pop_front().unwrap();
		match key.as_str() {
			"dotglob" => {
				self.dotglob = if let LashVal::Bool(val) = value { val } else {
					return Err(LashErr::Low(LashErrLow::ExecFailed(format!("Invalid value for core"))))
				};
			}
			"autocd" => {
				self.autocd = if let LashVal::Bool(val) = value { val } else {
					return Err(LashErr::Low(LashErrLow::ExecFailed(format!("Invalid value for core.autocd: {:?}", value))))
				};
			}
			"hist_ignore_dupes" => {
				self.hist_ignore_dupes = if let LashVal::Bool(val) = value { val } else {
					return Err(LashErr::Low(LashErrLow::ExecFailed(format!("Invalid value for core.hist_ignore_dupes: {:?}", value))))
				};
			}
			"max_hist" => {
				self.max_hist = if let LashVal::Int(val) = value { val as usize } else {
					return Err(LashErr::Low(LashErrLow::ExecFailed(format!("Invalid value for core.max_hist: {:?}", value))))
				};
			}
			"int_comments" => {
				self.int_comments = if let LashVal::Bool(val) = value { val } else {
					return Err(LashErr::Low(LashErrLow::ExecFailed(format!("Invalid value for core.int_comments: {:?}", value))))
				};
			}
			"auto_hist" => {
				self.auto_hist = if let LashVal::Bool(val) = value { val } else {
					return Err(LashErr::Low(LashErrLow::ExecFailed(format!("Invalid value for core.auto_hist: {:?}", value))))
				};
			}
			"bell_style" => {
				self.bell_style = if let LashVal::Int(val) = value { val as usize } else {
					return Err(LashErr::Low(LashErrLow::ExecFailed(format!("Invalid value for core.bell_style: {:?}", value))))
				};
			}
			"max_recurse_depth" => {
				self.max_recurse_depth = if let LashVal::Int(val) = value { val as usize } else {
					return Err(LashErr::Low(LashErrLow::ExecFailed(format!("Invalid value for core.max_recurse_depth: {:?}", value))))
				};
			}
			_ => {
				return Err(LashErr::Low(LashErrLow::ExecFailed(format!("Invalid core opts key: {}", key))))
			}
		}
		Ok(())
	}
}

#[derive(Clone, Debug)]
pub struct ShOptsPrompt {
	pub trunc_prompt_path: usize,
	pub edit_mode: String,
	pub comp_limit: usize,
	pub prompt_highlight: bool,
	pub tab_stop: usize,
	pub exit_status: PromptStatus, // Sub-group for exit status symbols
	pub custom: PromptCustom
}

impl ShOptsPrompt {
	pub fn get<'a>(&self, mut query: VecDeque<String>) -> LashResult<LashVal> {
		let key = query.pop_front().unwrap();
		match key.as_str() {
			"trunc_prompt_path" => Ok(LashVal::Int(self.trunc_prompt_path as i32)),
			"edit_mode" => Ok(LashVal::String(self.edit_mode.clone())),
			"comp_limit" => Ok(LashVal::Int(self.comp_limit as i32)),
			"prompt_highlight" => Ok(LashVal::Bool(self.prompt_highlight)),
			"tab_stop" => Ok(LashVal::Int(self.tab_stop as i32)),
			"exit_status" => Ok(self.exit_status.get(query)?),
			"custom" => Ok(self.custom.get(query)?),
			_ => Err(LashErr::Low(LashErrLow::ExecFailed(format!("Invalid key for prompt opts: {}",key))))
		}
	}

	pub fn set(&mut self, mut query: VecDeque<String>, value: LashVal) -> LashResult<()> {
		let key = query.pop_front().unwrap();
		match key.as_str() {
			"trunc_prompt_path" => {
				self.trunc_prompt_path = if let LashVal::Int(val) = value { val as usize } else {
					return Err(LashErr::Low(LashErrLow::ExecFailed(format!("Invalid value for core.trunc_prompt_path: {:?}", value))))
				};
			}
			"edit_mode" => {
				self.edit_mode = if let LashVal::String(val) = value { val } else {
					return Err(LashErr::Low(LashErrLow::ExecFailed(format!("Invalid value for core.edit_mode: {:?}", value))))
				};
			}
			"comp_limit" => {
				self.comp_limit = if let LashVal::Int(val) = value { val as usize } else {
					return Err(LashErr::Low(LashErrLow::ExecFailed(format!("Invalid value for core.comp_limit: {:?}", value))))
				};
			}
			"prompt_highlight" => {
				self.prompt_highlight = if let LashVal::Bool(val) = value { val } else {
					return Err(LashErr::Low(LashErrLow::ExecFailed(format!("Invalid value for core.prompt_highlight: {:?}", value))))
				};
			}
			"tab_stop" => {
				self.tab_stop = if let LashVal::Int(val) = value { val as usize } else {
					return Err(LashErr::Low(LashErrLow::ExecFailed(format!("Invalid value for core.tab_stop: {:?}", value))))
				};
			}
			"exit_status" => self.exit_status.set(query, value)?,
			"custom" => self.custom.set(query,value)?,
			_ => {
				return Err(LashErr::Low(LashErrLow::ExecFailed(format!("Invalid key for prompt opts: {}", key))))
			}
		}
		Ok(())
	}
}

#[derive(Clone, Debug)]
pub struct PromptCustom {
	opts: LashVal
}

impl PromptCustom {
	pub fn new() -> Self {
		Self { opts: LashVal::Dict(BTreeMap::new()) }
	}
	pub fn get<'a>(&self, mut query: VecDeque<String>) -> LashResult<LashVal> {
		// Start traversal at the root map
		if let LashVal::Dict(map) = &self.opts {
			let mut current_map = map.clone();

			while let Some(key) = query.pop_front() {
				match current_map.get(&key) {
					Some(LashVal::Dict(inner_map)) => {
						// If it's an object, descend into it
						current_map = inner_map.clone();
					}
					Some(value) => {
						// If it's a value, ensure it's the final key in the query
						if query.is_empty() {
							return Ok(value.clone());
						} else {
							return Err(LashErr::Low(LashErrLow::ExecFailed(format!("Key '{}' does not lead to an object, cannot continue traversal", key))));
						}
					}
					None => {
						// Key not found
						return Err(LashErr::Low(LashErrLow::ExecFailed(format!("Failed to find a value for key: {}", key))));
					}
				}
			}

			// If we reach here, the final key was an object, not a value
			Err(LashErr::Low(LashErrLow::ExecFailed("Expected a value but found a nested object".into())))
		} else { unreachable!() }
	}
	pub fn traverse_namespace(&mut self, map: &mut BTreeMap<String,LashVal>, mut query: VecDeque<String>, value: LashVal) -> LashResult<()> {
		if let Some(key) = query.pop_front() {
			if query.is_empty() {
				map.insert(key,value);
				return Ok(())
			}
			match map.get_mut(&key) {
				None => {
					let mut new_map = BTreeMap::new();
					self.traverse_namespace(&mut new_map, query, value)?;
					map.insert(key,LashVal::Dict(new_map));
				}
				Some(ref mut val) => {
					match val {
						LashVal::Dict(ref mut inner_map) => {
							self.traverse_namespace(inner_map, query, value)?;
						}
						_ => {
							return Err(LashErr::Low(LashErrLow::InternalErr(format!("Expected to find a map in prompt.custom, found this: {}", val.to_string()))))
						}
					}
				}
			}
		}
		Ok(())
	}
	pub fn insert_top_level(&mut self, key: String, val: LashVal) {
		if let LashVal::Dict(map) = &mut self.opts {
			map.insert(key,val);
		} else { unreachable!() }
	}

	pub fn set<'a>(&mut self, mut query: VecDeque<String>, value: LashVal) -> LashResult< ()> {
		if query.is_empty() {
			return Ok(());
		}
		if query.len() == 1 {
			let key = query.pop_front().unwrap();
			self.insert_top_level(key, value);
			return Ok(());
		}

		// Start with the first key and initialize the map stack
		let mut prev_key = query.pop_front().unwrap();
		let mut map_stack: Vec<(String, LashVal)> = vec![];
		let mut cur_map = LashVal::Dict(BTreeMap::new());

		// Build the map stack by iterating through the query
		while let Some(key) = query.pop_front() {
			if query.is_empty() {
				// If it's the last key, insert the value
				cur_map.try_insert(key, value.clone());
				// Push the final map onto the stack
				map_stack.push((prev_key.clone(), cur_map));
				break;
			} else {
				// Create a new map for the next level and push it to the stack
				let new_map = LashVal::Dict(BTreeMap::new());
				map_stack.push((key.clone(), new_map));
			}
			// Update the prev_key for the next iteration
			prev_key = key;
		}

		// Now we need to traverse the stack and insert each map into its parent map
		while let Some((key, mut map)) = map_stack.pop() {
			if let Some(LashVal::Dict(ref mut parent_map)) = self.opts.try_get_mut(&prev_key)? {
				parent_map.insert(key.clone(), map);
			} else {
				return Err(LashErr::Low(LashErrLow::InternalErr("Parent map not found".into())));
			}

			prev_key = key; // Update prev_key to the current key for the next insertion
		}

		Ok(())
	}
}

#[derive(Clone, Debug)]
pub struct PromptStatus {
	pub success: String,
	pub failure: String,
}

impl PromptStatus {
	pub fn get<'a>(&self, mut query: VecDeque<String>) -> LashResult<LashVal> {
		let key = query.pop_front().unwrap();
		match key.as_str() {
			"success" => Ok(LashVal::String(self.success.clone())),
			"failure" => Ok(LashVal::String(self.failure.clone())),
			_ => Err(LashErr::Low(LashErrLow::ExecFailed(format!("Invalid key for prompt exit status opts: {}",key))))
		}
	}

	pub fn set(&mut self, mut query: VecDeque<String>, value: LashVal) -> LashResult<()> {
		let key = query.pop_front().unwrap();
		match key.as_str() {
			"success" => {
				self.success = if let LashVal::String(val) = value { val } else {
					return Err(LashErr::Low(LashErrLow::ExecFailed(format!("Invalid value for core.success: {:?}", value))))
				};
			}
			"failure" => {
				self.failure = if let LashVal::String(val) = value { val } else {
					return Err(LashErr::Low(LashErrLow::ExecFailed(format!("Invalid value for core.failure: {:?}", value))))
				};
			}
			_ => {
				return Err(LashErr::Low(LashErrLow::ExecFailed(format!("Invalid key for prompt_status: {:?}", value))))
			}
		}
		Ok(())
	}
}

#[derive(Clone, Debug)]
pub struct ShOptsExec {
	pub exec_opts: BTreeMap<String, String>, // Keeping this dynamic for extensibility
}

impl ShOptsExec {
	pub fn get<'a>(&self, query: VecDeque<String>) -> LashResult<LashVal> {
		todo!()
	}
	pub fn set(&self, mut query: VecDeque<String>, value: LashVal) -> LashResult<()> {
		todo!()
	}
}
