use std::collections::{BTreeMap, VecDeque};

use crate::{event::ShError, interp::parse::Span, shellenv::AshVal, AshResult};


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
				opts: AshVal::Dict(BTreeMap::new()),
			}
		};
		let exec = ShOptsExec {
			exec_opts: BTreeMap::new(),
		};
		Self { core, prompt, exec }
	}

	pub fn get(&self, query: &str) -> AshResult<AshVal> {
		let mut query = query.split('.').map(|seg| seg.to_string()).collect::<VecDeque<String>>();
		let key = query.pop_front().unwrap();
		match key.as_str() {
			"core" => Ok(self.core.get(query)?),
			"prompt" => Ok(self.prompt.get(query)?),
			"exec" => Ok(self.exec.get(query)?),
			_ => Err(ShError::from_execf(format!("Invalid shopt key: {}",key).as_str(), 1, Span::new()))
		}
	}
	pub fn set(&mut self, mut query: VecDeque<String>, value: AshVal) -> AshResult<()> {
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

#[derive(Clone, Debug)]
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
	pub fn get(&self, mut query: VecDeque<String>) -> AshResult<AshVal> {
		let key = query.pop_front().unwrap();
		match key.as_str() {
			"dotglob" => Ok(AshVal::Bool(self.dotglob)),
			"autocd" => Ok(AshVal::Bool(self.autocd)),
			"hist_ignore_dupes" => Ok(AshVal::Bool(self.hist_ignore_dupes)),
			"max_hist" => Ok(AshVal::Int(self.max_hist as i32)),
			"int_comments" => Ok(AshVal::Bool(self.int_comments)),
			"auto_hist" => Ok(AshVal::Bool(self.auto_hist)),
			"bell_style" => Ok(AshVal::Int(self.bell_style as i32)),
			_ => Err(ShError::from_execf(format!("Invalid core opts key: {}",key).as_str(), 1, Span::new()))
		}
	}
	pub fn set(&mut self, mut query: VecDeque<String>, value: AshVal) -> AshResult<()> {
		let key = query.pop_front().unwrap();
		match key.as_str() {
			"dotglob" => {
				self.dotglob = if let AshVal::Bool(val) = value { val } else {
					return Err(ShError::from_execf(
						format!("Invalid value for core.dotglob: {:?}", value).as_str(),
						1,
						Span::new(),
					))
				};
			}
			"autocd" => {
				self.autocd = if let AshVal::Bool(val) = value { val } else {
					return Err(ShError::from_execf(
						format!("Invalid value for core.autocd: {:?}", value).as_str(),
						1,
						Span::new(),
					))
				};
			}
			"hist_ignore_dupes" => {
				self.hist_ignore_dupes = if let AshVal::Bool(val) = value { val } else {
					return Err(ShError::from_execf(
						format!("Invalid value for core.hist_ignore_dupes: {:?}", value).as_str(),
						1,
						Span::new(),
					))
				};
			}
			"max_hist" => {
				self.max_hist = if let AshVal::Int(val) = value { val as usize } else {
					return Err(ShError::from_execf(
						format!("Invalid value for core.max_hist: {:?}", value).as_str(),
						1,
						Span::new(),
					))
				};
			}
			"int_comments" => {
				self.int_comments = if let AshVal::Bool(val) = value { val } else {
					return Err(ShError::from_execf(
						format!("Invalid value for core.int_comments: {:?}", value).as_str(),
						1,
						Span::new(),
					))
				};
			}
			"auto_hist" => {
				self.auto_hist = if let AshVal::Bool(val) = value { val } else {
					return Err(ShError::from_execf(
						format!("Invalid value for core.auto_hist: {:?}", value).as_str(),
						1,
						Span::new(),
					))
				};
			}
			"bell_style" => {
				self.bell_style = if let AshVal::Int(val) = value { val as usize } else {
					return Err(ShError::from_execf(
						format!("Invalid value for core.bell_style: {:?}", value).as_str(),
						1,
						Span::new(),
					))
				};
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
	pub fn get(&self, mut query: VecDeque<String>) -> AshResult<AshVal> {
		let key = query.pop_front().unwrap();
		match key.as_str() {
			"trunc_prompt_path" => Ok(AshVal::Int(self.trunc_prompt_path as i32)),
			"edit_mode" => Ok(AshVal::String(self.edit_mode.clone())),
			"comp_limit" => Ok(AshVal::Int(self.comp_limit as i32)),
			"prompt_highlight" => Ok(AshVal::Bool(self.prompt_highlight)),
			"tab_stop" => Ok(AshVal::Int(self.tab_stop as i32)),
			"exit_status" => Ok(self.exit_status.get(query)?),
			"custom" => Ok(self.custom.get(query)?),
			_ => Err(ShError::from_execf(format!("Invalid key for prompt opts: {}",key).as_str(), 1, Span::new()))
		}
	}

	pub fn set(&mut self, mut query: VecDeque<String>, value: AshVal) -> AshResult<()> {
		let key = query.pop_front().unwrap();
		match key.as_str() {
			"trunc_prompt_path" => {
				self.trunc_prompt_path = if let AshVal::Int(val) = value { val as usize } else {
					return Err(ShError::from_execf(
						format!("Invalid value for core.trunc_prompt_path: {:?}", value).as_str(),
						1,
						Span::new(),
					))
				};
			}
			"edit_mode" => {
				self.edit_mode = if let AshVal::String(val) = value { val } else {
					return Err(ShError::from_execf(
						format!("Invalid value for core.edit_mode: {:?}", value).as_str(),
						1,
						Span::new(),
					))
				};
			}
			"comp_limit" => {
				self.comp_limit = if let AshVal::Int(val) = value { val as usize } else {
					return Err(ShError::from_execf(
						format!("Invalid value for core.comp_limit: {:?}", value).as_str(),
						1,
						Span::new(),
					))
				};
			}
			"prompt_highlight" => {
				self.prompt_highlight = if let AshVal::Bool(val) = value { val } else {
					return Err(ShError::from_execf(
						format!("Invalid value for core.prompt_highlight: {:?}", value).as_str(),
						1,
						Span::new(),
					))
				};
			}
			"tab_stop" => {
				self.tab_stop = if let AshVal::Int(val) = value { val as usize } else {
					return Err(ShError::from_execf(
						format!("Invalid value for core.tab_stop: {:?}", value).as_str(),
						1,
						Span::new(),
					))
				};
			}
			"exit_status" => self.exit_status.set(query, value)?,
			"custom" => self.custom.set(query,value)?,
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

#[derive(Clone, Debug)]
pub struct PromptCustom {
	opts: AshVal
}

impl PromptCustom {
	pub fn new() -> Self {
		Self { opts: AshVal::Dict(BTreeMap::new()) }
	}
	pub fn get(&self, mut query: VecDeque<String>) -> AshResult<AshVal> {
		// Start traversal at the root map
		if let AshVal::Dict(map) = &self.opts {
			let mut current_map = map.clone();

			while let Some(key) = query.pop_front() {
				match current_map.get(&key) {
					Some(AshVal::Dict(inner_map)) => {
						// If it's an object, descend into it
						current_map = inner_map.clone();
					}
					Some(value) => {
						// If it's a value, ensure it's the final key in the query
						if query.is_empty() {
							return Ok(value.clone());
						} else {
							return Err(ShError::from_execf(
									format!(
										"Key '{}' does not lead to an object, cannot continue traversal",
										key
									)
									.as_str(),
									1,
									Span::new(),
							));
						}
					}
					None => {
						// Key not found
						return Err(ShError::from_execf(
								format!("Failed to find a value for key: {}", key).as_str(),
								1,
								Span::new(),
						));
					}
				}
			}

			// If we reach here, the final key was an object, not a value
			Err(ShError::from_execf(
					"Expected a value but found a nested object",
					1,
					Span::new(),
			))
		} else { unreachable!() }
	}
	pub fn traverse_namespace(&mut self, map: &mut BTreeMap<String,AshVal>, mut query: VecDeque<String>, value: AshVal) -> AshResult<()> {
		if let Some(key) = query.pop_front() {
			if query.is_empty() {
				map.insert(key,value);
				return Ok(())
			}
			match map.get_mut(&key) {
				None => {
					let mut new_map = BTreeMap::new();
					self.traverse_namespace(&mut new_map, query, value)?;
					map.insert(key,AshVal::Dict(new_map));
				}
				Some(ref mut val) => {
					match val {
						AshVal::Dict(ref mut inner_map) => {
							self.traverse_namespace(inner_map, query, value)?;
						}
						_ => {
							return Err(ShError::from_internal(format!("Expected to find a map in prompt.custom, found this: {}", val.to_string()).as_str()))
						}
					}
				}
			}
		}
		Ok(())
	}
	pub fn insert_top_level(&mut self, key: String, val: AshVal) {
		if let AshVal::Dict(map) = &mut self.opts {
			map.insert(key,val);
		} else { unreachable!() }
	}
	pub fn set(&mut self, mut query: VecDeque<String>, value: AshVal) -> AshResult<()> {
		if query.len() == 1 {
			let key = query.pop_front().unwrap();
			self.insert_top_level(key, value);
			return Ok(());
		}

		let mut current_map = &mut self.opts;

		while let Some(key) = query.pop_front() {
			// Remove the current map to allow mutation
			let existing_value = current_map.try_remove(&key)?;

			match existing_value {
				Some(AshVal::Dict(mut map)) => {
					// Navigate into the existing map
					if query.is_empty() {
						map.insert(key.clone(), value.clone());
					}
					// Re-insert the modified map
					current_map.try_insert(key.clone(), AshVal::Dict(map))?;
					let new_map = current_map.try_get_mut(&key)?.unwrap();
					current_map = match new_map {
						AshVal::Dict(_) => new_map,
						_ => break
					};
				}
				Some(_) => {
					current_map.try_insert(key.clone(), value.clone())?;
				}
				None => {
					// If the key doesn't exist, create a new map for the remaining keys
					let new_map = AshVal::Dict(BTreeMap::new());
					if query.is_empty() {
						current_map.try_insert(key.clone(), value.clone())?; // Placeholder object
					} else {
						current_map.try_insert(key.clone(), new_map)?; // Placeholder object
					}
					let new_map = current_map.try_get_mut(&key)?.unwrap();
					current_map = match new_map {
						AshVal::Dict(_) => new_map,
						_ => break
					};
				}
			}
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
	pub fn get(&self, mut query: VecDeque<String>) -> AshResult<AshVal> {
		let key = query.pop_front().unwrap();
		match key.as_str() {
			"success" => Ok(AshVal::String(self.success.clone())),
			"failure" => Ok(AshVal::String(self.failure.clone())),
			_ => Err(ShError::from_execf(format!("Invalid key for prompt exit status opts: {}",key).as_str(), 1, Span::new()))
		}
	}

	pub fn set(&mut self, mut query: VecDeque<String>, value: AshVal) -> AshResult<()> {
		let key = query.pop_front().unwrap();
		match key.as_str() {
			"success" => {
				self.success = if let AshVal::String(val) = value { val } else {
					return Err(ShError::from_execf(
						format!("Invalid value for core.success: {:?}", value).as_str(),
						1,
						Span::new(),
					))
				};
			}
			"failure" => {
				self.failure = if let AshVal::String(val) = value { val } else {
					return Err(ShError::from_execf(
						format!("Invalid value for core.failure: {:?}", value).as_str(),
						1,
						Span::new(),
					))
				};
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

#[derive(Clone, Debug)]
pub struct ShOptsExec {
	pub exec_opts: BTreeMap<String, String>, // Keeping this dynamic for extensibility
}

impl ShOptsExec {
	pub fn get(&self, query: VecDeque<String>) -> AshResult<AshVal> {
		todo!()
	}
	pub fn set(&self, mut query: VecDeque<String>, value: AshVal) -> AshResult<()> {
		todo!()
	}
}
