use std::ffi::OsStr;
use std::fs::{read_dir, File, ReadDir};
use std::io::Read;
use std::path::{Path, PathBuf};

use crate::frontend::error::WriteFmt;

pub type Result<T> = std::result::Result<T, std::io::Error>;

pub struct SourceFile {
	pub source: String,
	pub path: PathBuf,
	pub module_path: Vec<String>,
	pub index: u32,
}

impl std::fmt::Debug for SourceFile {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		f.debug_struct("SourceFile")
			.field("source", &"...")
			.field("path", &self.path)
			.field("module_path", &self.module_path)
			.field("index", &self.index)
			.finish()
	}
}

pub fn load_single_file(path: PathBuf, files: &mut Vec<SourceFile>) -> Result<String> {
	let mut file = std::fs::File::open(&path)?;
	let capacity = file.metadata().map(|m| m.len()).unwrap_or(0);
	let mut source = String::with_capacity(capacity as usize);
	file.read_to_string(&mut source)?;

	let Some(stem) = path.file_stem() else {
		usage_error!("Input file has no name");
	};

	let extension_len = ".fae".len();
	let full_name = stem.to_string_lossy();
	let file_name = full_name[..full_name.len() - extension_len].to_owned();

	let module_path = vec![file_name.clone()];
	let index = files.len() as u32;
	files.push(SourceFile { source, path, module_path, index });

	Ok(file_name)
}

pub fn load_all_files(error_output: &mut impl WriteFmt, path: &Path, files: &mut Vec<SourceFile>) -> Result<bool> {
	struct DisallowedModulePath {
		path: PathBuf,
		module_path: Vec<String>,
		disallowed_indicies: Vec<usize>,
	}

	let mut walker = FileWalker::new(path)?;
	let mut disallowed = Vec::new();

	loop {
		let walked_file = match walker.next_file() {
			Ok(Some(file)) => file,
			Ok(None) => break,
			Err(_) => break, //TODO: Report this error
		};

		let mut file = walked_file.file;
		let path = walked_file.path;
		let module_path = walked_file.module_path;

		let mut disallowed_indicies = Vec::new();
		for (index, part) in module_path.iter().enumerate() {
			if crate::frontend::parser::is_word_reserved(part) {
				disallowed_indicies.push(index);
			}
		}

		if !disallowed_indicies.is_empty() {
			let path = path.clone();
			let module_path = module_path.clone();
			disallowed.push(DisallowedModulePath { path, module_path, disallowed_indicies });
		}

		let capacity = file.metadata().map(|m| m.len()).unwrap_or(0);
		let mut source = String::with_capacity(capacity as usize);
		file.read_to_string(&mut source)?;

		let index = files.len() as u32;
		files.push(SourceFile { source, path, module_path, index });
	}

	let errored = !disallowed.is_empty();
	disallowed.sort_by(|a, b| a.module_path.cmp(&b.module_path));
	for disallowed in disallowed {
		for index in disallowed.disallowed_indicies {
			let reserved = &disallowed.module_path[index];
			let module_path = disallowed.module_path.join("::");
			let path = &disallowed.path;
			let error = error!("Reserved word `{reserved}` may not be part of module path `{module_path}` for file {path:?}");
			error.print(error_output, &[], "Project");
		}
	}

	Ok(errored)
}

#[derive(Debug)]
struct Directory {
	name: String,
	reader: ReadDir,
}

struct WalkedFile {
	file: File,
	path: PathBuf,
	module_path: Vec<String>,
}

struct FileWalker {
	stack: Vec<Directory>,
}

impl FileWalker {
	fn new(path: &Path) -> Result<FileWalker> {
		let reader = read_dir(path)?;
		let directory = Directory { name: String::new(), reader };
		Ok(FileWalker { stack: vec![directory] })
	}

	fn next_file(&mut self) -> Result<Option<WalkedFile>> {
		loop {
			let directory = match self.stack.last_mut() {
				Some(directory) => directory,
				None => return Ok(None),
			};

			if let Some(entry) = directory.reader.next() {
				let entry = entry?;

				let file_type = entry.file_type()?;
				let path = entry.path();
				let extension = path.extension();

				if file_type.is_dir() {
					if let Some(name) = path.file_name() {
						let name = name.to_string_lossy().to_string();
						let reader = read_dir(path)?;
						let directory = Directory { reader, name };
						self.stack.push(directory);
					}
				} else if file_type.is_file() && extension == Some(OsStr::new("fae")) {
					let mut module_path = Vec::new();
					for item in &self.stack {
						if !item.name.is_empty() {
							module_path.push(item.name.to_owned());
						}
					}

					let extension_len = ".fae".len();
					let entry_file_name = entry.file_name();
					let full_name = entry_file_name.to_string_lossy();
					let file_name = full_name[..full_name.len() - extension_len].to_owned();
					if module_path.last().as_deref() != Some(&file_name) {
						module_path.push(file_name);
					}

					let file = File::open(&path)?;
					let walked_file = WalkedFile { file, path, module_path };
					return Ok(Some(walked_file));
				}
			} else {
				self.stack.pop();
			}
		}
	}
}
