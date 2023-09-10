use std::ffi::OsStr;
use std::fs::{read_dir, File, ReadDir};
use std::io::Read;
use std::path::{Path, PathBuf};

pub type Result<T> = std::result::Result<T, std::io::Error>;

pub struct SourceFile {
	pub source: String,
	pub path: PathBuf,
	pub module_path: Vec<String>,
	pub index: usize,
}

impl std::fmt::Debug for SourceFile {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("SourceFile")
			.field("source", &"...")
			.field("path", &self.path)
			.field("module_path", &self.module_path)
			.field("index", &self.index)
			.finish()
	}
}

pub fn load_all_files(path: &Path) -> Result<Vec<SourceFile>> {
	let mut walker = FileWalker::new(path)?;
	let mut files = Vec::new();

	loop {
		let walked_file = match walker.next_file() {
			Ok(Some(file)) => file,
			Ok(None) => break,
			Err(_) => break, //TODO: Report this error
		};

		let mut file = walked_file.file;
		let path = walked_file.path;
		let module_path = walked_file.module_path;

		let capacity = file.metadata().map(|m| m.len()).unwrap_or(0);
		let mut source = String::with_capacity(capacity as usize);
		file.read_to_string(&mut source)?;

		let index = files.len();
		files.push(SourceFile { source, path, module_path, index });
	}

	Ok(files)
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
					let full_name = entry.file_name().to_string_lossy().to_string();
					let file_name = full_name[..full_name.len() - extension_len].to_owned();
					module_path.push(file_name);

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
