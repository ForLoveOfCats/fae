use std::ffi::OsStr;
use std::fs::{read_dir, File, ReadDir};
use std::io::Read;
use std::path::{Path, PathBuf};

pub type Result<T> = std::result::Result<T, std::io::Error>;

pub struct SourceFile {
	pub source: String,
	pub path: PathBuf,
}

pub fn load_all_files<P: AsRef<Path>>(path: P) -> Result<Vec<SourceFile>> {
	let mut walker = FileWalker::new(path)?;
	let mut files = Vec::new();

	loop {
		let (mut file, path) = match walker.next_file() {
			Ok(Some((file, path))) => (file, path),
			Ok(None) => break,
			Err(_) => break, //TODO: Report this error
		};

		let capacity = file.metadata().map(|m| m.len()).unwrap_or(0);
		let mut source = String::with_capacity(capacity as usize);
		if file.read_to_string(&mut source).is_err() {
			break;
		}

		files.push(SourceFile { source, path });
	}

	Ok(files)
}

struct FileWalker {
	stack: Vec<ReadDir>,
}

impl FileWalker {
	fn new<P: AsRef<Path>>(path: P) -> Result<FileWalker> {
		Ok(FileWalker { stack: vec![read_dir(path)?] })
	}

	fn next_file(&mut self) -> Result<Option<(File, PathBuf)>> {
		loop {
			let reader = match self.stack.last_mut() {
				Some(reader) => reader,
				None => return Ok(None),
			};

			if let Some(entry) = reader.next() {
				let entry = entry?;

				let file_type = entry.file_type()?;
				let path = entry.path();

				if file_type.is_dir() {
					self.stack.push(read_dir(path)?);
				} else if file_type.is_file() && path.extension() == Some(OsStr::new("fae")) {
					return Ok(Some((File::open(&path)?, path)));
				}
			} else {
				self.stack.pop();
			}
		}
	}
}
