use std::ffi::OsStr;
use std::fs::{read_dir, File, ReadDir};
use std::path::{Path, PathBuf};

type Result<T> = std::result::Result<T, std::io::Error>;

pub struct FileWalker {
	stack: Vec<ReadDir>,
}

impl FileWalker {
	pub fn new<P: AsRef<Path>>(path: P) -> Result<FileWalker> {
		Ok(FileWalker {
			stack: vec![read_dir(path)?],
		})
	}

	pub fn next_file(&mut self) -> Result<Option<(File, PathBuf)>> {
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
				} else if file_type.is_file() && path.extension() == Some(OsStr::new("poetry")) {
					return Ok(Some((File::open(&path)?, path)));
				}
			} else {
				self.stack.pop();
			}
		}
	}
}
