use std::fs::{read_dir, File, ReadDir};
use std::path::Path;

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

	pub fn next_file(&mut self) -> Result<Option<File>> {
		loop {
			let reader = match self.stack.last_mut() {
				Some(reader) => reader,
				None => return Ok(None),
			};

			if let Some(entry) = reader.next() {
				let entry = entry?;
				let file_type = entry.file_type()?;

				if file_type.is_dir() {
					self.stack.push(read_dir(entry.path())?);
				} else if file_type.is_file() {
					return Ok(Some(File::open(entry.path())?));
				}
			} else {
				self.stack.pop();
			}
		}
	}
}
