use std::fs::{read_dir, File};
use std::path::Path;

type Result<T> = std::result::Result<T, std::io::Error>;

pub struct FileList {
	files: Vec<File>,
}

impl FileList {
	pub fn discover_files<P: AsRef<Path>>(path: P) -> Result<FileList> {
		let mut list = FileList { files: Vec::new() };
		list.walk_dir(path)?;
		Ok(list)
	}

	fn walk_dir<P: AsRef<Path>>(&mut self, path: P) -> Result<()> {
		for entry in read_dir(path)? {
			let entry = entry?;
			let file_type = entry.file_type()?;

			if file_type.is_dir() {
				self.walk_dir(entry.path())?;
			} else if file_type.is_file() {
				let file = File::open(entry.path())?;
				self.files.push(file);
			}
		}

		Ok(())
	}

	pub fn take_file(&mut self) -> Option<File> {
		self.files.pop()
	}
}
