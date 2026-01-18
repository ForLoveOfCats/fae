use std::fmt::Display;
use std::path::{Component, Path, PathBuf};

pub trait PathUtils {
	fn flattened(&self) -> PathBuf;

	fn unix_path_display<'a>(&'a self) -> UnixPathDisplay<'a>;
}

impl PathUtils for Path {
	fn unix_path_display<'a>(&'a self) -> UnixPathDisplay<'a> {
		UnixPathDisplay { path: self }
	}

	fn flattened(&self) -> PathBuf {
		let mut result = PathBuf::with_capacity(self.as_os_str().len());

		let mut first = true;
		for component in self.components() {
			match component {
				Component::Prefix(prefix_component) => {
					result.push(Path::new(prefix_component.as_os_str()));
				}

				Component::RootDir => {}

				Component::CurDir => {
					if first {
						result.push(".");
					}
				}

				Component::ParentDir => {
					if first {
						result.push("..");
					} else {
						result.pop();
					}
				}

				Component::Normal(os_str) => {
					result.push(Path::new(os_str));
				}
			}

			first = false;
		}

		result
	}
}

pub struct UnixPathDisplay<'a> {
	path: &'a Path,
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
impl<'a> Display for UnixPathDisplay<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		self.path.display().fmt(f)
	}
}

#[cfg(target_os = "windows")]
impl<'a> Display for UnixPathDisplay<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		use std::ffi::OsStr;
		use std::fmt::Write;

		let mut components = self.path.components().peekable();
		while let Some(component) = components.next() {
			match component {
				Component::Prefix(prefix_component) => {
					const DEVICE_PATH_PREFIX: &[u8] = b"\\\\?\\";

					let mut bytes = prefix_component.as_os_str().as_encoded_bytes();
					if bytes.starts_with(DEVICE_PATH_PREFIX) {
						bytes = &bytes[DEVICE_PATH_PREFIX.len()..];
					}

					let os_str = unsafe { OsStr::from_encoded_bytes_unchecked(bytes) };
					os_str.display().fmt(f)?;
					continue;
				}

				Component::CurDir => f.write_char('.')?,
				Component::ParentDir => f.write_str("..")?,
				Component::Normal(os_str) => os_str.display().fmt(f)?,

				Component::RootDir => {}
			}

			if components.peek().is_some() {
				f.write_char('/')?
			}
		}

		Ok(())
	}
}
