#[macro_use]
mod error;

#[macro_use]
mod multi_line_string;

// mod c_codegen; TODO Remove
mod codegen;
mod color;
mod file;
mod ir; // TODO Remove
mod parser;
mod project;
mod span;
mod test;
mod tokenizer;
mod tree;
mod type_store;
mod validator;

use std::{ffi::OsStr, path::Path};

use project::build_project;

// TODO: Re-enable standard library
pub const ENABLE_STANDARD_LIBRARY: bool = false;

fn main() {
	let mut args = std::env::args_os().skip(1);
	if args.next().as_deref() == Some(OsStr::new("t")) {
		let args = args.map(|s| s.to_str().expect("test name arguments must be valid Unicode").to_owned());
		test::run_tests(args.collect());
		return;
	}

	let mut stderr = std::io::stderr();
	let project_path = Path::new("./example");
	let root_name = "example".to_owned();
	let Some(_binary_path) = build_project(&mut stderr, project_path, root_name) else {
		return;
	};

	// TODO: Run executable
}
