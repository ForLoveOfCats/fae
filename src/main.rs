#[macro_use]
mod error;

mod c_codegen;
mod color;
mod file;
mod ir;
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

fn main() {
	let arg = std::env::args_os().skip(1).next();
	if arg.as_ref().map(|a| a.as_os_str()) == Some(OsStr::new("t")) {
		test::run_tests();
		return;
	}

	let mut stderr = std::io::stderr();
	let project_path = Path::new("./example");
	let root_name = "example".to_owned();
	let Some(binary_path) = build_project(&mut stderr, project_path, root_name, true) else {
		return;
	};

	let status = std::process::Command::new(&binary_path)
		.spawn()
		.expect("Failed to launch resulting binary")
		.wait();
	println!("Binary execution result: {status:?}");
}
