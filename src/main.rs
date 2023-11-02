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

use c_codegen::DebugCodegen;
use project::build_project;

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
	let Some(binary_path) = build_project(&mut stderr, project_path, root_name, DebugCodegen::Yes) else {
		return;
	};

	let status = std::process::Command::new(binary_path)
		.spawn()
		.expect("Failed to launch resulting binary")
		.wait();
	println!("Binary execution result: {status:?}");
}
