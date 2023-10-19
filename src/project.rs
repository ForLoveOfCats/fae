use std::path::{Path, PathBuf};

use crate::c_codegen::{generate_code, OptimizationLevel};
use crate::error::{Messages, WriteFmt};
use crate::file::load_all_files;
use crate::parser::parse_file;
use crate::type_store::TypeStore;
use crate::validator::{validate, CIncludeStore, FunctionStore, RootLayers};

pub fn build_project(err_output: &mut impl WriteFmt, path: &Path, root_name: String, debug_codegen: bool) -> Option<PathBuf> {
	// Can be folded into parallel parsing, ish
	let files = match load_all_files(path) {
		Ok(files) => files,
		Err(err) => {
			eprintln!("Error loading source files: {}", err);
			return None;
		}
	};

	let mut messages = Messages::new(&files);

	//Parallelizable
	let mut parsed_files = Vec::new();
	for file in &files {
		parsed_files.push(parse_file(&mut messages, file));
	}

	let any_parse_errors = messages.any_errors();
	messages.print_messages(err_output, "Parse");
	messages.reset();

	//Partially parallelizable
	let mut c_include_store = CIncludeStore::new();
	let mut type_store = TypeStore::new();
	let mut function_store = FunctionStore::new();
	let mut root_layers = RootLayers::new(root_name);
	validate(
		&mut messages,
		&mut root_layers,
		&mut c_include_store,
		&mut type_store,
		&mut function_store,
		&parsed_files,
	);

	messages.print_messages(err_output, "Validation");
	if any_parse_errors || messages.any_errors() {
		return None;
	}
	messages.reset();

	//Not parallelizable
	let binary_path = Path::new("./output.executable");
	generate_code(
		&mut messages,
		&c_include_store,
		&mut type_store,
		&mut function_store,
		OptimizationLevel::None,
		binary_path,
		debug_codegen,
	);
	assert!(!messages.any_errors());

	Some(binary_path.to_path_buf())
}
