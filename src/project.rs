use std::path::{Path, PathBuf};

use crate::codegen;
use crate::codegen::ir::IrModule;
use crate::error::{Messages, WriteFmt};
use crate::file::load_all_files;
use crate::parser::parse_file;
use crate::type_store::TypeStore;
use crate::validator::{validate, CIncludeStore, FunctionStore, RootLayers};

pub fn build_project(err_output: &mut impl WriteFmt, path: &Path, root_name: String) -> Option<PathBuf> {
	// Can be folded into parallel parsing, ish
	let mut files = Vec::new();
	if let Err(err) = load_all_files(Path::new("./lib"), &mut files) {
		eprintln!("Error loading standard library files: {}", err);
		return None;
	}
	if let Err(err) = load_all_files(path, &mut files) {
		eprintln!("Error loading source files: {}", err);
		return None;
	}

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
	let mut module = IrModule::new();
	validate(
		&mut messages,
		&mut root_layers,
		&mut c_include_store,
		&mut type_store,
		&mut function_store,
		&mut module,
		&parsed_files,
	);

	messages.print_messages(err_output, "Validation");
	if any_parse_errors || messages.any_errors() {
		return None;
	}
	messages.reset();

	module.debug_dump();
	println!("\n\n\n");
	let tracker = codegen::optimization::optimize(&mut module);
	module.debug_dump();
	println!("\n\n\n");
	println!("{tracker:#?}");
	println!();

	//Not parallelizable
	let binary_path = Path::new("./output.executable");
	let elf = codegen::amd64::elf::construct_elf(module);
	std::fs::write("./shared/executable.x64", elf).unwrap();
	// generate_code(
	// 	&mut messages,
	// 	&c_include_store,
	// 	&mut type_store,
	// 	&mut function_store,
	// 	OptimizationLevel::None,
	// 	binary_path,
	// 	debug_codegen,
	// );
	// assert!(!messages.any_errors());

	Some(binary_path.to_path_buf())
}
