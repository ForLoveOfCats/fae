use std::path::{Path, PathBuf};

use crate::cli_arguments::{CliArguments, CodegenBackend};
use crate::codegen::llvm;
use crate::frontend::error::{Messages, WriteFmt};
use crate::frontend::file::load_all_files;
use crate::frontend::function_store::FunctionStore;
use crate::frontend::lang_items::LangItems;
use crate::frontend::parser::parse_file;
use crate::frontend::root_layers::RootLayers;
use crate::frontend::symbols::Statics;
use crate::frontend::type_store::TypeStore;
use crate::frontend::validator::validate;

pub struct BuiltProject {
	pub binary_path: Option<PathBuf>,
	pub any_errors: bool,
	pub any_messages: bool,
}

pub fn build_project(
	cli_arguments: &CliArguments,
	err_output: &mut impl WriteFmt,
	path: &Path,
	root_name: String,
) -> BuiltProject {
	// Can be folded into parallel parsing, ish
	let mut files = Vec::new();
	if let Err(err) = load_all_files(Path::new("./lib"), &mut files) {
		panic!("Error loading standard library files: {}", err);
	}
	if let Err(err) = load_all_files(path, &mut files) {
		panic!("Error loading source files: {}", err);
	}

	let mut any_errors = false;
	let mut any_messages = false;
	let mut messages = Messages::new(&files);

	//Parallelizable
	let mut parsed_files = Vec::new();
	for file in &files {
		parsed_files.push(parse_file(&mut messages, file));
	}

	any_errors |= messages.any_errors();
	any_messages |= messages.any_messages();
	messages.print_messages(err_output, "Parse");
	messages.reset();

	//Partially parallelizable
	let mut lang_items = LangItems::new();
	let mut root_layers = RootLayers::new(root_name);
	let mut type_store = TypeStore::new();
	let mut function_store = FunctionStore::new();
	let mut statics = Statics::new();
	validate(
		cli_arguments,
		&mut messages,
		&mut lang_items,
		&mut root_layers,
		&mut type_store,
		&mut function_store,
		&mut statics,
		&parsed_files,
	);

	any_errors |= messages.any_errors();
	any_messages |= messages.any_messages();
	messages.print_messages(err_output, "Validation");
	messages.reset();
	if any_errors {
		return BuiltProject { binary_path: None, any_messages, any_errors };
	}

	//Not parallelizable
	let binary_path = match cli_arguments.codegen_backend {
		CodegenBackend::LLVM => {
			llvm::amd64::generate_code(&mut messages, &lang_items, &mut type_store, &mut function_store, &statics)
		}
	};

	assert!(!messages.any_errors());
	any_errors |= messages.any_errors();
	any_messages |= messages.any_messages();
	BuiltProject { binary_path: Some(binary_path), any_messages, any_errors }
}
