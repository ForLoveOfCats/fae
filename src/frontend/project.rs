use std::path::{Path, PathBuf};

use serde::Deserialize;

use crate::cli::{CliArguments, CodegenBackend, CompileCommand};
use crate::codegen::llvm;
use crate::color::*;
use crate::frontend::error::{Messages, WriteFmt};
use crate::frontend::file::{load_all_files, load_single_file};
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

#[derive(Deserialize)]
pub struct ProjectConfig {
	pub project_name: String,
	pub source_directory: PathBuf,
}

pub fn build_project(
	cli_arguments: &CliArguments,
	err_output: &mut impl WriteFmt,
	project_path: &Path,
	test_config: Option<ProjectConfig>,
) -> BuiltProject {
	let mut files = Vec::new();

	if cli_arguments.std_enabled {
		if let Err(err) = load_all_files(&std_path(), &mut files) {
			usage_error!("Failed to load standard library files: {}", err);
		}
	}

	let root_name = if project_path.is_dir() {
		let config = if let Some(test_config) = test_config {
			test_config
		} else {
			let config_path = project_path.join("fae.toml");
			let Ok(config_file) = std::fs::read_to_string(&config_path) else {
				usage_error!("Input directory does not contain a `fae.toml` file");
			};

			match toml::from_str::<ProjectConfig>(&config_file) {
				Ok(config) => config,
				Err(err) => usage_error!("Project config parse error {config_path:?}\n{err}"),
			}
		};

		let source_directory = project_path.join(config.source_directory);
		if let Err(err) = load_all_files(&source_directory, &mut files) {
			usage_error!("Failed to load source files: {}", err);
		}

		config.project_name
	} else if project_path.is_file() {
		match load_single_file(project_path.to_path_buf(), &mut files) {
			Ok(root_name) => root_name,
			Err(err) => usage_error!("Failed to load source file: {}", err),
		}
	} else {
		usage_error!("Input path is neither directory nor file");
	};

	if cli_arguments.command != CompileCommand::CompilerTest {
		eprintln!("    {BOLD_GREEN}Building project{RESET} {root_name}",);
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

	if cli_arguments.command != CompileCommand::CompilerTest {
		eprintln!("        {BOLD_GREEN}Built binary{RESET} {}", binary_path.display());
	}

	BuiltProject { binary_path: Some(binary_path), any_messages, any_errors }
}

fn std_path() -> PathBuf {
	if cfg!(feature = "bundled") {
		let Ok(mut path) = std::env::current_exe() else {
			usage_error!("Unable to get own executable path");
		};

		path.pop();
		path.join("./lib")
	} else {
		PathBuf::from("./lib")
	}
}
