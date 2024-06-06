use std::path::{Path, PathBuf};
use std::time::Instant;

use parking_lot::RwLock;
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
use crate::frontend::tokenizer::Tokenizer;
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
	let is_test = test_config.is_some();
	let mut files = Vec::new();

	if cli_arguments.std_enabled {
		let std_path = std_path();
		if let Err(err) = load_all_files(&std_path, &mut files) {
			usage_error!("Failed to load standard library files at {std_path:?}: {}", err);
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

		let source_directory = match project_path.join(config.source_directory).canonicalize() {
			Ok(source_directory) => source_directory,
			Err(err) => usage_error!("Unable to canonicalize source directory path: {err}"),
		};

		let cwd = match std::env::current_dir() {
			Ok(cwd) => cwd,
			Err(err) => usage_error!("Unable to get current working directory: {err}"),
		};

		let source_directory = source_directory
			.strip_prefix(cwd)
			.map(|dir| Path::new("./").join(dir))
			.unwrap_or(source_directory);

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

	match cli_arguments.command {
		CompileCommand::Parse => eprintln!("    {BOLD_GREEN}Parsing project{RESET} {root_name}"),
		CompileCommand::Check => eprintln!("    {BOLD_GREEN}Checking project{RESET} {root_name}"),
		CompileCommand::Build | CompileCommand::Run => eprintln!("    {BOLD_GREEN}Building project{RESET} {root_name}"),
		CompileCommand::CompilerTest => {}
	}

	let mut any_errors = false;
	let mut any_messages = false;
	let mut messages = Messages::new(&files);

	//Parallelizable
	let bump = bumpalo::Bump::new();
	let parse_start = Instant::now();
	let mut parsed_files = Vec::new();
	let mut tokens_vec = Vec::new();
	for file in &files {
		let mut tokenizer = Tokenizer::new(file.index, &file.source);
		let mut tokens = tokenizer.tokenize(std::mem::take(&mut tokens_vec), &mut messages);
		parsed_files.push(parse_file(&bump, &mut messages, &mut tokens, file));
		tokens_vec = tokens.tear_down();
	}

	if cli_arguments.command != CompileCommand::CompilerTest {
		eprintln!("    {BOLD_GREEN}Parsed all files{RESET} took {} ms", parse_start.elapsed().as_millis());
	}

	if cli_arguments.command == CompileCommand::Parse {
		if !is_test {
			std::mem::forget(parsed_files);
			std::mem::forget(tokens_vec);
			std::mem::forget(bump);
		}
		return BuiltProject { binary_path: None, any_messages, any_errors };
	}

	any_errors |= messages.any_errors();
	any_messages |= messages.any_messages();
	messages.print_messages(err_output, "Parse");
	messages.reset();

	// TODO: Remove this once confident about order independence
	// Make sure to remove the rand dependency while at it
	use rand::prelude::*;
	parsed_files.shuffle(&mut rand::thread_rng());

	//Partially parallelizable
	let validate_start = Instant::now();
	let herd = bumpalo_herd::Herd::new();
	let lang_items = RwLock::new(LangItems::new());
	let mut root_layers = RootLayers::new(root_name);
	let mut type_store = TypeStore::new(cli_arguments.debug_generics);
	let function_store = FunctionStore::new();
	let statics = RwLock::new(Statics::new());
	validate(
		cli_arguments,
		&herd,
		&mut messages,
		&lang_items,
		&mut root_layers,
		&mut type_store,
		&function_store,
		&statics,
		&parsed_files,
	);

	any_errors |= messages.any_errors();
	any_messages |= messages.any_messages();
	messages.print_messages(err_output, "Validation");
	messages.reset();
	if any_errors {
		if !is_test {
			std::mem::forget(function_store);
			std::mem::forget(type_store);
			std::mem::forget(parsed_files);
			std::mem::forget(tokens_vec);
			std::mem::forget(bump);
		}
		return BuiltProject { binary_path: None, any_messages, any_errors };
	}

	if cli_arguments.command != CompileCommand::CompilerTest {
		eprintln!("   {BOLD_GREEN}Validated project{RESET} took {} ms", validate_start.elapsed().as_millis());
	}

	if cli_arguments.command == CompileCommand::Check {
		if !is_test {
			std::mem::forget(function_store);
			std::mem::forget(type_store);
			std::mem::forget(parsed_files);
			std::mem::forget(tokens_vec);
			std::mem::forget(bump);
		}
		return BuiltProject { binary_path: None, any_messages, any_errors };
	}

	//Not parallelizable
	let codegen_start = Instant::now();
	let binary_path = match cli_arguments.codegen_backend {
		CodegenBackend::LLVM => llvm::driver::generate_code(
			cli_arguments,
			&mut messages,
			&lang_items.read(),
			&mut type_store,
			&function_store, // TODO: Avoid having single threaded generation pay the rwlock cost
			&statics.read(),
		),
	};
	if cli_arguments.command != CompileCommand::CompilerTest {
		eprintln!("    {BOLD_GREEN}Finished codegen{RESET} took {} ms", codegen_start.elapsed().as_millis());
	}

	assert!(!messages.any_errors());
	any_errors |= messages.any_errors();
	any_messages |= messages.any_messages();

	if cli_arguments.command != CompileCommand::CompilerTest {
		eprintln!("        {BOLD_GREEN}Built binary{RESET} {}", binary_path.display());
	}

	if !is_test {
		std::mem::forget(function_store);
		std::mem::forget(type_store);
		std::mem::forget(parsed_files);
		std::mem::forget(tokens_vec);
		std::mem::forget(bump);
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
