use std::path::{Path, PathBuf};
use std::time::Instant;

use serde::Deserialize;

use crate::cli::{CliArguments, CodegenBackend, CompileCommand};
use crate::codegen::llvm;
use crate::frontend::error::{Messages, RootMessages, WriteFmt};
use crate::frontend::file::{load_all_files, load_single_file};
use crate::frontend::function_store::FunctionStore;
use crate::frontend::lang_items::LangItems;
use crate::frontend::parser::parse_file;
use crate::frontend::root_layers::RootLayers;
use crate::frontend::symbols::Statics;
use crate::frontend::tokenizer::Tokenizer;
use crate::frontend::type_store::TypeStore;
use crate::frontend::validator::validate;
use crate::lock::RwLock;

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
	message_output: &mut impl WriteFmt,
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
		CompileCommand::Parse => message_output.alertln("    Parsing project", format_args!("{root_name}")),
		CompileCommand::Check => message_output.alertln("    Checking project", format_args!("{root_name}")),
		CompileCommand::Build | CompileCommand::Run => {
			message_output.alertln("    Building project", format_args!("{root_name}"))
		}
		CompileCommand::CompilerTest => {}
	}

	let mut any_errors = false;
	let mut any_messages = false;
	let mut root_messages = RootMessages::new(&files);

	//Parallelizable
	let bump = bumpalo::Bump::new();
	let parse_start = Instant::now();
	let mut parsed_files = Vec::new();
	let mut tokens_vec = Vec::new();
	for file in &files {
		let mut tokenizer = Tokenizer::new(file.index, &file.source);
		let mut messages = Messages::new(&file.module_path);
		let mut tokens = tokenizer.tokenize(std::mem::take(&mut tokens_vec), &mut messages);
		parsed_files.push(parse_file(&bump, &mut messages, &mut tokens, file));
		root_messages.add_messages_if_any(messages);
		tokens_vec = tokens.tear_down();
	}

	if cli_arguments.command != CompileCommand::CompilerTest {
		message_output.alertln("    Parsed all files", format_args!("took {} ms", parse_start.elapsed().as_millis()));
	}

	if cli_arguments.command == CompileCommand::Parse {
		if !is_test && !cfg!(feature = "measure-lock-contention") {
			std::mem::forget(parsed_files);
			std::mem::forget(tokens_vec);
			std::mem::forget(bump);
		}
		return BuiltProject { binary_path: None, any_messages, any_errors };
	}

	any_errors |= root_messages.any_errors();
	any_messages |= root_messages.any_messages();
	root_messages.print_messages(message_output, "Parse");
	root_messages.reset();

	// TODO: Remove this once confident about order independence
	// Make sure to remove the rand dependency while at it
	// use rand::prelude::*;
	// parsed_files.shuffle(&mut rand::thread_rng());
	// parsed_files.sort_by(|a, b| a.module_path.cmp(b.module_path));

	//Partially parallelizable
	let validate_start = Instant::now();
	let herd = bumpalo_herd::Herd::new();
	let lang_items = RwLock::new(LangItems::new());
	let mut root_layers = RootLayers::new(root_name);
	let mut type_store = TypeStore::new(cli_arguments.debug_generics, cli_arguments.debug_type_ids);
	let function_store = FunctionStore::new();
	let statics = RwLock::new(Statics::new());
	validate(
		cli_arguments,
		is_test,
		&herd,
		&mut root_messages,
		&lang_items,
		&mut root_layers,
		&mut type_store,
		&function_store,
		&statics,
		&parsed_files,
	);

	any_errors |= root_messages.any_errors();
	any_messages |= root_messages.any_messages();
	root_messages.print_messages(message_output, "Validation");
	root_messages.reset();
	if any_errors {
		if !is_test && !cfg!(feature = "measure-lock-contention") {
			std::mem::forget(function_store);
			std::mem::forget(type_store);
			std::mem::forget(parsed_files);
			std::mem::forget(tokens_vec);
			std::mem::forget(bump);
		}
		return BuiltProject { binary_path: None, any_messages, any_errors };
	}

	if cli_arguments.command != CompileCommand::CompilerTest {
		message_output.alertln("   Validated project", format_args!("took {} ms", validate_start.elapsed().as_millis()));
	}

	if cli_arguments.command == CompileCommand::Check {
		if !is_test && !cfg!(feature = "measure-lock-contention") {
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
	let mut codegen_messages = Messages::new(&[]);
	let binary_path = match cli_arguments.codegen_backend {
		CodegenBackend::LLVM => llvm::driver::generate_code(
			cli_arguments,
			&mut codegen_messages,
			&lang_items.read(),
			&mut type_store,
			&function_store, // TODO: Avoid having single threaded generation pay the rwlock cost
			&statics.read(),
		),
	};
	if cli_arguments.command != CompileCommand::CompilerTest {
		message_output.alertln("    Finished codegen", format_args!("took {} ms", codegen_start.elapsed().as_millis()));
	}

	assert!(!codegen_messages.any_messages());

	if cli_arguments.command != CompileCommand::CompilerTest {
		message_output.alertln("        Built binary", format_args!("{}", binary_path.display()));
	}

	if !is_test && !cfg!(feature = "measure-lock-contention") {
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
