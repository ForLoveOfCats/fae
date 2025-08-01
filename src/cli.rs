use std::ffi::{OsStr, OsString};
use std::path::PathBuf;

use crate::color::{BOLD_RED, BOLD_YELLOW, RESET};
use crate::version::version;

#[derive(Debug)]
pub struct CliArguments {
	pub project_path: Option<PathBuf>,
	pub command: CompileCommand,
	pub child_arguments: Vec<OsString>,
	pub loud: bool,
	pub color_messages: bool,
	pub std_enabled: bool,
	pub compiler_test_names: Vec<String>,
	pub codegen_backend: CodegenBackend,
	pub parallel_validator: bool,
	pub verify_llvm_module: bool,
	pub emit_llvm_ir: bool,
	pub optimize_artifacts: bool,
	pub debug_generics: bool,
	pub debug_type_ids: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompileCommand {
	Parse,
	Check,
	Build,
	Run,
	Clean,
	Test,
	CompilerTest,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CodegenBackend {
	LLVM,
}

#[macro_export]
macro_rules! usage_error {
	($($arg:tt)*) => {{
		eprint!("{}Usage error:{} ", crate::color::BOLD_RED, crate::color::RESET);
		eprintln!($( $arg )*);
		std::process::exit(-1);
	}}
}

pub fn parse_arguments() -> CliArguments {
	let mut iterator = std::env::args_os().skip(1);

	let mut any_errors = false;
	let mut cli_arguments = CliArguments {
		project_path: None,
		command: CompileCommand::Build,
		child_arguments: Vec::new(),
		loud: true,
		color_messages: true,
		std_enabled: true,
		compiler_test_names: Vec::new(),
		codegen_backend: CodegenBackend::LLVM,
		parallel_validator: false,
		verify_llvm_module: true,
		emit_llvm_ir: false,
		optimize_artifacts: false,
		debug_generics: false,
		debug_type_ids: false,
	};

	while let Some(arg) = iterator.next() {
		if arg.as_encoded_bytes().starts_with(b"-") {
			parse_tack_option(&mut cli_arguments, &mut any_errors, arg.as_os_str(), &mut iterator);
		} else if parse_command(&mut cli_arguments, &mut any_errors, arg.as_os_str(), &mut iterator) {
		} else if cli_arguments.project_path.is_none() {
			cli_arguments.project_path = Some(PathBuf::from(arg));
		} else {
			eprintln!("Unknown cli argument {arg:?}");
			any_errors = true;
		}
	}

	if any_errors {
		eprintln!("{BOLD_YELLOW}Usage hint:{RESET} To view commands and options try \"--help\"");
		std::process::exit(-1);
	}

	cli_arguments
}

fn parse_command(
	cli_arguments: &mut CliArguments,
	any_errors: &mut bool,
	arg: &OsStr,
	iterator: &mut impl Iterator<Item = OsString>,
) -> bool {
	let Some(arg) = arg.to_str() else {
		return false;
	};

	match arg {
		"parse" | "p" => cli_arguments.command = CompileCommand::Parse,

		"check" | "c" => cli_arguments.command = CompileCommand::Check,

		"build" | "b" => cli_arguments.command = CompileCommand::Build,

		"run" | "r" => cli_arguments.command = CompileCommand::Run,

		"test" | "t" => cli_arguments.command = CompileCommand::Test,

		"clean" => cli_arguments.command = CompileCommand::Clean,

		"ct" => {
			cli_arguments.command = CompileCommand::CompilerTest;
			parse_test_names(cli_arguments, any_errors, iterator);
		}

		_ => return false,
	}

	true
}

fn parse_tack_option(
	cli_arguments: &mut CliArguments,
	any_errors: &mut bool,
	arg: &OsStr,
	iterator: &mut impl Iterator<Item = OsString>,
) {
	match arg.to_str() {
		Some("--help" | "-h") => {
			eprintln!("Fae programming language compiler {}", version());
			eprintln!("Usage: fae [optional command] [zero or more options] [optional path]");
			eprintln!("Arguments after lone \"--\" are passed to launched executable");
			eprintln!();
			eprintln!("Commands:");
			eprintln!("  parse, p: Parse targeted project");
			eprintln!("  check, c: Check targeted project");
			eprintln!("  build, b: Build targeted project (default)");
			eprintln!("  run, r: Build and run targeted project");
			eprintln!("  clean: Remove the current target directory");
			#[cfg(not(feature = "bundled"))]
			eprintln!("  ct: Run compiler test suite");
			eprintln!();
			eprintln!("Options:");
			eprintln!("  --help, -h: Print this help message");
			eprintln!("  --version, -v: Print the version string");
			eprintln!("  --quiet, -q: Silence compilation progress messages");
			eprintln!("  --release, -r: Build artifacts with optimizations enabled");
			eprintln!();
			eprintln!("Advanced Options:");
			eprintln!("  --emit-llvm-ir: Write LLVM IR to a file while building");
			eprintln!("  --disable-message-color: Avoid printing messages with color highlights");
			if !cfg!(feature = "bundled") {
				eprintln!();
				eprintln!("Compiler Debugging Options:");
				eprintln!("  --parallel-validator: Enable experimential parallelized validator");
				eprintln!("  --debug-generics: Include useful debug information when printing types");
				eprintln!("  --debug-type-ids: Print types as their internal type id index value");
				eprintln!("  --disable-std: Avoid compiling the Fae standard library (will not successfully link)");
				eprintln!("  --disable-llvm-verification: Skip running LLVM IR validation step");
			}
			std::process::exit(0);
		}

		Some("--version" | "-v") => {
			eprintln!("Fae {}", version());
			std::process::exit(0);
		}

		Some("--quiet" | "-q") => cli_arguments.loud = false,

		Some("--release" | "-r") => cli_arguments.optimize_artifacts = true,

		Some("--parallel-validator") => cli_arguments.parallel_validator = true,

		Some("--disable-message-color") => cli_arguments.color_messages = false,

		Some("--debug-generics") => cli_arguments.debug_generics = true,

		Some("--debug-type-ids") => cli_arguments.debug_type_ids = true,

		Some("--disable-std") => cli_arguments.std_enabled = false,

		Some("--disable-llvm-verification") => cli_arguments.verify_llvm_module = false,

		Some("--emit-llvm-ir") => cli_arguments.emit_llvm_ir = true,

		Some("--") => parse_child_arguments(cli_arguments, iterator),

		_ => {
			eprintln!("{BOLD_RED}Usage error:{RESET} Unknown cli option {arg:?}");
			*any_errors = true;
		}
	}
}

fn parse_test_names(cli_arguments: &mut CliArguments, any_errors: &mut bool, iterator: &mut impl Iterator<Item = OsString>) {
	while let Some(arg) = iterator.next() {
		if arg.to_str().map(|a| a.starts_with('-')).unwrap_or(false) {
			parse_tack_option(cli_arguments, any_errors, &arg, iterator);
			continue;
		}

		let Some(arg) = arg.to_str() else {
			eprintln!("Invalid utf-8 for cli argument {arg:?}");
			continue;
		};

		cli_arguments.compiler_test_names.push(arg.to_owned());
	}
}

fn parse_child_arguments(cli_arguments: &mut CliArguments, iterator: &mut impl Iterator<Item = OsString>) {
	for arg in iterator {
		cli_arguments.child_arguments.push(arg);
	}
}
