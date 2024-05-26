use std::ffi::{OsStr, OsString};
use std::path::PathBuf;

#[derive(Debug)]
pub struct CliArguments {
	pub project_path: Option<PathBuf>,
	pub command: CompileCommand,
	pub std_enabled: bool,
	pub compiler_test_names: Vec<String>,
	pub codegen_backend: CodegenBackend,
	pub optimize_artifacts: bool,
	pub debug_generics: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompileCommand {
	Build,
	Run,
	CompilerTest,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CodegenBackend {
	LLVM,
}

#[macro_export]
macro_rules! usage_error {
	($($arg:tt)*) => {{
		eprint!("Usage error: ");
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
		std_enabled: true,
		compiler_test_names: Vec::new(),
		codegen_backend: CodegenBackend::LLVM,
		optimize_artifacts: false,
		debug_generics: false,
	};

	while let Some(arg) = iterator.next() {
		if arg.as_encoded_bytes().starts_with(b"-") {
			parse_tack_option(&mut cli_arguments, &mut any_errors, arg.as_os_str());
		} else if parse_command(&mut cli_arguments, &mut any_errors, arg.as_os_str(), &mut iterator) {
		} else if cli_arguments.project_path.is_none() {
			cli_arguments.project_path = Some(PathBuf::from(arg));
		} else {
			eprintln!("Unknown cli argument {arg:?}");
			any_errors = true;
		}
	}

	if any_errors {
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
		"build" | "b" => {
			cli_arguments.command = CompileCommand::Build;
			true
		}

		"run" | "r" => {
			cli_arguments.command = CompileCommand::Run;
			true
		}

		#[cfg(not(feature = "bundled"))]
		"ct" => {
			cli_arguments.command = CompileCommand::CompilerTest;
			parse_test_names(cli_arguments, any_errors, iterator);
			true
		}

		_ => false,
	}
}

fn parse_tack_option(cli_arguments: &mut CliArguments, any_errors: &mut bool, arg: &OsStr) {
	match arg.to_str() {
		Some("--help") => {
			eprintln!("Fae programming language compiler");
			eprintln!("Usage: fae [optional command] [zero or more options] [optional path]");
			eprintln!();
			eprintln!("Commands:");
			eprintln!("  build, b: Build targeted project (default)");
			eprintln!("  run, r: Build and run targeted project");
			#[cfg(not(feature = "bundled"))]
			eprintln!("  ct: Run compiler test suite");
			eprintln!();
			eprintln!("Options:");
			eprintln!("  --help: Print this help message");
			eprintln!("  --release: Build artifacts with optimizations enabled");
			eprintln!("  --debug-generics: Include useful debug information when printing types");
			eprintln!("  --disable-std: Avoid compiling the Fae standard library (will not successfully link)");
			std::process::exit(0);
		}

		Some("--release") => {
			cli_arguments.optimize_artifacts = true;
		}

		Some("--debug-generics") => {
			cli_arguments.debug_generics = true;
		}

		Some("--disable-std") => {
			cli_arguments.std_enabled = false;
		}

		arg => {
			eprintln!("Unknown cli option {arg:?}");
			*any_errors = true;
		}
	}
}

fn parse_test_names(cli_arguments: &mut CliArguments, any_errors: &mut bool, iterator: &mut impl Iterator<Item = OsString>) {
	for arg in iterator {
		if arg.to_str().map(|a| a.starts_with('-')).unwrap_or(false) {
			parse_tack_option(cli_arguments, any_errors, &arg);
			continue;
		}

		let Some(arg) = arg.to_str() else {
			eprintln!("Invalid utf-8 for cli argument {arg:?}");
			continue;
		};

		cli_arguments.compiler_test_names.push(arg.to_owned());
	}
}
