use std::ffi::{OsStr, OsString};
use std::path::PathBuf;

#[derive(Debug)]
pub struct CliArguments {
	pub project_path: Option<PathBuf>,
	pub run_compiler_tests: bool,
	pub compiler_test_names: Vec<String>,
	pub codegen_backend: CodegenBackend,
}

#[derive(Debug, PartialEq, Eq)]
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
	let mut iterator = std::env::args_os().skip(1).enumerate();

	let mut any_errors = false;
	let mut cli_arguments = CliArguments {
		project_path: None,
		run_compiler_tests: false,
		compiler_test_names: Vec::new(),
		codegen_backend: CodegenBackend::LLVM,
	};

	while let Some((index, arg)) = iterator.next() {
		if arg.as_encoded_bytes().starts_with(b"-") {
			parse_tack_argument(&mut cli_arguments, &mut any_errors, arg.as_os_str(), index);
		} else if let Some("t" | "test") = arg.to_str() {
			cli_arguments.run_compiler_tests = true;
			parse_test_names(&mut cli_arguments, &mut any_errors, &mut iterator);
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

fn parse_tack_argument(_cli_arguments: &mut CliArguments, any_errors: &mut bool, arg: &OsStr, index: usize) {
	match arg.to_str() {
		Some("--help") => {
			eprintln!("Fae programming language compiler");
			eprintln!();
			eprintln!("Command line arguments:");
			eprintln!("  test, t: Run compiler test suite");
			eprintln!("  --help: Print this help message");
			std::process::exit(0);
		}

		Some(arg) => {
			eprintln!("Unknown cli argument {arg:?}");
			*any_errors = true;
		}

		None => {
			let nth = index + 1;
			eprintln!("Invalid utf-8 for cli argument {nth}");
			*any_errors = true;
		}
	}
}

fn parse_test_names(
	cli_arguments: &mut CliArguments,
	any_errors: &mut bool,
	iterator: &mut impl Iterator<Item = (usize, OsString)>,
) {
	for (index, arg) in iterator {
		if arg.to_str().map(|a| a.starts_with('-')).unwrap_or(false) {
			parse_tack_argument(cli_arguments, any_errors, &arg, index);
			continue;
		}

		let Some(arg) = arg.to_str() else {
			let nth = index + 1;
			eprintln!("Invalid utf-8 for cli argument {nth}");
			continue;
		};

		cli_arguments.compiler_test_names.push(arg.to_owned());
	}
}
