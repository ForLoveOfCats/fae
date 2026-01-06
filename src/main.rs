#[macro_use]
mod multi_line_string;

#[macro_use]
mod tracy;

#[macro_use]
mod cli;

mod clean;
mod codegen;
mod color;
mod frontend;
mod lock;
mod reference;
mod test;
mod version;

use std::path::Path;
use std::process::Command;

use crate::cli::{parse_arguments, CompileCommand};
use crate::frontend::error::{StderrOutput, WriteFmt};
use crate::frontend::project::build_project;

#[cfg(not(target_env = "msvc"))]
#[global_allocator]
static GLOBAL: tikv_jemallocator::Jemalloc = tikv_jemallocator::Jemalloc;

pub const TARGET_DIR: &str = "./fae_target";

#[cfg(target_os = "windows")]
pub const DEFAULT_PROJECT_PATH: &str = ".\\";
#[cfg(not(target_os = "windows"))]
pub const DEFAULT_PROJECT_PATH: &str = "./";

fn main() {
	#[cfg(feature = "tracy-profile")]
	let _tracy = tracy::Main::new();

	let cli_arguments = parse_arguments();
	if cli_arguments.command == CompileCommand::CompilerTest {
		test::run_tests(cli_arguments);
	} else if cli_arguments.command == CompileCommand::Clean {
		clean::clean_workspace();
	}

	let supports_color = cli_arguments.color_messages;
	let mut stderr = std::io::stderr();
	let mut output = StderrOutput {
		supports_color,
		loud: cli_arguments.loud,
		stderr: &mut stderr,
	};

	let project_path = cli_arguments
		.project_path
		.as_deref()
		.unwrap_or_else(|| Path::new(DEFAULT_PROJECT_PATH));
	let built_project = build_project(&cli_arguments, &mut output, project_path, None);

	#[cfg(feature = "measure-lock-contention")]
	if let Some(lock) = parking_lot_contention::CONTENTIONS.get() {
		let contentions = lock.lock().unwrap();
		let mut contentions = contentions.iter().map(|(a, b)| (*a, *b)).collect::<Vec<_>>();
		contentions.sort_by_key(|c| c.1);
		contentions.reverse();
		dbg!(contentions);
	}

	let Some(binary_path) = built_project.binary_path else {
		if let CompileCommand::Build | CompileCommand::Run = cli_arguments.command {
			std::process::exit(-1);
		} else {
			return;
		}
	};

	if cli_arguments.command == CompileCommand::Run {
		output.alertln("     Running project", format_args!(""));
	} else if cli_arguments.command == CompileCommand::Test {
		output.alertln("     Testing project", format_args!(""));
	}

	if let CompileCommand::Run | CompileCommand::Test = cli_arguments.command {
		let mut command = Command::new(binary_path);
		command.args(cli_arguments.child_arguments);
		let mut child = command.spawn().unwrap();
		let status = child.wait().unwrap();

		if !status.success() {
			eprintln!("Child process exited with code {:?}", status.code())
		}
	}
}
