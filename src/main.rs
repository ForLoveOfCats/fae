#[macro_use]
mod multi_line_string;

#[macro_use]
mod cli;

mod codegen;
mod color;
mod frontend;
mod test;

use std::os::unix::process::ExitStatusExt;
use std::path::Path;
use std::process::Command;

use cli::{parse_arguments, CompileCommand};
use color::{BOLD_GREEN, RESET};
use frontend::project::build_project;

#[cfg(not(target_env = "msvc"))]
#[global_allocator]
static GLOBAL: tikv_jemallocator::Jemalloc = tikv_jemallocator::Jemalloc;

fn main() {
	let cli_arguments = parse_arguments();
	if cli_arguments.command == CompileCommand::CompilerTest {
		test::run_tests(cli_arguments);
		return;
	}

	let mut stderr = std::io::stderr();
	let project_path = cli_arguments.project_path.as_deref().unwrap_or_else(|| Path::new("./"));
	let built_project = build_project(&cli_arguments, &mut stderr, project_path, None);

	let Some(binary_path) = built_project.binary_path else {
		std::process::exit(-1);
	};

	if cli_arguments.command == CompileCommand::Run {
		eprintln!("  {BOLD_GREEN}Running executable{RESET}");

		let mut command = Command::new(binary_path);
		let mut child = command.spawn().unwrap();
		let status = child.wait().unwrap();

		if !status.success() {
			eprintln!("Child process exited with code {}", status.into_raw())
		}
	}
}
