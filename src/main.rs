#[macro_use]
mod multi_line_string;

mod cli_arguments;
mod codegen;
mod color;
mod frontend;
mod test;

use std::os::unix::process::ExitStatusExt;
use std::path::Path;
use std::process::Command;

use cli_arguments::parse_arguments;
use frontend::project::build_project;

fn main() {
	let cli_arguments = parse_arguments();
	if cli_arguments.run_compiler_tests {
		test::run_tests(&cli_arguments);
		return;
	}

	let mut stderr = std::io::stderr();
	let project_path = Path::new("./example");
	let root_name = "example".to_owned();
	let built_project = build_project(&cli_arguments, &mut stderr, project_path, root_name);

	let Some(binary_path) = built_project.binary_path else {
		std::process::exit(-1);
	};

	let mut command = Command::new(binary_path);
	let mut child = command.spawn().unwrap();
	let status = child.wait().unwrap();

	if !status.success() {
		eprintln!("Child process exited with code {}", status.into_raw())
	}
}
