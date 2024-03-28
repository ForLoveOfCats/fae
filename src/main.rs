#[macro_use]
mod error;

#[macro_use]
mod multi_line_string;

mod c_codegen;
mod cli_arguments;
mod codegen;
mod color;
mod file;
mod ir;
mod parser;
mod project;
mod span;
mod test;
mod tokenizer;
mod tree;
mod type_store;
mod validator;

use std::path::Path;

use cli_arguments::parse_arguments;
use project::build_project;

fn main() {
	let cli_arguments = parse_arguments();
	if cli_arguments.run_compiler_tests {
		test::run_tests(&cli_arguments);
		return;
	}

	let mut stderr = std::io::stderr();
	let project_path = Path::new("./example");
	let root_name = "example".to_owned();
	let _built_project = build_project(&cli_arguments, &mut stderr, project_path, root_name);

	// TODO: Run executable
}
