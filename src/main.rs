#[macro_use]
mod error;

mod file;
mod ir;
mod parser;
mod span;
mod tokenizer;
mod tree;
mod validator;

use error::Messages;
use file::load_all_files;
use parser::parse_file;
use validator::{validate, FunctionStore, RootLayers, TypeStore};

fn main() {
	let files = match load_all_files("./example") {
		Ok(files) => files,
		Err(err) => {
			eprintln!("Error loading source files: {}", err);
			return;
		}
	};

	let mut messages = Messages::new(&files);

	//Parallelizable
	let mut parsed_files = Vec::new();
	for file in &files {
		messages.set_current_file_index(file.index);
		parsed_files.push(parse_file(&mut messages, file));
	}

	messages.print_errors("Parse error");
	messages.reset();

	//Partially parallelizable
	let mut type_store = TypeStore::new();
	let mut function_store = FunctionStore::new();
	let mut root_layers = RootLayers::new();
	let blocks = validate(&mut messages, &mut root_layers, &mut type_store, &mut function_store, &parsed_files);

	messages.print_errors("Validation error");
	if messages.any_errors() {
		return;
	}
	messages.reset();

	//Codegen
}
