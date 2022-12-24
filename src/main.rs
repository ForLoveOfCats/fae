#[macro_use]
mod error;

mod file;
mod mir;
mod parser;
mod span;
mod tokenizer;
mod tree;
mod validator;

use error::Messages;
use file::load_all_files;
use parser::parse_file;
use validator::{FileLayers, FunctionStore, TypeStore};

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

	//Not parallelizable
	let mut type_store = TypeStore::new();
	let mut function_store = FunctionStore::new();
	let mut file_layers = match FileLayers::build(&mut messages, &parsed_files) {
		Some(file_layers) => file_layers,
		None => return,
	};
	validator::validate_file_layers(&mut messages, &mut file_layers, &mut type_store);

	messages.print_errors("Validation error");
	if messages.any_errors() {
		return;
	}
	messages.reset();

	//Parallelizable
	// let mut base_scope = root.base_scope();
	// for parsed_file in &parsed_files {
	// 	validate_parsed_file(&mut base_scope, parsed_file);
	// }
}
