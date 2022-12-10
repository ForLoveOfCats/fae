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
use validator::{FileLayers, TypeStore};

fn main() {
	let files = match load_all_files("./example") {
		Ok(files) => files,
		Err(err) => {
			eprintln!("Error loading source files: {}", err);
			return;
		}
	};

	let mut messages = Messages::new();

	//Parallelizable
	let mut parsed_files = Vec::new();
	for file in &files {
		let parsed_file = parse_file(&mut messages, file);

		for message in messages.errors() {
			message.print(&file.path, &file.source, "Parse error");
		}
		messages.remove_errors();

		parsed_files.push(parsed_file);
	}

	if messages.any_errors() {
		return;
	}
	messages.reset();

	//Not parallelizable
	let mut type_store = TypeStore::new();
	let mut file_layers = match FileLayers::build(&mut messages, &parsed_files) {
		Some(file_layers) => file_layers,
		None => return,
	};
	//Prints messages internally
	validator::fill_root_scopes(&mut messages, &mut file_layers, &mut type_store);

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
