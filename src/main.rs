#[macro_use]
mod error;

mod file;
mod parser;
mod span;
mod tokenizer;
mod tree;

use error::Messages;
use file::load_all_files;
use parser::parse_file_root;
use tokenizer::Tokenizer;

fn main() {
	let files = match load_all_files("./example") {
		Ok(files) => files,
		Err(err) => {
			eprintln!("Error loading source files: {}", err);
			return;
		}
	};

	let mut messages = Messages::new();
	let mut roots = Vec::new();

	for file in &files {
		messages.clear();
		let mut tokenizer = Tokenizer::new(&file.source);

		let root = parse_file_root(&mut messages, &mut tokenizer);

		for message in messages.errors() {
			message.print(&file.path, &file.source, "Parse error");
		}

		// println!("{:#?}\n", root);
		roots.push(root);
	}
}
