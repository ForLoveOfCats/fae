mod error;
mod file;
mod ice;
mod parser;
mod span;
mod tokenizer;
mod tree;

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

	let mut roots = Vec::new();

	for file in &files {
		let mut tokenizer = Tokenizer::new(&file.source);

		let root = match parse_file_root(&mut tokenizer) {
			Ok(root) => root,
			Err(err) => {
				err.print(&file.path, &file.source);
				return;
			}
		};

		let token_count = tokenizer.token_count();

		println!("{:#?}\n", root);
		println!("Finished parsing file with {} tokens", token_count);

		roots.push(root);
	}
}
