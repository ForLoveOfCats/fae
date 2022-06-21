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

fn main() -> Result<(), Box<dyn std::error::Error>> {
	let files = load_all_files("./example")?;

	let mut roots = Vec::new();

	for file in &files {
		let (root, token_count) = {
			let mut tokenizer = Tokenizer::new(&file.source);

			let root = match parse_file_root(&mut tokenizer) {
				Ok(root) => root,
				Err(err) => {
					err.print(&file.path, &file.source);
					return Ok(());
				}
			};

			println!("{:#?}\n", root);
			(root, tokenizer.token_count())
		};

		println!("Finished parsing file with {} tokens", token_count);

		roots.push(root);
	}

	Ok(())
}
