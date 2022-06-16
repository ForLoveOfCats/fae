mod error;
mod file;
mod ice;
mod parser;
mod span;
mod tokenizer;
mod tree;

use file::load_all_files;
use parser::parse_block;
use tokenizer::Tokenizer;

fn main() -> Result<(), Box<dyn std::error::Error>> {
	let files = load_all_files("./example")?;

	let mut roots = Vec::new();

	for file in &files {
		let (root_expression, token_count) = {
			let mut tokenizer = Tokenizer::new(&file.source);

			let root_expression = match parse_block(&mut tokenizer, true) {
				Ok(root_expression) => root_expression,
				Err(err) => {
					err.print(&file.path, &file.source);
					return Ok(());
				}
			};

			(root_expression, tokenizer.token_count())
		};

		println!("Finished parsing file with {} tokens", token_count,);

		roots.push(root_expression);
	}

	Ok(())
}
