use std::io::Read;

mod error;
mod file_walker;
mod location;
mod parser;
mod tokenizer;
mod tree;
mod validator;

use file_walker::FileWalker;
use parser::parse_block;
use tokenizer::Tokenizer;
use tree::Tree;
use validator::pre_validate;

fn main() -> Result<(), Box<dyn std::error::Error>> {
	let mut file_walker = FileWalker::new("./example")?;
	let mut next_symbol_id = 0;

	loop {
		let (mut file, path) = match file_walker.next_file() {
			Ok(Some((file, path))) => (file, path),
			Ok(None) => break,
			Err(_) => break, //TODO: Report this error
		};

		let capacity = file.metadata().map(|m| m.len()).unwrap_or(0);
		let mut source = String::with_capacity(capacity as usize);
		if file.read_to_string(&mut source).is_err() {
			break;
		}

		let (tree, token_count) = {
			let mut tokenizer = Tokenizer::new(&source);
			let mut tree = Tree::new();

			if let Err(err) = parse_block(&mut tokenizer, &mut tree, true) {
				err.print(&path, &source);
				return Ok(());
			}

			(tree, tokenizer.token_count())
		};

		println!(
			"Finished parsing file with {} tokens and {} tree children",
			token_count,
			tree.len()
		);

		pre_validate(&tree, &mut next_symbol_id);
	}

	Ok(())
}
