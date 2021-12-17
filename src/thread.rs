use std::io::Read;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

use crate::file_walker::FileWalker;
use crate::parser::parse_block;
use crate::tokenizer::Tokenizer;
use crate::tree::Tree;
use crate::validator::pre_validate;
use crate::THREAD_COUNT;

pub struct ThreadContext {
	pub index: u64,
	pub file_walker: Arc<Mutex<FileWalker>>,
	pub pre_validated_thread_count: Arc<AtomicU64>,
	pub next_symbol_index: Arc<AtomicU64>,
}

pub fn thread_main(ctx: ThreadContext) {
	loop {
		let (mut file, path) = {
			let mut file_walker = ctx.file_walker.lock().unwrap();
			let file_info = file_walker.next_file();
			drop(file_walker); //Release lock ASAP

			match file_info {
				Ok(Some((file, path))) => (file, path),
				Ok(None) => break,
				Err(_) => return, //TODO: Report this error
			}
		};
		println!("Thread {} got file", ctx.index);

		let capacity = file.metadata().map(|m| m.len()).unwrap_or(0);
		let mut source = String::with_capacity(capacity as usize);
		if file.read_to_string(&mut source).is_err() {
			return;
		}

		let (tree, token_count) = {
			let mut tokenizer = Tokenizer::new(&source);
			let mut tree = Tree::new();

			if let Err(err) = parse_block(&mut tokenizer, &mut tree, true) {
				//TODO: Block other threads from reporting errors during/after first error print
				err.print(&path, &source);
				return;
			}

			(tree, tokenizer.token_count())
		};

		println!(
			"Thread {} finished with {} tokens and {} tree children",
			ctx.index,
			token_count,
			tree.len()
		);

		pre_validate(&tree, ctx.next_symbol_index.as_ref());
	}

	ctx.pre_validated_thread_count
		.fetch_add(1, Ordering::SeqCst);

	//Wait for all threads to finish parsing and pre-validating
	while ctx.pre_validated_thread_count.load(Ordering::Relaxed) < THREAD_COUNT {}
}
