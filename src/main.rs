use std::sync::atomic::{AtomicBool, AtomicU64};
use std::sync::{Arc, Mutex};

mod error;
mod file_walker;
mod location;
mod parser;
mod thread;
mod tokenizer;
mod tree;
mod validator;

use file_walker::FileWalker;
use thread::ThreadContext;

pub const THREAD_COUNT: u64 = 4;

fn main() -> Result<(), Box<dyn std::error::Error>> {
	let file_walker = Arc::new(Mutex::new(FileWalker::new("./example")?));
	let pre_validated_thread_count = Arc::new(AtomicU64::new(0));
	let next_symbol_index = Arc::new(AtomicU64::new(0));

	let threads = (0..THREAD_COUNT)
		.into_iter()
		.map(|index| {
			let ctx = ThreadContext {
				index,
				file_walker: file_walker.clone(),
				pre_validated_thread_count: pre_validated_thread_count.clone(),
				next_symbol_index: next_symbol_index.clone(),
			};
			std::thread::spawn(move || thread::thread_main(ctx))
		})
		.collect::<Vec<_>>();

	for thread in threads {
		let _ = thread.join();
	}

	Ok(())
}
