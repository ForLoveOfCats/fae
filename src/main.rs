use std::io::Read;
use std::sync::{Arc, Mutex};

mod error;
mod file_walker;
mod location;
mod tokenizer;

use file_walker::FileWalker;

pub const THREAD_COUNT: u64 = 4;

fn main() -> Result<(), Box<dyn std::error::Error>> {
	let file_walker = FileWalker::new("./example")?;
	let file_walker = Arc::new(Mutex::new(file_walker));

	let threads = (0..THREAD_COUNT)
		.into_iter()
		.map(|index| {
			let file_walker = file_walker.clone(); //Clone the Arc
			std::thread::spawn(move || thread_main(index, file_walker))
		})
		.collect::<Vec<_>>();

	for thread in threads {
		let _ = thread.join();
	}

	Ok(())
}

fn thread_main(thread_index: u64, file_walker: Arc<Mutex<FileWalker>>) {
	loop {
		let mut file = {
			let mut file_walker = file_walker.lock().unwrap();
			let file = file_walker.next_file();
			drop(file_walker); //Release lock ASAP

			match file {
				Ok(Some(file)) => file,
				Ok(None) => return,
				Err(_) => return, //TODO: Report this error
			}
		};
		println!("Thread {} got file", thread_index);

		let capacity = file.metadata().map(|m| m.len()).unwrap_or(0);
		let mut source = String::with_capacity(capacity as usize);
		if file.read_to_string(&mut source).is_err() {
			return;
		}

		let mut tokenizer = tokenizer::Tokenizer::new(&source);
		while let Ok(_token) = tokenizer.next() {
			println!("Thread {} read token", thread_index);
		}
	}
}
