use std::io::Read;
use std::sync::{Arc, Mutex};

mod file_list;
mod location;
mod error;
mod tokenizer;

use file_list::FileList;

pub const THREAD_COUNT: u64 = 4;

fn main() -> Result<(), Box<dyn std::error::Error>> {
	let file_list = FileList::discover_files("./example")?;
	let file_list = Arc::new(Mutex::new(file_list));

	let threads = (0..THREAD_COUNT)
		.into_iter()
		.map(|index| {
			let file_list = file_list.clone(); //Clone the Arc
			std::thread::spawn(move || thread_main(index, file_list))
		})
		.collect::<Vec<_>>();

	for thread in threads {
		let _ = thread.join();
	}

	Ok(())
}

fn thread_main(thread_index: u64, file_list: Arc<Mutex<FileList>>) {
	loop {
		let mut file = {
			let mut file_list = file_list.lock().unwrap();
			let file = file_list.take_file();

			match file {
				Some(file) => file,
				None => return,
			}
		};
		println!("thread {} got file", thread_index);

		let capacity = file.metadata().map(|m| m.len()).unwrap_or(0);
		let mut source = String::with_capacity(capacity as usize);
		if file.read_to_string(&mut source).is_err() {
			return;
		}

		let mut tokenizer = tokenizer::Tokenizer::new(&source);
		while let Ok(token) = tokenizer.next() {
			println!("{}: {:?}", thread_index, token);
		}
	}
}
