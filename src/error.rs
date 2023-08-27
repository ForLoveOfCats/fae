use unicode_width::UnicodeWidthChar;

use crate::{file::SourceFile, span::Span};

pub const TABULATOR_SIZE: usize = 4;

pub type ParseResult<T> = std::result::Result<T, ()>;

#[derive(Debug)]
pub struct Messages<'a> {
	errors: Vec<Message>,
	//NOTE: `self.errors` will be renamed to contain errors and warnings
	//where each message knows what kind it is
	any_errors: bool,
	sources: &'a [SourceFile],
	current_file_index: usize,
}

impl<'a> Messages<'a> {
	pub fn new(sources: &'a [SourceFile]) -> Self {
		Messages {
			errors: Vec::new(),
			any_errors: false,
			sources,
			current_file_index: usize::MAX,
		}
	}

	pub fn current_file_index(&self) -> usize {
		self.current_file_index
	}

	pub fn set_current_file_index(&mut self, file_index: usize) {
		self.current_file_index = file_index;
	}

	pub fn print_errors(&mut self, prefix: &str) {
		for error in &self.errors {
			error.print(&self.sources, prefix);
			eprintln!();
		}

		self.errors.clear();
	}

	pub fn reset(&mut self) {
		self.errors.clear();
		self.any_errors = false;
		self.current_file_index = usize::MAX;
	}

	pub fn any_errors(&self) -> bool {
		self.any_errors
	}

	pub fn error(&mut self, mut message: Message) {
		if message.span.is_some() {
			message.file_index = Some(self.current_file_index);
		}

		self.errors.push(message);
		self.any_errors = true;
	}
}

#[derive(Debug)]
pub struct Message {
	file_index: Option<usize>,
	text: String,
	span: Option<Span>,
	notes: Vec<Note>,
}

impl Message {
	pub fn new(text: String) -> Message {
		Message { file_index: None, text, span: None, notes: Vec::new() }
	}

	pub fn span(mut self, span: Span) -> Message {
		self.span = Some(span);
		self
	}

	pub fn span_if_some(mut self, span: Option<Span>) -> Message {
		self.span = self.span.or(span);
		self
	}

	pub fn note(mut self, note: Note) -> Message {
		self.notes.push(note);
		self
	}

	pub fn note_if_some(mut self, span: Option<Span>, file_index: Option<usize>, text: &str) -> Message {
		if let Some(note) = Note::maybe_new(text, span, file_index) {
			self.notes.push(note);
		}
		self
	}

	fn print(&self, sources: &[SourceFile], prefix: &str) {
		Self::print_message(sources, self.file_index, self.span, &self.text, prefix);

		for note in &self.notes {
			Self::print_file_message(sources, note.file_index, note.span, &note.text, "Note");
		}
	}

	fn print_message(sources: &[SourceFile], file_index: Option<usize>, span: Option<Span>, text: &str, prefix: &str) {
		if let Some(span) = span {
			let file_index = file_index.expect("Cannot have a span without a file index");
			Self::print_file_message(sources, file_index, span, text, prefix);
		} else {
			eprintln!("{prefix}: {text}");
		}
	}

	//This is a big ball of mess
	fn print_file_message(sources: &[SourceFile], file_index: usize, span: Span, text: &str, prefix: &str) {
		let source_file = &sources[file_index];
		let path = &source_file.path;
		let source = &source_file.source;

		let (line, start, end) = {
			let mut line_start = span.start;
			while line_start > 0 {
				if matches!(source.as_bytes()[line_start], b'\r' | b'\n') && line_start != span.start {
					break;
				}
				line_start -= 1;
			}

			if line_start < span.start && matches!(source.as_bytes()[line_start], b'\r' | b'\n') {
				line_start += 1;
			}

			let mut line_end = span.start;
			while line_end < source.len() && !matches!(source.as_bytes()[line_end], b'\r' | b'\n') {
				line_end += 1;
			}

			(&source[line_start..line_end], span.start - line_start, span.end - line_start)
		};

		let line_num = span.get_line_num(source);
		let column_start = calc_spaces_from_byte_offset(line, start);

		eprint!("{prefix} {:?}, line {}, column {}: ", path, line_num, column_start);
		eprintln!("{}", text);

		assert_ne!(start, end);

		//TODO: Handle multi-line spans
		let gutter = format!("  {}| ", line_num);
		eprint!("{}", gutter);
		print_normalized_tabs(line);
		eprintln!();

		let gutter_spacer: String = (0..gutter.len()).map(|_| ' ').collect();
		let whitespace: String = (0..column_start.saturating_sub(1)).map(|_| ' ').collect();
		eprint!("{}{}", gutter_spacer, whitespace);

		let column_end = calc_spaces_from_byte_offset(line, end - 1);
		for _ in column_start..column_end + 1 {
			eprint!("^");
		}

		eprintln!();
	}
}

#[macro_export]
macro_rules! message {
	($($arg:tt)*) => {
		$crate::error::Message::new(format!( $($arg)* ))
	}
}

#[derive(Debug)]
pub struct Note {
	text: String,
	span: Span,
	file_index: usize,
}

impl Note {
	pub fn new(span: Span, file_index: usize, text: String) -> Note {
		Note { text, span, file_index }
	}

	pub fn maybe_new(text: &str, span: Option<Span>, file_index: Option<usize>) -> Option<Note> {
		Some(Note { text: text.to_owned(), span: span?, file_index: file_index? })
	}
}

#[macro_export]
macro_rules! note {
	($span:expr, $file_index:expr, $($arg:tt)*) => {
		$crate::error::Note::new($span, $file_index, format!( $($arg)* ))
	}
}

pub fn calc_spaces_from_byte_offset(line: &str, offset: usize) -> usize {
	if offset == 0 {
		return 1;
	}

	let mut spaces = 1_usize;
	for (current_offset, car) in line.char_indices() {
		if current_offset >= offset {
			return spaces;
		}

		match car {
			'\t' => {
				let diff = (spaces - 1) % TABULATOR_SIZE;
				spaces = if diff == 0 {
					spaces + TABULATOR_SIZE
				} else {
					spaces + (TABULATOR_SIZE - diff)
				};
			}

			_ => {
				spaces += UnicodeWidthChar::width(car).unwrap_or(0);
			}
		}
	}

	spaces
}

pub fn print_normalized_tabs(line: &str) {
	let mut spaces = 1_usize;
	for car in line.chars() {
		match car {
			'\r' | '\n' => {
				spaces = 1;
			}

			'\t' => {
				let diff = (spaces - 1) % TABULATOR_SIZE;

				let mut index = 0_usize;
				while index < TABULATOR_SIZE - diff {
					eprint!(" ");
					index += 1;
				}

				spaces = if diff == 0 {
					spaces + TABULATOR_SIZE
				} else {
					spaces + (TABULATOR_SIZE - diff)
				};
			}

			_ => {
				eprint!("{}", car);
				spaces += UnicodeWidthChar::width(car).unwrap_or(0);
			}
		}
	}
}
