use std::path::Path;

use unicode_width::UnicodeWidthChar;

use crate::span::Span;

pub const TABULATOR_SIZE: usize = 4;

pub type ParseResult<T> = std::result::Result<T, ()>;

pub struct Messages {
	errors: Vec<Message>,
	//NOTE: `self.errors` will be renamed to contain errors and warnings
	//where each message knows what kind it is
	any_errors: bool,
}

impl Messages {
	pub fn new() -> Messages {
		Messages {
			errors: Vec::new(),
			any_errors: false,
		}
	}

	pub fn remove_errors(&mut self) {
		self.errors.clear();
	}

	pub fn reset(&mut self) {
		self.remove_errors();
		self.any_errors = false;
	}

	pub fn any_errors(&self) -> bool {
		self.any_errors
	}

	pub fn error(&mut self, message: Message) {
		self.errors.push(message);
		self.any_errors = true;
	}

	pub fn errors(&self) -> &[Message] {
		&self.errors
	}
}

pub struct Message {
	text: String,
	span: Option<Span>,
	notes: Vec<Annotation>,
}

impl Message {
	pub fn new(text: String) -> Message {
		Message {
			text,
			span: None,
			notes: Vec::new(),
		}
	}

	pub fn span(mut self, span: Span) -> Message {
		self.span = Some(span);
		self
	}

	pub fn note(mut self, note: Annotation) -> Message {
		self.notes.push(note);
		self
	}

	pub fn print(&self, path: &Path, source: &str, message_kind: &str) {
		if let Some(span) = self.span {
			let (line, start, end) = {
				let mut line_start = span.start;
				while line_start > 0 {
					if matches!(source.as_bytes()[line_start], b'\r' | b'\n')
						&& line_start != span.start
					{
						break;
					}
					line_start -= 1;
				}

				if line_start < span.start && matches!(source.as_bytes()[line_start], b'\r' | b'\n')
				{
					line_start += 1;
				}

				let mut line_end = span.start;
				while line_end < source.len()
					&& !matches!(source.as_bytes()[line_end], b'\r' | b'\n')
				{
					line_end += 1;
				}

				(
					&source[line_start..line_end],
					span.start - line_start,
					span.end - line_start,
				)
			};

			let line_num = span.get_line_num(source);
			let column_start = calc_spaces_from_byte_offset(line, start);

			//TODO: Handle multi-line spans
			eprint!(
				"{message_kind} {:?}, line {}, column {}: ",
				path, line_num, column_start
			);
			eprint!("{}", self.text);

			if start != end {
				eprintln!();

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
			}

			eprintln!();
		} else {
			eprint!("{message_kind} {:?}: ", path);
			eprintln!("{}", self.text);
		}
	}
}

#[macro_export]
macro_rules! message {
	($($arg:tt)*) => {
		$crate::error::Message::new(format!( $($arg)* ))
	}
}

pub struct Annotation {
	span: Span,
	text: String,
}

impl Annotation {
	pub fn new(span: Span, text: String) -> Annotation {
		Annotation { span, text }
	}
}

#[macro_export]
macro_rules! note {
	($span:expr, $($arg:tt)*) => {
		$crate::error::Annotation::new($span, format!( $($arg)* ))
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
