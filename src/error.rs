use unicode_width::UnicodeWidthChar;

use crate::file::SourceFile;
use crate::span::Span;

pub const TABULATOR_SIZE: usize = 4;

pub type ParseResult<T> = std::result::Result<T, ()>;

pub trait WriteFmt {
	fn supports_color(&self) -> bool;
	fn write_fmt(&mut self, args: std::fmt::Arguments);
}

impl WriteFmt for String {
	fn supports_color(&self) -> bool {
		false
	}

	fn write_fmt(&mut self, args: std::fmt::Arguments) {
		std::fmt::Write::write_fmt(self, args).unwrap()
	}
}

impl WriteFmt for std::io::Stderr {
	fn supports_color(&self) -> bool {
		true
	}

	fn write_fmt(&mut self, args: std::fmt::Arguments) {
		std::io::Write::write_fmt(self, args).unwrap()
	}
}

struct Colors {
	label_color: &'static str,
	path_color: &'static str,
	message_color: &'static str,
	gutter_color: &'static str,
	underline_color: &'static str,
	reset_color: &'static str,
}

impl Colors {
	fn new(output: &impl WriteFmt) -> Colors {
		use crate::color::*;

		if output.supports_color() {
			Colors {
				label_color: RED,
				path_color: YELLOW,
				message_color: PURPLE,
				gutter_color: YELLOW,
				underline_color: CYAN,
				reset_color: RESET,
			}
		} else {
			Colors {
				label_color: "",
				path_color: "",
				message_color: "",
				gutter_color: "",
				underline_color: "",
				reset_color: "",
			}
		}
	}
}

#[derive(Debug)]
pub struct Messages<'a> {
	messages: Vec<Message>,
	any_errors: bool,
	sources: &'a [SourceFile],
}

impl<'a> Messages<'a> {
	pub fn new(sources: &'a [SourceFile]) -> Self {
		Messages { messages: Vec::new(), any_errors: false, sources }
	}

	pub fn print_messages(&mut self, output: &mut impl WriteFmt, stage: &str) {
		for message in &self.messages {
			message.print(output, self.sources, stage);
			writeln!(output);
		}

		self.messages.clear();
	}

	pub fn reset(&mut self) {
		self.messages.clear();
		self.any_errors = false;
	}

	pub fn any_errors(&self) -> bool {
		self.any_errors
	}

	pub fn any_messages(&self) -> bool {
		!self.messages.is_empty()
	}

	pub fn message(&mut self, message: Message) {
		self.any_errors |= message.kind == MessageKind::Error;
		self.messages.push(message);
	}
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MessageKind {
	Error,
	Warning,
}

impl MessageKind {
	fn name(self) -> &'static str {
		match self {
			MessageKind::Error => "error",
			MessageKind::Warning => "warning",
		}
	}
}

#[derive(Debug)]
pub struct Message {
	kind: MessageKind,
	text: String,
	span: Option<Span>,
	notes: Vec<Note>,
}

impl Message {
	pub fn error(text: String) -> Message {
		Message {
			kind: MessageKind::Error,
			text,
			span: None,
			notes: Vec::new(),
		}
	}

	pub fn warning(text: String) -> Message {
		Message {
			kind: MessageKind::Warning,
			text,
			span: None,
			notes: Vec::new(),
		}
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

	pub fn note_if_some(mut self, span: Option<Span>, text: &str) -> Message {
		if let Some(note) = Note::maybe_new(text, span) {
			self.notes.push(note);
		}
		self
	}

	fn print(&self, output: &mut impl WriteFmt, sources: &[SourceFile], stage: &str) {
		self.print_message(output, sources, stage);

		for note in &self.notes {
			Self::print_file_message(output, sources, None, note.span, &note.text, "Note");
		}
	}

	fn print_message(&self, output: &mut impl WriteFmt, sources: &[SourceFile], stage: &str) {
		if let Some(span) = self.span {
			Self::print_file_message(output, sources, Some(self.kind), span, &self.text, stage);
		} else {
			let kind = self.kind.name();
			let Colors { label_color, message_color, reset_color, .. } = Colors::new(output);
			writeln!(output, "{label_color}{stage} {kind}: {message_color}{}{reset_color}", self.text);
		}
	}

	//This is a big ball of mess
	fn print_file_message(
		output: &mut impl WriteFmt,
		sources: &[SourceFile],
		kind: Option<MessageKind>,
		span: Span,
		text: &str,
		stage: &str,
	) {
		let source_file = &sources[span.file_index];
		let path = &source_file.path;
		let source = &source_file.source;

		let Colors {
			label_color,
			path_color,
			message_color,
			gutter_color,
			underline_color,
			reset_color,
		} = Colors::new(output);

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

		if let Some(kind) = kind {
			let kind = kind.name();
			write!(
				output,
				"{label_color}{stage} {kind}: {path_color}{}{reset_color}, line {}: ",
				path.display(),
				line_num
			);
		} else {
			write!(
				output,
				"{label_color}{stage}: {path_color}{}{reset_color}, line {}: ",
				path.display(),
				line_num
			);
		}
		writeln!(output, "{message_color}{}{reset_color}", text);

		//TODO: Handle multi-line spans
		let gutter = format!("  {}| ", line_num);
		write!(output, "{gutter_color}{}{reset_color}", gutter);
		print_normalized_tabs(output, line);
		writeln!(output);

		let gutter_spacer: String = (0..gutter.len()).map(|_| ' ').collect();
		let whitespace: String = (0..column_start.saturating_sub(1)).map(|_| ' ').collect();
		write!(output, "{}{}{underline_color}", gutter_spacer, whitespace);

		let column_end = calc_spaces_from_byte_offset(line, end - 1);
		for _ in column_start..column_end + 1 {
			write!(output, "^");
		}

		writeln!(output, "{reset_color}");
	}
}

#[macro_export]
macro_rules! error {
	($($arg:tt)*) => {
		$crate::error::Message::error(format!( $($arg)* ))
	}
}

#[macro_export]
macro_rules! warning {
	($($arg:tt)*) => {
		$crate::error::Message::warning(format!( $($arg)* ))
	}
}

#[derive(Debug)]
pub struct Note {
	text: String,
	span: Span,
}

impl Note {
	pub fn new(span: Span, text: String) -> Note {
		Note { text, span }
	}

	pub fn maybe_new(text: &str, span: Option<Span>) -> Option<Note> {
		Some(Note { text: text.to_owned(), span: span? })
	}
}

#[macro_export]
macro_rules! note {
	($span:expr, $($arg:tt)*) => {
		$crate::error::Note::new($span, format!( $($arg)* ))
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

pub fn print_normalized_tabs(output: &mut impl WriteFmt, line: &str) {
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
					write!(output, " ");
					index += 1;
				}

				spaces = if diff == 0 {
					spaces + TABULATOR_SIZE
				} else {
					spaces + (TABULATOR_SIZE - diff)
				};
			}

			_ => {
				write!(output, "{}", car);
				spaces += UnicodeWidthChar::width(car).unwrap_or(0);
			}
		}
	}
}
