use std::io::Write;

use unicode_width::UnicodeWidthChar;

use crate::frontend::file::SourceFile;
use crate::frontend::span::Span;
use crate::frontend::symbols::Externs;

pub const TABULATOR_SIZE: usize = 4;

pub type ParseResult<T> = std::result::Result<T, ()>;

#[macro_export]
macro_rules! error {
	($($arg:tt)*) => {
		$crate::frontend::error::Message::error(format!( $($arg)* ))
	}
}

#[macro_export]
macro_rules! warning {
	($($arg:tt)*) => {
		$crate::frontend::error::Message::warning(format!( $($arg)* ))
	}
}

#[macro_export]
macro_rules! note {
	($span:expr, $($arg:tt)*) => {
		$crate::frontend::error::Note::new($span, format!( $($arg)* ))
	}
}

pub trait WriteFmt {
	fn supports_color(&self) -> bool;

	fn write_fmt(&mut self, args: std::fmt::Arguments);

	fn alertln(&mut self, prefix: &str, trailing: std::fmt::Arguments);
}

impl WriteFmt for String {
	fn supports_color(&self) -> bool {
		false
	}

	fn write_fmt(&mut self, args: std::fmt::Arguments) {
		std::fmt::Write::write_fmt(self, args).unwrap()
	}

	fn alertln(&mut self, _: &str, _: std::fmt::Arguments) {}
}

pub struct StderrOutput<'a> {
	pub supports_color: bool,
	pub stderr: &'a mut std::io::Stderr,
}

impl<'a> WriteFmt for StderrOutput<'a> {
	fn supports_color(&self) -> bool {
		self.supports_color
	}

	fn write_fmt(&mut self, args: std::fmt::Arguments) {
		std::io::Write::write_fmt(&mut self.stderr, args).unwrap()
	}

	fn alertln(&mut self, prefix: &str, trailing: std::fmt::Arguments) {
		use crate::color::{BOLD_GREEN, RESET};
		if self.supports_color {
			writeln!(self.stderr, "{BOLD_GREEN}{prefix}{RESET} {trailing}").unwrap();
		} else {
			writeln!(self.stderr, "{prefix}: {trailing}").unwrap();
		}
	}
}

struct Colors {
	error_label_color: &'static str,
	warning_label_color: &'static str,
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
				error_label_color: BOLD_RED,
				warning_label_color: BOLD_YELLOW,
				path_color: YELLOW,
				message_color: PURPLE,
				gutter_color: YELLOW,
				underline_color: CYAN,
				reset_color: RESET,
			}
		} else {
			Colors {
				error_label_color: "",
				warning_label_color: "",
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
	module_path: &'a [String],
}

impl<'a> Messages<'a> {
	pub fn new(module_path: &'a [String]) -> Self {
		Messages { messages: Vec::new(), any_errors: false, module_path }
	}

	pub fn module_path(&self) -> &'a [String] {
		self.module_path
	}

	pub fn any_messages(&self) -> bool {
		!self.messages.is_empty()
	}

	pub fn message(&mut self, message: Message) {
		self.any_errors |= message.kind == MessageKind::Error;
		self.messages.push(message);
	}
}

#[derive(Debug)]
pub struct RootMessages<'a> {
	file_messages: Vec<Messages<'a>>,
	missing_main: bool,
	any_errors: bool,
	sources: &'a [SourceFile],
}

impl<'a> RootMessages<'a> {
	pub fn new(sources: &'a [SourceFile]) -> Self {
		RootMessages {
			file_messages: Vec::new(),
			missing_main: false,
			any_errors: false,
			sources,
		}
	}

	pub fn mark_main_missing(&mut self) {
		self.missing_main = true;
		self.any_errors = true;
	}

	pub fn print_messages(&mut self, output: &mut impl WriteFmt, stage: &str, externs: &Externs, in_compiler_test: bool) -> bool {
		// Very important that this be a stable sort as different passes in the validator
		// can emit separate message packages for the same file and we need to maintain
		// their relative order
		self.file_messages.sort_by(|a, b| a.module_path.cmp(&b.module_path));

		let mut print_newline_above = !in_compiler_test;

		for file_messages in &self.file_messages {
			for message in &file_messages.messages {
				if print_newline_above {
					writeln!(output);
				}
				message.print(output, self.sources, stage);
				print_newline_above = true;
			}
		}

		let mut has_duplicates: Vec<_> = externs.externs.iter().filter(|(_, externs)| externs.len() > 1).collect();
		has_duplicates.sort_by_key(|(name, _)| *name);

		for duplicates in &has_duplicates {
			if print_newline_above {
				writeln!(output);
			}

			let mut externs = duplicates.1.clone();
			externs.sort_by_key(|e| e.module_path);

			let mut error = error!("Duplicate declarations for extern `{}`", duplicates.0);
			for duplicate in externs {
				error = error.note(note!(duplicate.span, "Declaration here"));
			}

			error.print(output, self.sources, stage);
			print_newline_above = true;
		}

		if self.missing_main {
			if print_newline_above {
				writeln!(output);
			}
			let error = error!("Project has no main function, is it missing or in the wrong file for project name?");
			error.print(output, self.sources, stage);
		}

		!has_duplicates.is_empty()
	}

	pub fn reset(&mut self) {
		self.file_messages.clear();
		self.missing_main = false;
		self.any_errors = false;
	}

	pub fn any_errors(&self) -> bool {
		self.any_errors
	}

	pub fn any_messages(&self) -> bool {
		!self.file_messages.is_empty() || self.missing_main
	}

	pub fn add_messages_if_any(&mut self, messages: Messages<'a>) {
		if messages.any_messages() {
			self.any_errors |= messages.any_errors;
			self.file_messages.push(messages);
		}
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

	pub fn print(&self, output: &mut impl WriteFmt, sources: &[SourceFile], stage: &str) {
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
			let Colors {
				error_label_color,
				warning_label_color,
				message_color,
				reset_color,
				..
			} = Colors::new(output);

			match self.kind {
				MessageKind::Error => {
					writeln!(output, "{error_label_color}{stage} {kind}: {message_color}{}{reset_color}", self.text)
				}

				MessageKind::Warning => {
					writeln!(output, "{warning_label_color}{stage} {kind}: {message_color}{}{reset_color}", self.text)
				}
			}
		}
	}

	// This is a big ball of mess
	fn print_file_message(
		output: &mut impl WriteFmt,
		sources: &[SourceFile],
		kind: Option<MessageKind>,
		span: Span,
		text: &str,
		stage: &str,
	) {
		let source_file = &sources[span.file_index as usize];
		let path = &source_file.path;
		let source = &source_file.source;

		let Colors {
			error_label_color,
			warning_label_color,
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

		let line_num = span.line_index + 1;
		let column_start = calc_spaces_from_byte_offset(line, start);

		if let Some(kind) = kind {
			let kind_name = kind.name();
			match kind {
				MessageKind::Error => write!(
					output,
					"{error_label_color}{stage} {kind_name}: {path_color}{}{reset_color}, line {}: ",
					path.display(),
					line_num
				),

				MessageKind::Warning => write!(
					output,
					"{warning_label_color}{stage} {kind_name}: {path_color}{}{reset_color}, line {}: ",
					path.display(),
					line_num
				),
			}
		} else {
			write!(
				output,
				"{warning_label_color}{stage}: {path_color}{}{reset_color}, line {}: ",
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

		let column_end = calc_spaces_from_byte_offset(line, end.saturating_sub(1));
		for _ in column_start..column_end + 1 {
			write!(output, "^");
		}

		writeln!(output, "{reset_color}");
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
