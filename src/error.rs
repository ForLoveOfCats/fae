use std::path::Path;

use unicode_width::UnicodeWidthChar;

use crate::span::Span;

pub const TABULATOR_SIZE: usize = 4;

pub type ParseResult<T> = std::result::Result<T, ParseError>;

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
	UnexpectedEof,

	Expected { expected: String, found: String },

	ExpectedExpression { found: String },

	ExpectedOperator { found: String },

	InvalidIntegerLiteral,

	InvalidFloatLiteral,

	ReservedWord { word: String },

	SubLevelFunction,

	SubLevelStruct,
}

#[derive(Debug, Clone)]
pub struct ParseError {
	pub span: Span,
	pub kind: ParseErrorKind,
}

impl ParseError {
	pub fn print(&self, path: &Path, source: &str) {
		let (line, start, end) = {
			let mut line_start = self.span.start;
			while line_start > 0 {
				if matches!(source.as_bytes()[line_start], b'\r' | b'\n')
					&& line_start != self.span.start
				{
					break;
				}
				line_start -= 1;
			}

			if line_start < self.span.start
				&& matches!(source.as_bytes()[line_start], b'\r' | b'\n')
			{
				line_start += 1;
			}

			let mut line_end = self.span.start;
			while line_end < source.len() && !matches!(source.as_bytes()[line_end], b'\r' | b'\n') {
				line_end += 1;
			}

			(
				&source[line_start..line_end],
				self.span.start - line_start,
				self.span.end - line_start,
			)
		};

		let line_num = self.get_line_num(source);
		let column_start = calc_spaces_from_byte_offset(line, start);
		//TODO: Handle multi-line errors
		print!(
			"Parse error {:?}, line {}, column {}: ",
			path, line_num, column_start
		);

		match &self.kind {
			ParseErrorKind::UnexpectedEof => print!("Unexpected EOF"),

			ParseErrorKind::Expected { expected, found } => {
				print!("Expected {} but found {}", expected, found);
			}

			ParseErrorKind::ExpectedExpression { found } => {
				print!("Expected expression but found {}", found);
			}

			ParseErrorKind::ExpectedOperator { found } => {
				print!("Expected operator but found {}", found);
			}

			ParseErrorKind::InvalidIntegerLiteral => print!("Invalid integer literal"),

			ParseErrorKind::InvalidFloatLiteral => print!("Invalid float literal"),

			ParseErrorKind::ReservedWord { word } => print!("Reserved word {:?}", word),

			ParseErrorKind::SubLevelFunction => {
				print!("Function declaration is only allowed in file root scope");
			}

			ParseErrorKind::SubLevelStruct => {
				print!("Struct declaration is only allowed in file root scope");
			}
		}

		if start != end {
			println!();

			//TODO: Print line num & handle multi-line errors
			let gutter = format!("  {}| ", line_num);
			print!("{}", gutter);
			print_normalized_tabs(line);
			println!();

			let gutter_spacer: String = (0..gutter.len()).map(|_| ' ').collect();
			let whitespace: String = (0..column_start.saturating_sub(1)).map(|_| ' ').collect();
			print!("{}{}", gutter_spacer, whitespace);

			let column_end = calc_spaces_from_byte_offset(line, end - 1);
			for _ in column_start..column_end + 1 {
				print!("^");
			}
		}

		println!();
	}

	fn get_line_num(&self, source: &str) -> usize {
		let mut current_line_num = 1;

		for (index, byte) in source.as_bytes().iter().enumerate() {
			if index >= self.span.start {
				break;
			} else if matches!(byte, b'\n') {
				current_line_num += 1;
			}
		}

		current_line_num
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
					print!(" ");
					index += 1;
				}

				spaces = if diff == 0 {
					spaces + TABULATOR_SIZE
				} else {
					spaces + (TABULATOR_SIZE - diff)
				};
			}

			_ => {
				print!("{}", car);
				spaces += UnicodeWidthChar::width(car).unwrap_or(0);
			}
		}
	}
}
