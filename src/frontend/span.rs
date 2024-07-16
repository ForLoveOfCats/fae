use std::ops::{Add, AddAssign};

use crate::frontend::tree;

#[derive(Debug, Clone, Copy)]
pub struct Span {
	pub start: usize,
	pub end: usize,
	pub file_index: u32,
	pub line_index: u32,
}

impl Add for Span {
	type Output = Span;

	fn add(self, other: Self) -> Span {
		assert_eq!(self.file_index, other.file_index);
		Span {
			start: self.start.min(other.start),
			end: self.end.max(other.end),
			file_index: self.file_index,
			line_index: self.line_index.min(other.line_index),
		}
	}
}

impl AddAssign for Span {
	fn add_assign(&mut self, rhs: Self) {
		*self = *self + rhs;
	}
}

impl Span {
	pub fn unusable() -> Span {
		Span {
			start: usize::MAX,
			end: usize::MAX,
			file_index: u32::MAX,
			line_index: u32::MAX,
		}
	}

	pub fn get_line_num(self, source: &str) -> usize {
		let mut current_line_num = 1;

		for &byte in &source.as_bytes()[..self.start] {
			if byte == b'\n' {
				current_line_num += 1;
			}
		}

		current_line_num
	}

	pub fn debug_location(self, parsed_files: &[tree::File]) -> DebugLocation {
		let file = &parsed_files[self.file_index as usize];
		let line_start = file.line_starts[self.line_index as usize];
		let offset_in_line = self.start - line_start + 1;

		DebugLocation {
			file_index: self.file_index,
			line: self.line_index + 1,
			offset_in_line: offset_in_line as u32,
		}
	}
}

#[derive(Debug, Clone, Copy)]
pub struct DebugLocation {
	pub file_index: u32,
	pub line: u32,
	pub offset_in_line: u32,
}

impl DebugLocation {
	pub fn unusable() -> DebugLocation {
		DebugLocation {
			file_index: u32::MAX,
			line: u32::MAX,
			offset_in_line: u32::MAX,
		}
	}
}
