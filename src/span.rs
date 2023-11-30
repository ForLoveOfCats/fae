use std::ops::Add;

#[derive(Debug, Copy, Clone)]
pub struct Span {
	pub start: usize,
	pub end: usize,
	pub file_index: usize,
}

impl Add for Span {
	type Output = Span;

	fn add(self, rhs: Self) -> Span {
		assert_eq!(self.file_index, rhs.file_index);
		Span {
			start: self.start.min(rhs.start),
			end: self.end.max(rhs.end),
			file_index: self.file_index,
		}
	}
}

impl Span {
	pub fn unusable() -> Span {
		Span { start: usize::MAX, end: usize::MAX, file_index: usize::MAX }
	}

	pub fn get_line_num(&self, source: &str) -> usize {
		let mut current_line_num = 1;

		for &byte in &source.as_bytes()[..self.start] {
			if byte == b'\n' {
				current_line_num += 1;
			}
		}

		current_line_num
	}
}
