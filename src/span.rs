use std::ops::Add;

#[derive(Debug, Copy, Clone)]
pub struct Span {
	pub start: usize,
	pub end: usize,
}

impl Add for Span {
	type Output = Span;

	fn add(self, rhs: Self) -> Span {
		Span { start: self.start.min(rhs.start), end: self.end.max(rhs.end) }
	}
}

impl Span {
	pub fn zero() -> Span {
		Span { start: 0, end: 0 }
	}

	pub fn get_line_num(&self, source: &str) -> usize {
		let mut current_line_num = 1;

		for (index, byte) in source.as_bytes().iter().enumerate() {
			if index >= self.start {
				break;
			} else if matches!(byte, b'\n') {
				current_line_num += 1;
			}
		}

		current_line_num
	}
}
