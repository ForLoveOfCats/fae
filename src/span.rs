use std::ops::Add;

#[derive(Debug, Copy, Clone)]
pub struct Span {
	pub start: usize,
	pub end: usize,
}

impl Add for Span {
	type Output = Span;

	fn add(self, rhs: Self) -> Span {
		Span {
			start: self.start.min(rhs.start),
			end: self.end.max(rhs.end),
		}
	}
}
