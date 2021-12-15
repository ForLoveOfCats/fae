use std::ops::Add;

#[derive(Debug, Copy, Clone)]
pub struct SourceLocation {
	pub start: usize,
	pub end: usize,
}

impl Add for SourceLocation {
	type Output = SourceLocation;

	fn add(self, rhs: Self) -> SourceLocation {
		SourceLocation {
			start: self.start.min(rhs.start),
			end: self.end.max(rhs.end),
		}
	}
}
