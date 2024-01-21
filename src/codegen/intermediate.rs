// NOTE: Intermediate values are represented in little-endian, but are displayed in big-endian

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Intermediate8(pub [u8; 1]);

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Intermediate16(pub [u8; 2]);

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Intermediate32(pub [u8; 4]);

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Intermediate64(pub [u8; 8]);

impl std::fmt::Display for Intermediate8 {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "{{{:#04x}}}_8", self.0[0])
	}
}

impl std::fmt::Display for Intermediate16 {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "{{{:#04x} {:#04x}}}_16", self.0[1], self.0[0])
	}
}

impl std::fmt::Display for Intermediate32 {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "{{{:#04x} {:#04x} {:#04x} {:#04x}}}_32", self.0[3], self.0[2], self.0[1], self.0[0])
	}
}

impl std::fmt::Display for Intermediate64 {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(
			f,
			"{{{:#04x} {:#04x} {:#04x} {:#04x} {:#04x} {:#04x} {:#04x} {:#04x}}}_64",
			self.0[7], self.0[6], self.0[5], self.0[4], self.0[3], self.0[2], self.0[1], self.0[0],
		)
	}
}

impl std::convert::From<i8> for Intermediate8 {
	fn from(value: i8) -> Intermediate8 {
		Intermediate8(value.to_le_bytes())
	}
}

impl std::convert::From<u8> for Intermediate8 {
	fn from(value: u8) -> Intermediate8 {
		Intermediate8(value.to_le_bytes())
	}
}

impl std::convert::From<bool> for Intermediate8 {
	fn from(value: bool) -> Intermediate8 {
		Intermediate8([value as u8])
	}
}

impl std::convert::From<i16> for Intermediate16 {
	fn from(value: i16) -> Intermediate16 {
		Intermediate16(value.to_le_bytes())
	}
}

impl std::convert::From<u16> for Intermediate16 {
	fn from(value: u16) -> Intermediate16 {
		Intermediate16(value.to_le_bytes())
	}
}

impl std::convert::From<i32> for Intermediate32 {
	fn from(value: i32) -> Intermediate32 {
		Intermediate32(value.to_le_bytes())
	}
}

impl std::convert::From<u32> for Intermediate32 {
	fn from(value: u32) -> Intermediate32 {
		Intermediate32(value.to_le_bytes())
	}
}

impl std::convert::From<f32> for Intermediate32 {
	fn from(value: f32) -> Intermediate32 {
		Intermediate32(value.to_le_bytes())
	}
}

impl std::convert::From<i64> for Intermediate64 {
	fn from(value: i64) -> Intermediate64 {
		Intermediate64(value.to_le_bytes())
	}
}

impl std::convert::From<u64> for Intermediate64 {
	fn from(value: u64) -> Intermediate64 {
		Intermediate64(value.to_le_bytes())
	}
}

impl std::convert::From<f64> for Intermediate64 {
	fn from(value: f64) -> Intermediate64 {
		Intermediate64(value.to_le_bytes())
	}
}
