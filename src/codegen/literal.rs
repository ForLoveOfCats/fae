// NOTE: Literal values are represented in little-endian, but are displayed in big-endian

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Literal8(pub [u8; 1]);

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Literal16(pub [u8; 2]);

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Literal32(pub [u8; 4]);

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Literal64(pub [u8; 8]);

impl std::fmt::Display for Literal8 {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "{{{:#04x}}}_8", self.0[0])
	}
}

impl std::fmt::Display for Literal16 {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "{{{:#04x} {:#04x}}}_16", self.0[1], self.0[0])
	}
}

impl std::fmt::Display for Literal32 {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "{{{:#04x} {:#04x} {:#04x} {:#04x}}}_32", self.0[3], self.0[2], self.0[1], self.0[0])
	}
}

impl std::fmt::Display for Literal64 {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(
			f,
			"{{{:#04x} {:#04x} {:#04x} {:#04x} {:#04x} {:#04x} {:#04x} {:#04x}}}_64",
			self.0[7], self.0[6], self.0[5], self.0[4], self.0[3], self.0[2], self.0[1], self.0[0],
		)
	}
}

impl std::convert::From<i8> for Literal8 {
	fn from(value: i8) -> Literal8 {
		Literal8(value.to_le_bytes())
	}
}

impl std::convert::From<u8> for Literal8 {
	fn from(value: u8) -> Literal8 {
		Literal8(value.to_le_bytes())
	}
}

impl std::convert::From<bool> for Literal8 {
	fn from(value: bool) -> Literal8 {
		Literal8([value as u8])
	}
}

impl std::convert::From<i16> for Literal16 {
	fn from(value: i16) -> Literal16 {
		Literal16(value.to_le_bytes())
	}
}

impl std::convert::From<u16> for Literal16 {
	fn from(value: u16) -> Literal16 {
		Literal16(value.to_le_bytes())
	}
}

impl std::convert::From<i32> for Literal32 {
	fn from(value: i32) -> Literal32 {
		Literal32(value.to_le_bytes())
	}
}

impl std::convert::From<u32> for Literal32 {
	fn from(value: u32) -> Literal32 {
		Literal32(value.to_le_bytes())
	}
}

impl std::convert::From<f32> for Literal32 {
	fn from(value: f32) -> Literal32 {
		Literal32(value.to_le_bytes())
	}
}

impl std::convert::From<i64> for Literal64 {
	fn from(value: i64) -> Literal64 {
		Literal64(value.to_le_bytes())
	}
}

impl std::convert::From<u64> for Literal64 {
	fn from(value: u64) -> Literal64 {
		Literal64(value.to_le_bytes())
	}
}

impl std::convert::From<f64> for Literal64 {
	fn from(value: f64) -> Literal64 {
		Literal64(value.to_le_bytes())
	}
}
