pub struct Assembler {
	bytes: Vec<u8>,
	start_offset: usize,
	lengths: Vec<usize>,
}

impl Assembler {
	pub fn new() -> Assembler {
		Assembler { bytes: Vec::new(), start_offset: 0, lengths: Vec::new() }
	}

	fn finalize_instruction(&mut self) {
		self.lengths.push(self.bytes.len() - self.start_offset);
		self.start_offset = self.bytes.len();
	}

	fn rex_prefix<const W: bool, const R: bool, const X: bool, const B: bool>(&mut self) {
		const MARKER: u8 = 0b0100 << 4;
		let prefix = MARKER | (W as u8) << 3 | (R as u8) << 2 | (X as u8) << 1 | (B as u8);
		self.bytes.push(prefix);
	}

	// reg and rm may not be > 0b111 (7)
	fn mod_rm(&mut self, addressing_mode: AddressingMode, reg: u8, rm: u8) {
		let mod_rm = (addressing_mode as u8) << 6 | reg << 3 | rm;
		self.bytes.push(mod_rm);
	}

	fn register32_rm_encoding(&mut self, register: Register32) -> u8 {
		let register = register as u8;
		if register >= 8 {
			self.rex_prefix::<false, false, false, true>();
			register - 8
		} else {
			register
		}
	}

	pub fn add_intermediate32_to_register32(&mut self, intermediate: Intermediate32, destination_register: Register32) {
		if destination_register == Register32::Eax {
			self.bytes.push(0x05);
			self.bytes.extend_from_slice(&intermediate.0);
			self.finalize_instruction();
			return;
		}

		let rm = self.register32_rm_encoding(destination_register);
		self.bytes.push(0x81);
		self.mod_rm(AddressingMode::RegisterDirect, 0, rm);
		self.bytes.extend_from_slice(&intermediate.0);
		self.finalize_instruction();
	}
}

impl std::fmt::Display for Assembler {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let mut offset = 0;
		for &length in &self.lengths {
			let bytes = &self.bytes[offset..offset + length];
			offset += length;

			for (index, byte) in bytes.iter().enumerate() {
				write!(f, "{byte:02x}")?;
				if index + 1 < bytes.len() {
					write!(f, " ")?;
				}
			}

			writeln!(f)?;
		}

		Ok(())
	}
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Intermediate8(pub [u8; 1]);

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Intermediate16(pub [u8; 2]);

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Intermediate32(pub [u8; 4]);

pub enum AddressingMode {
	RegisterDirect = 0b11,
	// todo: more
}

// #[derive(Debug, Copy, Clone, Eq, PartialEq)]
// pub enum Register8 {
// 	//todo add H registers
// 	Al = 0,
// 	Bl = 1,
// 	Cl = 2,
// 	Dl = 3,
// 	Sil = 4,
// 	Dil = 5,
// 	Dpl = 6,
// 	Spl = 7,

// 	R8b = 8,
// 	R9b = 9,
// 	R10b = 10,
// 	R11b = 11,
// 	R12b = 12,
// 	R13b = 13,
// 	R14b = 14,
// 	R15b = 15,
// }

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Register16 {
	Ax = 0,
	Bx = 3,
	Cx = 1,
	Dx = 2,
	Si = 6,
	Di = 7,
	Dp = 5,
	Sp = 4,

	R8w = 8,
	R9w = 9,
	R10w = 10,
	R11w = 11,
	R12w = 12,
	R13w = 13,
	R14w = 14,
	R15w = 15,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Register32 {
	Eax = 0,
	Ebx = 3,
	Ecx = 1,
	Edx = 2,
	Esi = 6,
	Edi = 7,
	Ebp = 5,
	Esp = 4,

	R8d = 8,
	R9d = 9,
	R10d = 10,
	R11d = 11,
	R12d = 12,
	R13d = 13,
	R14d = 14,
	R15d = 15,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Register64 {
	Rax = 0,
	Rbx = 3,
	Rcx = 1,
	Rdx = 2,
	Rsi = 6,
	Rdi = 7,
	Rbp = 5,
	Rsp = 4,

	R8 = 8,
	R9 = 9,
	R10 = 10,
	R11 = 11,
	R12 = 12,
	R13 = 13,
	R14 = 14,
	R15 = 15,
}

// pub enum Intermediate {
// 	Intermediate8([u8; 1]),
// 	Intermediate16([u8; 2]),
// 	Intermediate32([u8; 4]),
// }

// pub enum Instruction {
// 	AddIntermediateToRegister {
// 		intermediate: Intermediate,
// 		destination_register: Register64
// 	}
// }
