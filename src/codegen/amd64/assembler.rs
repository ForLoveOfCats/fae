use crate::codegen::intermediate::Intermediate32;

pub struct Assembler<'a> {
	bytes: &'a mut Vec<u8>,
	start_offset: usize,
	lengths: Vec<usize>,
}

impl<'a> Assembler<'a> {
	pub fn new(bytes: &'a mut Vec<u8>) -> Assembler<'a> {
		Assembler { bytes, start_offset: 0, lengths: Vec::new() }
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

	fn register_32_rex_prefix(&mut self, register: Register32) {
		if register as u8 >= 8 {
			self.rex_prefix::<false, false, false, true>();
		}
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

	// make sure to add mod_rm before instruction if using register >= r8w
	fn plus_rd(&mut self, register: Register32) {
		let mut value = register as u8;
		if value >= 8 {
			value -= 8;
		}

		*self.bytes.last_mut().unwrap() += value;
	}

	pub fn move_intermediate32_to_register32(
		&mut self,
		intermediate: impl Into<Intermediate32>,
		destination_register: Register32,
	) {
		self.register_32_rex_prefix(destination_register);
		self.bytes.push(0xb8);
		self.plus_rd(destination_register);
		self.bytes.extend_from_slice(&intermediate.into().0);
		self.finalize_instruction();
	}

	pub fn add_intermediate32_to_register32(
		&mut self,
		intermediate: impl Into<Intermediate32>,
		destination_register: Register32,
	) {
		let intermediate = intermediate.into();
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

	pub fn syscall(&mut self) {
		self.bytes.push(0x0f);
		self.bytes.push(0x05);
		self.finalize_instruction();
	}

	#[cfg(test)]
	pub fn expect(&self, expected: &str) {
		use crate::color::*;
		use crate::error::WriteFmt;

		let mut actual = String::new();
		write!(&mut actual, "{self}");

		let mut actual = actual.lines();
		let mut expected = expected.lines();

		let mut line_number = 0;
		loop {
			let actual = actual.next();
			let expected = expected.next();
			line_number += 1;

			let (Some(actual), Some(expected)) = (actual, expected) else {
				if let Some(actual) = actual {
					println!("{RED}Actual instructions has more lines than expected{RESET}");
					println!("{line_number:2}| Next actual line: {actual}");
				} else if let Some(expected) = expected {
					println!("{RED}Expected instructions has more lines than actual{RESET}");
					println!("{line_number:2}| Next expected line: {expected}");
				} else {
					break;
				}

				println!();
				println!("Full actual contents:");
				println!("{self}");

				std::process::exit(-1);
			};

			let mismatch = actual != expected;
			if mismatch {
				println!("{RED}Actual did not match expected:{RESET}");
			}

			println!("{line_number:2}| Expected: {expected}");
			println!("{line_number:2}| Actual:   {actual}");
			println!();

			if mismatch {
				std::process::exit(-1);
			}
		}
	}

	#[cfg(test)]
	#[allow(unused)]
	pub fn dump_test_case(&self) {
		use crate::error::WriteFmt;

		let mut actual = String::new();
		write!(&mut actual, "{self}");

		println!("assembler.expect(multi_line_string!(");

		for (index, line) in actual.lines().enumerate() {
			println!("    {line:?} // {}", index + 1);
		}

		println!("));");

		// Don't have to remember to pass `-- --nocapture`
		std::process::exit(-1);
	}
}

impl<'a> std::fmt::Display for Assembler<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
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
