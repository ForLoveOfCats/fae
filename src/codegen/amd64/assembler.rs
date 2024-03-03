use crate::codegen::literal::Literal32;

enum PrintEntry {
	Note { note: &'static str },
	Instruction { length: usize, label: String },
}

pub struct Assembler<'a> {
	bytes: &'a mut Vec<u8>,
	initial_bytes_len: usize,

	current_instruction_start_offset: usize,
	print_sequence: Vec<PrintEntry>,
}

impl<'a> Assembler<'a> {
	pub fn new(bytes: &'a mut Vec<u8>) -> Assembler<'a> {
		let initial_bytes_len = bytes.len();
		Assembler {
			bytes,
			initial_bytes_len,
			current_instruction_start_offset: initial_bytes_len,
			print_sequence: Vec::new(),
		}
	}

	pub fn note(&mut self, note: &'static str) {
		self.print_sequence.push(PrintEntry::Note { note });
	}

	fn finalize_instruction(&mut self, label: impl FnOnce() -> String) {
		let length = self.bytes.len() - self.current_instruction_start_offset;
		let label = label();

		self.print_sequence.push(PrintEntry::Instruction { length, label });
		self.current_instruction_start_offset = self.bytes.len();
	}

	fn rex_prefix<const W: bool, const R: bool, const X: bool, const B: bool>(&mut self) {
		const MARKER: u8 = 0b0100 << 4;
		let prefix = MARKER | (W as u8) << 3 | (R as u8) << 2 | (X as u8) << 1 | (B as u8);
		self.bytes.push(prefix);
	}

	fn runtime_rex_prefix(&mut self, w: bool, r: bool, x: bool, b: bool) {
		const MARKER: u8 = 0b0100 << 4;
		let prefix = MARKER | (w as u8) << 3 | (r as u8) << 2 | (x as u8) << 1 | (b as u8);
		self.bytes.push(prefix);
	}

	fn register32_rex_prefix(&mut self, register: Register32) {
		if register as u8 >= 8 {
			self.rex_prefix::<false, false, false, true>();
		}
	}

	fn register64_rex_prefix(&mut self, register: Register64) {
		if register as u8 >= 8 {
			self.rex_prefix::<true, false, false, true>();
		}
	}

	// reg and rm may not be > 0b111 (7)
	fn mod_rm(&mut self, addressing_mode: AddressingMode, reg: u8, rm: u8) {
		let mod_rm = (addressing_mode as u8) << 6 | reg << 3 | rm;
		self.bytes.push(mod_rm);
	}

	// Call before pushing any other opcode bytes
	fn register32_rm_encoding(&mut self, destination: Register32) -> u8 {
		let destination = destination as u8;

		if destination >= 8 {
			self.rex_prefix::<false, false, false, true>();
			destination - 8
		} else {
			destination
		}
	}

	// Call before pushing any other opcode bytes
	fn register32_reg_rm_encoding(&mut self, source: Register32, destination: Register32) -> (u8, u8) {
		let mut source = source as u8;
		let mut destination = destination as u8;

		let mut r = false;
		let mut b = false;
		if source >= 8 {
			r = true;
			source -= 8;
		}
		if destination >= 8 {
			b = true;
			destination -= 8;
		}

		self.runtime_rex_prefix(false, r, false, b);
		(source, destination)
	}

	// Call before pushing any other opcode bytes
	fn register64_rm_encoding(&mut self, destination: Register64) -> u8 {
		let destination = destination as u8;
		if destination >= 8 {
			self.rex_prefix::<false, false, false, true>();
			destination - 8
		} else {
			destination
		}
	}

	// Call before pushing any other opcode bytes
	fn register64_reg_rm_encoding(&mut self, source: Register64, destination: Register64) -> (u8, u8) {
		let mut source = source as u8;
		let mut destination = destination as u8;

		let mut r = false;
		let mut b = false;
		if source >= 8 {
			r = true;
			source -= 8;
		}
		if destination >= 8 {
			b = true;
			destination -= 8;
		}

		self.runtime_rex_prefix(true, r, false, b);
		(source, destination)
	}

	fn plus_rd(&mut self, register: Register32) {
		let mut value = register as u8;
		if value >= 8 {
			value -= 8;
		}

		*self.bytes.last_mut().unwrap() += value;
	}

	fn plus_rq(&mut self, register: Register64) {
		let mut value = register as u8;
		if value >= 8 {
			value -= 8;
		}

		*self.bytes.last_mut().unwrap() += value;
	}

	pub fn push_register64(&mut self, source_register: Register64) {
		self.register64_rm_encoding(source_register);
		self.bytes.push(0x50);
		self.plus_rq(source_register);
		self.finalize_instruction(|| format!("push_register64 {source_register}"));
	}

	pub fn move_literal32_to_register32(&mut self, literal: impl Into<Literal32>, destination_register: Register32) {
		let literal = literal.into();
		self.register32_rex_prefix(destination_register);
		self.bytes.push(0xb8);
		self.plus_rd(destination_register);
		self.bytes.extend_from_slice(&literal.0);
		self.finalize_instruction(|| format!("move_literal32_to_register32 {literal}, {destination_register}"));
	}

	pub fn add_literal32_to_register32(&mut self, literal: impl Into<Literal32>, destination_register: Register32) {
		let literal = literal.into();
		if destination_register == Register32::Eax {
			self.bytes.push(0x05);
			self.bytes.extend_from_slice(&literal.0);
			self.finalize_instruction(|| format!("add_literal32_to_register32 {literal}, {destination_register}"));
			return;
		}

		let rm = self.register32_rm_encoding(destination_register);
		self.bytes.push(0x81);
		self.mod_rm(AddressingMode::RegisterDirect, 0, rm);
		self.bytes.extend_from_slice(&literal.0);
		self.finalize_instruction(|| format!("add_literal32_to_register32 {literal}, {destination_register}"));
	}

	pub fn sub_literal32_to_register64(&mut self, literal: impl Into<Literal32>, destination_register: Register64) {
		let literal = literal.into();
		if destination_register == Register64::Rax {
			self.rex_prefix::<true, false, false, false>();
			self.bytes.push(0x2d);
			self.bytes.extend_from_slice(&literal.0);
			self.finalize_instruction(|| format!("sub_literal32_to_register64 {literal}, {destination_register}"));
			return;
		}

		let mut rm = destination_register as u8;
		if rm >= 8 {
			self.rex_prefix::<true, false, false, true>();
			rm -= 8;
		} else {
			self.rex_prefix::<true, false, false, false>();
		};

		self.bytes.push(0x81);
		self.mod_rm(AddressingMode::RegisterDirect, 5, rm);
		self.bytes.extend_from_slice(&literal.0);
		self.finalize_instruction(|| format!("sub_literal32_to_register64 {literal}, {destination_register}"));
	}

	pub fn move_register64_to_register64(&mut self, source_register: Register64, destination_register: Register64) {
		let (reg, rm) = self.register64_reg_rm_encoding(source_register, destination_register);
		self.bytes.push(0x89);
		self.mod_rm(AddressingMode::RegisterDirect, reg, rm);
		self.finalize_instruction(|| format!("move_register64_to_register64 {source_register}, {destination_register}"));
	}

	pub fn leave(&mut self) {
		self.bytes.push(0xc9);
		self.finalize_instruction(|| "leave".to_owned());
	}

	pub fn ret_near(&mut self) {
		self.bytes.push(0xc3);
		self.finalize_instruction(|| "ret_near".to_owned());
	}

	pub fn syscall(&mut self) {
		self.bytes.push(0x0f);
		self.bytes.push(0x05);
		self.finalize_instruction(|| "syscall".to_owned());
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

			println!("{line_number:2}| Expected: {expected:?}");
			println!("{line_number:2}| Actual:   {actual:?}");
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
		let mut offset = self.initial_bytes_len;
		for print_entry in &self.print_sequence {
			let (length, label) = match print_entry {
				PrintEntry::Note { note: text } => {
					#[cfg(not(features = "test-support"))]
					writeln!(f, "// {text}")?;
					continue;
				}

				PrintEntry::Instruction { length, label } => (length, label),
			};

			let bytes = &self.bytes[offset..offset + length];
			offset += length;

			let mut spaces_written = 0;
			for (index, byte) in bytes.iter().enumerate() {
				write!(f, "{byte:02x}")?;
				spaces_written += 2;

				if index + 1 < bytes.len() {
					write!(f, " ")?;
					spaces_written += 1;
				}
			}

			const MAX_BYTES: u32 = 15;
			const MAX_SPACES: u32 = MAX_BYTES * 2 + MAX_BYTES;
			#[cfg(not(test))]
			for _ in spaces_written..MAX_SPACES {
				write!(f, " ")?;
			}

			#[cfg(test)]
			writeln!(f)?;
			#[cfg(not(test))]
			writeln!(f, "{label}")?;
		}

		Ok(())
	}
}

pub enum AddressingMode {
	RegisterDirect = 0b11,
	// todo: more
}

#[repr(u8)]
pub enum UnsizedRegister {
	Uax = 0,
	Ubx = 3,
	Ucx = 1,
	Udx = 2,
	Usi = 6,
	Udi = 7,
	Udp = 5,
	Usp = 4,

	R8u = 8,
	R9u = 9,
	R10u = 10,
	R11u = 11,
	R12u = 12,
	R13u = 13,
	R14u = 14,
	R15u = 15,
}

// #[derive(Debug, Copy, Clone, Eq, PartialEq)]
// #[repr(u8)]
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
#[repr(u8)]
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

impl From<UnsizedRegister> for Register16 {
	fn from(value: UnsizedRegister) -> Self {
		unsafe { std::mem::transmute(value as u8) }
	}
}

impl std::fmt::Display for Register16 {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		use Register16::*;

		match self {
			Ax => f.write_str("Ax"),
			Bx => f.write_str("Bx"),
			Cx => f.write_str("Cx"),
			Dx => f.write_str("Dx"),
			Si => f.write_str("Si"),
			Di => f.write_str("Di"),
			Dp => f.write_str("Dp"),
			Sp => f.write_str("Sp"),
			R8w => f.write_str("R8w"),
			R9w => f.write_str("R9w"),
			R10w => f.write_str("R10w"),
			R11w => f.write_str("R11w"),
			R12w => f.write_str("R12w"),
			R13w => f.write_str("R13w"),
			R14w => f.write_str("R14w"),
			R15w => f.write_str("R15w"),
		}
	}
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[repr(u8)]
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

impl From<UnsizedRegister> for Register32 {
	fn from(value: UnsizedRegister) -> Self {
		unsafe { std::mem::transmute(value as u8) }
	}
}

impl std::fmt::Display for Register32 {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		use Register32::*;

		match self {
			Eax => f.write_str("Eax"),
			Ebx => f.write_str("Ebx"),
			Ecx => f.write_str("Ecx"),
			Edx => f.write_str("Edx"),
			Esi => f.write_str("Esi"),
			Edi => f.write_str("Edi"),
			Ebp => f.write_str("Ebp"),
			Esp => f.write_str("Esp"),
			R8d => f.write_str("R8d"),
			R9d => f.write_str("R9d"),
			R10d => f.write_str("R10d"),
			R11d => f.write_str("R11d"),
			R12d => f.write_str("R12d"),
			R13d => f.write_str("R13d"),
			R14d => f.write_str("R14d"),
			R15d => f.write_str("R15d"),
		}
	}
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[repr(u8)]
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

impl From<UnsizedRegister> for Register64 {
	fn from(value: UnsizedRegister) -> Self {
		unsafe { std::mem::transmute(value as u8) }
	}
}

impl std::fmt::Display for Register64 {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		use Register64::*;

		match self {
			Rax => f.write_str("Rax"),
			Rbx => f.write_str("Rbx"),
			Rcx => f.write_str("Rcx"),
			Rdx => f.write_str("Rdx"),
			Rsi => f.write_str("Rsi"),
			Rdi => f.write_str("Rdi"),
			Rbp => f.write_str("Rbp"),
			Rsp => f.write_str("Rsp"),
			R8 => f.write_str("R8"),
			R9 => f.write_str("R9"),
			R10 => f.write_str("R10"),
			R11 => f.write_str("R11"),
			R12 => f.write_str("R12"),
			R13 => f.write_str("R13"),
			R14 => f.write_str("R14"),
			R15 => f.write_str("R15"),
		}
	}
}
