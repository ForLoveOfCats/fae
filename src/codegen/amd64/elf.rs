use std::mem::{size_of, transmute};

use crate::codegen::amd64::assembler::{Assembler, Register32};
use crate::codegen::amd64::codegen;
use crate::codegen::ssa::SsaModule;

const ENTRY_ADDRESS: u64 = 0x40000;
const PAGE_SIZE: u64 = 4096;

#[repr(C, packed)]
pub struct ElfIdentifier {
	magic_sequence: [u8; 4],
	class: u8,
	endianness: u8,
	elf_version: u8,
	abi_selector: u8,
	abi_version: u8,
	padding: [u8; 7],
}

impl ElfIdentifier {
	pub fn new() -> ElfIdentifier {
		ElfIdentifier {
			magic_sequence: [0x7f, 0x45, 0x4c, 0x46],
			class: 0x02,        // 64 bit
			endianness: 0x01,   // Little-endian
			elf_version: 0x01,  // ELF version 1 (EV_CURRENT)
			abi_selector: 0x00, // System V
			abi_version: 0x00,  // 0
			padding: [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
		}
	}
}

#[repr(C, packed)]
pub struct ElfHeader {
	identifier: ElfIdentifier,
	elf_type: u16,
	machine: u16,
	version: u32,
	entry: u64,
	program_header_offset: u64,
	section_header_offset: u64,
	flags: u32,
	elf_header_size: u16,
	program_header_entry_size: u16,
	phnum: u16,
	section_header_size: u16,
	section_header_count: u16,
	section_header_string_names_index: u16,
}

impl ElfHeader {
	pub fn new() -> ElfHeader {
		ElfHeader {
			identifier: ElfIdentifier::new(),
			elf_type: 0x2, // Executable file (ET_EXEC)
			machine: 0x3e, // AMD64
			version: 0x1,  // Version 1
			entry: ENTRY_ADDRESS,
			program_header_offset: 64, // 64 bytes in 64-bit ELF file
			section_header_offset: 0,
			flags: 0x0,
			elf_header_size: 64, // 64 bytes in 64-bit ELF file
			program_header_entry_size: size_of::<ProgramHeader>() as u16,
			phnum: 1, // 1 entry in program header table
			section_header_size: 64,
			section_header_count: 0, // no section header table entries
			section_header_string_names_index: 0,
		}
	}
}

#[repr(C, packed)]
pub struct ProgramHeader {
	phdr_type: u32,
	flags: u32,
	offset: u64,
	vaddr: u64,
	paddr: u64,
	filesz: u64,
	memsz: u64,
	align: u64,
}

impl ProgramHeader {
	pub fn new(code_size: u64) -> ProgramHeader {
		ProgramHeader {
			phdr_type: 0x1,   // Loadable segment (PT_LOAD)
			flags: 0x1 | 0x4, // Executable (PF_X) and readable (PF_R) segment
			offset: PAGE_SIZE,
			vaddr: ENTRY_ADDRESS,
			paddr: 0,
			filesz: code_size,
			memsz: code_size,
			align: PAGE_SIZE,
		}
	}
}

pub fn construct_elf(module: SsaModule) -> Vec<u8> {
	let mut data = Vec::new();

	let elf_header = ElfHeader::new();
	let elf_header_slice: [u8; size_of::<ElfHeader>()] = unsafe { transmute(elf_header) };
	data.extend_from_slice(&elf_header_slice);

	let program_header_table_start = data.len();
	for _ in 0..size_of::<ProgramHeader>() {
		data.push(0);
	}

	for _ in 0..PAGE_SIZE - data.len() as u64 {
		data.push(0);
	}

	let mut assembler = Assembler::new(&mut data);
	codegen::generate(module, &mut assembler);
	assembler.move_intermediate32_to_register32(0, Register32::Edi); // Return code
	assembler.move_intermediate32_to_register32(0x3c, Register32::Eax); // Exit syscall
	assembler.syscall();
	let code_size = data.len() - program_header_table_start;

	let program_header_table = ProgramHeader::new(code_size as u64);
	let program_header_table_slice: [u8; size_of::<ProgramHeader>()] = unsafe { transmute(program_header_table) };

	let program_header_table_span = program_header_table_start..program_header_table_start + size_of::<ProgramHeader>();
	data[program_header_table_span].copy_from_slice(&program_header_table_slice);

	data
}
