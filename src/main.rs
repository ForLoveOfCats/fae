#[macro_use]
mod error;

mod c_codegen;
mod codegen;
mod color;
mod file;
mod ir;
mod parser;
mod project;
mod span;
mod ssa;
mod test;
mod tokenizer;
mod tree;
mod type_store;
mod validator;

use std::{ffi::OsStr, path::Path};

use c_codegen::DebugCodegen;
use project::build_project;

use crate::codegen::amd64::assembler::{Intermediate32, Register32};

fn main() {
	let mut assembler = codegen::amd64::assembler::Assembler::new();
	let v = Intermediate32(0x12345678_u32.to_le_bytes());
	assembler.add_intermediate32_to_register32(v, Register32::Eax);
	assembler.add_intermediate32_to_register32(v, Register32::Ebx);
	assembler.add_intermediate32_to_register32(v, Register32::Ecx);
	assembler.add_intermediate32_to_register32(v, Register32::Edx);
	assembler.add_intermediate32_to_register32(v, Register32::Esi);
	assembler.add_intermediate32_to_register32(v, Register32::Edi);
	assembler.add_intermediate32_to_register32(v, Register32::Ebp);
	assembler.add_intermediate32_to_register32(v, Register32::Esp);
	
	assembler.add_intermediate32_to_register32(v, Register32::R8d);
	assembler.add_intermediate32_to_register32(v, Register32::R9d);
	assembler.add_intermediate32_to_register32(v, Register32::R10d);
	assembler.add_intermediate32_to_register32(v, Register32::R11d);
	assembler.add_intermediate32_to_register32(v, Register32::R12d);
	assembler.add_intermediate32_to_register32(v, Register32::R13d);
	assembler.add_intermediate32_to_register32(v, Register32::R14d);
	assembler.add_intermediate32_to_register32(v, Register32::R15d);

	println!("{assembler}");
	return;

	let mut args = std::env::args_os().skip(1);
	if args.next().as_deref() == Some(OsStr::new("t")) {
		let args = args.map(|s| s.to_str().expect("test name arguments must be valid Unicode").to_owned());
		test::run_tests(args.collect());
		return;
	}

	let mut stderr = std::io::stderr();
	let project_path = Path::new("./example");
	let root_name = "example".to_owned();
	let Some(binary_path) = build_project(&mut stderr, project_path, root_name, DebugCodegen::No) else {
		return;
	};

	let status = std::process::Command::new(binary_path)
		.spawn()
		.expect("Failed to launch resulting binary")
		.wait();
	println!("Binary execution result: {status:?}");
}
