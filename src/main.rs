#[macro_use]
mod error;

#[macro_use]
mod multi_line_string;

mod c_codegen;
mod codegen;
mod color;
mod file;
mod ir;
mod parser;
mod project;
mod span;
mod test;
mod tokenizer;
mod tree;
mod type_store;
mod validator;

use std::{ffi::OsStr, path::Path};

use c_codegen::DebugCodegen;
use project::build_project;

use crate::{
	codegen::{intermediate::Intermediate32, ir::Instruction, optimization},
	type_store::NumericKind,
};

fn main() {
	let mut args = std::env::args_os().skip(1);
	if args.next().as_deref() == Some(OsStr::new("t")) {
		let args = args.map(|s| s.to_str().expect("test name arguments must be valid Unicode").to_owned());
		test::run_tests(args.collect());
		return;
	}

	let mut module = codegen::ir::IrModule::new();
	module.start_function();
	let condition = module.next_memory_slot();
	module.push(Instruction::Move8 { value: true.into(), destination: condition });
	let a = module.next_memory_slot();
	module.push(Instruction::Move32 { value: 0.into(), destination: a });
	let label = module.push_branch(condition);
	module.push(Instruction::Move32 { value: 1.into(), destination: a });
	module.push_label(label);
	let addend = module.next_memory_slot();
	module.push(Instruction::Add {
		kind: NumericKind::U32,
		left: a.into(),
		right: Intermediate32::from(42).into(),
		destination: a,
	});

	// let number = module.push_move_32(42);
	// let condition = module.push_move_8(true);
	// let label = module.push_branch(condition);
	// let a = module.push_move_32(1);
	// module.push_label(label);
	// let b = module.push_move_32(0);
	// let phi = module.push_phi(vec![a, b]);
	// module.push_add(type_store::NumericKind::I32, number, phi);

	module.debug_dump();
	println!("\n");
	optimization::optimize(&mut module);
	module.debug_dump();

	// let elf = codegen::amd64::elf::construct_elf(module);
	// std::fs::write("./shared/executable.x64", elf).unwrap();
	return;

	let mut stderr = std::io::stderr();
	let project_path = Path::new("./example");
	let root_name = "example".to_owned();
	let Some(binary_path) = build_project(&mut stderr, project_path, root_name, DebugCodegen::Yes) else {
		return;
	};

	let status = std::process::Command::new(binary_path)
		.spawn()
		.expect("Failed to launch resulting binary")
		.wait();
	println!("Binary execution result: {status:?}");
}
