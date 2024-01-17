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

use crate::codegen::intermediate::{Intermediate32, Intermediate8};
use crate::codegen::ir::InstructionKind;
use crate::codegen::optimization;
use crate::type_store::NumericKind;

fn main() {
	let mut args = std::env::args_os().skip(1);
	if args.next().as_deref() == Some(OsStr::new("t")) {
		let args = args.map(|s| s.to_str().expect("test name arguments must be valid Unicode").to_owned());
		test::run_tests(args.collect());
		return;
	}

	let mut module = codegen::ir::IrModule::new();
	module.start_function();
	let a = module.next_memory_slot();
	module.push(InstructionKind::Add {
		kind: NumericKind::I32,
		left: a.into(),
		right: Intermediate32::from(1).into(),
		destination: a,
	});

	module.start_function();
	let counter = module.next_memory_slot();
	module.push(InstructionKind::Move8 { value: Intermediate8::from(10u8), destination: counter });
	let loop_id = module.push_while_loop(counter);
	module.push(InstructionKind::Subtract {
		kind: NumericKind::U32,
		left: counter.into(),
		right: Intermediate32::from(1).into(),
		destination: counter,
	});
	module.push(InstructionKind::End { id: loop_id });

	module.start_function();
	let first = module.next_memory_slot();
	module.push(InstructionKind::Move32 { value: Intermediate32::from(40).into(), destination: first });
	let second = module.next_memory_slot();
	module.push(InstructionKind::Move32 { value: Intermediate32::from(2).into(), destination: second });
	let third = module.next_memory_slot();
	module.push(InstructionKind::Add {
		kind: NumericKind::I32,
		left: first.into(),
		right: second.into(),
		destination: third,
	});
	let forth = module.next_memory_slot();
	module.push(InstructionKind::Move32 { value: Intermediate32::from(3).into(), destination: forth });
	module.push(InstructionKind::Add {
		kind: NumericKind::I32,
		left: third.into(),
		right: second.into(),
		destination: forth,
	});

	module.start_function();
	let first = module.next_memory_slot();
	module.push(InstructionKind::Move32 { value: Intermediate32::from(40).into(), destination: first });
	let second = module.next_memory_slot();
	module.push(InstructionKind::Move32 { value: Intermediate32::from(2).into(), destination: second });
	let third = module.next_memory_slot();
	module.push(InstructionKind::Add {
		kind: NumericKind::I32,
		left: first.into(),
		right: second.into(),
		destination: third,
	});
	let forth = module.next_memory_slot();
	module.push(InstructionKind::Move32 { value: Intermediate32::from(3).into(), destination: forth });
	module.push(InstructionKind::Add {
		kind: NumericKind::I32,
		left: third.into(),
		right: second.into(),
		destination: forth,
	});
	module.push(InstructionKind::ForceUsed { slot: forth });

	module.start_function();
	let condition = module.next_memory_slot();
	module.push(InstructionKind::Move8 { value: true.into(), destination: condition });
	let a = module.next_memory_slot();
	module.push(InstructionKind::Move32 { value: 0.into(), destination: a });
	let branch_id = module.push_branch(condition);
	module.push(InstructionKind::Move32 { value: 1.into(), destination: a });
	module.push(InstructionKind::End { id: branch_id });
	let unused = module.next_memory_slot();
	module.push(InstructionKind::Move32 { value: 69.into(), destination: unused });
	module.push(InstructionKind::Add {
		kind: NumericKind::U32,
		left: a.into(),
		right: Intermediate32::from(42).into(),
		destination: a,
	});

	module.debug_dump();
	println!("\n\n\n");
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
