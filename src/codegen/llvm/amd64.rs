use std::path::Path;

use inkwell::context::Context;
use inkwell::targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple};
use inkwell::OptimizationLevel;

use crate::codegen::codegen::generate;
use crate::codegen::llvm::abi::SysvAbi;
use crate::codegen::llvm::generator::LLVMGenerator;
use crate::error::Messages;
use crate::type_store::TypeStore;
use crate::validator::FunctionStore;

pub fn generate_elf<'a>(
	messages: &mut Messages<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &mut FunctionStore<'a>,
) -> Vec<u8> {
	Target::initialize_x86(&InitializationConfig::default());

	let context = Context::create();
	let mut generator = LLVMGenerator::<SysvAbi>::new(&context);

	generate(messages, type_store, function_store, &mut generator);

	let triple = TargetTriple::create("x86_64-pc-linux-gnu");
	let target = Target::from_triple(&triple).unwrap();
	let _machine = target
		.create_target_machine(&triple, "", "", OptimizationLevel::None, RelocMode::Default, CodeModel::Default)
		.unwrap();

	let path = Path::new("./fae.ll");
	generator.module.print_to_file(path).unwrap();

	vec![]
}
