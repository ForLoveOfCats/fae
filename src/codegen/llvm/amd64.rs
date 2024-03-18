use inkwell::context::Context;

use crate::codegen::llvm::generator::LLVMGenerator;
use crate::error::Messages;
use crate::type_store::TypeStore;
use crate::validator::FunctionStore;

pub fn generate_elf<'a>(
	messages: &mut Messages<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &mut FunctionStore<'a>,
) -> Vec<u8> {
	let context = Context::create();
	let module = context.create_module("fae_translation_unit_module");

	let generator = LLVMGenerator::new(module);

	vec![]
}
