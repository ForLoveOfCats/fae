use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;

use crate::codegen::generator::Generator;
use crate::ir::Function;
use crate::type_store::TypeStore;

pub struct LLVMGenerator<'ctx> {
	pub context: &'ctx Context,
	pub module: Module<'ctx>,
	pub builder: Builder<'ctx>,
}

impl<'ctx> LLVMGenerator<'ctx> {
	pub fn new(context: &'ctx Context) -> Self {
		let module = context.create_module("fae_translation_unit_module");
		let builder = context.create_builder();
		LLVMGenerator { context, module, builder }
	}
}

pub struct Binding;

impl<'ctx> Generator for LLVMGenerator<'ctx> {
	type Binding = Binding;

	fn start_function(&mut self, type_store: &TypeStore, name: &str, function: &Function) {
		println!("LLVMGenerator start_function");

		// type_store.type_entries[function.return_type.index()];

		let fn_type = self.context.void_type().fn_type(&[], false);
		let llvm_function = self.module.add_function(name, fn_type, None);

		let basic_block = self.context.append_basic_block(llvm_function, name);
		self.builder.position_at_end(basic_block);
		self.builder.build_return(None).unwrap();
	}
}
