use inkwell::module::Module;

use crate::codegen::generator::Generator;
use crate::ir::Function;
use crate::type_store::TypeStore;

pub struct LLVMGenerator<'ctx> {
	module: Module<'ctx>,
}

impl<'ctx> LLVMGenerator<'ctx> {
	pub fn new(module: Module<'ctx>) -> Self {
		LLVMGenerator { module }
	}
}

pub struct Binding;

impl<'ctx> Generator for LLVMGenerator<'ctx> {
	type Binding = Binding;

	fn start_function(&mut self, type_store: &TypeStore, name: &str, function: Function) {
		// function.return_type
		type_store.type_entries[function.return_type.index()];

		self.module.add_function(name, ty, None);
	}
}
