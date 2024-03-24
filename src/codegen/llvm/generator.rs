use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, IntType};
use inkwell::values::IntValue;

use crate::codegen::generator::Generator;
use crate::ir::Function;
use crate::type_store::TypeStore;

use super::abi::LLVMAbi;

pub struct LLVMGenerator<'ctx, ABI: LLVMAbi> {
	pub context: &'ctx Context,
	pub module: Module<'ctx>,
	pub builder: Builder<'ctx>,

	state: State,
	parameter_type_buffer: Vec<BasicMetadataTypeEnum<'ctx>>,

	_marker: std::marker::PhantomData<ABI>,
}

impl<'ctx, ABI: LLVMAbi> LLVMGenerator<'ctx, ABI> {
	pub fn new(context: &'ctx Context) -> Self {
		let module = context.create_module("fae_translation_unit_module");
		let builder = context.create_builder();

		LLVMGenerator::<ABI> {
			context,
			module,
			builder,
			state: State::InModule,
			parameter_type_buffer: Vec::new(),
			_marker: std::marker::PhantomData::default(),
		}
	}

	fn finalize_function_if_in_function(&mut self) {
		if self.state == (State::InFunction { void_returning: true }) {
			self.builder.build_return(None).unwrap();
		}
	}
}

#[derive(Debug, PartialEq, Eq)]
enum State {
	InModule,
	InFunction { void_returning: bool },
}

pub struct Binding;

impl<'ctx, ABI: LLVMAbi> Generator for LLVMGenerator<'ctx, ABI> {
	type Binding = Binding;

	fn start_function(&mut self, type_store: &TypeStore, function: &Function, name: &str) {
		self.finalize_function_if_in_function();

		let void_returning = function.return_type.is_void(type_store);
		// let entry = type_store.type_entries[function.return_type.index()];

		self.parameter_type_buffer.clear();
		ABI::build_parameter_types(type_store, &self.context, function, &mut self.parameter_type_buffer);

		let fn_type = self.context.void_type().fn_type(&self.parameter_type_buffer, false);
		let llvm_function = self.module.add_function(name, fn_type, None);

		let basic_block = self.context.append_basic_block(llvm_function, name);
		self.builder.position_at_end(basic_block);

		self.state = State::InFunction { void_returning };
	}

	fn finalize_generator(&mut self) {
		self.finalize_function_if_in_function();
	}
}
