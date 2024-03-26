use inkwell::attributes::Attribute;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, IntType};
use inkwell::values::IntValue;

use crate::codegen::generator::Generator;
use crate::ir::Function;
use crate::type_store::TypeStore;

use super::abi::LLVMAbi;

pub struct LLVMGenerator<'ctx, ABI: LLVMAbi<'ctx>> {
	pub context: &'ctx Context,
	pub module: Module<'ctx>,
	pub builder: Builder<'ctx>,

	state: State,
	abi: ABI,
	parameter_type_buffer: Vec<BasicMetadataTypeEnum<'ctx>>,
	attribute_kinds: AttributeKinds,

	_marker: std::marker::PhantomData<ABI>,
}

pub struct AttributeKinds {
	pub sret: u32,
	pub byval: u32,
}

impl AttributeKinds {
	fn new() -> AttributeKinds {
		fn kind(name: &str) -> u32 {
			let kind = Attribute::get_named_enum_kind_id(name);
			assert_ne!(kind, 0);
			kind
		}

		AttributeKinds { sret: kind("sret"), byval: kind("byval") }
	}
}

impl<'ctx, ABI: LLVMAbi<'ctx>> LLVMGenerator<'ctx, ABI> {
	pub fn new(context: &'ctx Context) -> Self {
		let module = context.create_module("fae_translation_unit_module");
		let builder = context.create_builder();

		LLVMGenerator::<ABI> {
			context,
			module,
			builder,
			state: State::InModule,
			abi: ABI::new(),
			parameter_type_buffer: Vec::new(),
			attribute_kinds: AttributeKinds::new(),
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

impl<'ctx, ABI: LLVMAbi<'ctx>> Generator for LLVMGenerator<'ctx, ABI> {
	type Binding = Binding;

	fn start_function(&mut self, type_store: &TypeStore, function: &Function, name: &str) {
		self.finalize_function_if_in_function();

		let void_returning = function.return_type.is_void(type_store);
		// let entry = type_store.type_entries[function.return_type.index()];

		self.parameter_type_buffer.clear();
		let llvm_function =
			self.abi
				.build_function(type_store, &self.context, &mut self.module, &self.attribute_kinds, function, name);

		let basic_block = self.context.append_basic_block(llvm_function, name);
		self.builder.position_at_end(basic_block);

		self.state = State::InFunction { void_returning };
	}

	fn finalize_generator(&mut self) {
		self.finalize_function_if_in_function();
	}
}
