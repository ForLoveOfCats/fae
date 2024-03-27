use inkwell::attributes::Attribute;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::values::{BasicValueEnum, PointerValue};

use crate::codegen::generator::Generator;
use crate::codegen::llvm::abi::LLVMAbi;
use crate::ir::Function;
use crate::type_store::TypeStore;

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

#[derive(Debug, PartialEq, Eq)]
enum State {
	InModule,
	InFunction { void_returning: bool },
}

pub struct Binding {
	location_index: u32,
}

pub enum Location<'ctx> {
	Pointer(PointerValue<'ctx>),
	BasicValue(BasicValueEnum<'ctx>),
}

pub struct LLVMGenerator<'ctx, ABI: LLVMAbi<'ctx>> {
	pub context: &'ctx Context,
	pub module: Module<'ctx>,
	pub builder: Builder<'ctx>,

	state: State,
	abi: ABI,
	parameter_type_buffer: Vec<BasicMetadataTypeEnum<'ctx>>,
	attribute_kinds: AttributeKinds,

	locations: Vec<Location<'ctx>>,

	_marker: std::marker::PhantomData<ABI>,
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
			locations: Vec::new(),
			_marker: std::marker::PhantomData::default(),
		}
	}

	fn finalize_function_if_in_function(&mut self) {
		if self.state == (State::InFunction { void_returning: true }) {
			self.builder.build_return(None).unwrap();
		}
	}
}

impl<'ctx, ABI: LLVMAbi<'ctx>> Generator for LLVMGenerator<'ctx, ABI> {
	type Binding = Binding;

	fn start_function(&mut self, type_store: &TypeStore, function: &Function, name: &str) {
		self.finalize_function_if_in_function();

		self.parameter_type_buffer.clear();
		let llvm_function = self.abi.build_function(
			type_store,
			&self.context,
			&mut self.module,
			&mut self.builder,
			&self.attribute_kinds,
			&mut self.locations,
			function,
			name,
		);

		let basic_block = self.context.append_basic_block(llvm_function, name);
		self.builder.position_at_end(basic_block);

		let void_returning = function.return_type.is_void(type_store);
		self.state = State::InFunction { void_returning };
	}

	fn finalize_generator(&mut self) {
		self.finalize_function_if_in_function();
	}
}
