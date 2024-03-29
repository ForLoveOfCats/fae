use inkwell::attributes::Attribute;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicTypeEnum, PointerType, StructType};
use inkwell::values::BasicValueEnum;
use inkwell::AddressSpace;

use crate::codegen::generator::Generator;
use crate::codegen::llvm::abi::LLVMAbi;
use crate::ir::{Function, FunctionId};
use crate::tree::ExternAttribute;
use crate::type_store::{NumericKind, PrimativeKind, TypeEntryKind, TypeId, TypeStore, UserTypeKind};
use crate::validator::FunctionStore;

use super::abi::DefinedFunction;

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
	value_index: u32,
}

pub struct LLVMTypes<'ctx> {
	pub opaque_pointer: PointerType<'ctx>,
	pub slice_struct: StructType<'ctx>,

	user_type_structs: Vec<Vec<Option<StructType<'ctx>>>>,
}

impl<'ctx> LLVMTypes<'ctx> {
	fn new(context: &'ctx Context) -> Self {
		let opaque_pointer = context.i8_type().ptr_type(AddressSpace::default());
		let slice_struct = context.struct_type(
			&[
				BasicTypeEnum::IntType(context.i64_type()),
				BasicTypeEnum::IntType(context.i64_type()),
			],
			false,
		);

		LLVMTypes { opaque_pointer, slice_struct, user_type_structs: Vec::new() }
	}

	// Cannot accept `void` type and assumes that any struct asked for has been registered; does not follow pointers
	pub fn type_to_basic_type_enum(
		&self,
		context: &'ctx Context,
		type_store: &TypeStore,
		type_id: TypeId,
	) -> BasicTypeEnum<'ctx> {
		let entry = &type_store.type_entries[type_id.index()];

		match entry.kind {
			TypeEntryKind::BuiltinType { kind } => match kind {
				PrimativeKind::Bool => BasicTypeEnum::IntType(context.i8_type()),

				PrimativeKind::Numeric(numeric_kind) => match numeric_kind {
					NumericKind::I8 | NumericKind::U8 => BasicTypeEnum::IntType(context.i8_type()),
					NumericKind::I16 | NumericKind::U16 => BasicTypeEnum::IntType(context.i16_type()),
					NumericKind::I32 | NumericKind::U32 => BasicTypeEnum::IntType(context.i32_type()),
					NumericKind::I64 | NumericKind::U64 | NumericKind::USize => BasicTypeEnum::IntType(context.i64_type()),
					NumericKind::F32 => BasicTypeEnum::FloatType(context.f32_type()),
					NumericKind::F64 => BasicTypeEnum::FloatType(context.f64_type()),
				},

				PrimativeKind::AnyCollapse
				| PrimativeKind::Void
				| PrimativeKind::UntypedInteger
				| PrimativeKind::UntypedDecimal => unreachable!("{kind:?}"),
			},

			TypeEntryKind::UserType { shape_index, specialization_index } => {
				let struct_type = self.user_type_structs[shape_index][specialization_index];
				BasicTypeEnum::StructType(struct_type.unwrap())
			}

			TypeEntryKind::Pointer { .. } => BasicTypeEnum::PointerType(self.opaque_pointer),

			TypeEntryKind::Slice(_) => BasicTypeEnum::StructType(self.slice_struct),

			TypeEntryKind::UserTypeGeneric { .. } | TypeEntryKind::FunctionGeneric { .. } => unreachable!("{:?}", entry.kind),
		}
	}
}

pub struct LLVMGenerator<'ctx, ABI: LLVMAbi<'ctx>> {
	pub context: &'ctx Context,
	pub module: Module<'ctx>,
	pub builder: Builder<'ctx>,

	abi: ABI,
	attribute_kinds: AttributeKinds,
	llvm_types: LLVMTypes<'ctx>,

	state: State,
	functions: Vec<Vec<DefinedFunction<'ctx>>>,
	values: Vec<BasicValueEnum<'ctx>>,

	_marker: std::marker::PhantomData<ABI>,
}

impl<'ctx, ABI: LLVMAbi<'ctx>> LLVMGenerator<'ctx, ABI> {
	pub fn new(context: &'ctx Context) -> Self {
		let module = context.create_module("fae_translation_unit_module");
		let builder = context.create_builder();
		let llvm_types = LLVMTypes::new(context);

		LLVMGenerator::<ABI> {
			context,
			module,
			builder,

			abi: ABI::new(),
			attribute_kinds: AttributeKinds::new(),
			llvm_types,

			state: State::InModule,
			functions: Vec::new(),
			values: Vec::new(),

			_marker: std::marker::PhantomData::default(),
		}
	}

	fn finalize_function_if_in_function(&mut self) {
		if self.state == (State::InFunction { void_returning: true }) {
			self.builder.build_return(None).unwrap();
		}
		self.state = State::InModule;
	}
}

impl<'ctx, ABI: LLVMAbi<'ctx>> Generator for LLVMGenerator<'ctx, ABI> {
	type Binding = Binding;

	fn register_type_descriptions(&mut self, type_store: &mut TypeStore) {
		assert_eq!(self.llvm_types.user_type_structs.len(), 0);

		for shape in &type_store.user_types {
			let specialization_count = match &shape.kind {
				UserTypeKind::Struct { shape } => shape.specializations.len(),
			};

			let specializations = Vec::from_iter((0..specialization_count).into_iter().map(|_| None));
			self.llvm_types.user_type_structs.push(specializations);
		}

		let mut field_types_buffer = Vec::new();
		for &description in &type_store.user_type_generate_order {
			field_types_buffer.clear();
			let shape = &type_store.user_types[description.shape_index];

			#[allow(irrefutable_let_patterns)]
			if let UserTypeKind::Struct { shape } = &shape.kind {
				let specialization = &shape.specializations[description.specialization_index];
				for field in &specialization.fields {
					let llvm_type = self
						.llvm_types
						.type_to_basic_type_enum(self.context, type_store, field.type_id);
					field_types_buffer.push(llvm_type);
				}

				let llvm_struct = self.context.struct_type(&field_types_buffer, false);
				self.llvm_types.user_type_structs[description.shape_index][description.specialization_index] = Some(llvm_struct);
			}
		}
	}

	fn register_functions(&mut self, type_store: &TypeStore, function_store: &FunctionStore) {
		assert_eq!(self.functions.len(), 0);

		for function_shape_index in 0..function_store.shapes.len() {
			let shape = &function_store.shapes[function_shape_index];
			if let Some(extern_attribute) = shape.extern_attribute {
				if let ExternAttribute::Intrinsic = extern_attribute.item {
					self.functions.push(Vec::new());
					continue;
				}
			}

			let mut specializations = Vec::with_capacity(shape.specializations.len());

			for specialization_index in 0..shape.specializations.len() {
				let specialization = &shape.specializations[specialization_index];
				let defined_function = self.abi.define_function(
					type_store,
					&self.context,
					&mut self.module,
					&mut self.builder,
					&self.attribute_kinds,
					&self.llvm_types,
					shape,
					specialization,
				);

				specializations.push(defined_function);
			}

			self.functions.push(specializations);
		}
	}

	fn start_function(&mut self, type_store: &TypeStore, function: &Function, function_id: FunctionId) {
		self.finalize_function_if_in_function();

		let defined_function = &self.functions[function_id.function_shape_index][function_id.specialization_index];
		let entry_block = defined_function
			.entry_block
			.expect("Should only be None for extern functions");
		self.builder.position_at_end(entry_block);

		self.values.clear();
		self.values.extend_from_slice(&defined_function.argument_values);

		let void_returning = function.return_type.is_void(type_store);
		self.state = State::InFunction { void_returning };
	}

	fn finalize_generator(&mut self) {
		self.finalize_function_if_in_function();
	}
}
