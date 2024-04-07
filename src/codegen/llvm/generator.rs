use inkwell::attributes::Attribute;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicTypeEnum, PointerType, StructType};
use inkwell::values::{BasicValueEnum, PointerValue};
use inkwell::AddressSpace;

use crate::codegen::generator::Generator;
use crate::codegen::llvm::abi::{DefinedFunction, LLVMAbi};
use crate::frontend::function_store::FunctionStore;
use crate::frontend::ir::{Function, FunctionId};
use crate::frontend::type_store::{NumericKind, PrimativeKind, TypeEntryKind, TypeId, TypeStore, UserTypeKind};

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

#[derive(Debug, Clone, Copy)]
pub struct Binding<'ctx> {
	pub type_id: TypeId,
	pub kind: BindingKind<'ctx>,
}

#[derive(Debug, Clone, Copy)]
pub enum BindingKind<'ctx> {
	Value(BasicValueEnum<'ctx>),

	Pointer {
		pointer: PointerValue<'ctx>,
		pointed_type: BasicTypeEnum<'ctx>,
	},
}

impl<'ctx> Binding<'ctx> {
	pub fn to_value(self, builder: &mut Builder<'ctx>) -> BasicValueEnum<'ctx> {
		let (pointer, pointed_type) = match self.kind {
			BindingKind::Value(value) => return value,
			BindingKind::Pointer { pointer, pointed_type } => (pointer, pointed_type),
		};

		builder.build_load(pointed_type, pointer, "").unwrap()
	}
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
	functions: Vec<Vec<Option<DefinedFunction<'ctx>>>>,
	values: Vec<Option<Binding<'ctx>>>,

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
	type Binding = Binding<'ctx>;

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
			if shape.intrinsic_attribute.is_some() {
				self.functions.push(Vec::new());
				continue;
			}

			let mut specializations = Vec::with_capacity(shape.specializations.len());

			for specialization_index in 0..shape.specializations.len() {
				let specialization = &shape.specializations[specialization_index];
				if specialization.generic_poisioned {
					specializations.push(None);
					continue;
				}

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

				specializations.push(Some(defined_function));
			}

			self.functions.push(specializations);
		}
	}

	fn start_function(&mut self, type_store: &TypeStore, function: &Function, function_id: FunctionId) {
		self.finalize_function_if_in_function();

		let maybe_function = &self.functions[function_id.function_shape_index][function_id.specialization_index];
		let defined_function = maybe_function.as_ref().unwrap();
		let entry_block = defined_function
			.entry_block
			.expect("Should only be None for extern functions");
		self.builder.position_at_end(entry_block);

		self.values.clear();
		self.values.extend_from_slice(&defined_function.initial_values);

		let void_returning = function.return_type.is_void(type_store);
		self.state = State::InFunction { void_returning };
	}

	fn generate_integer_value(&mut self, type_store: &TypeStore, type_id: TypeId, value: i128) -> Self::Binding {
		let value = match type_id.numeric_kind(type_store).unwrap() {
			NumericKind::I8 | NumericKind::U8 => BasicValueEnum::IntValue(self.context.i8_type().const_int(value as u64, false)),

			NumericKind::I16 | NumericKind::U16 => {
				BasicValueEnum::IntValue(self.context.i16_type().const_int(value as u64, false))
			}

			NumericKind::I32 | NumericKind::U32 => {
				BasicValueEnum::IntValue(self.context.i32_type().const_int(value as u64, false))
			}

			NumericKind::I64 | NumericKind::U64 | NumericKind::USize => {
				BasicValueEnum::IntValue(self.context.i64_type().const_int(value as u64, false))
			}

			NumericKind::F32 => BasicValueEnum::FloatValue(self.context.f32_type().const_float(value as f64)),

			NumericKind::F64 => BasicValueEnum::FloatValue(self.context.f64_type().const_float(value as f64)),
		};

		let kind = BindingKind::Value(value);
		Binding { type_id, kind }
	}

	fn generate_string_literal(&mut self, type_store: &TypeStore, text: &str) -> Self::Binding {
		let global = self.builder.build_global_string_ptr(text, "").unwrap();
		let pointer = global.as_pointer_value();
		let len = self.context.i64_type().const_int(text.len() as u64, false);

		let a = BasicValueEnum::PointerValue(pointer);
		let b = BasicValueEnum::IntValue(len);
		let slice = self.llvm_types.slice_struct.const_named_struct(&[a, b]);

		let type_id = type_store.string_type_id();
		let kind = BindingKind::Value(BasicValueEnum::StructValue(slice));
		Binding { type_id, kind }
	}

	fn generate_struct_literal(
		&mut self,
		type_id: TypeId,
		shape_index: usize,
		specialization_index: usize,
		fields: &[Self::Binding],
	) -> Self::Binding {
		// TODO: Avoid this creating this vec every time
		let mut values = Vec::with_capacity(fields.len());
		for field in fields {
			values.push(field.to_value(&mut self.builder));
		}

		let struct_type = self.llvm_types.user_type_structs[shape_index][specialization_index].unwrap();
		let struct_value = struct_type.const_named_struct(&values);
		let kind = BindingKind::Value(BasicValueEnum::StructValue(struct_value));
		Binding { type_id, kind }
	}

	fn generate_call(
		&mut self,
		type_store: &TypeStore,
		function_id: FunctionId,
		arguments: &[Option<Binding<'ctx>>],
	) -> Option<Binding<'ctx>> {
		let maybe_function = &self.functions[function_id.function_shape_index][function_id.specialization_index];
		let function = maybe_function.as_ref().unwrap();
		self.abi
			.call_function(type_store, self.context, &mut self.builder, &self.llvm_types, function, &arguments)
	}

	fn generate_read(&mut self, readable_index: usize) -> Option<Self::Binding> {
		self.values[readable_index]
	}

	fn generate_field_read(&mut self, type_store: &TypeStore, base: Self::Binding, field_index: usize) -> Option<Self::Binding> {
		let index = field_index as u32;
		let (pointer, pointed_type) = match base.kind {
			BindingKind::Value(value) => {
				let pointed_type = value.get_type();
				let alloca = self.builder.build_alloca(pointed_type, "").unwrap();
				self.builder.build_store(alloca, value).unwrap();
				(alloca, pointed_type)
			}

			BindingKind::Pointer { pointer, pointed_type } => (pointer, pointed_type),
		};

		let pointed_struct = pointed_type.into_struct_type();
		let field_type = pointed_struct.get_field_type_at_index(index).unwrap();
		let field_pointer = self.builder.build_struct_gep(pointed_type, pointer, index, "").unwrap();

		let type_id = if let Some(_) = base.type_id.as_slice(type_store) {
			type_store.usize_type_id()
		} else if let Some(struct_type) = base.type_id.as_struct(type_store) {
			struct_type.fields[field_index].type_id
		} else {
			unreachable!("{:#?}", &type_store.type_entries[base.type_id.index()]);
		};

		let kind = BindingKind::Pointer { pointer: field_pointer, pointed_type: field_type };
		Some(Binding { type_id, kind })
	}

	fn generate_binding(&mut self, readable_index: usize, value: Option<Self::Binding>) {
		let value_index = self.values.len();
		self.values.push(value);
		assert_eq!(value_index, readable_index);
	}

	fn generate_return(&mut self, function_id: FunctionId, value: Option<Self::Binding>) {
		let maybe_function = &self.functions[function_id.function_shape_index][function_id.specialization_index];
		let function = maybe_function.as_ref().unwrap();
		self.abi.return_value(&self.context, &mut self.builder, function, value);
	}

	fn finalize_generator(&mut self) {
		self.finalize_function_if_in_function();
	}
}
