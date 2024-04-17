use inkwell::attributes::Attribute;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum, PointerType, StructType};
use inkwell::values::{BasicValue, BasicValueEnum, PointerValue};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};

use crate::codegen::codegen;
use crate::codegen::generator::Generator;
use crate::codegen::llvm::abi::{DefinedFunction, LLVMAbi};
use crate::frontend::function_store::FunctionStore;
use crate::frontend::ir::{Block, Expression, Function, FunctionId, IfElseChain};
use crate::frontend::lang_items::LangItems;
use crate::frontend::span::Span;
use crate::frontend::symbols::Statics;
use crate::frontend::tree::BinaryOperator;
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
	InFunction { function_id: FunctionId, void_returning: bool },
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

struct ValuePointer<'ctx> {
	pointer: PointerValue<'ctx>,
	pointed_type: BasicTypeEnum<'ctx>,
}

impl<'ctx> Binding<'ctx> {
	pub fn to_value(self, builder: &Builder<'ctx>) -> BasicValueEnum<'ctx> {
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
				BasicTypeEnum::PointerType(opaque_pointer),
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
				PrimativeKind::Bool => BasicTypeEnum::IntType(context.bool_type()),

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

struct BlockFrame {
	intial_readables_len: usize,
}

pub struct LLVMGenerator<'ctx, ABI: LLVMAbi<'ctx>> {
	pub context: &'ctx Context,
	pub module: Module<'ctx>,
	pub builder: Builder<'ctx>,

	abi: Option<ABI>, // I really dislike the lease pattern, oh well
	pub attribute_kinds: AttributeKinds,
	pub llvm_types: LLVMTypes<'ctx>,

	state: State,
	block_frames: Vec<BlockFrame>,
	functions: Vec<Vec<Option<DefinedFunction<'ctx>>>>,
	statics: Vec<Binding<'ctx>>,
	readables: Vec<Option<Binding<'ctx>>>,

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

			abi: Some(ABI::new()),
			attribute_kinds: AttributeKinds::new(),
			llvm_types,

			state: State::InModule,
			block_frames: Vec::new(),
			functions: Vec::new(),
			statics: Vec::new(),
			readables: Vec::new(),

			_marker: std::marker::PhantomData,
		}
	}

	fn finalize_function_if_in_function(&mut self) {
		if let State::InFunction { function_id, void_returning } = self.state {
			if void_returning && self.builder.get_insert_block().unwrap().get_terminator().is_none() {
				self.builder.build_return(None).unwrap();
			}

			let function = &self.functions[function_id.function_shape_index][function_id.specialization_index];
			let defined_function = function.as_ref().unwrap();

			let alloca_block = defined_function
				.alloca_block
				.expect("Should only be None for extern functions");
			let logic_begin_block = defined_function
				.logic_begin_block
				.expect("Should only be None for extern functions");

			self.builder.position_at_end(alloca_block);
			self.builder.build_unconditional_branch(logic_begin_block).unwrap();
			self.builder.clear_insertion_position();
		}

		self.state = State::InModule;
	}

	pub fn build_alloca<T: BasicType<'ctx>>(&self, llvm_type: T) -> PointerValue<'ctx> {
		let function_id = match self.state {
			State::InFunction { function_id, .. } => function_id,
			State::InModule => unreachable!(),
		};

		let original_block = self.builder.get_insert_block().unwrap();

		let maybe = &self.functions[function_id.function_shape_index][function_id.specialization_index];
		let defined_function = maybe.as_ref().unwrap();
		let alloca_block = defined_function
			.alloca_block
			.expect("Should only be None for extern functions");

		self.builder.position_at_end(alloca_block);
		let pointer = self.builder.build_alloca(llvm_type, "").unwrap();

		self.builder.position_at_end(original_block);
		pointer
	}

	fn value_pointer(&mut self, binding: Binding<'ctx>) -> ValuePointer<'ctx> {
		match binding.kind {
			BindingKind::Value(value) => {
				let pointed_type = value.get_type();
				let pointer = self.build_alloca(pointed_type);
				self.builder.build_store(pointer, value).unwrap();
				ValuePointer { pointer, pointed_type }
			}

			BindingKind::Pointer { pointer, pointed_type } => ValuePointer { pointer, pointed_type },
		}
	}
}

impl<'ctx, ABI: LLVMAbi<'ctx>> Generator for LLVMGenerator<'ctx, ABI> {
	type Binding = Binding<'ctx>;

	fn register_type_descriptions(&mut self, type_store: &TypeStore) {
		assert_eq!(self.llvm_types.user_type_structs.len(), 0);

		for shape in &type_store.user_types {
			let specialization_count = match &shape.kind {
				UserTypeKind::Struct { shape } => shape.specializations.len(),
			};

			let specializations = Vec::from_iter((0..specialization_count).map(|_| None));
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

	fn register_statics(&mut self, type_store: &TypeStore, statics: &Statics) {
		for static_instance in &statics.statics {
			let llvm_type = self
				.llvm_types
				.type_to_basic_type_enum(self.context, type_store, static_instance.type_id);

			let extern_attribute = static_instance.extern_attribute.unwrap();
			let global = self.module.add_global(llvm_type, None, extern_attribute.name);

			let value = global.as_basic_value_enum();
			let kind = BindingKind::Value(value);
			let binding = Binding { type_id: static_instance.type_id, kind };
			self.statics.push(binding);
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

				assert_eq!(self.state, State::InModule);
				let function_id = FunctionId { function_shape_index, specialization_index };
				let void_returning = specialization.return_type.is_void(type_store);
				self.state = State::InFunction { function_id, void_returning };

				let mut abi = self.abi.take().unwrap();
				let defined_function = abi.define_function(
					type_store,
					self.context,
					&mut self.module,
					&mut self.builder,
					&self.attribute_kinds,
					&self.llvm_types,
					shape,
					specialization,
				);
				self.abi = Some(abi);

				specializations.push(Some(defined_function));
				self.state = State::InModule
			}

			self.functions.push(specializations);
		}
	}

	fn start_block(&mut self) {
		let frame = BlockFrame { intial_readables_len: self.readables.len() };
		self.block_frames.push(frame);
	}

	fn end_block(&mut self) {
		let frame = self.block_frames.pop().expect("Every end must match a start");
		self.readables.truncate(frame.intial_readables_len);
	}

	fn start_function(&mut self, type_store: &TypeStore, function: &Function, function_id: FunctionId) {
		self.finalize_function_if_in_function();

		let maybe_function = &self.functions[function_id.function_shape_index][function_id.specialization_index];
		let defined_function = maybe_function.as_ref().unwrap();
		let logic_begin_block = defined_function
			.logic_begin_block
			.expect("Should only be None for extern functions");
		self.builder.position_at_end(logic_begin_block);

		self.readables.clear();
		self.readables.extend_from_slice(&defined_function.initial_values);

		let void_returning = function.return_type.is_void(type_store);
		self.state = State::InFunction { function_id, void_returning };
	}

	fn generate_if_else_chain(
		&mut self,
		context: &mut codegen::Context,
		chain_expression: &IfElseChain,
		mut condition_callback: impl FnMut(&mut codegen::Context, &mut Self, &Expression) -> Self::Binding,
		mut body_callback: impl FnMut(&mut codegen::Context, &mut Self, &Block),
	) {
		let original_block = self.builder.get_insert_block().unwrap();
		let following_block = self
			.context
			.insert_basic_block_after(original_block, "if_else_following_block");

		let mut next_condition_block = self.context.insert_basic_block_after(original_block, "condition_block");
		let mut insert_after = next_condition_block;

		self.builder.build_unconditional_branch(next_condition_block).unwrap();

		for entry in &chain_expression.entries {
			let condition_block = next_condition_block;
			self.builder.position_at_end(condition_block);
			next_condition_block = self.context.insert_basic_block_after(insert_after, "condition_block");

			let condition = condition_callback(context, self, &entry.condition);
			let condition = condition.to_value(&self.builder).into_int_value();

			let if_block = self.context.insert_basic_block_after(insert_after, "if_block");
			insert_after = if_block;

			let zero = condition.get_type().const_zero();
			let flag = self.builder.build_int_compare(IntPredicate::NE, condition, zero, "").unwrap();
			self.builder
				.build_conditional_branch(flag, if_block, next_condition_block)
				.unwrap();

			self.builder.position_at_end(if_block);
			body_callback(context, self, &entry.body);
			let current_block = self.builder.get_insert_block().unwrap();
			if current_block.get_terminator().is_none() {
				self.builder.build_unconditional_branch(following_block).unwrap();
			}
		}

		if let Some(body) = &chain_expression.else_body {
			let block = next_condition_block;
			block.set_name("else_block");

			self.builder.position_at_end(block);
			body_callback(context, self, body);
			let current_block = self.builder.get_insert_block().unwrap();
			if current_block.get_terminator().is_none() {
				self.builder.build_unconditional_branch(following_block).unwrap();
			}
		} else {
			let block = next_condition_block;
			block.set_name("non_existant_else_block");
			self.builder.position_at_end(block);
			self.builder.build_unconditional_branch(following_block).unwrap();
		}

		self.builder.position_at_end(following_block);
	}

	fn generate_while(
		&mut self,
		context: &mut codegen::Context,
		condition_callback: impl FnOnce(&mut codegen::Context, &mut Self) -> Self::Binding,
		body_callback: impl FnOnce(&mut codegen::Context, &mut Self),
	) {
		let original_block = self.builder.get_insert_block().unwrap();
		let condition_block = self.context.insert_basic_block_after(original_block, "while_condition_block");
		let while_block = self.context.insert_basic_block_after(condition_block, "while_body_block");
		let following_block = self.context.insert_basic_block_after(while_block, "while_following_block");

		self.builder.build_unconditional_branch(condition_block).unwrap();
		self.builder.position_at_end(condition_block);

		let condition_binding = condition_callback(context, self);
		let condition = condition_binding.to_value(&self.builder).into_int_value();
		let zero = condition.get_type().const_zero();
		let flag = self.builder.build_int_compare(IntPredicate::NE, condition, zero, "").unwrap();
		self.builder
			.build_conditional_branch(flag, while_block, following_block)
			.unwrap();

		self.builder.position_at_end(while_block);
		body_callback(context, self);
		let current_block = self.builder.get_insert_block().unwrap();
		if current_block.get_terminator().is_none() {
			self.builder.build_unconditional_branch(condition_block).unwrap();
		}

		self.builder.position_at_end(following_block);
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

	fn generate_decimal_value(&mut self, type_store: &TypeStore, type_id: TypeId, value: f64) -> Self::Binding {
		let value = match type_id.numeric_kind(type_store).unwrap() {
			NumericKind::F32 => BasicValueEnum::FloatValue(self.context.f32_type().const_float(value)),
			NumericKind::F64 => BasicValueEnum::FloatValue(self.context.f64_type().const_float(value)),
			kind => unreachable!("{kind}"),
		};

		let kind = BindingKind::Value(value);
		Binding { type_id, kind }
	}

	fn generate_boolean_literal(&mut self, type_store: &TypeStore, literal: bool) -> Self::Binding {
		let value = self.context.bool_type().const_int(literal as u64, false);
		let kind = BindingKind::Value(BasicValueEnum::IntValue(value));
		Binding { type_id: type_store.bool_type_id(), kind }
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

	fn generate_array_literal(
		&mut self,
		type_store: &TypeStore,
		elements: &[Self::Binding],
		element_type_id: TypeId,
		slice_type_id: TypeId,
	) -> Self::Binding {
		assert!(!elements.is_empty());

		let element_type = self
			.llvm_types
			.type_to_basic_type_enum(self.context, type_store, element_type_id);
		let array_type = element_type.array_type(elements.len() as u32);
		let alloca = self.build_alloca(array_type);

		let zero = self.context.i64_type().const_int(0, false);
		for (index, element) in elements.iter().enumerate() {
			let index = self.context.i64_type().const_int(index as u64, false);
			let pointer = unsafe { self.builder.build_gep(array_type, alloca, &[zero, index], "").unwrap() };
			match element.kind {
				BindingKind::Value(value) => {
					self.builder.build_store(pointer, value).unwrap();
				}

				BindingKind::Pointer { pointer: value_pointer, .. } => {
					let layout = type_store.type_layout(element_type_id);
					let align = layout.alignment as u32;
					let size = self.context.i64_type().const_int(layout.size as u64, false);
					self.builder.build_memcpy(pointer, align, value_pointer, align, size).unwrap();
				}
			}
		}

		let slice_type = self.llvm_types.slice_struct;
		let slice_alloca = self.build_alloca(slice_type);

		let pointer_pointer = self.builder.build_struct_gep(slice_type, slice_alloca, 0, "").unwrap();
		let pointer_value = BasicValueEnum::PointerValue(alloca);
		self.builder.build_store(pointer_pointer, pointer_value).unwrap();

		let len_pointer = self.builder.build_struct_gep(slice_type, slice_alloca, 1, "").unwrap();
		let len = self.context.i64_type().const_int(elements.len() as u64, false);
		self.builder.build_store(len_pointer, BasicValueEnum::IntValue(len)).unwrap();

		let pointed_type = self
			.llvm_types
			.type_to_basic_type_enum(self.context, type_store, slice_type_id);

		let kind = BindingKind::Pointer { pointer: slice_alloca, pointed_type };
		Binding { type_id: slice_type_id, kind }
	}

	fn generate_struct_literal(
		&mut self,
		type_id: TypeId,
		shape_index: usize,
		specialization_index: usize,
		fields: &[Self::Binding],
	) -> Self::Binding {
		let struct_type = self.llvm_types.user_type_structs[shape_index][specialization_index].unwrap();

		let alloca = self.build_alloca(struct_type);
		for (index, field) in fields.iter().enumerate() {
			let value = field.to_value(&self.builder);
			let field_pointer = self.builder.build_struct_gep(struct_type, alloca, index as u32, "").unwrap();
			self.builder.build_store(field_pointer, value).unwrap();
		}

		let pointed_type = BasicTypeEnum::StructType(struct_type);
		let kind = BindingKind::Pointer { pointer: alloca, pointed_type };
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

		let mut abi = self.abi.take().unwrap();
		let binding = abi.call_function(self, type_store, function, arguments);
		self.abi = Some(abi);

		binding
	}

	fn generate_read(&mut self, readable_index: usize) -> Option<Self::Binding> {
		self.readables[readable_index]
	}

	fn generate_static_read(&mut self, static_index: usize) -> Self::Binding {
		self.statics[static_index]
	}

	fn generate_field_read(&mut self, type_store: &TypeStore, base: Self::Binding, field_index: usize) -> Option<Self::Binding> {
		let index = field_index as u32;

		let ValuePointer { pointer, pointed_type } = self.value_pointer(base);
		let pointed_struct = pointed_type.into_struct_type();
		let field_type = pointed_struct.get_field_type_at_index(index).unwrap();
		let field_pointer = self.builder.build_struct_gep(pointed_type, pointer, index, "").unwrap();

		let type_id = if base.type_id.as_slice(type_store).is_some() {
			type_store.usize_type_id()
		} else if let Some(struct_type) = base.type_id.as_struct(type_store) {
			struct_type.fields[field_index].type_id
		} else {
			unreachable!("{:#?}", &type_store.type_entries[base.type_id.index()]);
		};

		let kind = BindingKind::Pointer { pointer: field_pointer, pointed_type: field_type };
		Some(Binding { type_id, kind })
	}

	fn generate_negate(&mut self, value: Self::Binding, type_id: TypeId) -> Self::Binding {
		let value = value.to_value(&self.builder);

		let negated = if value.is_int_value() {
			let int = self.builder.build_int_neg(value.into_int_value(), "").unwrap();
			BasicValueEnum::IntValue(int)
		} else if value.is_float_value() {
			let float = self.builder.build_float_neg(value.into_float_value(), "").unwrap();
			BasicValueEnum::FloatValue(float)
		} else {
			unreachable!("{value:?}");
		};

		let kind = BindingKind::Value(negated);
		Binding { type_id, kind }
	}

	fn generate_invert(&mut self, value: Self::Binding) -> Self::Binding {
		let type_id = value.type_id;
		let value = value.to_value(&self.builder);
		let inverted = self.builder.build_not(value.into_int_value(), "").unwrap();
		let kind = BindingKind::Value(BasicValueEnum::IntValue(inverted));
		Binding { type_id, kind }
	}

	fn generate_address_of(&mut self, base: Self::Binding, pointer_type_id: TypeId) -> Self::Binding {
		let pointer = self.value_pointer(base);
		let kind = BindingKind::Value(BasicValueEnum::PointerValue(pointer.pointer));
		Binding { type_id: pointer_type_id, kind }
	}

	fn generate_dereference(&mut self, type_store: &TypeStore, base: Self::Binding, pointed_type_id: TypeId) -> Self::Binding {
		let pointer = match base.kind {
			BindingKind::Value(value) => value.into_pointer_value(),

			BindingKind::Pointer { pointer, pointed_type } => {
				let value = self.builder.build_load(pointed_type, pointer, "").unwrap();
				value.into_pointer_value()
			}
		};

		let pointed_type = self
			.llvm_types
			.type_to_basic_type_enum(self.context, type_store, pointed_type_id);
		let kind = BindingKind::Pointer { pointer, pointed_type };
		Binding { type_id: pointed_type_id, kind }
	}

	fn generate_cast(&mut self, type_store: &TypeStore, base: Self::Binding, to: TypeId) -> Self::Binding {
		let from = base.to_value(&self.builder);
		let from_pointer = from.is_pointer_value();
		let to_pointer = to.is_pointer(type_store);

		// Pointer to pointer, nop
		if from_pointer && to_pointer {
			let kind = BindingKind::Value(from);
			return Binding { type_id: to, kind };
		}

		let from_int = from.is_int_value();

		// Int to pointer
		if from_int && to_pointer {
			let int = from.into_int_value();
			let pointer_type = self.llvm_types.opaque_pointer;
			let pointer = self.builder.build_int_to_ptr(int, pointer_type, "").unwrap();

			let kind = BindingKind::Value(BasicValueEnum::PointerValue(pointer));
			return Binding { type_id: to, kind };
		}

		let to_kind = to.numeric_kind(type_store).unwrap();
		use NumericKind::*;
		let (to_int_type, to_float_type) = match to_kind {
			I8 | U8 => (Some(self.context.i8_type()), None),
			I16 | U16 => (Some(self.context.i16_type()), None),
			I32 | U32 => (Some(self.context.i32_type()), None),
			I64 | U64 | USize => (Some(self.context.i64_type()), None),
			F32 => (None, Some(self.context.f32_type())),
			F64 => (None, Some(self.context.f64_type())),
		};

		// Pointer to int
		if from_pointer {
			if let Some(to_type) = to_int_type {
				let int = self.builder.build_ptr_to_int(from.into_pointer_value(), to_type, "").unwrap();
				let kind = BindingKind::Value(BasicValueEnum::IntValue(int));
				return Binding { type_id: to, kind };
			}
		}

		// Int to int
		if from_int {
			if let Some(to_type) = to_int_type {
				let int = self.builder.build_int_cast(from.into_int_value(), to_type, "").unwrap();
				let kind = BindingKind::Value(BasicValueEnum::IntValue(int));
				return Binding { type_id: to, kind };
			}
		}

		let from_kind = base.type_id.numeric_kind(type_store).unwrap();

		// Int to float
		if from_int {
			if let Some(to_type) = to_float_type {
				let int = from.into_int_value();
				let float = if from_kind.is_signed() {
					self.builder.build_signed_int_to_float(int, to_type, "").unwrap()
				} else {
					self.builder.build_unsigned_int_to_float(int, to_type, "").unwrap()
				};
				let kind = BindingKind::Value(BasicValueEnum::FloatValue(float));
				return Binding { type_id: to, kind };
			}
		}

		let from_float = from.is_float_value();

		// Float to int
		if from_float {
			if let Some(to_type) = to_int_type {
				let float = from.into_float_value();
				let int = if to_kind.is_signed() {
					self.builder.build_float_to_signed_int(float, to_type, "").unwrap()
				} else {
					self.builder.build_float_to_unsigned_int(float, to_type, "").unwrap()
				};
				let kind = BindingKind::Value(BasicValueEnum::IntValue(int));
				return Binding { type_id: to, kind };
			}
		}

		// Float to float
		if from_float {
			if let Some(to_type) = to_float_type {
				let float = from.into_float_value();
				let float = self.builder.build_float_cast(float, to_type, "").unwrap();
				let kind = BindingKind::Value(BasicValueEnum::FloatValue(float));
				return Binding { type_id: to, kind };
			}
		}

		unreachable!()
	}

	fn generate_slice_index(
		&mut self,
		lang_items: &LangItems,
		type_store: &TypeStore,
		item_type: TypeId,
		base: Self::Binding,
		index: Self::Binding,
		_index_span: Span,
	) -> Option<Self::Binding> {
		let original_block = self.builder.get_insert_block().unwrap();
		let failure_block = self.context.insert_basic_block_after(original_block, "bounds_check_failure");
		let success_block = self.context.insert_basic_block_after(failure_block, "");

		let ValuePointer { pointer: value_pointer, .. } = self.value_pointer(base);

		let pointer_type = self.llvm_types.opaque_pointer;
		let struct_type = self.llvm_types.slice_struct;
		let pointer_pointer = self.builder.build_struct_gep(struct_type, value_pointer, 0, "").unwrap();
		let pointer_value = self.builder.build_load(pointer_type, pointer_pointer, "").unwrap();
		let pointer = pointer_value.into_pointer_value();

		let i64_type = self.context.i64_type();
		let len_pointer = self.builder.build_struct_gep(struct_type, value_pointer, 1, "").unwrap();
		let len = self.builder.build_load(i64_type, len_pointer, "").unwrap().into_int_value();

		let index = index.to_value(&self.builder).into_int_value();

		let zero = i64_type.const_int(0, false);
		let greater_than_zero = self.builder.build_int_compare(IntPredicate::SGE, index, zero, "").unwrap();
		let less_than_len = self.builder.build_int_compare(IntPredicate::SLT, index, len, "").unwrap();
		let in_bounds = self.builder.build_and(greater_than_zero, less_than_len, "").unwrap();

		self.builder
			.build_conditional_branch(in_bounds, success_block, failure_block)
			.unwrap();

		self.builder.position_at_end(failure_block);
		// TODO: Build some abstraction for calling lang item functions
		let failure_args = {
			let kind = BindingKind::Value(BasicValueEnum::IntValue(len));
			let len = Some(Binding { type_id: type_store.i64_type_id(), kind });
			let kind = BindingKind::Value(BasicValueEnum::IntValue(index));
			let index = Some(Binding { type_id: type_store.i64_type_id(), kind });
			[len, index]
		};
		self.generate_call(type_store, lang_items.slice_bound_check_failure.unwrap(), &failure_args);
		self.builder.build_unreachable().unwrap();

		self.builder.position_at_end(success_block);

		let item_layout = type_store.type_layout(item_type);
		if item_layout.size <= 0 {
			return None;
		}

		let indicies = &[index];
		let pointed_type = self.llvm_types.type_to_basic_type_enum(self.context, type_store, item_type);
		let adjusted = unsafe { self.builder.build_gep(pointed_type, pointer, indicies, "").unwrap() };

		let kind = BindingKind::Pointer { pointer: adjusted, pointed_type };
		Some(Binding { type_id: item_type, kind })
	}

	fn generate_assign(&mut self, type_store: &TypeStore, left: Self::Binding, right: Self::Binding) {
		let left = match left.kind {
			BindingKind::Pointer { pointer, .. } => pointer,
			BindingKind::Value(value) => unreachable!("{value:?}"),
		};

		match right.kind {
			BindingKind::Value(value) => {
				self.builder.build_store(left, value).unwrap();
			}

			BindingKind::Pointer { pointer: right_pointer, .. } => {
				let layout = type_store.type_layout(right.type_id);
				let align = layout.alignment as u32;
				let size = self.context.i64_type().const_int(layout.size as u64, false);
				self.builder.build_memcpy(left, align, right_pointer, align, size).unwrap();
			}
		}
	}

	fn generate_binary_operation(
		&mut self,
		type_store: &TypeStore,
		left: Self::Binding,
		right: Self::Binding,
		op: BinaryOperator,
		source_type_id: TypeId,
		result_type_id: TypeId,
	) -> Option<Self::Binding> {
		if let BinaryOperator::Assign = op {
			let left = match left.kind {
				BindingKind::Pointer { pointer, .. } => pointer,
				BindingKind::Value(value) => unreachable!("{value:?}"),
			};

			match right.kind {
				BindingKind::Value(value) => {
					self.builder.build_store(left, value).unwrap();
				}

				BindingKind::Pointer { pointer: right_pointer, .. } => {
					let layout = type_store.type_layout(right.type_id);
					let align = layout.alignment as u32;
					let size = self.context.i64_type().const_int(layout.size as u64, false);
					self.builder.build_memcpy(left, align, right_pointer, align, size).unwrap();
				}
			}

			return None;
		}

		if matches!(
			op,
			BinaryOperator::AddAssign
				| BinaryOperator::SubAssign
				| BinaryOperator::MulAssign
				| BinaryOperator::DivAssign
				| BinaryOperator::ModuloAssign
				| BinaryOperator::BitshiftLeftAssign
				| BinaryOperator::BitshiftRightAssign
		) {
			let target = match left.kind {
				BindingKind::Pointer { pointer, .. } => pointer,
				BindingKind::Value(value) => unreachable!("{value:?}"),
			};
			let left = left.to_value(&self.builder).into_int_value();
			let right = right.to_value(&self.builder).into_int_value();
			assert_eq!(left.get_type(), right.get_type());

			match op {
				BinaryOperator::AddAssign => {
					let int = self.builder.build_int_add(left, right, "").unwrap();
					self.builder.build_store(target, BasicValueEnum::IntValue(int)).unwrap();
					return None;
				}

				BinaryOperator::SubAssign => {
					let int = self.builder.build_int_sub(left, right, "").unwrap();
					self.builder.build_store(target, BasicValueEnum::IntValue(int)).unwrap();
					return None;
				}

				BinaryOperator::MulAssign => {
					let int = self.builder.build_int_mul(left, right, "").unwrap();
					self.builder.build_store(target, BasicValueEnum::IntValue(int)).unwrap();
					return None;
				}

				BinaryOperator::DivAssign => {
					let int = if source_type_id.numeric_kind(type_store).unwrap().is_signed() {
						self.builder.build_int_signed_div(left, right, "").unwrap()
					} else {
						self.builder.build_int_unsigned_div(left, right, "").unwrap()
					};

					self.builder.build_store(target, BasicValueEnum::IntValue(int)).unwrap();
					return None;
				}

				BinaryOperator::ModuloAssign => {
					let value = if source_type_id.numeric_kind(type_store).unwrap().is_signed() {
						use IntPredicate::*;
						let zero = left.get_type().const_zero();
						let srem_result = self.builder.build_int_signed_rem(left, right, "").unwrap();

						let absolute_value = {
							let negated = self.builder.build_int_neg(right, "").unwrap();
							let is_negative = self.builder.build_int_compare(SLT, right, zero, "").unwrap();
							let selected = self.builder.build_select(is_negative, negated, right, "").unwrap();
							selected.into_int_value()
						};

						let is_negative = self.builder.build_int_compare(SLT, srem_result, zero, "").unwrap();
						let selected = self.builder.build_select(is_negative, absolute_value, zero, "").unwrap();
						let addened = selected.into_int_value();

						let result = self.builder.build_int_add(srem_result, addened, "").unwrap();
						BasicValueEnum::IntValue(result)
					} else {
						let int = self.builder.build_int_unsigned_rem(left, right, "").unwrap();
						BasicValueEnum::IntValue(int)
					};

					self.builder.build_store(target, value).unwrap();
					return None;
				}

				BinaryOperator::BitshiftLeftAssign => {
					let int = self.builder.build_left_shift(left, right, "").unwrap();
					self.builder.build_store(target, BasicValueEnum::IntValue(int)).unwrap();
					return None;
				}

				BinaryOperator::BitshiftRightAssign => {
					let int = if source_type_id.numeric_kind(type_store).unwrap().is_signed() {
						self.builder.build_right_shift(left, right, true, "").unwrap()
					} else {
						self.builder.build_right_shift(left, right, false, "").unwrap()
					};
					self.builder.build_store(target, BasicValueEnum::IntValue(int)).unwrap();
					return None;
				}

				_ => unreachable!(),
			}
		}

		let left = left.to_value(&self.builder);
		let right = right.to_value(&self.builder);

		if matches!(op, BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr) {
			let mut left = left.into_int_value();
			if left.get_type().get_bit_width() > 1 {
				left = self.builder.build_int_truncate(left, self.context.bool_type(), "").unwrap();
			}

			let mut right = right.into_int_value();
			if right.get_type().get_bit_width() > 1 {
				right = self.builder.build_int_truncate(right, self.context.bool_type(), "").unwrap();
			}

			assert_eq!(left.get_type(), right.get_type());

			let result = match op {
				BinaryOperator::LogicalAnd => self.builder.build_and(left, right, "").unwrap(),
				BinaryOperator::LogicalOr => self.builder.build_or(left, right, "").unwrap(),
				_ => unreachable!("{op:?}"),
			};

			let kind = BindingKind::Value(BasicValueEnum::IntValue(result));
			return Some(Binding { type_id: result_type_id, kind });
		}

		assert_eq!(left.get_type(), right.get_type());

		let value = if left.is_int_value() {
			let left = left.into_int_value();
			let right = right.into_int_value();

			use IntPredicate::*;
			match op {
				BinaryOperator::Add => {
					let int = self.builder.build_int_add(left, right, "").unwrap();
					BasicValueEnum::IntValue(int)
				}

				BinaryOperator::Sub => {
					let int = self.builder.build_int_sub(left, right, "").unwrap();
					BasicValueEnum::IntValue(int)
				}

				BinaryOperator::Mul => {
					let int = self.builder.build_int_mul(left, right, "").unwrap();
					BasicValueEnum::IntValue(int)
				}

				BinaryOperator::Div => {
					let int = if source_type_id.numeric_kind(type_store).unwrap().is_signed() {
						self.builder.build_int_signed_div(left, right, "").unwrap()
					} else {
						self.builder.build_int_unsigned_div(left, right, "").unwrap()
					};
					BasicValueEnum::IntValue(int)
				}

				BinaryOperator::Modulo => {
					if source_type_id.numeric_kind(type_store).unwrap().is_signed() {
						let zero = left.get_type().const_zero();
						let srem_result = self.builder.build_int_signed_rem(left, right, "").unwrap();

						let absolute_value = {
							let negated = self.builder.build_int_neg(right, "").unwrap();
							let is_negative = self.builder.build_int_compare(SLT, right, zero, "").unwrap();
							let selected = self.builder.build_select(is_negative, negated, right, "").unwrap();
							selected.into_int_value()
						};

						let is_negative = self.builder.build_int_compare(SLT, srem_result, zero, "").unwrap();
						let selected = self.builder.build_select(is_negative, absolute_value, zero, "").unwrap();
						let addened = selected.into_int_value();

						let result = self.builder.build_int_add(srem_result, addened, "").unwrap();
						BasicValueEnum::IntValue(result)
					} else {
						let int = self.builder.build_int_unsigned_rem(left, right, "").unwrap();
						BasicValueEnum::IntValue(int)
					}
				}

				BinaryOperator::BitshiftLeft => {
					let int = self.builder.build_left_shift(left, right, "").unwrap();
					BasicValueEnum::IntValue(int)
				}

				BinaryOperator::BitshiftRight => {
					let int = if source_type_id.numeric_kind(type_store).unwrap().is_signed() {
						self.builder.build_right_shift(left, right, true, "").unwrap()
					} else {
						self.builder.build_right_shift(left, right, false, "").unwrap()
					};
					BasicValueEnum::IntValue(int)
				}

				BinaryOperator::Equals => {
					let result = self.builder.build_int_compare(EQ, left, right, "").unwrap();
					BasicValueEnum::IntValue(result)
				}

				BinaryOperator::NotEquals => {
					let result = self.builder.build_int_compare(NE, left, right, "").unwrap();
					BasicValueEnum::IntValue(result)
				}

				BinaryOperator::GreaterThan => {
					let result = if source_type_id.numeric_kind(type_store).unwrap().is_signed() {
						self.builder.build_int_compare(SGT, left, right, "").unwrap()
					} else {
						self.builder.build_int_compare(UGT, left, right, "").unwrap()
					};
					BasicValueEnum::IntValue(result)
				}

				BinaryOperator::GreaterThanEquals => {
					let result = if source_type_id.numeric_kind(type_store).unwrap().is_signed() {
						self.builder.build_int_compare(SGE, left, right, "").unwrap()
					} else {
						self.builder.build_int_compare(UGE, left, right, "").unwrap()
					};
					BasicValueEnum::IntValue(result)
				}

				BinaryOperator::LessThan => {
					let result = if source_type_id.numeric_kind(type_store).unwrap().is_signed() {
						self.builder.build_int_compare(SLT, left, right, "").unwrap()
					} else {
						self.builder.build_int_compare(ULT, left, right, "").unwrap()
					};
					BasicValueEnum::IntValue(result)
				}

				BinaryOperator::LessThanEquals => {
					let result = if source_type_id.numeric_kind(type_store).unwrap().is_signed() {
						self.builder.build_int_compare(SLE, left, right, "").unwrap()
					} else {
						self.builder.build_int_compare(ULE, left, right, "").unwrap()
					};
					BasicValueEnum::IntValue(result)
				}

				BinaryOperator::Assign
				| BinaryOperator::AddAssign
				| BinaryOperator::SubAssign
				| BinaryOperator::MulAssign
				| BinaryOperator::DivAssign
				| BinaryOperator::ModuloAssign
				| BinaryOperator::BitshiftLeftAssign
				| BinaryOperator::BitshiftRightAssign
				| BinaryOperator::LogicalAnd
				| BinaryOperator::LogicalOr => unreachable!(),
			}
		} else {
			let left = left.into_float_value();
			let right = right.into_float_value();

			use FloatPredicate::*;
			match op {
				BinaryOperator::Add => {
					let float = self.builder.build_float_add(left, right, "").unwrap();
					BasicValueEnum::FloatValue(float)
				}

				BinaryOperator::Sub => {
					let float = self.builder.build_float_sub(left, right, "").unwrap();
					BasicValueEnum::FloatValue(float)
				}

				BinaryOperator::Mul => {
					let float = self.builder.build_float_mul(left, right, "").unwrap();
					BasicValueEnum::FloatValue(float)
				}

				BinaryOperator::Div => {
					let float = self.builder.build_float_div(left, right, "").unwrap();
					BasicValueEnum::FloatValue(float)
				}

				BinaryOperator::Equals => {
					let result = self.builder.build_float_compare(OEQ, left, right, "").unwrap();
					BasicValueEnum::IntValue(result)
				}

				BinaryOperator::NotEquals => {
					let result = self.builder.build_float_compare(ONE, left, right, "").unwrap();
					BasicValueEnum::IntValue(result)
				}

				BinaryOperator::GreaterThan => {
					let result = self.builder.build_float_compare(OGT, left, right, "").unwrap();
					BasicValueEnum::IntValue(result)
				}

				BinaryOperator::GreaterThanEquals => {
					let result = self.builder.build_float_compare(OGE, left, right, "").unwrap();
					BasicValueEnum::IntValue(result)
				}

				BinaryOperator::LessThan => {
					let result = self.builder.build_float_compare(OLT, left, right, "").unwrap();
					BasicValueEnum::IntValue(result)
				}

				BinaryOperator::LessThanEquals => {
					let result = self.builder.build_float_compare(OLE, left, right, "").unwrap();
					BasicValueEnum::IntValue(result)
				}

				BinaryOperator::Assign
				| BinaryOperator::AddAssign
				| BinaryOperator::SubAssign
				| BinaryOperator::MulAssign
				| BinaryOperator::DivAssign
				| BinaryOperator::Modulo
				| BinaryOperator::ModuloAssign
				| BinaryOperator::BitshiftLeft
				| BinaryOperator::BitshiftLeftAssign
				| BinaryOperator::BitshiftRight
				| BinaryOperator::BitshiftRightAssign
				| BinaryOperator::LogicalAnd
				| BinaryOperator::LogicalOr => unreachable!(),
			}
		};

		let kind = BindingKind::Value(value);
		Some(Binding { type_id: result_type_id, kind })
	}

	fn generate_binding(&mut self, readable_index: usize, value: Option<Self::Binding>, type_id: TypeId) {
		assert_eq!(self.readables.len(), readable_index);
		let Some(value) = value else {
			self.readables.push(None);
			return;
		};

		let (pointer, pointed_type) = match value.kind {
			BindingKind::Value(value) => {
				let alloca = self.build_alloca(value.get_type());
				self.builder.build_store(alloca, value).unwrap();
				(alloca, value.get_type())
			}

			BindingKind::Pointer { pointer, pointed_type } => (pointer, pointed_type),
		};

		let kind = BindingKind::Pointer { pointer, pointed_type };
		let binding = Binding { type_id, kind };
		self.readables.push(Some(binding));
	}

	fn generate_return(&mut self, function_id: FunctionId, value: Option<Self::Binding>) {
		let maybe_function = &self.functions[function_id.function_shape_index][function_id.specialization_index];
		let function = maybe_function.as_ref().unwrap();

		let mut abi = self.abi.take().unwrap();
		abi.return_value(self.context, &mut self.builder, function, value);
		self.abi = Some(abi);
	}

	fn generate_non_null_invalid_pointer(&mut self, pointer_type_id: TypeId) -> Self::Binding {
		let value = self.context.i64_type().const_int(1, false);
		let pointer_type = self.llvm_types.opaque_pointer;
		let pointer = self.builder.build_int_to_ptr(value, pointer_type, "").unwrap();

		let kind = BindingKind::Value(BasicValueEnum::PointerValue(pointer));
		Binding { type_id: pointer_type_id, kind }
	}

	fn generate_non_null_invalid_slice(&mut self, slice_type_id: TypeId, len: u64) -> Self::Binding {
		let alignment = self.context.i64_type().const_int(1, false);
		let pointer_type = self.llvm_types.opaque_pointer;
		let pointer = self.builder.build_int_to_ptr(alignment, pointer_type, "").unwrap();

		let len = BasicValueEnum::IntValue(self.context.i64_type().const_int(len, false));
		let fields = &[BasicValueEnum::PointerValue(pointer), len];
		let slice = self.llvm_types.slice_struct.const_named_struct(fields);

		let kind = BindingKind::Value(BasicValueEnum::StructValue(slice));
		Binding { type_id: slice_type_id, kind }
	}

	fn finalize_generator(&mut self) {
		self.finalize_function_if_in_function();
	}
}
