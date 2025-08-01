use rust_decimal::Decimal;

use crate::codegen::codegen;
use crate::frontend::function_store::FunctionStore;
use crate::frontend::ir::{Block, CheckIs, Expression, For, Function, FunctionId, IfElseChain, Match};
use crate::frontend::lang_items::LangItems;
use crate::frontend::span::DebugLocation;
use crate::frontend::symbols::Statics;
use crate::frontend::tree::{self, BinaryOperator};
use crate::frontend::type_store::{NumericKind, TypeId, TypeStore};

// TODO: Rip out this abstraction, it's causing more touble than it's worth. It didn't end up moving
// very much common logic out of the generator and resulted in plenty of complications
pub trait Generator {
	type Binding: Clone + Copy + std::fmt::Debug;

	fn register_type_descriptions(&mut self, type_store: &mut TypeStore);

	fn register_statics(&mut self, type_store: &mut TypeStore, statics: &Statics);

	fn register_functions(
		&mut self,
		parsed_files: &[tree::File],
		type_store: &mut TypeStore,
		function_store: &FunctionStore,
		optimizing: bool,
	);

	fn start_function(&mut self, type_store: &TypeStore, function: &Function, function_id: FunctionId);

	fn start_block(&mut self);

	fn end_block(&mut self);

	fn start_block_expression(&mut self, type_store: &mut TypeStore, yield_target_index: usize, type_id: TypeId);

	fn end_block_expression(&mut self, yield_target_index: usize) -> Option<Self::Binding>;

	fn generate_if_else_chain<'a, 'b>(
		&mut self,
		context: &mut codegen::Context<'a, 'b>,
		chain_expression: &'b IfElseChain<'a>,
		condition_callback: impl FnMut(&mut codegen::Context<'a, 'b>, &mut Self, &'b Expression<'a>) -> Self::Binding,
		body_callback: impl FnMut(&mut codegen::Context<'a, 'b>, &mut Self, &'b Block<'a>, bool),
	);

	fn generate_match<'a, 'b>(
		&mut self,
		context: &mut codegen::Context<'a, 'b>,
		value: Self::Binding,
		enum_shape_index: usize,
		enum_specialization_index: usize,
		match_expression: &'b Match<'a>,
		body_callback: impl FnMut(&mut codegen::Context<'a, 'b>, &mut Self, &'b Block<'a>),
	);

	// I hate this API but it has to pass the context back through to avoid having both closures
	// have to capture a mutable reference at the same time
	fn generate_while<'a, 'b>(
		&mut self,
		context: &mut codegen::Context<'a, 'b>,
		debug_location: DebugLocation,
		condition_callback: impl FnOnce(&mut codegen::Context<'a, 'b>, &mut Self) -> Self::Binding,
		body_callback: impl FnOnce(&mut codegen::Context<'a, 'b>, &mut Self),
	);

	fn generate_for_array<'a, 'b>(
		&mut self,
		context: &mut codegen::Context<'a, 'b>,
		statement: &'b For<'a>,
		initializer: Self::Binding,
		debug_location: DebugLocation,
		body_callback: impl FnOnce(&mut codegen::Context<'a, 'b>, &mut Self),
	);

	fn generate_for_slice<'a, 'b>(
		&mut self,
		context: &mut codegen::Context<'a, 'b>,
		statement: &'b For<'a>,
		initializer: Self::Binding,
		debug_location: DebugLocation,
		body_callback: impl FnOnce(&mut codegen::Context<'a, 'b>, &mut Self),
	);

	fn generate_for_range<'a, 'b>(
		&mut self,
		context: &mut codegen::Context<'a, 'b>,
		statement: &'b For<'a>,
		initializer: Self::Binding,
		debug_location: DebugLocation,
		body_callback: impl FnOnce(&mut codegen::Context<'a, 'b>, &mut Self),
	);

	fn generate_number_value(&mut self, type_store: &TypeStore, type_id: TypeId, value: Decimal) -> Self::Binding;

	fn generate_boolean_literal(&mut self, type_store: &TypeStore, literal: bool) -> Self::Binding;

	fn generate_string_literal(&mut self, type_store: &TypeStore, text: &str) -> Self::Binding;

	fn generate_array_literal(
		&mut self,
		type_store: &mut TypeStore,
		elements: &[Self::Binding],
		element_type_id: TypeId,
		array_type_id: TypeId,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_slice_literal(
		&mut self,
		type_store: &mut TypeStore,
		elements: &[Self::Binding],
		element_type_id: TypeId,
		slice_type_id: TypeId,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_struct_literal(
		&mut self,
		type_id: TypeId,
		shape_index: usize,
		specialization_index: usize,
		fields: &[Self::Binding],
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_integer_bitflags_literal(
		&mut self,
		type_id: TypeId,
		tag_kind: NumericKind,
		shape_index: usize,
		specialization_index: usize,
		value: u64,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_call(
		&mut self,
		type_store: &mut TypeStore,
		function_id: FunctionId,
		arguments: &[Option<Self::Binding>],
		debug_location: DebugLocation,
	) -> Option<Self::Binding>;

	fn generate_method_call(
		&mut self,
		type_store: &mut TypeStore,
		function_id: FunctionId,
		base_pointer_type_id: TypeId,
		arguments: &mut [Option<Self::Binding>],
		debug_location: DebugLocation,
	) -> Option<Self::Binding>;

	fn generate_read(&mut self, readable_index: usize) -> Option<Self::Binding>;

	fn generate_static_read(&mut self, static_index: usize) -> Self::Binding;

	fn generate_field_read(
		&mut self,
		lang_items: &LangItems,
		type_store: &mut TypeStore,
		base: Self::Binding,
		field_index: usize,
		debug_location: DebugLocation,
	) -> Option<Self::Binding>;

	fn generate_negate(&mut self, value: Self::Binding, type_id: TypeId, debug_location: DebugLocation) -> Self::Binding;

	fn generate_invert(&mut self, value: Self::Binding, debug_location: DebugLocation) -> Self::Binding;

	fn generate_bitwise_not(
		&mut self,
		type_store: &mut TypeStore,
		value: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_address_of(
		&mut self,
		base: Self::Binding,
		pointer_type_id: TypeId,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_dereference(
		&mut self,
		type_store: &mut TypeStore,
		base: Self::Binding,
		pointed_type_id: TypeId,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_cast(
		&mut self,
		type_store: &mut TypeStore,
		base: Self::Binding,
		to: TypeId,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_array_index(
		&mut self,
		lang_items: &LangItems,
		type_store: &mut TypeStore,
		item_type: TypeId,
		base: Option<Self::Binding>,
		base_type_id: TypeId,
		index: Self::Binding,
		debug_location: DebugLocation,
	) -> Option<Self::Binding>;

	fn generate_slice_index(
		&mut self,
		lang_items: &LangItems,
		type_store: &mut TypeStore,
		item_type: TypeId,
		base: Self::Binding,
		index: Self::Binding,
		debug_location: DebugLocation,
	) -> Option<Self::Binding>;

	fn generate_array_slice(
		&mut self,
		lang_items: &LangItems,
		type_store: &mut TypeStore,
		item_type_id: TypeId,
		base: Self::Binding,
		range: Self::Binding,
		debug_location: DebugLocation,
	) -> Option<Self::Binding>;

	fn generate_slice_slice(
		&mut self,
		lang_items: &LangItems,
		type_store: &mut TypeStore,
		item_type_id: TypeId,
		base: Self::Binding,
		range: Self::Binding,
		debug_location: DebugLocation,
	) -> Option<Self::Binding>;

	fn generate_binary_operation<'a, 'b>(
		&mut self,
		context: &mut codegen::Context<'a, 'b>,
		left: &'b Expression<'a>,
		right: &'b Expression<'a>,
		op: BinaryOperator,
		source_type_id: TypeId,
		result_type_id: TypeId,
		debug_location: DebugLocation,
	) -> Option<Self::Binding>;

	fn generate_check_is(
		&mut self,
		context: &mut codegen::Context,
		value: Self::Binding,
		enum_shape_index: usize,
		enum_specialization_index: usize,
		check_expression: &CheckIs,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_enum_variant_to_enum(
		&mut self,
		type_store: &mut TypeStore,
		enum_type_id: TypeId,
		enum_shape_index: usize,
		enum_specialization_index: usize,
		tag_value: i128,
		variant_binding: Option<Self::Binding>,
	) -> Self::Binding;

	fn generate_union_variant_to_union(
		&mut self,
		type_store: &mut TypeStore,
		union_type_id: TypeId,
		union_shape_index: usize,
		union_specialization_index: usize,
		variant_binding: Option<Self::Binding>,
	) -> Self::Binding;

	fn generate_initialized_binding(
		&mut self,
		readable_index: usize,
		value: Option<Self::Binding>,
		type_id: TypeId,
		name: &str,
		debug_location: DebugLocation,
	);

	fn generate_zero_initialized_binding(
		&mut self,
		type_store: &mut TypeStore,
		readable_index: usize,
		type_id: TypeId,
		name: &str,
		debug_location: DebugLocation,
	);

	fn generate_break(&mut self, loop_index: usize, debug_location: DebugLocation);

	fn generate_continue(&mut self, loop_index: usize, debug_location: DebugLocation);

	fn generate_yield<'a, 'b>(
		&mut self,
		context: &mut codegen::Context<'a, 'b>,
		yield_target_index: usize,
		value: Option<Self::Binding>,
		debug_location: DebugLocation,
		defer_callback: impl FnOnce(&mut codegen::Context<'a, 'b>, &mut Self),
	);

	fn generate_return<'a, 'b>(
		&mut self,
		context: &mut codegen::Context<'a, 'b>,
		function_id: FunctionId,
		value: Option<Self::Binding>,
		debug_location: DebugLocation,
		defer_callback: impl FnOnce(&mut codegen::Context<'a, 'b>, &mut Self),
	);

	fn generate_slice(
		&mut self,
		slice_type_id: TypeId,
		pointer: Self::Binding,
		length: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_non_null_invalid_pointer(&mut self, pointer_type_id: TypeId, debug_location: DebugLocation) -> Self::Binding;

	fn generate_non_null_invalid_slice(
		&mut self,
		slice_type_id: TypeId,
		length: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_debugger_break(&mut self, debug_location: DebugLocation);

	fn generate_min_i8(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_min_i16(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_min_i32(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_min_i64(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_min_u8(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_min_u16(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_min_u32(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_min_u64(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_min_isize(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_min_usize(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_min_f32(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_min_f64(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_max_i8(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_max_i16(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_max_i32(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_max_i64(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_max_u8(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_max_u16(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_max_u32(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_max_u64(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_max_isize(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_max_usize(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_max_f32(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_max_f64(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_round_f32(
		&mut self,
		type_store: &TypeStore,
		input: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding;

	fn generate_round_f64(
		&mut self,
		type_store: &TypeStore,
		input: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding;

	// TODO: Remove this in favor of a "end_function"
	fn finalize_generator(&mut self);
}
