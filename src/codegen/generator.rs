use crate::codegen::codegen;
use crate::frontend::file::SourceFile;
use crate::frontend::function_store::FunctionStore;
use crate::frontend::ir::{Block, CheckIs, Expression, Function, FunctionId, IfElseChain, Match};
use crate::frontend::lang_items::LangItems;
use crate::frontend::span::DebugLocation;
use crate::frontend::symbols::Statics;
use crate::frontend::tree::BinaryOperator;
use crate::frontend::type_store::{TypeId, TypeStore};

// TODO: Rip out this abstraction, it's causing more touble than it's worth. It didn't end up moving
// very much common logic out of the generator and resulted in plenty of complications
pub trait Generator {
	type Binding: Clone + Copy + std::fmt::Debug;

	fn register_type_descriptions(&mut self, type_store: &mut TypeStore);

	fn register_statics(&mut self, type_store: &mut TypeStore, statics: &Statics);

	fn register_functions(
		&mut self,
		source_files: &[SourceFile],
		type_store: &mut TypeStore,
		function_store: &FunctionStore,
		optimizing: bool,
	);

	fn start_function(&mut self, type_store: &TypeStore, function: &Function, function_id: FunctionId);

	fn start_block(&mut self);

	fn end_block(&mut self);

	fn generate_if_else_chain(
		&mut self,
		context: &mut codegen::Context,
		chain_expression: &IfElseChain,
		condition_callback: impl FnMut(&mut codegen::Context, &mut Self, &Expression) -> Self::Binding,
		body_callback: impl FnMut(&mut codegen::Context, &mut Self, &Block, bool),
	);

	fn generate_match<'a>(
		&mut self,
		context: &mut codegen::Context,
		value: Self::Binding,
		enum_shape_index: usize,
		enum_specialization_index: usize,
		match_expression: &Match,
		body_callback: impl FnMut(&mut codegen::Context, &mut Self, &Block),
	);

	// I hate this API but it has to pass the context back through to avoid having both closures
	// have to capture a mutable reference at the same time
	fn generate_while(
		&mut self,
		context: &mut codegen::Context,
		debug_location: DebugLocation,
		condition_callback: impl FnOnce(&mut codegen::Context, &mut Self) -> Self::Binding,
		body_callback: impl FnOnce(&mut codegen::Context, &mut Self),
	);

	fn generate_integer_value(&mut self, type_store: &TypeStore, type_id: TypeId, value: i128) -> Self::Binding;

	fn generate_decimal_value(&mut self, type_store: &TypeStore, type_id: TypeId, value: f64) -> Self::Binding;

	fn generate_boolean_literal(&mut self, type_store: &TypeStore, literal: bool) -> Self::Binding;

	fn generate_string_literal(&mut self, type_store: &TypeStore, text: &str) -> Self::Binding;

	fn generate_array_literal(
		&mut self,
		type_store: &mut TypeStore,
		elements: &[Self::Binding],
		element_type_id: TypeId,
		slice_type_id: TypeId,
	) -> Self::Binding;

	fn generate_struct_literal(
		&mut self,
		type_id: TypeId,
		shape_index: usize,
		specialization_index: usize,
		fields: &[Self::Binding],
	) -> Self::Binding;

	fn generate_call(
		&mut self,
		type_store: &mut TypeStore,
		function_id: FunctionId,
		arguments: &[Option<Self::Binding>],
	) -> Option<Self::Binding>;

	fn generate_method_call(
		&mut self,
		type_store: &mut TypeStore,
		function_id: FunctionId,
		base_pointer_type_id: TypeId,
		arguments: &mut [Option<Self::Binding>],
	) -> Option<Self::Binding>;

	fn generate_read(&mut self, readable_index: usize) -> Option<Self::Binding>;

	fn generate_static_read(&mut self, static_index: usize) -> Self::Binding;

	fn generate_field_read(
		&mut self,
		type_store: &mut TypeStore,
		base: Self::Binding,
		field_index: usize,
	) -> Option<Self::Binding>;

	fn generate_negate(&mut self, value: Self::Binding, type_id: TypeId) -> Self::Binding;

	fn generate_invert(&mut self, value: Self::Binding) -> Self::Binding;

	fn generate_address_of(&mut self, base: Self::Binding, pointer_type_id: TypeId) -> Self::Binding;

	fn generate_dereference(&mut self, type_store: &mut TypeStore, base: Self::Binding, pointed_type_id: TypeId)
		-> Self::Binding;

	fn generate_cast(&mut self, type_store: &mut TypeStore, base: Self::Binding, to: TypeId) -> Self::Binding;

	fn generate_slice_index(
		&mut self,
		lang_items: &LangItems,
		type_store: &mut TypeStore,
		item_type: TypeId,
		base: Self::Binding,
		index: Self::Binding,
	) -> Option<Self::Binding>;

	fn generate_slice_slice(
		&mut self,
		lang_items: &LangItems,
		type_store: &mut TypeStore,
		item_type: TypeId,
		base: Self::Binding,
		range: Self::Binding,
	) -> Option<Self::Binding>;

	fn generate_binary_operation(
		&mut self,
		context: &mut codegen::Context,
		left: &Expression,
		right: &Expression,
		op: BinaryOperator,
		source_type_id: TypeId,
		result_type_id: TypeId,
	) -> Option<Self::Binding>;

	fn generate_check_is(
		&mut self,
		context: &mut codegen::Context,
		value: Self::Binding,
		enum_shape_index: usize,
		enum_specialization_index: usize,
		check_expression: &CheckIs,
	) -> Self::Binding;

	fn generate_enum_variant_to_enum(
		&mut self,
		type_store: &mut TypeStore,
		enum_type_id: TypeId,
		enum_shape_index: usize,
		enum_specialization_index: usize,
		variant_index: usize,
		variant_binding: Option<Self::Binding>,
	) -> Self::Binding;

	fn generate_binding(
		&mut self,
		readable_index: usize,
		value: Option<Self::Binding>,
		type_id: TypeId,
		name: &str,
		debug_location: DebugLocation,
	);

	fn generate_break(&mut self, loop_index: usize, debug_location: DebugLocation);

	fn generate_continue(&mut self, loop_index: usize, debug_location: DebugLocation);

	fn generate_return(&mut self, function_id: FunctionId, value: Option<Self::Binding>, debug_location: DebugLocation);

	fn generate_slice(&mut self, slice_type_id: TypeId, pointer: Self::Binding, length: Self::Binding) -> Self::Binding;

	fn generate_non_null_invalid_pointer(&mut self, pointer_type_id: TypeId) -> Self::Binding;

	fn generate_non_null_invalid_slice(&mut self, slice_type_id: TypeId, length: u64) -> Self::Binding;

	fn generate_debugger_break(&mut self);

	// TODO: Remove this in favor of a "end_function"
	fn finalize_generator(&mut self);
}
