use crate::frontend::function_store::FunctionStore;
use crate::frontend::ir::{Function, FunctionId};
use crate::frontend::lang_items::LangItems;
use crate::frontend::span::Span;
use crate::frontend::tree::BinaryOperator;
use crate::frontend::type_store::{TypeId, TypeStore};

/*
TODO: Move more zero-sized-type handling logic out of generator implementations and
into the codegen driver. The primary roadblock currently is that the validator cannot
fully reason about which values are zero-sized as generic types have an unknown size
so it is forced to emit readable indicies for zero-sized bindings. This then requires that the
generator know that these indicies do not have a runtime value and it virally infects all
expression handling logic.

Perhaps the validator can process all function specializations and generate readable
indicies at that point so it can skip zero-sized bindings. This would require the validator to
emit pre-specialized IR for each function version which seems necessary for const-eval
anyway so that is probably the path forward. This should be done at the same time as
a significant IR flattening to make it cheaper to construct and interpret.
*/

pub trait Generator {
	type Binding: Clone + Copy;

	fn register_type_descriptions(&mut self, type_store: &TypeStore);

	fn register_functions(&mut self, type_store: &TypeStore, function_store: &FunctionStore);

	fn start_function(&mut self, type_store: &TypeStore, function: &Function, function_id: FunctionId);

	fn start_block(&mut self);

	fn end_block(&mut self);

	fn generate_if(&mut self, condition: Self::Binding, body_callback: impl FnOnce(&mut Self));

	fn generate_integer_value(&mut self, type_store: &TypeStore, type_id: TypeId, value: i128) -> Self::Binding;

	fn generate_decimal_value(&mut self, type_store: &TypeStore, type_id: TypeId, value: f64) -> Self::Binding;

	fn generate_boolean_literal(&mut self, type_store: &TypeStore, literal: bool) -> Self::Binding;

	fn generate_string_literal(&mut self, type_store: &TypeStore, text: &str) -> Self::Binding;

	fn generate_array_literal(
		&mut self,
		type_store: &TypeStore,
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
		type_store: &TypeStore,
		function_id: FunctionId,
		arguments: &[Option<Self::Binding>],
	) -> Option<Self::Binding>;

	fn generate_read(&mut self, readable_index: usize) -> Option<Self::Binding>;

	fn generate_field_read(&mut self, type_store: &TypeStore, base: Self::Binding, field_index: usize) -> Option<Self::Binding>;

	fn generate_address_of(&mut self, base: Self::Binding, pointer_type_id: TypeId) -> Self::Binding;

	fn generate_dereference(&mut self, type_store: &TypeStore, base: Self::Binding, pointed_type_id: TypeId) -> Self::Binding;

	fn generate_cast(&mut self, type_store: &TypeStore, base: Self::Binding, to: TypeId) -> Self::Binding;

	fn generate_slice_index(
		&mut self,
		lang_items: &LangItems,
		type_store: &TypeStore,
		item_type: TypeId,
		base: Self::Binding,
		index: Self::Binding,
		index_span: Span,
	) -> Option<Self::Binding>;

	fn generate_assign(&mut self, type_store: &TypeStore, left: Self::Binding, right: Self::Binding);

	fn generate_binary_operation(
		&mut self,
		type_store: &TypeStore,
		left: Self::Binding,
		right: Self::Binding,
		op: BinaryOperator,
		source_type_id: TypeId,
		result_type_id: TypeId,
	) -> Option<Self::Binding>;

	fn generate_binding(&mut self, readable_index: usize, value: Option<Self::Binding>, type_id: TypeId);

	fn generate_return(&mut self, function_id: FunctionId, value: Option<Self::Binding>);

	// TODO: Remove this in favor of a "end_function"
	fn finalize_generator(&mut self);
}
