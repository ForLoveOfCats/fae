use crate::ir::{Function, FunctionId};
use crate::type_store::{NumericKind, TypeStore};
use crate::validator::FunctionStore;

pub trait Generator {
	type Binding: Clone + Copy;

	fn register_type_descriptions(&mut self, type_store: &mut TypeStore);

	fn register_functions(&mut self, type_store: &TypeStore, function_store: &FunctionStore);

	fn start_function(&mut self, type_store: &TypeStore, function: &Function, function_id: FunctionId);

	fn generate_integer_value(&mut self, kind: NumericKind, value: i128) -> Self::Binding;

	fn generate_struct_literal(
		&mut self,
		shape_index: usize,
		specialization_index: usize,
		fields: &[Self::Binding],
	) -> Self::Binding;

	fn generate_call(&mut self, function_id: FunctionId, arguments: &[Option<Self::Binding>]) -> Option<Self::Binding>;

	fn generate_read(&mut self, readable_index: usize) -> Option<Self::Binding>;

	fn generate_binding(&mut self, readable_index: usize, value: Option<Self::Binding>);

	fn generate_return(&mut self, function_id: FunctionId, value: Option<Self::Binding>);

	fn finalize_generator(&mut self);
}
