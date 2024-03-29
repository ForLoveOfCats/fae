use crate::ir::{Function, FunctionId};
use crate::type_store::TypeStore;
use crate::validator::FunctionStore;

pub trait Generator {
	type Binding;

	fn register_type_descriptions(&mut self, type_store: &mut TypeStore);

	fn register_functions(&mut self, type_store: &TypeStore, function_store: &FunctionStore);

	fn start_function(&mut self, type_store: &TypeStore, function: &Function, function_id: FunctionId);

	fn finalize_generator(&mut self);
}
