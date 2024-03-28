use crate::ir::Function;
use crate::type_store::TypeStore;

pub trait Generator {
	type Binding;

	fn register_type_descriptions(&mut self, type_store: &mut TypeStore);

	fn start_function(&mut self, type_store: &TypeStore, function: &Function, name: &str);

	fn finalize_generator(&mut self);
}
