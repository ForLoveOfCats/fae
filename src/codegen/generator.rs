use crate::ir::Function;
use crate::type_store::TypeStore;

pub trait Generator {
	type Binding;

	fn start_function(&mut self, type_store: &TypeStore, name: &str, function: &Function);
}
