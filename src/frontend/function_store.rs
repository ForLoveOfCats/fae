use crate::frontend::error::Messages;
use crate::frontend::ir::{
	Function, FunctionId, FunctionShape, FunctionSpecializationResult, GenericParameters, GenericUsage, Parameter, TypeArguments,
};
use crate::frontend::span::Span;
use crate::frontend::type_store::{ImplementationStatus, TypeStore};
use crate::lock::RwLock;
use crate::reference::{Ref, SliceRef};

#[derive(Debug)]
pub struct FunctionStore<'a> {
	pub shapes: RwLock<Vec<Option<Ref<RwLock<FunctionShape<'a>>>>>>,

	// Need to have a copy of each shape's generic parameters around before
	// the shape has been fully constructed so signature types can be looked up
	pub generics: RwLock<Vec<GenericParameters<'a>>>,

	pub main: RwLock<Option<FunctionId>>,
}

impl<'a> FunctionStore<'a> {
	pub fn new() -> Self {
		FunctionStore {
			shapes: RwLock::new(Vec::new()),
			generics: RwLock::new(Vec::new()),
			main: RwLock::new(None),
		}
	}

	pub fn get_or_add_specialization(
		&self,
		messages: &mut Messages<'a>,
		type_store: &mut TypeStore<'a>,
		module_path: &'a [String],
		generic_usages: &mut Vec<GenericUsage>,
		function_shape_index: usize,
		type_arguments: Ref<TypeArguments>,
		invoke_span: Option<Span>,
	) -> Option<FunctionSpecializationResult> {
		let _zone = zone!("function specialization");

		let lock = self.shapes.read()[function_shape_index].as_ref().unwrap().clone();
		let mut shape = lock.write();

		if shape.generic_parameters.explicit_len() != type_arguments.explicit_len {
			let expected = shape.generic_parameters.explicit_len();
			let got = type_arguments.explicit_len;
			let error = error!("Expected {expected} type arguments, got {got}");
			messages.message(error.span_if_some(invoke_span));
			return None;
		}

		assert_eq!(shape.generic_parameters.implicit_len(), type_arguments.implicit_len);
		assert_eq!(shape.generic_parameters.method_base_len(), type_arguments.method_base_len);

		if let Some(&specialization_index) = shape.specializations_by_type_arguments.get(&type_arguments) {
			let return_type = shape.specializations[specialization_index].return_type;
			return Some(FunctionSpecializationResult { specialization_index, return_type });
		}

		let mut constraint_failure = false;
		let generic_parameters_iter = shape.generic_parameters.explicit_parameters().iter();
		let generic_arguments_iter = type_arguments.explicit_ids().iter();
		for (type_parameter, &type_argument) in generic_parameters_iter.zip(generic_arguments_iter) {
			for &contraint in type_parameter.constraints.iter() {
				match type_store.check_type_implements_trait(messages, self, module_path, type_argument, contraint) {
					ImplementationStatus::Implemented => {}
					ImplementationStatus::NotImplemented => constraint_failure = true,
				}
			}
		}

		if constraint_failure {
			return None;
		}

		let generic_poisoned = type_arguments
			.ids
			.iter()
			.any(|id| type_store.type_entries.get(id.item).generic_poisoned);

		let unspecialized_return_type = shape.return_type;
		let parameters = shape.parameters.clone();

		let parameters = parameters
			.item
			.iter()
			.map(|parameter| {
				let type_id = type_store.specialize_with_function_generics(
					messages,
					self,
					module_path,
					generic_usages,
					function_shape_index,
					&type_arguments,
					parameter.type_id,
				);

				Parameter { type_id, is_mutable: parameter.is_mutable }
			})
			.collect::<Vec<_>>();

		let return_type = type_store.specialize_with_function_generics(
			messages,
			self,
			module_path,
			generic_usages,
			function_shape_index,
			&type_arguments,
			unspecialized_return_type.item,
		);

		let specialization_index = shape.specializations.len();
		let concrete = Function {
			type_arguments: type_arguments.clone(),
			generic_poisoned,
			parameters: SliceRef::from(parameters),
			return_type,
		};

		shape.specializations.push(concrete);
		shape
			.specializations_by_type_arguments
			.insert(type_arguments.clone(), specialization_index);

		if generic_poisoned {
			let usage = GenericUsage::Function { type_arguments, function_shape_index };
			generic_usages.push(usage)
		} else {
			let shape_generic_usages = shape.generic_usages.clone();
			drop(shape);
			for generic_usage in shape_generic_usages.iter() {
				generic_usage.apply_specialization(
					messages,
					type_store,
					self,
					module_path,
					generic_usages,
					function_shape_index,
					&type_arguments,
					invoke_span,
				);
			}
		}

		Some(FunctionSpecializationResult { specialization_index, return_type })
	}

	pub fn specialize_function_with_function_generics(
		&self,
		messages: &mut Messages<'a>,
		type_store: &mut TypeStore<'a>,
		function_id: FunctionId,
		caller_shape_index: usize,
		caller_type_arguments: &TypeArguments,
	) -> FunctionId {
		let lock = self.shapes.read()[function_id.function_shape_index].as_ref().unwrap().clone();
		let shape = lock.read();
		let specialization = &shape.specializations[function_id.specialization_index];
		if specialization.type_arguments.is_empty() {
			return function_id;
		}

		let generic_poisoned = specialization
			.type_arguments
			.ids
			.iter()
			.any(|id| type_store.type_entries.get(id.item).generic_poisoned);
		if !generic_poisoned {
			return function_id;
		}

		let mut generic_usages = Vec::new();
		let mut type_arguments = TypeArguments::clone(&specialization.type_arguments);
		type_arguments.specialize_with_function_generics(
			messages,
			type_store,
			self,
			shape.module_path,
			&mut generic_usages,
			caller_shape_index,
			caller_type_arguments,
		);
		assert!(generic_usages.is_empty());

		let &specialization_index = shape.specializations_by_type_arguments.get(&type_arguments).unwrap();
		drop(shape);

		FunctionId {
			function_shape_index: function_id.function_shape_index,
			specialization_index,
		}
	}
}
