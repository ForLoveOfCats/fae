use crate::frontend::error::Messages;
use crate::frontend::ir::{
	Function, FunctionId, FunctionShape, FunctionSpecializationResult, GenericParameters, GenericUsage, Parameter, TypeArguments,
};
use crate::frontend::span::Span;
use crate::frontend::type_store::TypeStore;

#[derive(Debug)]
pub struct FunctionStore<'a> {
	pub shapes: Vec<FunctionShape<'a>>,

	// Need to have a copy of each shape's generic parameters around before
	// the shape has been fully constructed so signature types can be looked up
	pub generics: Vec<GenericParameters<'a>>,

	pub main: Option<FunctionId>,
}

impl<'a> FunctionStore<'a> {
	pub fn new() -> Self {
		FunctionStore { shapes: Vec::new(), generics: Vec::new(), main: None }
	}

	fn get_specialization(
		&self,
		type_store: &TypeStore<'a>,
		function_shape_index: usize,
		type_arguments: &TypeArguments,
	) -> Option<FunctionSpecializationResult> {
		let shape = &self.shapes[function_shape_index];
		for (specialization_index, existing) in shape.specializations.iter().enumerate() {
			if existing.type_arguments.direct_matches(type_arguments, type_store) {
				return Some(FunctionSpecializationResult { specialization_index, return_type: existing.return_type });
			}
		}

		None
	}

	pub fn get_or_add_specialization(
		&mut self,
		messages: &mut Messages<'a>,
		type_store: &mut TypeStore<'a>,
		module_path: &'a [String],
		generic_usages: &mut Vec<GenericUsage>,
		function_shape_index: usize,
		invoke_span: Option<Span>,
		type_arguments: TypeArguments,
	) -> Option<FunctionSpecializationResult> {
		let shape = &self.shapes[function_shape_index];

		if shape.generic_parameters.explicit_len() != type_arguments.explicit_len() {
			let expected = shape.generic_parameters.explicit_len();
			let got = type_arguments.explicit_len();
			let error = error!("Expected {expected} type arguments, got {got}");
			messages.message(error.span_if_some(invoke_span));
			return None;
		}

		// TODO: Should this be an ICE instead?
		if shape.generic_parameters.implicit_len() != type_arguments.implicit_len() {
			let expected = shape.generic_parameters.implicit_len();
			let got = type_arguments.implicit_len();
			let error = error!("Expected {expected} implicit type arguments, got {got}");
			messages.message(error.span_if_some(invoke_span));
			return None;
		}

		if let Some(result) = self.get_specialization(type_store, function_shape_index, &type_arguments) {
			return Some(result);
		}

		let shape = &self.shapes[function_shape_index];
		let generic_poisioned = type_arguments
			.ids()
			.iter()
			.any(|id| type_store.type_entries[id.index()].generic_poisoned);

		let parameters = shape
			.parameters
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

				let is_mutable = parameter.is_mutable;
				let readable_index = parameter.readable_index;
				Parameter { type_id, readable_index, is_mutable }
			})
			.collect::<Vec<_>>();

		let return_type = type_store.specialize_with_function_generics(
			messages,
			self,
			module_path,
			generic_usages,
			function_shape_index,
			&type_arguments,
			shape.return_type,
		);

		let specialization_index = shape.specializations.len();
		let concrete = Function {
			type_arguments: type_arguments.clone(),
			generic_poisioned,
			parameters,
			return_type,
			been_queued: false,
			been_generated: false,
		};
		let shape = &mut self.shapes[function_shape_index];
		shape.specializations.push(concrete);

		if generic_poisioned {
			let usage = GenericUsage::Function { type_arguments, function_shape_index };
			generic_usages.push(usage)
		} else {
			let function_type_arguments = type_arguments;

			for generic_usage in shape.generic_usages.clone() {
				generic_usage.apply_specialization(
					messages,
					type_store,
					self,
					module_path,
					generic_usages,
					function_shape_index,
					&function_type_arguments,
					invoke_span,
				);
			}
		}

		Some(FunctionSpecializationResult { specialization_index, return_type })
	}

	pub fn specialize_with_function_generics(
		&self,
		messages: &mut Messages<'a>,
		type_store: &mut TypeStore<'a>,
		function_id: FunctionId,
		caller_shape_index: usize,
		caller_type_arguments: &TypeArguments,
	) -> FunctionId {
		let shape = &self.shapes[function_id.function_shape_index];
		let specialization = &shape.specializations[function_id.specialization_index];
		if specialization.type_arguments.is_empty() {
			return function_id;
		}

		let generic_poisoned = specialization
			.type_arguments
			.ids()
			.iter()
			.any(|id| type_store.type_entries[id.index()].generic_poisoned);
		if !generic_poisoned {
			return function_id;
		}

		let mut generic_usages = Vec::new();
		let mut type_arguments = specialization.type_arguments.clone();
		type_arguments.specialize_with_function_generics(
			messages,
			type_store,
			self,
			shape.module_path,
			&mut generic_usages,
			caller_shape_index,
			caller_type_arguments,
		);

		let result = self
			.get_specialization(type_store, function_id.function_shape_index, &type_arguments)
			.unwrap();
		assert!(generic_usages.is_empty());

		FunctionId {
			function_shape_index: function_id.function_shape_index,
			specialization_index: result.specialization_index,
		}
	}
}
