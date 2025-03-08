use crate::frontend::error::Messages;
use crate::frontend::ir::{
	Function, FunctionId, FunctionShape, FunctionSpecializationResult, GenericParameters, GenericUsage, Parameter, TypeArguments,
};
use crate::frontend::span::Span;
use crate::frontend::tree::Node;
use crate::frontend::type_store::{TraitId, TypeEntryKind, TypeId, TypeIdSpecializationSituation, TypeStore, UserTypeKind};
use crate::lock::RwLock;
use crate::reference::{Ref, SliceRef};

#[derive(Debug, Clone)]
pub enum MethodBaseType {
	UserType { shape_index: usize, specialization_index: usize },
	Trait { trait_ids: SliceRef<TraitId>, self_type_id: TypeId },
	Other,
}

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
		enclosing_generic_parameters: &GenericParameters<'a>,
		function_shape_index: usize,
		invoke_span: Option<Span>,
		type_arguments: Ref<TypeArguments>,
	) -> Option<FunctionSpecializationResult> {
		let _zone = zone!("function specialization");

		let lock = self.shapes.read()[function_shape_index].as_ref().unwrap().clone();
		let shape = lock.read();

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
		let generic_parameters = shape.generic_parameters.clone();
		let generic_parameters_iter = generic_parameters.explicit_parameters().iter();
		drop(shape);
		let generic_arguments_iter = type_arguments.explicit_ids().iter();
		for (type_parameter, &type_argument) in generic_parameters_iter.zip(generic_arguments_iter) {
			for contraint in type_parameter.generic_constraints.iter() {
				let mut trait_type_arguments = TypeArguments::clone(&contraint.type_arguments);
				trait_type_arguments.specialize_with_generics(
					messages,
					type_store,
					self,
					module_path,
					generic_usages,
					enclosing_generic_parameters,
					TypeIdSpecializationSituation::Function { function_shape_index },
					&type_arguments,
				);

				if !type_store.check_type_implements_trait(
					messages,
					self,
					module_path,
					generic_usages,
					enclosing_generic_parameters,
					type_argument,
					contraint,
					Ref::new(trait_type_arguments),
					invoke_span,
				) {
					constraint_failure = true;
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

		let mut shape = lock.write();
		let unspecialized_return_type = shape.return_type;
		let parameters = shape.parameters.clone();

		let parameters = parameters
			.item
			.iter()
			.map(|parameter| {
				let type_id = type_store.specialize_type_id_with_generics(
					messages,
					self,
					module_path,
					generic_usages,
					enclosing_generic_parameters,
					parameter.type_id,
					&type_arguments,
					TypeIdSpecializationSituation::Function { function_shape_index },
				);

				Parameter { type_id, is_mutable: parameter.is_mutable }
			})
			.collect::<Vec<_>>();

		let return_type = type_store.specialize_type_id_with_generics(
			messages,
			self,
			module_path,
			generic_usages,
			enclosing_generic_parameters,
			unspecialized_return_type.item,
			&type_arguments,
			TypeIdSpecializationSituation::Function { function_shape_index },
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
					enclosing_generic_parameters,
					function_shape_index,
					&type_arguments,
					invoke_span,
				);
			}
		}

		Some(FunctionSpecializationResult { specialization_index, return_type })
	}

	pub fn get_method_function_specialization(
		&self,
		messages: &mut Messages<'a>,
		type_store: &mut TypeStore<'a>,
		module_path: &'a [String],
		generic_usages: &mut Vec<GenericUsage>,
		enclosing_generic_parameters: &GenericParameters<'a>,
		explicit_type_arguments: Vec<Node<TypeId>>,
		function_shape_index: usize,
		method_base_type: MethodBaseType,
		span: Option<Span>,
	) -> Option<FunctionSpecializationResult> {
		let mut type_arguments = TypeArguments::new_from_explicit(explicit_type_arguments);
		let lock = self.shapes.read()[function_shape_index].as_ref().unwrap().clone();
		let shape = lock.read();
		if shape.generic_parameters.implicit_len() != 0 {
			// The only functions with implicit generic parameters are inner functions, and if we have it
			// in scope then that means it must be somewhere within ourselves or our function parent chain
			let count = shape.generic_parameters.implicit_len();
			for parameter in &enclosing_generic_parameters.parameters()[0..count] {
				type_arguments.push_implicit(Node::new(parameter.generic_type_id, parameter.name.span));
			}
		}
		drop(shape);

		match method_base_type {
			MethodBaseType::UserType { shape_index, specialization_index } => {
				let user_type = type_store.user_types.read()[shape_index].clone();
				let user_type = user_type.read();
				let method_base_arguments = match &user_type.kind {
					UserTypeKind::Struct { shape } => shape.specializations[specialization_index].type_arguments.ids.as_slice(),
					UserTypeKind::Enum { shape } => shape.specializations[specialization_index].type_arguments.ids.as_slice(),
				};

				for &base_argument in method_base_arguments {
					type_arguments.push_method_base(base_argument);
				}
			}

			MethodBaseType::Trait { trait_ids, self_type_id } => {
				let traits = type_store.traits.read();
				for &trait_id in trait_ids.iter() {
					let shape = traits[trait_id.shape_index as usize].as_ref().unwrap().read();
					let has_function = shape
						.methods
						.iter()
						.any(|method| method.fake_function_shape_index == function_shape_index);
					if !has_function {
						continue;
					}

					let specialization = &shape.specializations[trait_id.specialization_index as usize];
					for &base_argument in &specialization.type_arguments.ids {
						type_arguments.push_method_base(base_argument);
					}

					break;
				}

				type_arguments.push_method_base(Node::new(self_type_id, Span::unusable())); // TODO: HACK
			}

			MethodBaseType::Other => {}
		}

		self.get_or_add_specialization(
			messages,
			type_store,
			module_path,
			generic_usages,
			enclosing_generic_parameters,
			function_shape_index,
			span,
			Ref::new(type_arguments),
		)
	}

	pub fn specialize_function_with_function_generics(
		&self,
		messages: &mut Messages<'a>,
		type_store: &mut TypeStore<'a>,
		function_id: FunctionId,
		caller_shape_index: usize,
		caller_type_arguments: &TypeArguments,
		base_type_id: Option<TypeId>,
	) -> FunctionId {
		let lock = self.shapes.read()[function_id.function_shape_index].as_ref().unwrap().clone();
		let shape = lock.read();
		if base_type_id.is_none() {
			assert!(shape.trait_method_marker.is_none(), "{:?}", shape.trait_method_marker);
		}
		let specialization = &shape.specializations[function_id.specialization_index];

		if let Some(base_type_id) = base_type_id {
			if let Some(trait_method_marker) = shape.trait_method_marker {
				drop(shape);

				let type_entry = type_store.type_entries.get(base_type_id);
				let methods_index = match type_entry.kind {
					TypeEntryKind::BuiltinType { methods_index, .. }
					| TypeEntryKind::UserType { methods_index, .. }
					| TypeEntryKind::UserTypeGeneric { methods_index, .. }
					| TypeEntryKind::FunctionGeneric { methods_index, .. }
					| TypeEntryKind::TraitGeneric { methods_index, .. } => methods_index,

					TypeEntryKind::Module | TypeEntryKind::Type | TypeEntryKind::Pointer { .. } | TypeEntryKind::Slice(_) => {
						// If we know that this function is a method (we passed a base type id) then it stands to reason that the
						// base type must have the ability to have methods, otherwise we've done something wrong somewhere
						unreachable!("{:#?}", type_entry.kind);
					}
				};

				let implementations = type_store.implementations.read();
				let infos = implementations[methods_index].read();

				// If we know that this method is a trait method, and that it exists on the base type id, then we must
				// have checked it for conformance at some point and so we know that this item exists
				let actual_method_indices = match infos.actual_indices.get(&trait_method_marker.trait_shape_index) {
					Some(actual_method_indices) => actual_method_indices,
					None => {
						for implementation_info in implementations.as_slice() {
							dbg!(&*implementation_info.read());
						}
						dbg!(&*infos, trait_method_marker.trait_shape_index);
						drop(infos);
						drop(implementations);
						let type_name = type_store.debugging_type_name(base_type_id);
						unreachable!("type: {type_name}")
					}
				};

				let actual_method_info = actual_method_indices.actual_method_indices[trait_method_marker.trait_method_index];
				drop(infos);
				drop(implementations);

				let method_collections = type_store.method_collections.read();
				let method_collection = method_collections[methods_index].read();
				let function_shape_index = method_collection.methods[actual_method_info.method_index].function_shape_index;
				drop(method_collection);
				drop(method_collections);

				let lock = self.shapes.read()[function_shape_index].as_ref().unwrap().clone();
				let shape = lock.read();
				assert!(shape.trait_method_marker.is_none());
				let specialization = &shape.specializations[actual_method_info.specialization_index];

				// TODO: Something is wrong here ⬇️
				let mut generic_usages = Vec::new();
				let mut type_arguments = TypeArguments::clone(&specialization.type_arguments);
				type_arguments.specialize_with_generics(
					messages,
					type_store,
					self,
					shape.module_path,
					&mut generic_usages,
					&GenericParameters::new_from_explicit(Vec::new()),
					TypeIdSpecializationSituation::Function { function_shape_index: caller_shape_index },
					caller_type_arguments,
				);
				assert!(generic_usages.is_empty());

				let &specialization_index = shape.specializations_by_type_arguments.get(&type_arguments).unwrap();
				drop(shape);

				return FunctionId { function_shape_index, specialization_index };
			}
		}

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

		// TODO: Something is wrong here ⬇️
		let mut generic_usages = Vec::new();
		let mut type_arguments = TypeArguments::clone(&specialization.type_arguments);
		type_arguments.specialize_with_generics(
			messages,
			type_store,
			self,
			shape.module_path,
			&mut generic_usages,
			&GenericParameters::new_from_explicit(Vec::new()),
			TypeIdSpecializationSituation::Function { function_shape_index: caller_shape_index },
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
