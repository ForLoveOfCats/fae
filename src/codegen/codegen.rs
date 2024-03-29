use crate::error::Messages;
use crate::ir::{Block, Expression, FunctionId, FunctionShape, TypeArguments};
use crate::type_store::TypeStore;
use crate::validator::FunctionStore;

use super::generator::Generator;

pub fn generate<'a, G: Generator>(
	messages: &mut Messages<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &mut FunctionStore<'a>,
	generator: &mut G,
) {
	generator.register_type_descriptions(type_store);
	generator.register_functions(type_store, function_store);

	for function_shape_index in 0..function_store.shapes.len() {
		let shape = &function_store.shapes[function_shape_index];
		if shape.extern_attribute.is_some() {
			continue;
		}

		for specialization_index in 0..shape.specializations.len() {
			let function_id = FunctionId { function_shape_index, specialization_index };
			generate_function(messages, type_store, function_store, generator, shape, function_id);
		}
	}

	generator.finalize_generator();
}

struct Context<'a, 'b> {
	messages: &'b mut Messages<'a>,
	type_store: &'b mut TypeStore<'a>,
	function_store: &'b FunctionStore<'a>,
	module_path: &'a [String],
	function_type_arguments: &'b TypeArguments,
	function_id: FunctionId,
}

impl<'a, 'b> Context<'a, 'b> {}

pub fn generate_function<'a, G: Generator>(
	messages: &mut Messages<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	generator: &mut G,
	shape: &FunctionShape<'a>,
	function_id: FunctionId,
) {
	let specialization = &shape.specializations[function_id.specialization_index];

	for type_argument in specialization.type_arguments.ids() {
		let entry = &type_store.type_entries[type_argument.index()];
		if entry.generic_poisoned {
			return;
		}
	}

	generator.start_function(type_store, specialization, function_id);

	let module_path = shape.module_path;
	let type_arguments = specialization.type_arguments.clone();
	let Some(block) = shape.block.clone() else {
		unreachable!("{shape:?}");
	};

	let mut context = Context {
		messages,
		type_store,
		function_store,
		module_path,
		function_type_arguments: &type_arguments,
		function_id,
	};

	generate_block(&mut context, generator, block.as_ref());
}

fn generate_block<G: Generator>(context: &mut Context, generator: &mut G, block: &Block) {
	for statement in &block.statements {
		match &statement.kind {
			crate::ir::StatementKind::Expression(expression) => generate_expression(context, generator, expression),

			// crate::ir::StatementKind::Block(_) => todo!("generate StatementKind::Block"),
			// crate::ir::StatementKind::Binding(_) => todo!("generate StatementKind::Binding"),
			// crate::ir::StatementKind::Return(_) => todo!("generate StatementKind::Return"),
			_ => None,
		};
	}
}

fn generate_expression<G: Generator>(_context: &mut Context, _generator: &mut G, _expression: &Expression) -> Option<G::Binding> {
	None
}
