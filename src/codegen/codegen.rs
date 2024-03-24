use crate::error::Messages;
use crate::ir::{Block, Expression, FunctionId, TypeArguments};
use crate::type_store::TypeStore;
use crate::validator::FunctionStore;

use super::generator::Generator;

pub fn generate<'a, G: Generator>(
	messages: &mut Messages<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &mut FunctionStore<'a>,
	generator: &mut G,
) {
	for function_shape_index in 0..function_store.shapes.len() {
		let shape = &function_store.shapes[function_shape_index];
		for specialization_index in 0..shape.specializations.len() {
			let function_id = FunctionId { function_shape_index, specialization_index };
			generate_function(messages, type_store, function_store, generator, function_id);
		}
	}

	generator.finalize_generator();
}

struct Context<'a, 'b> {
	messages: &'b mut Messages<'a>,
	type_store: &'b mut TypeStore<'a>,
	function_store: &'b mut FunctionStore<'a>,
	module_path: &'a [String],
	function_type_arguments: &'b TypeArguments,
	function_id: FunctionId,
}

impl<'a, 'b> Context<'a, 'b> {}

pub fn generate_function<'a, G: Generator>(
	messages: &mut Messages<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &mut FunctionStore<'a>,
	generator: &mut G,
	function_id: FunctionId,
) {
	let shape = &mut function_store.shapes[function_id.function_shape_index];
	assert!(shape.extern_attribute.is_none(), "{:?}", shape.extern_attribute);
	let specialization = &mut shape.specializations[function_id.specialization_index];

	assert!(!specialization.been_generated);
	specialization.been_generated = true;

	let shape = &function_store.shapes[function_id.function_shape_index];
	let specialization = &shape.specializations[function_id.specialization_index];

	for type_argument in specialization.type_arguments.ids() {
		let entry = &type_store.type_entries[type_argument.index()];
		if entry.generic_poisoned {
			return;
		}
	}

	generator.start_function(type_store, specialization, shape.name.item);

	let block = shape.block.clone();
	let module_path = shape.module_path;
	let type_arguments = specialization.type_arguments.clone();

	let mut context = Context {
		messages,
		type_store,
		function_store,
		module_path,
		function_type_arguments: &type_arguments,
		function_id,
	};

	// assembler.note("Function prelude");
	// assembler.push_register64(Register64::Rbp);
	// assembler.move_register64_to_register64(Register64::Rsp, Register64::Rbp);
	// assembler.sub_literal32_to_register64(Literal32::from(32), Register64::Rsp); // TODO: Calculate stack usage

	generate_block(&mut context, generator, block.as_ref().unwrap());

	// assembler.note("Function shutdown");
	// assembler.leave();
	// assembler.ret_near();
}

fn generate_block<G: Generator>(context: &mut Context, generator: &mut G, block: &Block) {
	for statement in &block.statements {
		match &statement.kind {
			crate::ir::StatementKind::Expression(expression) => generate_expression(context, generator, expression),

			crate::ir::StatementKind::Block(_) => todo!("generate StatementKind::Block"),
			crate::ir::StatementKind::Binding(_) => todo!("generate StatementKind::Binding"),
			crate::ir::StatementKind::Return(_) => todo!("generate StatementKind::Return"),
		};
	}
}

fn generate_expression<G: Generator>(context: &mut Context, generator: &mut G, expression: &Expression) -> Option<G::Binding> {
	None
}
