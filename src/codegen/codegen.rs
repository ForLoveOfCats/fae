use crate::error::Messages;
use crate::ir::{Block, Expression, ExpressionKind, FunctionId, FunctionShape, IntegerValue, StructLiteral, TypeArguments};
use crate::type_store::{TypeEntryKind, TypeStore};
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

fn generate_expression<G: Generator>(context: &mut Context, generator: &mut G, expression: &Expression) -> Option<G::Binding> {
	match &expression.kind {
		ExpressionKind::Block(_) => todo!("generate expression Block"),

		ExpressionKind::If(_) => todo!("generate expression If"),

		ExpressionKind::IntegerValue(value) => generate_integer_value(context, generator, value),

		ExpressionKind::DecimalValue(_) => todo!("generate expression DecimalValue"),

		ExpressionKind::BooleanLiteral(_) => todo!("generate expression BooleanLiteral"),

		ExpressionKind::CodepointLiteral(_) => todo!("generate expression CodepointLiteral"),

		ExpressionKind::StringLiteral(_) => todo!("generate expression StringLiteral"),

		ExpressionKind::ArrayLiteral(_) => todo!("generate expression ArrayLiteral"),

		ExpressionKind::StructLiteral(literal) => generate_struct_literal(context, generator, literal),

		ExpressionKind::Call(call) => generate_call(context, generator, call),

		ExpressionKind::Read(_) => todo!("generate expression Read"),

		ExpressionKind::FieldRead(_) => todo!("generate expression FieldRead"),

		ExpressionKind::UnaryOperation(_) => todo!("generate expression UnaryOperation"),

		ExpressionKind::BinaryOperation(_) => todo!("generate expression BinaryOperation"),

		ExpressionKind::SliceMutableToImmutable(_) => todo!("generate expression SliceMutableToImmutable"),

		ExpressionKind::Void => None,

		ExpressionKind::AnyCollapse => unreachable!(),
	}
}

fn generate_integer_value<G: Generator>(context: &mut Context, generator: &mut G, value: &IntegerValue) -> Option<G::Binding> {
	let kind = value.collapsed().numeric_kind(context.type_store).unwrap();
	Some(generator.generate_integer_value(kind, value.value()))
}

fn generate_struct_literal<G: Generator>(
	context: &mut Context,
	generator: &mut G,
	literal: &StructLiteral,
) -> Option<G::Binding> {
	// TODO: Avoid this creating this vec every time
	let mut fields = Vec::with_capacity(literal.field_initializers.len());
	for initalizer in &literal.field_initializers {
		if let Some(step) = generate_expression(context, generator, &initalizer.expression) {
			fields.push(step);
		}
	}

	let layout = context.type_store.type_layout(literal.type_id);
	if layout.size <= 0 {
		assert_eq!(fields.len(), 0);
		None
	} else {
		assert!(!fields.is_empty());

		let entry = context.type_store.type_entries[literal.type_id.index()];
		let (shape_index, specialization_index) = match entry.kind {
			TypeEntryKind::UserType { shape_index, specialization_index } => (shape_index, specialization_index),
			_ => unreachable!("{:?}", entry.kind),
		};

		Some(generator.generate_struct_literal(shape_index, specialization_index, &fields))
	}
}

fn generate_call<G: Generator>(
	context: &mut Context<'_, '_>,
	generator: &mut G,
	call: &crate::ir::Call<'_>,
) -> Option<G::Binding> {
	// TODO: Avoid this creating this vec every time
	let mut arguments = Vec::with_capacity(call.arguments.len());
	for argument in &call.arguments {
		if let Some(binding) = generate_expression(context, generator, argument) {
			arguments.push(binding);
		}
	}

	generator.generate_call(call.function_id, &arguments)
}
