use crate::codegen::generator::Generator;
use crate::frontend::error::Messages;
use crate::frontend::function_store::FunctionStore;
use crate::frontend::ir::{
	Binding, Block, Call, Expression, ExpressionKind, FieldRead, FunctionId, FunctionShape, IntegerValue, Read, Return,
	StatementKind, StringLiteral, StructLiteral, TypeArguments, UnaryOperation, UnaryOperator,
};
use crate::frontend::type_store::{TypeEntryKind, TypeStore};

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
		if shape.extern_attribute.is_some() || shape.intrinsic_attribute.is_some() {
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
			StatementKind::Expression(expression) => {
				generate_expression(context, generator, expression);
			}

			StatementKind::Block(_) => todo!("generate StatementKind::Block"),

			StatementKind::Binding(binding) => generate_binding(context, generator, binding),

			StatementKind::Return(statement) => generate_return(context, generator, statement),
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

		ExpressionKind::StringLiteral(literal) => generate_string_literal(context, generator, literal),

		ExpressionKind::ArrayLiteral(_) => todo!("generate expression ArrayLiteral"),

		ExpressionKind::StructLiteral(literal) => generate_struct_literal(context, generator, literal),

		ExpressionKind::Call(call) => generate_call(context, generator, call),

		ExpressionKind::Read(read) => generate_read(generator, read),

		ExpressionKind::FieldRead(read) => generate_field_read(context, generator, read),

		ExpressionKind::UnaryOperation(operation) => generate_unary_operation(context, generator, operation),

		ExpressionKind::BinaryOperation(_) => todo!("generate expression BinaryOperation"),

		ExpressionKind::SliceMutableToImmutable(_) => todo!("generate expression SliceMutableToImmutable"),

		ExpressionKind::Void => None,

		ExpressionKind::AnyCollapse => unreachable!(),
	}
}

fn generate_integer_value<G: Generator>(context: &Context, generator: &mut G, value: &IntegerValue) -> Option<G::Binding> {
	Some(generator.generate_integer_value(context.type_store, value.collapsed(), value.value()))
}

fn generate_string_literal<G: Generator>(context: &Context, generator: &mut G, value: &StringLiteral) -> Option<G::Binding> {
	Some(generator.generate_string_literal(context.type_store, &value.value))
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

	let mut generic_usages = Vec::new();
	let type_id = context.type_store.specialize_with_function_generics(
		context.messages,
		context.function_store,
		context.module_path,
		&mut generic_usages,
		context.function_id.function_shape_index,
		context.function_type_arguments,
		literal.type_id,
	);
	assert_eq!(generic_usages.len(), 0);

	let layout = context.type_store.type_layout(type_id);
	if layout.size <= 0 {
		assert_eq!(fields.len(), 0);
		None
	} else {
		assert!(!fields.is_empty());

		let entry = context.type_store.type_entries[type_id.index()];
		let (shape_index, specialization_index) = match entry.kind {
			TypeEntryKind::UserType { shape_index, specialization_index } => (shape_index, specialization_index),
			_ => unreachable!("{:?}", entry.kind),
		};

		Some(generator.generate_struct_literal(type_id, shape_index, specialization_index, &fields))
	}
}

fn generate_call<G: Generator>(context: &mut Context, generator: &mut G, call: &Call) -> Option<G::Binding> {
	let function_id = context.function_store.specialize_with_function_generics(
		context.messages,
		context.type_store,
		call.function_id,
		context.function_id.function_shape_index,
		context.function_type_arguments,
	);

	let is_intrinsic = {
		let shape = &context.function_store.shapes[function_id.function_shape_index];
		shape.intrinsic_attribute.is_some()
	};

	if is_intrinsic {
		return generate_intrinsic(context, generator, function_id, call);
	}

	// TODO: Avoid this creating this vec every time
	let mut arguments = Vec::with_capacity(call.arguments.len());
	for argument in &call.arguments {
		let binding = generate_expression(context, generator, argument);
		arguments.push(binding);
	}

	generator.generate_call(context.type_store, function_id, &arguments)
}

fn generate_read<G: Generator>(generator: &mut G, read: &Read) -> Option<G::Binding> {
	generator.generate_read(read.readable_index)
}

fn generate_field_read<G: Generator>(context: &mut Context, generator: &mut G, read: &FieldRead) -> Option<G::Binding> {
	let Some(base) = generate_expression(context, generator, &read.base) else {
		return None;
	};

	generator.generate_field_read(context.type_store, base, read.field_index)
}

fn generate_unary_operation<G: Generator>(
	context: &mut Context,
	generator: &mut G,
	operation: &UnaryOperation,
) -> Option<G::Binding> {
	let expression = generate_expression(context, generator, &operation.expression)?;

	match &operation.op {
		UnaryOperator::Negate => todo!("UnaryOperator::Negate"),

		UnaryOperator::Invert => todo!("UnaryOperator::Invert"),

		UnaryOperator::AddressOf => todo!("UnaryOperator::AddressOf"),

		UnaryOperator::AddressOfMut => todo!("UnaryOperator::AddressOfMut"),

		UnaryOperator::Dereference => todo!("UnaryOperator::Dereference"),

		UnaryOperator::Cast { .. } => todo!("UnaryOperator::Cast"),

		UnaryOperator::Index { index_expression } => {
			let span = index_expression.span;
			let index_expression = generate_expression(context, generator, index_expression).unwrap();
			generator.generate_slice_index(context.type_store, operation.type_id, expression, index_expression, span)
		}
	}
}

fn generate_binding<G: Generator>(context: &mut Context, generator: &mut G, binding: &Binding) {
	let value = generate_expression(context, generator, &binding.expression);
	generator.generate_binding(binding.readable_index, value);
}

fn generate_return<G: Generator>(context: &mut Context, generator: &mut G, statement: &Return) {
	let Some(expression) = &statement.expression else {
		generator.generate_return(context.function_id, None);
		return;
	};

	let value = generate_expression(context, generator, expression);
	generator.generate_return(context.function_id, value);
}

fn generate_intrinsic<G: Generator>(
	context: &mut Context,
	generator: &mut G,
	function_id: FunctionId,
	call: &Call,
) -> Option<G::Binding> {
	let span = call.span;

	let shape = &context.function_store.shapes[function_id.function_shape_index];
	let specialization = &shape.specializations[function_id.specialization_index];

	match call.name {
		"size_of" => {
			assert_eq!(specialization.type_arguments.explicit_len(), 1);
			let type_id = specialization.type_arguments.explicit_ids()[0];
			let size = context.type_store.type_layout(type_id).size as i128;
			let integer = IntegerValue::new_collapsed(size, span, context.type_store.i64_type_id());
			generate_integer_value(context, generator, &integer)
		}

		"alignment_of" => {
			assert_eq!(specialization.type_arguments.explicit_len(), 1);
			let type_id = specialization.type_arguments.explicit_ids()[0];
			let alignment = context.type_store.type_layout(type_id).alignment as i128;
			let integer = IntegerValue::new_collapsed(alignment, span, context.type_store.i64_type_id());
			generate_integer_value(context, generator, &integer)
		}

		_ => unreachable!(),
	}
}
