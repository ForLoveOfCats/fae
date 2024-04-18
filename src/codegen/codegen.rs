use crate::codegen::generator::Generator;
use crate::frontend::error::Messages;
use crate::frontend::function_store::FunctionStore;
use crate::frontend::ir::{
	ArrayLiteral, BinaryOperation, Binding, Block, Break, Call, CodepointLiteral, DecimalValue, Expression, ExpressionKind,
	FieldRead, FunctionId, FunctionShape, IfElseChain, IntegerValue, Read, Return, SliceMutableToImmutable, StatementKind,
	StaticRead, StringLiteral, StructLiteral, TypeArguments, UnaryOperation, UnaryOperator, While,
};
use crate::frontend::lang_items::LangItems;
use crate::frontend::symbols::Statics;
use crate::frontend::tree::BinaryOperator;
use crate::frontend::type_store::{TypeEntryKind, TypeId, TypeStore, UserTypeKind};

pub fn generate<'a, G: Generator>(
	messages: &mut Messages<'a>,
	lang_items: &LangItems,
	type_store: &mut TypeStore<'a>,
	function_store: &mut FunctionStore<'a>,
	statics: &Statics,
	generator: &mut G,
) {
	generator.register_type_descriptions(type_store);
	generator.register_statics(type_store, statics);
	generator.register_functions(type_store, function_store);

	for function_shape_index in 0..function_store.shapes.len() {
		let shape = &function_store.shapes[function_shape_index];
		if shape.extern_attribute.is_some() || shape.intrinsic_attribute.is_some() {
			continue;
		}

		for specialization_index in 0..shape.specializations.len() {
			let specialization = &shape.specializations[specialization_index];
			if specialization.generic_poisioned {
				continue;
			}

			for type_argument in specialization.type_arguments.ids() {
				let entry = &type_store.type_entries[type_argument.index()];
				if entry.generic_poisoned {
					panic!("The legacy C backend had this, does it ever get hit?")
					// continue;
				}
			}

			let function_id = FunctionId { function_shape_index, specialization_index };
			generate_function(messages, lang_items, type_store, function_store, generator, shape, function_id);
		}
	}

	generator.finalize_generator();
}

pub struct Context<'a, 'b> {
	messages: &'b mut Messages<'a>,
	lang_items: &'b LangItems,
	type_store: &'b mut TypeStore<'a>,
	function_store: &'b FunctionStore<'a>,
	module_path: &'a [String],
	function_type_arguments: &'b TypeArguments,
	function_id: FunctionId,
}

impl<'a, 'b> Context<'a, 'b> {
	fn specialize_type_id(&mut self, type_id: TypeId) -> TypeId {
		let mut generic_usages = Vec::new();
		let type_id = self.type_store.specialize_with_function_generics(
			self.messages,
			self.function_store,
			self.module_path,
			&mut generic_usages,
			self.function_id.function_shape_index,
			self.function_type_arguments,
			type_id,
		);
		assert_eq!(generic_usages.len(), 0);
		type_id
	}
}

pub fn generate_function<'a, G: Generator>(
	messages: &mut Messages<'a>,
	lang_items: &LangItems,
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
		lang_items,
		type_store,
		function_store,
		module_path,
		function_type_arguments: &type_arguments,
		function_id,
	};

	generate_block(&mut context, generator, block.as_ref());
}

// TODO: Handle scope alloc lifetime
fn generate_block<G: Generator>(context: &mut Context, generator: &mut G, block: &Block) {
	generator.start_block();

	for statement in &block.statements {
		match &statement.kind {
			StatementKind::Expression(expression) => {
				generate_expression(context, generator, expression);
			}

			StatementKind::Block(block) => generate_block(context, generator, block),

			StatementKind::While(statement) => generate_while(context, generator, statement),

			StatementKind::Binding(binding) => generate_binding(context, generator, binding),

			StatementKind::Break(statement) => {
				generate_break(generator, statement);
				break;
			}

			StatementKind::Return(statement) => {
				generate_return(context, generator, statement);
				break;
			}
		};
	}

	generator.end_block();
}

fn generate_expression<G: Generator>(context: &mut Context, generator: &mut G, expression: &Expression) -> Option<G::Binding> {
	match &expression.kind {
		ExpressionKind::Block(block) => {
			generate_block(context, generator, block);
			None
		}

		ExpressionKind::IfElseChain(chain_expression) => generate_if_else_chain(context, generator, chain_expression),

		ExpressionKind::IntegerValue(value) => generate_integer_value(context, generator, value),

		ExpressionKind::DecimalValue(value) => generate_decimal_value(context, generator, value),

		&ExpressionKind::BooleanLiteral(literal) => generate_boolean_literal(context, generator, literal),

		ExpressionKind::CodepointLiteral(literal) => generate_codepoint_literal(context, generator, literal),

		ExpressionKind::StringLiteral(literal) => generate_string_literal(context, generator, literal),

		ExpressionKind::ArrayLiteral(literal) => generate_array_literal(context, generator, literal),

		ExpressionKind::StructLiteral(literal) => generate_struct_literal(context, generator, literal),

		ExpressionKind::Call(call) => generate_call(context, generator, call),

		ExpressionKind::Read(read) => generate_read(generator, read),

		ExpressionKind::StaticRead(static_read) => generate_static_read(generator, static_read),

		ExpressionKind::FieldRead(read) => generate_field_read(context, generator, read),

		ExpressionKind::UnaryOperation(operation) => generate_unary_operation(context, generator, operation),

		ExpressionKind::BinaryOperation(operation) => generate_binary_operation(context, generator, operation),

		ExpressionKind::SliceMutableToImmutable(conversion) => {
			generate_mutable_slice_to_immutable(context, generator, conversion)
		}

		ExpressionKind::Void => None,

		ExpressionKind::AnyCollapse => unreachable!(),
	}
}

fn generate_if_else_chain<G: Generator>(
	context: &mut Context,
	generator: &mut G,
	chain_expression: &IfElseChain,
) -> Option<G::Binding> {
	generator.generate_if_else_chain(
		context,
		chain_expression,
		|context, generator, condition| generate_expression(context, generator, condition).unwrap(),
		|context, generator, body| generate_block(context, generator, body),
	);

	None
}

fn generate_while<G: Generator>(context: &mut Context, generator: &mut G, statement: &While) {
	generator.generate_while(
		context,
		|context, generator| generate_expression(context, generator, &statement.condition).unwrap(),
		|context, generator| {
			generate_block(context, generator, &statement.body);
		},
	);
}

fn generate_integer_value<G: Generator>(context: &Context, generator: &mut G, value: &IntegerValue) -> Option<G::Binding> {
	Some(generator.generate_integer_value(context.type_store, value.collapsed(), value.value()))
}

fn generate_decimal_value<G: Generator>(context: &Context, generator: &mut G, value: &DecimalValue) -> Option<G::Binding> {
	Some(generator.generate_decimal_value(context.type_store, value.collapsed(), value.value()))
}

fn generate_boolean_literal<G: Generator>(context: &Context, generator: &mut G, literal: bool) -> Option<G::Binding> {
	Some(generator.generate_boolean_literal(context.type_store, literal))
}

fn generate_codepoint_literal<G: Generator>(
	context: &Context,
	generator: &mut G,
	literal: &CodepointLiteral,
) -> Option<G::Binding> {
	let type_id = context.type_store.u32_type_id();
	Some(generator.generate_integer_value(context.type_store, type_id, literal.value as i128))
}

fn generate_string_literal<G: Generator>(context: &Context, generator: &mut G, literal: &StringLiteral) -> Option<G::Binding> {
	Some(generator.generate_string_literal(context.type_store, &literal.value))
}

fn generate_array_literal<G: Generator>(context: &mut Context, generator: &mut G, literal: &ArrayLiteral) -> Option<G::Binding> {
	let mut elements = Vec::with_capacity(literal.expressions.len());
	for expression in &literal.expressions {
		if let Some(step) = generate_expression(context, generator, expression) {
			elements.push(step);
		}
	}

	let pointee_type_id = context.specialize_type_id(literal.pointee_type_id);
	let type_id = context.specialize_type_id(literal.type_id);

	let pointee_layout = context.type_store.type_layout(pointee_type_id);
	if pointee_layout.size <= 0 {
		return Some(generator.generate_non_null_invalid_slice(type_id, literal.expressions.len() as u64));
	}

	Some(generator.generate_array_literal(context.type_store, &elements, pointee_type_id, type_id))
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

	let type_id = context.specialize_type_id(literal.type_id);
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

fn generate_static_read<G: Generator>(generator: &mut G, static_read: &StaticRead) -> Option<G::Binding> {
	Some(generator.generate_static_read(static_read.static_index))
}

fn generate_field_read<G: Generator>(context: &mut Context, generator: &mut G, read: &FieldRead) -> Option<G::Binding> {
	let base = generate_expression(context, generator, &read.base)?;

	let type_id = context.specialize_type_id(read.base.type_id);
	let entry = &context.type_store.type_entries[type_id.index()];
	if let TypeEntryKind::UserType { shape_index, specialization_index } = entry.kind {
		match &context.type_store.user_types[shape_index].kind {
			UserTypeKind::Struct { shape } => {
				let specialization = &shape.specializations[specialization_index];
				let field_type_id = specialization.fields[read.field_index].type_id;
				let field_layout = context.type_store.type_layout(field_type_id);
				if field_layout.size <= 0 {
					return None;
				}
			}
		}
	}

	generator.generate_field_read(context.type_store, base, read.field_index)
}

fn generate_unary_operation<G: Generator>(
	context: &mut Context,
	generator: &mut G,
	operation: &UnaryOperation,
) -> Option<G::Binding> {
	let type_id = context.specialize_type_id(operation.type_id);
	let Some(expression) = generate_expression(context, generator, &operation.expression) else {
		if matches!(operation.op, UnaryOperator::AddressOf | UnaryOperator::AddressOfMut) {
			return Some(generator.generate_non_null_invalid_pointer(type_id));
		}
		return None;
	};

	match &operation.op {
		UnaryOperator::Negate => Some(generator.generate_negate(expression, type_id)),

		UnaryOperator::Invert => Some(generator.generate_invert(expression)),

		UnaryOperator::AddressOf | UnaryOperator::AddressOfMut => Some(generator.generate_address_of(expression, type_id)),

		UnaryOperator::Dereference => Some(generator.generate_dereference(context.type_store, expression, type_id)),

		&UnaryOperator::Cast { type_id: to } => {
			let to = context.specialize_type_id(to);
			Some(generator.generate_cast(context.type_store, expression, to))
		}

		UnaryOperator::Index { index_expression } => {
			let span = index_expression.span;
			let index_expression = generate_expression(context, generator, index_expression).unwrap();
			generator.generate_slice_index(context.lang_items, context.type_store, type_id, expression, index_expression, span)
		}
	}
}

fn generate_binary_operation<G: Generator>(
	context: &mut Context,
	generator: &mut G,
	operation: &BinaryOperation,
) -> Option<G::Binding> {
	let left = generate_expression(context, generator, &operation.left);
	let right = generate_expression(context, generator, &operation.right);

	let Some(left) = left else {
		assert!(right.is_none());

		let value = match operation.op {
			BinaryOperator::Assign => return None,

			BinaryOperator::Equals => true,
			BinaryOperator::NotEquals => false,

			op => unreachable!("{op:?}"),
		};

		return Some(generator.generate_boolean_literal(context.type_store, value));
	};

	let right = right?;
	let source_type_id = context.specialize_type_id(operation.left.type_id);
	let result_type_id = context.specialize_type_id(operation.type_id);
	generator.generate_binary_operation(context.type_store, left, right, operation.op, source_type_id, result_type_id)
}

fn generate_mutable_slice_to_immutable<G: Generator>(
	context: &mut Context,
	generator: &mut G,
	conversion: &SliceMutableToImmutable,
) -> Option<G::Binding> {
	Some(generate_expression(context, generator, &conversion.expression).unwrap())
}

fn generate_binding<G: Generator>(context: &mut Context, generator: &mut G, binding: &Binding) {
	let value = generate_expression(context, generator, &binding.expression);
	let type_id = context.specialize_type_id(binding.type_id);
	generator.generate_binding(binding.readable_index, value, type_id);
}

fn generate_break<G: Generator>(generator: &mut G, statement: &Break) {
	generator.generate_break(statement.loop_index);
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
			let type_id = context.specialize_type_id(specialization.type_arguments.explicit_ids()[0]);
			let size = context.type_store.type_layout(type_id).size as i128;
			let integer = IntegerValue::new_collapsed(size, span, context.type_store.i64_type_id());
			generate_integer_value(context, generator, &integer)
		}

		"alignment_of" => {
			assert_eq!(specialization.type_arguments.explicit_len(), 1);
			let type_id = context.specialize_type_id(specialization.type_arguments.explicit_ids()[0]);
			let alignment = context.type_store.type_layout(type_id).alignment as i128;
			let integer = IntegerValue::new_collapsed(alignment, span, context.type_store.i64_type_id());
			generate_integer_value(context, generator, &integer)
		}

		_ => unreachable!(),
	}
}
