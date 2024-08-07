use llvm_sys::prelude::LLVMBasicBlockRef;

use crate::codegen::generator::Generator;
use crate::frontend::error::Messages;
use crate::frontend::function_store::FunctionStore;
use crate::frontend::ir::{
	ArrayLiteral, BinaryOperation, Binding, Block, Break, ByteCodepointLiteral, Call, CheckIs, CodepointLiteral, Continue,
	DecimalValue, EnumVariantToEnum, Expression, ExpressionKind, FieldRead, For, ForKind, FormatStringItem, FormatStringLiteral,
	FunctionId, FunctionShape, IfElseChain, IntegerValue, Match, MethodCall, Read, SliceMutableToImmutable, Statement,
	StatementKind, StaticRead, StringLiteral, StringToFormatString, StructLiteral, TypeArguments, UnaryOperation, UnaryOperator,
	While,
};
use crate::frontend::lang_items::LangItems;
use crate::frontend::span::DebugLocation;
use crate::frontend::symbols::Statics;
use crate::frontend::tree::{self, BinaryOperator};
use crate::frontend::type_store::{TypeEntryKind, TypeId, TypeStore, UserTypeKind};
use crate::lock::ReadGuard;

pub fn generate<'a, G: Generator>(
	parsed_files: &[tree::File],
	messages: &mut Messages<'a>,
	lang_items: &LangItems,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	statics: &Statics,
	generator: &mut G,
	optimizing: bool,
) {
	generator.register_type_descriptions(type_store);
	generator.register_statics(type_store, statics);
	generator.register_functions(parsed_files, type_store, function_store, optimizing);

	let function_count = function_store.shapes.read().len();
	for function_shape_index in 0..function_count {
		let lock = function_store.shapes.read()[function_shape_index].as_ref().unwrap().clone();
		let shape = lock.read();
		if shape.extern_attribute.is_some() || shape.intrinsic_attribute.is_some() {
			continue;
		}

		let specialization_count = shape.specializations.len();
		drop(shape);

		for specialization_index in 0..specialization_count {
			let shape = lock.read();
			let specialization = &shape.specializations[specialization_index];
			if specialization.generic_poisoned {
				continue;
			}

			for &type_argument in &specialization.type_arguments.ids {
				let entry = type_store.type_entries.get(type_argument);
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

// `break` and `continue` in a loop need to inject defer expressions but only
// those defined *within* the loop they are controlling, whereas a `return`
// must inject *all* defer expressions in the stack. This struct marks the start
// of each loop in the stack, so every defer at `start_index..` have been
// defined in the loop the marker corresponds to
struct LoopMarker {
	start_index: usize,
}

pub struct Context<'a, 'b> {
	pub messages: &'b mut Messages<'a>,
	pub lang_items: &'b LangItems,
	pub type_store: &'b mut TypeStore<'a>,
	pub function_store: &'b FunctionStore<'a>,

	pub module_path: &'a [String],

	pub function_type_arguments: &'b TypeArguments,
	pub function_id: FunctionId,

	defer_stack: Vec<&'b Expression<'a>>,
	loop_stack: Vec<LoopMarker>,

	// TODO: Merge codegen and llvm/generator ASAP, way too many details have leaked
	pub if_condition_binding_block: Option<LLVMBasicBlockRef>,
}

impl<'a, 'b> Context<'a, 'b> {
	pub fn specialize_type_id(&mut self, type_id: TypeId) -> TypeId {
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
	shape: ReadGuard<FunctionShape<'a>>,
	function_id: FunctionId,
) {
	let specialization = &shape.specializations[function_id.specialization_index];

	for &type_argument in &specialization.type_arguments.ids {
		let entry = type_store.type_entries.get(type_argument);
		if entry.generic_poisoned {
			return;
		}
	}

	generator.start_function(type_store, specialization, function_id);

	let module_path = shape.module_path;
	let type_arguments = specialization.type_arguments.clone();
	let Some(block) = shape.block.clone() else {
		unreachable!("{:?}", *shape);
	};

	drop(shape);

	let mut context = Context {
		messages,
		lang_items,
		type_store,
		function_store,
		module_path,
		function_type_arguments: &type_arguments,
		function_id,
		defer_stack: Vec::new(),
		loop_stack: Vec::new(),
		if_condition_binding_block: None,
	};

	generate_block(&mut context, generator, block.as_ref());
}

fn generate_block<'a, 'b, G: Generator>(context: &mut Context<'a, 'b>, generator: &mut G, block: &'b Block<'a>) {
	generator.start_block();
	let block_start_defer_len = context.defer_stack.len();
	let mut should_generate_defer_stack = true;

	for statement in &block.statements {
		if generate_statement(context, generator, statement, block_start_defer_len, &mut should_generate_defer_stack) {
			break;
		}
	}

	if should_generate_defer_stack {
		let mut defer_stack = std::mem::take(&mut context.defer_stack);
		for expression in defer_stack[block_start_defer_len..].iter().rev() {
			generate_expression(context, generator, expression);
		}

		defer_stack.truncate(block_start_defer_len);
		context.defer_stack = defer_stack;
	}

	generator.end_block();
}

// True means break statement loop
fn generate_statement<'a, 'b, G: Generator>(
	context: &mut Context<'a, 'b>,
	generator: &mut G,
	statement: &'b Statement<'a>,
	block_start_defer_len: usize,
	should_generate_defer_stack: &mut bool,
) -> bool {
	let debug_location = statement.debug_location;

	match &statement.kind {
		StatementKind::Expression(expression) => {
			generate_expression(context, generator, expression);
		}

		StatementKind::When(block) => {
			for statement in &block.statements {
				if generate_statement(context, generator, statement, block_start_defer_len, should_generate_defer_stack) {
					return true;
				}
			}
		}

		StatementKind::Block(block) => generate_block(context, generator, block),

		StatementKind::While(statement) => {
			let start_index = context.defer_stack.len();
			context.loop_stack.push(LoopMarker { start_index });
			generate_while(context, generator, statement, debug_location);
			context.loop_stack.pop();
		}

		StatementKind::For(statement) => {
			let start_index = context.defer_stack.len();
			context.loop_stack.push(LoopMarker { start_index });
			generate_for(context, generator, statement, debug_location);
			context.loop_stack.pop();
		}

		StatementKind::Binding(binding) => generate_binding(context, generator, binding, debug_location),

		StatementKind::Defer(statement) => {
			context.defer_stack.push(&statement.expression);
		}

		StatementKind::Break(statement) => {
			let marker = context.loop_stack.last().unwrap();
			let mut defer_stack = std::mem::take(&mut context.defer_stack);
			for expression in defer_stack[marker.start_index..].iter().rev() {
				generate_expression(context, generator, expression);
			}
			defer_stack.truncate(block_start_defer_len);
			context.defer_stack = defer_stack;
			*should_generate_defer_stack = false;

			generate_break(generator, statement, debug_location);
			return true;
		}

		StatementKind::Continue(statement) => {
			let marker = context.loop_stack.last().unwrap();
			let mut defer_stack = std::mem::take(&mut context.defer_stack);
			for expression in defer_stack[marker.start_index..].iter().rev() {
				generate_expression(context, generator, expression);
			}
			defer_stack.truncate(block_start_defer_len);
			context.defer_stack = defer_stack;
			*should_generate_defer_stack = false;

			generate_continue(generator, statement, debug_location);
			return true;
		}

		StatementKind::Return(statement) => {
			let value = statement
				.expression
				.as_ref()
				.and_then(|expression| generate_expression(context, generator, expression));

			generator.generate_return(context, context.function_id, value, debug_location, |context, generator| {
				let mut defer_stack = std::mem::take(&mut context.defer_stack);
				for expression in defer_stack.iter().rev() {
					generate_expression(context, generator, expression);
				}
				defer_stack.truncate(block_start_defer_len);
				context.defer_stack = defer_stack;
			});

			*should_generate_defer_stack = false;
			return true;
		}
	}

	false
}

pub fn generate_expression<'a, 'b, G: Generator>(
	context: &mut Context<'a, 'b>,
	generator: &mut G,
	expression: &'b Expression<'a>,
) -> Option<G::Binding> {
	let debug_location = expression.debug_location;

	match &expression.kind {
		ExpressionKind::Block(block) => {
			generate_block(context, generator, block);
			None
		}

		ExpressionKind::IfElseChain(chain_expression) => generate_if_else_chain(context, generator, chain_expression),

		ExpressionKind::Match(match_expression) => generate_match(context, generator, match_expression),

		ExpressionKind::IntegerValue(value) => generate_integer_value(context, generator, value),

		ExpressionKind::DecimalValue(value) => generate_decimal_value(context, generator, value),

		&ExpressionKind::BooleanLiteral(literal) => generate_boolean_literal(context, generator, literal),

		ExpressionKind::CodepointLiteral(literal) => generate_codepoint_literal(context, generator, literal),

		ExpressionKind::ByteCodepointLiteral(literal) => generate_byte_codepoint_literal(context, generator, literal),

		ExpressionKind::StringLiteral(literal) => generate_string_literal(context, generator, literal),

		ExpressionKind::FormatStringLiteral(literal) => {
			generate_format_string_literal(context, generator, literal, debug_location)
		}

		ExpressionKind::ArrayLiteral(literal) => generate_array_literal(context, generator, literal, debug_location),

		ExpressionKind::StructLiteral(literal) => generate_struct_literal(context, generator, literal, debug_location),

		ExpressionKind::Call(call) => generate_call(context, generator, call, debug_location),

		ExpressionKind::MethodCall(method_call) => generate_method_call(context, generator, method_call, debug_location),

		ExpressionKind::Read(read) => generate_read(generator, read),

		ExpressionKind::StaticRead(static_read) => generate_static_read(generator, static_read),

		ExpressionKind::FieldRead(read) => generate_field_read(context, generator, read, debug_location),

		ExpressionKind::UnaryOperation(operation) => generate_unary_operation(context, generator, operation, debug_location),

		ExpressionKind::BinaryOperation(operation) => generate_binary_operation(context, generator, operation, debug_location),

		ExpressionKind::CheckIs(check_is) => generate_check_is(context, generator, check_is, debug_location),

		ExpressionKind::SliceMutableToImmutable(conversion) => {
			generate_mutable_slice_to_immutable(context, generator, conversion)
		}

		ExpressionKind::StringToFormatString(conversion) => {
			generate_string_to_format_string(context, generator, conversion, debug_location)
		}

		ExpressionKind::EnumVariantToEnum(conversion) => generate_enum_variant_to_enum(context, generator, conversion),

		ExpressionKind::Void => None,

		ExpressionKind::AnyCollapse | ExpressionKind::Type(_) => unreachable!("{:?}", &expression.kind),
	}
}

fn generate_if_else_chain<'a, 'b, G: Generator>(
	context: &mut Context<'a, 'b>,
	generator: &mut G,
	chain_expression: &'b IfElseChain<'a>,
) -> Option<G::Binding> {
	generator.generate_if_else_chain(
		context,
		chain_expression,
		|context, generator, condition| {
			generator.start_block();
			generate_expression(context, generator, condition).unwrap()
		},
		|context, generator, body, is_else| {
			let block = generate_block(context, generator, body);
			if !is_else {
				generator.end_block();
			}
			block
		},
	);

	None
}

fn generate_match<'a, 'b, G: Generator>(
	context: &mut Context<'a, 'b>,
	generator: &mut G,
	match_expression: &'b Match<'a>,
) -> Option<G::Binding> {
	let value = generate_expression(context, generator, &match_expression.expression).unwrap();
	let expression_type_id = context.specialize_type_id(match_expression.expression.type_id);
	let value_type_id = match expression_type_id.as_pointed(context.type_store) {
		Some(as_pointer) => as_pointer.type_id,
		None => expression_type_id,
	};

	let entry = context.type_store.type_entries.get(value_type_id);
	let TypeEntryKind::UserType {
		shape_index: enum_shape_index,
		specialization_index: enum_specialization_index,
	} = entry.kind
	else {
		unreachable!("{:?}", entry.kind);
	};

	generator.generate_match(
		context,
		value,
		enum_shape_index,
		enum_specialization_index,
		match_expression,
		|context, generator, block| {
			generate_block(context, generator, block);
		},
	);

	None
}

fn generate_while<'a, 'b, G: Generator>(
	context: &mut Context<'a, 'b>,
	generator: &mut G,
	statement: &'b While<'a>,
	debug_location: DebugLocation,
) {
	generator.generate_while(
		context,
		debug_location,
		|context, generator| generate_expression(context, generator, &statement.condition).unwrap(),
		|context, generator| {
			generate_block(context, generator, &statement.body);
		},
	);
}

fn generate_for<'a, 'b, G: Generator>(
	context: &mut Context<'a, 'b>,
	generator: &mut G,
	statement: &'b For<'a>,
	debug_location: DebugLocation,
) {
	match statement.kind {
		ForKind::InSlice | ForKind::OfSlice => {
			let initializer = generate_expression(context, generator, &statement.initializer).unwrap();
			generator.generate_for_slice(context, statement, initializer, debug_location, |context, generator| {
				generate_block(context, generator, &statement.body);
			});
		}

		ForKind::Range => {
			let initializer = generate_expression(context, generator, &statement.initializer).unwrap();
			generator.generate_for_range(context, statement, initializer, debug_location, |context, generator| {
				generate_block(context, generator, &statement.body);
			});
		}

		ForKind::AnyCollapse => unreachable!(),
	}
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

fn generate_byte_codepoint_literal<G: Generator>(
	context: &Context,
	generator: &mut G,
	literal: &ByteCodepointLiteral,
) -> Option<G::Binding> {
	let type_id = context.type_store.u8_type_id();
	Some(generator.generate_integer_value(context.type_store, type_id, literal.value as i128))
}

fn generate_string_literal<G: Generator>(context: &Context, generator: &mut G, literal: &StringLiteral) -> Option<G::Binding> {
	Some(generator.generate_string_literal(context.type_store, &literal.value))
}

fn generate_format_string_literal<'a, 'b, G: Generator>(
	context: &mut Context<'a, 'b>,
	generator: &mut G,
	literal: &'b FormatStringLiteral<'a>,
	debug_location: DebugLocation,
) -> Option<G::Binding> {
	let pointee_type_id = context.lang_items.format_string_item_type.unwrap();
	let type_id = context.type_store.format_string_type_id();

	let enum_entry = context.type_store.type_entries.get(pointee_type_id);
	let (enum_shape_index, enum_specialization_index) = match enum_entry.kind {
		TypeEntryKind::UserType { shape_index, specialization_index } => (shape_index, specialization_index),
		_ => unreachable!("{:?}", enum_entry.kind),
	};

	let mut elements = Vec::with_capacity(literal.items.len());
	for item in &literal.items {
		match item {
			FormatStringItem::Text(text) => {
				let variant_type_id = context.type_store.string_type_id();
				let variant_index = variant_type_id.format_item_variant_index(context.type_store);

				let variant_binding = generator.generate_string_literal(context.type_store, text);

				let wrapped = generator.generate_enum_variant_to_enum(
					context.type_store,
					pointee_type_id,
					enum_shape_index,
					enum_specialization_index,
					variant_index,
					Some(variant_binding),
				);

				elements.push(wrapped);
			}

			FormatStringItem::Expression(expression) => {
				let variant_type_id = context.specialize_type_id(expression.type_id);
				let variant_index = variant_type_id.format_item_variant_index(context.type_store);

				let variant_binding = generate_expression(context, generator, expression);
				let wrapped = generator.generate_enum_variant_to_enum(
					context.type_store,
					pointee_type_id,
					enum_shape_index,
					enum_specialization_index,
					variant_index,
					variant_binding,
				);

				elements.push(wrapped);
			}
		}
	}

	Some(generator.generate_array_literal(context.type_store, &elements, pointee_type_id, type_id, debug_location))
}

fn generate_array_literal<'a, 'b, G: Generator>(
	context: &mut Context<'a, 'b>,
	generator: &mut G,
	literal: &'b ArrayLiteral<'a>,
	debug_location: DebugLocation,
) -> Option<G::Binding> {
	let mut elements = Vec::with_capacity(literal.expressions.len());
	for expression in &literal.expressions {
		if let Some(step) = generate_expression(context, generator, expression) {
			elements.push(step);
		}
	}

	let pointee_type_id = context.specialize_type_id(literal.pointee_type_id);
	let type_id = context.specialize_type_id(literal.type_id);

	let pointee_layout = context.type_store.type_layout(pointee_type_id);
	if pointee_layout.size <= 0 || elements.is_empty() {
		return Some(generator.generate_non_null_invalid_slice(type_id, literal.expressions.len() as u64, debug_location));
	}

	Some(generator.generate_array_literal(context.type_store, &elements, pointee_type_id, type_id, debug_location))
}

fn generate_struct_literal<'a, 'b, G: Generator>(
	context: &mut Context<'a, 'b>,
	generator: &mut G,
	literal: &'b StructLiteral<'a>,
	debug_location: DebugLocation,
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

		let entry = context.type_store.type_entries.get(type_id);
		let (shape_index, specialization_index) = match entry.kind {
			TypeEntryKind::UserType { shape_index, specialization_index } => (shape_index, specialization_index),
			_ => unreachable!("{:?}", entry.kind),
		};

		Some(generator.generate_struct_literal(type_id, shape_index, specialization_index, &fields, debug_location))
	}
}

fn generate_call<'a, 'b, G: Generator>(
	context: &mut Context<'a, 'b>,
	generator: &mut G,
	call: &'b Call<'a>,
	debug_location: DebugLocation,
) -> Option<G::Binding> {
	let function_id = context.function_store.specialize_function_with_function_generics(
		context.messages,
		context.type_store,
		call.function_id,
		context.function_id.function_shape_index,
		context.function_type_arguments,
	);

	let is_intrinsic = context.function_store.shapes.read()[function_id.function_shape_index]
		.as_ref()
		.unwrap()
		.read()
		.intrinsic_attribute
		.is_some();

	if is_intrinsic {
		return generate_intrinsic(context, generator, function_id, call, debug_location);
	}

	// TODO: Avoid this creating this vec every time
	let mut arguments = Vec::with_capacity(call.arguments.len());
	for argument in &call.arguments {
		let binding = generate_expression(context, generator, argument);
		arguments.push(binding);
	}

	generator.generate_call(context.type_store, function_id, &arguments, debug_location)
}

fn generate_method_call<'a, 'b, G: Generator>(
	context: &mut Context<'a, 'b>,
	generator: &mut G,
	method_call: &'b MethodCall<'a>,
	debug_location: DebugLocation,
) -> Option<G::Binding> {
	let base_pointer_type_id = if method_call.base.type_id.is_pointer(context.type_store) {
		method_call.base.type_id
	} else {
		context
			.type_store
			.pointer_to(method_call.base.type_id, method_call.base.is_mutable)
	};

	let base = generate_expression(context, generator, &method_call.base)
		.unwrap_or(generator.generate_non_null_invalid_pointer(base_pointer_type_id, debug_location));

	let function_id = context.function_store.specialize_function_with_function_generics(
		context.messages,
		context.type_store,
		method_call.function_id,
		context.function_id.function_shape_index,
		context.function_type_arguments,
	);

	// TODO: Avoid this creating this vec every time
	let mut arguments = Vec::with_capacity(method_call.arguments.len() + 1);
	arguments.push(Some(base));
	for argument in &method_call.arguments {
		let binding = generate_expression(context, generator, argument);
		arguments.push(binding);
	}

	generator.generate_method_call(context.type_store, function_id, base_pointer_type_id, &mut arguments, debug_location)
}

fn generate_read<G: Generator>(generator: &mut G, read: &Read) -> Option<G::Binding> {
	generator.generate_read(read.readable_index)
}

fn generate_static_read<G: Generator>(generator: &mut G, static_read: &StaticRead) -> Option<G::Binding> {
	Some(generator.generate_static_read(static_read.static_index))
}

fn generate_field_read<'a, 'b, G: Generator>(
	context: &mut Context<'a, 'b>,
	generator: &mut G,
	read: &'b FieldRead<'a>,
	debug_location: DebugLocation,
) -> Option<G::Binding> {
	let base = generate_expression(context, generator, &read.base)?;

	let type_id = context.specialize_type_id(read.base.type_id);
	let entry = &context.type_store.type_entries.get(type_id);
	if let TypeEntryKind::UserType { shape_index, specialization_index } = entry.kind {
		let user_type = context.type_store.user_types.read()[shape_index].clone();
		let user_type = user_type.read();
		match &user_type.kind {
			UserTypeKind::Struct { shape } => {
				let specialization = &shape.specializations[specialization_index];
				let field_type_id = specialization.fields[read.field_index].type_id;
				let field_layout = context.type_store.type_layout(field_type_id);
				if field_layout.size <= 0 {
					return None;
				}
			}

			UserTypeKind::Enum { shape } => {
				let specialization = &shape.specializations[specialization_index];
				let field_type_id = specialization.shared_fields[read.field_index].type_id;
				let field_layout = context.type_store.type_layout(field_type_id);
				if field_layout.size <= 0 {
					return None;
				}
			}
		}
	}

	generator.generate_field_read(context.lang_items, context.type_store, base, read.field_index, debug_location)
}

fn generate_unary_operation<'a, 'b, G: Generator>(
	context: &mut Context<'a, 'b>,
	generator: &mut G,
	operation: &'b UnaryOperation<'a>,
	debug_location: DebugLocation,
) -> Option<G::Binding> {
	let type_id = context.specialize_type_id(operation.type_id);
	let Some(expression) = generate_expression(context, generator, &operation.expression) else {
		if matches!(operation.op, UnaryOperator::AddressOf | UnaryOperator::AddressOfMut) {
			return Some(generator.generate_non_null_invalid_pointer(type_id, debug_location));
		}
		return None;
	};

	let layout = context.type_store.type_layout(type_id);
	if layout.size <= 0 {
		// HACK: We still need to generate the bounds check. Right now the backend is assumed to
		// understand index of slice of zero sized type.
		// TODO: Add a separate `generator.generate_bounds_check` to untangle this mess
		if let UnaryOperator::Index { index_expression } = &operation.op {
			let index_expression = generate_expression(context, generator, index_expression).unwrap();
			return generator.generate_slice_index(
				context.lang_items,
				context.type_store,
				type_id,
				expression,
				index_expression,
				debug_location,
			);
		}

		return None;
	}

	match &operation.op {
		UnaryOperator::Negate => Some(generator.generate_negate(expression, type_id, debug_location)),

		UnaryOperator::Invert => Some(generator.generate_invert(expression, debug_location)),

		UnaryOperator::AddressOf | UnaryOperator::AddressOfMut => {
			Some(generator.generate_address_of(expression, type_id, debug_location))
		}

		UnaryOperator::Dereference => {
			Some(generator.generate_dereference(context.type_store, expression, type_id, debug_location))
		}

		&UnaryOperator::Cast { type_id: to } => {
			let to = context.specialize_type_id(to);
			Some(generator.generate_cast(context.type_store, expression, to, debug_location))
		}

		UnaryOperator::Index { index_expression } => {
			let index_expression = generate_expression(context, generator, index_expression).unwrap();
			generator.generate_slice_index(
				context.lang_items,
				context.type_store,
				type_id,
				expression,
				index_expression,
				debug_location,
			)
		}

		UnaryOperator::RangeIndex { index_expression } => {
			let index_expression = generate_expression(context, generator, index_expression).unwrap();
			let item_type = if type_id.is_string(context.type_store) {
				context.type_store.u8_type_id()
			} else {
				context.type_store.sliced_of(type_id).unwrap().0
			};
			generator.generate_slice_slice(
				context.lang_items,
				context.type_store,
				item_type,
				expression,
				index_expression,
				debug_location,
			)
		}
	}
}

fn generate_binary_operation<'a, 'b, G: Generator>(
	context: &mut Context<'a, 'b>,
	generator: &mut G,
	operation: &'b BinaryOperation<'a>,
	debug_location: DebugLocation,
) -> Option<G::Binding> {
	let left_type_id = context.specialize_type_id(operation.left.type_id);
	let result_type_id = context.specialize_type_id(operation.type_id);

	let left_untyped_integer = left_type_id.is_untyped_integer(context.type_store);
	let left_untyped_decimal = left_type_id.is_untyped_decimal(context.type_store);
	let left_has_size = left_untyped_integer || left_untyped_decimal || context.type_store.type_layout(left_type_id).size > 0;

	if !left_has_size {
		let left = generate_expression(context, generator, &operation.left);
		assert!(left.is_none());
		let right = generate_expression(context, generator, &operation.right);
		assert!(right.is_none());

		let value = match operation.op {
			BinaryOperator::Assign => return None,

			BinaryOperator::Equals => true,
			BinaryOperator::NotEquals => false,

			op => unreachable!("{op:?}"),
		};

		return Some(generator.generate_boolean_literal(context.type_store, value));
	}

	generator.generate_binary_operation(
		context,
		&operation.left,
		&operation.right,
		operation.op,
		left_type_id,
		result_type_id,
		debug_location,
	)
}

fn generate_check_is<'a, 'b, G: Generator>(
	context: &mut Context<'a, 'b>,
	generator: &mut G,
	check_is: &'b CheckIs<'a>,
	debug_location: DebugLocation,
) -> Option<G::Binding> {
	let value = generate_expression(context, generator, &check_is.left).unwrap();
	let left_type_id = context.specialize_type_id(check_is.left.type_id);
	let value_type_id = match left_type_id.as_pointed(context.type_store) {
		Some(as_pointer) => as_pointer.type_id,
		None => left_type_id,
	};

	let entry = context.type_store.type_entries.get(value_type_id);
	let TypeEntryKind::UserType {
		shape_index: enum_shape_index,
		specialization_index: enum_specialization_index,
	} = entry.kind
	else {
		unreachable!("{:?}", entry.kind);
	};

	Some(generator.generate_check_is(context, value, enum_shape_index, enum_specialization_index, &check_is, debug_location))
}

fn generate_mutable_slice_to_immutable<'a, 'b, G: Generator>(
	context: &mut Context<'a, 'b>,
	generator: &mut G,
	conversion: &'b SliceMutableToImmutable<'a>,
) -> Option<G::Binding> {
	Some(generate_expression(context, generator, &conversion.expression).unwrap())
}

fn generate_string_to_format_string<'a, 'b, G: Generator>(
	context: &mut Context<'a, 'b>,
	generator: &mut G,
	conversion: &'b StringToFormatString<'a>,
	debug_location: DebugLocation,
) -> Option<G::Binding> {
	let string_binding = generate_expression(context, generator, &conversion.expression).unwrap();

	let pointee_type_id = context.lang_items.format_string_item_type.unwrap();
	let type_id = context.type_store.format_string_type_id();

	let enum_entry = context.type_store.type_entries.get(pointee_type_id);
	let (enum_shape_index, enum_specialization_index) = match enum_entry.kind {
		TypeEntryKind::UserType { shape_index, specialization_index } => (shape_index, specialization_index),
		_ => unreachable!("{:?}", enum_entry.kind),
	};

	let variant_type_id = context.type_store.string_type_id();
	let variant_index = variant_type_id.format_item_variant_index(context.type_store);

	let wrapped = generator.generate_enum_variant_to_enum(
		context.type_store,
		pointee_type_id,
		enum_shape_index,
		enum_specialization_index,
		variant_index,
		Some(string_binding),
	);

	let elements = vec![wrapped];
	Some(generator.generate_array_literal(context.type_store, &elements, pointee_type_id, type_id, debug_location))
}

fn generate_enum_variant_to_enum<'a, 'b, G: Generator>(
	context: &mut Context<'a, 'b>,
	generator: &mut G,
	conversion: &'b EnumVariantToEnum<'a>,
) -> Option<G::Binding> {
	let expression_type_id = context.specialize_type_id(conversion.expression.type_id);
	let entry = context.type_store.type_entries.get(expression_type_id);
	let variant_index = match entry.kind {
		TypeEntryKind::UserType { shape_index, .. } => match &context.type_store.user_types.read()[shape_index].read().kind {
			UserTypeKind::Struct { shape } => shape.variant_index.unwrap(),
			kind => unreachable!("{kind:?}"),
		},

		kind => unreachable!("{kind:?}"),
	};

	let variant_binding = generate_expression(context, generator, &conversion.expression);

	let type_id = context.specialize_type_id(conversion.type_id);
	let entry = context.type_store.type_entries.get(type_id);
	let (shape_index, specialization_index) = match entry.kind {
		TypeEntryKind::UserType { shape_index, specialization_index } => (shape_index, specialization_index),
		_ => unreachable!("{:?}", entry.kind),
	};

	Some(generator.generate_enum_variant_to_enum(
		context.type_store,
		type_id,
		shape_index,
		specialization_index,
		variant_index,
		variant_binding,
	))
}

fn generate_binding<'a, 'b, G: Generator>(
	context: &mut Context<'a, 'b>,
	generator: &mut G,
	binding: &'b Binding<'a>,
	debug_location: DebugLocation,
) {
	let value = generate_expression(context, generator, &binding.expression);
	let type_id = context.specialize_type_id(binding.type_id);
	generator.generate_binding(binding.readable_index, value, type_id, binding.name, debug_location);
}

fn generate_break<G: Generator>(generator: &mut G, statement: &Break, debug_location: DebugLocation) {
	generator.generate_break(statement.loop_index, debug_location);
}

fn generate_continue<G: Generator>(generator: &mut G, statement: &Continue, debug_location: DebugLocation) {
	generator.generate_continue(statement.loop_index, debug_location);
}

fn generate_intrinsic<'a, 'b, G: Generator>(
	context: &mut Context<'a, 'b>,
	generator: &mut G,
	function_id: FunctionId,
	call: &'b Call<'a>,
	debug_location: DebugLocation,
) -> Option<G::Binding> {
	let span = call.span;

	let lock = context.function_store.shapes.read()[function_id.function_shape_index]
		.as_ref()
		.unwrap()
		.clone();
	let shape = lock.read();
	let specialization = &shape.specializations[function_id.specialization_index];

	match call.name {
		"size_of" => {
			assert_eq!(specialization.type_arguments.explicit_len, 1);
			let type_id = context.specialize_type_id(specialization.type_arguments.explicit_ids()[0]);
			let size = context.type_store.type_layout(type_id).size as i128;
			let integer = IntegerValue::new_collapsed(size, span, context.type_store.isize_type_id());
			generate_integer_value(context, generator, &integer)
		}

		"alignment_of" => {
			assert_eq!(specialization.type_arguments.explicit_len, 1);
			let type_id = context.specialize_type_id(specialization.type_arguments.explicit_ids()[0]);
			let alignment = context.type_store.type_layout(type_id).alignment as i128;
			let integer = IntegerValue::new_collapsed(alignment, span, context.type_store.isize_type_id());
			generate_integer_value(context, generator, &integer)
		}

		"create_slice" | "create_slice_mut" => {
			assert_eq!(specialization.type_arguments.explicit_len, 1);
			assert_eq!(specialization.parameters.len(), 2);
			assert_eq!(call.arguments.len(), 2);

			let pointer = generate_expression(context, generator, &call.arguments[0]).unwrap();
			let length = generate_expression(context, generator, &call.arguments[1]).unwrap();

			Some(generator.generate_slice(specialization.return_type, pointer, length, debug_location))
		}

		"create_str" => {
			assert_eq!(specialization.type_arguments.explicit_len, 0);
			assert_eq!(specialization.parameters.len(), 2);
			assert_eq!(call.arguments.len(), 2);

			let pointer = generate_expression(context, generator, &call.arguments[0]).unwrap();
			let length = generate_expression(context, generator, &call.arguments[1]).unwrap();

			Some(generator.generate_slice(specialization.return_type, pointer, length, debug_location))
		}

		"debugger_break" => {
			assert_eq!(specialization.type_arguments.explicit_len, 0);
			assert_eq!(specialization.parameters.len(), 0);

			generator.generate_debugger_break(debug_location);
			None
		}

		"user_main_function" => {
			assert_eq!(specialization.type_arguments.explicit_len, 0);
			assert_eq!(specialization.parameters.len(), 0);

			if let Some(main) = *context.function_store.main.read() {
				generator.generate_call(context.type_store, main, &[], debug_location);
			}
			None
		}

		_ => unreachable!(),
	}
}
