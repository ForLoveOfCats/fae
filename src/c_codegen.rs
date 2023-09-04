use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};

use crate::error::Messages;
use crate::ir::*;
use crate::tree::BinaryOperator;
use crate::type_store::*;
use crate::validator::FunctionStore;

const CC: &str = "clang";

type Result<T> = std::io::Result<T>;
// type Output<'a> = &'a mut std::process::ChildStdin;
type Output<'a> = &'a mut Vec<u8>;

#[allow(unused)]
pub enum OptimizationLevel {
	None,
	Release,
}

struct Context<'a, 'b> {
	messages: &'b mut Messages<'a>,
	type_store: &'b mut TypeStore<'a>,
	function_store: &'b mut FunctionStore<'a>,
	function_generate_queue: &'b mut Vec<FunctionId>,
	function_type_arguments: &'b [TypeId],
	function_id: FunctionId,
	next_temp_id: u64,
}

impl<'a, 'b> Context<'a, 'b> {
	fn generate_temp_id(&mut self) -> u64 {
		let id = self.next_temp_id;
		self.next_temp_id += 1;
		id
	}
}

pub fn generate_code<'a>(
	messages: &mut Messages<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &mut FunctionStore<'a>,
	optimization_level: OptimizationLevel,
	binary_path: &Path,
) {
	// Ignore failure, it's probably just because there's no file there yet
	_ = std::fs::remove_file(binary_path);

	let optimization_flag = match optimization_level {
		OptimizationLevel::None => "-O0",
		OptimizationLevel::Release => "-O2",
	};

	let mut cc = Command::new(CC)
		.args(&[
			"-x",
			"c",
			"-std=c11",
			"-pedantic",
			"-funsigned-char",
			"-Wall",
			"-Wextra",
			"-Werror",
			"-Wvla",
			"-Wshadow",
			"-Wno-format",
			"-Wno-unused-variable",
			"-Wno-unused-parameter",
			"-Wno-unused-but-set-variable",
			optimization_flag,
			"-o",
		])
		.arg(binary_path)
		.arg("-")
		.stdin(Stdio::piped())
		.stdout(Stdio::piped())
		.stderr(Stdio::piped())
		.spawn()
		.expect("Failed to launch C compiler");

	let mut stdin = cc.stdin.take().expect("Failed to get C compiler stdin");
	let stderr = cc.stderr.take().expect("Failed to get C compiler stdout");

	let mut output: Vec<u8> = Vec::new();

	generate_initial_output(&mut output).unwrap();

	for &description in &type_store.user_type_generate_order {
		forward_declare_user_type(type_store, description, &mut output).unwrap();
	}

	for &description in &type_store.slice_descriptions {
		generate_slice_specialization(type_store, description, &mut output).unwrap();
	}

	for &description in &type_store.user_type_generate_order {
		generate_user_type(type_store, description, &mut output).unwrap();
	}

	let main = function_store.main.unwrap();
	let mut function_generate_queue = vec![main];
	while let Some(function) = function_generate_queue.pop() {
		generate_function(messages, type_store, function_store, &mut function_generate_queue, function, &mut output)
			.unwrap();
	}

	let output = String::from_utf8(output).unwrap();
	println!("{output}");

	write!(stdin, "{output}").expect("Failed to write output to C compiler stdin");
	drop(stdin);

	if !cc.wait().unwrap().success() {
		println!("{}", std::io::read_to_string(stderr).expect("Failed to read C compiler stderr"));
		panic!("C Compiler failed");
	}
}

fn generate_initial_output(output: Output) -> Result<()> {
	write!(output, "{}\n", include_str!("./initial_output.c"))
}

fn forward_declare_user_type(
	type_store: &TypeStore,
	description: UserTypeSpecializationDescription,
	output: Output,
) -> Result<()> {
	let user_type = &type_store.user_types[description.shape_index];
	match &user_type.kind {
		UserTypeKind::Struct { shape } => {
			let specialization = &shape.specializations[description.specialization_index];
			let entry = &type_store.type_entries[specialization.type_id.index()];
			if entry.generic_poisoned {
				return Ok(());
			}

			write!(output, "typedef struct ")?;
			generate_raw_type_id(type_store, specialization.type_id, output)?;

			write!(output, " ")?;
			generate_raw_type_id(type_store, specialization.type_id, output)?;
			write!(output, ";\n\n")?;
		}
	}

	Ok(())
}

fn generate_user_type(
	type_store: &TypeStore,
	description: UserTypeSpecializationDescription,
	output: Output,
) -> Result<()> {
	let user_type = &type_store.user_types[description.shape_index];
	match &user_type.kind {
		UserTypeKind::Struct { shape } => {
			let specialization = &shape.specializations[description.specialization_index];
			let entry = &type_store.type_entries[specialization.type_id.index()];
			if entry.generic_poisoned {
				return Ok(());
			}

			write!(output, "typedef struct ")?;
			generate_raw_type_id(type_store, specialization.type_id, output)?;
			write!(output, " {{\n")?;

			for (index, field) in specialization.fields.iter().enumerate() {
				generate_raw_type_id(type_store, field.type_id, output)?;
				write!(output, " fi_{index};\n")?;
			}

			write!(output, "}} ")?;
			generate_raw_type_id(type_store, specialization.type_id, output)?;
			write!(output, ";\n\n")?;
		}
	}

	Ok(())
}

fn generate_slice_specialization(type_store: &TypeStore, description: SliceDescription, output: Output) -> Result<()> {
	write!(output, "typedef struct {{ ")?;
	generate_raw_type_id(type_store, description.sliced_type_id, output)?;
	write!(output, " const *items; usize len; }} sl_{};\n\n", description.entry)?;

	write!(output, "typedef struct {{ ")?;
	generate_raw_type_id(type_store, description.sliced_type_id, output)?;
	write!(output, " *items; usize len; }} sl_{};\n\n", description.entry + 1)
}

fn generate_function_signature<'a>(
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	function_id: FunctionId,
	output: Output,
) -> Result<()> {
	let shape = &function_store.shapes[function_id.function_shape_index];
	let specialization = &shape.specializations[function_id.specialization_index];

	generate_raw_type_id(type_store, specialization.return_type, output)?;
	write!(output, " ")?;

	if shape.is_main {
		write!(output, "fae_main")?;
	} else {
		generate_functon_id(function_id, output)?;
	}

	write!(output, "(")?;

	let mut first = true;
	for parameter in &specialization.parameters {
		if !first {
			write!(output, ", ")?;
		}
		first = false;
		generate_raw_type_id(type_store, parameter.type_id, output)?;
		write!(output, " ")?;
		generate_readable_index(parameter.readable_index, output)?;
	}
	if specialization.parameters.is_empty() {
		write!(output, "void")?;
	}
	write!(output, ")")
}

fn generate_function<'a>(
	messages: &mut Messages<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &mut FunctionStore<'a>,
	function_generate_queue: &mut Vec<FunctionId>,
	function_id: FunctionId,
	output: Output,
) -> Result<()> {
	let shape = &mut function_store.shapes[function_id.function_shape_index];
	let specialization = &mut shape.specializations[function_id.specialization_index];

	assert_eq!(specialization.been_generated, false);
	specialization.been_generated = true;

	let shape = &function_store.shapes[function_id.function_shape_index];
	let specialization = &shape.specializations[function_id.specialization_index];

	for type_argument in &specialization.type_arguments {
		let entry = &type_store.type_entries[type_argument.index()];
		if entry.generic_poisoned {
			return Ok(());
		}
	}

	generate_function_signature(type_store, function_store, function_id, output)?;
	write!(output, "{{\n")?;

	let block = shape.block.clone();
	let type_arguments = specialization.type_arguments.clone();

	let mut context = Context {
		messages,
		type_store,
		function_store,
		function_type_arguments: &type_arguments,
		function_id,
		function_generate_queue,
		next_temp_id: 0,
	};

	generate_block(&mut context, block.as_ref().unwrap(), output)?;

	write!(output, "}}\n\n")
}

fn generate_block(context: &mut Context, block: &Block, output: Output) -> Result<()> {
	for statement in &block.statements {
		match &statement.kind {
			StatementKind::Expression(expression) => {
				generate_expression(context, expression, output)?;
			}

			StatementKind::Block(block) => {
				// Probably wrong?
				generate_block(context, block, output)?;
			}

			StatementKind::Binding(binding) => {
				let id = generate_expression(context, &binding.expression, output)?.unwrap();
				generate_type_id(context, binding.type_id, output)?;
				write!(output, " ")?;
				generate_readable_index(binding.readable_index, output)?;
				write!(output, " = t_{id};\n")?;
			}

			StatementKind::Return(statement) => {
				if let Some(expression) = &statement.expression {
					let id = generate_expression(context, expression, output)?.unwrap();
					write!(output, "return t_{id};\n")?;
				} else {
					write!(output, "return;\n")?;
				}
			}
		}
	}

	Ok(())
}

fn generate_struct_literal(context: &mut Context, literal: &StructLiteral, output: Output) -> Result<u64> {
	let mut ids = Vec::new(); // Belch
	for initalizer in &literal.field_initializers {
		let id = generate_expression(context, &initalizer.expression, output)?.unwrap();
		ids.push(id);
	}

	let id = context.generate_temp_id();
	generate_type_id(context, literal.type_id, output)?;
	write!(output, " t_{id} = ")?;

	generate_struct_construction_open(context, literal.type_id, output)?;
	for (index, id) in ids.into_iter().enumerate() {
		write!(output, ".fi_{index} = t_{id}, ")?;
	}
	generate_struct_construction_close(output)?;

	write!(output, ";\n")?;
	Ok(id)
}

fn generate_call(context: &mut Context, call: &Call, output: Output) -> Result<Option<u64>> {
	let function_id = context.function_store.specialize_with_function_generics(
		context.messages,
		context.type_store,
		call.function_id,
		context.function_id.function_shape_index,
		context.function_type_arguments,
	);

	let mut ids = Vec::new(); // Belch
	for argument in &call.arguments {
		let id = generate_expression(context, argument, output)?.unwrap();
		ids.push(id);
	}

	let shape = &mut context.function_store.shapes[function_id.function_shape_index];
	let specialization = &mut shape.specializations[function_id.specialization_index];

	if !specialization.been_generated {
		if !specialization.been_queued {
			specialization.been_queued = true;
			context.function_generate_queue.push(function_id);
		}

		generate_function_signature(context.type_store, context.function_store, function_id, output)?;
		write!(output, ";\n")?;
	}

	let shape = &context.function_store.shapes[function_id.function_shape_index];
	let specialization = &shape.specializations[function_id.specialization_index];

	let mut maybe_id = None;
	let void = context.type_store.void_type_id();
	if !context.type_store.direct_equal(specialization.return_type, void) {
		let id = context.next_temp_id;
		context.next_temp_id += 1;
		maybe_id = Some(id);
		let return_type = specialization.return_type;
		generate_type_id(context, return_type, output)?;
		write!(output, " t_{id} = ")?;
	}

	generate_functon_id(function_id, output)?;
	write!(output, "(")?;

	let mut first = true;
	for id in ids {
		if !first {
			write!(output, ", ")?;
		}
		first = false;
		write!(output, "t_{id}")?;
	}

	write!(output, ");\n")?;

	Ok(maybe_id)
}

fn generate_unary_operation(context: &mut Context, operation: &UnaryOperation, output: Output) -> Result<u64> {
	let id = generate_expression(context, &operation.expression, output)?.unwrap();

	let op = match operation.op {
		UnaryOperator::Negate => "-",
	};

	write!(output, "t_{id} = {op}(t_{id});\n")?;

	Ok(id)
}

fn generate_binary_operation(context: &mut Context, operation: &BinaryOperation, output: Output) -> Result<u64> {
	let left_id = generate_expression(context, &operation.left, output)?.unwrap();
	let right_id = generate_expression(context, &operation.right, output)?.unwrap();

	if operation.op == BinaryOperator::Assign {
		write!(output, "t_{left_id} = t_{right_id};\n")?;
		return Ok(left_id);
	}

	write!(output, "t_{left_id} = (t_{left_id}")?;

	let op = match operation.op {
		BinaryOperator::Assign => unreachable!(),
		BinaryOperator::Add => "+",
		BinaryOperator::Sub => "-",
		BinaryOperator::Mul => "*",
		BinaryOperator::Div => "/",
	};
	write!(output, " {} ", op)?;

	write!(output, "t_{right_id});\n")?;

	Ok(left_id)
}

fn generate_expression(context: &mut Context, expression: &Expression, output: Output) -> std::io::Result<Option<u64>> {
	let id = context.generate_temp_id();

	match &expression.kind {
		ExpressionKind::IntegerValue(value) => {
			let type_id = value.get_collapse();
			generate_type_id(context, type_id, output)?;
			write!(output, " t_{id} = {};\n", value.value())?;
		}

		ExpressionKind::DecimalValue(value) => {
			let type_id = value.get_collapse();
			generate_type_id(context, type_id, output)?;
			write!(output, " t_{id} = {};\n", value.value())?;
		}

		ExpressionKind::CodepointLiteral(literal) => write!(output, "u64 t_{id} = {};\n", literal.value as u32)?,

		ExpressionKind::StringLiteral(literal) => {
			let type_id = context.type_store.string_type_id();
			generate_type_id(context, type_id, output)?;
			write!(output, " t_{id} = ")?;
			generate_struct_construction_open(context, type_id, output)?;
			write!(output, ".items = (u8*){:?}, .len = {}", literal.value, literal.value.len())?;
			generate_struct_construction_close(output)?;
			write!(output, ";\n")?;
		}

		ExpressionKind::StructLiteral(literal) => {
			return generate_struct_literal(context, literal, output).map(|id| Some(id));
		}

		ExpressionKind::Call(call) => return generate_call(context, call, output),

		ExpressionKind::Read(read) => {
			generate_type_id(context, read.type_id, output)?;
			write!(output, " t_{id} = ")?;
			generate_readable_index(read.readable_index, output)?;
			write!(output, ";\n")?;
		}

		ExpressionKind::UnaryOperation(operation) => {
			return generate_unary_operation(context, operation, output).map(|id| Some(id));
		}

		ExpressionKind::BinaryOperation(operation) => {
			return generate_binary_operation(context, operation, output).map(|id| Some(id));
		}

		kind => unimplemented!("expression {kind:?}"),
	}

	Ok(Some(id))
}

fn generate_struct_construction_open(context: &mut Context, type_id: TypeId, output: Output) -> Result<()> {
	write!(output, "((")?;
	generate_type_id(context, type_id, output)?;
	write!(output, ") {{ ")
}

fn generate_struct_construction_close(output: Output) -> Result<()> {
	write!(output, " }})")
}

fn generate_type_id(context: &mut Context, type_id: TypeId, output: Output) -> Result<()> {
	let mut generic_usages = Vec::new();
	let type_id = context.type_store.specialize_with_function_generics(
		context.messages,
		&mut generic_usages,
		context.function_id.function_shape_index,
		context.function_type_arguments,
		type_id,
	);
	assert_eq!(generic_usages.len(), 0);

	generate_raw_type_id(context.type_store, type_id, output)
}

fn generate_raw_type_id(type_store: &TypeStore, type_id: TypeId, output: Output) -> Result<()> {
	let entry = &type_store.type_entries[type_id.index()];
	match &entry.kind {
		TypeEntryKind::BuiltinType { kind } => write!(output, "{}", kind.name()),

		TypeEntryKind::UserType { shape_index, specialization_index } => {
			write!(output, "ty_{shape_index}_{specialization_index}")
		}

		TypeEntryKind::Pointer { type_id, .. } => {
			write!(output, "*")?;
			generate_raw_type_id(type_store, *type_id, output)
		}

		TypeEntryKind::Slice { .. } => write!(output, "sl_{}", type_id.index()),

		TypeEntryKind::UserTypeGeneric { .. } => unreachable!(),
		TypeEntryKind::FunctionGeneric { .. } => unreachable!(),
	}
}

fn generate_functon_id(function_id: FunctionId, output: Output) -> Result<()> {
	write!(output, "fn_{}_{}", function_id.function_shape_index, function_id.specialization_index)
}

fn generate_readable_index(readable_index: usize, output: Output) -> Result<()> {
	write!(output, "re_{}", readable_index)
}
