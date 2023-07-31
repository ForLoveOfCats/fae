use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};

use crate::error::Messages;
use crate::ir::*;
use crate::tree::BinaryOperator;
use crate::type_store::*;
use crate::validator::FunctionStore;

const CC: &str = "clang";

type Result = std::io::Result<()>;
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
	function_store: &'b FunctionStore<'a>,
	function_type_arguments: &'b [TypeId],
	function_id: FunctionId,
}

pub fn generate_code<'a>(
	messages: &mut Messages<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
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
			"-Wvla",
			"-Wshadow",
			"-Werror",
			"-Wno-format",
			"-Wno-unused-variable",
			"-Wno-unused-parameter",
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

	for &description in function_store.generate_order() {
		generate_function(messages, type_store, function_store, description, &mut output).unwrap();
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

fn generate_initial_output(output: Output) -> Result {
	write!(output, "{}\n", include_str!("./initial_output.c"))
}

fn forward_declare_user_type(
	type_store: &TypeStore,
	description: UserTypeSpecializationDescription,
	output: Output,
) -> Result {
	let user_type = &type_store.user_types[description.shape_index];
	match &user_type.kind {
		UserTypeKind::Struct { shape } => {
			let specialization = &shape.specializations[description.specialization_index];
			let entry = &type_store.type_entries[specialization.type_id.index()];
			if entry.generic_poisoned {
				return Ok(());
			}

			write!(output, "typedef struct ")?;
			generate_type_id(type_store, specialization.type_id, output)?;

			write!(output, " ")?;
			generate_type_id(type_store, specialization.type_id, output)?;
			write!(output, ";\n\n")?;
		}
	}

	Ok(())
}

fn generate_user_type(
	type_store: &TypeStore,
	description: UserTypeSpecializationDescription,
	output: Output,
) -> Result {
	let user_type = &type_store.user_types[description.shape_index];
	match &user_type.kind {
		UserTypeKind::Struct { shape } => {
			let specialization = &shape.specializations[description.specialization_index];
			let entry = &type_store.type_entries[specialization.type_id.index()];
			if entry.generic_poisoned {
				return Ok(());
			}

			write!(output, "typedef struct ")?;
			generate_type_id(type_store, specialization.type_id, output)?;
			write!(output, " {{\n")?;

			for field in &specialization.fields {
				generate_type_id(type_store, field.type_id, output)?;
				write!(output, " fi_{};\n", field.field_index)?;
			}

			write!(output, "}} ")?;
			generate_type_id(type_store, specialization.type_id, output)?;
			write!(output, ";\n\n")?;
		}
	}

	Ok(())
}

fn generate_slice_specialization(type_store: &TypeStore, description: SliceDescription, output: Output) -> Result {
	write!(output, "typedef struct {{ ")?;
	generate_type_id(type_store, description.sliced_type_id, output)?;
	write!(output, " const *items; usize len; }} sl_{};\n\n", description.entry)?;

	write!(output, "typedef struct {{ ")?;
	generate_type_id(type_store, description.sliced_type_id, output)?;
	write!(output, " *items; usize len; }} sl_{};\n\n", description.entry + 1)
}

fn generate_function<'a>(
	messages: &mut Messages<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	function_id: FunctionId,
	output: Output,
) -> Result {
	let shape = &function_store.shapes()[function_id.function_shape_index];
	let specialization = &shape.specializations[function_id.specialization_index];

	for type_argument in &specialization.type_arguments {
		let entry = &type_store.type_entries[type_argument.index()];
		if entry.generic_poisoned {
			return Ok(());
		}
	}

	generate_type_id(type_store, specialization.return_type, output)?;
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
		generate_type_id(type_store, parameter.type_id, output)?;
		write!(output, " ")?;
		generate_readable_index(parameter.readable_index, output)?;
	}
	if specialization.parameters.is_empty() {
		write!(output, "void")?;
	}
	write!(output, ") {{\n")?;

	let mut context = Context {
		messages,
		type_store,
		function_store,
		function_type_arguments: &specialization.type_arguments,
		function_id,
	};

	generate_block(&mut context, shape.block.as_ref().unwrap(), output)?;

	write!(output, "}}\n\n")
}

fn generate_block(context: &mut Context, block: &Block, output: Output) -> Result {
	for statement in &block.statements {
		match &statement.kind {
			StatementKind::Expression(expression) => {
				generate_expression(context, expression, output)?;
				write!(output, ";\n")?;
			}

			StatementKind::Block(block) => {
				// Probably wrong?
				generate_block(context, block, output)?;
			}

			StatementKind::Const(_) => {}

			StatementKind::Binding(binding) => {
				generate_type_id(context.type_store, binding.type_id, output)?;
				write!(output, " ")?;
				generate_readable_index(binding.readable_index, output)?;
				write!(output, " = ")?;
				generate_expression(context, &binding.expression, output)?;
				write!(output, ";\n")?;
			}

			StatementKind::Return(statement) => {
				if let Some(expression) = &statement.expression {
					write!(output, "return ")?;
					generate_expression(context, expression, output)?;
					write!(output, ";\n")?;
				} else {
					write!(output, "return;\n")?;
				}
			}
		}
	}

	Ok(())
}

fn generate_struct_literal(context: &mut Context, literal: &StructLiteral, output: Output) -> Result {
	generate_struct_construction_open(context.type_store, literal.type_id, output)?;

	for initalizer in &literal.field_initializers {
		write!(output, ".fi_{} = ", initalizer.field_index)?;
		generate_expression(context, &initalizer.expression, output)?;
		write!(output, ", ")?;
	}

	generate_struct_construction_close(output)
}

fn generate_call(context: &mut Context, call: &Call, output: Output) -> Result {
	let function_id = context.function_store.specialize_with_function_generics(
		context.messages,
		context.type_store,
		call.function_id,
		context.function_id.function_shape_index,
		context.function_type_arguments,
	);

	generate_functon_id(function_id, output)?;
	write!(output, "(")?;

	let mut first = true;
	for argument in &call.arguments {
		if !first {
			write!(output, ", ")?;
		}
		first = false;
		generate_expression(context, argument, output)?;
	}

	write!(output, ")")
}

fn generate_binary_operation(context: &mut Context, operation: &BinaryOperation, output: Output) -> Result {
	write!(output, "(")?;
	generate_expression(context, &operation.left, output)?;

	let op = match operation.op {
		BinaryOperator::Assign => "=",
		BinaryOperator::Add => "+",
		BinaryOperator::Sub => "-",
		BinaryOperator::Mul => "*",
		BinaryOperator::Div => "/",
	};
	write!(output, " {} ", op)?;

	generate_expression(context, &operation.right, output)?;
	write!(output, ")")
}

fn generate_expression(context: &mut Context, expression: &Expression, output: Output) -> Result {
	match &expression.kind {
		ExpressionKind::IntegerLiteral(literal) => write!(output, "{}", literal.value),
		ExpressionKind::FloatLiteral(literal) => write!(output, "{}", literal.value),

		ExpressionKind::CodepointLiteral(literal) => write!(output, "{}", literal.value as u32),

		ExpressionKind::StringLiteral(literal) => {
			let type_id = context.type_store.string_type_id();
			generate_struct_construction_open(context.type_store, type_id, output)?;
			write!(output, ".items = (u8*){:?}, .len = {}", literal.value, literal.value.len())?;
			generate_struct_construction_close(output)
		}

		ExpressionKind::StructLiteral(literal) => generate_struct_literal(context, literal, output),

		ExpressionKind::Call(call) => generate_call(context, call, output),

		ExpressionKind::Read(read) => generate_readable_index(read.readable_index, output),

		ExpressionKind::BinaryOperation(operation) => generate_binary_operation(context, operation, output),

		kind => unimplemented!("expression {kind:?}"),
	}
}

fn generate_struct_construction_open(type_store: &TypeStore, type_id: TypeId, output: Output) -> Result {
	write!(output, "((")?;
	generate_type_id(type_store, type_id, output)?;
	write!(output, ") {{ ")
}

fn generate_struct_construction_close(output: Output) -> Result {
	write!(output, " }})")
}

fn generate_type_id(type_store: &TypeStore, type_id: TypeId, output: Output) -> Result {
	let entry = &type_store.type_entries[type_id.index()];
	match &entry.kind {
		TypeEntryKind::BuiltinType { kind } => write!(output, "{}", kind.name()),

		TypeEntryKind::UserType { shape_index, specialization_index } => {
			write!(output, "ty_{shape_index}_{specialization_index}")
		}

		TypeEntryKind::Pointer { type_id, .. } => {
			write!(output, "*")?;
			generate_type_id(type_store, *type_id, output)
		}

		TypeEntryKind::Slice { type_id, .. } => write!(output, "sl_{}", type_id.entry),

		TypeEntryKind::UserTypeGeneric { .. } => unreachable!(),
		TypeEntryKind::FunctionGeneric { .. } => unreachable!(),
	}
}

fn generate_functon_id(function_id: FunctionId, output: Output) -> Result {
	write!(output, "fn_{}_{}", function_id.function_shape_index, function_id.specialization_index)
}

fn generate_readable_index(readable_index: usize, output: Output) -> Result {
	write!(output, "re_{}", readable_index)
}
