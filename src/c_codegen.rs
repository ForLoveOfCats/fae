use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};

use crate::ir::*;
use crate::tree::BinaryOperator;
use crate::validator::{FunctionStore, TypeStore};

const CC: &str = "clang";

type Result = std::io::Result<()>;
// type Output<'a> = &'a mut std::process::ChildStdin;
type Output<'a> = &'a mut Vec<u8>;

#[allow(unused)]
pub enum OptimizationLevel {
	None,
	Release,
}

pub fn generate_code(
	type_store: &TypeStore,
	function_store: &FunctionStore,
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

	for (index, user_type) in type_store.user_types().iter().enumerate() {
		forward_declare_user_type(type_store, index + type_store.primative_len(), user_type, &mut output).unwrap();
	}

	for &specialization in type_store.slice_specializations() {
		generate_slice_specializations(type_store, specialization, &mut output).unwrap();
	}

	for (index, user_type) in type_store.user_types().iter().enumerate() {
		generate_user_type(type_store, index + type_store.primative_len(), user_type, &mut output).unwrap();
	}

	for (shape_index, shape) in function_store.shapes().iter().enumerate() {
		for (specialization_index, concrete) in shape.concrete.iter().enumerate() {
			let function_id = FunctionId { shape_index, specialization_index };
			generate_function(type_store, concrete, function_id, shape.is_main, &mut output).unwrap();
		}
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

fn forward_declare_user_type(type_store: &TypeStore, index: usize, user_type: &UserType, output: Output) -> Result {
	match &user_type.kind {
		UserTypeKind::Struct { shape } => {
			for specialization in 0..shape.concrete.len() {
				write!(output, "typedef struct ")?;
				generate_type_id(type_store, TypeId { index, specialization }, output)?;
				write!(output, " ")?;
				generate_type_id(type_store, TypeId { index, specialization }, output)?;
				write!(output, ";\n\n")?;
			}
		}
	}

	Ok(())
}

fn generate_user_type(type_store: &TypeStore, index: usize, user_type: &UserType, output: Output) -> Result {
	match &user_type.kind {
		UserTypeKind::Struct { shape } => {
			for (specialization, concrete) in shape.concrete.iter().enumerate() {
				write!(output, "typedef struct ")?;
				generate_type_id(type_store, TypeId { index, specialization }, output)?;
				write!(output, " {{\n")?;

				for field in &concrete.fields {
					generate_type_id(type_store, field.type_id, output)?;
					// TODO: These should have an id instead of using the name here
					write!(output, " {};\n", field.name)?;
				}

				write!(output, "}} ")?;
				generate_type_id(type_store, TypeId { index, specialization }, output)?;
				write!(output, ";\n\n")?;
			}
		}
	}

	Ok(())
}

fn generate_slice_specializations(type_store: &TypeStore, specialization: usize, output: Output) -> Result {
	write!(output, "typedef struct {{ ")?;

	let sliced_id = TypeStore::unpack_ref_slice_specialization(specialization);
	generate_type_id(type_store, sliced_id, output)?;
	write!(output, " *items; usize len; }} ")?;

	let type_id = TypeId { index: type_store.slice_type_index(), specialization };
	generate_type_id(type_store, type_id, output)?;

	write!(output, ";\n\n")
}

fn generate_function(
	type_store: &TypeStore,
	function: &Function,
	function_id: FunctionId,
	is_main: bool,
	output: Output,
) -> Result {
	generate_type_id(type_store, function.return_type, output)?;
	write!(output, " ")?;

	if is_main {
		write!(output, "fae_main")?;
	} else {
		generate_functon_id(function_id, output)?;
	}

	write!(output, "(")?;

	let mut first = true;
	for parameter in &function.parameters {
		if !first {
			write!(output, ", ")?;
		}
		first = false;
		generate_type_id(type_store, parameter.type_id, output)?;
		write!(output, " ")?;
		generate_readable_index(parameter.readable_index, output)?;
	}
	if function.parameters.is_empty() {
		write!(output, "void")?;
	}
	write!(output, ") {{\n")?;

	generate_block(type_store, function.block.as_ref().unwrap(), output)?;

	write!(output, "}}\n\n")
}

fn generate_block(type_store: &TypeStore, block: &Block, output: Output) -> Result {
	for statement in &block.statements {
		match &statement.kind {
			StatementKind::Expression(expression) => {
				generate_expression(expression, output)?;
				write!(output, ";\n")?;
			}

			StatementKind::Block(block) => generate_block(type_store, block, output)?, // Probably wrong?

			StatementKind::Const(_) => {}

			StatementKind::Binding(binding) => {
				generate_type_id(type_store, binding.type_id, output)?;
				write!(output, " ")?;
				generate_readable_index(binding.readable_index, output)?;
				write!(output, " = ")?;
				generate_expression(&binding.expression, output)?;
				write!(output, ";\n")?;
			}

			StatementKind::Return(statement) => {
				if let Some(expression) = &statement.expression {
					write!(output, "return ")?;
					generate_expression(expression, output)?;
					write!(output, ";\n")?;
				} else {
					write!(output, "return;\n")?;
				}
			}
		}
	}

	Ok(())
}

fn generate_binary_operation(operation: &BinaryOperation, output: Output) -> Result {
	write!(output, "(")?;
	generate_expression(&operation.left, output)?;

	let op = match operation.op {
		BinaryOperator::Assign => "=",
		BinaryOperator::Add => "+",
		BinaryOperator::Sub => "-",
		BinaryOperator::Mul => "*",
		BinaryOperator::Div => "/",
	};
	write!(output, " {} ", op)?;

	generate_expression(&operation.right, output)?;
	write!(output, ")")
}

fn generate_expression(expression: &Expression, output: Output) -> Result {
	match &expression.kind {
		ExpressionKind::IntegerLiteral(literal) => write!(output, "{}", literal.value)?,
		ExpressionKind::FloatLiteral(literal) => write!(output, "{}", literal.value)?,

		ExpressionKind::CharLiteral(literal) => write!(output, "{}", literal.value as u32)?,
		ExpressionKind::StringLiteral(literal) => write!(output, "{:?}", literal.value)?, // This will fail in some cases

		ExpressionKind::Call(call) => {
			generate_functon_id(call.function_id, output)?;
			write!(output, "(")?;

			let mut first = true;
			for argument in &call.arguments {
				if !first {
					write!(output, ", ")?;
				}
				first = false;
				generate_expression(argument, output)?;
			}

			write!(output, ")")?;
		}

		ExpressionKind::Read(read) => generate_readable_index(read.readable_index, output)?,

		ExpressionKind::BinaryOperation(operation) => generate_binary_operation(operation, output)?,

		kind => unimplemented!("expression {kind:?}"),
	}

	Ok(())
}

fn generate_type_id(type_store: &TypeStore, type_id: TypeId, output: Output) -> Result {
	if type_store.is_reference(type_id) {
		write!(output, "*")?;
		let type_id = TypeStore::unpack_ref_slice_specialization(type_id.specialization);
		return generate_type_id(type_store, type_id, output);
	}

	if let Some(primative) = type_store.primative(type_id) {
		return write!(output, "{}", primative.name);
	}

	write!(output, "ty_{}_{}", type_id.index, type_id.specialization)
}

fn generate_functon_id(function_id: FunctionId, output: Output) -> Result {
	write!(output, "fn_{}_{}", function_id.shape_index, function_id.specialization_index)
}

fn generate_readable_index(readable_index: usize, output: Output) -> Result {
	write!(output, "re_{}", readable_index)
}
