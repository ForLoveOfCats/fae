use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};

use crate::error::Messages;
use crate::ir::*;
use crate::tree::BinaryOperator;
use crate::type_store::*;
use crate::validator::{CInclude, CIncludeStore, FunctionStore};

const CC: &str = "clang";
const DEBUG_SOURCE_DUMP_PATH: &str = "./output.c";

type Result<T> = std::io::Result<T>;
// type Output<'a> = &'a mut std::process::ChildStdin;
type Output<'a> = &'a mut Vec<u8>;

macro_rules! annotate_comment {
	($output:ident, $($arg:tt)*) => {
		writeln!($output, "// {}", format_args!( $($arg)* ))
	}
}

// macro_rules! annotate_comment {
// 	($($arg:tt)*) => {
// 		Result::Ok(())
// 	}
// }

#[allow(dead_code)]
#[derive(PartialEq, Eq)]
pub enum DebugCodegen {
	No,
	Yes,
	OnFailure,
}

#[allow(unused)]
pub enum OptimizationLevel {
	None,
	Release,
}

struct Context<'a, 'b> {
	messages: &'b mut Messages<'a>,
	type_store: &'b mut TypeStore<'a>,
	function_store: &'b mut FunctionStore<'a>,
	module_path: &'a [String],
	function_generate_queue: &'b mut Vec<FunctionId>,
	function_type_arguments: &'b TypeArguments,
	function_id: FunctionId,
	next_temp_id: u64,
}

impl<'a, 'b> Context<'a, 'b> {
	fn generate_temp_id(&mut self) -> TempId {
		let id = self.next_temp_id;
		self.next_temp_id += 1;
		TempId { id, dereference: false }
	}
}

#[derive(Debug, Copy, Clone)]
struct TempId {
	id: u64,
	dereference: bool,
}

impl std::fmt::Display for TempId {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if self.dereference {
			write!(f, "(*t_{})", self.id)
		} else {
			write!(f, "t_{}", self.id)
		}
	}
}

#[derive(Debug, Copy, Clone)]
enum Step {
	Temp { temp_id: TempId },
	Readable { readable_index: usize },
	ConstantInteger(i128),
	ConstantDecimal(f64),
	ConstantBool(bool),
	ConstantCodepoint(char),
}

impl std::fmt::Display for Step {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Step::Temp { temp_id } => temp_id.fmt(f),
			Step::Readable { readable_index } => write!(f, "re_{}", readable_index),
			Step::ConstantInteger(integer) => write!(f, "{}", integer),
			Step::ConstantDecimal(decimal) => write!(f, "{}", decimal),
			Step::ConstantBool(literal) => write!(f, "{}", literal),
			Step::ConstantCodepoint(codepoint) => write!(f, "{}", *codepoint as u32),
		}
	}
}

pub fn generate_code<'a>(
	messages: &mut Messages<'a>,
	c_include_store: &CIncludeStore<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &mut FunctionStore<'a>,
	optimization_level: OptimizationLevel,
	binary_path: &Path,
	debug_codegen: DebugCodegen,
) {
	// Ignore failure, it's probably just because there's no file there yet
	_ = std::fs::remove_file(binary_path);

	let optimization_flag = match optimization_level {
		OptimizationLevel::None => "-O0",
		OptimizationLevel::Release => "-O2",
	};

	let mut cc = Command::new(CC)
		.args([
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
			"-Wcast-qual",
			"-Wconversion",
			"-Wwrite-strings",
			"-Wno-pointer-sign",
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

	for include in &c_include_store.includes {
		generate_include(include, &mut output).unwrap();
	}

	for &description in &type_store.user_type_generate_order {
		forward_declare_user_type(type_store, description, &mut output).unwrap();
	}

	for &description in &type_store.slice_descriptions {
		generate_slice_specialization(type_store, description, &mut output).unwrap();
	}

	// Belch
	for description in type_store.user_type_generate_order.clone() {
		generate_user_type(type_store, function_store, description, &mut output).unwrap();
	}

	let main = function_store.main.unwrap();
	let mut function_generate_queue = vec![main];
	while let Some(function) = function_generate_queue.pop() {
		generate_function(messages, type_store, function_store, &mut function_generate_queue, function, &mut output).unwrap();
	}

	let output = String::from_utf8(output).unwrap();
	if debug_codegen == DebugCodegen::Yes {
		dump_codegen(&output);
	}

	write!(stdin, "{output}").expect("Failed to write output to C compiler stdin");
	drop(stdin);

	if !cc.wait().unwrap().success() {
		if debug_codegen == DebugCodegen::OnFailure {
			dump_codegen(&output);
		}

		println!("{}", std::io::read_to_string(stderr).expect("Failed to read C compiler stderr"));
		panic!("C Compiler failed");
	}
}

fn dump_codegen(output: &str) {
	std::fs::write(DEBUG_SOURCE_DUMP_PATH, output).unwrap();
	println!("\n{output}");
}

fn generate_initial_output(output: Output) -> Result<()> {
	writeln!(output, "{}", include_str!("./initial_output.c"))
}

fn generate_include(include: &CInclude, output: Output) -> Result<()> {
	match include {
		CInclude::System(include) => writeln!(output, "#include \"{}\"\n", include),
	}
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
			writeln!(output, ";\n")?;
		}
	}

	Ok(())
}

fn generate_user_type(
	type_store: &mut TypeStore,
	function_store: &FunctionStore,
	description: UserTypeSpecializationDescription,
	output: Output,
) -> Result<()> {
	match &type_store.user_types[description.shape_index].kind {
		UserTypeKind::Struct { shape } => {
			let specialization = &shape.specializations[description.specialization_index];
			let entry = &type_store.type_entries[specialization.type_id.index()];
			if entry.generic_poisoned {
				return Ok(());
			}

			if type_store.type_layout(specialization.type_id).size <= 0 {
				return Ok(());
			}
		}
	}

	let user_type = &type_store.user_types[description.shape_index];
	match &user_type.kind {
		UserTypeKind::Struct { shape } => {
			let specialization = &shape.specializations[description.specialization_index];
			annotate_comment!(
				output,
				"struct {}[{}]",
				shape.name,
				specialization
					.type_arguments
					.iter()
					.map(|id| type_store.internal_type_name(function_store, &[], *id))
					.collect::<Vec<_>>()
					.join(", ")
			)?;

			write!(output, "typedef struct ")?;
			let type_id = specialization.type_id;
			generate_raw_type_id(type_store, type_id, output)?;
			writeln!(output, " {{")?;

			// Belch
			let field_types: Vec<_> = specialization.fields.iter().map(|f| f.type_id).collect();
			for (index, type_id) in field_types.into_iter().enumerate() {
				if type_store.type_layout(type_id).size <= 0 {
					continue;
				}

				generate_raw_type_id(type_store, type_id, output)?;
				writeln!(output, " fi_{index};")?;
			}

			write!(output, "}} ")?;
			generate_raw_type_id(type_store, type_id, output)?;
			writeln!(output, ";\n")?;
		}
	}

	Ok(())
}

fn generate_slice_specialization(type_store: &TypeStore, description: SliceDescription, output: Output) -> Result<()> {
	write!(output, "typedef struct {{ ")?;
	generate_raw_type_id(type_store, description.sliced_type_id, output)?;
	writeln!(output, " const *fi_0; i64 fi_1; }} sl_{};\n", description.entry)?;

	write!(output, "typedef struct {{ ")?;
	generate_raw_type_id(type_store, description.sliced_type_id, output)?;
	writeln!(output, " *fi_0; i64 fi_1; }} sl_{};\n", description.entry + 1)
}

fn generate_function_signature<'a>(
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	function_id: FunctionId,
	output: Output,
) -> Result<()> {
	let shape = &function_store.shapes[function_id.function_shape_index];
	assert!(shape.extern_attribute.is_none(), "{:?}", shape.extern_attribute);
	let specialization = &shape.specializations[function_id.specialization_index];

	if type_store.type_layout(specialization.return_type).size > 0 {
		generate_raw_type_id(type_store, specialization.return_type, output)?;
		write!(output, " ")?;
	} else {
		write!(output, "void ")?;
	}

	if shape.is_main {
		write!(output, "fae_main")?;
	} else {
		generate_functon_id(function_id, output)?;
	}

	write!(output, "(")?;

	let mut first = true;
	for parameter in &specialization.parameters {
		if type_store.type_layout(parameter.type_id).size <= 0 {
			continue;
		}

		if !first {
			write!(output, ", ")?;
		}
		first = false;
		generate_raw_type_id(type_store, parameter.type_id, output)?;
		write!(output, " ")?;
		generate_readable_index(parameter.readable_index, output)?;
	}

	if first {
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
	assert!(shape.extern_attribute.is_none(), "{:?}", shape.extern_attribute);
	let specialization = &mut shape.specializations[function_id.specialization_index];

	assert!(!specialization.been_generated);
	specialization.been_generated = true;

	let shape = &function_store.shapes[function_id.function_shape_index];
	let specialization = &shape.specializations[function_id.specialization_index];

	for type_argument in specialization.type_arguments.ids() {
		let entry = &type_store.type_entries[type_argument.index()];
		if entry.generic_poisoned {
			return Ok(());
		}
	}

	// Belch
	annotate_comment!(
		output,
		"fn {}[{}][{}]",
		shape.name.item,
		specialization.type_arguments.ids()[..specialization.type_arguments.implicit_len()]
			.iter()
			.map(|id| type_store.internal_type_name(function_store, &[], *id))
			.collect::<Vec<_>>()
			.join(", "),
		specialization.type_arguments.ids()[specialization.type_arguments.implicit_len()..]
			.iter()
			.map(|id| type_store.internal_type_name(function_store, &[], *id))
			.collect::<Vec<_>>()
			.join(", ")
	)?;

	generate_function_signature(type_store, function_store, function_id, output)?;
	writeln!(output, " {{")?;

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
		function_generate_queue,
		next_temp_id: 0,
	};

	generate_block(&mut context, block.as_ref().unwrap(), output)?;

	writeln!(output, "}}\n")
}

fn generate_block(context: &mut Context, block: &Block, output: Output) -> Result<()> {
	for statement in &block.statements {
		match &statement.kind {
			StatementKind::Expression(expression) => {
				generate_expression(context, expression, output)?;
			}

			StatementKind::Block(block) => {
				writeln!(output, "{{")?;
				generate_block(context, block, output)?;
				writeln!(output, "}}")?;
			}

			StatementKind::Binding(binding) => {
				let Some(step) = generate_expression(context, &binding.expression, output)? else {
					continue;
				};

				if let Some(result) = generate_type_id(context, binding.type_id, output) {
					result?;
				} else {
					continue;
				}

				if binding.is_mutable {
					write!(output, " ")?;
				} else {
					write!(output, " const ")?;
				}

				generate_readable_index(binding.readable_index, output)?;
				writeln!(output, " = {step};")?;
			}

			StatementKind::Return(statement) => {
				if let Some(expression) = &statement.expression {
					if let Some(step) = generate_expression(context, expression, output)? {
						writeln!(output, "return {step};")?;
					}
				} else {
					writeln!(output, "return;")?;
				}
			}
		}
	}

	Ok(())
}

fn generate_struct_literal(context: &mut Context, literal: &StructLiteral, output: Output) -> Result<Option<Step>> {
	let mut field_steps = Vec::new(); // Belch
	for initalizer in &literal.field_initializers {
		if let Some(step) = generate_expression(context, &initalizer.expression, output)? {
			field_steps.push(step);
		}
	}

	if let Some(result) = generate_type_id(context, literal.type_id, output) {
		result?;
	} else {
		return Ok(None);
	}
	let temp_id = context.generate_temp_id();
	write!(output, " {temp_id} = ")?;

	generate_struct_construction_open(context, literal.type_id, output)?;
	for field_step in field_steps.into_iter() {
		write!(output, "{field_step}, ")?;
	}
	generate_struct_construction_close(output)?;

	writeln!(output, ";")?;
	Ok(Some(Step::Temp { temp_id }))
}

fn generate_call(context: &mut Context, call: &Call, output: Output) -> Result<Option<Step>> {
	let function_id = context.function_store.specialize_with_function_generics(
		context.messages,
		context.type_store,
		call.function_id,
		context.function_id.function_shape_index,
		context.function_type_arguments,
	);

	let mut steps = Vec::new(); // Belch
	for argument in &call.arguments {
		if let Some(step) = generate_expression(context, argument, output)? {
			steps.push(step);
		}
	}

	let shape = &mut context.function_store.shapes[function_id.function_shape_index];
	let specialization = &mut shape.specializations[function_id.specialization_index];

	if !specialization.been_generated && shape.extern_attribute.is_none() {
		if !specialization.been_queued {
			specialization.been_queued = true;
			context.function_generate_queue.push(function_id);
		}

		generate_function_signature(context.type_store, context.function_store, function_id, output)?;
		writeln!(output, ";")?;
	}

	let shape = &context.function_store.shapes[function_id.function_shape_index];
	let specialization = &shape.specializations[function_id.specialization_index];

	let mut maybe_id = None;
	let void = context.type_store.void_type_id();
	if !context.type_store.direct_match(specialization.return_type, void) {
		let return_type = specialization.return_type;
		let id = context.generate_temp_id();
		maybe_id = Some(id);
		if let Some(result) = generate_type_id(context, return_type, output) {
			result?;
			write!(output, " {id} = ")?;
		}
	}

	let shape = &context.function_store.shapes[function_id.function_shape_index];
	if let Some(extern_attribute) = shape.extern_attribute {
		match extern_attribute.item {
			crate::tree::ExternAttribute::Name(name) => write!(output, "{name}")?,

			crate::tree::ExternAttribute::Intrinsic => {
				generate_intrinsic(context, call, function_id, output)?;
				writeln!(output, ";")?;
				return Ok(maybe_id.map(|temp_id| Step::Temp { temp_id }));
			}
		}
	} else {
		generate_functon_id(function_id, output)?;
	}

	write!(output, "(")?;

	let mut first = true;
	for step in steps {
		if !first {
			write!(output, ", ")?;
		}
		first = false;
		write!(output, "{step}")?;
	}

	writeln!(output, ");")?;

	Ok(maybe_id.map(|temp_id| Step::Temp { temp_id }))
}

fn generate_intrinsic(context: &mut Context, call: &Call, function_id: FunctionId, output: Output) -> Result<()> {
	let shape = &mut context.function_store.shapes[function_id.function_shape_index];
	let specialization = &mut shape.specializations[function_id.specialization_index];

	match call.name {
		"size_of" => {
			assert_eq!(specialization.type_arguments.explicit_len(), 1);
			let type_id = specialization.type_arguments.explicit_ids()[0];
			let size = context.type_store.type_layout(type_id).size;
			write!(output, "{size}")
		}

		"alignment_of" => {
			assert_eq!(specialization.type_arguments.explicit_len(), 1);
			let type_id = specialization.type_arguments.explicit_ids()[0];
			let alignment = context.type_store.type_layout(type_id).alignment;
			write!(output, "{alignment}")
		}

		_ => unimplemented!(),
	}
}

fn generate_unary_operation(context: &mut Context, operation: &UnaryOperation, output: Output) -> Result<Option<Step>> {
	let Some(step) = generate_expression(context, &operation.expression, output)? else {
		return Ok(None);
	};

	let op = match operation.op {
		UnaryOperator::Negate => "-",
		UnaryOperator::Invert => "!",
		UnaryOperator::AddressOf | UnaryOperator::AddressOfMut => "&",
		UnaryOperator::Dereference => "*",
	};

	if let Some(result) = generate_type_id(context, operation.type_id, output) {
		result?;
	} else {
		return Ok(None);
	}

	let mut temp_id = context.generate_temp_id();
	if operation.op == UnaryOperator::Dereference {
		let Some((_, mutable)) = context.type_store.pointed_to(operation.expression.type_id) else {
			unreachable!("{:?}", context.type_store.type_entries[operation.expression.type_id.index()]);
		};

		if mutable {
			writeln!(output, " * const {temp_id} = {step};")?;
		} else {
			writeln!(output, " const * const {temp_id} = {step}; // not mutable")?;
		}
		temp_id.dereference = true;
	} else {
		writeln!(output, " const {temp_id} = {op}{step};")?;
	}

	Ok(Some(Step::Temp { temp_id }))
}

fn generate_binary_operation(context: &mut Context, operation: &BinaryOperation, output: Output) -> Result<Option<Step>> {
	if operation.op == BinaryOperator::Assign {
		if let ExpressionKind::Read(read) = &operation.left.kind {
			let Some(right_step) = generate_expression(context, &operation.right, output)? else {
				return Ok(None);
			};

			if context.type_store.type_layout(read.type_id).size <= 0 {
				return Ok(None);
			}

			generate_readable_index(read.readable_index, output)?;
			writeln!(output, " = {right_step};")?;
			return Ok(Some(Step::Readable { readable_index: read.readable_index }));
		}
	}

	let left_step = generate_expression(context, &operation.left, output)?;
	let right_step = generate_expression(context, &operation.right, output)?;

	let Some(left_step) = left_step else {
		return Ok(None);
	};
	let Some(right_step) = right_step else {
		return Ok(None);
	};

	if operation.op == BinaryOperator::Assign {
		writeln!(output, "{left_step} = {right_step};")?;
		return Ok(Some(left_step));
	}

	if let Some(result) = generate_type_id(context, operation.type_id, output) {
		result?;
	} else {
		return Ok(None);
	}
	let temp_id = context.generate_temp_id();
	write!(output, " {temp_id} = ({left_step}")?;

	let op = match operation.op {
		BinaryOperator::Assign => unreachable!(),

		BinaryOperator::Add => "+",
		BinaryOperator::Sub => "-",
		BinaryOperator::Mul => "*",
		BinaryOperator::Div => "/",

		BinaryOperator::Equals => "==",
		BinaryOperator::NotEquals => "!=",

		BinaryOperator::GreaterThan => ">",
		BinaryOperator::GreaterThanEquals => ">=",

		BinaryOperator::LessThan => "<",
		BinaryOperator::LessThanEquals => "<=",
	};
	write!(output, " {} ", op)?;

	writeln!(output, "{right_step});")?;

	Ok(Some(Step::Temp { temp_id }))
}

fn generate_expression(context: &mut Context, expression: &Expression, output: Output) -> std::io::Result<Option<Step>> {
	let mut temp_id = context.generate_temp_id();

	match &expression.kind {
		ExpressionKind::Block(block) => {
			generate_block(context, block, output)?;
			return Ok(None); // TODO: Fix when actual block expression
		}

		ExpressionKind::If(if_expression) => {
			let Some(condition_step) = generate_expression(context, &if_expression.condition, output)? else {
				return Ok(None);
			};

			writeln!(output, "if ({condition_step}) {{")?;
			generate_expression(context, &if_expression.body, output)?;
			writeln!(output, "}}")?;

			return Ok(None); // TODO: Fix when actual if expression
		}

		ExpressionKind::IntegerValue(value) => {
			return Ok(Some(Step::ConstantInteger(value.value())));
		}

		ExpressionKind::DecimalValue(value) => {
			return Ok(Some(Step::ConstantDecimal(value.value())));
		}

		ExpressionKind::BooleanLiteral(literal) => {
			return Ok(Some(Step::ConstantBool(*literal)));
		}

		ExpressionKind::CodepointLiteral(literal) => {
			return Ok(Some(Step::ConstantCodepoint(literal.value)));
		}

		ExpressionKind::StringLiteral(literal) => {
			let type_id = context.type_store.string_type_id();
			if let Some(result) = generate_type_id(context, type_id, output) {
				result?;
			} else {
				return Ok(None);
			}
			write!(output, " {temp_id} = ")?;
			generate_struct_construction_open(context, type_id, output)?;
			write!(output, ".fi_0 = (u8 const*){:?}, .fi_1 = {}", literal.value, literal.value.len())?;
			generate_struct_construction_close(output)?;
			writeln!(output, ";")?;
		}

		ExpressionKind::StructLiteral(literal) => {
			return generate_struct_literal(context, literal, output);
		}

		ExpressionKind::Call(call) => return generate_call(context, call, output),

		ExpressionKind::Read(read) => {
			if let Some(result) = generate_type_id(context, read.type_id, output) {
				result?;
			} else {
				return Ok(None);
			}

			if expression.mutable {
				write!(output, " *")?;
			} else {
				write!(output, " const *")?;
			}

			write!(output, " {temp_id} = &")?;
			generate_readable_index(read.readable_index, output)?;
			writeln!(output, ";")?;
			temp_id.dereference = true;
		}

		ExpressionKind::FieldRead(field_read) => {
			let Some(struct_step) = generate_expression(context, &field_read.base, output)? else {
				return Ok(None);
			};

			if let Some(result) = generate_type_id(context, field_read.type_id, output) {
				result?;
			} else {
				return Ok(None);
			}

			if expression.mutable {
				write!(output, " *")?;
			} else {
				write!(output, " const *")?;
			}

			writeln!(output, "{temp_id} = &{struct_step}.fi_{};", field_read.field_index)?;
			temp_id.dereference = true;
		}

		ExpressionKind::UnaryOperation(operation) => {
			return generate_unary_operation(context, operation, output);
		}

		ExpressionKind::BinaryOperation(operation) => {
			return generate_binary_operation(context, operation, output);
		}

		ExpressionKind::AnyCollapse => unreachable!(),
	}

	Ok(Some(Step::Temp { temp_id }))
}

fn generate_struct_construction_open(context: &mut Context, type_id: TypeId, output: Output) -> Result<()> {
	write!(output, "((")?;
	generate_type_id(context, type_id, output).unwrap()?;
	write!(output, ") {{ ")
}

fn generate_struct_construction_close(output: Output) -> Result<()> {
	write!(output, " }})")
}

fn generate_type_id(context: &mut Context, type_id: TypeId, output: Output) -> Option<Result<()>> {
	let mut generic_usages = Vec::new();
	let type_id = context.type_store.specialize_with_function_generics(
		context.messages,
		context.function_store,
		context.module_path,
		&mut generic_usages,
		context.function_id.function_shape_index,
		context.function_type_arguments,
		type_id,
	);
	assert_eq!(generic_usages.len(), 0);

	if context.type_store.type_layout(type_id).size <= 0 {
		return None;
	}

	Some(generate_raw_type_id(context.type_store, type_id, output))
}

fn generate_raw_type_id(type_store: &TypeStore, type_id: TypeId, output: Output) -> Result<()> {
	let entry = &type_store.type_entries[type_id.index()];
	match &entry.kind {
		TypeEntryKind::BuiltinType {
			kind: PrimativeKind::AnyCollapse | PrimativeKind::UntypedInteger | PrimativeKind::UntypedDecimal,
		} => unreachable!("cannot generate type id of kind: {:?}", entry.kind),

		TypeEntryKind::BuiltinType { kind } => write!(output, "{}", kind.name()),

		TypeEntryKind::UserType { shape_index, specialization_index } => {
			write!(output, "ty_{shape_index}_{specialization_index}")
		}

		TypeEntryKind::Pointer { type_id, mutable } => {
			generate_raw_type_id(type_store, *type_id, output)?;
			if *mutable {
				write!(output, " *")
			} else {
				write!(output, " const *")
			}
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
