use std::rc::Rc;

use crate::error::Messages;
use crate::span::Span;
use crate::tree::{BinaryOperator, Node};
use crate::type_store::*;

/*
 * The current structure of the IR utilizes nested `Box`-es and `Vec`-es which is rather inefficient
 * for both speed of construction and also speed of walking. The cases with `Vec` should use some
 * sort of small-vec and the cases with `Box` should probably use some sort of arena bump allocator.
 * The small-vec would be a relatively small change but would require writing a small vec which I'm not
 * going to bother with right now. Utilizing a bump allocator is a lot more work however, but will be
 * far easier to do once self-hosted if I design the language appropriately.
 *
 * It would be possible to emulate the advantages of an arena bump allocator without actually using one
 * by using several big `Vec`s and passing around indicies, but wrapping that up in a nice API is much
 * more effort than it is worth. See `ir_alloc_perf.rs` in the repo root for an example of this.
 */

#[derive(Debug, Clone)]
pub struct Import<'a> {
	pub segments: Vec<Node<&'a str>>,
}

#[derive(Debug, Copy, Clone)]
pub struct Symbol<'a> {
	pub name: &'a str,
	pub kind: SymbolKind,
	pub span: Option<Span>,
	pub file_index: Option<usize>,
}

#[derive(Debug, Clone, Copy)]
pub enum SymbolKind {
	BuiltinType { type_id: TypeId },
	Type { shape_index: usize },
	UserTypeGeneric { shape_index: usize, generic_index: usize },
	FunctionGeneric { function_shape_index: usize, generic_index: usize },
	Function { function_shape_index: usize },
	Const { constant_index: usize },
	Let { readable_index: usize },
	Mut { readable_index: usize },
}

impl std::fmt::Display for SymbolKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let name = match self {
			SymbolKind::BuiltinType { .. } => "a built in type",
			SymbolKind::Type { .. } => "a type",
			SymbolKind::UserTypeGeneric { .. } => "a type generic parameter",
			SymbolKind::FunctionGeneric { .. } => "a function generic parameter",
			SymbolKind::Function { .. } => "a function",
			SymbolKind::Const { .. } => "a constant",
			SymbolKind::Let { .. } => "an immutable binding",
			SymbolKind::Mut { .. } => "a mutable binding",
		};

		f.write_str(name)
	}
}

#[derive(Debug, Clone, Copy)]
pub struct Readable<'a> {
	pub name: &'a str,
	pub type_id: TypeId,
	pub kind: ReadableKind,
}

#[derive(Debug, Clone, Copy)]
pub enum ReadableKind {
	Let,
	Mut,
}

#[derive(Debug, Copy, Clone)]
pub struct GenericParameter<'a> {
	pub name: Node<&'a str>,
	pub generic_type_id: TypeId,
}

#[derive(Debug, Clone)]
pub struct GenericUsage {
	pub type_arguments: Vec<TypeId>,
	pub kind: GenericUsageKind,
}

#[derive(Debug, Copy, Clone)]
pub enum GenericUsageKind {
	UserType { shape_index: usize },
	Function { function_shape_index: usize },
}

#[derive(Debug, Clone)]
pub struct FunctionShape<'a> {
	pub name: Node<&'a str>,
	pub module_path: &'a [String],
	pub file_index: usize,
	pub is_main: bool,

	pub generics: Vec<GenericParameter<'a>>,
	pub parameters: Vec<ParameterShape<'a>>,
	pub return_type: TypeId,
	pub block: Option<Rc<Block<'a>>>,
	pub generic_usages: Vec<GenericUsage>,

	pub specializations: Vec<Function<'a>>,
}

// Anonymous structs pls save me
pub struct FunctionSpecializationResult {
	pub specialization_index: usize,
	pub return_type: TypeId,
}

impl<'a> FunctionShape<'a> {
	pub fn new(
		name: Node<&'a str>,
		module_path: &'a [String],
		file_index: usize,
		generics: Vec<GenericParameter<'a>>,
		parameters: Vec<ParameterShape<'a>>,
		return_type: TypeId,
	) -> Self {
		let is_main = module_path == &["main"] && name.item == "main";

		FunctionShape {
			name,
			module_path,
			file_index,
			is_main,
			generics,
			parameters,
			return_type,
			block: None,
			generic_usages: Vec::new(),
			specializations: Vec::new(),
		}
	}
}

#[derive(Debug, Clone)]
pub struct ParameterShape<'a> {
	pub name: Node<&'a str>,
	pub type_id: TypeId,
	pub is_mutable: bool,
	pub readable_index: usize,
}

#[derive(Debug, Clone)]
pub struct Function<'a> {
	pub type_arguments: Vec<TypeId>,
	pub parameters: Vec<Parameter<'a>>,
	pub return_type: TypeId,
	pub been_queued: bool,
	pub been_generated: bool,
}

#[derive(Debug, Clone, Copy)]
pub struct Parameter<'a> {
	pub name: Node<&'a str>,
	pub type_id: TypeId,
	pub readable_index: usize,
	pub is_mutable: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId {
	pub function_shape_index: usize,
	pub specialization_index: usize,
}

#[derive(Debug, Clone)]
pub struct Block<'a> {
	pub type_id: TypeId,
	pub statements: Vec<Statement<'a>>,
}

#[derive(Debug, Clone)]
pub struct Statement<'a> {
	pub type_id: TypeId,
	pub kind: StatementKind<'a>,
}

#[derive(Debug, Clone)]
pub enum StatementKind<'a> {
	Expression(Expression<'a>),

	Block(Block<'a>),

	Binding(Box<Binding<'a>>),

	Return(Box<Return<'a>>),
}

#[derive(Debug, Clone)]
pub struct Binding<'a> {
	pub name: &'a str,
	pub type_id: TypeId,
	pub expression: Expression<'a>,
	pub readable_index: usize,
	pub is_mutable: bool,
}

#[derive(Debug, Clone)]
pub struct Return<'a> {
	pub span: Span,
	pub expression: Option<Expression<'a>>,
}

#[derive(Debug, Clone)]
pub struct Expression<'a> {
	pub span: Span,
	pub type_id: TypeId,
	pub kind: ExpressionKind<'a>,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind<'a> {
	Block(Block<'a>),

	IntegerValue(IntegerValue),
	DecimalValue(DecimalValue),

	CodepointLiteral(CodepointLiteral),
	StringLiteral(StringLiteral<'a>),

	StructLiteral(StructLiteral<'a>),
	Call(Call<'a>),
	Read(Read<'a>),

	UnaryOperation(Box<UnaryOperation<'a>>),
	BinaryOperation(Box<BinaryOperation<'a>>),
}

impl<'a> ExpressionKind<'a> {
	pub fn name_with_article(&self) -> &'static str {
		match self {
			ExpressionKind::Block(_) => "a block",
			ExpressionKind::IntegerValue(_) => "an untyped integer",
			ExpressionKind::DecimalValue(_) => "an untyped decimal",
			ExpressionKind::CodepointLiteral(_) => "a codepoint literal",
			ExpressionKind::StringLiteral(_) => "a string literal",
			ExpressionKind::StructLiteral(_) => "a struct literal",
			ExpressionKind::Call(_) => "a function call",
			ExpressionKind::Read(_) => "a binding read",
			ExpressionKind::UnaryOperation(_) => "an unary operation",
			ExpressionKind::BinaryOperation(_) => "a binary operation",
		}
	}
}

#[derive(Debug, Copy, Clone)]
pub enum ConstantValue<'a> {
	IntegerValue(i128),
	DecimalValue(f64),
	CodepointLiteral(char),
	StringLiteral(&'a str),
}

#[derive(Debug, Copy, Clone)]
pub struct IntegerValue {
	value: i128,
	span: Span,
}

impl IntegerValue {
	pub fn new(value: i128, span: Span) -> IntegerValue {
		IntegerValue { value, span }
	}

	pub fn value(&self) -> i128 {
		self.value
	}

	pub fn span(&self) -> Span {
		self.span
	}

	pub fn negate(&mut self, messages: &mut Messages, sign_span: Span) {
		let Some(value) = self.value.checked_neg() else {
			let value = self.value;
			let err = message!("Constant integer {value} overflows compiler representation if inverted");
			messages.error(err.span(self.span));
			return;
		};

		self.value = value;
		self.span = self.span + sign_span;
	}

	pub fn add(self, messages: &mut Messages, other: IntegerValue) -> Option<IntegerValue> {
		let span = self.span + other.span;

		let Some(value) = self.value.checked_add(other.value) else {
			let err = message!("Overflow or underflow in constant addition");
			messages.error(err.span(span));
			return None;
		};

		Some(IntegerValue { value, span })
	}

	pub fn sub(self, messages: &mut Messages, other: IntegerValue) -> Option<IntegerValue> {
		let span = self.span + other.span;

		let Some(value) = self.value.checked_sub(other.value) else {
			let err = message!("Overflow or underflow in constant subtraction");
			messages.error(err.span(span));
			return None;
		};

		Some(IntegerValue { value, span })
	}

	pub fn mul(self, messages: &mut Messages, other: IntegerValue) -> Option<IntegerValue> {
		let span = self.span + other.span;

		let Some(value) = self.value.checked_mul(other.value) else {
			let err = message!("Overflow or underflow in constant multiplication");
			messages.error(err.span(span));
			return None;
		};

		Some(IntegerValue { value, span })
	}

	pub fn div(self, messages: &mut Messages, other: IntegerValue) -> Option<IntegerValue> {
		let span = self.span + other.span;

		let Some(value) = self.value.checked_div(other.value) else {
			let err = message!("Overflow or underflow in constant division");
			messages.error(err.span(span));
			return None;
		};

		Some(IntegerValue { value, span })
	}
}

#[derive(Debug, Copy, Clone)]
pub struct DecimalValue {
	value: f64,
	span: Span,
}

impl DecimalValue {
	pub fn new(value: f64, span: Span) -> DecimalValue {
		DecimalValue { value, span }
	}

	pub fn value(&self) -> f64 {
		self.value
	}

	pub fn span(&self) -> Span {
		self.span
	}

	pub fn negate(&mut self, sign_span: Span) {
		self.value = -self.value;
		self.span = self.span + sign_span;
	}

	pub fn add(self, other: DecimalValue) -> DecimalValue {
		let span = self.span + other.span;
		let value = self.value + other.value;
		DecimalValue { value, span }
	}

	pub fn sub(self, other: DecimalValue) -> DecimalValue {
		let span = self.span + other.span;
		let value = self.value - other.value;
		DecimalValue { value, span }
	}

	pub fn mul(self, other: DecimalValue) -> DecimalValue {
		let span = self.span + other.span;
		let value = self.value * other.value;
		DecimalValue { value, span }
	}

	pub fn div(self, other: DecimalValue) -> DecimalValue {
		let span = self.span + other.span;
		let value = self.value / other.value;
		DecimalValue { value, span }
	}
}

#[derive(Debug, Clone)]
pub struct CodepointLiteral {
	pub value: char,
}

#[derive(Debug, Clone)]
pub struct StringLiteral<'a> {
	pub value: &'a str,
}

#[derive(Debug, Clone)]
pub struct StructLiteral<'a> {
	pub type_id: TypeId,
	pub field_initializers: Vec<FieldInitializer<'a>>,
}

#[derive(Debug, Clone)]
pub struct FieldInitializer<'a> {
	pub expression: Expression<'a>,
}

#[derive(Debug, Clone)]
pub struct Call<'a> {
	pub name: &'a str,
	pub function_id: FunctionId,
	pub arguments: Vec<Expression<'a>>,
}

#[derive(Debug, Clone)]
pub struct Read<'a> {
	pub name: &'a str,
	pub type_id: TypeId,
	pub readable_index: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
	Negate,
}

#[derive(Debug, Clone)]
pub struct UnaryOperation<'a> {
	pub op: UnaryOperator,
	pub expression: Expression<'a>,
}

#[derive(Debug, Clone)]
pub struct BinaryOperation<'a> {
	pub op: BinaryOperator,
	pub left: Expression<'a>,
	pub right: Expression<'a>,
}
