use std::borrow::Cow;
use std::rc::Rc;

use crate::error::Messages;
use crate::span::Span;
use crate::tree::{BinaryOperator, ExternAttribute, Node};
use crate::type_store::*;
use crate::validator::FunctionStore;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
pub struct GenericParameters<'a> {
	parameters: Vec<GenericParameter<'a>>,
	explicit_len: usize,
	implicit_len: usize,
}

impl<'a> GenericParameters<'a> {
	pub fn new_from_explicit(explicit: Vec<GenericParameter<'a>>) -> Self {
		let explicit_len = explicit.len();
		GenericParameters { parameters: explicit, explicit_len, implicit_len: 0 }
	}

	pub fn push_implicit(&mut self, implict: GenericParameter<'a>) {
		self.parameters.push(implict);
		self.implicit_len += 1;
	}

	pub fn parameters(&self) -> &[GenericParameter<'a>] {
		&self.parameters
	}

	pub fn explicit_len(&self) -> usize {
		self.explicit_len
	}

	pub fn implicit_len(&self) -> usize {
		self.implicit_len
	}
}

#[derive(Debug, Clone)]
pub enum GenericUsage {
	UserType { type_arguments: Vec<TypeId>, shape_index: usize },
	Function { type_arguments: TypeArguments, function_shape_index: usize },
}

impl GenericUsage {
	pub fn apply_specialization<'a>(
		&self,
		messages: &mut Messages<'a>,
		type_store: &mut TypeStore<'a>,
		function_store: &mut FunctionStore<'a>,
		module_path: &'a [String],
		generic_usages: &mut Vec<GenericUsage>,
		function_shape_index: usize,
		function_type_arguments: &TypeArguments,
		invoke_span: Option<Span>,
	) {
		match self {
			GenericUsage::UserType { type_arguments, shape_index } => {
				let mut specialized_type_arguments = Vec::with_capacity(type_arguments.len());
				for &type_argument in type_arguments {
					let type_id = type_store.specialize_with_function_generics(
						messages,
						function_store,
						module_path,
						generic_usages,
						function_shape_index,
						function_type_arguments,
						type_argument,
					);
					specialized_type_arguments.push(type_id);
				}

				type_store.get_or_add_shape_specialization(
					messages,
					function_store,
					module_path,
					generic_usages,
					*shape_index,
					None,
					specialized_type_arguments,
				);
			}

			GenericUsage::Function {
				type_arguments,
				function_shape_index: usage_function_shape_index,
			} => {
				let mut type_arguments = type_arguments.clone();
				type_arguments.specialize_with_function_generics(
					messages,
					type_store,
					function_store,
					module_path,
					generic_usages,
					function_shape_index,
					function_type_arguments,
				);

				function_store.get_or_add_specialization(
					messages,
					type_store,
					module_path,
					generic_usages,
					*usage_function_shape_index,
					invoke_span,
					type_arguments,
				);
			}
		}
	}
}

#[derive(Debug, Clone)]
pub struct FunctionShape<'a> {
	pub name: Node<&'a str>,
	pub module_path: &'a [String],
	pub file_index: usize,
	pub is_main: bool,

	pub extern_attribute: Option<Node<ExternAttribute<'a>>>,

	pub generics: GenericParameters<'a>,
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
		is_main: bool,
		generics: GenericParameters<'a>,
		extern_attribute: Option<Node<ExternAttribute<'a>>>,
		parameters: Vec<ParameterShape<'a>>,
		return_type: TypeId,
	) -> Self {
		FunctionShape {
			name,
			module_path,
			file_index,
			is_main,
			extern_attribute,
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
pub struct TypeArguments {
	ids: Vec<TypeId>,
	explicit_len: usize,
	implicit_len: usize,
}

impl TypeArguments {
	pub fn new_from_explicit(explicit: Vec<TypeId>) -> TypeArguments {
		let explicit_len = explicit.len();
		TypeArguments { ids: explicit, explicit_len, implicit_len: 0 }
	}

	pub fn push_implicit(&mut self, implict: TypeId) {
		self.ids.push(implict);
		self.implicit_len += 1;
	}

	pub fn is_empty(&self) -> bool {
		self.ids.is_empty()
	}

	pub fn explicit_len(&self) -> usize {
		self.explicit_len
	}

	pub fn implicit_len(&self) -> usize {
		self.implicit_len
	}

	pub fn ids(&self) -> &[TypeId] {
		&self.ids
	}

	pub fn explicit_ids(&self) -> &[TypeId] {
		&self.ids[0..self.explicit_len]
	}

	pub fn matches(&self, other: &TypeArguments, type_store: &TypeStore) -> bool {
		if self.implicit_len != other.implicit_len || self.explicit_len != other.explicit_len {
			return false;
		}

		for (index, &implicit) in self.ids[..self.implicit_len].iter().enumerate() {
			if !type_store.direct_match(implicit, other.ids[index]) {
				return false;
			}
		}

		for (index, &explicit) in self.ids[self.implicit_len..].iter().enumerate() {
			if !type_store.direct_match(explicit, other.ids[index]) {
				return false;
			}
		}

		true
	}

	pub fn specialize_with_function_generics<'a>(
		&mut self,
		messages: &mut Messages<'a>,
		type_store: &mut TypeStore<'a>,
		function_store: &FunctionStore<'a>,
		module_path: &'a [String],
		generic_usages: &mut Vec<GenericUsage>,
		function_shape_index: usize,
		function_type_arguments: &TypeArguments,
	) {
		for original_id in &mut self.ids {
			let new_id = type_store.specialize_with_function_generics(
				messages,
				function_store,
				module_path,
				generic_usages,
				function_shape_index,
				function_type_arguments,
				*original_id,
			);

			*original_id = new_id;
		}
	}
}

#[derive(Debug, Clone)]
pub struct Function<'a> {
	pub type_arguments: TypeArguments,
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
pub struct If<'a> {
	pub type_id: TypeId, // TODO: Meaningless until else-if/else have been added
	pub condition: Expression<'a>,
	pub body: Expression<'a>,
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
	pub mutable: bool,
	pub kind: ExpressionKind<'a>,
}

impl<'a> Expression<'a> {
	pub fn any_collapse(type_store: &TypeStore<'a>, span: Span) -> Self {
		Expression {
			span,
			type_id: type_store.any_collapse_type_id(),
			mutable: true, // TODO: Think about this harder?
			kind: ExpressionKind::AnyCollapse,
		}
	}
}

#[derive(Debug, Clone)]
pub enum ExpressionKind<'a> {
	AnyCollapse,

	Block(Block<'a>),
	If(Box<If<'a>>),

	IntegerValue(IntegerValue),
	DecimalValue(DecimalValue),

	BooleanLiteral(bool),
	CodepointLiteral(CodepointLiteral),
	StringLiteral(StringLiteral<'a>),

	StructLiteral(StructLiteral<'a>),

	Call(Call<'a>),
	Read(Read<'a>),
	FieldRead(Box<FieldRead<'a>>),

	UnaryOperation(Box<UnaryOperation<'a>>),
	BinaryOperation(Box<BinaryOperation<'a>>),
}

impl<'a> ExpressionKind<'a> {
	pub fn name_with_article(&self) -> &'static str {
		match self {
			ExpressionKind::AnyCollapse => "an AnyCollapse",
			ExpressionKind::Block(_) => "a block",
			ExpressionKind::If(_) => "a if expression",
			ExpressionKind::IntegerValue(_) => "an untyped integer",
			ExpressionKind::DecimalValue(_) => "an untyped decimal",
			ExpressionKind::BooleanLiteral(_) => "a boolean literal",
			ExpressionKind::CodepointLiteral(_) => "a codepoint literal",
			ExpressionKind::StringLiteral(_) => "a string literal",
			ExpressionKind::StructLiteral(_) => "a struct literal",
			ExpressionKind::Call(_) => "a function call",
			ExpressionKind::Read(_) => "a binding read",
			ExpressionKind::FieldRead(_) => "a field read",
			ExpressionKind::UnaryOperation(_) => "an unary operation",
			ExpressionKind::BinaryOperation(_) => "a binary operation",
		}
	}
}

#[derive(Debug, Clone)]
pub enum ConstantValue<'a> {
	IntegerValue(i128),
	DecimalValue(f64),
	CodepointLiteral(char),
	StringLiteral(Cow<'a, str>),
}

#[track_caller]
fn assert_not_collapsed(collapse: Option<TypeId>) {
	if collapse.is_some() {
		panic!("Assertion collapse is none failed: {collapse:?}");
	}
}

#[derive(Debug, Copy, Clone)]
pub struct IntegerValue {
	value: i128,
	span: Span,
	collapse: Option<TypeId>,
}

impl IntegerValue {
	pub fn new(value: i128, span: Span) -> IntegerValue {
		IntegerValue { value, span, collapse: None }
	}

	pub fn value(&self) -> i128 {
		self.value
	}

	pub fn span(&self) -> Span {
		self.span
	}

	pub fn collapse(&mut self, type_id: TypeId) {
		assert_not_collapsed(self.collapse);
		self.collapse = Some(type_id);
	}

	pub fn negate(&mut self, messages: &mut Messages, sign_span: Span) {
		assert_not_collapsed(self.collapse);

		let Some(value) = self.value.checked_neg() else {
			let value = self.value;
			let err = error!("Constant integer {value} overflows compiler representation if inverted");
			messages.message(err.span(self.span));
			return;
		};

		self.value = value;
		self.span = self.span + sign_span;
	}

	pub fn add(self, messages: &mut Messages, other: IntegerValue) -> Option<IntegerValue> {
		assert_not_collapsed(self.collapse);
		assert_not_collapsed(other.collapse);

		let span = self.span + other.span;

		let Some(value) = self.value.checked_add(other.value) else {
			let err = error!("Overflow or underflow in constant addition");
			messages.message(err.span(span));
			return None;
		};

		Some(IntegerValue { value, span, collapse: None })
	}

	pub fn sub(self, messages: &mut Messages, other: IntegerValue) -> Option<IntegerValue> {
		assert_not_collapsed(self.collapse);
		assert_not_collapsed(other.collapse);

		let span = self.span + other.span;

		let Some(value) = self.value.checked_sub(other.value) else {
			let err = error!("Overflow or underflow in constant subtraction");
			messages.message(err.span(span));
			return None;
		};

		Some(IntegerValue { value, span, collapse: None })
	}

	pub fn mul(self, messages: &mut Messages, other: IntegerValue) -> Option<IntegerValue> {
		assert_not_collapsed(self.collapse);
		assert_not_collapsed(other.collapse);

		let span = self.span + other.span;

		let Some(value) = self.value.checked_mul(other.value) else {
			let err = error!("Overflow or underflow in constant multiplication");
			messages.message(err.span(span));
			return None;
		};

		Some(IntegerValue { value, span, collapse: None })
	}

	pub fn div(self, messages: &mut Messages, other: IntegerValue) -> Option<IntegerValue> {
		assert_not_collapsed(self.collapse);
		assert_not_collapsed(other.collapse);

		let span = self.span + other.span;

		let Some(value) = self.value.checked_div(other.value) else {
			let err = error!("Overflow or underflow in constant division");
			messages.message(err.span(span));
			return None;
		};

		Some(IntegerValue { value, span, collapse: None })
	}
}

#[derive(Debug, Copy, Clone)]
pub struct DecimalValue {
	value: f64,
	span: Span,
	collapse: Option<TypeId>,
}

impl DecimalValue {
	pub fn new(value: f64, span: Span) -> DecimalValue {
		DecimalValue { value, span, collapse: None }
	}

	pub fn value(&self) -> f64 {
		self.value
	}

	pub fn span(&self) -> Span {
		self.span
	}

	pub fn collapse(&mut self, type_id: TypeId) {
		assert_not_collapsed(self.collapse);
		self.collapse = Some(type_id);
	}

	pub fn negate(&mut self, sign_span: Span) {
		assert_not_collapsed(self.collapse);

		self.value = -self.value;
		self.span = self.span + sign_span;
	}

	pub fn add(self, other: DecimalValue) -> DecimalValue {
		assert_not_collapsed(self.collapse);
		assert_not_collapsed(other.collapse);

		let span = self.span + other.span;
		let value = self.value + other.value;
		DecimalValue { value, span, collapse: None }
	}

	pub fn sub(self, other: DecimalValue) -> DecimalValue {
		assert_not_collapsed(self.collapse);
		assert_not_collapsed(other.collapse);

		let span = self.span + other.span;
		let value = self.value - other.value;
		DecimalValue { value, span, collapse: None }
	}

	pub fn mul(self, other: DecimalValue) -> DecimalValue {
		assert_not_collapsed(self.collapse);
		assert_not_collapsed(other.collapse);

		let span = self.span + other.span;
		let value = self.value * other.value;
		DecimalValue { value, span, collapse: None }
	}

	pub fn div(self, other: DecimalValue) -> DecimalValue {
		assert_not_collapsed(self.collapse);
		assert_not_collapsed(other.collapse);

		let span = self.span + other.span;
		let value = self.value / other.value;
		DecimalValue { value, span, collapse: None }
	}
}

#[derive(Debug, Clone)]
pub struct CodepointLiteral {
	pub value: char,
}

#[derive(Debug, Clone)]
pub struct StringLiteral<'a> {
	pub value: Cow<'a, str>,
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

#[derive(Debug, Clone)]
pub struct FieldRead<'a> {
	pub base: Expression<'a>,
	pub name: &'a str,
	pub type_id: TypeId,
	pub field_index: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
	Negate,
	Invert,
	AddressOf,
	AddressOfMut,
	Dereference,
}

#[derive(Debug, Clone)]
pub struct UnaryOperation<'a> {
	pub op: UnaryOperator,
	pub type_id: TypeId,
	pub expression: Expression<'a>,
}

#[derive(Debug, Clone)]
pub struct BinaryOperation<'a> {
	pub op: BinaryOperator,
	pub left: Expression<'a>,
	pub right: Expression<'a>,
	pub type_id: TypeId,
}
