use std::borrow::Cow;
use std::rc::Rc;

use crate::frontend::error::Messages;
use crate::frontend::function_store::FunctionStore;
use crate::frontend::span::Span;
use crate::frontend::tree::{BinaryOperator, ExportAttribute, ExternAttribute, IntrinsicAttribute, LangAttribute, Node};
use crate::frontend::type_store::*;

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

	pub fn explicit_parameters(&self) -> &[GenericParameter<'a>] {
		&self.parameters[0..self.explicit_len]
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

#[derive(Debug)]
pub struct FunctionShape<'a> {
	pub name: Node<&'a str>,
	pub module_path: &'a [String],
	pub file_index: usize,
	pub is_main: bool,

	pub extern_attribute: Option<Node<ExternAttribute<'a>>>,
	pub export_attribute: Option<Node<ExportAttribute<'a>>>,
	pub intrinsic_attribute: Option<Node<IntrinsicAttribute>>,
	pub lang_attribute: Option<Node<LangAttribute<'a>>>,

	pub generics: GenericParameters<'a>,
	pub parameters: Vec<ParameterShape<'a>>,
	pub c_varargs: bool,
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
		export_attribute: Option<Node<ExportAttribute<'a>>>,
		intrinsic_attribute: Option<Node<IntrinsicAttribute>>,
		lang_attribute: Option<Node<LangAttribute<'a>>>,
		parameters: Vec<ParameterShape<'a>>,
		c_varargs: bool,
		return_type: TypeId,
	) -> Self {
		FunctionShape {
			name,
			module_path,
			file_index,
			is_main,
			extern_attribute,
			export_attribute,
			intrinsic_attribute,
			lang_attribute,
			generics,
			parameters,
			c_varargs,
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
	pub generic_poisioned: bool,
	pub parameters: Vec<Parameter<'a>>,
	pub return_type: TypeId,
	pub been_queued: bool,
	pub been_generated: bool, // TODO: Remove
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
	pub returns: bool,
	pub statements: Vec<Statement<'a>>,
}

#[derive(Debug, Clone)]
pub struct IfElseChainEntry<'a> {
	pub condition: Expression<'a>,
	pub body: Block<'a>,
}

#[derive(Debug, Clone)]
pub struct IfElseChain<'a> {
	pub type_id: TypeId, // TODO: Meaningless until else-if/else have been added
	pub entries: Vec<IfElseChainEntry<'a>>,
	pub else_body: Option<Block<'a>>,
}

#[derive(Debug, Clone)]
pub struct While<'a> {
	pub condition: Expression<'a>,
	pub body: Block<'a>,
}

#[derive(Debug, Clone)]
pub struct Statement<'a> {
	pub kind: StatementKind<'a>,
}

#[derive(Debug, Clone)]
pub enum StatementKind<'a> {
	Expression(Expression<'a>),

	Block(Block<'a>),
	While(While<'a>),

	Binding(Box<Binding<'a>>),

	Break(Break),
	Continue(Continue),
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
pub struct Break {
	pub loop_index: usize,
}

#[derive(Debug, Clone)]
pub struct Continue {
	pub loop_index: usize,
}

#[derive(Debug, Clone)]
pub struct Return<'a> {
	pub expression: Option<Expression<'a>>,
}

#[derive(Debug, Clone)]
pub struct Expression<'a> {
	pub span: Span,
	pub type_id: TypeId,
	pub mutable: bool,
	pub returns: bool,
	pub kind: ExpressionKind<'a>,
}

impl<'a> Expression<'a> {
	pub fn any_collapse(type_store: &TypeStore<'a>, span: Span) -> Self {
		Expression {
			span,
			type_id: type_store.any_collapse_type_id(),
			mutable: true,
			returns: false, // TODO: This could cause erronious error messages?
			kind: ExpressionKind::AnyCollapse,
		}
	}

	pub fn void(type_store: &TypeStore<'a>, span: Span) -> Self {
		Expression {
			span,
			type_id: type_store.void_type_id(),
			mutable: true, // TODO: Think about this harder?
			returns: false,
			kind: ExpressionKind::Void,
		}
	}
}

#[derive(Debug, Clone)]
pub enum ExpressionKind<'a> {
	AnyCollapse,
	Void,

	Block(Block<'a>),
	IfElseChain(Box<IfElseChain<'a>>),

	IntegerValue(IntegerValue),
	DecimalValue(DecimalValue),

	BooleanLiteral(bool),
	CodepointLiteral(CodepointLiteral),
	StringLiteral(StringLiteral<'a>),

	ArrayLiteral(ArrayLiteral<'a>),
	StructLiteral(StructLiteral<'a>),

	Call(Call<'a>),
	Read(Read<'a>),
	StaticRead(StaticRead<'a>),
	FieldRead(Box<FieldRead<'a>>),

	UnaryOperation(Box<UnaryOperation<'a>>),
	BinaryOperation(Box<BinaryOperation<'a>>),

	SliceMutableToImmutable(Box<SliceMutableToImmutable<'a>>),
}

impl<'a> ExpressionKind<'a> {
	pub fn name(&self) -> &'static str {
		match self {
			ExpressionKind::AnyCollapse => "AnyCollapse",
			ExpressionKind::Void => "void value",
			ExpressionKind::Block(_) => "block",
			ExpressionKind::IfElseChain(_) => "if expression",
			ExpressionKind::IntegerValue(_) => "untyped integer",
			ExpressionKind::DecimalValue(_) => "untyped decimal",
			ExpressionKind::BooleanLiteral(_) => "boolean literal",
			ExpressionKind::CodepointLiteral(_) => "codepoint literal",
			ExpressionKind::StringLiteral(_) => "string literal",
			ExpressionKind::ArrayLiteral(_) => "array literal",
			ExpressionKind::StructLiteral(_) => "struct literal",
			ExpressionKind::Call(_) => "function call",
			ExpressionKind::Read(_) => "binding read",
			ExpressionKind::StaticRead(_) => "static read",
			ExpressionKind::FieldRead(_) => "field read",
			ExpressionKind::UnaryOperation(_) => "unary operation",
			ExpressionKind::BinaryOperation(_) => "binary operation",
			ExpressionKind::SliceMutableToImmutable(inner) => inner.expression.kind.name_with_article(),
		}
	}

	pub fn name_with_article(&self) -> &'static str {
		match self {
			ExpressionKind::AnyCollapse => "an AnyCollapse",
			ExpressionKind::Void => "a void value",
			ExpressionKind::Block(_) => "a block",
			ExpressionKind::IfElseChain(_) => "a if expression",
			ExpressionKind::IntegerValue(_) => "an untyped integer",
			ExpressionKind::DecimalValue(_) => "an untyped decimal",
			ExpressionKind::BooleanLiteral(_) => "a boolean literal",
			ExpressionKind::CodepointLiteral(_) => "a codepoint literal",
			ExpressionKind::StringLiteral(_) => "a string literal",
			ExpressionKind::ArrayLiteral(_) => "an array literal",
			ExpressionKind::StructLiteral(_) => "a struct literal",
			ExpressionKind::Call(_) => "a function call",
			ExpressionKind::Read(_) => "a binding read",
			ExpressionKind::StaticRead(_) => "a static read",
			ExpressionKind::FieldRead(_) => "a field read",
			ExpressionKind::UnaryOperation(_) => "an unary operation",
			ExpressionKind::BinaryOperation(_) => "a binary operation",
			ExpressionKind::SliceMutableToImmutable(inner) => inner.expression.kind.name_with_article(),
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

	pub fn new_collapsed(value: i128, span: Span, collapse: TypeId) -> IntegerValue {
		IntegerValue { value, span, collapse: Some(collapse) }
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

	pub fn collapsed(&self) -> TypeId {
		self.collapse.unwrap()
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
		self.span += sign_span;
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

	// TODO: These error messages should be thought out a bit better
	pub fn modulo(self, messages: &mut Messages, other: IntegerValue) -> Option<IntegerValue> {
		assert_not_collapsed(self.collapse);
		assert_not_collapsed(other.collapse);
		let span = self.span + other.span;

		let truncated_remainder = self.value % other.value;
		let value = if truncated_remainder < 0 {
			let Some(abs) = other.value.checked_abs() else {
				let err = error!("Absolute value failure in constant modulo");
				messages.message(err.span(span));
				return None;
			};

			let Some(added) = truncated_remainder.checked_add(abs) else {
				let err = error!("Addition failure in constant modulo");
				messages.message(err.span(span));
				return None;
			};

			added
		} else {
			truncated_remainder
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

	pub fn collapsed(&self) -> TypeId {
		self.collapse.unwrap()
	}

	pub fn negate(&mut self, sign_span: Span) {
		assert_not_collapsed(self.collapse);

		self.value = -self.value;
		self.span += sign_span;
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
pub struct ArrayLiteral<'a> {
	pub type_id: TypeId,
	pub pointee_type_id: TypeId,
	pub expressions: Vec<Expression<'a>>,
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
	pub span: Span,
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
pub struct StaticRead<'a> {
	pub name: &'a str,
	pub type_id: TypeId,
	pub static_index: usize,
}

#[derive(Debug, Clone)]
pub struct FieldRead<'a> {
	pub base: Expression<'a>,
	pub name: &'a str,
	pub type_id: TypeId,
	pub field_index: usize,
}

#[derive(Debug, Clone)]
pub enum UnaryOperator<'a> {
	Negate,
	Invert,
	AddressOf,
	AddressOfMut,
	Dereference,
	Cast { type_id: TypeId },
	Index { index_expression: Expression<'a> },
}

#[derive(Debug, Clone)]
pub struct UnaryOperation<'a> {
	pub op: UnaryOperator<'a>,
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

#[derive(Debug, Clone)]
pub struct SliceMutableToImmutable<'a> {
	pub type_id: TypeId,
	pub expression: Expression<'a>,
}
