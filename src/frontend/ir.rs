use std::borrow::Cow;

use rustc_hash::FxHashMap;

use crate::frontend::error::Messages;
use crate::frontend::function_store::FunctionStore;
use crate::frontend::span::Span;
use crate::frontend::tree::{BinaryOperator, ExportAttribute, ExternAttribute, IntrinsicAttribute, LangAttribute, Node};
use crate::frontend::type_store::*;
use crate::reference::{Ref, SliceRef};

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
	method_base_len: usize,
}

impl<'a> GenericParameters<'a> {
	pub fn new_from_explicit(explicit: Vec<GenericParameter<'a>>) -> Self {
		let explicit_len = explicit.len();
		GenericParameters {
			parameters: explicit,
			explicit_len,
			implicit_len: 0,
			method_base_len: 0,
		}
	}

	pub fn push_implicit(&mut self, implict: GenericParameter<'a>) {
		assert_eq!(self.method_base_len, 0);
		self.parameters.push(implict);
		self.implicit_len += 1;
	}

	pub fn push_method_base(&mut self, parameter: GenericParameter<'a>) {
		self.parameters.push(parameter);
		self.method_base_len += 1;
	}

	pub fn parameters(&self) -> &[GenericParameter<'a>] {
		&self.parameters
	}

	pub fn explicit_parameters(&self) -> &[GenericParameter<'a>] {
		&self.parameters[0..self.explicit_len]
	}

	pub fn implicit_parameters(&self) -> &[GenericParameter<'a>] {
		&self.parameters[self.explicit_len..self.explicit_len + self.implicit_len]
	}

	pub fn explicit_len(&self) -> usize {
		self.explicit_len
	}

	pub fn implicit_len(&self) -> usize {
		self.implicit_len
	}

	pub fn method_base_len(&self) -> usize {
		self.method_base_len
	}
}

#[derive(Debug, Clone)]
pub enum GenericUsage {
	UserType {
		type_arguments: Ref<TypeArguments>,
		shape_index: usize,
	},
	Function {
		type_arguments: Ref<TypeArguments>,
		function_shape_index: usize,
	},
}

impl GenericUsage {
	pub fn apply_specialization<'a>(
		&self,
		messages: &mut Messages<'a>,
		type_store: &mut TypeStore<'a>,
		function_store: &FunctionStore<'a>,
		module_path: &'a [String],
		generic_usages: &mut Vec<GenericUsage>,
		function_shape_index: usize,
		function_type_arguments: &TypeArguments,
		invoke_span: Option<Span>,
	) {
		match self {
			GenericUsage::UserType { type_arguments, shape_index } => {
				let mut specialized_type_arguments = TypeArguments::clone(type_arguments);
				for type_argument in &mut specialized_type_arguments.ids {
					*type_argument = type_store.specialize_with_function_generics(
						messages,
						function_store,
						module_path,
						generic_usages,
						function_shape_index,
						function_type_arguments,
						*type_argument,
					);
				}

				type_store.get_or_add_shape_specialization(
					messages,
					function_store,
					module_path,
					generic_usages,
					*shape_index,
					None,
					Ref::new(specialized_type_arguments),
				);
			}

			GenericUsage::Function {
				type_arguments,
				function_shape_index: usage_function_shape_index,
			} => {
				let mut type_arguments = TypeArguments::clone(&type_arguments);
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
					Ref::new(type_arguments),
					invoke_span,
				);
			}
		}
	}
}

#[derive(Debug)]
pub struct FunctionShape<'a> {
	pub name: Node<&'a str>,
	pub module_path: &'a [String],
	pub is_main: bool,

	pub extern_attribute: Option<&'a Node<ExternAttribute<'a>>>,
	pub export_attribute: Option<&'a Node<ExportAttribute<'a>>>,
	pub intrinsic_attribute: Option<&'a Node<IntrinsicAttribute>>,
	pub lang_attribute: Option<&'a Node<LangAttribute<'a>>>,

	pub method_base_index: Option<usize>,
	pub generic_parameters: GenericParameters<'a>,
	pub parameters: Vec<ParameterShape>,
	pub c_varargs: bool,
	pub return_type: TypeId,
	pub block: Option<Ref<Block<'a>>>,
	pub generic_usages: SliceRef<GenericUsage>,

	pub specializations_by_type_arguments: FxHashMap<Ref<TypeArguments>, usize>,
	pub specializations: Vec<Function>,
}

// Anonymous structs pls save me
pub struct FunctionSpecializationResult {
	pub specialization_index: usize,
	pub return_type: TypeId,
}

#[derive(Debug, Clone, Copy)]
pub struct ParameterShape {
	pub type_id: TypeId,
	pub readable_index: usize,
}

#[derive(Debug, Clone, Hash)]
pub struct TypeArguments {
	pub explicit_len: usize,
	pub implicit_len: usize,
	pub method_base_len: usize,
	pub ids: Vec<TypeId>,
}

impl std::cmp::Eq for TypeArguments {}

impl std::cmp::PartialEq for TypeArguments {
	fn eq(&self, other: &Self) -> bool {
		!self.ne(other)
	}

	fn ne(&self, other: &Self) -> bool {
		let len_mismatch = self.explicit_len != other.explicit_len
			|| self.implicit_len != other.implicit_len
			|| self.method_base_len != other.method_base_len;

		if len_mismatch {
			return true;
		}

		for (a, b) in self.ids.iter().copied().zip(other.ids.iter().copied()) {
			if a.index() != b.index() {
				return true;
			}
		}

		false
	}
}

impl TypeArguments {
	pub fn new_from_explicit(explicit: Vec<TypeId>) -> TypeArguments {
		let explicit_len = explicit.len();
		TypeArguments {
			ids: explicit,
			explicit_len,
			implicit_len: 0,
			method_base_len: 0,
		}
	}

	pub fn push_implicit(&mut self, implict: TypeId) {
		assert_eq!(self.method_base_len, 0);
		self.ids.push(implict);
		self.implicit_len += 1;
	}

	pub fn push_method_base(&mut self, type_id: TypeId) {
		self.ids.push(type_id);
		self.method_base_len += 1;
	}

	pub fn is_empty(&self) -> bool {
		self.ids.is_empty()
	}

	pub fn explicit_ids(&self) -> &[TypeId] {
		&self.ids[0..self.explicit_len]
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
			*original_id = type_store.specialize_with_function_generics(
				messages,
				function_store,
				module_path,
				generic_usages,
				function_shape_index,
				function_type_arguments,
				*original_id,
			);
		}
	}
}

#[derive(Debug, Clone)]
pub struct Function {
	pub type_arguments: Ref<TypeArguments>,
	pub generic_poisoned: bool,
	pub parameters: Vec<Parameter>,
	pub return_type: TypeId,
}

#[derive(Debug, Clone, Copy)]
pub struct Parameter {
	pub type_id: TypeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId {
	pub function_shape_index: usize,
	pub specialization_index: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId {
	pub file_index: usize,
	pub scope_index: usize,
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
	pub _type_id: TypeId,
	pub entries: Vec<IfElseChainEntry<'a>>,
	pub else_body: Option<Block<'a>>,
}

#[derive(Debug, Clone)]
pub struct Match<'a> {
	pub expression: Expression<'a>,
	pub arms: Vec<MatchArm<'a>>,
	pub else_arm: Option<Block<'a>>,
}

#[derive(Debug, Clone)]
pub struct MatchArm<'a> {
	pub binding: Option<CheckIsResultBinding>,
	pub block: Block<'a>,
	pub variant_infos: Vec<VariantInfo>,
}

#[derive(Debug, Clone)]
pub struct VariantInfo {
	pub type_id: TypeId,
	pub variant_index: usize,
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
	pub is_mutable: bool,
	pub returns: bool,
	pub kind: ExpressionKind<'a>,
}

impl<'a> Expression<'a> {
	pub fn any_collapse(type_store: &TypeStore<'a>, span: Span) -> Self {
		Expression {
			span,
			type_id: type_store.any_collapse_type_id(),
			is_mutable: true,
			returns: false, // TODO: This could cause erronious error messages?
			kind: ExpressionKind::AnyCollapse,
		}
	}

	pub fn void(type_store: &TypeStore<'a>, span: Span) -> Self {
		Expression {
			span,
			type_id: type_store.void_type_id(),
			is_mutable: true, // TODO: Think about this harder?
			returns: false,
			kind: ExpressionKind::Void,
		}
	}
}

#[derive(Debug, Clone)]
pub enum ExpressionKind<'a> {
	AnyCollapse,
	Void,
	Type(TypeId),

	Block(Block<'a>),
	IfElseChain(Box<IfElseChain<'a>>),
	Match(Box<Match<'a>>),

	IntegerValue(IntegerValue),
	DecimalValue(DecimalValue),

	BooleanLiteral(bool),
	CodepointLiteral(CodepointLiteral),
	ByteCodepointLiteral(ByteCodepointLiteral),
	StringLiteral(StringLiteral<'a>),

	ArrayLiteral(ArrayLiteral<'a>),
	StructLiteral(StructLiteral<'a>),

	Call(Call<'a>),
	MethodCall(Box<MethodCall<'a>>),
	Read(Read<'a>),
	StaticRead(StaticRead),
	FieldRead(Box<FieldRead<'a>>),

	UnaryOperation(Box<UnaryOperation<'a>>),
	BinaryOperation(Box<BinaryOperation<'a>>),
	CheckIs(Box<CheckIs<'a>>),

	EnumVariantToEnum(Box<EnumVariantToEnum<'a>>),
	SliceMutableToImmutable(Box<SliceMutableToImmutable<'a>>),
}

impl<'a> ExpressionKind<'a> {
	pub fn name(&self) -> &'static str {
		match self {
			ExpressionKind::AnyCollapse => "AnyCollapse",
			ExpressionKind::Void => "void value",
			ExpressionKind::Type(_) => "type",
			ExpressionKind::Block(_) => "block",
			ExpressionKind::IfElseChain(_) => "if expression",
			ExpressionKind::Match(_) => "match expression",
			ExpressionKind::IntegerValue(_) => "untyped integer",
			ExpressionKind::DecimalValue(_) => "untyped decimal",
			ExpressionKind::BooleanLiteral(_) => "boolean literal",
			ExpressionKind::CodepointLiteral(_) => "codepoint literal",
			ExpressionKind::ByteCodepointLiteral(_) => "byte codepoint literal",
			ExpressionKind::StringLiteral(_) => "string literal",
			ExpressionKind::ArrayLiteral(_) => "array literal",
			ExpressionKind::StructLiteral(_) => "struct literal",
			ExpressionKind::Call(_) => "function call",
			ExpressionKind::MethodCall(_) => "method call",
			ExpressionKind::Read(_) => "binding read",
			ExpressionKind::StaticRead(_) => "static read",
			ExpressionKind::FieldRead(_) => "field read",
			ExpressionKind::UnaryOperation(_) => "unary operation",
			ExpressionKind::BinaryOperation(_) => "binary operation",
			ExpressionKind::CheckIs(_) => "check is operation",
			ExpressionKind::EnumVariantToEnum(inner) => inner.expression.kind.name_with_article(),
			ExpressionKind::SliceMutableToImmutable(inner) => inner.expression.kind.name_with_article(),
		}
	}

	pub fn name_with_article(&self) -> &'static str {
		match self {
			ExpressionKind::AnyCollapse => "an AnyCollapse",
			ExpressionKind::Void => "a void value",
			ExpressionKind::Type(_) => "a type",
			ExpressionKind::Block(_) => "a block",
			ExpressionKind::IfElseChain(_) => "a if expression",
			ExpressionKind::Match(_) => "a match expression",
			ExpressionKind::IntegerValue(_) => "an untyped integer",
			ExpressionKind::DecimalValue(_) => "an untyped decimal",
			ExpressionKind::BooleanLiteral(_) => "a boolean literal",
			ExpressionKind::CodepointLiteral(_) => "a codepoint literal",
			ExpressionKind::ByteCodepointLiteral(_) => "a byte codepoint literal",
			ExpressionKind::StringLiteral(_) => "a string literal",
			ExpressionKind::ArrayLiteral(_) => "an array literal",
			ExpressionKind::StructLiteral(_) => "a struct literal",
			ExpressionKind::Call(_) => "a function call",
			ExpressionKind::MethodCall(_) => "a method call",
			ExpressionKind::Read(_) => "a binding read",
			ExpressionKind::StaticRead(_) => "a static read",
			ExpressionKind::FieldRead(_) => "a field read",
			ExpressionKind::UnaryOperation(_) => "an unary operation",
			ExpressionKind::BinaryOperation(_) => "a binary operation",
			ExpressionKind::CheckIs(_) => "a check is operation",
			ExpressionKind::EnumVariantToEnum(inner) => inner.expression.kind.name_with_article(),
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

	pub fn bitwise_and(self, other: IntegerValue) -> IntegerValue {
		let value = self.value & other.value;
		let span = self.span + other.span;
		IntegerValue { value, span, collapse: None }
	}

	pub fn bitwise_or(self, other: IntegerValue) -> IntegerValue {
		let value = self.value | other.value;
		let span = self.span + other.span;
		IntegerValue { value, span, collapse: None }
	}

	pub fn bitwise_xor(self, other: IntegerValue) -> IntegerValue {
		let value = self.value ^ other.value;
		let span = self.span + other.span;
		IntegerValue { value, span, collapse: None }
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
pub struct ByteCodepointLiteral {
	pub value: u8,
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
pub struct MethodCall<'a> {
	pub base: Expression<'a>,
	pub function_id: FunctionId,
	pub arguments: Vec<Expression<'a>>,
}

#[derive(Debug, Clone)]
pub struct Read<'a> {
	pub name: &'a str,
	pub readable_index: usize,
}

#[derive(Debug, Clone)]
pub struct StaticRead {
	pub static_index: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FieldReadImmutableReason {
	Readable,
	ReadOnly,
}

#[derive(Debug, Clone)]
pub struct FieldRead<'a> {
	pub base: Expression<'a>,
	pub name: &'a str,
	pub field_index: usize,
	pub immutable_reason: Option<FieldReadImmutableReason>,
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
pub struct CheckIs<'a> {
	pub left: Expression<'a>,
	pub binding: Option<CheckIsResultBinding>,
	pub variant_infos: Vec<VariantInfo>,
}

#[derive(Debug, Clone)]
pub struct CheckIsResultBinding {
	pub type_id: TypeId,
	pub readable_index: usize,
}

#[derive(Debug, Clone)]
pub struct EnumVariantToEnum<'a> {
	pub type_id: TypeId,
	pub expression: Expression<'a>,
}

#[derive(Debug, Clone)]
pub struct SliceMutableToImmutable<'a> {
	pub expression: Expression<'a>,
}
