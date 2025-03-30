use std::borrow::Cow;
use std::cmp::Ordering;

use rust_decimal::prelude::ToPrimitive;
use rust_decimal::Decimal;
use rustc_hash::FxHashMap;

use crate::frontend::error::Messages;
use crate::frontend::function_store::FunctionStore;
use crate::frontend::root_layers::RootLayer;
use crate::frontend::span::{DebugLocation, Span};
use crate::frontend::tree::{self, BinaryOperator, ExportAttribute, ExternAttribute, IntrinsicAttribute, LangAttribute, Node};
use crate::frontend::type_store::*;
use crate::frontend::validator::Context;
use crate::lock::RwLock;
use crate::reference::{Ref, SliceRef};

#[derive(Debug, Clone)]
pub struct GenericConstraint {
	pub trait_shape_index: usize,
	pub type_arguments: Ref<TypeArguments>,
}

#[derive(Debug, Clone)]
pub struct GenericParameter<'a> {
	pub name: Node<&'a str>,
	pub constraint_trait_ids: SliceRef<TraitId>,
	pub generic_constraints: SliceRef<GenericConstraint>,
	pub generic_type_id: TypeId,
}

#[derive(Debug, Clone)]
pub struct GenericParameters<'a> {
	parameters: Vec<GenericParameter<'a>>, // TODO: This should be a SliceRef
	explicit_len: usize,
	implicit_len: usize,
	method_base_len: usize,
	has_trait_self: bool,
}

impl<'a> GenericParameters<'a> {
	pub fn new_from_explicit(explicit: Vec<GenericParameter<'a>>) -> Self {
		let explicit_len = explicit.len();
		GenericParameters {
			parameters: explicit,
			explicit_len,
			implicit_len: 0,
			method_base_len: 0,
			has_trait_self: false,
		}
	}

	pub fn push_implicit(&mut self, implict: GenericParameter<'a>) {
		assert_eq!(self.method_base_len, 0);
		assert_eq!(self.has_trait_self, false);
		self.parameters.push(implict);
		self.implicit_len += 1;
	}

	pub fn push_method_base(&mut self, parameter: GenericParameter<'a>) {
		assert_eq!(self.has_trait_self, false);
		self.parameters.push(parameter);
		self.method_base_len += 1;
	}

	pub fn push_trait_self(&mut self, parameter: GenericParameter<'a>) {
		assert_eq!(self.has_trait_self, false);
		self.parameters.push(parameter);
		self.has_trait_self = true;
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

	Trait {
		type_arguments: Ref<TypeArguments>,
		trait_shape_index: usize,
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
		enclosing_generic_parameters: &GenericParameters<'a>,
		function_shape_index: usize,
		function_type_arguments: &TypeArguments,
		invoke_span: Option<Span>,
	) {
		match self {
			GenericUsage::UserType { type_arguments, shape_index } => {
				let mut type_arguments = TypeArguments::clone(&type_arguments);
				type_arguments.specialize_with_generics(
					messages,
					type_store,
					function_store,
					module_path,
					generic_usages,
					enclosing_generic_parameters,
					TypeIdSpecializationSituation::Function { function_shape_index },
					function_type_arguments,
				);

				type_store.get_or_add_shape_specialization(
					messages,
					function_store,
					module_path,
					generic_usages,
					enclosing_generic_parameters,
					*shape_index,
					invoke_span,
					Ref::new(type_arguments),
				);
			}

			GenericUsage::Function {
				type_arguments,
				function_shape_index: usage_function_shape_index,
			} => {
				let mut type_arguments = TypeArguments::clone(&type_arguments);
				type_arguments.specialize_with_generics(
					messages,
					type_store,
					function_store,
					module_path,
					generic_usages,
					enclosing_generic_parameters,
					TypeIdSpecializationSituation::Function { function_shape_index },
					function_type_arguments,
				);

				function_store.get_or_add_specialization(
					messages,
					type_store,
					module_path,
					generic_usages,
					enclosing_generic_parameters,
					*usage_function_shape_index,
					invoke_span,
					Ref::new(type_arguments),
				);
			}

			GenericUsage::Trait { type_arguments, trait_shape_index } => {
				let mut type_arguments = TypeArguments::clone(&type_arguments);
				type_arguments.specialize_with_generics(
					messages,
					type_store,
					function_store,
					module_path,
					generic_usages,
					enclosing_generic_parameters,
					TypeIdSpecializationSituation::Function { function_shape_index },
					function_type_arguments,
				);

				type_store.get_or_add_trait_shape_specialization(
					messages,
					function_store,
					module_path,
					generic_usages,
					enclosing_generic_parameters,
					*trait_shape_index,
					invoke_span,
					Ref::new(type_arguments),
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

	pub trait_method_marker: Option<TraitMethodMarker>,

	pub extern_attribute: Option<&'a Node<ExternAttribute<'a>>>,
	pub export_attribute: Option<&'a Node<ExportAttribute<'a>>>,
	pub intrinsic_attribute: Option<&'a Node<IntrinsicAttribute>>,
	pub lang_attribute: Option<&'a Node<LangAttribute<'a>>>,

	pub method_base_index: Option<usize>,
	pub generic_parameters: GenericParameters<'a>,
	pub parameters: Node<Vec<ParameterShape<'a>>>,
	pub c_varargs: Option<Span>,
	pub return_type: Node<TypeId>,
	pub block: Option<Ref<Block<'a>>>,
	pub generic_usages: SliceRef<GenericUsage>,

	pub specializations_by_type_arguments: FxHashMap<Ref<TypeArguments>, usize>,
	pub specializations: Vec<Function>,
}

#[derive(Debug, Clone, Copy)]
pub struct TraitMethodMarker {
	pub trait_shape_index: usize,
	pub trait_method_index: usize,
}

// Anonymous structs pls save me
pub struct FunctionSpecializationResult {
	pub specialization_index: usize,
	pub return_type: TypeId,
}

#[derive(Debug, Clone, Copy)]
pub struct ParameterShape<'a> {
	pub span: Span,
	pub label: Option<&'a str>,
	pub type_id: TypeId,
	pub readable_index: usize,
	pub is_mutable: bool,
}

#[derive(Debug, Clone, Hash)]
pub struct TypeArguments {
	pub explicit_len: usize,
	pub implicit_len: usize,
	pub method_base_len: usize,
	pub ids: Vec<Node<TypeId>>, // TODO: Should this be a SliceRef?
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
			if a.item.index() != b.item.index() {
				return true;
			}
		}

		false
	}
}

impl TypeArguments {
	pub fn new_from_explicit(explicit: Vec<Node<TypeId>>) -> TypeArguments {
		let explicit_len = explicit.len();
		TypeArguments {
			ids: explicit,
			explicit_len,
			implicit_len: 0,
			method_base_len: 0,
		}
	}

	pub fn push_implicit(&mut self, implict: Node<TypeId>) {
		assert_eq!(self.method_base_len, 0);
		self.ids.push(implict);
		self.implicit_len += 1;
	}

	pub fn push_method_base(&mut self, type_id: Node<TypeId>) {
		self.ids.push(type_id);
		self.method_base_len += 1;
	}

	pub fn is_empty(&self) -> bool {
		self.ids.is_empty()
	}

	pub fn explicit_ids(&self) -> &[Node<TypeId>] {
		&self.ids[0..self.explicit_len]
	}

	pub fn specialize_with_generics<'a>(
		&mut self,
		messages: &mut Messages<'a>,
		type_store: &mut TypeStore<'a>,
		function_store: &FunctionStore<'a>,
		module_path: &'a [String],
		generic_usages: &mut Vec<GenericUsage>,
		enclosing_generic_parameters: &GenericParameters<'a>,
		situation: TypeIdSpecializationSituation,
		type_arguments: &TypeArguments,
	) {
		for original_id in &mut self.ids {
			original_id.item = type_store.specialize_type_id_with_generics(
				messages,
				function_store,
				module_path,
				generic_usages,
				enclosing_generic_parameters,
				original_id.item,
				type_arguments,
				situation,
			);
		}
	}
}

#[derive(Debug, Clone)]
pub struct Function {
	pub type_arguments: Ref<TypeArguments>,
	pub generic_poisoned: bool,
	pub parameters: SliceRef<Parameter>,
	pub return_type: TypeId,
}

#[derive(Debug, Clone, Copy)]
pub struct Parameter {
	pub type_id: TypeId,
	pub is_mutable: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId {
	pub function_shape_index: usize,
	pub specialization_index: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId {
	pub file_index: u32,
	pub scope_index: usize,
}

#[derive(Debug)]
pub struct Block<'a> {
	pub type_id: TypeId,
	pub yields: bool,
	pub yield_target_index: Option<usize>,
	pub returns: bool,
	pub statements: Vec<Statement<'a>>,
}

#[derive(Debug)]
pub struct IfElseChainEntry<'a> {
	pub condition: Expression<'a>,
	pub body: Block<'a>,
}

#[derive(Debug)]
pub struct IfElseChain<'a> {
	pub type_id: TypeId,
	pub yield_target_index: Option<usize>,
	pub entries: Vec<IfElseChainEntry<'a>>,
	pub else_body: Option<Block<'a>>,
}

#[derive(Debug)]
pub struct Match<'a> {
	pub type_id: TypeId,
	pub yield_target_index: Option<usize>,
	pub expression: Expression<'a>,
	pub arms: Vec<MatchArm<'a>>,
	pub else_arm: Option<Block<'a>>,
}

#[derive(Debug)]
pub struct MatchArm<'a> {
	pub binding: Option<ResultBinding>,
	pub block: Block<'a>,
	pub variant_infos: Vec<VariantInfo>,
}

#[derive(Debug, Clone)]
pub struct VariantInfo {
	pub type_id: TypeId,
	pub variant_index: usize,
}

#[derive(Debug)]
pub struct While<'a> {
	pub condition: Expression<'a>,
	pub body: Block<'a>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ForKind {
	InSlice,
	OfSlice,
	Range,
	AnyCollapse,
}

#[derive(Debug)]
pub struct For<'a> {
	pub kind: ForKind,
	pub item: ResultBinding,
	pub index: Option<ResultBinding>,
	pub is_last: Option<ResultBinding>,
	pub initializer: Expression<'a>,
	pub body: Block<'a>,
}

#[derive(Debug)]
pub struct Statement<'a> {
	pub kind: StatementKind<'a>,
	pub debug_location: DebugLocation,
}

#[derive(Debug)]
pub enum StatementKind<'a> {
	Expression(Expression<'a>),

	When(Block<'a>),
	Block(Block<'a>),
	While(While<'a>),
	For(For<'a>),

	Binding(Binding<'a>),

	Defer(Box<Defer<'a>>),

	Break(Break),
	Continue(Continue),
	Yield(Yield<'a>),
	Return(Return<'a>),
}

#[derive(Debug)]
pub struct Binding<'a> {
	pub name: &'a str,
	pub type_id: TypeId,
	pub expression: Expression<'a>,
	pub readable_index: usize,
}

#[derive(Debug)]
pub struct Defer<'a> {
	pub statement: Statement<'a>,
}

#[derive(Debug)]
pub struct Break {
	pub loop_index: usize,
}

#[derive(Debug)]
pub struct Continue {
	pub loop_index: usize,
}

#[derive(Debug)]
pub struct Yield<'a> {
	pub yield_target_index: usize,
	pub expression: Expression<'a>,
}

#[derive(Debug)]
pub struct Return<'a> {
	pub expression: Option<Expression<'a>>,
}

#[derive(Debug)]
pub struct Expression<'a> {
	pub span: Span,
	pub type_id: TypeId,
	pub is_itself_mutable: bool,
	pub is_pointer_access_mutable: bool,
	pub yields: bool,
	pub returns: bool,
	pub kind: ExpressionKind<'a>,
	pub debug_location: DebugLocation,
}

impl<'a> Expression<'a> {
	pub fn any_collapse(type_store: &TypeStore<'a>, span: Span) -> Self {
		Expression {
			span,
			type_id: type_store.any_collapse_type_id(),
			is_itself_mutable: true,
			is_pointer_access_mutable: true,
			yields: true,
			returns: true,
			kind: ExpressionKind::AnyCollapse,
			debug_location: DebugLocation::unusable(),
		}
	}

	pub fn void(type_store: &TypeStore<'a>, parsed_files: &[tree::File], span: Span) -> Self {
		Expression {
			span,
			type_id: type_store.void_type_id(),
			is_itself_mutable: true,
			is_pointer_access_mutable: true,
			yields: false,
			returns: false,
			kind: ExpressionKind::Void,
			debug_location: span.debug_location(parsed_files),
		}
	}

	// Returns true if it *turned into* a value
	pub fn into_value(&mut self, context: &mut Context<'a, '_, '_>) -> bool {
		let ExpressionKind::Type { type_id } = self.kind else {
			return false;
		};

		let entry = context.type_store.type_entries.get(type_id);
		let TypeEntryKind::UserType { shape_index, specialization_index, .. } = entry.kind else {
			return false;
		};

		let user_type = context.type_store.user_types.read()[shape_index].clone();
		let user_type = user_type.read();
		let UserTypeKind::Struct { shape } = &user_type.kind else {
			return false;
		};

		if shape.parent_kind == StructParentKind::None {
			return false; // Not a variant
		}

		let specialization = &shape.specializations[specialization_index];
		assert!(specialization.been_filled);

		if !specialization.fields.is_empty() {
			let is_transparent_variant = shape.is_transparent_variant;
			drop(user_type);

			let name = context.type_name(type_id);
			let message = if is_transparent_variant {
				error!("Cannot construct transparent enum variant {name} without an initializer")
			} else {
				error!("Cannot construct struct-like enum variant {name} without an initializer")
			};
			context.message(message.span(self.span));

			self.type_id = context.type_store.any_collapse_type_id();

			// We know we are an enum variant, have fields, and we are implicitly
			// being constructed without a struct or transparent initializer
			return false;
		}

		// We are a field-less enum variant so it is valid to treat us like an instance
		self.type_id = type_id;
		true
	}
}

#[derive(Debug)]
pub enum ExpressionKind<'a> {
	AnyCollapse,
	Void,

	ModuleLayer(Ref<RwLock<RootLayer<'a>>>),
	Type { type_id: TypeId },

	Block(Block<'a>),
	IfElseChain(Box<IfElseChain<'a>>),
	Match(Box<Match<'a>>),

	NumberValue(NumberValue),

	BooleanLiteral(bool),
	CodepointLiteral(CodepointLiteral),
	ByteCodepointLiteral(ByteCodepointLiteral),
	StringLiteral(StringLiteral<'a>),
	FormatStringLiteral(FormatStringLiteral<'a>),

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
	UnionVariantToUnion(Box<UnionVariantToUnion<'a>>),
	SliceMutableToImmutable(Box<SliceMutableToImmutable<'a>>),
	StringToFormatString(Box<StringToFormatString<'a>>),
}

impl<'a> ExpressionKind<'a> {
	pub fn name(&self) -> &'static str {
		match self {
			ExpressionKind::AnyCollapse => "AnyCollapse",
			ExpressionKind::Void => "void value",
			ExpressionKind::ModuleLayer(_) => "module",
			ExpressionKind::Type { .. } => "type",
			ExpressionKind::Block(_) => "block",
			ExpressionKind::IfElseChain(_) => "if expression",
			ExpressionKind::Match(_) => "match expression",
			ExpressionKind::NumberValue(_) => "untyped number",
			ExpressionKind::BooleanLiteral(_) => "boolean literal",
			ExpressionKind::CodepointLiteral(_) => "codepoint literal",
			ExpressionKind::ByteCodepointLiteral(_) => "byte codepoint literal",
			ExpressionKind::StringLiteral(_) => "string literal",
			ExpressionKind::FormatStringLiteral(_) => "format string literal",
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
			ExpressionKind::UnionVariantToUnion(inner) => inner.expression.kind.name_with_article(),
			ExpressionKind::SliceMutableToImmutable(inner) => inner.expression.kind.name_with_article(),
			ExpressionKind::StringToFormatString(inner) => inner.expression.kind.name_with_article(),
		}
	}

	pub fn name_with_article(&self) -> &'static str {
		match self {
			ExpressionKind::AnyCollapse => "an AnyCollapse",
			ExpressionKind::Void => "a void value",
			ExpressionKind::ModuleLayer(_) => "a module",
			ExpressionKind::Type { .. } => "a type",
			ExpressionKind::Block(_) => "a block",
			ExpressionKind::IfElseChain(_) => "a if expression",
			ExpressionKind::Match(_) => "a match expression",
			ExpressionKind::NumberValue(_) => "an untyped number",
			ExpressionKind::BooleanLiteral(_) => "a boolean literal",
			ExpressionKind::CodepointLiteral(_) => "a codepoint literal",
			ExpressionKind::ByteCodepointLiteral(_) => "a byte codepoint literal",
			ExpressionKind::StringLiteral(_) => "a string literal",
			ExpressionKind::FormatStringLiteral(_) => "a format string literal",
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
			ExpressionKind::UnionVariantToUnion(inner) => inner.expression.kind.name_with_article(),
			ExpressionKind::SliceMutableToImmutable(inner) => inner.expression.kind.name_with_article(),
			ExpressionKind::StringToFormatString(inner) => inner.expression.kind.name_with_article(),
		}
	}
}

#[derive(Debug, Clone)]
pub enum ConstantValue<'a> {
	AnyCollapse,
	NumberValue(NumberValue),
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
pub struct NumberValue {
	value: Decimal,
	pub span: Span,
	collapse: Option<TypeId>,
}

impl NumberValue {
	pub fn new(value: Decimal, span: Span) -> NumberValue {
		NumberValue { value, span, collapse: None }
	}

	pub fn new_collapsed(value: Decimal, span: Span, collapse: TypeId) -> NumberValue {
		NumberValue { value, span, collapse: Some(collapse) }
	}

	pub fn value(&self) -> Decimal {
		self.value
	}

	pub fn is_integer(&self) -> bool {
		self.value.is_integer()
	}

	// Return value indicates success if true
	pub fn collapse(&mut self, type_store: &TypeStore, type_id: TypeId) -> bool {
		let mut success = true;
		if let Some(collapsed) = self.collapse {
			success = type_store.direct_match(collapsed, type_id);
		}

		self.collapse = Some(type_id);
		success
	}

	pub fn collapsed(&self) -> Option<TypeId> {
		self.collapse
	}

	pub fn negate(&mut self, sign_span: Span) {
		assert_not_collapsed(self.collapse);
		self.value.set_sign_positive(self.value.is_sign_negative());
		self.span += sign_span;
	}

	pub fn add(self, messages: &mut Messages, other: NumberValue) -> Option<NumberValue> {
		assert_not_collapsed(self.collapse);
		assert_not_collapsed(other.collapse);
		let span = self.span + other.span;

		let Some(value) = self.value.checked_add(other.value) else {
			let err = error!("Overflow or underflow in constant addition");
			messages.message(err.span(span));
			return None;
		};

		Some(NumberValue { value, span, collapse: None })
	}

	pub fn sub(self, messages: &mut Messages, other: NumberValue) -> Option<NumberValue> {
		assert_not_collapsed(self.collapse);
		assert_not_collapsed(other.collapse);

		let span = self.span + other.span;

		let Some(value) = self.value.checked_sub(other.value) else {
			let err = error!("Overflow or underflow in constant subtraction");
			messages.message(err.span(span));
			return None;
		};

		Some(NumberValue { value, span, collapse: None })
	}

	pub fn mul(self, messages: &mut Messages, other: NumberValue) -> Option<NumberValue> {
		assert_not_collapsed(self.collapse);
		assert_not_collapsed(other.collapse);
		let span = self.span + other.span;

		let Some(value) = self.value.checked_mul(other.value) else {
			let err = error!("Overflow or underflow in constant multiplication");
			messages.message(err.span(span));
			return None;
		};

		Some(NumberValue { value, span, collapse: None })
	}

	pub fn div(self, messages: &mut Messages, other: NumberValue) -> Option<NumberValue> {
		assert_not_collapsed(self.collapse);
		assert_not_collapsed(other.collapse);
		let span = self.span + other.span;

		let Some(value) = self.value.checked_div(other.value) else {
			let err = error!("Overflow or underflow in constant division");
			messages.message(err.span(span));
			return None;
		};

		Some(NumberValue { value, span, collapse: None })
	}

	// TODO: These error messages should be thought out a bit better
	pub fn modulo(self, messages: &mut Messages, other: NumberValue) -> Option<NumberValue> {
		assert_not_collapsed(self.collapse);
		assert_not_collapsed(other.collapse);
		let span = self.span + other.span;

		let truncated_remainder = self.value % other.value;
		let value = if truncated_remainder.is_sign_negative() {
			let abs = other.value.abs();
			let Some(added) = truncated_remainder.checked_add(abs) else {
				let err = error!("Addition failure in constant modulo");
				messages.message(err.span(span));
				return None;
			};

			added
		} else {
			truncated_remainder
		};

		Some(NumberValue { value, span, collapse: None })
	}

	fn check_not_integer_error(&self, messages: &mut Messages, other: NumberValue, op: &str, side: &str) -> bool {
		if !self.value.is_integer() {
			let error = error!("Cannot perform constant {op} with {side} side value {}", self.value);
			messages.message(error.span(self.span + other.span));
			return true;
		}

		false
	}

	pub fn bitwise_and(self, messages: &mut Messages, other: NumberValue) -> Option<NumberValue> {
		if self.check_not_integer_error(messages, other, "bitwise and", "left") {
			return None;
		}

		if other.check_not_integer_error(messages, self, "bitwise and", "right") {
			return None;
		}

		let result = self.value.to_i128().unwrap() & other.value.to_i128().unwrap();
		let value = Decimal::from(result);
		let span = self.span + other.span;
		Some(NumberValue { value, span, collapse: None })
	}

	pub fn bitwise_or(self, messages: &mut Messages, other: NumberValue) -> Option<NumberValue> {
		if self.check_not_integer_error(messages, other, "bitwise or", "left") {
			return None;
		}

		if other.check_not_integer_error(messages, self, "bitwise or", "right") {
			return None;
		}

		let result = self.value.to_i128().unwrap() | other.value.to_i128().unwrap();
		let value = Decimal::from(result);
		let span = self.span + other.span;
		Some(NumberValue { value, span, collapse: None })
	}

	pub fn bitwise_xor(self, messages: &mut Messages, other: NumberValue) -> Option<NumberValue> {
		if self.check_not_integer_error(messages, other, "bitwise xor", "left") {
			return None;
		}

		if other.check_not_integer_error(messages, self, "bitwise xor", "right") {
			return None;
		}

		let result = self.value.to_i128().unwrap() ^ other.value.to_i128().unwrap();
		let value = Decimal::from(result);
		let span = self.span + other.span;
		Some(NumberValue { value, span, collapse: None })
	}

	pub fn bitshift_left(self, messages: &mut Messages, other: NumberValue) -> Option<NumberValue> {
		if !self.value.is_integer() {
			let error = error!("Cannot perform constant bitshift left with left side value {}", self.value);
			messages.message(error.span(self.span + other.span));
			return None;
		}

		if other.value.is_sign_negative() || other.value.is_zero() || !other.value.is_integer() {
			let error = error!("Cannot perform constant bitshift left with right side value {}", other.value);
			messages.message(error.span(self.span + other.span));
			return None;
		}

		// TODO: This is rather silly but rust_decimal does not seem to have "proper" shift methods

		let span = self.span + other.span;
		let mut count = other.value;
		let mut value = self.value;
		while count.cmp(&Decimal::ZERO) == Ordering::Greater {
			count = count.saturating_sub(Decimal::ONE);

			let Some(new_value) = value.checked_mul(Decimal::TWO) else {
				let error = error!("Overflow or underflow in constant bitshift left");
				messages.message(error.span(span));
				return None;
			};

			value = new_value;
		}

		Some(NumberValue { value, span, collapse: None })
	}

	pub fn bitshift_right(self, messages: &mut Messages, other: NumberValue) -> Option<NumberValue> {
		if !self.value.is_integer() {
			let error = error!("Cannot perform constant bitshift right with left side value {}", self.value);
			messages.message(error.span(self.span + other.span));
			return None;
		}

		if other.value.is_sign_negative() || other.value.is_zero() || !other.value.is_integer() {
			let error = error!("Cannot perform constant bitshift right with right side value {}", other.value);
			messages.message(error.span(self.span + other.span));
			return None;
		}

		// TODO: This is rather silly but rust_decimal does not seem to have "proper" shift methods

		let span = self.span + other.span;
		let mut count = other.value;
		let mut value = self.value;
		while count.cmp(&Decimal::ZERO) == Ordering::Greater {
			count = count.saturating_sub(Decimal::ONE);

			let Some(new_value) = value.checked_div(Decimal::TWO) else {
				let error = error!("Underflow or underflow in constant bitshift right");
				messages.message(error.span(span));
				return None;
			};

			value = new_value.trunc();
		}

		Some(NumberValue { value, span, collapse: None })
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

#[derive(Debug)]
pub enum FormatStringItem<'a> {
	Text(Cow<'a, str>),
	Expression(Expression<'a>),
}

#[derive(Debug)]
pub struct FormatStringLiteral<'a> {
	pub items: Vec<FormatStringItem<'a>>,
}

#[derive(Debug)]
pub struct ArrayLiteral<'a> {
	pub type_id: TypeId,
	pub pointee_type_id: TypeId,
	pub expressions: Vec<Expression<'a>>,
}

#[derive(Debug)]
pub struct StructLiteral<'a> {
	pub type_id: TypeId,
	pub field_initializers: Vec<FieldInitializer<'a>>,
}

#[derive(Debug)]
pub struct FieldInitializer<'a> {
	pub expression: Expression<'a>,
}

#[derive(Debug)]
pub struct Call<'a> {
	pub span: Span,
	pub name: &'a str,
	pub function_id: FunctionId,
	pub arguments: Vec<Expression<'a>>,
}

#[derive(Debug)]
pub struct MethodCall<'a> {
	pub base: Expression<'a>,
	pub function_id: FunctionId,
	pub arguments: Vec<Expression<'a>>,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct FieldRead<'a> {
	pub base: Expression<'a>,
	pub name: &'a str,
	pub field_index: usize,
	pub immutable_reason: Option<FieldReadImmutableReason>,
}

#[derive(Debug)]
pub enum UnaryOperator<'a> {
	Negate,
	Invert,
	AddressOf,
	AddressOfMut,
	Dereference,
	Cast { type_id: TypeId },
	Index { index_expression: Expression<'a> },
	RangeIndex { index_expression: Expression<'a> },
}

#[derive(Debug)]
pub struct UnaryOperation<'a> {
	pub op: UnaryOperator<'a>,
	pub type_id: TypeId,
	pub expression: Expression<'a>,
}

#[derive(Debug)]
pub struct BinaryOperation<'a> {
	pub op: BinaryOperator,
	pub left: Expression<'a>,
	pub right: Expression<'a>,
	pub type_id: TypeId,
}

#[derive(Debug)]
pub struct CheckIs<'a> {
	pub left: Expression<'a>,
	pub binding: Option<ResultBinding>,
	pub variant_infos: Vec<VariantInfo>,
}

#[derive(Debug, Clone, Copy)]
pub struct ResultBinding {
	pub type_id: TypeId,
	pub readable_index: usize,
}

#[derive(Debug)]
pub struct EnumVariantToEnum<'a> {
	pub type_id: TypeId,
	pub expression: Expression<'a>,
}

#[derive(Debug)]
pub struct UnionVariantToUnion<'a> {
	pub type_id: TypeId,
	pub expression: Expression<'a>,
}

#[derive(Debug)]
pub struct SliceMutableToImmutable<'a> {
	pub expression: Expression<'a>,
}

#[derive(Debug)]
pub struct StringToFormatString<'a> {
	pub expression: Expression<'a>,
}
