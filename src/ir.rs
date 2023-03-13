use crate::error::Messages;
use crate::span::Span;
use crate::tree::Node;

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
	BuiltinType { type_index: usize }, //Not used for slice/reference as those are not symbols
	Type { type_index: usize },
	Function { shape_index: usize },
}

#[derive(Debug)]
pub enum GenericOrTypeId {
	TypeId { id: TypeId },
	Generic { index: usize },
}

#[derive(Debug)]
pub struct UserType<'a> {
	pub span: Span,
	pub module_path: &'a [String],
	pub kind: UserTypeKind<'a>,
}

#[derive(Debug)]
pub enum UserTypeKind<'a> {
	Struct { shape: StructShape<'a> },
}

#[derive(Debug, Clone, Copy)]
pub struct PrimativeType {
	pub name: &'static str,
	pub kind: PrimativeKind,
	pub type_id: TypeId,
}

#[derive(Debug, Clone, Copy)]
pub enum PrimativeKind {
	Void,

	I8,
	I16,
	I32,
	I64,

	U8,
	U16,
	U32,
	U64,

	F16,
	F32,
	F64,
}

#[derive(Debug)]
pub struct StructShape<'a> {
	pub name: &'a str,
	pub generics: Vec<Node<&'a str>>,

	pub fields: Vec<Node<FieldShape<'a>>>,

	pub concrete: Vec<Struct<'a>>,
}

impl<'a> StructShape<'a> {
	pub fn get_or_add_specialization(
		&mut self,
		messages: &mut Messages,
		invoke_span: Span,
		arguments: Vec<TypeId>,
	) -> Option<usize> {
		for (index, existing) in self.concrete.iter().enumerate() {
			if existing.type_arguments.len() == arguments.len() {
				if existing.type_arguments.iter().zip(&arguments).all(|(a, b)| a == b) {
					return Some(index);
				}
			}
		}

		if self.generics.len() != arguments.len() {
			messages.error(
				message!("Expected {} type arguments, got {}", self.generics.len(), arguments.len()).span(invoke_span),
			);
			return None;
		}

		let fields = self
			.fields
			.iter()
			.map(|field| {
				let type_id = match field.item.field_type {
					GenericOrTypeId::TypeId { id } => id,
					GenericOrTypeId::Generic { index } => arguments[index],
				};

				Field { name: field.item.name, type_id }
			})
			.collect::<Vec<_>>();

		let concrete = Struct { type_arguments: arguments, fields };
		self.concrete.push(concrete);
		Some(self.concrete.len() - 1)
	}
}

#[derive(Debug)]
pub struct FieldShape<'a> {
	pub name: &'a str,
	pub field_type: GenericOrTypeId,
}

#[derive(Debug)]
pub struct Struct<'a> {
	pub type_arguments: Vec<TypeId>,
	pub fields: Vec<Field<'a>>,
}

#[derive(Debug)]
pub struct Field<'a> {
	pub name: &'a str,
	pub type_id: TypeId,
}

#[derive(Debug)]
pub struct FunctionShape<'a> {
	pub name: &'a str,
	pub generics: Vec<Node<&'a str>>,

	pub parameters: Vec<ParameterShape<'a>>,
	pub return_type: GenericOrTypeId,

	pub concrete: Vec<Function<'a>>,
}

impl<'a> FunctionShape<'a> {
	pub fn new(
		name: &'a str,
		generics: Vec<Node<&'a str>>,
		parameters: Vec<ParameterShape<'a>>,
		return_type: GenericOrTypeId,
	) -> Self {
		FunctionShape {
			name,
			generics,
			parameters,
			return_type,
			concrete: Vec::new(),
		}
	}
}

#[derive(Debug)]
pub struct ParameterShape<'a> {
	pub name: &'a str,
	pub param_type: GenericOrTypeId,
}

#[derive(Debug)]
pub struct Function<'a> {
	pub paremeters: Vec<Parameter<'a>>,
	pub return_type: TypeId,
}

#[derive(Debug)]
pub struct Parameter<'a> {
	pub name: &'a str,
	pub type_id: TypeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId {
	pub shape_index: usize,
	pub specialization_index: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId {
	pub index: usize,
	pub specialization: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ReadableId {
	pub index: usize,
	pub thread: usize,
}

#[derive(Debug)]
pub struct Block<'a> {
	pub type_id: TypeId,
	pub statements: Vec<Statement<'a>>,
}

#[derive(Debug)]
pub struct Statement<'a> {
	pub type_id: TypeId,
	pub kind: StatementKind<'a>,
}

#[derive(Debug)]
pub enum StatementKind<'a> {
	Expression(Expression<'a>),

	Block(Block<'a>),

	Const(Box<Const<'a>>),
	Let(Box<Let<'a>>),
	Mut(Box<Mut<'a>>),

	Return(Box<Return<'a>>),
}

#[derive(Debug)]
pub struct Const<'a> {
	pub name: &'a str,
	pub type_id: TypeId,
	pub expression: Expression<'a>,
}

#[derive(Debug)]
pub struct Let<'a> {
	pub name: &'a str,
	pub type_id: TypeId,
	pub expression: Expression<'a>,
}

#[derive(Debug)]
pub struct Mut<'a> {
	pub name: &'a str,
	pub type_id: TypeId,
	pub expression: Expression<'a>,
}

#[derive(Debug)]
pub struct Return<'a> {
	pub expression: Expression<'a>,
}

#[derive(Debug)]
pub struct Expression<'a> {
	pub type_id: TypeId,
	pub kind: ExpressionKind<'a>,
}

#[derive(Debug)]
pub enum ExpressionKind<'a> {
	Block(Block<'a>),

	IntegerLiteral(IntegerLiteral),
	FloatLiteral(FloatLiteral),

	CharLiteral(CharLiteral),
	StringLiteral(StringLiteral<'a>),

	StructLiteral(StructLiteral<'a>),
	Call(Call<'a>),
	Read(Read<'a>),

	UnaryOperation(Box<UnaryOperation<'a>>),
	BinaryOperation(Box<BinaryOperation<'a>>),
}

#[derive(Debug)]
pub struct IntegerLiteral {
	pub value: u64,
}

#[derive(Debug)]
pub struct FloatLiteral {
	pub value: f64,
}

#[derive(Debug)]
pub struct CharLiteral {
	pub value: char,
}

#[derive(Debug)]
pub struct StringLiteral<'a> {
	pub value: &'a str,
}

#[derive(Debug)]
pub struct StructLiteral<'a> {
	pub name: &'a str,
	pub field_initializers: Vec<FieldInitializer<'a>>,
}

#[derive(Debug)]
pub struct FieldInitializer<'a> {
	pub name: &'a str,
	pub expression: Expression<'a>,
}

#[derive(Debug)]
pub struct Call<'a> {
	pub name: &'a str,
	pub function_id: FunctionId,
	pub arguments: Vec<Expression<'a>>,
}

#[derive(Debug)]
pub struct Read<'a> {
	pub name: &'a str,
	pub readable_id: ReadableId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
	Negate,
}

#[derive(Debug)]
pub struct UnaryOperation<'a> {
	pub op: UnaryOperator,
	pub expression: Expression<'a>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
	Assign,
	Add,
	Sub,
	Mul,
	Div,
}

#[derive(Debug)]
pub struct BinaryOperation<'a> {
	pub op: BinaryOperator,
	pub left: Expression<'a>,
	pub right: Expression<'a>,
}
