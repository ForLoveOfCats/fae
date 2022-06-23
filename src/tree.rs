use crate::span::Span;
use crate::tokenizer::Token;

#[must_use]
#[derive(Debug)]
pub struct File<'a> {
	pub module: Node<Module<'a>>,
	pub items: Vec<Item<'a>>,
}

#[must_use]
#[derive(Debug)]
pub struct PathSegments<'a> {
	//TODO: Write a `Vec` wrapper which guarantees at least one item as well as infallible `.first()` and `.last()`
	pub segments: Vec<Node<&'a str>>,
}

#[must_use]
#[derive(Debug)]
pub struct Module<'a> {
	pub path_segments: Node<PathSegments<'a>>,
}

#[must_use]
#[derive(Debug)]
pub struct Using<'a> {
	pub path_segments: Node<PathSegments<'a>>,
}

#[must_use]
#[derive(Debug)]
pub enum Type<'a> {
	Void,
	Reference(Box<Node<Type<'a>>>),
	Pointer(Box<Node<Type<'a>>>),
	Slice(Box<Node<Type<'a>>>),
	Path(PathSegments<'a>),
}

#[must_use]
#[derive(Debug)]
pub struct Struct<'a> {
	pub name: Node<&'a str>,
	pub fields: Vec<Field<'a>>,
}

#[must_use]
#[derive(Debug)]
pub struct Field<'a> {
	pub name: Node<&'a str>,
	pub parsed_type: Node<Type<'a>>,
}

#[must_use]
#[derive(Debug)]
pub struct Function<'a> {
	pub name: Node<&'a str>,
	pub parameters: Vec<Node<Parameter<'a>>>,
	pub parsed_type: Node<Type<'a>>,
	pub block: Node<Vec<Statement<'a>>>,
}

#[must_use]
#[derive(Debug)]
pub struct Parameter<'a> {
	pub name: Node<&'a str>,
	pub parsed_type: Node<Type<'a>>,
}

#[must_use]
#[derive(Debug)]
pub struct Const<'a> {
	pub name: Node<&'a str>,
	pub parsed_type: Option<Node<Type<'a>>>,
	pub expression: Node<Expression<'a>>,
}

#[must_use]
#[derive(Debug)]
pub struct Let<'a> {
	pub name: Node<&'a str>,
	pub parsed_type: Option<Node<Type<'a>>>,
	pub expression: Node<Expression<'a>>,
}

#[must_use]
#[derive(Debug)]
pub struct UnsignedIntegerLiteral {
	pub value: Node<u64>,
}

#[must_use]
#[derive(Debug)]
pub struct SignedIntegerLiteral {
	pub value: Node<i64>,
}

#[must_use]
#[derive(Debug)]
pub struct FloatLiteral {
	pub value: Node<f64>,
}

#[must_use]
#[derive(Debug)]
pub struct CharLiteral {
	pub value: Node<char>,
}

#[must_use]
#[derive(Debug)]
pub struct StringLiteral<'a> {
	pub value: Node<&'a str>,
}

#[must_use]
#[derive(Debug)]
pub struct StructLiteral<'a> {
	pub path_segments: Node<PathSegments<'a>>,
	pub initializer: Node<StructInitializer<'a>>,
}

#[must_use]
#[derive(Debug)]
pub struct StructInitializer<'a> {
	pub field_initializers: Vec<FieldInitializer<'a>>,
}

#[must_use]
#[derive(Debug)]
pub struct FieldInitializer<'a> {
	pub name: Node<&'a str>,
	pub expression: Node<Expression<'a>>,
}

#[must_use]
#[derive(Debug, Clone, Copy)]
pub enum Operator {
	Add,
	Sub,
	Mul,
	Div,
}

impl Operator {
	pub fn precedence(self) -> u32 {
		use Operator::*;
		match self {
			Add | Sub => 0,
			Mul | Div => 1,
		}
	}
}

#[must_use]
#[derive(Debug)]
pub struct BinaryOperation<'a> {
	pub op: Node<Operator>,
	pub left: Node<Expression<'a>>,
	pub right: Node<Expression<'a>>,
}

#[must_use]
#[derive(Debug)]
pub struct Call<'a> {
	pub path_segments: Node<PathSegments<'a>>,
	pub arguments: Node<Vec<Expression<'a>>>,
}

#[must_use]
#[derive(Debug)]
pub struct Read<'a> {
	pub path_segments: Node<PathSegments<'a>>,
}

#[must_use]
#[derive(Debug)]
pub struct Return<'a> {
	pub expression: Node<Expression<'a>>,
}

#[must_use]
#[derive(Debug)]
pub enum Item<'a> {
	Using(Using<'a>),

	Struct(Struct<'a>),
	Function(Box<Function<'a>>),

	Const(Box<Const<'a>>),
}

#[must_use]
#[derive(Debug)]
pub enum Statement<'a> {
	Expression(Expression<'a>),

	Block(Vec<Statement<'a>>),

	Using(Using<'a>),

	Struct(Struct<'a>),
	Function(Box<Function<'a>>),

	Const(Box<Const<'a>>),
	Let(Box<Let<'a>>),

	Return(Box<Return<'a>>),
}

#[must_use]
#[derive(Debug)]
pub enum Expression<'a> {
	Block(Vec<Statement<'a>>),

	UnsignedIntegerLiteral(UnsignedIntegerLiteral),
	SignedIntegerLiteral(SignedIntegerLiteral),

	FloatLiteral(FloatLiteral),

	CharLiteral(CharLiteral),
	StringLiteral(StringLiteral<'a>),

	StructLiteral(StructLiteral<'a>),

	Call(Call<'a>),
	Read(Read<'a>),

	BinaryOperation(Box<BinaryOperation<'a>>),
}

#[must_use]
#[derive(Debug)]
pub struct Node<T> {
	pub node: T,
	pub span: Span,
}

impl<T> Node<T> {
	pub fn new(node: T, span: Span) -> Node<T> {
		Node { node, span }
	}

	pub fn from_token(node: T, token: Token) -> Node<T> {
		Node {
			node,
			span: token.span,
		}
	}
}
