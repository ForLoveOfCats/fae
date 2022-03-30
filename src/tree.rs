use crate::location::SourceLocation;
use crate::tokenizer::Token;

#[must_use]
#[derive(Debug)]
pub struct File<'a> {
	pub module: Node<Module<'a>>,
	pub root_expression: Node<Expression<'a>>,
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
pub struct Struct<'a> {
	pub name: Node<&'a str>,
	pub fields: Vec<Field<'a>>,
}

#[must_use]
#[derive(Debug)]
pub struct Field<'a> {
	pub name: Node<&'a str>,
	pub type_path_segments: Node<PathSegments<'a>>,
}

#[must_use]
#[derive(Debug)]
pub struct Function<'a> {
	pub name: Node<&'a str>,
	pub parameters: Vec<Node<Parameter<'a>>>,
	pub type_path_segments: Node<PathSegments<'a>>,
	pub block: Node<Expression<'a>>,
}

#[must_use]
#[derive(Debug)]
pub struct Parameter<'a> {
	pub name: Node<&'a str>,
	pub type_path_segments: Node<PathSegments<'a>>,
}

#[must_use]
#[derive(Debug)]
pub struct Const<'a> {
	pub name: Node<&'a str>,
	pub type_path_segments: Option<Node<PathSegments<'a>>>,
	pub expression: Node<Expression<'a>>,
}

#[must_use]
#[derive(Debug)]
pub struct Let<'a> {
	pub name: Node<&'a str>,
	pub type_path_segments: Option<Node<PathSegments<'a>>>,
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
pub enum Expression<'a> {
	Block(Vec<Expression<'a>>),

	Module(Module<'a>),
	Using(Using<'a>),

	Struct(Struct<'a>),
	Function(Box<Function<'a>>),

	Const(Box<Const<'a>>),
	Let(Box<Let<'a>>),

	UnsignedIntegerLiteral(UnsignedIntegerLiteral),
	SignedIntegerLiteral(SignedIntegerLiteral),

	FloatLiteral(FloatLiteral),

	CharLiteral(CharLiteral),
	StringLiteral(StringLiteral<'a>),

	StructLiteral(StructLiteral<'a>),

	Call(Call<'a>),
	Read(Read<'a>),

	BinaryOperation(Box<BinaryOperation<'a>>),

	Return(Box<Return<'a>>),
}

#[must_use]
#[derive(Debug)]
pub struct Node<T> {
	pub node: T,
	pub location: SourceLocation,
}

impl<T> Node<T> {
	pub fn new(node: T, location: SourceLocation) -> Node<T> {
		Node { node, location }
	}

	pub fn from_token(node: T, token: Token) -> Node<T> {
		Node {
			node,
			location: token.location,
		}
	}
}
