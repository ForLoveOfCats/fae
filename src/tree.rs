use crate::file::SourceFile;
use crate::span::Span;
use crate::tokenizer::Token;

#[must_use]
#[derive(Debug)]
pub struct File<'a> {
	pub source_file: &'a SourceFile,
	pub module_path: &'a [String],
	pub block: Block<'a>,
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
	Slice(Box<Node<Type<'a>>>),

	Path {
		segments: PathSegments<'a>,
		arguments: Vec<Node<Type<'a>>>,
	},
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
	pub block: Node<Block<'a>>,
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
pub struct Mut<'a> {
	pub name: Node<&'a str>,
	pub parsed_type: Option<Node<Type<'a>>>,
	pub expression: Node<Expression<'a>>,
}

#[must_use]
#[derive(Debug)]
pub struct IntegerLiteral {
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
	Negate,
}

#[must_use]
#[derive(Debug)]
pub struct UnaryOperation<'a> {
	pub op: Node<UnaryOperator>,
	pub expression: Node<Expression<'a>>,
}

#[must_use]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Associativity {
	Left,
	Right,
}

#[must_use]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
	Assign,
	Add,
	Sub,
	Mul,
	Div,
}

impl BinaryOperator {
	pub fn precedence(self) -> u32 {
		use BinaryOperator::*;
		match self {
			Assign => 0,
			Add | Sub => 1,
			Mul | Div => 2,
		}
	}

	pub fn associativity(self) -> Associativity {
		use BinaryOperator::*;
		match self {
			Assign => Associativity::Right,
			Add | Sub | Mul | Div => Associativity::Left,
		}
	}
}

#[must_use]
#[derive(Debug)]
pub struct BinaryOperation<'a> {
	pub op: Node<BinaryOperator>,
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
pub enum Statement<'a> {
	Expression(Node<Expression<'a>>),

	Block(Node<Block<'a>>),

	Using(Node<Using<'a>>),

	Struct(Struct<'a>),
	Function(Box<Function<'a>>),

	Const(Box<Node<Const<'a>>>),
	Let(Box<Node<Let<'a>>>),
	Mut(Box<Node<Mut<'a>>>),

	Return(Box<Node<Return<'a>>>),
}

impl<'a> Statement<'a> {
	pub fn span(&self) -> Span {
		use Statement::*;

		//TODO: Struct and Function could be improved
		match self {
			Expression(statement) => statement.span,
			Block(statement) => statement.span,
			Using(statement) => statement.span,
			Struct(statement) => statement.name.span,
			Function(statement) => statement.name.span,
			Const(statement) => statement.span,
			Let(statement) => statement.span,
			Mut(statement) => statement.span,
			Return(statement) => statement.span,
			_ => unimplemented!(),
		}
	}
}

#[must_use]
#[derive(Debug)]
pub struct Block<'a> {
	pub statements: Vec<Statement<'a>>,
}

#[must_use]
#[derive(Debug)]
pub enum Expression<'a> {
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
		Node { node, span: token.span }
	}
}

impl<T: Copy> Copy for Node<T> {}

impl<T: Clone> Clone for Node<T> {
	fn clone(&self) -> Self {
		Self {
			node: self.node.clone(),
			span: self.span,
		}
	}
}
