use std::borrow::Cow;

use crate::frontend::file::SourceFile;
use crate::frontend::span::Span;
use crate::frontend::tokenizer::Token;

#[derive(Debug)]
pub struct File<'a> {
	pub source_file: &'a SourceFile,
	pub module_path: &'a [String],
	pub block: Block<'a>,
}

#[derive(Debug)]
pub struct PathSegments<'a> {
	//TODO: Write a `Vec` wrapper which guarantees at least one item as well as infallible `.first()` and `.last()`
	pub segments: Vec<Node<&'a str>>,
}

impl<'a> PathSegments<'a> {
	pub fn len(&self) -> usize {
		self.segments.len()
	}

	pub fn is_empty(&self) -> bool {
		self.segments.is_empty()
	}
}

#[derive(Debug)]
pub struct Module<'a> {
	pub path_segments: Node<PathSegments<'a>>,
}

#[derive(Debug)]
pub struct Import<'a> {
	pub path_segments: PathSegments<'a>,
	pub symbol_names: Vec<Node<&'a str>>,
}

#[derive(Debug)]
pub struct GenericAttribute<'a> {
	pub names: Vec<Node<&'a str>>,
}

#[derive(Debug, Clone, Copy)]
pub enum ExternAttribute<'a> {
	Name(&'a str),
	Intrinsic,
}

#[derive(Debug)]
pub struct Attributes<'a> {
	pub generic_attribute: Option<Node<GenericAttribute<'a>>>,
	pub extern_attribute: Option<Node<ExternAttribute<'a>>>,
}

impl<'a> Attributes<'a> {
	pub const FIELD_COUNT: usize = 2;

	pub fn blank() -> Self {
		Attributes { generic_attribute: None, extern_attribute: None }
	}

	pub fn attribute_spans<'b>(&self, buffer: &'b mut [Span]) -> &'b [Span] {
		fn push_potential_span<T>(attribute: &Option<Node<T>>, buffer: &mut [Span], index: &mut usize) {
			if let Some(attribute) = attribute {
				buffer[*index] = attribute.span;
				*index += 1;
			}
		}

		let mut index = 0;
		push_potential_span(&self.generic_attribute, buffer, &mut index);
		push_potential_span(&self.extern_attribute, buffer, &mut index);

		&buffer[0..index]
	}
}

#[derive(Debug)]
pub enum Type<'a> {
	Void,

	Pointer {
		pointee: Box<Node<Type<'a>>>,
		mutable: bool,
	},

	Slice {
		pointee: Box<Node<Type<'a>>>,
		mutable: bool,
	},

	Path {
		path_segments: Node<PathSegments<'a>>,
		type_arguments: Vec<Node<Type<'a>>>,
	},
}

#[derive(Debug)]
pub struct Struct<'a> {
	pub generics: Vec<Node<&'a str>>,
	pub name: Node<&'a str>,
	pub fields: Vec<Field<'a>>,
}

#[derive(Debug)]
pub struct Field<'a> {
	pub name: Node<&'a str>,
	pub parsed_type: Node<Type<'a>>,
}

#[derive(Debug)]
pub struct Function<'a> {
	pub generics: Vec<Node<&'a str>>,
	pub extern_attribute: Option<Node<ExternAttribute<'a>>>,
	pub name: Node<&'a str>,
	pub parameters: Vec<Node<Parameter<'a>>>,
	pub parsed_type: Option<Node<Type<'a>>>,
	pub block: Option<Node<Block<'a>>>,
}

#[derive(Debug)]
pub struct Parameter<'a> {
	pub name: Node<&'a str>,
	pub parsed_type: Node<Type<'a>>,
	pub is_mutable: bool,
}

#[derive(Debug)]
pub struct Const<'a> {
	pub name: Node<&'a str>,
	pub parsed_type: Option<Node<Type<'a>>>,
	pub expression: Node<Expression<'a>>,
}

#[derive(Debug)]
pub struct Binding<'a> {
	pub name: Node<&'a str>,
	pub parsed_type: Option<Node<Type<'a>>>,
	pub expression: Node<Expression<'a>>,
	pub is_mutable: bool,
}

#[derive(Debug)]
pub struct IntegerLiteral {
	pub value: Node<i128>,
}

#[derive(Debug)]
pub struct FloatLiteral {
	pub value: Node<f64>,
}

#[derive(Debug)]
pub struct CodepointLiteral {
	pub value: Node<char>,
}

#[derive(Debug)]
pub struct StringLiteral<'a> {
	pub value: Node<Cow<'a, str>>,
}

#[derive(Debug)]
pub struct ArrayLiteral<'a> {
	pub expressions: Vec<Node<Expression<'a>>>,
}

#[derive(Debug)]
pub struct StructLiteral<'a> {
	pub parsed_type: Node<Type<'a>>,
	pub initializer: Node<StructInitializer<'a>>,
}

#[derive(Debug)]
pub struct StructInitializer<'a> {
	pub field_initializers: Vec<FieldInitializer<'a>>,
}

#[derive(Debug)]
pub struct FieldInitializer<'a> {
	pub name: Node<&'a str>,
	pub expression: Node<Expression<'a>>,
}

#[derive(Debug)]
pub enum UnaryOperator<'a> {
	Negate,
	Invert,
	AddressOf,
	AddressOfMut,
	Dereference,
	Cast { parsed_type: Node<Type<'a>> },
	Index { index_expression: Node<Expression<'a>> },
}

#[derive(Debug)]
pub struct UnaryOperation<'a> {
	pub op: Node<UnaryOperator<'a>>,
	pub expression: Node<Expression<'a>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Associativity {
	Left,
	Right,
}

// Should this type live in `ir.rs` instead?
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
	Assign,

	Add,
	Sub,
	Mul,
	Div,

	Equals,
	NotEquals,

	GreaterThan,
	GreaterThanEquals,

	LessThan,
	LessThanEquals,

	LogicalAnd,
	LogicalOr,
}

impl BinaryOperator {
	pub fn name(self) -> &'static str {
		match self {
			BinaryOperator::Assign => "Assignment",
			BinaryOperator::Add => "Addition",
			BinaryOperator::Sub => "Subtraction",
			BinaryOperator::Mul => "Multiplication",
			BinaryOperator::Div => "Division",
			BinaryOperator::Equals => "Equals",
			BinaryOperator::NotEquals => "Not Equals",
			BinaryOperator::GreaterThan => "Greater Than",
			BinaryOperator::GreaterThanEquals => "Greater Than Equals",
			BinaryOperator::LessThan => "Less Than",
			BinaryOperator::LessThanEquals => "Less Than Equals",
			BinaryOperator::LogicalAnd => "Logical And",
			BinaryOperator::LogicalOr => "Logical Or",
		}
	}
}

impl BinaryOperator {
	pub fn precedence(self) -> u32 {
		use BinaryOperator::*;
		match self {
			Assign => 0,
			LogicalAnd | LogicalOr => 1,
			Equals | NotEquals | GreaterThan | GreaterThanEquals | LessThan | LessThanEquals => 2,
			Add | Sub => 3,
			Mul | Div => 4,
		}
	}

	pub fn associativity(self) -> Associativity {
		use BinaryOperator::*;
		match self {
			Assign => Associativity::Right,
			Add | Sub | Mul | Div => Associativity::Left,
			Equals | NotEquals | GreaterThan | GreaterThanEquals | LessThan | LessThanEquals => Associativity::Left,
			LogicalAnd | LogicalOr => Associativity::Left,
		}
	}
}

#[derive(Debug)]
pub struct BinaryOperation<'a> {
	pub op: Node<BinaryOperator>,
	pub left: Node<Expression<'a>>,
	pub right: Node<Expression<'a>>,
}

#[derive(Debug)]
pub struct Call<'a> {
	pub path_segments: Node<PathSegments<'a>>,
	pub type_arguments: Vec<Node<Type<'a>>>,
	pub arguments: Vec<Node<Expression<'a>>>,
}

#[derive(Debug)]
pub struct Read<'a> {
	pub path_segments: Node<PathSegments<'a>>,
}

#[derive(Debug)]
pub struct FieldRead<'a> {
	pub base: Node<Expression<'a>>,
	pub name: Node<&'a str>,
}

#[derive(Debug)]
pub struct Return<'a> {
	pub expression: Option<Node<Expression<'a>>>,
}

#[derive(Debug)]
pub enum Statement<'a> {
	Expression(Node<Expression<'a>>),

	Block(Node<Block<'a>>),

	Import(Node<Import<'a>>),

	Struct(Struct<'a>),
	Function(Box<Function<'a>>),

	Const(Box<Node<Const<'a>>>),
	Binding(Box<Node<Binding<'a>>>),

	Return(Box<Node<Return<'a>>>),

	CIncludeSystem(Node<&'a str>),
}

impl<'a> Statement<'a> {
	pub fn span(&self) -> Span {
		use Statement::*;

		//TODO: Struct and Function could be improved
		match self {
			Expression(statement) => statement.span,
			Block(statement) => statement.span,
			Import(statement) => statement.span,
			Struct(statement) => statement.name.span,
			Function(statement) => statement.name.span,
			Const(statement) => statement.span,
			Binding(statement) => statement.span,
			Return(statement) => statement.span,
			CIncludeSystem(statement) => statement.span,
		}
	}

	pub fn name_and_article(&self) -> &'static str {
		use Statement::*;

		match self {
			Expression(..) => "An expression",
			Block(..) => "A block",
			Import(..) => "An import statement",
			Struct(..) => "A struct definition",
			Function(..) => "A function definition",
			Const(..) => "A const definition",
			Binding(..) => "A binding definition",
			Return(..) => "A return statement",
			CIncludeSystem(..) => "A C include statement",
		}
	}
}

#[derive(Debug)]
pub struct Block<'a> {
	pub statements: Vec<Statement<'a>>,
}

#[derive(Debug)]
pub struct If<'a> {
	pub condition: Node<Expression<'a>>,
	pub body: Node<Expression<'a>>,
}

#[derive(Debug)]
pub enum Expression<'a> {
	Block(Block<'a>),
	If(Box<If<'a>>),

	IntegerLiteral(IntegerLiteral),
	FloatLiteral(FloatLiteral),

	BooleanLiteral(bool),
	CodepointLiteral(CodepointLiteral),
	StringLiteral(StringLiteral<'a>),

	ArrayLiteral(ArrayLiteral<'a>),
	StructLiteral(StructLiteral<'a>),

	Call(Call<'a>),
	Read(Read<'a>),
	FieldRead(Box<FieldRead<'a>>),

	UnaryOperation(Box<UnaryOperation<'a>>),
	BinaryOperation(Box<BinaryOperation<'a>>),
}

#[derive(Debug)]
pub struct Node<T> {
	pub item: T,
	pub span: Span,
}

impl<T> Node<T> {
	pub fn new(node: T, span: Span) -> Node<T> {
		Node { item: node, span }
	}

	pub fn from_token(node: T, token: Token) -> Node<T> {
		Node { item: node, span: token.span }
	}
}

impl<T: Copy> Copy for Node<T> {}

impl<T: Clone> Clone for Node<T> {
	fn clone(&self) -> Self {
		Self { item: self.item.clone(), span: self.span }
	}
}
