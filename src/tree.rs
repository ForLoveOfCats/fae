use crate::file::SourceFile;
use crate::span::Span;
use crate::tokenizer::Token;

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
}

#[derive(Debug)]
pub struct Module<'a> {
	pub path_segments: Node<PathSegments<'a>>,
}

#[derive(Debug)]
pub struct Using<'a> {
	pub path_segments: Node<PathSegments<'a>>,
}

#[derive(Debug)]
pub struct GenericAttribute<'a> {
	pub names: Vec<Node<&'a str>>,
}

#[derive(Debug)]
pub struct Attributes<'a> {
	pub generic_attribute: Option<Node<GenericAttribute<'a>>>,
}

impl<'a> Attributes<'a> {
	pub const FIELD_COUNT: usize = 1;

	pub fn blank() -> Self {
		Attributes { generic_attribute: None }
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

		&buffer[0..index]
	}
}

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

impl<'a> Type<'a> {
	pub fn as_single_segment(&self) -> Option<&'a str> {
		match self {
			Type::Path { segments, arguments } if segments.len() == 1 && arguments.is_empty() => {
				Some(segments.segments[0].item)
			}

			_ => None,
		}
	}
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
	pub name: Node<&'a str>,
	pub parameters: Vec<Node<Parameter<'a>>>,
	pub parsed_type: Node<Type<'a>>,
	pub block: Node<Block<'a>>,
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
pub struct Let<'a> {
	pub name: Node<&'a str>,
	pub parsed_type: Option<Node<Type<'a>>>,
	pub expression: Node<Expression<'a>>,
}

#[derive(Debug)]
pub struct Mut<'a> {
	pub name: Node<&'a str>,
	pub parsed_type: Option<Node<Type<'a>>>,
	pub expression: Node<Expression<'a>>,
}

#[derive(Debug)]
pub struct IntegerLiteral {
	pub value: Node<u64>,
}

#[derive(Debug)]
pub struct FloatLiteral {
	pub value: Node<f64>,
}

#[derive(Debug)]
pub struct CharLiteral {
	pub value: Node<char>,
}

#[derive(Debug)]
pub struct StringLiteral<'a> {
	pub value: Node<&'a str>,
}

#[derive(Debug)]
pub struct StructLiteral<'a> {
	pub path_segments: Node<PathSegments<'a>>,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
	Negate,
}

#[derive(Debug)]
pub struct UnaryOperation<'a> {
	pub op: Node<UnaryOperator>,
	pub expression: Node<Expression<'a>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Associativity {
	Left,
	Right,
}

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
pub struct Return<'a> {
	pub expression: Node<Expression<'a>>,
}

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
		}
	}

	pub fn name_and_article(&self) -> &'static str {
		use Statement::*;

		match self {
			Expression(..) => "An expression",
			Block(..) => "A block",
			Using(..) => "A using statement",
			Struct(..) => "A struct definition",
			Function(..) => "A function definition",
			Const(..) => "A const definition",
			Let(..) => "A let statement",
			Mut(..) => "A mut statement",
			Return(..) => "A return statement",
		}
	}
}

#[derive(Debug)]
pub struct Block<'a> {
	pub statements: Vec<Statement<'a>>,
}

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
