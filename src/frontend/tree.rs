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

//TODO: Write a `Vec` wrapper which guarantees at least one item as well as infallible `.first()` and `.last()`
#[derive(Debug)]
pub struct PathSegments<'a> {
	pub segments: Vec<Node<&'a str>>,
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
pub struct ExternAttribute<'a> {
	pub name: &'a str,
}

#[derive(Debug, Clone, Copy)]
pub struct ExportAttribute<'a> {
	pub name: &'a str,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MethodKind {
	Static,
	ImmutableSelf,
	MutableSelf,
}

#[derive(Debug)]
pub struct MethodAttribute<'a> {
	pub base_type: Node<PathSegments<'a>>,
	pub kind: MethodKind,
}

#[derive(Debug, Clone, Copy)]
pub struct IntrinsicAttribute;

#[derive(Debug, Clone, Copy)]
pub struct LangAttribute<'a> {
	pub name: &'a str,
}

pub struct AllowedAttributes {
	pub generic_attribute: bool,
	pub extern_attribute: bool,
	pub export_attribute: bool,
	pub method_attribute: bool,
	pub intrinsic_attribute: bool,
	pub lang_attribute: bool,
}

#[derive(Debug)]
pub struct Attributes<'a> {
	pub generic_attribute: Option<Node<GenericAttribute<'a>>>,
	pub extern_attribute: Option<Node<ExternAttribute<'a>>>,
	pub export_attribute: Option<Node<ExportAttribute<'a>>>,
	pub method_attribute: Option<Node<MethodAttribute<'a>>>,
	pub intrinsic_attribute: Option<Node<IntrinsicAttribute>>,
	pub lang_attribute: Option<Node<LangAttribute<'a>>>,
}

impl<'a> Attributes<'a> {
	pub const FIELD_COUNT: usize = 2;

	pub fn blank() -> Self {
		Attributes {
			generic_attribute: None,
			extern_attribute: None,
			export_attribute: None,
			method_attribute: None,
			intrinsic_attribute: None,
			lang_attribute: None,
		}
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
		push_potential_span(&self.export_attribute, buffer, &mut index);

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
		dot_access: Option<Node<&'a str>>,
	},
}

#[derive(Debug)]
pub struct Struct<'a> {
	pub generics: Vec<Node<&'a str>>,
	pub name: Node<&'a str>,
	pub fields: Vec<Field<'a>>,
}

#[derive(Debug)]
pub struct Enum<'a> {
	pub generics: Vec<Node<&'a str>>,
	pub name: Node<&'a str>,
	pub shared_fields: Vec<Field<'a>>,
	pub variants: Vec<EnumVariant<'a>>,
}

#[derive(Debug)]
pub struct EnumVariant<'a> {
	pub name: Node<&'a str>,
	pub fields: Vec<Field<'a>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FieldAttribute {
	Private,
	Readable,
}

#[derive(Debug)]
pub struct Field<'a> {
	pub name: Node<&'a str>,
	pub parsed_type: Node<Type<'a>>,
	pub attribute: Option<Node<FieldAttribute>>,
	pub read_only: bool,
}

#[derive(Debug)]
pub struct Function<'a> {
	pub generics: Vec<Node<&'a str>>,
	pub extern_attribute: Option<Node<ExternAttribute<'a>>>,
	pub export_attribute: Option<Node<ExportAttribute<'a>>>,
	pub method_attribute: Option<Node<MethodAttribute<'a>>>,
	pub intrinsic_attribute: Option<Node<IntrinsicAttribute>>,
	pub lang_attribute: Option<Node<LangAttribute<'a>>>,
	pub name: Node<&'a str>,
	pub parameters: Parameters<'a>,
	pub parsed_type: Option<Node<Type<'a>>>,
	pub block: Option<Node<Block<'a>>>,
	pub index_in_block: usize,
}

#[derive(Debug)]
pub struct Parameters<'a> {
	pub parameters: Vec<Node<Parameter<'a>>>,
	pub c_varargs: Option<Span>,
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
pub struct Static<'a> {
	pub name: Node<&'a str>,
	pub parsed_type: Node<Type<'a>>,
	pub extern_attribute: Option<Node<ExternAttribute<'a>>>,
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
pub struct ByteCodepointLiteral {
	pub value: Node<u8>,
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
	AddAssign,
	Sub,
	SubAssign,
	Mul,
	MulAssign,
	Div,
	DivAssign,
	Modulo,
	ModuloAssign,

	BitshiftLeft,
	BitshiftLeftAssign,
	BitshiftRight,
	BitshiftRightAssign,

	BitwiseAnd,
	BitwiseAndAssign,
	BitwiseOr,
	BitwiseOrAssign,
	BitwiseXor,
	BitwiseXorAssign,

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
			BinaryOperator::AddAssign => "Addition assignment",
			BinaryOperator::Sub => "Subtraction",
			BinaryOperator::SubAssign => "Subtraction assignment",
			BinaryOperator::Mul => "Multiplication",
			BinaryOperator::MulAssign => "Multiplication assignment",
			BinaryOperator::Div => "Division",
			BinaryOperator::DivAssign => "Division assignment",
			BinaryOperator::Modulo => "Modulo",
			BinaryOperator::ModuloAssign => "Modulo assignment",

			BinaryOperator::BitshiftLeft => "Bitshift left",
			BinaryOperator::BitshiftLeftAssign => "Bitshift left assignment",
			BinaryOperator::BitshiftRight => "Bitshift right",
			BinaryOperator::BitshiftRightAssign => "Bitshift right assignment",

			BinaryOperator::BitwiseAnd => "Bitwise and",
			BinaryOperator::BitwiseAndAssign => "Bitwise and assignment",
			BinaryOperator::BitwiseOr => "Bitwise or",
			BinaryOperator::BitwiseOrAssign => "Bitwise or assignment",
			BinaryOperator::BitwiseXor => "Bitwise xor",
			BinaryOperator::BitwiseXorAssign => "Bitwise xor assignment",

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
			Assign | AddAssign | SubAssign | MulAssign | DivAssign | ModuloAssign | BitshiftLeftAssign | BitshiftRightAssign
			| BitwiseAndAssign | BitwiseOrAssign | BitwiseXorAssign => 0,

			LogicalAnd | LogicalOr => 1,

			Equals | NotEquals | GreaterThan | GreaterThanEquals | LessThan | LessThanEquals => 2,

			BitwiseAnd | BitwiseOr | BitwiseXor => 3,

			BitshiftLeft | BitshiftRight => 4,

			Add | Sub => 5,

			Mul | Div | Modulo => 6,
		}
	}

	pub fn associativity(self) -> Associativity {
		use BinaryOperator::*;
		match self {
			Assign | AddAssign | SubAssign | MulAssign | DivAssign | ModuloAssign | BitshiftLeftAssign | BitshiftRightAssign
			| BitwiseAndAssign | BitwiseOrAssign | BitwiseXorAssign => Associativity::Right,

			Add | Sub | Mul | Div | Modulo | BitshiftLeft | BitshiftRight | BitwiseAnd | BitwiseOr | BitwiseXor => {
				Associativity::Left
			}

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
pub struct CheckIs<'a> {
	pub left: Node<Expression<'a>>,
	pub binding_name: Option<Node<&'a str>>,
	pub variant_name: Node<&'a str>,
}

#[derive(Debug)]
pub struct Call<'a> {
	pub path_segments: Node<PathSegments<'a>>,
	pub type_arguments: Vec<Node<Type<'a>>>,
	pub arguments: Vec<Node<Expression<'a>>>,
}

#[derive(Debug)]
pub struct MethodCall<'a> {
	pub base: Node<Expression<'a>>,
	pub name: Node<&'a str>,
	pub type_arguments: Vec<Node<Type<'a>>>,
	pub arguments: Vec<Node<Expression<'a>>>,
}

#[derive(Debug)]
pub struct Read<'a> {
	pub path_segments: Node<PathSegments<'a>>,
	pub type_arguments: Vec<Node<Type<'a>>>,
}

#[derive(Debug)]
pub struct DotAccess<'a> {
	pub base: Node<Expression<'a>>,
	pub name: Node<&'a str>,
	pub struct_initializer: Option<Node<StructInitializer<'a>>>,
}

#[derive(Debug)]
pub struct Break;

#[derive(Debug)]
pub struct Continue;

#[derive(Debug)]
pub struct Return<'a> {
	pub expression: Option<Node<Expression<'a>>>,
}

#[derive(Debug)]
pub enum Statement<'a> {
	Expression(Node<Expression<'a>>),

	Block(Node<Block<'a>>),
	While(Node<While<'a>>),

	Import(Node<Import<'a>>),

	Struct(Struct<'a>),
	Enum(Enum<'a>),
	Function(Box<Function<'a>>),

	Const(Box<Node<Const<'a>>>),
	Static(Box<Node<Static<'a>>>),
	Binding(Box<Node<Binding<'a>>>),

	Break(Node<Break>),
	Continue(Node<Continue>),
	Return(Box<Node<Return<'a>>>),
}

impl<'a> Statement<'a> {
	pub fn span(&self) -> Span {
		use Statement::*;

		//TODO: Struct and Function could be improved
		match self {
			Expression(statement) => statement.span,
			Block(statement) => statement.span,
			While(statement) => statement.span,
			Import(statement) => statement.span,
			Struct(statement) => statement.name.span,
			Enum(statement) => statement.name.span,
			Function(statement) => statement.name.span,
			Const(statement) => statement.span,
			Static(statement) => statement.span,
			Binding(statement) => statement.span,
			Break(statement) => statement.span,
			Continue(statement) => statement.span,
			Return(statement) => statement.span,
		}
	}

	pub fn name_and_article(&self) -> &'static str {
		use Statement::*;

		match self {
			Expression(..) => "An expression",
			Block(..) => "A block",
			While(..) => "A while loop",
			Import(..) => "An import statement",
			Struct(..) => "A struct definition",
			Enum(..) => "An enum definition",
			Function(..) => "A function definition",
			Const(..) => "A const definition",
			Static(..) => "A static definition",
			Binding(..) => "A binding definition",
			Break(..) => "A break statement",
			Continue(..) => "A continue statement",
			Return(..) => "A return statement",
		}
	}
}

#[derive(Debug)]
pub struct Block<'a> {
	pub statements: Vec<Statement<'a>>,
}

#[derive(Debug)]
pub struct IfElseChainEntry<'a> {
	pub condition: Node<Expression<'a>>,
	pub body: Node<Block<'a>>,
}

#[derive(Debug)]
pub struct IfElseChain<'a> {
	pub entries: Vec<IfElseChainEntry<'a>>,
	pub else_body: Option<Node<Block<'a>>>,
}

#[derive(Debug)]
pub struct Match<'a> {
	pub expression: Node<Expression<'a>>,
	pub arms: Vec<MatchArm<'a>>,
}

#[derive(Debug)]
pub struct MatchArm<'a> {
	pub binding_name: Option<Node<&'a str>>,
	pub variant_name: Node<&'a str>,
	pub block: Node<Block<'a>>,
}

#[derive(Debug)]
pub struct While<'a> {
	pub condition: Node<Expression<'a>>,
	pub body: Node<Block<'a>>,
}

#[derive(Debug)]
pub enum Expression<'a> {
	Block(Block<'a>),
	IfElseChain(Box<IfElseChain<'a>>),
	Match(Box<Match<'a>>),

	IntegerLiteral(IntegerLiteral),
	FloatLiteral(FloatLiteral),

	BooleanLiteral(bool),
	CodepointLiteral(CodepointLiteral),
	ByteCodepointLiteral(ByteCodepointLiteral),
	StringLiteral(StringLiteral<'a>),

	ArrayLiteral(ArrayLiteral<'a>),
	StructLiteral(StructLiteral<'a>),

	Call(Call<'a>),
	MethodCall(Box<MethodCall<'a>>),
	Read(Read<'a>),
	DotAcccess(Box<DotAccess<'a>>),

	UnaryOperation(Box<UnaryOperation<'a>>),
	BinaryOperation(Box<BinaryOperation<'a>>),
	CheckIs(Box<CheckIs<'a>>),
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
