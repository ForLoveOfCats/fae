use std::borrow::Cow;
use std::hash::Hash;

use rust_decimal::Decimal;

use crate::frontend::file::SourceFile;
use crate::frontend::span::Span;
use crate::frontend::tokenizer::Token;

#[derive(Debug)]
pub struct File<'a> {
	pub source_file: &'a SourceFile,
	pub line_starts: Vec<usize>, // zero indexed line number -> line's starting byte offset
	pub module_path: &'a [String],
	pub block: Block<'a>,
}

#[derive(Debug)]
pub struct PathSegments<'a> {
	pub segments: &'a [Node<&'a str>],
}

#[derive(Debug)]
pub struct Import<'a> {
	pub path_segments: PathSegments<'a>,
	pub symbol_names: &'a [Node<&'a str>],
}

#[derive(Debug)]
pub struct GenericAttribute<'a> {
	pub names: &'a [GenericName<'a>],
}

#[derive(Debug)]
pub struct GenericName<'a> {
	pub name: Node<&'a str>,
	pub constraints: &'a [Node<GenericConstraint<'a>>],
}

#[derive(Debug)]
pub struct GenericConstraint<'a> {
	pub path: PathSegments<'a>,
	pub type_arguments: &'a [Node<Type<'a>>],
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

impl MethodKind {
	pub fn name(self) -> &'static str {
		match self {
			MethodKind::Static => "static",
			MethodKind::ImmutableSelf => "immutable",
			MethodKind::MutableSelf => "mutable",
		}
	}
}

#[derive(Debug)]
pub struct MethodAttribute<'a> {
	pub base_type: Node<PathSegments<'a>>,
	pub kind: Node<MethodKind>,
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
	pub generic_attribute: Option<&'a Node<GenericAttribute<'a>>>,
	pub extern_attribute: Option<&'a Node<ExternAttribute<'a>>>,
	pub export_attribute: Option<&'a Node<ExportAttribute<'a>>>,
	pub method_attribute: Option<&'a Node<MethodAttribute<'a>>>,
	pub intrinsic_attribute: Option<&'a Node<IntrinsicAttribute>>,
	pub lang_attribute: Option<&'a Node<LangAttribute<'a>>>,
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
		fn push_potential_span<T>(attribute: Option<&Node<T>>, buffer: &mut [Span], index: &mut usize) {
			if let Some(attribute) = attribute {
				buffer[*index] = attribute.span;
				*index += 1;
			}
		}

		let mut index = 0;
		push_potential_span(self.generic_attribute, buffer, &mut index);
		push_potential_span(self.extern_attribute, buffer, &mut index);
		push_potential_span(self.export_attribute, buffer, &mut index);
		push_potential_span(self.method_attribute, buffer, &mut index);
		push_potential_span(self.intrinsic_attribute, buffer, &mut index);
		push_potential_span(self.lang_attribute, buffer, &mut index);

		&buffer[0..index]
	}
}

#[derive(Debug)]
pub enum Type<'a> {
	Void,

	Pointer {
		pointee: &'a Node<Type<'a>>,
		mutable: bool,
	},

	Slice {
		pointee: &'a Node<Type<'a>>,
		mutable: bool,
	},

	Path {
		path_segments: Node<PathSegments<'a>>,
		type_arguments: &'a [Node<Type<'a>>],
		dot_access_chain: &'a [Node<&'a str>],
	},
}

#[derive(Debug)]
pub struct Struct<'a> {
	pub lang_attribute: Option<&'a Node<LangAttribute<'a>>>,
	pub generics: &'a [GenericName<'a>],
	pub name: Node<&'a str>,
	pub fields: &'a [Field<'a>],
}

#[derive(Debug)]
pub struct Enum<'a> {
	pub lang_attribute: Option<&'a Node<LangAttribute<'a>>>,
	pub generics: &'a [GenericName<'a>],
	pub name: Node<&'a str>,
	pub shared_fields: &'a [Field<'a>],
	pub variants: &'a [EnumVariant<'a>],
}

#[derive(Debug)]
pub enum EnumVariant<'a> {
	StructLike(StructLikeVariant<'a>),
	Transparent(TransparentVariant<'a>),
}

#[derive(Debug)]
pub struct StructLikeVariant<'a> {
	pub name: Node<&'a str>,
	pub fields: &'a [Field<'a>],
}

#[derive(Debug)]
pub struct TransparentVariant<'a> {
	pub name: Node<&'a str>,
	pub parsed_type: Node<Type<'a>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FieldAttribute {
	Internal,
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
pub struct Trait<'a> {
	pub generics: &'a [GenericName<'a>],
	pub name: Node<&'a str>,
	pub methods: &'a [TraitMethod<'a>],
}

#[derive(Debug)]
pub struct TraitMethod<'a> {
	pub kind: Node<MethodKind>,
	pub name: Node<&'a str>,
	pub parameters: Node<Parameters<'a>>,
	pub parsed_type: Option<Node<Type<'a>>>,
}

#[derive(Debug)]
pub struct Function<'a> {
	pub generics: &'a [GenericName<'a>],
	pub extern_attribute: Option<&'a Node<ExternAttribute<'a>>>,
	pub export_attribute: Option<&'a Node<ExportAttribute<'a>>>,
	pub method_attribute: Option<&'a Node<MethodAttribute<'a>>>,
	pub intrinsic_attribute: Option<&'a Node<IntrinsicAttribute>>,
	pub lang_attribute: Option<&'a Node<LangAttribute<'a>>>,
	pub name: Node<&'a str>,
	pub parameters: Node<Parameters<'a>>,
	pub parsed_type: Option<Node<Type<'a>>>,
	pub block: Option<Node<Block<'a>>>,
}

#[derive(Debug)]
pub struct Parameters<'a> {
	pub parameters: &'a [Node<Parameter<'a>>],
	pub c_varargs: Option<Span>,
}

#[derive(Debug)]
pub struct Parameter<'a> {
	pub name: Node<&'a str>,
	pub label: Option<&'a str>,
	pub parsed_type: Node<Type<'a>>,
	pub is_mutable: bool,
}

#[derive(Debug)]
pub struct Const<'a> {
	pub name: Node<&'a str>,
	pub parsed_type: Option<&'a Node<Type<'a>>>,
	pub expression: Node<Expression<'a>>,
}

#[derive(Debug)]
pub struct Static<'a> {
	pub name: Node<&'a str>,
	pub parsed_type: Node<Type<'a>>,
	pub extern_attribute: Option<&'a Node<ExternAttribute<'a>>>,
}

#[derive(Debug)]
pub struct Binding<'a> {
	pub name: Node<&'a str>,
	pub parsed_type: Option<&'a Node<Type<'a>>>,
	pub expression: Node<Expression<'a>>,
	pub is_mutable: bool,
}

#[derive(Debug)]
pub struct NumberLiteral {
	pub value: Node<Decimal>,
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
	pub value: Cow<'a, str>,
}

#[derive(Debug)]
pub enum FormatStringItem<'a> {
	Text(Cow<'a, str>),
	Expression(Node<Expression<'a>>),
}

#[derive(Debug)]
pub struct FormatStringLiteral<'a> {
	pub items: &'a [FormatStringItem<'a>],
}

#[derive(Debug)]
pub struct ArrayLiteral<'a> {
	pub parsed_type: Option<Node<Type<'a>>>,
	pub expressions: &'a [Node<Expression<'a>>],
}

#[derive(Debug)]
pub struct StructLiteral<'a> {
	pub base: Node<Expression<'a>>,
	pub initializer: Node<StructInitializer<'a>>,
}

#[derive(Debug)]
pub struct StructInitializer<'a> {
	pub field_initializers: &'a [FieldInitializer<'a>],
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
	LogicalIsAnd,
	LogicalOr,

	Range,
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

			BinaryOperator::LogicalAnd | Self::LogicalIsAnd => "Logical And",
			BinaryOperator::LogicalOr => "Logical Or",

			BinaryOperator::Range => "Range",
		}
	}
}

impl BinaryOperator {
	pub fn precedence(self) -> u32 {
		use BinaryOperator::*;
		match self {
			Assign | AddAssign | SubAssign | MulAssign | DivAssign | ModuloAssign | BitshiftLeftAssign | BitshiftRightAssign
			| BitwiseAndAssign | BitwiseOrAssign | BitwiseXorAssign => 0,

			Range => 1,

			LogicalAnd | LogicalIsAnd | LogicalOr => 2,

			Equals | NotEquals | GreaterThan | GreaterThanEquals | LessThan | LessThanEquals => 3,

			BitwiseAnd | BitwiseOr | BitwiseXor => 4,

			BitshiftLeft | BitshiftRight => 5,

			Add | Sub => 6,

			Mul | Div | Modulo => 7,
		}
	}

	pub fn associativity(self) -> Associativity {
		use BinaryOperator::*;
		match self {
			Assign | AddAssign | SubAssign | MulAssign | DivAssign | ModuloAssign | BitshiftLeftAssign | BitshiftRightAssign
			| BitwiseAndAssign | BitwiseOrAssign | BitwiseXorAssign => Associativity::Right,

			Range => Associativity::Left,

			Add | Sub | Mul | Div | Modulo | BitshiftLeft | BitshiftRight | BitwiseAnd | BitwiseOr | BitwiseXor => {
				Associativity::Left
			}

			Equals | NotEquals | GreaterThan | GreaterThanEquals | LessThan | LessThanEquals => Associativity::Left,

			LogicalAnd | LogicalIsAnd | LogicalOr => Associativity::Left,
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
	pub variant_names: &'a [Node<&'a str>],
}

#[derive(Debug)]
pub struct Call<'a> {
	pub base: Option<Node<Expression<'a>>>,
	pub name: Node<&'a str>,
	pub type_arguments: &'a [Node<Type<'a>>],
	pub arguments: &'a [Argument<'a>],
}

#[derive(Debug)]
pub struct Argument<'a> {
	pub label: Option<Node<&'a str>>,
	pub expression: Node<Expression<'a>>,
}

#[derive(Debug)]
pub struct Read<'a> {
	pub name: Node<&'a str>,
	pub type_arguments: &'a [Node<Type<'a>>],
}

#[derive(Debug)]
pub struct DotAccess<'a> {
	pub base: Node<Expression<'a>>,
	pub name: Node<&'a str>,
	pub type_arguments: &'a [Node<Type<'a>>],
}

#[derive(Debug)]
pub struct DotInfer<'a> {
	pub name: Node<&'a str>,
}

#[derive(Debug)]
pub struct DotInferCall<'a> {
	pub name: Node<&'a str>,
	pub arguments: &'a [Argument<'a>],
}

#[derive(Debug)]
pub struct Defer<'a> {
	pub statement: Statement<'a>,
}

#[derive(Debug)]
pub struct Break;

#[derive(Debug)]
pub struct Continue;

#[derive(Debug)]
pub struct Yield<'a> {
	pub expression: Node<Expression<'a>>,
}

#[derive(Debug)]
pub struct Return<'a> {
	pub expression: Option<Node<Expression<'a>>>,
}

#[derive(Debug)]
pub enum Statement<'a> {
	Expression(Node<Expression<'a>>),

	Block(Node<Block<'a>>),
	WhenElseChain(Node<WhenElseChain<'a>>),
	IfElseChain(Node<IfElseChain<'a>>),
	Match(Node<Match<'a>>),
	While(Node<While<'a>>),
	For(Node<For<'a>>),

	Import(Node<Import<'a>>),

	Struct(Struct<'a>),
	Enum(Enum<'a>),
	Trait(Trait<'a>),
	Function(&'a Function<'a>),

	Const(&'a Node<Const<'a>>),
	Static(&'a Node<Static<'a>>),
	Binding(&'a Node<Binding<'a>>),

	Defer(&'a Node<Defer<'a>>),

	Break(Node<Break>),
	Continue(Node<Continue>),
	Yield(&'a Node<Yield<'a>>),
	Return(&'a Node<Return<'a>>),
}

impl<'a> Statement<'a> {
	pub fn span(&self) -> Span {
		use Statement::*;

		//TODO: Struct and Function could be improved
		match self {
			Expression(statement) => statement.span,
			Block(statement) => statement.span,
			IfElseChain(statement) => statement.span,
			Match(statement) => statement.span,
			WhenElseChain(statement) => statement.span,
			While(statement) => statement.span,
			For(statement) => statement.span,
			Import(statement) => statement.span,
			Struct(statement) => statement.name.span,
			Enum(statement) => statement.name.span,
			Trait(statement) => statement.name.span,
			Function(statement) => statement.name.span,
			Const(statement) => statement.span,
			Static(statement) => statement.span,
			Binding(statement) => statement.span,
			Defer(statement) => statement.span,
			Break(statement) => statement.span,
			Continue(statement) => statement.span,
			Yield(statement) => statement.span,
			Return(statement) => statement.span,
		}
	}

	pub fn name_and_article(&self) -> &'static str {
		use Statement::*;

		match self {
			Expression(..) => "An expression",
			Block(..) => "A block",
			IfElseChain(..) => "An if-else statement",
			Match(..) => "A match statement",
			While(..) => "A while loop",
			For(..) => "A for loop",
			WhenElseChain(..) => "A when statement",
			Import(..) => "An import statement",
			Struct(..) => "A struct definition",
			Enum(..) => "An enum definition",
			Trait(..) => "A trait definition",
			Function(..) => "A function definition",
			Const(..) => "A const definition",
			Static(..) => "A static definition",
			Binding(..) => "A binding definition",
			Defer(..) => "A defer statement",
			Break(..) => "A break statement",
			Continue(..) => "A continue statement",
			Yield(..) => "A yield statement",
			Return(..) => "A return statement",
		}
	}
}

#[derive(Debug)]
pub struct Block<'a> {
	pub statements: &'a [Statement<'a>],
}

#[derive(Debug)]
pub struct WhenElseChain<'a> {
	pub entries: &'a [WhenElseChainEntry<'a>],
	pub else_body: Option<Node<Block<'a>>>,
}

#[derive(Debug)]
pub struct WhenElseChainEntry<'a> {
	pub condition: Node<&'a str>,
	pub body: Node<Block<'a>>,
}

#[derive(Debug)]
pub struct IfElseChain<'a> {
	pub entries: &'a [IfElseChainEntry<'a>],
	pub else_body: Option<Node<Block<'a>>>,
}

#[derive(Debug)]
pub struct IfElseChainEntry<'a> {
	pub condition: Node<Expression<'a>>,
	pub body: Node<Block<'a>>,
}

#[derive(Debug)]
pub struct Match<'a> {
	pub expression: Node<Expression<'a>>,
	pub arms: &'a [MatchArm<'a>],
	pub else_arm: Option<ElseArm<'a>>,
}

#[derive(Debug)]
pub struct MatchArm<'a> {
	pub binding_name: Option<Node<&'a str>>,
	pub variant_names: &'a [Node<&'a str>],
	pub block: Node<Block<'a>>,
}

#[derive(Debug)]
pub struct ElseArm<'a> {
	pub block: Node<Block<'a>>,
	pub else_span: Span,
}

#[derive(Debug)]
pub struct While<'a> {
	pub condition: Node<Expression<'a>>,
	pub body: Node<Block<'a>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IterationKind {
	In,
	Of,
}

// TODO: Store much of this out-of-band
#[derive(Debug)]
pub struct For<'a> {
	pub item: Node<&'a str>,
	pub index: Option<Node<&'a str>>,
	pub is_last: Option<Node<&'a str>>,
	pub iteration_kind: Node<IterationKind>,
	pub initializer: Node<Expression<'a>>,
	pub body: Node<Block<'a>>,
}

#[derive(Debug)]
pub enum Expression<'a> {
	Block(Block<'a>),
	IfElseChain(&'a IfElseChain<'a>),
	Match(&'a Match<'a>),

	NumberLiteral(NumberLiteral),

	BooleanLiteral(bool),
	CodepointLiteral(CodepointLiteral),
	ByteCodepointLiteral(ByteCodepointLiteral),
	StringLiteral(StringLiteral<'a>),
	FormatStringLiteral(FormatStringLiteral<'a>),

	ArrayLiteral(ArrayLiteral<'a>),
	StructLiteral(&'a StructLiteral<'a>),

	Call(&'a Call<'a>),
	Read(Read<'a>),
	DotAccess(&'a DotAccess<'a>),
	DotInfer(&'a DotInfer<'a>),
	DotInferCall(&'a DotInferCall<'a>),

	UnaryOperation(&'a UnaryOperation<'a>),
	BinaryOperation(&'a BinaryOperation<'a>),
	CheckIs(&'a CheckIs<'a>),
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

// This could be a potential footgun 😅
impl<T: Hash> Hash for Node<T> {
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
		self.item.hash(state);
	}
}
