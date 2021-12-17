use crate::location::SourceLocation;
use crate::tokenizer::Token;

#[derive(Debug)]
pub enum NodeKind<'a> {
	//Followed by one or more PathSegment
	Module,

	//Followed by one or more PathSegment
	Use,

	PathSegment { text: &'a str },

	//Followed by zero or more Parameters then one or more PathSegment then a StartExpression
	Function { name: &'a str },

	//Followed by one or more PathSegment
	Parameter { name: &'a str },

	//Followed by zero ore more Field
	Struct { name: &'a str },

	//Followed by one or more PathSegment
	Field { name: &'a str },

	//Followed by zero or more PathSegment then a StartExpression
	Const { name: &'a str },

	//Followed by zero or more PathSegment then a StartExpression
	Let { name: &'a str },

	//Followed by a StartExpression
	Return,

	//Expressed in "normal" infix notation, precedence handled in semantic pass
	StartExpression,
	EndExpression,

	//Follows one or more PathSegment, followed by zero or more StartExpression then an EndCallArgs
	Call,
	EndCallArgs,

	UnsignedIntegerLiteral { value: u64 },
	SignedIntegerLiteral { value: i64 },
	FloatLiteral { value: f64 },

	StringLiteral { value: &'a str },
	CharLiteral { value: char },

	Add,
	Sub,
	Mul,
	Div,
}

#[derive(Debug)]
pub struct Node<'a> {
	pub kind: NodeKind<'a>,
	pub location: Option<SourceLocation>,
}

impl<'a> Node<'a> {
	pub fn new(kind: NodeKind<'a>, location: SourceLocation) -> Node<'a> {
		Node {
			kind,
			location: Some(location),
		}
	}

	pub fn from_token(kind: NodeKind<'a>, token: Token) -> Node<'a> {
		Node {
			kind,
			location: Some(token.location),
		}
	}

	pub fn without_location(kind: NodeKind<'a>) -> Node<'a> {
		Node {
			kind,
			location: None,
		}
	}
}

#[derive(Debug)]
pub enum UnorderedItem {
	Function { index: usize },
	Struct { index: usize },
}

#[derive(Debug)]
pub struct Tree<'a> {
	nodes: Vec<Node<'a>>,
	unordered_items: Vec<UnorderedItem>,
}

impl<'a> Tree<'a> {
	pub fn new() -> Tree<'a> {
		Tree {
			nodes: Vec::new(),
			unordered_items: Vec::new(),
		}
	}

	pub fn push(&mut self, node: Node<'a>) {
		match node.kind {
			NodeKind::Function { .. } => self.unordered_items.push(UnorderedItem::Function {
				index: self.nodes.len(),
			}),

			NodeKind::Struct { .. } => self.unordered_items.push(UnorderedItem::Struct {
				index: self.nodes.len(),
			}),

			_ => {}
		}

		self.nodes.push(node);
	}

	pub fn len(&self) -> usize {
		self.nodes.len()
	}
}
