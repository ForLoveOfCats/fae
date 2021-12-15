use crate::location::SourceLocation;
use crate::tokenizer::Token;

#[derive(Debug)]
pub enum NodeKind<'a> {
	//Followed by one or more PathSegment
	Use,

	PathSegment { text: &'a str },

	//Followed by zero or more Parameters then one or more PathSegment then a StartExpression
	Function { name: &'a str },

	//Followed by one or more PathSegment
	Parameter { name: &'a str },

	//Followed by zero or more PathSegment then a StartExpression
	Const { name: &'a str },

	//Followed by zero or more PathSegment then a StartExpression
	Let { name: &'a str },

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
pub struct Tree<'a> {
	nodes: Vec<Node<'a>>,
}

impl<'a> Tree<'a> {
	pub fn new() -> Tree<'a> {
		Tree { nodes: Vec::new() }
	}

	pub fn push(&mut self, node: Node<'a>) {
		self.nodes.push(node);
	}
}
