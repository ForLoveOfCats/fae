use crate::span::Span;
use crate::tree::Node;

#[derive(Debug, Clone)]
pub struct Import<'a> {
	pub segments: Vec<Node<&'a str>>,
}

#[derive(Debug, Clone)]
pub struct Symbol<'a> {
	pub name: &'a str,
	pub kind: SymbolKind,
	pub span: Option<Span>,
	pub file_index: Option<usize>,
}

#[derive(Debug, Clone, Copy)]
pub enum SymbolKind {
	Type { concrete_index: usize },
	Function { shape_index: usize },
	Const { type_id: TypeId },
}

#[derive(Debug)]
pub enum GenericOrTypeId {
	Generic { index: usize },
	TypeId { id: TypeId },
}

#[derive(Debug)]
pub struct Type<'a> {
	pub name: String,
	pub kind: TypeKind<'a>,
	pub specialization: Vec<Specialization>,
}

impl<'a> Type<'a> {
	pub fn get_or_add_specialization(&mut self, arguments: Vec<TypeId>) -> usize {
		for (index, existing) in self.specialization.iter().enumerate() {
			if existing.arguments.len() == arguments.len() {
				let all_match = existing.arguments.iter().zip(&arguments).all(|(a, b)| a == b);

				if all_match {
					return index;
				}
			}
		}

		self.specialization.push(Specialization { arguments });
		self.specialization.len() - 1
	}
}

#[derive(Debug)]
pub struct Specialization {
	arguments: Vec<TypeId>,
}

#[derive(Debug)]
pub enum TypeKind<'a> {
	Primative,
	Reference,
	Struct { fields: Vec<Field<'a>> },
}

#[derive(Debug)]
pub struct Field<'a> {
	pub name: &'a str,
	pub type_id: TypeId,
}

#[derive(Debug)]
pub struct FunctionShape<'a> {
	pub name: &'a str, //Purely for debugging purposes
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
	pub concrete_index: usize,
	pub specialization_index: usize,
}
