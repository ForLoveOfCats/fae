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

#[derive(Debug, Clone)]
pub enum SymbolKind {
	Type { concrete_index: usize },
	Function { parameters: Vec<TypeId> },
	Const { type_id: TypeId },
}

#[derive(Debug)]
pub struct Type<'a> {
	pub name: String,
	pub kind: TypeKind<'a>,
	pub specialization: Vec<Specialization>,
}

impl<'a> Type<'a> {
	pub fn get_or_add_specialization(&mut self, arguments: Vec<TypeId>) -> usize {
		assert!(arguments.len() > 0);

		for (index, existing) in self.specialization.iter().enumerate() {
			if existing.arguments.len() == arguments.len() {
				let all_match = existing
					.arguments
					.iter()
					.zip(&arguments)
					.all(|(a, b)| a == b);

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
	name: &'a str,
	type_id: TypeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId {
	pub concrete_index: usize,
	pub specialization_index: usize,
}

impl TypeId {
	pub fn invalid() -> TypeId {
		TypeId {
			concrete_index: usize::MAX,
			specialization_index: usize::MAX,
		}
	}
}
