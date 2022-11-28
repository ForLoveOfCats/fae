use crate::tree;
use tree::Node;

#[derive(Debug, Clone)]
pub struct Import<'a> {
	pub segments: Vec<Node<&'a str>>,
}

#[derive(Debug, Copy, Clone)]
pub struct Symbol<'a> {
	pub name: &'a str,
	pub kind: SymbolKind,
}

#[derive(Debug, Copy, Clone)]
pub enum SymbolKind {
	Type { concrete_index: usize },
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
			if existing
				.arguments
				.iter()
				.zip(&arguments)
				.all(|(a, b)| a == b)
			{
				return index;
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
