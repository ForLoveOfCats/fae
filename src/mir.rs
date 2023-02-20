use crate::error::Messages;
use crate::span::Span;
use crate::tree::Node;

#[derive(Debug, Clone)]
pub struct Import<'a> {
	pub segments: Vec<Node<&'a str>>,
}

#[derive(Debug, Copy, Clone)]
pub struct Symbol<'a> {
	pub name: &'a str,
	pub kind: SymbolKind,
	pub span: Option<Span>,
	pub file_index: Option<usize>,
}

#[derive(Debug, Clone, Copy)]
pub enum SymbolKind {
	BuiltinType { type_index: usize }, //Not used for slice/reference as those are not symbols
	Type { type_index: usize },
	Function { shape_index: usize },
}

#[derive(Debug)]
pub enum GenericOrTypeId {
	TypeId { id: TypeId },
	Generic { index: usize },
}

#[derive(Debug)]
pub struct UserType<'a> {
	pub span: Span,
	pub kind: UserTypeKind<'a>,
}

#[derive(Debug)]
pub enum UserTypeKind<'a> {
	Struct { shape: StructShape<'a> },
}

#[derive(Debug)]
pub struct PrimativeType {
	pub name: &'static str,
	pub kind: PrimativeKind,
	pub type_id: TypeId,
}

#[derive(Debug)]
pub enum PrimativeKind {
	Void,

	I8,
	I16,
	I32,
	I64,

	U8,
	U16,
	U32,
	U64,

	F16,
	F32,
	F64,
}

#[derive(Debug)]
pub struct StructShape<'a> {
	pub name: &'a str,
	pub generics: Vec<Node<&'a str>>,

	pub fields: Vec<Node<FieldShape<'a>>>,

	pub concrete: Vec<Struct<'a>>,
}

impl<'a> StructShape<'a> {
	pub fn get_or_add_specialization(
		&mut self,
		messages: &mut Messages,
		invoke_span: Span,
		arguments: Vec<TypeId>,
	) -> Option<usize> {
		for (index, existing) in self.concrete.iter().enumerate() {
			if existing.type_arguments.len() == arguments.len() {
				if existing.type_arguments.iter().zip(&arguments).all(|(a, b)| a == b) {
					return Some(index);
				}
			}
		}

		if self.generics.len() != arguments.len() {
			messages.error(
				message!("Expected {} type arguments, got {}", self.generics.len(), arguments.len()).span(invoke_span),
			);
			return None;
		}

		let fields = self
			.fields
			.iter()
			.map(|field| {
				let type_id = match field.item.field_type {
					GenericOrTypeId::TypeId { id } => id,
					GenericOrTypeId::Generic { index } => arguments[index],
				};

				Field { name: field.item.name, type_id }
			})
			.collect::<Vec<_>>();

		let concrete = Struct { type_arguments: arguments, fields };
		self.concrete.push(concrete);
		Some(self.concrete.len() - 1)
	}
}

#[derive(Debug)]
pub struct FieldShape<'a> {
	pub name: &'a str,
	pub field_type: GenericOrTypeId,
}

#[derive(Debug)]
pub struct Struct<'a> {
	pub type_arguments: Vec<TypeId>,
	pub fields: Vec<Field<'a>>,
}

#[derive(Debug)]
pub struct Field<'a> {
	pub name: &'a str,
	pub type_id: TypeId,
}

#[derive(Debug)]
pub struct FunctionShape<'a> {
	pub name: &'a str,
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
	pub index: usize,
	pub specialization: usize,
}
