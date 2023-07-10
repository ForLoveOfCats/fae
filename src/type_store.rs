use crate::error::Messages;
use crate::ir::{Symbol, SymbolKind};
use crate::span::Span;
use crate::tree::{self, Node};
use crate::validator::{RootLayers, Symbols};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeId {
	entry: u32,
}

impl TypeId {
	pub fn index(self) -> usize {
		self.entry as usize
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum GenericOrTypeId {
	TypeId { id: TypeId },
	Generic { index: usize },
}

#[derive(Debug)]
pub struct StructShape<'a> {
	pub name: &'a str,
	pub generics: Vec<Node<&'a str>>,

	pub fields: Vec<Node<FieldShape<'a>>>,

	pub specializations: Vec<Struct<'a>>,
}

impl<'a> StructShape<'a> {
	pub fn new(name: &'a str, generics: Vec<Node<&'a str>>) -> Self {
		StructShape {
			name,
			generics,
			fields: Vec::new(),
			specializations: Vec::new(),
		}
	}

	pub fn get_or_add_specialization(
		&mut self,
		messages: &mut Messages,
		type_entries: &mut Vec<TypeEntry>,
		shape_index: usize,
		invoke_span: Span,
		type_arguments: Vec<TypeId>,
	) -> Option<TypeId> {
		if type_arguments.len() != self.generics.len() {
			messages.error(
				message!("Expected {} type arguments, got {}", self.generics.len(), type_arguments.len())
					.span(invoke_span),
			);
			return None;
		}

		for existing in &self.specializations {
			if existing.type_arguments == type_arguments {
				return Some(existing.type_id);
			}
		}

		let mut fields = Vec::with_capacity(self.fields.len());
		for (field_index, field) in self.fields.iter().enumerate() {
			let type_id = match field.item.field_type {
				GenericOrTypeId::TypeId { id } => id,
				GenericOrTypeId::Generic { index } => type_arguments[index],
			};
			fields.push(Field { name: field.item.name, type_id, field_index });
		}

		let specialization_index = self.specializations.len();
		let type_id = TypeId { entry: type_entries.len() as u32 };
		let entry = TypeEntry::new(TypeEntryKind::UserType { shape_index, specialization_index });
		type_entries.push(entry);

		self.specializations.push(Struct { type_id, type_arguments, fields });
		Some(type_id)
	}
}

#[derive(Debug)]
pub struct FieldShape<'a> {
	pub name: &'a str,
	pub field_type: GenericOrTypeId,
}

#[derive(Debug)]
pub struct Struct<'a> {
	pub type_id: TypeId,
	pub type_arguments: Vec<TypeId>,
	pub fields: Vec<Field<'a>>,
}

#[derive(Debug, Clone, Copy)]
pub struct Field<'a> {
	pub name: &'a str,
	pub type_id: TypeId,
	pub field_index: usize,
}

#[derive(Debug)]
pub struct UserType<'a> {
	pub span: Span,
	pub module_path: &'a [String],
	pub kind: UserTypeKind<'a>,
}

#[derive(Debug)]
pub enum UserTypeKind<'a> {
	Struct { shape: StructShape<'a> },
}

#[derive(Debug, Clone, Copy, Hash)]
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

#[derive(Debug, Clone, Copy, Hash)]
pub struct TypeEntry {
	pub kind: TypeEntryKind,
	pub reference_entries: Option<u32>,
}

impl TypeEntry {
	fn new(kind: TypeEntryKind) -> TypeEntry {
		TypeEntry { kind, reference_entries: None }
	}
}

#[derive(Debug, Clone, Copy, Hash)]
pub enum TypeEntryKind {
	BuiltinType { kind: PrimativeKind },
	UserType { shape_index: usize, specialization_index: usize },
	Pointer { mutable: bool, type_id: TypeId },
	Slice { mutable: bool, type_id: TypeId },
	UserTypeGeneric { shape_index: usize, generic_index: usize },
	FunctionGeneric { function_shape_index: usize, generic_index: usize },
}

#[derive(Debug)]
pub struct TypeStore<'a> {
	pub primative_type_symbols: Vec<Symbol<'a>>,

	pub type_entries: Vec<TypeEntry>,
	pub user_types: Vec<UserType<'a>>,

	void_type_id: TypeId,
	u32_type_id: TypeId,
	u64_type_id: TypeId,
	f64_type_id: TypeId,
	string_type_id: TypeId,
}

impl<'a> TypeStore<'a> {
	pub fn new() -> Self {
		let mut primative_type_symbols = Vec::new();
		let mut type_entries = Vec::new();

		let mut push_primative = |name, kind| {
			let type_id = TypeId { entry: type_entries.len() as u32 };
			let kind = TypeEntryKind::BuiltinType { kind };
			type_entries.push(TypeEntry { kind, reference_entries: None });

			let kind = SymbolKind::BuiltinType { type_id };
			let symbol = Symbol { name, kind, span: None, file_index: None };
			primative_type_symbols.push(symbol);

			type_id
		};

		let void_type_id = push_primative("void", PrimativeKind::Void);

		push_primative("i8", PrimativeKind::I8);
		push_primative("i16", PrimativeKind::I16);
		push_primative("i32", PrimativeKind::I32);
		push_primative("i64", PrimativeKind::I64);

		let u8_type_id = push_primative("u8", PrimativeKind::U8);
		push_primative("u16", PrimativeKind::U16);
		let u32_type_id = push_primative("u32", PrimativeKind::U32);
		let u64_type_id = push_primative("u64", PrimativeKind::U64);

		push_primative("f16", PrimativeKind::F16);
		push_primative("f32", PrimativeKind::F32);
		let f64_type_id = push_primative("f64", PrimativeKind::F64);

		let mut type_store = TypeStore {
			primative_type_symbols,
			type_entries,
			user_types: Vec::new(),
			void_type_id,
			u32_type_id,
			u64_type_id,
			f64_type_id,
			string_type_id: TypeId { entry: 0 },
		};

		// This is a hack, consider moving the saved type ids into a different struct
		let string_type_id = type_store.slice_of(u8_type_id, false);
		type_store.string_type_id = string_type_id;

		type_store
	}

	pub fn void_type_id(&self) -> TypeId {
		self.void_type_id
	}

	pub fn u32_type_id(&self) -> TypeId {
		self.u32_type_id
	}

	pub fn u64_type_id(&self) -> TypeId {
		self.u64_type_id
	}

	pub fn f64_type_id(&self) -> TypeId {
		self.f64_type_id
	}

	pub fn string_type_id(&self) -> TypeId {
		self.string_type_id
	}

	fn get_or_create_reference_entries(&mut self, type_id: TypeId) -> u32 {
		let entry = &self.type_entries[type_id.index()];
		if let Some(entries) = entry.reference_entries {
			return entries;
		}

		// Order of reference entries:
		// - immutable pointer
		// - mutable pointer
		// - immutable slice
		// - mutable slice

		let entries = self.type_entries.len() as u32;

		let kind = TypeEntryKind::Pointer { mutable: false, type_id };
		self.type_entries.push(TypeEntry::new(kind));

		let kind = TypeEntryKind::Pointer { mutable: true, type_id };
		self.type_entries.push(TypeEntry::new(kind));

		let kind = TypeEntryKind::Slice { mutable: false, type_id };
		self.type_entries.push(TypeEntry::new(kind));

		let kind = TypeEntryKind::Slice { mutable: true, type_id };
		self.type_entries.push(TypeEntry::new(kind));

		let entry = &mut self.type_entries[type_id.index()];
		entry.reference_entries = Some(entries);
		entries
	}

	pub fn pointer_to(&mut self, type_id: TypeId, mutable: bool) -> TypeId {
		let reference_entries = self.get_or_create_reference_entries(type_id);
		let entry = match mutable {
			false => reference_entries,
			true => reference_entries + 1,
		};
		TypeId { entry }
	}

	pub fn slice_of(&mut self, type_id: TypeId, mutable: bool) -> TypeId {
		let reference_entries = self.get_or_create_reference_entries(type_id);
		let entry = match mutable {
			false => reference_entries + 2,
			true => reference_entries + 3,
		};
		TypeId { entry }
	}

	pub fn register_type(
		&mut self,
		name: &'a str,
		kind: UserTypeKind<'a>,
		span: Span,
		file_index: Option<usize>,
		module_path: &'a [String],
	) -> Symbol<'a> {
		// Type entry gets added during specialization
		let shape_index = self.user_types.len();
		self.user_types.push(UserType { span, module_path, kind });

		let kind = SymbolKind::Type { shape_index };
		Symbol { name, kind, span: Some(span), file_index }
	}

	pub fn register_function_generic(
		&mut self,
		name: &'a str,
		span: Span,
		function_shape_index: usize,
		generic_index: usize,
		file_index: usize,
	) -> Symbol<'a> {
		let kind = TypeEntryKind::FunctionGeneric { function_shape_index, generic_index };
		self.type_entries.push(TypeEntry::new(kind));
		let kind = SymbolKind::Generic { function_shape_index, generic_index };
		Symbol { name, kind, span: Some(span), file_index: Some(file_index) }
	}

	pub fn lookup_type(
		&mut self,
		messages: &mut Messages,
		root_layers: &RootLayers<'a>,
		symbols: &Symbols<'a>,
		parsed_type: &Node<tree::Type<'a>>,
	) -> Option<TypeId> {
		let (path_segments, type_arguments) = match &parsed_type.item {
			tree::Type::Void => return Some(self.void_type_id),

			tree::Type::Reference(inner) => {
				let inner_id = self.lookup_type(messages, root_layers, symbols, &inner)?;
				return Some(self.pointer_to(inner_id, false)); // TODO: Parse mutability
			}

			tree::Type::Slice(inner) => {
				let inner_id = self.lookup_type(messages, root_layers, symbols, &inner)?;
				return Some(self.slice_of(inner_id, false)); // TODO: Parse mutability
			}

			tree::Type::Path { path_segments, type_arguments } => (path_segments, type_arguments),
		};

		assert!(!path_segments.item.segments.is_empty());
		let symbol = symbols.lookup_symbol(messages, root_layers, self, &path_segments.item)?;

		let shape_index = match symbol.kind {
			SymbolKind::BuiltinType { type_id } => {
				if !type_arguments.is_empty() {
					messages.error(message!("Builtin types do not accept type arguments").span(parsed_type.span));
					return None;
				}

				return Some(type_id);
			}

			SymbolKind::Type { shape_index } => shape_index,

			_ => {
				messages.error(message!("Symbol {:?} is not a type", symbol.name).span(path_segments.span));
				return None;
			}
		};

		let mut type_args = Vec::with_capacity(type_arguments.len());
		for argument in type_arguments {
			type_args.push(self.lookup_type(messages, root_layers, symbols, &argument)?);
		}

		let user_type = &mut self.user_types[shape_index];
		let type_entries = &mut self.type_entries;
		let type_id = match &mut user_type.kind {
			UserTypeKind::Struct { shape } => {
				shape.get_or_add_specialization(messages, type_entries, shape_index, user_type.span, type_args)?
			}
		};

		Some(type_id)
	}

	pub fn type_name(&self, module_path: &'a [String], type_id: TypeId) -> String {
		format!("`{}`", self.internal_type_name(module_path, type_id))
	}

	fn internal_type_name(&self, module_path: &'a [String], type_id: TypeId) -> String {
		match self.type_entries[type_id.index()].kind {
			TypeEntryKind::BuiltinType { kind } => match kind {
				PrimativeKind::Void => "void",
				PrimativeKind::I8 => "i8",
				PrimativeKind::I16 => "i16",
				PrimativeKind::I32 => "i32",
				PrimativeKind::I64 => "i64",
				PrimativeKind::U8 => "u8",
				PrimativeKind::U16 => "u16",
				PrimativeKind::U32 => "u32",
				PrimativeKind::U64 => "u64",
				PrimativeKind::F16 => "f16",
				PrimativeKind::F32 => "f32",
				PrimativeKind::F64 => "f64",
			}
			.to_owned(),

			TypeEntryKind::UserType { shape_index, specialization_index: _ } => {
				let shape = &self.user_types[shape_index];
				match &shape.kind {
					// TODO: Take specialization into account
					UserTypeKind::Struct { shape } => shape.name.to_owned(),
				}
			}

			TypeEntryKind::Pointer { mutable, type_id } => {
				let inner = self.internal_type_name(module_path, type_id);
				match mutable {
					true => format!("&mut {}", inner),
					false => format!("&{}", inner),
				}
			}

			TypeEntryKind::Slice { mutable, type_id } => {
				let inner = self.internal_type_name(module_path, type_id);
				match mutable {
					true => format!("&mut [{}]", inner),
					false => format!("&[{}]", inner),
				}
			}

			TypeEntryKind::FunctionGeneric { function_shape_index, generic_index } => {
				format!("function generic {function_shape_index}:{generic_index}") // TODO
			}

			TypeEntryKind::UserTypeGeneric { shape_index, generic_index } => {
				format!("user type generic {shape_index}:{generic_index}") // TODO
			}
		}
	}

	pub fn is_pointer(&self, type_id: TypeId) -> bool {
		matches!(self.type_entries[type_id.index()].kind, TypeEntryKind::Pointer { .. })
	}
}
