use crate::error::Messages;
use crate::ir::{GenericParameter, Symbol, SymbolKind};
use crate::span::Span;
use crate::tree::{self, Node};
use crate::validator::{FunctionStore, RootLayers, Symbols};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeId {
	pub entry: u32,
}

impl TypeId {
	pub fn index(self) -> usize {
		self.entry as usize
	}
}

#[derive(Debug)]
pub struct StructShape<'a> {
	pub name: &'a str,
	pub generics: Vec<GenericParameter<'a>>,

	pub fields: Vec<Node<FieldShape<'a>>>,

	pub specializations: Vec<Struct<'a>>,
}

impl<'a> StructShape<'a> {
	pub fn new(name: &'a str, generics: Vec<GenericParameter<'a>>) -> Self {
		StructShape {
			name,
			generics,
			fields: Vec::new(),
			specializations: Vec::new(),
		}
	}
}

#[derive(Debug)]
pub struct FieldShape<'a> {
	pub name: &'a str,
	pub field_type: TypeId,
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

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
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

impl PrimativeKind {
	pub fn name(self) -> &'static str {
		match self {
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
	}
}

#[derive(Debug, Clone, Copy, Hash)]
pub struct TypeEntry {
	pub kind: TypeEntryKind,
	pub reference_entries: Option<u32>,
}

impl TypeEntry {
	pub fn new(kind: TypeEntryKind) -> TypeEntry {
		TypeEntry { kind, reference_entries: None }
	}
}

#[derive(Debug, Clone, Copy, Hash)]
pub enum TypeEntryKind {
	BuiltinType { kind: PrimativeKind },
	UserType { shape_index: usize, specialization_index: usize },
	Pointer { type_id: TypeId, mutable: bool },
	Slice { type_id: TypeId, mutable: bool },
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

		let kind = TypeEntryKind::Pointer { type_id, mutable: false };
		self.type_entries.push(TypeEntry::new(kind));

		let kind = TypeEntryKind::Pointer { type_id, mutable: true };
		self.type_entries.push(TypeEntry::new(kind));

		let kind = TypeEntryKind::Slice { type_id, mutable: false };
		self.type_entries.push(TypeEntry::new(kind));

		let kind = TypeEntryKind::Slice { type_id, mutable: true };
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

	pub fn register_function_generic(&mut self, function_shape_index: usize, generic_index: usize) -> TypeId {
		let entry = self.type_entries.len() as u32;
		let kind = TypeEntryKind::FunctionGeneric { function_shape_index, generic_index };
		self.type_entries.push(TypeEntry::new(kind));
		TypeId { entry }
	}

	pub fn lookup_type(
		&mut self,
		messages: &mut Messages,
		function_store: &mut FunctionStore,
		root_layers: &RootLayers<'a>,
		symbols: &Symbols<'a>,
		parsed_type: &Node<tree::Type<'a>>,
	) -> Option<TypeId> {
		let (path_segments, type_arguments) = match &parsed_type.item {
			tree::Type::Void => return Some(self.void_type_id),

			tree::Type::Reference(inner) => {
				let inner_id = self.lookup_type(messages, function_store, root_layers, symbols, &inner)?;
				return Some(self.pointer_to(inner_id, false)); // TODO: Parse mutability
			}

			tree::Type::Slice(inner) => {
				let inner_id = self.lookup_type(messages, function_store, root_layers, symbols, &inner)?;
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

			SymbolKind::UserTypeGeneric { shape_index, generic_index } => {
				let user_type = &self.user_types[shape_index];
				match &user_type.kind {
					UserTypeKind::Struct { shape } => {
						let generic = &shape.generics[generic_index];
						return Some(generic.generic_type_id);
					}
				}
			}

			SymbolKind::FunctionGeneric { function_shape_index, generic_index } => {
				let generics = &function_store.generics()[function_shape_index];
				let generic = &generics[generic_index];
				return Some(generic.generic_type_id);
			}

			_ => {
				messages.error(message!("Symbol {:?} is not a type", symbol.name).span(path_segments.span));
				return None;
			}
		};

		let mut type_args = Vec::with_capacity(type_arguments.len());
		for argument in type_arguments {
			type_args.push(self.lookup_type(messages, function_store, root_layers, symbols, &argument)?);
		}

		let invoke_span = self.user_types[shape_index].span;
		self.get_or_add_shape_specialization(messages, shape_index, Some(invoke_span), type_args)
	}

	pub fn get_or_add_shape_specialization(
		&mut self,
		messages: &mut Messages,
		shape_index: usize,
		invoke_span: Option<Span>,
		type_arguments: Vec<TypeId>,
	) -> Option<TypeId> {
		let user_type = &self.user_types[shape_index];
		let shape = match &user_type.kind {
			UserTypeKind::Struct { shape } => shape,
		};

		if type_arguments.len() != shape.generics.len() {
			messages.error(
				message!(
					"Expected {} type arguments for `{}`, got {}",
					shape.generics.len(),
					shape.name,
					type_arguments.len()
				)
				.span_if_some(invoke_span),
			);
			return None;
		}

		for existing in &shape.specializations {
			if existing.type_arguments == type_arguments {
				return Some(existing.type_id);
			}
		}

		let mut fields = Vec::with_capacity(shape.fields.len());
		for (field_index, field) in shape.fields.iter().enumerate() {
			fields.push(Field {
				name: field.item.name,
				type_id: field.item.field_type,
				field_index,
			});
		}

		for field in &mut fields {
			let type_id = field.type_id;
			field.type_id = self.specialize_with_user_type_generics(messages, shape_index, &type_arguments, type_id);
		}

		let user_type = &mut self.user_types[shape_index];
		let shape = match &mut user_type.kind {
			UserTypeKind::Struct { shape } => shape,
		};

		let specialization_index = shape.specializations.len();
		let type_id = TypeId { entry: self.type_entries.len() as u32 };
		let entry = TypeEntry::new(TypeEntryKind::UserType { shape_index, specialization_index });
		self.type_entries.push(entry);

		shape.specializations.push(Struct { type_id, type_arguments, fields });
		Some(type_id)
	}

	pub fn specialize_with_user_type_generics(
		&mut self,
		messages: &mut Messages,
		type_shape_index: usize,
		type_arguments: &[TypeId],
		type_id: TypeId,
	) -> TypeId {
		let entry = self.type_entries[type_id.index()];
		match &entry.kind {
			TypeEntryKind::BuiltinType { .. } => type_id,

			TypeEntryKind::UserType { shape_index, specialization_index } => {
				let shape = &mut self.user_types[*shape_index];
				match &mut shape.kind {
					UserTypeKind::Struct { shape } => {
						let specialization = &mut shape.specializations[*specialization_index];
						let any_user_type_generic = specialization.type_arguments.iter().any(|t| {
							matches!(self.type_entries[t.index()].kind, TypeEntryKind::UserTypeGeneric { .. })
						});

						if !any_user_type_generic {
							return type_id;
						}

						let mut new_struct_type_arguments = Vec::new();
						for &struct_type_argument in &specialization.type_arguments {
							let entry = &self.type_entries[struct_type_argument.index()];
							match &entry.kind {
								TypeEntryKind::UserTypeGeneric { shape_index, generic_index } => {
									assert_eq!(type_shape_index, *shape_index);
									new_struct_type_arguments.push(type_arguments[*generic_index]);
								}

								TypeEntryKind::FunctionGeneric { .. } => unreachable!(),

								_ => new_struct_type_arguments.push(struct_type_argument),
							}
						}

						self.get_or_add_shape_specialization(messages, *shape_index, None, new_struct_type_arguments)
							.unwrap()
					}
				}
			}

			TypeEntryKind::Pointer { type_id, mutable } => {
				let index = type_shape_index;
				let type_id = self.specialize_with_user_type_generics(messages, index, type_arguments, *type_id);
				self.pointer_to(type_id, *mutable)
			}

			TypeEntryKind::Slice { type_id, mutable } => {
				let index = type_shape_index;
				let type_id = self.specialize_with_user_type_generics(messages, index, type_arguments, *type_id);
				self.slice_of(type_id, *mutable)
			}

			TypeEntryKind::UserTypeGeneric { shape_index, generic_index } => {
				assert_eq!(type_shape_index, *shape_index);
				type_arguments[*generic_index]
			}

			TypeEntryKind::FunctionGeneric { .. } => unreachable!(),
		}
	}

	pub fn specialize_with_function_generics(
		&mut self,
		messages: &mut Messages,
		function_shape_index: usize,
		function_type_arguments: &[TypeId],
		type_id: TypeId,
	) -> TypeId {
		let entry = self.type_entries[type_id.index()];
		match &entry.kind {
			TypeEntryKind::BuiltinType { .. } => type_id,

			TypeEntryKind::UserType { shape_index, specialization_index } => {
				let shape = &mut self.user_types[*shape_index];
				match &mut shape.kind {
					UserTypeKind::Struct { shape } => {
						let specialization = &mut shape.specializations[*specialization_index];
						let any_function_generic = specialization.type_arguments.iter().any(|t| {
							matches!(self.type_entries[t.index()].kind, TypeEntryKind::FunctionGeneric { .. })
						});

						if !any_function_generic {
							return type_id;
						}

						let mut new_struct_type_arguments = Vec::new();
						for &struct_type_argument in &specialization.type_arguments {
							let entry = &self.type_entries[struct_type_argument.index()];
							match &entry.kind {
								TypeEntryKind::UserTypeGeneric { .. } => unreachable!(),

								TypeEntryKind::FunctionGeneric { function_shape_index: shape_index, generic_index } => {
									assert_eq!(function_shape_index, *shape_index);
									new_struct_type_arguments.push(function_type_arguments[*generic_index]);
								}

								_ => new_struct_type_arguments.push(struct_type_argument),
							}
						}

						self.get_or_add_shape_specialization(messages, *shape_index, None, new_struct_type_arguments)
							.unwrap()
					}
				}
			}

			TypeEntryKind::Pointer { type_id, mutable } => {
				let type_id = self.specialize_with_function_generics(
					messages,
					function_shape_index,
					function_type_arguments,
					*type_id,
				);

				self.pointer_to(type_id, *mutable)
			}

			TypeEntryKind::Slice { type_id, mutable } => {
				let type_id = self.specialize_with_function_generics(
					messages,
					function_shape_index,
					function_type_arguments,
					*type_id,
				);

				self.slice_of(type_id, *mutable)
			}

			TypeEntryKind::UserTypeGeneric { .. } => unreachable!(),

			TypeEntryKind::FunctionGeneric { function_shape_index: shape_index, generic_index } => {
				assert_eq!(function_shape_index, *shape_index);
				function_type_arguments[*generic_index]
			}
		}
	}

	pub fn type_name(&self, function_store: &FunctionStore, module_path: &'a [String], type_id: TypeId) -> String {
		format!("`{}`", self.internal_type_name(function_store, module_path, type_id))
	}

	fn internal_type_name(&self, function_store: &FunctionStore, module_path: &'a [String], type_id: TypeId) -> String {
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

			TypeEntryKind::UserType { shape_index, specialization_index } => {
				let shape = &self.user_types[shape_index];
				match &shape.kind {
					UserTypeKind::Struct { shape } => {
						if shape.generics.is_empty() {
							return shape.name.to_owned();
						}

						let specialization = &shape.specializations[specialization_index];
						let type_arguments = specialization
							.type_arguments
							.iter()
							.map(|a| self.internal_type_name(function_store, module_path, *a))
							.collect::<Vec<_>>()
							.join(", ");

						format!("{}[{}]", shape.name, type_arguments)
					}
				}
			}

			TypeEntryKind::Pointer { type_id, mutable } => {
				let inner = self.internal_type_name(function_store, module_path, type_id);
				match mutable {
					true => format!("&mut {}", inner),
					false => format!("&{}", inner),
				}
			}

			TypeEntryKind::Slice { type_id, mutable } => {
				let inner = self.internal_type_name(function_store, module_path, type_id);
				match mutable {
					true => format!("&mut [{}]", inner),
					false => format!("&[{}]", inner),
				}
			}

			TypeEntryKind::FunctionGeneric { function_shape_index, generic_index } => {
				let shape = &function_store.shapes()[function_shape_index];
				let generic = &shape.generics[generic_index];
				generic.name.item.to_owned()
			}

			TypeEntryKind::UserTypeGeneric { shape_index, generic_index } => {
				let shape = &self.user_types[shape_index];
				match &shape.kind {
					UserTypeKind::Struct { shape } => {
						let generic = shape.generics[generic_index];
						generic.name.item.to_owned()
					}
				}
			}
		}
	}

	pub fn is_pointer(&self, type_id: TypeId) -> bool {
		matches!(self.type_entries[type_id.index()].kind, TypeEntryKind::Pointer { .. })
	}
}
