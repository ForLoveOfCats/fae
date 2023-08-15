use crate::error::Messages;
use crate::ir::{Expression, GenericParameter, GenericUsage, GenericUsageKind, Symbol, SymbolKind};
use crate::span::Span;
use crate::tree::{self, Node};
use crate::validator::{FunctionStore, RootLayers, Symbols};

#[derive(Debug, Clone, Copy)]
pub struct TypeId {
	entry: u32,
}

impl TypeId {
	pub fn new(entry: u32) -> TypeId {
		TypeId { entry }
	}

	pub fn is_void(self, type_store: &TypeStore) -> bool {
		type_store.direct_equal(self, type_store.void_type_id)
	}

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimativeKind {
	Void,
	UntypedInteger,
	UntypedDecimal,

	I8,
	I16,
	I32,
	I64,

	U8,
	U16,
	U32,
	U64,

	F32,
	F64,
}

impl PrimativeKind {
	pub fn name(self) -> &'static str {
		match self {
			PrimativeKind::Void => "void",
			PrimativeKind::UntypedInteger => "untyped integer",
			PrimativeKind::UntypedDecimal => "untyped decimal",
			PrimativeKind::I8 => "i8",
			PrimativeKind::I16 => "i16",
			PrimativeKind::I32 => "i32",
			PrimativeKind::I64 => "i64",
			PrimativeKind::U8 => "u8",
			PrimativeKind::U16 => "u16",
			PrimativeKind::U32 => "u32",
			PrimativeKind::U64 => "u64",
			PrimativeKind::F32 => "f32",
			PrimativeKind::F64 => "f64",
		}
	}
}

#[derive(Debug, Clone, Copy)]
pub struct TypeEntry {
	pub kind: TypeEntryKind,
	pub reference_entries: Option<u32>,
	pub generic_poisoned: bool,
}

impl TypeEntry {
	pub fn new(type_store: &TypeStore, kind: TypeEntryKind) -> TypeEntry {
		let generic_poisoned = match kind {
			TypeEntryKind::BuiltinType { .. } => false,

			TypeEntryKind::UserType { shape_index, specialization_index } => {
				let user_type = &type_store.user_types[shape_index];
				match &user_type.kind {
					UserTypeKind::Struct { shape } => {
						let specialization = &shape.specializations[specialization_index];
						specialization
							.type_arguments
							.iter()
							.any(|t| type_store.type_entries[t.index()].generic_poisoned)
					}
				}
			}

			TypeEntryKind::Pointer { type_id, .. } | TypeEntryKind::Slice { type_id, .. } => {
				let entry = type_store.type_entries[type_id.index()];
				entry.generic_poisoned
			}

			TypeEntryKind::UserTypeGeneric { .. } | TypeEntryKind::FunctionGeneric { .. } => true,
		};

		TypeEntry { kind, reference_entries: None, generic_poisoned }
	}
}

#[derive(Debug, Clone, Copy)]
pub enum TypeEntryKind {
	BuiltinType { kind: PrimativeKind },
	UserType { shape_index: usize, specialization_index: usize },
	Pointer { type_id: TypeId, mutable: bool },
	Slice { type_id: TypeId, mutable: bool },
	UserTypeGeneric { shape_index: usize, generic_index: usize },
	FunctionGeneric { function_shape_index: usize, generic_index: usize },
}

#[derive(Debug, Clone, Copy)]
pub struct SliceDescription {
	pub entry: u32, // First immutable, then mutable directly after
	pub sliced_type_id: TypeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UserTypeSpecializationDescription {
	pub shape_index: usize,
	pub specialization_index: usize,
}

#[derive(Debug)]
pub struct TypeStore<'a> {
	pub primative_type_symbols: Vec<Symbol<'a>>,

	pub type_entries: Vec<TypeEntry>,
	pub slice_descriptions: Vec<SliceDescription>,
	pub user_types: Vec<UserType<'a>>,
	pub user_type_generate_order: Vec<UserTypeSpecializationDescription>,

	void_type_id: TypeId,

	integer_type_id: TypeId,
	decimal_type_id: TypeId,

	u8_type_id: TypeId,
	u16_type_id: TypeId,
	u32_type_id: TypeId,
	u64_type_id: TypeId,

	i8_type_id: TypeId,
	i16_type_id: TypeId,
	i32_type_id: TypeId,
	i64_type_id: TypeId,

	f32_type_id: TypeId,
	f64_type_id: TypeId,

	string_type_id: TypeId,
}

impl<'a> TypeStore<'a> {
	pub fn new() -> Self {
		let mut primative_type_symbols = Vec::new();
		let mut type_entries = Vec::new();

		let mut push_primative = |name: Option<&'a str>, kind| {
			let type_id = TypeId { entry: type_entries.len() as u32 };
			let kind = TypeEntryKind::BuiltinType { kind };
			type_entries.push(TypeEntry { kind, reference_entries: None, generic_poisoned: false });

			if let Some(name) = name {
				let kind = SymbolKind::BuiltinType { type_id };
				let symbol = Symbol { name, kind, span: None, file_index: None };
				primative_type_symbols.push(symbol);
			}

			type_id
		};

		let void_type_id = push_primative(Some("void"), PrimativeKind::Void);
		let integer_type_id = push_primative(None, PrimativeKind::UntypedInteger);
		let decimal_type_id = push_primative(None, PrimativeKind::UntypedDecimal);

		let i8_type_id = push_primative(Some("i8"), PrimativeKind::I8);
		let i16_type_id = push_primative(Some("i16"), PrimativeKind::I16);
		let i32_type_id = push_primative(Some("i32"), PrimativeKind::I32);
		let i64_type_id = push_primative(Some("i64"), PrimativeKind::I64);

		let u8_type_id = push_primative(Some("u8"), PrimativeKind::U8);
		let u16_type_id = push_primative(Some("u16"), PrimativeKind::U16);
		let u32_type_id = push_primative(Some("u32"), PrimativeKind::U32);
		let u64_type_id = push_primative(Some("u64"), PrimativeKind::U64);

		let f32_type_id = push_primative(Some("f32"), PrimativeKind::F32);
		let f64_type_id = push_primative(Some("f64"), PrimativeKind::F64);

		let mut type_store = TypeStore {
			primative_type_symbols,
			type_entries,
			slice_descriptions: Vec::new(),
			user_types: Vec::new(),
			user_type_generate_order: Vec::new(),
			void_type_id,
			integer_type_id,
			decimal_type_id,
			u8_type_id,
			u16_type_id,
			u32_type_id,
			u64_type_id,
			i8_type_id,
			i16_type_id,
			i32_type_id,
			i64_type_id,
			f32_type_id,
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

	pub fn integer_type_id(&self) -> TypeId {
		self.integer_type_id
	}

	pub fn decimal_type_id(&self) -> TypeId {
		self.decimal_type_id
	}

	pub fn string_type_id(&self) -> TypeId {
		self.string_type_id
	}

	pub fn type_arguments_direct_equal(&self, a: &[TypeId], b: &[TypeId]) -> bool {
		if a.len() != b.len() {
			return false;
		}

		for (a, b) in a.iter().zip(b.iter()) {
			if !self.direct_equal(*a, *b) {
				return false;
			}
		}

		true
	}

	pub fn direct_equal(&self, a: TypeId, b: TypeId) -> bool {
		a.entry == b.entry
	}

	pub fn collapse_fair(&self, a: TypeId, b: TypeId) -> Option<TypeId> {
		if a.entry == b.entry {
			return Some(a);
		}

		// if either type is an untyped number then collapse to it, checking left first

		None
	}

	pub fn collapse_to(&self, messages: &mut Messages<'a>, to: TypeId, from: &mut Expression<'a>) -> Option<bool> {
		if to.entry == from.type_id.entry {
			return Some(true);
		}

		let to_integer = to.entry == self.integer_type_id.entry;
		let from_integer = from.type_id.entry == self.integer_type_id.entry;

		let to_decimal = to.entry == self.decimal_type_id.entry;
		let from_decimal = from.type_id.entry == self.decimal_type_id.entry;

		if (to_integer && from_integer) || (to_decimal && from_decimal) {
			return Some(true);
		}

		if (to_integer || to_decimal) && !(from_integer || from_decimal) {
			return None;
			// unreachable!("Attempted to collapse to untyped number from an expression which is not an untyped number");
		}

		// constant integer -> signed of large enough | unsigned of large enough if not negative | float of large enough | TODO decimal
		// constant decimal -> float of large enough

		if from_integer {
			let (value, span) = match &from.kind {
				crate::ir::ExpressionKind::IntegerValue(value) => (value.value(), value.span()),
				kind => panic!("Collapsing from_integer with a non-IntegerValue expression: {kind:#?}"),
			};

			let (to_float, bit_count, max_float_integer) = match to.entry {
				e if e == self.f32_type_id.entry => (true, 32, 1_6777_215), // 2^24
				e if e == self.f64_type_id.entry => (true, 64, 9_007_199_254_740_992), // 2^53
				_ => (false, 0, 0),
			};

			if to_float {
				if value.abs() > max_float_integer {
					let err = message!("Constant integer {value} is unable to be represented as an `f{bit_count}`");
					messages.error(err.span(span));
					return None;
				}

				// constant integer -> float of large enough
				return Some(true);
			}

			let (to_signed, to_unsigned, bit_count) = match to.entry {
				e if e == self.i8_type_id.entry => (true, false, 8),
				e if e == self.i16_type_id.entry => (true, false, 16),
				e if e == self.i32_type_id.entry => (true, false, 32),
				e if e == self.i64_type_id.entry => (true, false, 64),

				e if e == self.u8_type_id.entry => (false, true, 8),
				e if e == self.u16_type_id.entry => (false, true, 16),
				e if e == self.u32_type_id.entry => (false, true, 32),
				e if e == self.u64_type_id.entry => (false, true, 64),

				_ => unreachable!(),
			};

			let min_value = -i128::pow(2, bit_count - 1);
			let max_value = i128::pow(2, bit_count - 1) - 1;

			if to_signed {
				if value.is_negative() {
					if value < min_value {
						messages.error(
							message!("Constant integer {value} is too small to be represented as an `i{bit_count}`")
								.span(span),
						);
						return None;
					}
				} else {
					if value > max_value {
						messages.error(
							message!("Constant integer {value} is too large to be represented as an `i{bit_count}`")
								.span(span),
						);
						return None;
					}
				}

				// constant integer -> signed of large enough
				return Some(true);
			}

			assert!(to_unsigned);

			if value.is_negative() {
				messages.error(
					message!("Constant integer {value} is negative and so cannot be represented as a `u{bit_count}`")
						.span(span),
				);
				return None;
			}

			if value > max_value {
				let err = message!("Constant integer {value} is too large to be represented as a `u{bit_count}`");
				messages.error(err.span(span));
				return None;
			}

			// constant integer -> unsigned of large enough if not negative
			return Some(true);
		}

		if from_decimal {
			let (value, span) = match &from.kind {
				crate::ir::ExpressionKind::DecimalValue(value) => (value.value(), value.span()),
				kind => panic!("Collapsing from_decimal with a non-DecimalValue expression: {kind:#?}"),
			};

			let (to_float, bit_count) = match to.entry {
				e if e == self.f32_type_id.entry => (true, 32),
				e if e == self.f64_type_id.entry => (true, 64),
				_ => (false, 0),
			};

			if to_float {
				if bit_count == 32 {
					let cast = value as f32 as f64;
					if cast != value {
						let err = message!("Constant decimal {value} cannot be represented as an `f{bit_count}` without a loss in precision");
						messages.error(err.span(span));
						return None;
					}
				}

				// constant decimal -> float of large enough
				return Some(true);
			}
		}

		Some(false)
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
		self.type_entries.push(TypeEntry::new(self, kind));

		let kind = TypeEntryKind::Pointer { type_id, mutable: true };
		self.type_entries.push(TypeEntry::new(self, kind));

		let kind = TypeEntryKind::Slice { type_id, mutable: false };
		let description = SliceDescription {
			entry: self.type_entries.len() as u32,
			sliced_type_id: type_id,
		};
		self.type_entries.push(TypeEntry::new(self, kind));
		self.slice_descriptions.push(description);

		let kind = TypeEntryKind::Slice { type_id, mutable: true };
		self.type_entries.push(TypeEntry::new(self, kind));

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
		self.type_entries.push(TypeEntry::new(self, kind));
		TypeId { entry }
	}

	pub fn lookup_type(
		&mut self,
		messages: &mut Messages,
		function_store: &mut FunctionStore,
		generic_usages: &mut Vec<GenericUsage>,
		root_layers: &RootLayers<'a>,
		symbols: &Symbols<'a>,
		parsed_type: &Node<tree::Type<'a>>,
	) -> Option<TypeId> {
		let (path_segments, type_arguments) = match &parsed_type.item {
			tree::Type::Void => return Some(self.void_type_id),

			tree::Type::Reference(inner) => {
				let id = self.lookup_type(messages, function_store, generic_usages, root_layers, symbols, &inner)?;
				return Some(self.pointer_to(id, false)); // TODO: Parse mutability
			}

			tree::Type::Slice(inner) => {
				let id = self.lookup_type(messages, function_store, generic_usages, root_layers, symbols, &inner)?;
				return Some(self.slice_of(id, false)); // TODO: Parse mutability
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
				let generics = &function_store.generics[function_shape_index];
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
			let id = self.lookup_type(messages, function_store, generic_usages, root_layers, symbols, &argument)?;
			type_args.push(id);
		}

		let invoke_span = self.user_types[shape_index].span;
		self.get_or_add_shape_specialization(messages, generic_usages, shape_index, Some(invoke_span), type_args)
	}

	pub fn get_or_add_shape_specialization(
		&mut self,
		messages: &mut Messages,
		generic_usages: &mut Vec<GenericUsage>,
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
			if self.type_arguments_direct_equal(&existing.type_arguments, &type_arguments) {
				return Some(existing.type_id);
			}
		}

		let type_arguments_generic_poisoned = type_arguments
			.iter()
			.any(|id| self.type_entries[id.index()].generic_poisoned);

		let mut fields = Vec::with_capacity(shape.fields.len());
		for field in &shape.fields {
			fields.push(Field { name: field.item.name, type_id: field.item.field_type });
		}

		for field in &mut fields {
			field.type_id = self.specialize_with_user_type_generics(
				messages,
				generic_usages,
				shape_index,
				&type_arguments,
				field.type_id,
			);
		}

		let user_type = &mut self.user_types[shape_index];
		let shape = match &mut user_type.kind {
			UserTypeKind::Struct { shape } => shape,
		};

		let specialization_index = shape.specializations.len();
		let type_id = TypeId { entry: self.type_entries.len() as u32 };
		let specialization = Struct { type_id, type_arguments: type_arguments.clone(), fields };
		shape.specializations.push(specialization);

		let entry = TypeEntry::new(self, TypeEntryKind::UserType { shape_index, specialization_index });
		self.type_entries.push(entry);

		if type_arguments_generic_poisoned {
			let kind = GenericUsageKind::UserType { shape_index };
			let usage = GenericUsage { type_arguments, kind };
			generic_usages.push(usage)
		}

		let description = UserTypeSpecializationDescription { shape_index, specialization_index };
		self.user_type_generate_order.push(description);

		Some(type_id)
	}

	pub fn specialize_with_user_type_generics(
		&mut self,
		messages: &mut Messages,
		generic_usages: &mut Vec<GenericUsage>,
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

						self.get_or_add_shape_specialization(
							messages,
							generic_usages,
							*shape_index,
							None,
							new_struct_type_arguments,
						)
						.unwrap()
					}
				}
			}

			TypeEntryKind::Pointer { type_id, mutable } => {
				let type_id = self.specialize_with_user_type_generics(
					messages,
					generic_usages,
					type_shape_index,
					type_arguments,
					*type_id,
				);
				self.pointer_to(type_id, *mutable)
			}

			TypeEntryKind::Slice { type_id, mutable } => {
				let type_id = self.specialize_with_user_type_generics(
					messages,
					generic_usages,
					type_shape_index,
					type_arguments,
					*type_id,
				);
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
		generic_usages: &mut Vec<GenericUsage>,
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

						self.get_or_add_shape_specialization(
							messages,
							generic_usages,
							*shape_index,
							None,
							new_struct_type_arguments,
						)
						.unwrap()
					}
				}
			}

			TypeEntryKind::Pointer { type_id, mutable } => {
				let type_id = self.specialize_with_function_generics(
					messages,
					generic_usages,
					function_shape_index,
					function_type_arguments,
					*type_id,
				);
				self.pointer_to(type_id, *mutable)
			}

			TypeEntryKind::Slice { type_id, mutable } => {
				let type_id = self.specialize_with_function_generics(
					messages,
					generic_usages,
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
			TypeEntryKind::BuiltinType { kind } => kind.name().to_owned(),

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
				let shape = &function_store.shapes[function_shape_index];
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
}
