use rustc_hash::FxHashMap;

use crate::frontend::error::Messages;
use crate::frontend::function_store::FunctionStore;
use crate::frontend::ir::{
	DecimalValue, EnumVariantToEnum, Expression, ExpressionKind, GenericParameters, GenericUsage, ScopeId,
	SliceMutableToImmutable, TypeArguments,
};
use crate::frontend::root_layers::RootLayers;
use crate::frontend::span::Span;
use crate::frontend::symbols::{Symbol, SymbolKind, Symbols};
use crate::frontend::tree::{self, FieldAttribute, Node};
use crate::lock::{ReentrantMutex, RwLock};
use crate::reference::{Ref, SliceRef};

// TODO: This should probably be a u64
#[derive(Debug, Clone, Copy, Hash)]
pub struct TypeId {
	entry: u32,
}

impl TypeId {
	pub fn unusable() -> TypeId {
		TypeId { entry: u32::MAX }
	}

	pub fn is_any_collapse(self, type_store: &TypeStore) -> bool {
		type_store.direct_match(self, type_store.any_collapse_type_id)
	}

	pub fn is_noreturn(self, type_store: &TypeStore) -> bool {
		type_store.direct_match(self, type_store.noreturn_type_id)
	}

	pub fn is_void(self, type_store: &TypeStore) -> bool {
		type_store.direct_match(self, type_store.void_type_id)
	}

	pub fn is_untyped_integer(self, type_store: &TypeStore) -> bool {
		type_store.direct_match(self, type_store.integer_type_id)
	}

	pub fn is_untyped_decimal(self, type_store: &TypeStore) -> bool {
		type_store.direct_match(self, type_store.decimal_type_id)
	}

	pub fn is_numeric(self, type_store: &TypeStore) -> bool {
		let range = type_store.integer_type_id.entry..=type_store.f64_type_id.entry;
		range.contains(&self.entry) || self.is_any_collapse(type_store)
	}

	pub fn is_integer(self, type_store: &TypeStore) -> bool {
		let range = type_store.i8_type_id.entry..=type_store.usize_type_id.entry;
		range.contains(&self.entry) || self.entry == type_store.integer_type_id.entry || self.is_any_collapse(type_store)
	}

	pub fn is_bool(self, type_store: &TypeStore) -> bool {
		type_store.direct_match(self, type_store.bool_type_id)
	}

	pub fn is_string(self, type_store: &TypeStore) -> bool {
		type_store.direct_match(self, type_store.string_type_id)
	}

	pub fn numeric_kind(self, type_store: &TypeStore) -> Option<NumericKind> {
		if !self.is_numeric(type_store) {
			return None;
		}

		const BUFFER: &[NumericKind] = &[
			NumericKind::I8,
			NumericKind::I16,
			NumericKind::I32,
			NumericKind::I64,
			NumericKind::U8,
			NumericKind::U16,
			NumericKind::U32,
			NumericKind::U64,
			NumericKind::ISize,
			NumericKind::USize,
			NumericKind::F32,
			NumericKind::F64,
		];

		Some(BUFFER[self.index() - type_store.i8_type_id.index()])
	}

	pub fn is_pointer(self, type_store: &mut TypeStore) -> bool {
		let entry = type_store.type_entries.get(self);
		matches!(entry.kind, TypeEntryKind::Pointer { .. })
	}

	pub fn as_pointed(self, type_store: &mut TypeStore) -> Option<AsPointed> {
		let entry = type_store.type_entries.get(self);
		match entry.kind {
			TypeEntryKind::Pointer { type_id, mutable } => Some(AsPointed { type_id, mutable }),
			_ => None,
		}
	}

	pub fn is_primative(self, type_store: &mut TypeStore) -> bool {
		let entry = type_store.type_entries.get(self);
		match entry.kind {
			TypeEntryKind::BuiltinType { .. } | TypeEntryKind::Pointer { .. } => true,
			TypeEntryKind::UserType { .. } | TypeEntryKind::Slice(_) => false,
			TypeEntryKind::UserTypeGeneric { .. } => unreachable!(),
			TypeEntryKind::FunctionGeneric { .. } => unreachable!(),
		}
	}

	pub fn as_struct<'a, R, F: FnOnce(&StructShape, &Struct) -> R>(self, type_store: &mut TypeStore<'a>, func: F) -> Option<R> {
		let entry = type_store.type_entries.get(self);
		if let TypeEntryKind::UserType { shape_index, specialization_index } = entry.kind {
			let lock = type_store.user_types.read()[shape_index].clone();
			let user_type = lock.read();
			if let UserTypeKind::Struct { shape } = &user_type.kind {
				let specialization = &shape.specializations[specialization_index];
				return Some(func(shape, specialization));
			}
		}

		None
	}

	pub fn as_slice(self, type_entries: &mut TypeEntries) -> Option<Slice> {
		let entry = type_entries.get(self);
		if let TypeEntryKind::Slice(slice) = entry.kind {
			return Some(slice);
		}

		None
	}

	pub fn index(self) -> usize {
		self.entry as usize
	}
}

pub struct AsPointed {
	pub type_id: TypeId,
	pub mutable: bool,
}

#[derive(Debug, Clone, Copy)]
pub struct Layout {
	pub size: i64,
	pub alignment: i64,
}

#[derive(Debug)]
pub struct StructShape<'a> {
	/* A race condition can occur if one thread attempts to create a new specialization while
	another thread is filling the shape and pre-existing specializations where the newly
	created specialization will not be filled.

	We avoid that by preventing any threads from creating specializations while another thread
	is actively filling the shape and pre-existing specializations. However it must still allow
	that same thread to access the shape deeper in the callstack as it is valid in some cases
	for a user type to be self-referential, eg via a pointer, so it must be able to add new
	specializations mid-filling. */
	pub filling_lock: Ref<ReentrantMutex<()>>,
	pub been_filled: bool,

	pub fields: Vec<Node<FieldShape<'a>>>,

	pub parent_enum_shape_index: Option<usize>,
	pub variant_index: Option<usize>,
	pub is_transparent_variant: bool,

	pub specializations_by_type_arguments: FxHashMap<Ref<TypeArguments>, usize>,
	pub specializations: Vec<Struct<'a>>,
}

impl<'a> StructShape<'a> {
	pub fn new(parent_enum_shape_index: Option<usize>, variant_index: Option<usize>, is_transparent_variant: bool) -> Self {
		StructShape {
			filling_lock: Ref::new(ReentrantMutex::new(())),
			been_filled: false,
			fields: Vec::new(),
			parent_enum_shape_index,
			variant_index,
			is_transparent_variant,
			specializations_by_type_arguments: FxHashMap::default(),
			specializations: Vec::new(),
		}
	}
}

#[derive(Debug, Clone, Copy)]
pub struct FieldShape<'a> {
	pub name: &'a str,
	pub field_type: TypeId,
	pub attribute: Option<Node<FieldAttribute>>,
	pub read_only: bool,
}

#[derive(Debug, Clone)]
pub struct Struct<'a> {
	pub type_id: TypeId,
	pub type_arguments: Ref<TypeArguments>,
	pub been_filled: bool,
	pub fields: SliceRef<Field<'a>>,
	pub layout: Option<Layout>,
}

#[derive(Debug, Clone, Copy)]
pub struct Field<'a> {
	pub span: Option<Span>,
	pub name: &'a str,
	pub type_id: TypeId,
	pub attribute: Option<Node<FieldAttribute>>,
	pub read_only: bool,
}

#[derive(Debug)]
pub struct EnumShape<'a> {
	// See comment on `filling_lock` field of `StructShape` for details
	pub filling_lock: Ref<ReentrantMutex<()>>,
	pub been_filled: bool,

	pub shared_fields: SliceRef<Node<FieldShape<'a>>>,

	pub variant_shapes: SliceRef<EnumVariantShape<'a>>,

	pub specializations_by_type_arguments: FxHashMap<Ref<TypeArguments>, usize>,
	pub specializations: Vec<Enum<'a>>,
}

impl<'a> EnumShape<'a> {
	pub fn new(variant_shapes: Vec<EnumVariantShape<'a>>) -> Self {
		EnumShape {
			filling_lock: Ref::new(ReentrantMutex::new(())),
			been_filled: false,
			shared_fields: SliceRef::new_empty(),
			variant_shapes: SliceRef::from(variant_shapes),
			specializations_by_type_arguments: FxHashMap::default(),
			specializations: Vec::new(),
		}
	}
}

#[derive(Debug, Clone, Copy)]
pub struct EnumVariantShape<'a> {
	pub name: &'a str,
	pub span: Span,
	pub variant_index: usize,
	pub struct_shape_index: usize,
	pub is_transparent: bool,
}

#[derive(Debug, Clone)]
pub struct Enum<'a> {
	pub type_id: TypeId,
	pub type_arguments: Ref<TypeArguments>,
	pub been_filled: bool,
	pub shared_fields: SliceRef<Field<'a>>,
	pub variants: SliceRef<EnumVariant>,
	pub variants_by_name: Ref<FxHashMap<&'a str, usize>>, // Index into variants vec
	pub layout: Option<Layout>,
	pub tag_memory_size: Option<i64>,
}

#[derive(Debug, Clone, Copy)]
pub struct EnumVariant {
	pub span: Span,
	pub type_id: TypeId,
	pub is_transparent: bool,
}

#[derive(Debug)]
pub struct UserType<'a> {
	pub name: &'a str,
	pub span: Span,
	pub scope_id: ScopeId,
	pub generic_parameters: GenericParameters<'a>,
	pub methods: FxHashMap<&'a str, MethodInfo>,
	pub kind: UserTypeKind<'a>,
}

#[derive(Debug)]
pub struct MethodInfo {
	pub function_shape_index: usize,
	pub kind: tree::MethodKind,
}

#[derive(Debug)]
pub enum UserTypeKind<'a> {
	Struct { shape: StructShape<'a> },
	Enum { shape: EnumShape<'a> },
}

#[derive(Debug)]
pub struct UserTypeChainLink<'a> {
	pub user_type: TypeId,
	pub field_name: &'a str,
	pub field_span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumericKind {
	I8,
	I16,
	I32,
	I64,

	U8,
	U16,
	U32,
	U64,

	ISize,
	USize,

	F32,
	F64,
}

impl std::fmt::Display for NumericKind {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		f.write_str(self.name())
	}
}

impl NumericKind {
	pub fn name(self) -> &'static str {
		match self {
			NumericKind::I8 => "i8",
			NumericKind::I16 => "i16",
			NumericKind::I32 => "i32",
			NumericKind::I64 => "i64",
			NumericKind::U8 => "u8",
			NumericKind::U16 => "u16",
			NumericKind::U32 => "u32",
			NumericKind::U64 => "u64",
			NumericKind::ISize => "isize",
			NumericKind::USize => "usize",
			NumericKind::F32 => "f32",
			NumericKind::F64 => "f64",
		}
	}

	pub fn layout(self) -> Layout {
		match self {
			NumericKind::I8 => Layout { size: 1, alignment: 1 },
			NumericKind::I16 => Layout { size: 2, alignment: 2 },
			NumericKind::I32 => Layout { size: 4, alignment: 4 },
			NumericKind::I64 => Layout { size: 8, alignment: 8 },
			NumericKind::U8 => Layout { size: 1, alignment: 1 },
			NumericKind::U16 => Layout { size: 2, alignment: 2 },
			NumericKind::U32 => Layout { size: 4, alignment: 4 },
			NumericKind::U64 => Layout { size: 8, alignment: 8 },
			NumericKind::ISize => Layout { size: 8, alignment: 8 },
			NumericKind::USize => Layout { size: 8, alignment: 8 },
			NumericKind::F32 => Layout { size: 4, alignment: 4 },
			NumericKind::F64 => Layout { size: 8, alignment: 8 },
		}
	}

	pub fn is_signed(self) -> bool {
		match self {
			NumericKind::I8 | NumericKind::I16 | NumericKind::I32 | NumericKind::I64 | NumericKind::ISize => true,
			NumericKind::U8 | NumericKind::U16 | NumericKind::U32 | NumericKind::U64 | NumericKind::USize => false,
			NumericKind::F32 | NumericKind::F64 => true,
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimativeKind {
	AnyCollapse,
	NoReturn,
	Void,

	UntypedInteger,
	UntypedDecimal,

	Bool,
	Numeric(NumericKind),
	String,
}

impl PrimativeKind {
	pub fn name(self) -> &'static str {
		match self {
			PrimativeKind::AnyCollapse => "any collapse",
			PrimativeKind::NoReturn => "noreturn",
			PrimativeKind::Void => "void",
			PrimativeKind::UntypedInteger => "untyped integer",
			PrimativeKind::UntypedDecimal => "untyped decimal",
			PrimativeKind::Bool => "bool",
			PrimativeKind::Numeric(numeric) => numeric.name(),
			PrimativeKind::String => "str",
		}
	}

	pub fn layout(self) -> Layout {
		match self {
			PrimativeKind::AnyCollapse => Layout { size: 0, alignment: 1 },
			PrimativeKind::NoReturn => Layout { size: 0, alignment: 1 },
			PrimativeKind::Void => Layout { size: 0, alignment: 1 },
			PrimativeKind::UntypedInteger => unreachable!(),
			PrimativeKind::UntypedDecimal => unreachable!(),
			PrimativeKind::Bool => Layout { size: 1, alignment: 1 },
			PrimativeKind::Numeric(numeric) => numeric.layout(),
			PrimativeKind::String => Layout { size: 8 * 2, alignment: 8 },
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
	pub fn new(type_store: &mut TypeStore, kind: TypeEntryKind) -> TypeEntry {
		let generic_poisoned = match kind {
			TypeEntryKind::BuiltinType { .. } => false,

			TypeEntryKind::UserType { shape_index, specialization_index } => {
				let user_type = type_store.user_types.read()[shape_index].clone();
				let user_type = user_type.read();
				match &user_type.kind {
					UserTypeKind::Struct { shape } => {
						let specialization = &shape.specializations[specialization_index];
						specialization
							.type_arguments
							.ids
							.iter()
							.any(|t| type_store.type_entries.get(*t).generic_poisoned)
					}

					UserTypeKind::Enum { shape } => {
						let specialization = &shape.specializations[specialization_index];
						specialization
							.type_arguments
							.ids
							.iter()
							.any(|t| type_store.type_entries.get(*t).generic_poisoned)
					}
				}
			}

			TypeEntryKind::Pointer { type_id, .. } | TypeEntryKind::Slice(Slice { type_id, .. }) => {
				let entry = type_store.type_entries.get(type_id);
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
	Slice(Slice),
	UserTypeGeneric { shape_index: usize, generic_index: usize },
	FunctionGeneric { function_shape_index: usize, generic_index: usize },
}

#[derive(Debug, Clone, Copy)]
pub struct Slice {
	pub type_id: TypeId,
	pub mutable: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UserTypeSpecializationDescription {
	pub shape_index: usize,
	pub specialization_index: usize,
}

const TYPE_ENTRY_CHUNK_MAX_LENGTH: usize = 50;

#[derive(Debug)]
pub struct TypeEntries {
	local_chunks: Vec<Option<Vec<TypeEntry>>>,
	global_chunks: Ref<RwLock<Vec<Vec<TypeEntry>>>>,
}

// TODO: Remove this once the root pass is parallelized and we don't need to "cheap clone" anymore
// Each worker thread will just reuse their existing type store between both passes
impl Clone for TypeEntries {
	fn clone(&self) -> Self {
		TypeEntries {
			local_chunks: Vec::new(),
			global_chunks: self.global_chunks.clone(),
		}
	}
}

impl TypeEntries {
	fn new() -> TypeEntries {
		TypeEntries {
			local_chunks: Vec::new(),
			global_chunks: Ref::new(RwLock::new(vec![Vec::with_capacity(TYPE_ENTRY_CHUNK_MAX_LENGTH)])),
		}
	}

	fn push_entry(&mut self, entry: TypeEntry) -> TypeId {
		let mut global_chunks = self.global_chunks.write();

		if global_chunks.last().unwrap().len() >= TYPE_ENTRY_CHUNK_MAX_LENGTH {
			global_chunks.push(Vec::with_capacity(TYPE_ENTRY_CHUNK_MAX_LENGTH));
		}

		let full_chunks = global_chunks.len() - 1;
		let chunk = global_chunks.last_mut().unwrap();
		let index = chunk.len() + (full_chunks * TYPE_ENTRY_CHUNK_MAX_LENGTH);
		chunk.push(entry);
		TypeId { entry: index as u32 }
	}

	pub fn get(&mut self, type_id: TypeId) -> TypeEntry {
		let overall_index = type_id.index();
		let index = overall_index % TYPE_ENTRY_CHUNK_MAX_LENGTH;
		let chunk_index = overall_index / TYPE_ENTRY_CHUNK_MAX_LENGTH;

		let mut updated = false;
		if chunk_index >= self.local_chunks.len() || self.local_chunks[chunk_index].is_none() {
			self.update_chunk(chunk_index);
			updated = true;
		}

		loop {
			let entry = self.local_chunks[chunk_index].as_ref().unwrap().get(index);
			if let Some(entry) = entry {
				return *entry;
			}

			if updated {
				panic!("TypeId index {overall_index} is out of range");
			}
			self.update_chunk(chunk_index);
			updated = true;
		}
	}

	fn update_chunk(&mut self, chunk_index: usize) {
		while self.local_chunks.len() <= chunk_index {
			self.local_chunks.push(None);
		}

		let local_chunk = &mut self.local_chunks[chunk_index];
		if let Some(local_chunk) = local_chunk {
			local_chunk.clear();
			let global_chunks = self.global_chunks.read();
			local_chunk.extend_from_slice(&global_chunks[chunk_index]);
		} else {
			let global_chunks = self.global_chunks.read();
			*local_chunk = Some(global_chunks[chunk_index].clone());
		}
	}
}

#[derive(Debug, Clone)]
pub struct TypeStore<'a> {
	pub debug_generics: bool,
	pub debug_type_ids: bool,

	pub primative_type_symbols: SliceRef<Symbol<'a>>,

	pub type_entries: TypeEntries,
	pub user_types: Ref<RwLock<Vec<Ref<RwLock<UserType<'a>>>>>>,
	pub user_type_generate_order: Ref<RwLock<Vec<UserTypeSpecializationDescription>>>,

	any_collapse_type_id: TypeId,
	noreturn_type_id: TypeId,
	void_type_id: TypeId,

	integer_type_id: TypeId,
	decimal_type_id: TypeId,

	i8_type_id: TypeId,
	i16_type_id: TypeId,
	i32_type_id: TypeId,
	i64_type_id: TypeId,

	u8_type_id: TypeId,
	u16_type_id: TypeId,
	u32_type_id: TypeId,
	u64_type_id: TypeId,

	isize_type_id: TypeId,
	usize_type_id: TypeId,

	f32_type_id: TypeId,
	f64_type_id: TypeId,

	bool_type_id: TypeId,
	string_type_id: TypeId,

	u8_slice_type_id: TypeId,
}

impl<'a> TypeStore<'a> {
	pub fn new(debug_generics: bool, debug_type_ids: bool) -> Self {
		let mut primative_type_symbols = Vec::new();
		let mut type_entries = TypeEntries::new();

		let mut push_primative = |name: Option<&'a str>, kind| {
			let kind = TypeEntryKind::BuiltinType { kind };
			let type_entry = TypeEntry { kind, reference_entries: None, generic_poisoned: false };
			let type_id = type_entries.push_entry(type_entry);

			if let Some(name) = name {
				let kind = SymbolKind::BuiltinType { type_id };
				let symbol = Symbol { name, kind, span: None };
				primative_type_symbols.push(symbol);
			}

			type_id
		};

		let any_collapse_type_id = push_primative(None, PrimativeKind::AnyCollapse);
		let noreturn_type_id = push_primative(Some("noreturn"), PrimativeKind::NoReturn);
		let void_type_id = push_primative(Some("void"), PrimativeKind::Void);

		// NOTE: These numeric type ids must all be generated together for the `is_numeric` range check
		let integer_type_id = push_primative(None, PrimativeKind::UntypedInteger);
		let decimal_type_id = push_primative(None, PrimativeKind::UntypedDecimal);

		let i8_type_id = push_primative(Some("i8"), PrimativeKind::Numeric(NumericKind::I8));
		let i16_type_id = push_primative(Some("i16"), PrimativeKind::Numeric(NumericKind::I16));
		let i32_type_id = push_primative(Some("i32"), PrimativeKind::Numeric(NumericKind::I32));
		let i64_type_id = push_primative(Some("i64"), PrimativeKind::Numeric(NumericKind::I64));

		let u8_type_id = push_primative(Some("u8"), PrimativeKind::Numeric(NumericKind::U8));
		let u16_type_id = push_primative(Some("u16"), PrimativeKind::Numeric(NumericKind::U16));
		let u32_type_id = push_primative(Some("u32"), PrimativeKind::Numeric(NumericKind::U32));
		let u64_type_id = push_primative(Some("u64"), PrimativeKind::Numeric(NumericKind::U64));

		let isize_type_id = push_primative(Some("isize"), PrimativeKind::Numeric(NumericKind::ISize));
		let usize_type_id = push_primative(Some("usize"), PrimativeKind::Numeric(NumericKind::USize));

		let f32_type_id = push_primative(Some("f32"), PrimativeKind::Numeric(NumericKind::F32));
		let f64_type_id = push_primative(Some("f64"), PrimativeKind::Numeric(NumericKind::F64));

		let bool_type_id = push_primative(Some("bool"), PrimativeKind::Bool);
		let string_type_id = push_primative(Some("str"), PrimativeKind::String);

		let mut type_store = TypeStore {
			debug_generics,
			debug_type_ids,
			primative_type_symbols: SliceRef::from(primative_type_symbols),
			type_entries,
			user_types: Ref::new(RwLock::new(Vec::new())),
			user_type_generate_order: Ref::new(RwLock::new(Vec::new())),
			any_collapse_type_id,
			noreturn_type_id,
			void_type_id,
			integer_type_id,
			decimal_type_id,
			bool_type_id,
			i8_type_id,
			i16_type_id,
			i32_type_id,
			i64_type_id,
			u8_type_id,
			u16_type_id,
			u32_type_id,
			u64_type_id,
			isize_type_id,
			usize_type_id,
			f32_type_id,
			f64_type_id,
			string_type_id,
			u8_slice_type_id: TypeId { entry: u32::MAX },
		};

		// This is a hack, consider moving the saved type ids into a different struct
		let u8_slice_type_id = type_store.slice_of(u8_type_id, false);
		type_store.u8_slice_type_id = u8_slice_type_id;

		type_store
	}

	pub fn any_collapse_type_id(&self) -> TypeId {
		self.any_collapse_type_id
	}

	pub fn void_type_id(&self) -> TypeId {
		self.void_type_id
	}

	pub fn integer_type_id(&self) -> TypeId {
		self.integer_type_id
	}

	pub fn decimal_type_id(&self) -> TypeId {
		self.decimal_type_id
	}

	pub fn u8_type_id(&self) -> TypeId {
		self.u8_type_id
	}

	pub fn u32_type_id(&self) -> TypeId {
		self.u32_type_id
	}

	pub fn i64_type_id(&self) -> TypeId {
		self.i64_type_id
	}

	pub fn u64_type_id(&self) -> TypeId {
		self.u64_type_id
	}

	pub fn isize_type_id(&self) -> TypeId {
		self.isize_type_id
	}

	pub fn usize_type_id(&self) -> TypeId {
		self.usize_type_id
	}

	pub fn bool_type_id(&self) -> TypeId {
		self.bool_type_id
	}

	pub fn string_type_id(&self) -> TypeId {
		self.string_type_id
	}

	pub fn u8_slice_type_id(&self) -> TypeId {
		self.u8_slice_type_id
	}

	pub fn direct_match(&self, a: TypeId, b: TypeId) -> bool {
		a.entry == b.entry
	}

	pub fn collapse_fair(
		&mut self,
		messages: &mut Messages<'a>,
		function_store: &FunctionStore,
		a: &mut Expression<'a>,
		b: &mut Expression<'a>,
	) -> Result<TypeId, ()> {
		if a.type_id.entry == b.type_id.entry {
			return Ok(a.type_id);
		}

		// if either are any collapse then collapse to the other, even if it is also any collapse
		// if both are untyped numbers then we know they are different, collapse to the decimal
		// if either type is an untyped number we know the other isn't, collapse to the other type
		// if either type is an enum, collapse the other to it

		if a.type_id.entry == self.any_collapse_type_id.entry {
			return Ok(b.type_id);
		} else if b.type_id.entry == self.any_collapse_type_id.entry {
			return Ok(a.type_id);
		}

		let a_number = a.type_id.entry == self.integer_type_id.entry || a.type_id.entry == self.decimal_type_id.entry;
		let b_number = b.type_id.entry == self.integer_type_id.entry || b.type_id.entry == self.decimal_type_id.entry;

		if a_number && b_number {
			let non_decimal = if a.type_id.entry == self.decimal_type_id.entry {
				b
			} else if b.type_id.entry == self.decimal_type_id.entry {
				a
			} else {
				unreachable!();
			};

			let collapsed = self.collapse_to(messages, function_store, self.decimal_type_id, non_decimal)?;
			return match collapsed {
				true => Ok(self.decimal_type_id),
				false => Err(()),
			};
		}

		if a_number {
			assert!(!b_number);
			let collapsed = self.collapse_to(messages, function_store, b.type_id, a)?;
			return match collapsed {
				true => Ok(b.type_id),
				false => Err(()),
			};
		}

		if b_number {
			assert!(!a_number);
			let collapsed = self.collapse_to(messages, function_store, a.type_id, b)?;
			return match collapsed {
				true => Ok(a.type_id),
				false => Err(()),
			};
		}

		let is_enum = |type_store: &mut Self, e: &Expression| {
			let entry = type_store.type_entries.get(e.type_id);
			match entry.kind {
				TypeEntryKind::UserType { shape_index, .. } => match type_store.user_types.read()[shape_index].read().kind {
					UserTypeKind::Enum { .. } => return true,
					_ => return false,
				},
				_ => return false,
			}
		};

		if is_enum(self, a) {
			let collapsed = self.collapse_to(messages, function_store, a.type_id, b)?;
			return match collapsed {
				true => Ok(a.type_id),
				false => Err(()),
			};
		}

		if is_enum(self, b) {
			let collapsed = self.collapse_to(messages, function_store, b.type_id, a)?;
			return match collapsed {
				true => Ok(b.type_id),
				false => Err(()),
			};
		}

		Err(())
	}

	// `true` -> collapsed or was already the same type
	// `false` -> was was a different type and could not collapse
	pub fn collapse_to(
		&mut self,
		messages: &mut Messages<'a>,
		function_store: &FunctionStore,
		to: TypeId,
		from: &mut Expression<'a>,
	) -> Result<bool, ()> {
		if to.entry == from.type_id.entry {
			return Ok(true);
		}

		// any collapse -> anything else
		// untyped integer -> signed of large enough | unsigned of large enough if not negative | float of large enough | decimal if small enough
		// untyped decimal -> float of large enough
		// mutable reference -> immutable reference
		// mutable slice -> immutable slice
		// enum variant -> enum

		if from.type_id.entry == self.any_collapse_type_id.entry {
			// From any collapse
			// No need to convert anything, this only gets introduced in case of error so we know we won't codegen
			return Ok(true);
		}

		if from.type_id.entry == self.integer_type_id.entry {
			// From untyped integer

			let to_decimal = to.entry == self.decimal_type_id.entry;

			let (value, span, from_value) = match &mut from.kind {
				ExpressionKind::IntegerValue(value) => (value.value(), value.span(), value),
				kind => panic!("Collapsing from_integer with a non-IntegerValue expression: {kind:#?}"),
			};

			const MAX_F23_INTEGER: i128 = 16_777_215; // 2^24
			const MAX_F64_INTEGER: i128 = 9_007_199_254_740_991; // 2^53 - 1

			if to_decimal {
				if value.abs() > MAX_F64_INTEGER {
					let err = error!("Constant integer {value} is unable to be represented as a constant decimal");
					messages.message(err.span(span));
					return Err(());
				}

				let type_id = self.decimal_type_id;
				let mutable = from.is_mutable;
				let returns = from.returns;
				let kind = ExpressionKind::DecimalValue(DecimalValue::new(value as f64, span));
				*from = Expression {
					span: from.span,
					type_id,
					is_mutable: mutable,
					returns,
					kind,
					debug_location: from.span.debug_location(),
				};
				return Ok(true);
			}

			let (to_float, max_float_integer) = match to.entry {
				e if e == self.f32_type_id.entry => (true, MAX_F23_INTEGER),
				e if e == self.f64_type_id.entry => (true, MAX_F64_INTEGER),
				_ => (false, 0),
			};

			if to_float {
				if value.abs() > max_float_integer {
					let name = self.type_name(function_store, &[], to);
					let err = error!("Constant integer {value} is unable to be represented as {name}");
					messages.message(err.span(span));
					return Err(());
				}

				// constant integer -> float of large enough
				from_value.collapse(to);
				return Ok(true);
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

				e if e == self.isize_type_id.entry => (true, false, 64),
				e if e == self.usize_type_id.entry => (false, true, 64),

				_ => (false, false, 0),
			};

			if to_signed {
				if value.is_negative() {
					let min_value = -i128::pow(2, bit_count - 1);
					if value < min_value {
						let name = self.type_name(function_store, &[], to);
						let error = error!("Constant integer {value} is too small to be represented as {name}");
						messages.message(error.span(span));
						return Err(());
					}
				} else {
					let max_value = i128::pow(2, bit_count - 1) - 1;
					if value > max_value {
						let name = self.type_name(function_store, &[], to);
						let error = error!("Constant integer {value} is too large to be represented as {name}");
						messages.message(error.span(span));
						return Err(());
					}
				}

				// constant integer -> signed of large enough
				from_value.collapse(to);
				return Ok(true);
			}

			if to_unsigned {
				if value.is_negative() {
					let name = self.type_name(function_store, &[], to);
					let error = error!("Constant integer {value} is negative and so cannot be represented as {name}",);
					messages.message(error.span(span));
					return Err(());
				}

				let max_value = i128::pow(2, bit_count) - 1;
				if value > max_value {
					let name = self.type_name(function_store, &[], to);
					let error = error!("Constant integer {value} is too large to be represented as {name}");
					messages.message(error.span(span));
					return Err(());
				}

				// constant integer -> unsigned of large enough if not negative
				from_value.collapse(to);
				return Ok(true);
			}
		}

		if from.type_id.entry == self.decimal_type_id.entry {
			// From untyped decimal

			let (value, span, from_value) = match &mut from.kind {
				ExpressionKind::DecimalValue(value) => (value.value(), value.span(), value),
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
						let name = self.type_name(function_store, &[], to);
						let err = error!("Constant decimal {value} cannot be represented as {name} without a loss in precision");
						messages.message(err.span(span));
						return Err(());
					}
				}

				// constant decimal -> float of large enough
				from_value.collapse(to);
				return Ok(true);
			}
		}

		let to_entry = self.type_entries.get(to);
		let from_entry = self.type_entries.get(from.type_id);

		// mutable reference -> immutable reference
		if let TypeEntryKind::Pointer { type_id: to, mutable: to_mutable } = to_entry.kind {
			if let TypeEntryKind::Pointer { type_id: from, mutable: from_mutable } = from_entry.kind {
				if to.entry == from.entry && from_mutable && !to_mutable {
					return Ok(true);
				}
			}
		}

		// mutable slice -> immutable slice
		if let TypeEntryKind::Slice(Slice { type_id: to_type_id, mutable: to_mutable }) = to_entry.kind {
			if let TypeEntryKind::Slice(Slice { type_id: from_type_id, mutable: from_mutable }) = from_entry.kind {
				if to_type_id.entry == from_type_id.entry && from_mutable && !to_mutable {
					if let ExpressionKind::ArrayLiteral(literal) = &mut from.kind {
						// See `get_or_create_reference_entries`, mutable slice directly follows immutable slice of same type
						// Back up one entry to turn the array's slice from mutable to immutable
						literal.type_id.entry -= 1;
						return Ok(true);
					}

					// TODO: This replace is a dumb solution
					let expression = std::mem::replace(from, Expression::any_collapse(self, from.span));
					let returns = expression.returns;
					let type_id = TypeId { entry: expression.type_id.entry - 1 };
					let conversion = Box::new(SliceMutableToImmutable { expression });
					let kind = ExpressionKind::SliceMutableToImmutable(conversion);
					*from = Expression {
						span: from.span,
						type_id,
						is_mutable: false,
						returns,
						kind,
						debug_location: from.span.debug_location(),
					};
					return Ok(true);
				}
			}
		}

		// enum variant -> enum
		if let TypeEntryKind::UserType { shape_index, .. } = from_entry.kind {
			let user_type = self.user_types.read()[shape_index].clone();
			let user_type = user_type.read();
			if let UserTypeKind::Struct { shape } = &user_type.kind {
				if let Some(parent_enum_shape_index) = shape.parent_enum_shape_index {
					if let TypeEntryKind::UserType { shape_index, .. } = to_entry.kind {
						if shape_index == parent_enum_shape_index {
							// TODO: This replace is a dumb solution
							let expression = std::mem::replace(from, Expression::any_collapse(self, from.span));
							let returns = expression.returns;
							let conversion = Box::new(EnumVariantToEnum { type_id: to, expression });
							let kind = ExpressionKind::EnumVariantToEnum(conversion);
							*from = Expression {
								span: from.span,
								type_id: to,
								is_mutable: false,
								returns,
								kind,
								debug_location: from.span.debug_location(),
							};
							return Ok(true);
						}
					}
				}
			}
		}

		Ok(false)
	}

	fn get_or_create_reference_entries(&mut self, type_id: TypeId) -> u32 {
		let entry = self.type_entries.get(type_id);
		if let Some(entries) = entry.reference_entries {
			return entries;
		}

		// Order of reference entries:
		// - immutable pointer
		// - mutable pointer
		// - immutable slice
		// - mutable slice

		let a_kind = TypeEntryKind::Pointer { type_id, mutable: false };
		let b_kind = TypeEntryKind::Pointer { type_id, mutable: true };
		let c_kind = TypeEntryKind::Slice(Slice { type_id, mutable: false });
		let d_kind = TypeEntryKind::Slice(Slice { type_id, mutable: true });

		let entries = &[
			TypeEntry::new(self, a_kind),
			TypeEntry::new(self, b_kind),
			TypeEntry::new(self, c_kind),
			TypeEntry::new(self, d_kind),
		];

		let mut global_chunks = self.type_entries.global_chunks.write();
		// Check again, another thread may have already created these while we were waiting for the lock
		let entry = global_chunks[type_id.index() / TYPE_ENTRY_CHUNK_MAX_LENGTH][type_id.index() % TYPE_ENTRY_CHUNK_MAX_LENGTH];
		if let Some(reference_entries) = entry.reference_entries {
			drop(global_chunks);
			self.type_entries.local_chunks[type_id.index() / TYPE_ENTRY_CHUNK_MAX_LENGTH]
				.as_mut()
				.unwrap()[type_id.index() % TYPE_ENTRY_CHUNK_MAX_LENGTH]
				.reference_entries = Some(reference_entries);
			return reference_entries;
		}

		let previous_chunks_len = (global_chunks.len() - 1) * TYPE_ENTRY_CHUNK_MAX_LENGTH;
		let index = previous_chunks_len + global_chunks.last().unwrap().len();
		let reference_entries = index as u32;

		for &entry in entries.into_iter() {
			if global_chunks.last().unwrap().len() >= TYPE_ENTRY_CHUNK_MAX_LENGTH {
				global_chunks.push(Vec::with_capacity(TYPE_ENTRY_CHUNK_MAX_LENGTH));
			}

			let chunk = global_chunks.last_mut().unwrap();
			chunk.push(entry);
		}

		let entry =
			&mut global_chunks[type_id.index() / TYPE_ENTRY_CHUNK_MAX_LENGTH][type_id.index() % TYPE_ENTRY_CHUNK_MAX_LENGTH];
		entry.reference_entries = Some(reference_entries);

		drop(global_chunks);

		self.type_entries.local_chunks[type_id.index() / TYPE_ENTRY_CHUNK_MAX_LENGTH]
			.as_mut()
			.unwrap()[type_id.index() % TYPE_ENTRY_CHUNK_MAX_LENGTH]
			.reference_entries = Some(reference_entries);
		reference_entries
	}

	pub fn pointer_to(&mut self, type_id: TypeId, mutable: bool) -> TypeId {
		if type_id.is_any_collapse(self) {
			return type_id;
		}

		let reference_entries = self.get_or_create_reference_entries(type_id);
		let entry = match mutable {
			false => reference_entries,
			true => reference_entries + 1,
		};
		TypeId { entry }
	}

	pub fn slice_of(&mut self, type_id: TypeId, mutable: bool) -> TypeId {
		if type_id.is_any_collapse(self) {
			return type_id;
		}

		let reference_entries = self.get_or_create_reference_entries(type_id);
		let entry = match mutable {
			false => reference_entries + 2,
			true => reference_entries + 3,
		};
		TypeId { entry }
	}

	// TODO: Dear god I need anonymous structs
	// (TypeId, mutable)
	pub fn pointed_to(&mut self, type_id: TypeId) -> Option<(TypeId, bool)> {
		let entry = self.type_entries.get(type_id);
		match entry.kind {
			TypeEntryKind::Pointer { type_id, mutable } => Some((type_id, mutable)),
			TypeEntryKind::BuiltinType { kind: PrimativeKind::AnyCollapse } => Some((self.any_collapse_type_id, false)),
			_ => None,
		}
	}

	// (TypeId, mutable)
	pub fn sliced_of(&mut self, type_id: TypeId) -> Option<(TypeId, bool)> {
		let entry = self.type_entries.get(type_id);
		match entry.kind {
			TypeEntryKind::Slice(Slice { type_id, mutable }) => Some((type_id, mutable)),
			TypeEntryKind::BuiltinType { kind: PrimativeKind::AnyCollapse } => Some((self.any_collapse_type_id, false)),
			_ => None,
		}
	}

	pub fn register_type(
		user_types: &mut Vec<Ref<RwLock<UserType<'a>>>>,
		name: &'a str,
		generic_parameters: GenericParameters<'a>,
		kind: UserTypeKind<'a>,
		scope_id: ScopeId,
		span: Span,
	) -> usize {
		// Type entry gets added during specialization

		let shape_index = user_types.len();
		let user_type = UserType {
			name,
			span,
			scope_id,
			generic_parameters,
			methods: FxHashMap::default(),
			kind,
		};
		user_types.push(Ref::new(RwLock::new(user_type)));

		shape_index
	}

	pub fn register_user_type_generic(&mut self, shape_index: usize, generic_index: usize) -> TypeId {
		let kind = TypeEntryKind::UserTypeGeneric { shape_index, generic_index };
		let type_entry = TypeEntry { kind, reference_entries: None, generic_poisoned: true };
		self.type_entries.push_entry(type_entry)
	}

	pub fn register_function_generic(&mut self, function_shape_index: usize, generic_index: usize) -> TypeId {
		let kind = TypeEntryKind::FunctionGeneric { function_shape_index, generic_index };
		let type_entry = TypeEntry { kind, reference_entries: None, generic_poisoned: true };
		self.type_entries.push_entry(type_entry)
	}

	pub fn calculate_layout(&mut self, type_id: TypeId) {
		let entry = self.type_entries.get(type_id);
		if let TypeEntryKind::UserType { shape_index, specialization_index } = entry.kind {
			let lock = self.user_types.read()[shape_index].clone();
			let user_type = lock.read();
			match &user_type.kind {
				UserTypeKind::Struct { shape } => {
					let specialization = &shape.specializations[specialization_index];
					if specialization.layout.is_some() {
						return;
					}

					// Belch
					let field_types: Vec<_> = specialization.fields.iter().map(|f| f.type_id).collect();
					drop(user_type);

					let mut size = 0;
					let mut alignment = 1;

					for field_type_id in field_types {
						self.calculate_layout(field_type_id);
						let field_layout = self.type_layout(field_type_id);

						if field_layout.alignment != 0 {
							size += size % field_layout.alignment;
						}

						size += field_layout.size;
						alignment = alignment.max(field_layout.alignment);
					}

					if size != 0 && size % alignment != 0 {
						size = (size / alignment) * alignment + alignment;
					}

					if !entry.generic_poisoned {
						let description = UserTypeSpecializationDescription { shape_index, specialization_index };
						self.user_type_generate_order.write().push(description);
					}

					let layout = Layout { size, alignment };
					let mut user_type = lock.write();
					match &mut user_type.kind {
						UserTypeKind::Struct { shape } => {
							shape.specializations[specialization_index].layout = Some(layout);
						}

						kind => unreachable!("{kind:?}"),
					}
				}

				UserTypeKind::Enum { shape } => {
					let specialization = &shape.specializations[specialization_index];
					if specialization.layout.is_some() {
						return;
					}

					let variants: Vec<_> = specialization.variants.iter().map(|v| v.type_id).collect();
					drop(user_type);

					let mut size = 0;
					let mut alignment = 1;

					for variant in variants {
						self.calculate_layout(variant);
						let variant_layout = self.type_layout(variant);

						size = size.max(variant_layout.size);
						alignment = alignment.max(variant_layout.alignment);
					}

					if !entry.generic_poisoned {
						let description = UserTypeSpecializationDescription { shape_index, specialization_index };
						self.user_type_generate_order.write().push(description);
					}

					let tag_memory_size = alignment.max(1);
					size += tag_memory_size;

					let layout = Layout { size, alignment };
					let mut user_type = lock.write();
					match &mut user_type.kind {
						UserTypeKind::Enum { shape } => {
							let specialization = &mut shape.specializations[specialization_index];
							specialization.layout = Some(layout);
							specialization.tag_memory_size = Some(tag_memory_size);
						}

						kind => unreachable!("{kind:?}"),
					}
				}
			}
		}
	}

	pub fn type_layout(&mut self, type_id: TypeId) -> Layout {
		match self.type_entries.get(type_id).kind {
			TypeEntryKind::BuiltinType { kind } => kind.layout(),

			TypeEntryKind::UserType { shape_index, specialization_index } => {
				match &self.user_types.read()[shape_index].read().kind {
					UserTypeKind::Struct { shape } => {
						let specialization = &shape.specializations[specialization_index];
						specialization
							.layout
							.expect("should have called `calculate_layout` before `type_layout`")
					}

					UserTypeKind::Enum { shape } => {
						let specialization = &shape.specializations[specialization_index];
						specialization
							.layout
							.expect("should have called `calculate_layout` before `type_layout`")
					}
				}
			}

			TypeEntryKind::Pointer { .. } => Layout { size: 8, alignment: 8 },

			TypeEntryKind::Slice(_) => Layout { size: 16, alignment: 8 },

			// TODO: These are probably wrong, take care to make sure this doesn't break size_of in generic functions
			TypeEntryKind::UserTypeGeneric { .. } => Layout { size: 0, alignment: 1 },
			TypeEntryKind::FunctionGeneric { .. } => Layout { size: 0, alignment: 1 },
		}
	}

	pub fn find_user_type_dependency_chain(&mut self, from: TypeId, to: TypeId) -> Option<Vec<UserTypeChainLink<'a>>> {
		let entry = self.type_entries.get(from);
		let (shape_index, specialization_index) = match entry.kind {
			TypeEntryKind::UserType { shape_index, specialization_index } => (shape_index, specialization_index),
			_ => return None,
		};

		// This should not be able to double-read as we should catch that we've hit a duplicate before we get here
		let user_type = self.user_types.read()[shape_index].clone();
		let user_type = user_type.read();
		match &user_type.kind {
			UserTypeKind::Struct { shape } => {
				let specialization = &shape.specializations[specialization_index];
				for field in specialization.fields.iter() {
					if field.type_id.entry == to.entry {
						let link = UserTypeChainLink {
							user_type: specialization.type_id,
							field_name: field.name,
							field_span: field.span.expect("User type fields should always have a span"),
						};
						return Some(vec![link]);
					}

					let chain = self.find_user_type_dependency_chain(field.type_id, to);
					if let Some(mut chain) = chain {
						chain.insert(
							0,
							UserTypeChainLink {
								user_type: specialization.type_id,
								field_name: field.name,
								field_span: field.span.expect("User type fields should always have a span"),
							},
						);
						return Some(chain);
					}
				}
			}

			UserTypeKind::Enum { shape } => {
				let specialization = &shape.specializations[specialization_index];

				for (name, &variant_index) in specialization.variants_by_name.iter() {
					let variant = &specialization.variants[variant_index];
					if variant.type_id.entry == to.entry {
						let link = UserTypeChainLink {
							user_type: specialization.type_id,
							field_name: name,
							field_span: variant.span,
						};
						return Some(vec![link]);
					}

					let chain = self.find_user_type_dependency_chain(variant.type_id, to);
					if let Some(mut chain) = chain {
						chain.insert(
							0,
							UserTypeChainLink {
								user_type: specialization.type_id,
								field_name: name,
								field_span: variant.span,
							},
						);
						return Some(chain);
					}
				}
			}
		}

		None
	}

	pub fn lookup_type(
		&mut self,
		messages: &mut Messages<'a>,
		function_store: &FunctionStore<'a>,
		module_path: &[String],
		generic_usages: &mut Vec<GenericUsage>,
		root_layers: &RootLayers<'a>,
		symbols: &Symbols<'a>,
		function_initial_symbols_len: usize,
		enclosing_generic_parameters: &GenericParameters<'a>,
		parsed_type: &Node<tree::Type<'a>>,
	) -> Option<TypeId> {
		let (path_segments, type_arguments, dot_access) = match &parsed_type.item {
			tree::Type::Void => return Some(self.void_type_id),

			tree::Type::Pointer { pointee, mutable } => {
				let id = self.lookup_type(
					messages,
					function_store,
					module_path,
					generic_usages,
					root_layers,
					symbols,
					function_initial_symbols_len,
					enclosing_generic_parameters,
					pointee,
				)?;
				return Some(self.pointer_to(id, *mutable));
			}

			tree::Type::Slice { pointee, mutable } => {
				let id = self.lookup_type(
					messages,
					function_store,
					module_path,
					generic_usages,
					root_layers,
					symbols,
					function_initial_symbols_len,
					enclosing_generic_parameters,
					pointee,
				)?;
				return Some(self.slice_of(id, *mutable));
			}

			tree::Type::Path { path_segments, type_arguments, dot_access } => (path_segments, type_arguments, dot_access),
		};

		let symbol = symbols.lookup_symbol(messages, root_layers, self, function_initial_symbols_len, &path_segments.item)?;
		let shape_index = match symbol.kind {
			SymbolKind::BuiltinType { type_id } => {
				if !type_arguments.is_empty() {
					messages.message(error!("Builtin types do not accept type arguments").span(parsed_type.span));
					return None;
				}

				if let Some(dot_access) = dot_access {
					messages.message(error!("Builtin types do not have variants").span(dot_access.span));
					return None;
				}

				return Some(type_id);
			}

			SymbolKind::Type { shape_index } => shape_index,

			SymbolKind::UserTypeGeneric { shape_index, generic_index } => {
				if let Some(dot_access) = dot_access {
					let error = error!("User type generic type parameters do not have variants");
					messages.message(error.span(dot_access.span));
					return None;
				}

				let user_type = &self.user_types.read()[shape_index];
				let generic = user_type.read().generic_parameters.parameters()[generic_index];
				return Some(generic.generic_type_id);
			}

			SymbolKind::FunctionGeneric { function_shape_index, generic_index } => {
				if let Some(dot_access) = dot_access {
					let error = error!("Function generic type parameters do not have variants");
					messages.message(error.span(dot_access.span));
					return None;
				}

				let generics = &function_store.generics.read()[function_shape_index];
				let generic = &generics.parameters()[generic_index];
				return Some(generic.generic_type_id);
			}

			_ => {
				messages.message(error!("Symbol {:?} is not a type", symbol.name).span(path_segments.span));
				return None;
			}
		};

		let invoke_span = Some(parsed_type.span);
		let type_id = self.get_or_add_shape_specialization_in_scope(
			messages,
			function_store,
			module_path,
			generic_usages,
			root_layers,
			symbols,
			shape_index,
			invoke_span,
			function_initial_symbols_len,
			enclosing_generic_parameters,
			type_arguments,
		)?;

		if let &Some(&dot_access) = dot_access {
			self.get_enum_variant(messages, function_store, module_path, type_id, dot_access)
		} else {
			Some(type_id)
		}
	}

	pub fn get_enum_variant(
		&mut self,
		messages: &mut Messages<'a>,
		function_store: &FunctionStore<'a>,
		module_path: &[String],
		base: TypeId,
		name: Node<&'a str>,
	) -> Option<TypeId> {
		let mut report_not_enum_error = |this: &mut Self| {
			let found = this.type_name(function_store, module_path, base);
			let error = error!("Type {found} does not have variants as it is not an enum");
			messages.message(error.span(name.span));
		};

		let entry = self.type_entries.get(base);
		let user_types = self.user_types.read();

		let (lock, specialization_index) = match entry.kind {
			TypeEntryKind::UserType { shape_index, specialization_index } => {
				(user_types[shape_index].clone(), specialization_index)
			}

			_ => {
				drop(user_types);
				report_not_enum_error(self);
				return None;
			}
		};
		drop(user_types);

		let shape = lock.read();
		let specialization = if let UserTypeKind::Enum { shape } = &shape.kind {
			&shape.specializations[specialization_index]
		} else {
			drop(shape);
			report_not_enum_error(self);
			return None;
		};

		if let Some(&variant_index) = specialization.variants_by_name.get(name.item) {
			Some(specialization.variants[variant_index].type_id)
		} else {
			drop(shape);
			let found = self.type_name(function_store, module_path, base);
			let error = error!("No variant `{}` found on enum {found}", name.item);
			messages.message(error.span(name.span));
			None
		}
	}

	pub fn get_or_add_shape_specialization_in_scope(
		&mut self,
		messages: &mut Messages<'a>,
		function_store: &FunctionStore<'a>,
		module_path: &[String],
		generic_usages: &mut Vec<GenericUsage>,
		root_layers: &RootLayers<'a>,
		symbols: &Symbols<'a>,
		shape_index: usize,
		invoke_span: Option<Span>,
		function_initial_symbols_len: usize,
		enclosing_generic_parameters: &GenericParameters<'a>,
		type_arguments: &[Node<tree::Type<'a>>],
	) -> Option<TypeId> {
		let mut explicit_arguments = Vec::with_capacity(type_arguments.len() + enclosing_generic_parameters.parameters().len());
		for argument in type_arguments {
			let type_id = self.lookup_type(
				messages,
				function_store,
				module_path,
				generic_usages,
				root_layers,
				symbols,
				function_initial_symbols_len,
				enclosing_generic_parameters,
				argument,
			)?;
			explicit_arguments.push(type_id);
		}

		let mut type_arguments = TypeArguments::new_from_explicit(explicit_arguments);

		let user_type = self.user_types.read()[shape_index].clone();
		let shape = user_type.read();
		assert_eq!(shape.generic_parameters.method_base_len(), 0);
		if shape.generic_parameters.implicit_len() > 0 {
			assert_eq!(shape.generic_parameters.implicit_len(), enclosing_generic_parameters.parameters().len());
			for implicit in enclosing_generic_parameters.parameters().iter().map(|p| p.generic_type_id) {
				type_arguments.push_implicit(implicit);
			}
		}
		drop(shape);

		self.get_or_add_shape_specialization(
			messages,
			function_store,
			module_path,
			generic_usages,
			shape_index,
			invoke_span,
			Ref::new(type_arguments),
		)
	}

	pub fn get_or_add_shape_specialization(
		&mut self,
		messages: &mut Messages<'a>,
		function_store: &FunctionStore<'a>,
		module_path: &[String],
		generic_usages: &mut Vec<GenericUsage>,
		shape_index: usize,
		invoke_span: Option<Span>,
		type_arguments: Ref<TypeArguments>,
	) -> Option<TypeId> {
		let _zone = zone!("user type specialization");

		let user_type = self.user_types.read()[shape_index].clone();
		let shape = user_type.read();
		match &shape.kind {
			UserTypeKind::Struct { .. } => {
				drop(shape);
				self.get_or_add_struct_shape_specialization(
					messages,
					function_store,
					module_path,
					generic_usages,
					shape_index,
					invoke_span,
					type_arguments,
				)
			}

			UserTypeKind::Enum { .. } => {
				drop(shape);
				self.get_or_add_enum_shape_specialization(
					messages,
					function_store,
					module_path,
					generic_usages,
					shape_index,
					invoke_span,
					type_arguments,
				)
			}
		}
	}

	fn get_or_add_struct_shape_specialization(
		&mut self,
		messages: &mut Messages<'a>,
		function_store: &FunctionStore<'a>,
		module_path: &[String],
		generic_usages: &mut Vec<GenericUsage>,
		shape_index: usize,
		invoke_span: Option<Span>,
		type_arguments: Ref<TypeArguments>,
	) -> Option<TypeId> {
		let _zone = zone!("struct specialization");

		let lock = self.user_types.read()[shape_index].clone();
		let mut user_type = lock.read();
		let mut shape = match &user_type.kind {
			UserTypeKind::Struct { shape } => shape,
			kind => unreachable!("{kind:?}"),
		};

		if !shape.been_filled {
			let filling_lock = shape.filling_lock.clone();

			drop(user_type);
			filling_lock.lock();

			user_type = lock.read();
			shape = match &user_type.kind {
				UserTypeKind::Struct { shape } => shape,
				kind => unreachable!("{kind:?}"),
			};
		}

		if type_arguments.explicit_len != user_type.generic_parameters.explicit_len() {
			let error = error!(
				"Expected {} type arguments for struct `{}`, got {}",
				user_type.generic_parameters.explicit_len(),
				user_type.name,
				type_arguments.explicit_len,
			);
			messages.message(error.span_if_some(invoke_span));
			return None;
		}

		if let Some(&specialization_index) = shape.specializations_by_type_arguments.get(&type_arguments) {
			let existing = &shape.specializations[specialization_index];
			return Some(existing.type_id);
		}

		let mut fields = Vec::with_capacity(shape.fields.len());
		for field in &shape.fields {
			fields.push(Field {
				span: Some(field.span),
				name: field.item.name,
				type_id: field.item.field_type,
				attribute: field.item.attribute,
				read_only: field.item.read_only,
			});
		}
		drop(user_type);

		let type_arguments_generic_poisoned = type_arguments
			.ids
			.iter()
			.any(|id| self.type_entries.get(*id).generic_poisoned);

		for field in &mut fields {
			field.type_id = self.specialize_with_user_type_generics(
				messages,
				function_store,
				module_path,
				generic_usages,
				shape_index,
				type_arguments.clone(),
				field.type_id,
			);
		}

		let mut user_type = lock.write();
		let shape = match &mut user_type.kind {
			UserTypeKind::Struct { shape } => shape,
			kind => unreachable!("{kind:?}"),
		};

		let been_filled = shape.been_filled;
		let specialization = Struct {
			type_id: TypeId::unusable(),
			type_arguments: type_arguments.clone(),
			been_filled,
			fields: SliceRef::from(fields),
			layout: None,
		};

		let specialization_index = shape.specializations.len();
		shape.specializations.push(specialization);
		shape
			.specializations_by_type_arguments
			.insert(type_arguments.clone(), specialization_index);
		let entry = TypeEntry {
			kind: TypeEntryKind::UserType { shape_index, specialization_index },
			reference_entries: None,
			generic_poisoned: type_arguments_generic_poisoned,
		};
		let type_id = self.type_entries.push_entry(entry);
		shape.specializations[specialization_index].type_id = type_id;

		drop(user_type);

		if type_arguments_generic_poisoned {
			let usage = GenericUsage::UserType { type_arguments, shape_index };
			generic_usages.push(usage)
		}

		if been_filled {
			self.calculate_layout(type_id);
		}

		Some(type_id)
	}

	fn get_or_add_enum_shape_specialization(
		&mut self,
		messages: &mut Messages<'a>,
		function_store: &FunctionStore<'a>,
		module_path: &[String],
		generic_usages: &mut Vec<GenericUsage>,
		enum_shape_index: usize,
		invoke_span: Option<Span>,
		type_arguments: Ref<TypeArguments>,
	) -> Option<TypeId> {
		let _zone = zone!("enum specialization");

		let lock = self.user_types.read()[enum_shape_index].clone();
		let mut user_type = lock.read();
		let mut shape = match &user_type.kind {
			UserTypeKind::Enum { shape } => shape,
			kind => unreachable!("{kind:?}"),
		};

		if !shape.been_filled {
			let filling_lock = shape.filling_lock.clone();

			drop(user_type);
			filling_lock.lock();

			user_type = lock.read();
			shape = match &user_type.kind {
				UserTypeKind::Enum { shape } => shape,
				kind => unreachable!("{kind:?}"),
			};
		}

		if type_arguments.explicit_len != user_type.generic_parameters.explicit_len() {
			let error = error!(
				"Expected {} type arguments for enum `{}`, got {}",
				user_type.generic_parameters.explicit_len(),
				user_type.name,
				type_arguments.explicit_len,
			);
			messages.message(error.span_if_some(invoke_span));
			return None;
		}

		if let Some(&specialization_index) = shape.specializations_by_type_arguments.get(&type_arguments) {
			let existing = &shape.specializations[specialization_index];
			return Some(existing.type_id);
		}

		let mut shared_fields = Vec::with_capacity(shape.shared_fields.len() + 1);
		let unspecialized_shared_fields = shape.shared_fields.clone();
		drop(user_type);

		let type_arguments_generic_poisoned = type_arguments
			.ids
			.iter()
			.any(|id| self.type_entries.get(*id).generic_poisoned);

		for field in unspecialized_shared_fields.iter() {
			let type_id = self.specialize_with_user_type_generics(
				messages,
				function_store,
				module_path,
				generic_usages,
				enum_shape_index,
				type_arguments.clone(),
				field.item.field_type,
			);

			shared_fields.push(Field {
				span: Some(field.span),
				name: field.item.name,
				type_id,
				attribute: field.item.attribute,
				read_only: field.item.read_only,
			});
		}

		let user_type = lock.read();
		let shape = match &user_type.kind {
			UserTypeKind::Enum { shape } => shape,
			kind => unreachable!("{kind:?}"),
		};
		let variant_shapes: Vec<_> = shape.variant_shapes.iter().copied().collect();
		drop(user_type);

		let mut variants_by_name = FxHashMap::default();
		let mut variants = Vec::new();

		for variant_shape in variant_shapes {
			let mut new_struct_type_arguments = TypeArguments::clone(&type_arguments);

			for struct_type_argument in &mut new_struct_type_arguments.ids {
				let entry = self.type_entries.get(*struct_type_argument);
				match entry.kind {
					TypeEntryKind::UserType { .. } => {
						*struct_type_argument = self.specialize_with_user_type_generics(
							messages,
							function_store,
							module_path,
							generic_usages,
							enum_shape_index,
							type_arguments.clone(),
							*struct_type_argument,
						);
					}

					TypeEntryKind::UserTypeGeneric { .. } => unreachable!(),

					_ => {}
				}
			}

			let type_id = self
				.get_or_add_struct_shape_specialization(
					messages,
					function_store,
					module_path,
					generic_usages,
					variant_shape.struct_shape_index,
					None,
					Ref::new(new_struct_type_arguments),
				)
				.unwrap();

			let span = variant_shape.span;
			let variant_index = variant_shape.variant_index;
			assert_eq!(variant_index, variants.len());
			let is_transparent = variant_shape.is_transparent;
			variants.push(EnumVariant { span, type_id, is_transparent });
			variants_by_name.insert(variant_shape.name, variant_index);
		}

		shared_fields.insert(
			0,
			Field {
				span: None,
				name: "tag",
				type_id: self.u8_type_id,
				attribute: None,
				read_only: true,
			},
		);

		let mut user_type = lock.write();
		let shape = match &mut user_type.kind {
			UserTypeKind::Enum { shape } => shape,
			kind => unreachable!("{kind:?}"),
		};

		let been_filled = shape.been_filled;
		let specialization = Enum {
			type_id: TypeId::unusable(),
			type_arguments: type_arguments.clone(),
			been_filled,
			shared_fields: SliceRef::from(shared_fields),
			variants: SliceRef::from(variants),
			variants_by_name: Ref::new(variants_by_name),
			layout: None,
			tag_memory_size: None,
		};
		let specialization_index = shape.specializations.len();
		shape.specializations.push(specialization);
		shape
			.specializations_by_type_arguments
			.insert(type_arguments.clone(), specialization_index);

		let kind = TypeEntryKind::UserType { shape_index: enum_shape_index, specialization_index };
		let entry = TypeEntry {
			kind,
			reference_entries: None,
			generic_poisoned: type_arguments_generic_poisoned,
		};
		let type_id = self.type_entries.push_entry(entry);

		let shape = match &mut user_type.kind {
			UserTypeKind::Enum { shape } => shape,
			kind => unreachable!("{kind:?}"),
		};
		shape.specializations[specialization_index].type_id = type_id;

		drop(user_type);

		if type_arguments_generic_poisoned {
			let usage = GenericUsage::UserType { type_arguments, shape_index: enum_shape_index };
			generic_usages.push(usage)
		}

		if been_filled {
			self.calculate_layout(type_id);
		}

		Some(type_id)
	}

	pub fn specialize_with_user_type_generics(
		&mut self,
		messages: &mut Messages<'a>,
		function_store: &FunctionStore<'a>,
		module_path: &[String],
		generic_usages: &mut Vec<GenericUsage>,
		type_shape_index: usize,
		type_arguments: Ref<TypeArguments>,
		type_id: TypeId,
	) -> TypeId {
		let entry = self.type_entries.get(type_id);
		match &entry.kind {
			TypeEntryKind::BuiltinType { .. } => type_id,

			TypeEntryKind::UserType { shape_index, specialization_index } => {
				let user_type = self.user_types.read()[*shape_index].clone();
				let user_type = user_type.read();
				match &user_type.kind {
					UserTypeKind::Struct { shape } => {
						let specialization = &shape.specializations[*specialization_index];
						let mut new_struct_type_arguments = TypeArguments::clone(&specialization.type_arguments);
						drop(user_type);

						for struct_type_argument in &mut new_struct_type_arguments.ids {
							let entry = self.type_entries.get(*struct_type_argument);
							match entry.kind {
								TypeEntryKind::UserTypeGeneric { shape_index, generic_index } => {
									assert_eq!(type_shape_index, shape_index);
									*struct_type_argument = type_arguments.ids[generic_index];
								}

								TypeEntryKind::UserType { .. } => {
									*struct_type_argument = self.specialize_with_user_type_generics(
										messages,
										function_store,
										module_path,
										generic_usages,
										type_shape_index,
										type_arguments.clone(),
										*struct_type_argument,
									);
								}

								_ => {}
							}
						}

						self.get_or_add_struct_shape_specialization(
							messages,
							function_store,
							module_path,
							generic_usages,
							*shape_index,
							None,
							Ref::new(new_struct_type_arguments),
						)
						.unwrap()
					}

					UserTypeKind::Enum { shape } => {
						let specialization = &shape.specializations[*specialization_index];
						let mut new_enum_type_arguments = TypeArguments::clone(&specialization.type_arguments);
						drop(user_type);

						for enum_type_argument in &mut new_enum_type_arguments.ids {
							let entry = self.type_entries.get(*enum_type_argument);
							match entry.kind {
								TypeEntryKind::UserTypeGeneric { shape_index, generic_index } => {
									assert_eq!(type_shape_index, shape_index);
									*enum_type_argument = type_arguments.ids[generic_index];
								}

								TypeEntryKind::UserType { .. } => {
									*enum_type_argument = self.specialize_with_user_type_generics(
										messages,
										function_store,
										module_path,
										generic_usages,
										type_shape_index,
										type_arguments.clone(),
										*enum_type_argument,
									);
								}

								_ => {}
							}
						}

						self.get_or_add_enum_shape_specialization(
							messages,
							function_store,
							module_path,
							generic_usages,
							*shape_index,
							None,
							Ref::new(new_enum_type_arguments),
						)
						.unwrap()
					}
				}
			}

			TypeEntryKind::Pointer { type_id, mutable } => {
				let type_id = self.specialize_with_user_type_generics(
					messages,
					function_store,
					module_path,
					generic_usages,
					type_shape_index,
					type_arguments,
					*type_id,
				);
				self.pointer_to(type_id, *mutable)
			}

			TypeEntryKind::Slice(Slice { type_id, mutable }) => {
				let type_id = self.specialize_with_user_type_generics(
					messages,
					function_store,
					module_path,
					generic_usages,
					type_shape_index,
					type_arguments,
					*type_id,
				);
				self.slice_of(type_id, *mutable)
			}

			&TypeEntryKind::UserTypeGeneric { shape_index, generic_index } => {
				assert_eq!(type_shape_index, shape_index);
				type_arguments.ids[generic_index]
			}

			&TypeEntryKind::FunctionGeneric { function_shape_index, generic_index } => {
				function_store.shapes.read()[function_shape_index]
					.as_ref()
					.unwrap()
					.read()
					.generic_parameters
					.parameters()[generic_index]
					.generic_type_id
			}
		}
	}

	pub fn specialize_with_function_generics(
		&mut self,
		messages: &mut Messages<'a>,
		function_store: &FunctionStore<'a>,
		module_path: &[String],
		generic_usages: &mut Vec<GenericUsage>,
		function_shape_index: usize,
		function_type_arguments: &TypeArguments,
		type_id: TypeId,
	) -> TypeId {
		let entry = self.type_entries.get(type_id);
		match &entry.kind {
			TypeEntryKind::BuiltinType { .. } => type_id,

			TypeEntryKind::UserType { shape_index, specialization_index } => {
				let user_type = self.user_types.read()[*shape_index].clone();
				let user_type = user_type.read();
				match &user_type.kind {
					UserTypeKind::Struct { shape } => {
						let specialization = &shape.specializations[*specialization_index];
						let mut new_struct_type_arguments = TypeArguments::clone(&specialization.type_arguments);
						drop(user_type);

						for struct_type_argument in &mut new_struct_type_arguments.ids {
							let entry = self.type_entries.get(*struct_type_argument);
							match entry.kind {
								TypeEntryKind::UserTypeGeneric { .. } => unreachable!(),

								TypeEntryKind::FunctionGeneric { function_shape_index: shape_index, generic_index } => {
									assert_eq!(function_shape_index, shape_index);
									*struct_type_argument = function_type_arguments.ids[generic_index];
								}

								TypeEntryKind::UserType { .. } => {
									*struct_type_argument = self.specialize_with_function_generics(
										messages,
										function_store,
										module_path,
										generic_usages,
										function_shape_index,
										function_type_arguments,
										*struct_type_argument,
									);
								}

								_ => {}
							}
						}

						self.get_or_add_struct_shape_specialization(
							messages,
							function_store,
							module_path,
							generic_usages,
							*shape_index,
							None,
							Ref::new(new_struct_type_arguments),
						)
						.unwrap()
					}

					UserTypeKind::Enum { shape } => {
						let specialization = &shape.specializations[*specialization_index];
						let any_function_generic = specialization
							.type_arguments
							.ids
							.iter()
							.any(|t| matches!(self.type_entries.get(*t).kind, TypeEntryKind::FunctionGeneric { .. }));

						if !any_function_generic {
							return type_id;
						}

						let type_arguments = specialization.type_arguments.clone();
						drop(user_type);

						let mut new_enum_type_arguments = TypeArguments::clone(&type_arguments);
						for enum_type_argument in &mut new_enum_type_arguments.ids {
							let entry = self.type_entries.get(*enum_type_argument);
							match &entry.kind {
								TypeEntryKind::UserTypeGeneric { .. } => unreachable!(),

								TypeEntryKind::FunctionGeneric { function_shape_index: shape_index, generic_index } => {
									assert_eq!(function_shape_index, *shape_index);
									*enum_type_argument = function_type_arguments.ids[*generic_index];
								}

								_ => {}
							}
						}

						self.get_or_add_enum_shape_specialization(
							messages,
							function_store,
							module_path,
							generic_usages,
							*shape_index,
							None,
							Ref::new(new_enum_type_arguments),
						)
						.unwrap()
					}
				}
			}

			TypeEntryKind::Pointer { type_id, mutable } => {
				let type_id = self.specialize_with_function_generics(
					messages,
					function_store,
					module_path,
					generic_usages,
					function_shape_index,
					function_type_arguments,
					*type_id,
				);
				self.pointer_to(type_id, *mutable)
			}

			TypeEntryKind::Slice(Slice { type_id, mutable }) => {
				let type_id = self.specialize_with_function_generics(
					messages,
					function_store,
					module_path,
					generic_usages,
					function_shape_index,
					function_type_arguments,
					*type_id,
				);
				self.slice_of(type_id, *mutable)
			}

			&TypeEntryKind::UserTypeGeneric { shape_index, generic_index } => {
				// TODO: This could have unintended consequences
				self.user_types.read()[shape_index].read().generic_parameters.parameters()[generic_index].generic_type_id
			}

			&TypeEntryKind::FunctionGeneric { function_shape_index: shape_index, generic_index } => {
				assert_eq!(function_shape_index, shape_index);
				function_type_arguments.ids[generic_index]
			}
		}
	}

	pub fn type_name(&mut self, function_store: &FunctionStore, module_path: &[String], type_id: TypeId) -> String {
		let name = self.internal_type_name(Some(function_store), module_path, type_id, self.debug_generics, self.debug_type_ids);
		format!("`{name}`")
	}

	#[allow(unused)]
	pub fn debugging_type_name(&mut self, type_id: TypeId) -> String {
		format!(
			"`{} aka {}`",
			self.internal_type_name(None, &[], type_id, true, false),
			self.internal_type_name(None, &[], type_id, false, true)
		)
	}

	// TODO: Use module path to have or not have paths for local types?
	fn internal_type_name(
		&mut self,
		function_store: Option<&FunctionStore>,
		_module_path: &[String],
		type_id: TypeId,
		debug_generics: bool,
		debug_type_ids: bool,
	) -> String {
		if debug_type_ids {
			return format!("{}", type_id.index());
		}

		let type_entry = self.type_entries.get(type_id);
		match type_entry.kind {
			TypeEntryKind::BuiltinType { kind } => kind.name().to_owned(),

			TypeEntryKind::UserType { shape_index, specialization_index } => {
				let user_type = self.user_types.read()[shape_index].clone();
				let user_type = user_type.read();
				let user_type_name = user_type.name;
				let explicit_generic_parameters_len = user_type.generic_parameters.explicit_len();

				match &user_type.kind {
					UserTypeKind::Struct { shape } => {
						let parent_enum_shape_index = shape.parent_enum_shape_index;
						let specialization = &shape.specializations[specialization_index];
						let type_arguments = specialization.type_arguments.ids[0..explicit_generic_parameters_len].to_vec();
						drop(user_type);

						let type_arguments = type_arguments
							.iter()
							.map(|a| self.internal_type_name(function_store, _module_path, *a, debug_generics, debug_type_ids))
							.collect::<Vec<_>>()
							.join(", ");

						if let Some(parent_enum_shape_index) = parent_enum_shape_index {
							let enum_name = self.user_types.read()[parent_enum_shape_index].read().name;
							if explicit_generic_parameters_len == 0 {
								format!("{enum_name}.{}", user_type_name)
							} else {
								format!("{enum_name}.{}<{}>", user_type_name, type_arguments)
							}
						} else {
							if explicit_generic_parameters_len == 0 {
								user_type_name.to_owned()
							} else {
								format!("{}<{}>", user_type_name, type_arguments)
							}
						}
					}

					UserTypeKind::Enum { shape } => {
						if explicit_generic_parameters_len == 0 {
							return user_type_name.to_owned();
						}

						let specialization = &shape.specializations[specialization_index];
						let type_arguments = specialization.type_arguments.ids[0..explicit_generic_parameters_len].to_vec();
						drop(user_type);
						let type_arguments = type_arguments
							.iter()
							.map(|a| self.internal_type_name(function_store, _module_path, *a, debug_generics, debug_type_ids))
							.collect::<Vec<_>>()
							.join(", ");

						format!("{}<{}>", user_type_name, type_arguments)
					}
				}
			}

			TypeEntryKind::Pointer { type_id, mutable } => {
				let inner = self.internal_type_name(function_store, _module_path, type_id, debug_generics, debug_type_ids);
				match mutable {
					true => format!("&mut {}", inner),
					false => format!("&{}", inner),
				}
			}

			TypeEntryKind::Slice(Slice { type_id, mutable }) => {
				let inner = self.internal_type_name(function_store, _module_path, type_id, debug_generics, debug_type_ids);
				match mutable {
					true => format!("[]mut {}", inner),
					false => format!("[]{}", inner),
				}
			}

			TypeEntryKind::FunctionGeneric { function_shape_index, generic_index } => {
				if let Some(function_store) = function_store {
					let generic = function_store.shapes.read()[function_shape_index]
						.as_ref()
						.unwrap()
						.read()
						.generic_parameters
						.parameters()[generic_index];

					if debug_generics {
						format!("FunctionGeneric {function_shape_index} {generic_index} {}", generic.name.item)
					} else {
						generic.name.item.to_owned()
					}
				} else {
					if debug_generics {
						format!("FunctionGeneric {function_shape_index} {generic_index}")
					} else {
						String::from("FunctionGeneric")
					}
				}
			}

			TypeEntryKind::UserTypeGeneric { shape_index, generic_index } => {
				let user_type = &self.user_types.read()[shape_index];
				let generic = user_type.read().generic_parameters.parameters()[generic_index];

				if debug_generics {
					format!("UserTypeGeneric {shape_index} {generic_index} {}", generic.name.item)
				} else {
					generic.name.item.to_owned()
				}
			}
		}
	}
}
