use std::collections::hash_map;

use rust_decimal::Decimal;
use rust_decimal_macros::dec;
use rustc_hash::FxHashMap;

use crate::frontend::error::Messages;
use crate::frontend::function_store::{FunctionStore, MethodBaseType};
use crate::frontend::ir::{
	EnumVariantToEnum, Expression, ExpressionKind, GenericConstraint, GenericParameters, GenericUsage, ScopeId,
	SliceMutableToImmutable, StringToFormatString, TypeArguments,
};
use crate::frontend::root_layers::RootLayers;
use crate::frontend::span::Span;
use crate::frontend::symbols::{Symbol, SymbolKind, Symbols};
use crate::frontend::tree::{self, FieldAttribute, MethodKind, Node};
use crate::frontend::validator;
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

	pub fn is_untyped_number(self, type_store: &TypeStore) -> bool {
		type_store.direct_match(self, type_store.number_type_id)
	}

	pub fn is_numeric(self, type_store: &TypeStore) -> bool {
		let range = type_store.number_type_id.entry..=type_store.f64_type_id.entry;
		range.contains(&self.entry) || self.is_any_collapse(type_store)
	}

	pub fn is_integer(self, type_store: &TypeStore, expression: &Expression) -> bool {
		let range = type_store.i8_type_id.entry..=type_store.usize_type_id.entry;
		range.contains(&self.entry)
			|| self.is_any_collapse(type_store)
			|| match &expression.kind {
				ExpressionKind::NumberValue(value) => value.is_integer(),
				_ => false,
			}
	}

	pub fn is_bool(self, type_store: &TypeStore) -> bool {
		type_store.direct_match(self, type_store.bool_type_id)
	}

	pub fn is_string(self, type_store: &TypeStore) -> bool {
		type_store.direct_match(self, type_store.string_type_id)
	}

	pub fn is_format_string(self, type_store: &TypeStore) -> bool {
		type_store.direct_match(self, type_store.format_string_type_id)
	}

	pub fn is_opaque(self, type_store: &mut TypeStore) -> bool {
		let type_entry = type_store.type_entries.get(self);
		if let TypeEntryKind::UserType { shape_index, .. } = type_entry.kind {
			let user_types = type_store.user_types.read();
			let shape = user_types[shape_index].as_ref().read();
			if let UserTypeKind::Struct { shape } = &shape.kind {
				return shape.opaque;
			}
		}

		false
	}

	pub fn is_formattable(self, type_store: &TypeStore, expression: &Expression) -> bool {
		let range = type_store.i8_type_id.entry..=type_store.format_string_type_id.entry;
		range.contains(&self.entry)
			|| self.is_any_collapse(type_store)
			|| match &expression.kind {
				ExpressionKind::NumberValue(value) => match value.collapsed() {
					Some(collapsed) => range.contains(&collapsed.entry),
					None => false,
				},
				_ => false,
			}
	}

	pub fn format_item_variant_index(self, type_store: &TypeStore, expression: Option<&Expression>) -> usize {
		let entry = match expression {
			Some(Expression { kind: ExpressionKind::NumberValue(value), .. }) => match value.collapsed() {
				Some(collapsed) => collapsed.entry,
				None => self.entry,
			},

			_ => self.entry,
		};

		let range = type_store.i8_type_id.entry..=type_store.format_string_type_id.entry;
		assert!(range.contains(&entry), "{range:?}, {}", entry);
		(entry - type_store.i8_type_id.entry) as usize
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
			TypeEntryKind::Module | TypeEntryKind::Type | TypeEntryKind::BuiltinType { .. } | TypeEntryKind::Pointer { .. } => {
				true
			}

			TypeEntryKind::UserType { .. }
			| TypeEntryKind::Slice(_)
			| TypeEntryKind::UserTypeGeneric { .. }
			| TypeEntryKind::FunctionGeneric { .. }
			| TypeEntryKind::TraitGeneric { .. } => false,
		}
	}

	pub fn is_comparable(self, type_store: &mut TypeStore) -> bool {
		use PrimativeKind::*;

		let entry = type_store.type_entries.get(self);
		match entry.kind {
			TypeEntryKind::BuiltinType { kind: Void | UntypedNumber | Bool | Numeric(_), .. } => true,
			TypeEntryKind::Pointer { .. } => true,
			_ => false,
		}
	}

	pub fn as_struct<'a, R, F: FnOnce(&StructShape, &Struct) -> R>(self, type_store: &mut TypeStore<'a>, func: F) -> Option<R> {
		let entry = type_store.type_entries.get(self);
		if let TypeEntryKind::UserType { shape_index, specialization_index, .. } = entry.kind {
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

impl Layout {
	pub fn tag_memory_size(self) -> i64 {
		// The one is the single guarenteed byte
		// TODO: Update this once user can request larger tag
		// Maybe it should just be a field, we'll see
		self.alignment.max(1)
	}
}

pub struct RegisterTypeResult {
	pub shape_index: usize,
	pub methods_index: usize,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct TraitId {
	pub shape_index: u32,
	pub specialization_index: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImplementationStatus {
	NotImplemented,
	Implemented,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ActualMethodInfo {
	pub method_index: usize,
	pub specialization_index: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ActualMethodIndices {
	pub actual_method_indices: Vec<ActualMethodInfo>,
}

#[derive(Debug, Default)]
pub struct ImplementationInfo {
	pub statuses: FxHashMap<TraitId, ImplementationStatus>,
	pub actual_indices: FxHashMap<usize, ActualMethodIndices>,
}

#[derive(Debug)]
pub struct TraitShape<'a> {
	// See comment on `filling_lock` field of `StructShape` for details
	pub filling_lock: Ref<ReentrantMutex<()>>,
	pub been_filled: bool,

	pub name: &'a str,
	pub generic_parameters: GenericParameters<'a>,
	pub methods: Vec<TraitMethod<'a>>,

	pub specializations_by_type_arguments: FxHashMap<Ref<TypeArguments>, usize>,
	pub specializations: Vec<Trait<'a>>,
}

impl<'a> TraitShape<'a> {
	pub fn new(name: &'a str, generic_parameters: GenericParameters<'a>) -> Self {
		TraitShape {
			filling_lock: Ref::new(ReentrantMutex::new(())),
			been_filled: false,
			name,
			generic_parameters,
			methods: Vec::new(),
			specializations_by_type_arguments: FxHashMap::default(),
			specializations: Vec::new(),
		}
	}
}

#[derive(Debug, Clone)]
pub struct Trait<'a> {
	pub trait_id: TraitId,
	pub type_arguments: Ref<TypeArguments>,
	pub been_filled: bool,
	pub methods: SliceRef<TraitMethod<'a>>,
}

#[derive(Debug, Clone)]
pub struct TraitMethod<'a> {
	pub kind: Node<MethodKind>,
	pub name: Node<&'a str>,
	pub parameters: Vec<TraitParameter>,
	pub return_type: TypeId,
	pub fake_function_shape_index: usize,
}

impl<'a> TraitMethod<'a> {
	pub fn specialize_with_trait_generics(
		&mut self,
		messages: &mut Messages<'a>,
		type_store: &mut TypeStore<'a>,
		function_store: &FunctionStore<'a>,
		module_path: &'a [String],
		generic_usages: &mut Vec<GenericUsage>,
		enclosing_generic_parameters: &GenericParameters<'a>,
		trait_shape_index: usize,
		trait_type_arguments: &TypeArguments,
	) {
		for parameter in &mut self.parameters {
			parameter.type_id = type_store.specialize_type_id_with_generics(
				messages,
				function_store,
				module_path,
				generic_usages,
				enclosing_generic_parameters,
				parameter.type_id,
				&trait_type_arguments,
				TypeIdSpecializationSituation::Trait { trait_shape_index },
			);
		}

		self.return_type = type_store.specialize_type_id_with_generics(
			messages,
			function_store,
			module_path,
			generic_usages,
			enclosing_generic_parameters,
			self.return_type,
			&trait_type_arguments,
			TypeIdSpecializationSituation::Trait { trait_shape_index },
		);
	}
}

#[derive(Debug, Clone)]
pub struct TraitParameter {
	pub type_id: TypeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StructParentKind {
	None,
	Enum,
	Union,
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

	pub opaque: bool,
	pub fields: Vec<Node<FieldShape<'a>>>,

	pub parent_kind: StructParentKind,
	pub parent_shape_index: usize,
	pub variant_index: Option<usize>,
	pub is_transparent_variant: bool,

	pub specializations_by_type_arguments: FxHashMap<Ref<TypeArguments>, usize>,
	pub specializations: Vec<Struct<'a>>,
}

impl<'a> StructShape<'a> {
	pub fn new(
		opaque: bool,
		parent_kind: StructParentKind,
		parent_shape_index: usize,
		variant_index: Option<usize>,
		is_transparent_variant: bool,
	) -> Self {
		StructShape {
			filling_lock: Ref::new(ReentrantMutex::new(())),
			been_filled: false,
			opaque,
			fields: Vec::new(),
			parent_kind,
			parent_shape_index,
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
	pub generic_poisoned: bool,
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
	pub methods_index: usize,
	pub is_transparent: bool,
}

#[derive(Debug, Clone)]
pub struct Enum<'a> {
	pub type_id: TypeId,
	pub generic_poisoned: bool,
	pub type_arguments: Ref<TypeArguments>,
	pub been_filled: bool,
	pub shared_fields: SliceRef<Field<'a>>,
	pub variants: SliceRef<Variant>,
	pub variants_by_name: Ref<FxHashMap<&'a str, usize>>, // Index into variants vec
	pub layout: Option<Layout>,
}

#[derive(Debug, Clone, Copy)]
pub struct Variant {
	pub span: Span,
	pub type_id: TypeId,
	pub is_transparent: bool,
}

#[allow(unused)] // TODO: Finish implementing unions
#[derive(Debug)]
pub struct UnionShape<'a> {
	// See comment on `filling_lock` field of `StructShape` for details
	pub filling_lock: Ref<ReentrantMutex<()>>,
	pub been_filled: bool,

	pub variant_shapes: SliceRef<UnionVariantShape<'a>>,

	pub specializations_by_type_arguments: FxHashMap<Ref<TypeArguments>, usize>,
	pub specializations: Vec<Union<'a>>,
}

#[allow(unused)] // TODO: Finish implementing unions
impl<'a> UnionShape<'a> {
	pub fn new(variant_shapes: Vec<UnionVariantShape<'a>>) -> Self {
		UnionShape {
			filling_lock: Ref::new(ReentrantMutex::new(())),
			been_filled: false,
			variant_shapes: SliceRef::from(variant_shapes),
			specializations_by_type_arguments: FxHashMap::default(),
			specializations: Vec::new(),
		}
	}
}

#[allow(unused)] // TODO: Finish implementing unions
#[derive(Debug, Clone)]
pub struct Union<'a> {
	pub type_id: TypeId,
	pub generic_poisoned: bool,
	pub type_arguments: Ref<TypeArguments>,
	pub been_filled: bool,
	pub fields: SliceRef<Field<'a>>,
	pub variants: SliceRef<Variant>,
	pub variants_by_name: Ref<FxHashMap<&'a str, usize>>, // Index into variants vec
	pub layout: Option<Layout>,
}

#[allow(unused)] // TODO: Finish implementing unions
#[derive(Debug, Clone, Copy)]
pub struct UnionVariantShape<'a> {
	pub name: &'a str,
	pub span: Span,
	pub variant_index: usize,
	pub struct_shape_index: usize,
	pub methods_index: usize,
	pub is_transparent: bool,
}

#[derive(Debug)]
pub struct UserType<'a> {
	pub name: &'a str,
	pub span: Span,
	pub scope_id: ScopeId,
	pub generic_parameters: GenericParameters<'a>,
	pub methods_index: usize,
	pub kind: UserTypeKind<'a>,
}

#[derive(Debug)]
pub struct MethodCollection<'a> {
	pub methods_by_name: FxHashMap<&'a str, usize>, // Indicies into methods vec below
	pub methods: Vec<MethodInfo>,
}

impl<'a> MethodCollection<'a> {
	pub fn blank() -> Self {
		MethodCollection { methods_by_name: FxHashMap::default(), methods: Vec::new() }
	}

	pub fn for_generic_satisfying_constraints(type_store: &TypeStore<'a>, constraints: &[TraitId]) -> Self {
		let mut methods_by_name = FxHashMap::default();
		let mut methods = Vec::new();

		let traits = type_store.traits.read();
		for constraint in constraints {
			let trait_shape = traits[constraint.shape_index as usize].as_ref().unwrap().read();
			let trait_instance = &trait_shape.specializations[constraint.specialization_index as usize];
			for trait_method in trait_instance.methods.iter() {
				let function_shape_index = trait_method.fake_function_shape_index;
				let kind = trait_method.kind;
				let info = MethodInfo { function_shape_index, kind };

				let index = methods.len();
				methods.push(info);
				// TODO: Detect duplicate trait methods
				assert!(methods_by_name.insert(trait_method.name.item, index).is_none());
			}
		}

		MethodCollection { methods_by_name, methods }
	}
}

#[derive(Debug, Clone, Copy)]
pub struct MethodInfo {
	pub function_shape_index: usize,
	pub kind: Node<MethodKind>,
}

#[allow(unused)] // TODO: Finish implementing unions
#[derive(Debug)]
pub enum UserTypeKind<'a> {
	Struct { shape: StructShape<'a> },
	Enum { shape: EnumShape<'a> },
	Union { shape: UnionShape<'a> },
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

	UntypedNumber,

	Bool,
	Numeric(NumericKind),
	String,
	FormatString,
}

impl PrimativeKind {
	pub fn name(self) -> &'static str {
		match self {
			PrimativeKind::AnyCollapse => "any collapse",
			PrimativeKind::NoReturn => "noreturn",
			PrimativeKind::Void => "void",
			PrimativeKind::UntypedNumber => "untyped number",
			PrimativeKind::Bool => "bool",
			PrimativeKind::Numeric(numeric) => numeric.name(),
			PrimativeKind::String => "str",
			PrimativeKind::FormatString => "fstr",
		}
	}

	pub fn layout(self) -> Layout {
		match self {
			PrimativeKind::AnyCollapse => Layout { size: 0, alignment: 1 },
			PrimativeKind::NoReturn => Layout { size: 0, alignment: 1 },
			PrimativeKind::Void => Layout { size: 0, alignment: 1 },
			PrimativeKind::UntypedNumber => unreachable!(),
			PrimativeKind::Bool => Layout { size: 1, alignment: 1 },
			PrimativeKind::Numeric(numeric) => numeric.layout(),
			PrimativeKind::String | PrimativeKind::FormatString => Layout { size: 8 * 2, alignment: 8 },
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

			TypeEntryKind::UserType { shape_index, specialization_index, .. } => {
				let user_type = type_store.user_types.read()[shape_index].clone();
				let user_type = user_type.read();
				match &user_type.kind {
					UserTypeKind::Struct { shape } => {
						let specialization = &shape.specializations[specialization_index];
						specialization
							.type_arguments
							.ids
							.iter()
							.any(|t| type_store.type_entries.get(t.item).generic_poisoned)
					}

					UserTypeKind::Enum { shape } => {
						let specialization = &shape.specializations[specialization_index];
						specialization
							.type_arguments
							.ids
							.iter()
							.any(|t| type_store.type_entries.get(t.item).generic_poisoned)
					}

					UserTypeKind::Union { shape } => {
						let specialization = &shape.specializations[specialization_index];
						specialization
							.type_arguments
							.ids
							.iter()
							.any(|t| type_store.type_entries.get(t.item).generic_poisoned)
					}
				}
			}

			TypeEntryKind::Pointer { type_id, .. } | TypeEntryKind::Slice(Slice { type_id, .. }) => {
				let entry = type_store.type_entries.get(type_id);
				entry.generic_poisoned
			}

			TypeEntryKind::UserTypeGeneric { .. }
			| TypeEntryKind::FunctionGeneric { .. }
			| TypeEntryKind::TraitGeneric { .. } => true,

			TypeEntryKind::Module | TypeEntryKind::Type => unreachable!(),
		};

		TypeEntry { kind, reference_entries: None, generic_poisoned }
	}
}

#[derive(Debug, Clone, Copy)]
pub enum TypeEntryKind {
	Module,
	Type,

	BuiltinType {
		kind: PrimativeKind,
		methods_index: usize,
	},

	UserType {
		shape_index: usize,
		specialization_index: usize,
		methods_index: usize,
	},

	Pointer {
		type_id: TypeId,
		mutable: bool,
	},

	Slice(Slice),

	UserTypeGeneric {
		shape_index: usize,
		generic_index: usize,
		methods_index: usize,
	},

	FunctionGeneric {
		function_shape_index: usize,
		generic_index: usize,
		methods_index: usize,
		is_trait_self: bool,
		originator_trait_shape_index: usize,
	},

	TraitGeneric {
		trait_shape_index: usize,
		generic_index: usize,
		methods_index: usize,
		is_self: bool,
	},
}

impl TypeEntryKind {
	#[allow(dead_code)] // TODO: Do we need to keep this around?
	pub fn name(self) -> &'static str {
		match self {
			TypeEntryKind::Module => "module",
			TypeEntryKind::Type | TypeEntryKind::BuiltinType { .. } | TypeEntryKind::UserType { .. } => "type",
			TypeEntryKind::Pointer { .. } => "pointer",
			TypeEntryKind::Slice(_) => "slice",
			TypeEntryKind::UserTypeGeneric { .. } => "type generic",
			TypeEntryKind::FunctionGeneric { .. } => "function generic",
			TypeEntryKind::TraitGeneric { .. } => "trait generic",
		}
	}

	pub fn methods_index(self) -> Option<usize> {
		use TypeEntryKind::*;

		match self {
			BuiltinType { methods_index, .. }
			| UserType { methods_index, .. }
			| UserTypeGeneric { methods_index, .. }
			| FunctionGeneric { methods_index, .. } => Some(methods_index),

			_ => None,
		}
	}
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
	pub method_collections: Ref<RwLock<Vec<Ref<RwLock<MethodCollection<'a>>>>>>,
	pub traits: Ref<RwLock<Vec<Option<Ref<RwLock<TraitShape<'a>>>>>>>,
	pub implementations: Ref<RwLock<Vec<RwLock<ImplementationInfo>>>>, // Methods index -> Implementation info

	module_type_id: TypeId,
	type_type_id: TypeId,
	any_collapse_type_id: TypeId,
	noreturn_type_id: TypeId,
	void_type_id: TypeId,

	number_type_id: TypeId,

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
	format_string_type_id: TypeId,

	u8_slice_type_id: TypeId,
}

#[derive(Debug, Clone, Copy)]
pub enum TypeIdSpecializationSituation {
	UserType { user_type_shape_index: usize },
	Function { function_shape_index: usize },
	Trait { trait_shape_index: usize },
}

impl<'a> TypeStore<'a> {
	pub fn new(debug_generics: bool, debug_type_ids: bool) -> Self {
		let mut primative_type_symbols = Vec::new();
		let mut type_entries = TypeEntries::new();
		let mut method_collections = Vec::new();
		let mut implementations = Vec::new();

		let module_type_id = {
			let kind = TypeEntryKind::Module;
			let type_entry = TypeEntry { kind, reference_entries: None, generic_poisoned: false };
			type_entries.push_entry(type_entry)
		};

		let type_type_id = {
			let kind = TypeEntryKind::Type;
			let type_entry = TypeEntry { kind, reference_entries: None, generic_poisoned: false };
			type_entries.push_entry(type_entry)
		};

		let mut push_primative = |name: Option<&'a str>, kind| {
			let methods_index = method_collections.len();
			method_collections.push(Ref::new(RwLock::new(MethodCollection::blank())));

			assert_eq!(implementations.len(), methods_index);
			implementations.push(RwLock::new(ImplementationInfo::default()));

			let kind = TypeEntryKind::BuiltinType { kind, methods_index };
			let type_entry = TypeEntry { kind, reference_entries: None, generic_poisoned: false };
			let type_id = type_entries.push_entry(type_entry);

			if let Some(name) = name {
				let kind = SymbolKind::BuiltinType { type_id, methods_index };
				let symbol = Symbol { name, kind, span: None, used: true, imported: false };
				primative_type_symbols.push(symbol);
			}

			type_id
		};

		let any_collapse_type_id = push_primative(None, PrimativeKind::AnyCollapse);
		let noreturn_type_id = push_primative(Some("noreturn"), PrimativeKind::NoReturn);
		let void_type_id = push_primative(Some("void"), PrimativeKind::Void);

		// NOTE: These numeric type ids must all be generated together for the `is_numeric` range check
		let number_type_id = push_primative(None, PrimativeKind::UntypedNumber);

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
		let format_string_type_id = push_primative(Some("fstr"), PrimativeKind::FormatString);

		let mut type_store = TypeStore {
			debug_generics,
			debug_type_ids,
			primative_type_symbols: SliceRef::from(primative_type_symbols),
			type_entries,
			user_types: Ref::new(RwLock::new(Vec::new())),
			method_collections: Ref::new(RwLock::new(method_collections)),
			traits: Ref::new(RwLock::new(Vec::new())),
			implementations: Ref::new(RwLock::new(implementations)),
			module_type_id,
			type_type_id,
			any_collapse_type_id,
			noreturn_type_id,
			void_type_id,
			number_type_id,
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
			format_string_type_id,
			u8_slice_type_id: TypeId { entry: u32::MAX },
		};

		// This is a hack, consider moving the saved type ids into a different struct
		let u8_slice_type_id = type_store.slice_of(u8_type_id, false);
		type_store.u8_slice_type_id = u8_slice_type_id;

		type_store
	}

	pub fn module_type_id(&self) -> TypeId {
		self.module_type_id
	}

	pub fn type_type_id(&self) -> TypeId {
		self.type_type_id
	}

	pub fn any_collapse_type_id(&self) -> TypeId {
		self.any_collapse_type_id
	}

	pub fn void_type_id(&self) -> TypeId {
		self.void_type_id
	}

	pub fn number_type_id(&self) -> TypeId {
		self.number_type_id
	}

	pub fn i8_type_id(&self) -> TypeId {
		self.i8_type_id
	}

	pub fn i16_type_id(&self) -> TypeId {
		self.i16_type_id
	}

	pub fn i32_type_id(&self) -> TypeId {
		self.i32_type_id
	}

	pub fn i64_type_id(&self) -> TypeId {
		self.i64_type_id
	}

	pub fn u8_type_id(&self) -> TypeId {
		self.u8_type_id
	}

	pub fn u16_type_id(&self) -> TypeId {
		self.u16_type_id
	}

	pub fn u32_type_id(&self) -> TypeId {
		self.u32_type_id
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

	pub fn f32_type_id(&self) -> TypeId {
		self.f32_type_id
	}

	pub fn f64_type_id(&self) -> TypeId {
		self.f64_type_id
	}

	pub fn bool_type_id(&self) -> TypeId {
		self.bool_type_id
	}

	pub fn string_type_id(&self) -> TypeId {
		self.string_type_id
	}

	pub fn format_string_type_id(&self) -> TypeId {
		self.format_string_type_id
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

		// if either are any collapse then collapse to any collapse
		// if either type is an untyped number we know the other isn't, collapse to the other type
		// if either type is a pointer, collapse the other to it, preferring to collapse mutable to immutable
		// if either type is a slice, collapse the other to it, preferring to collapse mutable to immutable
		// if either type is an enum, collapse the other to it
		// if either type is a transparent variant, collaps it to the other

		if a.type_id.entry == self.any_collapse_type_id.entry {
			return Ok(self.any_collapse_type_id);
		} else if b.type_id.entry == self.any_collapse_type_id.entry {
			return Ok(self.any_collapse_type_id);
		}

		let a_number = a.type_id.entry == self.number_type_id.entry;
		let b_number = b.type_id.entry == self.number_type_id.entry;

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

		let a_entry = self.type_entries.get(a.type_id);
		let b_entry = self.type_entries.get(b.type_id);

		if let TypeEntryKind::Pointer { mutable: a_mutable, .. } = a_entry.kind {
			let collapsed = if let TypeEntryKind::Pointer { mutable: b_mutable, .. } = b_entry.kind {
				if a_mutable && !b_mutable {
					self.collapse_to(messages, function_store, b.type_id, a)?
				} else {
					self.collapse_to(messages, function_store, a.type_id, b)?
				}
			} else {
				self.collapse_to(messages, function_store, a.type_id, b)?
			};

			return match collapsed {
				true => Ok(a.type_id),
				false => Err(()),
			};
		}

		if matches!(b_entry.kind, TypeEntryKind::Pointer { .. }) {
			// No need to check for mutability and prefer a different direction because if both
			// are pointers then that case would have been caught by the `a` block
			let collapsed = self.collapse_to(messages, function_store, b.type_id, a)?;
			return match collapsed {
				true => Ok(b.type_id),
				false => Err(()),
			};
		}

		if let TypeEntryKind::Slice(a_slice) = a_entry.kind {
			let collapsed = if let TypeEntryKind::Slice(b_slice) = b_entry.kind {
				if a_slice.mutable && !b_slice.mutable {
					self.collapse_to(messages, function_store, b.type_id, a)?
				} else {
					self.collapse_to(messages, function_store, a.type_id, b)?
				}
			} else {
				self.collapse_to(messages, function_store, a.type_id, b)?
			};

			return match collapsed {
				true => Ok(a.type_id),
				false => Err(()),
			};
		}

		if matches!(b_entry.kind, TypeEntryKind::Slice(_)) {
			// No need to check for mutability and prefer a different direction because if both
			// are slices then that case would have been caught by the `a` block
			let collapsed = self.collapse_to(messages, function_store, b.type_id, a)?;
			return match collapsed {
				true => Ok(b.type_id),
				false => Err(()),
			};
		}

		let user_types = self.user_types.read();
		let is_enum = |entry: TypeEntry| match entry.kind {
			TypeEntryKind::UserType { shape_index, .. } => match user_types[shape_index].read().kind {
				UserTypeKind::Enum { .. } => return true,
				_ => return false,
			},
			_ => return false,
		};

		if is_enum(a_entry) {
			drop(user_types);
			let collapsed = self.collapse_to(messages, function_store, a.type_id, b)?;
			return match collapsed {
				true => Ok(a.type_id),
				false => Err(()),
			};
		}

		if is_enum(b_entry) {
			drop(user_types);
			let collapsed = self.collapse_to(messages, function_store, b.type_id, a)?;
			return match collapsed {
				true => Ok(b.type_id),
				false => Err(()),
			};
		}

		let is_transparent_variant = |entry: TypeEntry| match entry.kind {
			TypeEntryKind::UserType { shape_index, .. } => match &user_types[shape_index].read().kind {
				UserTypeKind::Struct { shape } => return shape.is_transparent_variant,
				_ => return false,
			},
			_ => return false,
		};

		if is_transparent_variant(a_entry) {
			drop(user_types);
			let collapsed = self.collapse_to(messages, function_store, b.type_id, a)?;
			return match collapsed {
				true => Ok(b.type_id),
				false => Err(()),
			};
		}

		if is_transparent_variant(b_entry) {
			drop(user_types);
			let collapsed = self.collapse_to(messages, function_store, a.type_id, b)?;
			return match collapsed {
				true => Ok(a.type_id),
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
		// untyped number -> signed of large enough if whole number | unsigned of large enough if not negative whole number | float of large enough
		// mutable reference -> immutable reference
		// mutable slice -> immutable slice
		// str -> fstr
		// enum/union variant -> enum/union
		// transparent variant -> its wrapped type

		let any_collapse_entry = self.any_collapse_type_id.entry;
		if from.type_id.entry == any_collapse_entry || to.entry == any_collapse_entry {
			// From or to any collapse
			// No need to convert anything, this only gets introduced in case of error so we know we won't codegen
			return Ok(true);
		}

		if from.type_id.entry == self.number_type_id.entry {
			// From untyped number

			let (value, span, from_value) = match &mut from.kind {
				ExpressionKind::NumberValue(value) => (value.value(), value.span(), value),
				kind => panic!("Collapsing from_number with a non-NumberValue expression: {kind:#?}"),
			};

			const MAX_F23_INTEGER: Decimal = dec!(16_777_215); // 2^24
			const MAX_F64_INTEGER: Decimal = dec!(9_007_199_254_740_991); // 2^53 - 1

			let (to_float, max_float_integer) = match to.entry {
				e if e == self.f32_type_id.entry => (true, MAX_F23_INTEGER),
				e if e == self.f64_type_id.entry => (true, MAX_F64_INTEGER),
				_ => (false, Decimal::ZERO),
			};

			if to_float {
				if value.is_integer() && value.abs() > max_float_integer {
					let name = self.type_name(function_store, &[], to);
					let err = error!("Constant number {value} is unable to be represented as {name}");
					messages.message(err.span(span));
					return Err(());
				}

				// constant number -> float of large enough
				from.type_id = to;
				return Ok(from_value.collapse(self, to));
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
				if !value.is_integer() {
					let name = self.type_name(function_store, &[], to);
					let error = error!("Constant number {value} is a decimal and so cannot be represented as {name}");
					messages.message(error.span(span));
					return Err(());
				} else if value.is_sign_negative() {
					let min_value = Decimal::from(-i128::pow(2, bit_count - 1));
					if value < min_value {
						let name = self.type_name(function_store, &[], to);
						let error = error!("Constant number {value} is too small to be represented as {name}");
						messages.message(error.span(span));
						return Err(());
					}
				} else {
					let max_value = Decimal::from(i128::pow(2, bit_count - 1) - 1);
					if value > max_value {
						let name = self.type_name(function_store, &[], to);
						let error = error!("Constant number {value} is too large to be represented as {name}");
						messages.message(error.span(span));
						return Err(());
					}
				}

				// constant number -> signed of large enough
				from.type_id = to;
				return Ok(from_value.collapse(self, to));
			}

			if to_unsigned {
				if !value.is_integer() {
					let name = self.type_name(function_store, &[], to);
					let error = error!("Constant number {value} is a decimal and so cannot be represented as {name}");
					messages.message(error.span(span));
					return Err(());
				} else if value.is_sign_negative() {
					let name = self.type_name(function_store, &[], to);
					let error = error!("Constant number {value} is negative and so cannot be represented as {name}",);
					messages.message(error.span(span));
					return Err(());
				}

				let max_value = Decimal::from(i128::pow(2, bit_count) - 1);
				if value > max_value {
					let name = self.type_name(function_store, &[], to);
					let error = error!("Constant number {value} is too large to be represented as {name}");
					messages.message(error.span(span));
					return Err(());
				}

				// constant number -> unsigned of large enough if not negative
				return Ok(from_value.collapse(self, to));
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
					let span = expression.span;
					let yields = expression.yields;
					let returns = expression.returns;
					let debug_location = expression.debug_location;
					let type_id = TypeId { entry: expression.type_id.entry - 1 };
					let conversion = Box::new(SliceMutableToImmutable { expression });
					let kind = ExpressionKind::SliceMutableToImmutable(conversion);
					*from = Expression {
						span,
						type_id,
						is_itself_mutable: false,
						is_pointer_access_mutable: false,
						yields,
						returns,
						kind,
						debug_location,
					};
					return Ok(true);
				}
			}
		}

		// str -> fstr
		if from.type_id.is_string(self) && to.is_format_string(self) {
			// TODO: This replace is a dumb solution
			let expression = std::mem::replace(from, Expression::any_collapse(self, from.span));
			let span = expression.span;
			let yields = expression.yields;
			let returns = expression.returns;
			let debug_location = expression.debug_location;
			let conversion = Box::new(StringToFormatString { expression });
			let kind = ExpressionKind::StringToFormatString(conversion);
			*from = Expression {
				span,
				type_id: to,
				is_itself_mutable: false,
				is_pointer_access_mutable: false,
				yields,
				returns,
				kind,
				debug_location,
			};
			return Ok(true);
		}

		if let TypeEntryKind::UserType { shape_index, specialization_index, .. } = from_entry.kind {
			let user_types = self.user_types.read();

			let user_type = user_types[shape_index].clone();
			let user_type = user_type.read();

			if let UserTypeKind::Struct { shape } = &user_type.kind {
				if shape.parent_kind != StructParentKind::None {
					// enum/union variant -> enum/union
					if let TypeEntryKind::UserType {
						shape_index: to_shape_index,
						specialization_index: to_specialization_index,
						..
					} = to_entry.kind
					{
						let variant_index = shape.variant_index.unwrap();
						if to_shape_index == shape.parent_shape_index {
							let parent_shape = user_types[to_shape_index].read();
							let expected_variant_type_id = match &parent_shape.kind {
								UserTypeKind::Enum { shape: parent_shape } => {
									let parent_specialization = &parent_shape.specializations[to_specialization_index];
									parent_specialization.variants[variant_index].type_id
								}

								UserTypeKind::Union { shape: parent_shape } => {
									let parent_specialization = &parent_shape.specializations[to_specialization_index];
									parent_specialization.variants[variant_index].type_id
								}

								_ => unreachable!(),
							};

							if expected_variant_type_id.entry == from.type_id.entry {
								// TODO: This replace is a dumb solution
								let expression = std::mem::replace(from, Expression::any_collapse(self, from.span));
								let yields = expression.yields;
								let returns = expression.returns;
								let is_pointer_access_mutable = expression.is_pointer_access_mutable;
								let conversion = Box::new(EnumVariantToEnum { type_id: to, expression });
								let kind = ExpressionKind::EnumVariantToEnum(conversion);
								*from = Expression {
									span: from.span,
									type_id: to,
									is_itself_mutable: false,
									is_pointer_access_mutable,
									yields,
									returns,
									kind,
									debug_location: from.debug_location,
								};
								return Ok(true);
							}
						}
					}

					// transparent variant -> its wrapped type
					if shape.is_transparent_variant {
						let specialization = &shape.specializations[specialization_index];
						let wrapped_type_id = specialization.fields.first().unwrap().type_id;
						if wrapped_type_id.entry == to.entry {
							return Ok(true);
						}
					}
				}
			}
		}

		// transparent variant -> its wrapped type

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
			TypeEntryKind::BuiltinType { kind: PrimativeKind::AnyCollapse, .. } => Some((self.any_collapse_type_id, false)),
			_ => None,
		}
	}

	// (TypeId, mutable)
	pub fn sliced_of(&mut self, type_id: TypeId) -> Option<(TypeId, bool)> {
		let entry = self.type_entries.get(type_id);
		match entry.kind {
			TypeEntryKind::Slice(Slice { type_id, mutable }) => Some((type_id, mutable)),
			TypeEntryKind::BuiltinType { kind: PrimativeKind::AnyCollapse, .. } => Some((self.any_collapse_type_id, true)),
			_ => None,
		}
	}

	pub fn register_type(
		user_types: &mut Vec<Ref<RwLock<UserType<'a>>>>,
		method_collections: &RwLock<Vec<Ref<RwLock<MethodCollection<'a>>>>>,
		implementations: &RwLock<Vec<RwLock<ImplementationInfo>>>,
		name: &'a str,
		generic_parameters: GenericParameters<'a>,
		kind: UserTypeKind<'a>,
		scope_id: ScopeId,
		span: Span,
	) -> RegisterTypeResult {
		// Type entry gets added during specialization

		let shape_index = user_types.len();
		let mut method_collections = method_collections.write();
		let methods_index = method_collections.len();
		let user_type = UserType {
			name,
			span,
			scope_id,
			generic_parameters,
			methods_index,
			kind,
		};

		user_types.push(Ref::new(RwLock::new(user_type)));
		method_collections.push(Ref::new(RwLock::new(MethodCollection::blank())));

		let mut implementations = implementations.write();
		assert_eq!(implementations.len(), methods_index);
		implementations.push(RwLock::new(ImplementationInfo::default()));
		drop(implementations);

		RegisterTypeResult { shape_index, methods_index }
	}

	pub fn format_trait_name(
		&mut self,
		function_store: &FunctionStore<'a>,
		module_path: &'a [String],
		trait_id: TraitId,
	) -> String {
		let traits = self.traits.read();
		let shape = traits[trait_id.shape_index as usize].as_ref().unwrap().read();
		let specialization = &shape.specializations[trait_id.specialization_index as usize];

		let mut name = String::from(shape.name);
		let type_arguments = specialization.type_arguments.clone();
		drop(shape);
		drop(traits);

		if !type_arguments.explicit_ids().is_empty() {
			name.push_str("<");
		}

		for (index, type_argument) in type_arguments.explicit_ids().iter().enumerate() {
			name.push_str(&self.internal_type_name(Some(function_store), module_path, type_argument.item, false, false));
			if index + 1 < type_arguments.explicit_ids().len() {
				name.push_str(", ");
			}
		}

		if !type_arguments.explicit_ids().is_empty() {
			name.push_str(">");
		}
		name
	}

	pub fn pre_create_empty_trait_shape(traits: &mut Vec<Option<Ref<RwLock<TraitShape<'a>>>>>) -> usize {
		let shape_index = traits.len();
		traits.push(None);
		shape_index
	}

	pub fn register_trait_shape(
		traits: &mut Vec<Option<Ref<RwLock<TraitShape<'a>>>>>,
		trait_shape: TraitShape<'a>,
		index: usize,
	) {
		let destination = &mut traits[index];
		assert!(destination.is_none());
		*destination = Some(Ref::new(RwLock::new(trait_shape)));
	}

	pub fn lookup_constraints(
		&mut self,
		messages: &mut Messages<'a>,
		root_layers: &RootLayers<'a>,
		function_store: &FunctionStore<'a>,
		module_path: &'a [String],
		symbols: &mut Symbols<'a>,
		generic_usages: &mut Vec<GenericUsage>,
		function_initial_symbols_length: usize,
		constraints: &[Node<tree::GenericConstraint<'a>>],
		enclosing_generic_parameters: &GenericParameters<'a>,
	) -> (SliceRef<TraitId>, SliceRef<GenericConstraint>) {
		let mut trait_ids = Vec::with_capacity(constraints.len());
		let mut generic_constraints = Vec::with_capacity(constraints.len());

		for constraint in constraints {
			let Some(symbol) =
				symbols.lookup_path_symbol(messages, root_layers, self, function_initial_symbols_length, &constraint.item.path)
			else {
				continue;
			};

			let SymbolKind::Trait { trait_shape_index } = symbol.kind else {
				let message = error!("Generic constraint must be a trait, found {}", symbol.kind);
				messages.message(message.span(constraint.span));
				continue;
			};

			let expected_len = constraint.item.type_arguments.len() + enclosing_generic_parameters.parameters().len();
			let mut explicit_arguments = Vec::with_capacity(expected_len);
			let mut lookup_failed = false;
			for argument in constraint.item.type_arguments {
				if let Some(type_id) = self.lookup_type(
					messages,
					function_store,
					module_path,
					generic_usages,
					root_layers,
					symbols,
					function_initial_symbols_length,
					enclosing_generic_parameters,
					argument,
				) {
					explicit_arguments.push(Node::new(type_id, argument.span));
				} else {
					lookup_failed = true;
				}
			}

			if lookup_failed {
				continue;
			}

			let mut type_arguments = TypeArguments::new_from_explicit(explicit_arguments);

			let traits = self.traits.read();
			let trait_shape = traits[trait_shape_index].as_ref().unwrap().read();
			assert_eq!(trait_shape.generic_parameters.method_base_len(), 0);
			if trait_shape.generic_parameters.implicit_len() > 0 {
				assert_eq!(
					trait_shape.generic_parameters.implicit_len(),
					enclosing_generic_parameters.parameters().len()
				);
				let enclosing_parameters = enclosing_generic_parameters.parameters().iter();
				for implicit in enclosing_parameters.map(|p| Node::new(p.generic_type_id, p.name.span)) {
					type_arguments.push_implicit(implicit);
				}
			}
			drop(trait_shape);
			drop(traits);

			let type_arguments = Ref::new(type_arguments);
			let Some(trait_id) = self.get_or_add_trait_shape_specialization(
				messages,
				function_store,
				module_path,
				generic_usages,
				enclosing_generic_parameters,
				trait_shape_index,
				Some(constraint.span),
				type_arguments.clone(),
			) else {
				continue;
			};

			trait_ids.push(trait_id);
			generic_constraints.push(GenericConstraint { trait_shape_index, type_arguments })
		}

		(SliceRef::from(trait_ids), SliceRef::from(generic_constraints))
	}

	fn set_up_generic_method_collection(&mut self, constraints: &[TraitId]) -> usize {
		let mut method_collections = self.method_collections.write();
		let methods_index = method_collections.len();

		let mut implementations = self.implementations.write();
		assert_eq!(implementations.len(), methods_index);
		let mut info = ImplementationInfo::default();
		for &constraint in constraints {
			info.statuses.insert(constraint, ImplementationStatus::Implemented);
		}
		implementations.push(RwLock::new(info));
		drop(implementations);

		let collection = MethodCollection::for_generic_satisfying_constraints(self, constraints);
		method_collections.push(Ref::new(RwLock::new(collection)));
		drop(method_collections);

		methods_index
	}

	pub fn register_user_type_generic(&mut self, shape_index: usize, generic_index: usize, constraints: &[TraitId]) -> TypeId {
		let methods_index = self.set_up_generic_method_collection(constraints);
		let kind = TypeEntryKind::UserTypeGeneric { shape_index, generic_index, methods_index };
		let type_entry = TypeEntry { kind, reference_entries: None, generic_poisoned: true };
		self.type_entries.push_entry(type_entry)
	}

	pub fn register_function_generic(
		&mut self,
		function_shape_index: usize,
		generic_index: usize,
		is_trait_self: bool,
		originator_trait_shape_index: usize,
		constraints: &[TraitId],
	) -> TypeId {
		let methods_index = self.set_up_generic_method_collection(constraints);
		let kind = TypeEntryKind::FunctionGeneric {
			function_shape_index,
			generic_index,
			methods_index,
			is_trait_self,
			originator_trait_shape_index,
		};
		let type_entry = TypeEntry { kind, reference_entries: None, generic_poisoned: true };
		let type_id = self.type_entries.push_entry(type_entry);
		type_id
	}

	pub fn register_trait_generic(
		&mut self,
		trait_shape_index: usize,
		generic_index: usize,
		is_self: bool,
		constraints: &[TraitId],
	) -> TypeId {
		let methods_index = self.set_up_generic_method_collection(constraints);
		let kind = TypeEntryKind::TraitGeneric { trait_shape_index, generic_index, methods_index, is_self };
		let type_entry = TypeEntry { kind, reference_entries: None, generic_poisoned: true };
		self.type_entries.push_entry(type_entry)
	}

	pub fn type_layout(&mut self, type_id: TypeId) -> Layout {
		let entry = self.type_entries.get(type_id);
		match entry.kind {
			TypeEntryKind::BuiltinType { kind, .. } => kind.layout(),

			TypeEntryKind::UserType { shape_index, specialization_index, .. } => {
				let lock = self.user_types.read()[shape_index].clone();
				let user_type = lock.read();
				match &user_type.kind {
					UserTypeKind::Struct { shape } => {
						let specialization = &shape.specializations[specialization_index];
						if let Some(layout) = specialization.layout {
							return layout;
						}
						assert!(specialization.been_filled);

						// Belch
						let field_types: Vec<_> = specialization.fields.iter().map(|f| f.type_id).collect();
						drop(user_type);

						let mut size = 0;
						let mut alignment = 1;

						for field_type_id in field_types {
							let field_layout = self.type_layout(field_type_id);

							if (size / field_layout.alignment) * field_layout.alignment < size {
								size = (size / field_layout.alignment) * field_layout.alignment + field_layout.alignment;
							}

							size += field_layout.size;
							alignment = alignment.max(field_layout.alignment);
						}

						if (size / alignment) * alignment < size {
							size = (size / alignment) * alignment + alignment;
						}

						let layout = Layout { size, alignment };
						let mut user_type = lock.write();
						match &mut user_type.kind {
							UserTypeKind::Struct { shape } => {
								shape.specializations[specialization_index].layout = Some(layout);
								layout
							}

							kind => unreachable!("{kind:?}"),
						}
					}

					UserTypeKind::Enum { shape } => {
						let specialization = &shape.specializations[specialization_index];
						if let Some(layout) = specialization.layout {
							return layout;
						}
						assert!(specialization.been_filled);

						let variants: Vec<_> = specialization.variants.iter().map(|v| v.type_id).collect();
						drop(user_type);

						let mut size = 0;
						let mut alignment = 1;

						for variant in variants {
							let variant_layout = self.type_layout(variant);

							size = size.max(variant_layout.size);
							alignment = alignment.max(variant_layout.alignment);
						}

						let mut layout = Layout { size, alignment };
						layout.size += layout.tag_memory_size();

						let mut user_type = lock.write();
						match &mut user_type.kind {
							UserTypeKind::Enum { shape } => {
								let specialization = &mut shape.specializations[specialization_index];
								specialization.layout = Some(layout);
								layout
							}

							kind => unreachable!("{kind:?}"),
						}
					}

					UserTypeKind::Union { shape } => {
						let specialization = &shape.specializations[specialization_index];
						if let Some(layout) = specialization.layout {
							return layout;
						}
						assert!(specialization.been_filled);

						let variants: Vec<_> = specialization.variants.iter().map(|v| v.type_id).collect();
						drop(user_type);

						let mut size = 0;
						let mut alignment = 1;

						for variant in variants {
							let variant_layout = self.type_layout(variant);

							size = size.max(variant_layout.size);
							alignment = alignment.max(variant_layout.alignment);
						}

						let layout = Layout { size, alignment };

						let mut user_type = lock.write();
						match &mut user_type.kind {
							UserTypeKind::Union { shape } => {
								let specialization = &mut shape.specializations[specialization_index];
								specialization.layout = Some(layout);
								layout
							}

							kind => unreachable!("{kind:?}"),
						}
					}
				}
			}

			TypeEntryKind::Pointer { .. } => Layout { size: 8, alignment: 8 },

			TypeEntryKind::Slice(_) => Layout { size: 16, alignment: 8 },

			// TODO: These are probably wrong, take care to make sure this doesn't break size_of in generic functions
			TypeEntryKind::UserTypeGeneric { .. } => Layout { size: 0, alignment: 1 },
			TypeEntryKind::FunctionGeneric { .. } => Layout { size: 0, alignment: 1 },
			TypeEntryKind::TraitGeneric { .. } => Layout { size: 0, alignment: 1 },

			TypeEntryKind::Module | TypeEntryKind::Type => panic!("Forbidden"),
		}
	}

	pub fn check_type_implements_trait(
		&mut self,
		messages: &mut Messages<'a>,
		function_store: &FunctionStore<'a>,
		module_path: &'a [String],
		generic_usages: &mut Vec<GenericUsage>,
		enclosing_generic_parameters: &GenericParameters<'a>,
		type_id: Node<TypeId>,
		generic_constraint: &GenericConstraint,
		trait_type_arguments: Ref<TypeArguments>,
		invoke_span: Option<Span>,
	) -> bool {
		let Some(trait_id) = self.get_or_add_trait_shape_specialization(
			messages,
			function_store,
			module_path,
			generic_usages, // TODO: Is this a bug waiting to happen?
			enclosing_generic_parameters,
			generic_constraint.trait_shape_index,
			invoke_span,
			trait_type_arguments,
		) else {
			return false;
		};

		let entry = self.type_entries.get(type_id.item);
		let (methods_index, method_base_type) = match entry.kind {
			TypeEntryKind::BuiltinType { methods_index, .. } => (methods_index, MethodBaseType::Other),

			TypeEntryKind::TraitGeneric { trait_shape_index, is_self: true, .. } => {
				return trait_shape_index == generic_constraint.trait_shape_index
			}

			TypeEntryKind::FunctionGeneric { originator_trait_shape_index, is_trait_self: true, .. } => {
				return originator_trait_shape_index == generic_constraint.trait_shape_index
			}

			TypeEntryKind::UserTypeGeneric { methods_index, .. }
			| TypeEntryKind::FunctionGeneric { methods_index, .. }
			| TypeEntryKind::TraitGeneric { methods_index, .. } => {
				let method_base_type = validator::get_method_base_type_for_generic(
					messages,
					self,
					function_store,
					module_path,
					generic_usages,
					enclosing_generic_parameters,
					&entry.kind,
					type_id.item,
				);
				(methods_index, method_base_type)
			}

			TypeEntryKind::UserType { shape_index, specialization_index, methods_index, .. } => {
				(methods_index, MethodBaseType::UserType { shape_index, specialization_index })
			}

			TypeEntryKind::Pointer { .. } | TypeEntryKind::Slice(_) => {
				let type_name = self.type_name(function_store, module_path, type_id.item);
				let trait_name = self.format_trait_name(function_store, module_path, trait_id);
				let message = error!("Type {type_name} is unable to conform to trait `{trait_name}` as it cannot have methods");
				messages.message(message.span(type_id.span));
				return false;
			}

			TypeEntryKind::Module | TypeEntryKind::Type => unreachable!(),
		};

		let implementations = self.implementations.clone();
		let implementations = implementations.read();
		let mut info = implementations[methods_index].write();

		let status_entry = info.statuses.entry(trait_id);
		let status_vacancy = match status_entry {
			hash_map::Entry::Occupied(status) => return matches!(status.get(), ImplementationStatus::Implemented),
			hash_map::Entry::Vacant(vacancy) => vacancy,
		};

		let methods_collections = self.method_collections.read();
		let methods_collection = methods_collections[methods_index].clone();
		let methods_collection = methods_collection.read();
		drop(methods_collections);

		let traits = self.traits.clone();
		let traits = traits.read();
		let trait_shape = traits[trait_id.shape_index as usize].as_ref().unwrap().read();
		assert!(trait_shape.been_filled);
		let trait_instance = &trait_shape.specializations[trait_id.specialization_index as usize];
		assert!(trait_instance.been_filled);
		let trait_methods = trait_instance.methods.clone();
		drop(trait_shape);
		drop(traits);

		let mut implemented = true;
		let mut actual_method_indices = Vec::new();
		let mut notes = Vec::new();
		for trait_method in trait_methods.iter() {
			let name = trait_method.name.item;
			let Some(&actual_method_index) = methods_collection.methods_by_name.get(name) else {
				notes.push(note!(trait_method.name.span, "Type does not implement method `{name}`"));
				implemented = false;
				continue;
			};
			let actual_method_info = methods_collection.methods[actual_method_index];

			let expected_mutable = trait_method.kind.item == MethodKind::MutableSelf;
			let actual_immutable = actual_method_info.kind.item == MethodKind::ImmutableSelf;
			let mutability_downcast = expected_mutable && actual_immutable;
			if actual_method_info.kind.item != trait_method.kind.item && !mutability_downcast {
				notes.push(note!(
					actual_method_info.kind.span,
					"Expected method `{name}` to be {} but found it to be {}",
					trait_method.kind.item.name(),
					actual_method_info.kind.item.name()
				));
				implemented = false;
			}

			// TODO: We're aquiring this twice in the same function, sad :(
			let function_shapes = function_store.shapes.read();
			let shape = function_shapes[actual_method_info.function_shape_index].as_ref().unwrap();
			let shape = shape.read();
			let name_span = shape.name.span;

			if let Some(varargs) = shape.c_varargs {
				notes.push(note!(varargs, "Trait methods may not have accept varargs"));
				implemented = false;
			}

			if shape.generic_parameters.explicit_len() > 0 {
				notes.push(note!(
					shape.generic_parameters.explicit_parameters().first().unwrap().name.span,
					"Trait method may not be generic",
				));
				implemented = false;
			}

			if shape.parameters.item.len() - 1 != trait_method.parameters.len() {
				notes.push(note!(
					shape.parameters.span,
					"Expected method `{name}` to have {} parameters, found {}",
					trait_method.parameters.len(),
					shape.parameters.item.len() - 1
				));
				implemented = false;
			}

			drop(shape);
			drop(function_shapes);

			if !implemented {
				break;
			}

			// TODO: This may be obscuring a bug
			let mut generic_usages = Vec::new();
			let Some(actual_specialization_result) = function_store.get_method_function_specialization(
				messages,
				self,
				module_path,
				&mut generic_usages,
				enclosing_generic_parameters,
				Vec::new(), // TODO: This will not work once trait methods can be generic
				actual_method_info.function_shape_index,
				method_base_type.clone(),
				None,
			) else {
				notes.push(note!(
					name_span,
					"TODO: Specializing a method while checking for type trait conformance failed",
				));
				implemented = false;
				break;
			};
			generic_usages.clear();

			let Some(expected_specialization_result) = function_store.get_method_function_specialization(
				messages,
				self,
				module_path,
				&mut generic_usages,
				enclosing_generic_parameters,
				Vec::new(), // TODO: This will not work once trait methods can be generic
				trait_method.fake_function_shape_index,
				MethodBaseType::Trait {
					trait_ids: SliceRef::from(vec![trait_id]), // This is an unfortunate allocation
					self_type_id: type_id.item,
				},
				None,
			) else {
				notes.push(note!(
					name_span,
					"TODO: Specializing a trait method while checking for type trait conformance failed",
				));
				implemented = false;
				break;
			};

			let function_shapes = function_store.shapes.read();
			let actual_shape_lock = function_shapes[actual_method_info.function_shape_index].clone().unwrap();
			let expected_shape_lock = function_shapes[trait_method.fake_function_shape_index].clone().unwrap();
			drop(function_shapes);

			let mut actual_shape = actual_shape_lock.read();
			let actual_specialization = &actual_shape.specializations[actual_specialization_result.specialization_index];
			let mut expected_shape = expected_shape_lock.read();
			let expected_specialization = &expected_shape.specializations[expected_specialization_result.specialization_index];

			if !self.direct_match(actual_specialization.return_type, expected_specialization.return_type) {
				let span = actual_shape.return_type.span;
				let actual_return_type = actual_specialization.return_type;
				let expected_return_type = expected_specialization.return_type;
				drop(actual_shape);
				drop(expected_shape);
				let expected = self.type_name(function_store, module_path, expected_return_type);
				let actual = self.type_name(function_store, module_path, actual_return_type);
				notes.push(note!(
					span,
					"Expected method `{name}` to have the return type {expected}, found return type {actual}"
				));

				actual_shape = actual_shape_lock.read();
				expected_shape = expected_shape_lock.read();
				implemented = false;
			}

			let actual_specialization = &actual_shape.specializations[actual_specialization_result.specialization_index];
			let expected_specialization = &expected_shape.specializations[expected_specialization_result.specialization_index];
			for ((actual_index, actual), expected) in actual_specialization
				.parameters
				.iter()
				.enumerate()
				.skip(1)
				.zip(expected_specialization.parameters.iter().skip(1))
			{
				if expected.type_id.is_any_collapse(self) {
					continue;
				}

				// TODO: Release function store locks when calling `type_name`
				if !self.direct_match(actual.type_id, expected.type_id) {
					let span = actual_shape.parameters.item[actual_index].span;
					let expected = self.type_name(function_store, module_path, expected.type_id);
					let actual = self.type_name(function_store, module_path, actual.type_id);
					notes.push(note!(span, "Expected parameter of type {expected}, found type {actual}"));
					implemented = false;
				}
			}

			if implemented {
				actual_method_indices.push(ActualMethodInfo {
					method_index: actual_method_index,
					specialization_index: actual_specialization_result.specialization_index,
				});
			}
		}

		let status = match implemented {
			true => ImplementationStatus::Implemented,
			false => ImplementationStatus::NotImplemented,
		};
		status_vacancy.insert(status);
		let trait_index = trait_id.shape_index as usize;
		let actual_method_indices = ActualMethodIndices { actual_method_indices };
		let replaced_indicies = info.actual_indices.insert(trait_index, actual_method_indices);
		assert!(replaced_indicies.is_none()); // TODO: This is *going* to be hit, be smarter about initializing this

		if implemented {
			return true;
		}

		let trait_name = self.format_trait_name(function_store, module_path, trait_id);
		let mut message = error!("Type is expected to conform to trait `{trait_name}` but does not");

		for note in notes {
			message = message.note(note);
		}

		messages.message(message.span(type_id.span));
		false
	}

	pub fn find_user_type_dependency_chain(&mut self, from: TypeId, to: TypeId) -> Option<Vec<UserTypeChainLink<'a>>> {
		let entry = self.type_entries.get(from);
		let (shape_index, specialization_index) = match entry.kind {
			TypeEntryKind::UserType { shape_index, specialization_index, .. } => (shape_index, specialization_index),
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

			UserTypeKind::Union { shape } => {
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
		module_path: &'a [String],
		generic_usages: &mut Vec<GenericUsage>,
		root_layers: &RootLayers<'a>,
		symbols: &mut Symbols<'a>,
		function_initial_symbols_len: usize,
		enclosing_generic_parameters: &GenericParameters<'a>,
		parsed_type: &Node<tree::Type<'a>>,
	) -> Option<TypeId> {
		let (path_segments, type_arguments, dot_access_chain) = match &parsed_type.item {
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

			tree::Type::Path { path_segments, type_arguments, dot_access_chain } => {
				(path_segments, *type_arguments, *dot_access_chain)
			}
		};

		let last_path_span = path_segments.item.segments.last().unwrap().span;
		let symbol =
			symbols.lookup_path_symbol(messages, root_layers, self, function_initial_symbols_len, &path_segments.item)?;
		let shape_index = match symbol.kind {
			SymbolKind::BuiltinType { type_id, .. } => {
				if !type_arguments.is_empty() {
					messages.message(error!("Builtin types do not accept type arguments").span(parsed_type.span));
					return None;
				}

				if !dot_access_chain.is_empty() {
					messages.message(error!("Builtin types may not be dot-accessed").span(parsed_type.span));
					return None;
				}

				return Some(type_id);
			}

			SymbolKind::UserType { shape_index, .. } => shape_index,

			SymbolKind::UserTypeGeneric { shape_index, generic_index } => {
				if !type_arguments.is_empty() {
					messages.message(error!("Type generic parameters do not accept type arguments").span(parsed_type.span));
					return None;
				}

				if let [first_access, ..] = dot_access_chain {
					let message = error!("Type generic parameters may not be dot-accessed");
					messages.message(message.span(last_path_span + first_access.span));
					return None;
				}

				let user_type = &self.user_types.read()[shape_index];
				let user_type = user_type.read();
				let generic = &user_type.generic_parameters.parameters()[generic_index];
				return Some(generic.generic_type_id);
			}

			SymbolKind::FunctionGeneric { function_shape_index, generic_index } => {
				if !type_arguments.is_empty() {
					messages.message(error!("Function generic parameters do not accept type arguments").span(parsed_type.span));
					return None;
				}

				if let [first_access, ..] = dot_access_chain {
					let message = error!("Function generic parameters may not be dot-accessed");
					messages.message(message.span(last_path_span + first_access.span));
					return None;
				}

				let generics = &function_store.generics.read()[function_shape_index];
				let generic = &generics.parameters()[generic_index];
				return Some(generic.generic_type_id);
			}

			SymbolKind::TraitGeneric { trait_shape_index, generic_index } => {
				if !type_arguments.is_empty() {
					messages.message(error!("Trait body generic parameters do not accept type arguments").span(parsed_type.span));
					return None;
				}

				if let [first_access, ..] = dot_access_chain {
					let message = error!("Trait body generic parameters may not be dot-accessed");
					messages.message(message.span(last_path_span + first_access.span));
					return None;
				}

				let traits = self.traits.read();
				let trait_shape = traits[trait_shape_index].as_ref().unwrap().read();
				let generic = &trait_shape.generic_parameters.parameters()[generic_index];
				return Some(generic.generic_type_id);
			}

			_ => {
				messages.message(error!("Symbol {:?} is not a type", symbol.name).span(path_segments.span));
				return None;
			}
		};

		if !type_arguments.is_empty() {
			let user_types = self.user_types.read();
			let shape = user_types[shape_index].as_ref().read();
			if let UserTypeKind::Struct { shape } = &shape.kind {
				if shape.parent_kind != StructParentKind::None {
					// Note: This error can also be emitted in `validate_dot_access` during malformed variant construction
					let error = error!("Type arguments not permitted on a variant");
					messages.message(error.span(parsed_type.span));
					return None;
				}
			}
		}

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

		let [first_access, ..] = dot_access_chain else {
			return Some(type_id);
		};

		// TODO: Weird error, should go away once enums can be arbitrarily nested
		if dot_access_chain.len() > 1 {
			let depth = dot_access_chain.len();
			let message = error!("Types may not have more than one level of dot access, found {depth} levels");
			let last_access_span = dot_access_chain.last().unwrap().span;
			messages.message(message.span(last_path_span + last_access_span));
			return None;
		}

		let entry = self.type_entries.get(type_id);
		if let TypeEntryKind::UserType { shape_index, specialization_index, .. } = entry.kind {
			let user_type = self.user_types.read()[shape_index].clone();
			let user_type = user_type.read();
			if let UserTypeKind::Enum { shape } = &user_type.kind {
				let specialization = &shape.specializations[specialization_index];
				if let Some(&variant_index) = specialization.variants_by_name.get(first_access.item) {
					let variant = specialization.variants[variant_index];
					return Some(variant.type_id);
				} else {
					drop(user_type);
					let name = self.type_name(function_store, module_path, type_id);
					let message = error!("Enum {name} has no variant named `{}`", first_access.item);
					messages.message(message.span(first_access.span));
					return None;
				}
			}
		}

		let name = self.type_name(function_store, module_path, type_id);
		let message = error!("Type {name} may not be dot-accessed");
		messages.message(message.span(last_path_span + first_access.span));
		None
	}

	pub fn get_or_add_shape_specialization_in_scope(
		&mut self,
		messages: &mut Messages<'a>,
		function_store: &FunctionStore<'a>,
		module_path: &'a [String],
		generic_usages: &mut Vec<GenericUsage>,
		root_layers: &RootLayers<'a>,
		symbols: &mut Symbols<'a>,
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
			explicit_arguments.push(Node::new(type_id, argument.span));
		}

		let mut type_arguments = TypeArguments::new_from_explicit(explicit_arguments);

		let user_type = self.user_types.read()[shape_index].clone();
		let shape = user_type.read();
		assert_eq!(shape.generic_parameters.method_base_len(), 0);
		if shape.generic_parameters.implicit_len() > 0 {
			assert_eq!(shape.generic_parameters.implicit_len(), enclosing_generic_parameters.parameters().len());
			let enclosing_parameters = enclosing_generic_parameters.parameters().iter();
			for implicit in enclosing_parameters.map(|p| Node::new(p.generic_type_id, p.name.span)) {
				type_arguments.push_implicit(implicit);
			}
		}
		drop(shape);

		self.get_or_add_shape_specialization(
			messages,
			function_store,
			module_path,
			generic_usages,
			enclosing_generic_parameters,
			shape_index,
			invoke_span,
			Ref::new(type_arguments),
		)
	}

	pub fn get_or_add_shape_specialization(
		&mut self,
		messages: &mut Messages<'a>,
		function_store: &FunctionStore<'a>,
		module_path: &'a [String],
		generic_usages: &mut Vec<GenericUsage>,
		enclosing_generic_parameters: &GenericParameters<'a>,
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
					enclosing_generic_parameters,
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
					enclosing_generic_parameters,
					shape_index,
					invoke_span,
					type_arguments,
				)
			}

			UserTypeKind::Union { .. } => {
				todo!();
			}
		}
	}

	fn get_or_add_struct_shape_specialization(
		&mut self,
		messages: &mut Messages<'a>,
		function_store: &FunctionStore<'a>,
		module_path: &'a [String],
		generic_usages: &mut Vec<GenericUsage>,
		enclosing_generic_parameters: &GenericParameters<'a>,
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

		let mut constraint_failure = false;
		let generic_parameters_iter = user_type.generic_parameters.explicit_parameters().iter();
		let generic_arguments_iter = type_arguments.explicit_ids().iter();
		for (type_parameter, &type_argument) in generic_parameters_iter.zip(generic_arguments_iter) {
			for constraint in type_parameter.generic_constraints.iter() {
				let mut trait_type_arguments = TypeArguments::clone(&constraint.type_arguments);
				trait_type_arguments.specialize_with_generics(
					messages,
					self,
					function_store,
					module_path,
					generic_usages,
					enclosing_generic_parameters,
					TypeIdSpecializationSituation::UserType { user_type_shape_index: shape_index },
					&type_arguments,
				);

				if !self.check_type_implements_trait(
					messages,
					function_store,
					module_path,
					generic_usages,
					enclosing_generic_parameters,
					type_argument,
					constraint,
					Ref::new(trait_type_arguments),
					invoke_span,
				) {
					constraint_failure = true;
				}
			}
		}

		if constraint_failure {
			return None;
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
			.any(|id| self.type_entries.get(id.item).generic_poisoned);

		for field in &mut fields {
			field.type_id = self.specialize_type_id_with_generics(
				messages,
				function_store,
				module_path,
				generic_usages,
				enclosing_generic_parameters,
				field.type_id,
				&type_arguments,
				TypeIdSpecializationSituation::UserType { user_type_shape_index: shape_index },
			);
		}

		let mut user_type = lock.write();
		let methods_index = user_type.methods_index;
		let shape = match &mut user_type.kind {
			UserTypeKind::Struct { shape } => shape,
			kind => unreachable!("{kind:?}"),
		};

		let been_filled = shape.been_filled;
		let specialization = Struct {
			type_id: TypeId::unusable(),
			generic_poisoned: type_arguments_generic_poisoned,
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
			kind: TypeEntryKind::UserType { shape_index, specialization_index, methods_index },
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

		Some(type_id)
	}

	fn get_or_add_enum_shape_specialization(
		&mut self,
		messages: &mut Messages<'a>,
		function_store: &FunctionStore<'a>,
		module_path: &'a [String],
		generic_usages: &mut Vec<GenericUsage>,
		enclosing_generic_parameters: &GenericParameters<'a>,
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

		let mut constraint_failure = false;
		let generic_parameters_iter = user_type.generic_parameters.explicit_parameters().iter();
		let generic_arguments_iter = type_arguments.explicit_ids().iter();
		for (type_parameter, &type_argument) in generic_parameters_iter.zip(generic_arguments_iter) {
			for constraint in type_parameter.generic_constraints.iter() {
				let mut trait_type_arguments = TypeArguments::clone(&constraint.type_arguments);
				trait_type_arguments.specialize_with_generics(
					messages,
					self,
					function_store,
					module_path,
					generic_usages,
					enclosing_generic_parameters,
					TypeIdSpecializationSituation::UserType { user_type_shape_index: enum_shape_index },
					&type_arguments,
				);

				if !self.check_type_implements_trait(
					messages,
					function_store,
					module_path,
					generic_usages,
					enclosing_generic_parameters,
					type_argument,
					constraint,
					Ref::new(trait_type_arguments),
					invoke_span,
				) {
					constraint_failure = true;
				}
			}
		}

		if constraint_failure {
			return None;
		}

		let mut shared_fields = Vec::with_capacity(shape.shared_fields.len() + 1);
		let unspecialized_shared_fields = shape.shared_fields.clone();
		drop(user_type);

		let type_arguments_generic_poisoned = type_arguments
			.ids
			.iter()
			.any(|id| self.type_entries.get(id.item).generic_poisoned);

		for field in unspecialized_shared_fields.iter() {
			let type_id = self.specialize_type_id_with_generics(
				messages,
				function_store,
				module_path,
				generic_usages,
				enclosing_generic_parameters,
				field.item.field_type,
				&type_arguments,
				TypeIdSpecializationSituation::UserType { user_type_shape_index: enum_shape_index },
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
				let entry = self.type_entries.get(struct_type_argument.item);
				match entry.kind {
					TypeEntryKind::UserType { .. } => {
						struct_type_argument.item = self.specialize_type_id_with_generics(
							messages,
							function_store,
							module_path,
							generic_usages,
							enclosing_generic_parameters,
							struct_type_argument.item,
							&type_arguments,
							TypeIdSpecializationSituation::UserType { user_type_shape_index: enum_shape_index },
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
					enclosing_generic_parameters,
					variant_shape.struct_shape_index,
					None,
					Ref::new(new_struct_type_arguments),
				)
				.unwrap();

			let span = variant_shape.span;
			let variant_index = variant_shape.variant_index;
			assert_eq!(variant_index, variants.len());
			let is_transparent = variant_shape.is_transparent;
			variants.push(Variant { span, type_id, is_transparent });
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
			generic_poisoned: type_arguments_generic_poisoned,
			type_arguments: type_arguments.clone(),
			been_filled,
			shared_fields: SliceRef::from(shared_fields),
			variants: SliceRef::from(variants),
			variants_by_name: Ref::new(variants_by_name),
			layout: None,
		};
		let specialization_index = shape.specializations.len();
		shape.specializations.push(specialization);
		shape
			.specializations_by_type_arguments
			.insert(type_arguments.clone(), specialization_index);

		let methods_index = user_type.methods_index;
		let kind = TypeEntryKind::UserType {
			shape_index: enum_shape_index,
			specialization_index,
			methods_index,
		};
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

		Some(type_id)
	}

	pub fn get_or_add_trait_shape_specialization(
		&mut self,
		messages: &mut Messages<'a>,
		function_store: &FunctionStore<'a>,
		module_path: &'a [String],
		generic_usages: &mut Vec<GenericUsage>,
		enclosing_generic_parameters: &GenericParameters<'a>,
		trait_shape_index: usize,
		invoke_span: Option<Span>,
		trait_type_arguments: Ref<TypeArguments>,
	) -> Option<TraitId> {
		let _zone = zone!("trait specialization");

		let lock = self.traits.read()[trait_shape_index].clone();
		let mut shape = lock.as_ref().unwrap().read();

		if !shape.been_filled {
			let filling_lock = shape.filling_lock.clone();

			drop(shape);
			filling_lock.lock();

			shape = lock.as_ref().unwrap().read();
		}

		if trait_type_arguments.explicit_len != shape.generic_parameters.explicit_len() {
			let error = error!(
				"Expected {} type arguments for trait `{}`, got {}",
				shape.generic_parameters.explicit_len(),
				shape.name,
				trait_type_arguments.explicit_len,
			);
			messages.message(error.span_if_some(invoke_span));
			return None;
		}

		if let Some(&specialization_index) = shape.specializations_by_type_arguments.get(&trait_type_arguments) {
			let existing = &shape.specializations[specialization_index];
			return Some(existing.trait_id);
		}

		let mut constraint_failure = false;
		let generic_parameters_iter = shape.generic_parameters.explicit_parameters().iter();
		let generic_arguments_iter = trait_type_arguments.explicit_ids().iter();
		for (type_parameter, &type_argument) in generic_parameters_iter.zip(generic_arguments_iter) {
			for constraint in type_parameter.generic_constraints.iter() {
				let mut constraint_trait_type_arguments = TypeArguments::clone(&constraint.type_arguments);
				constraint_trait_type_arguments.specialize_with_generics(
					messages,
					self,
					function_store,
					module_path,
					generic_usages,
					enclosing_generic_parameters,
					TypeIdSpecializationSituation::Trait { trait_shape_index },
					&trait_type_arguments,
				);

				if !self.check_type_implements_trait(
					messages,
					function_store,
					module_path,
					generic_usages,
					enclosing_generic_parameters,
					type_argument,
					constraint,
					Ref::new(constraint_trait_type_arguments),
					invoke_span,
				) {
					constraint_failure = true;
				}
			}
		}

		if constraint_failure {
			return None;
		}

		let mut methods = shape.methods.clone();
		drop(shape);

		let type_arguments_generic_poisoned = trait_type_arguments
			.ids
			.iter()
			.any(|id| self.type_entries.get(id.item).generic_poisoned);

		for method in &mut methods {
			method.specialize_with_trait_generics(
				messages,
				self,
				function_store,
				module_path,
				generic_usages,
				enclosing_generic_parameters,
				trait_shape_index,
				&trait_type_arguments,
			);
		}

		let mut shape = lock.as_ref().unwrap().write();

		let been_filled = shape.been_filled;
		let specialization_index = shape.specializations.len();
		let trait_id = TraitId {
			shape_index: trait_shape_index as u32,
			specialization_index: specialization_index as u32,
		};
		let type_arguments = trait_type_arguments.clone();
		let specialization = Trait {
			trait_id,
			type_arguments,
			been_filled,
			methods: SliceRef::from(methods),
		};

		shape.specializations.push(specialization);
		shape
			.specializations_by_type_arguments
			.insert(trait_type_arguments.clone(), specialization_index);

		drop(shape);

		if type_arguments_generic_poisoned {
			let usage = GenericUsage::Trait { type_arguments: trait_type_arguments, trait_shape_index };
			generic_usages.push(usage)
		}

		Some(trait_id)
	}

	pub fn specialize_type_id_with_generics(
		&mut self,
		messages: &mut Messages<'a>,
		function_store: &FunctionStore<'a>,
		module_path: &'a [String],
		generic_usages: &mut Vec<GenericUsage>,
		enclosing_generic_parameters: &GenericParameters<'a>,
		type_id: TypeId,
		type_arguments: &TypeArguments,
		situation: TypeIdSpecializationSituation,
	) -> TypeId {
		let entry = self.type_entries.get(type_id);
		match &entry.kind {
			TypeEntryKind::BuiltinType { .. } => type_id,

			TypeEntryKind::UserType { shape_index, specialization_index, .. } => {
				let user_type = self.user_types.read()[*shape_index].clone();
				let user_type = user_type.read();
				match &user_type.kind {
					UserTypeKind::Struct { shape } => {
						let specialization = &shape.specializations[*specialization_index];
						let mut new_struct_type_arguments = TypeArguments::clone(&specialization.type_arguments);
						drop(user_type);

						for struct_type_argument in &mut new_struct_type_arguments.ids {
							struct_type_argument.item = self.specialize_type_id_with_generics(
								messages,
								function_store,
								module_path,
								generic_usages,
								enclosing_generic_parameters,
								struct_type_argument.item,
								type_arguments,
								situation,
							);
						}

						self.get_or_add_struct_shape_specialization(
							messages,
							function_store,
							module_path,
							generic_usages,
							enclosing_generic_parameters,
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
							enum_type_argument.item = self.specialize_type_id_with_generics(
								messages,
								function_store,
								module_path,
								generic_usages,
								enclosing_generic_parameters,
								enum_type_argument.item,
								type_arguments,
								situation,
							);
						}

						self.get_or_add_enum_shape_specialization(
							messages,
							function_store,
							module_path,
							generic_usages,
							enclosing_generic_parameters,
							*shape_index,
							None,
							Ref::new(new_enum_type_arguments),
						)
						.unwrap()
					}

					UserTypeKind::Union { shape: _ } => {
						todo!();
					}
				}
			}

			TypeEntryKind::Pointer { type_id, mutable } => {
				let type_id = self.specialize_type_id_with_generics(
					messages,
					function_store,
					module_path,
					generic_usages,
					enclosing_generic_parameters,
					*type_id,
					type_arguments,
					situation,
				);
				self.pointer_to(type_id, *mutable)
			}

			TypeEntryKind::Slice(Slice { type_id, mutable }) => {
				let type_id = self.specialize_type_id_with_generics(
					messages,
					function_store,
					module_path,
					generic_usages,
					enclosing_generic_parameters,
					*type_id,
					type_arguments,
					situation,
				);
				self.slice_of(type_id, *mutable)
			}

			&TypeEntryKind::UserTypeGeneric { shape_index, generic_index, .. } => {
				if let TypeIdSpecializationSituation::UserType { user_type_shape_index } = situation {
					assert_eq!(user_type_shape_index, shape_index);
					type_arguments.ids[generic_index].item
				} else {
					// TODO: This could have unintended consequences
					self.user_types.read()[shape_index].read().generic_parameters.parameters()[generic_index].generic_type_id
				}
			}

			&TypeEntryKind::FunctionGeneric { function_shape_index: shape_index, generic_index, .. } => {
				if let TypeIdSpecializationSituation::Function { function_shape_index } = situation {
					if function_shape_index != shape_index {
						let shapes = function_store.shapes.read();
						dbg!(
							shapes[function_shape_index].as_ref().map(|s| s.read().name.item),
							shapes[shape_index].as_ref().map(|s| s.read().name.item)
						);
						dbg!(self.debugging_type_name(type_id));
					}
					assert_eq!(function_shape_index, shape_index);
					type_arguments.ids[generic_index].item
				} else {
					// TODO: This could have unintended consequences
					function_store.generics.read()[shape_index].parameters()[generic_index].generic_type_id
				}
			}

			&TypeEntryKind::TraitGeneric { trait_shape_index: shape_index, generic_index, .. } => {
				if let TypeIdSpecializationSituation::Trait { trait_shape_index } = situation {
					assert_eq!(trait_shape_index, shape_index);
					type_arguments.ids[generic_index].item
				} else {
					// TODO: This could have unintended consequences
					self.traits.read()[shape_index]
						.as_ref()
						.unwrap()
						.read()
						.generic_parameters
						.parameters()[generic_index]
						.generic_type_id
				}
			}

			TypeEntryKind::Module | TypeEntryKind::Type => panic!("{:?}", &entry.kind),
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
			TypeEntryKind::Module => "a module".to_owned(),

			TypeEntryKind::Type => "a type".to_owned(),

			TypeEntryKind::BuiltinType { kind, .. } => kind.name().to_owned(),

			TypeEntryKind::UserType { shape_index, specialization_index, .. } => {
				let user_type = self.user_types.read()[shape_index].clone();
				let user_type = user_type.read();
				let user_type_name = user_type.name;
				let explicit_generic_parameters_len = user_type.generic_parameters.explicit_len();

				match &user_type.kind {
					UserTypeKind::Struct { shape } => {
						let parent_kind = shape.parent_kind;
						let parent_shape_index = shape.parent_shape_index;
						let specialization = &shape.specializations[specialization_index];
						let type_arguments = specialization.type_arguments.ids[0..explicit_generic_parameters_len].to_vec();
						drop(user_type);

						let type_arguments = type_arguments
							.iter()
							.map(|argument| {
								self.internal_type_name(
									function_store,
									_module_path,
									argument.item,
									debug_generics,
									debug_type_ids,
								)
							})
							.collect::<Vec<_>>()
							.join(", ");

						if parent_kind != StructParentKind::None {
							let parent_name = self.user_types.read()[parent_shape_index].read().name;
							if explicit_generic_parameters_len == 0 {
								format!("{parent_name}.{}", user_type_name)
							} else {
								format!("{parent_name}::{}<{}>", user_type_name, type_arguments)
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
							.map(|argument| {
								self.internal_type_name(
									function_store,
									_module_path,
									argument.item,
									debug_generics,
									debug_type_ids,
								)
							})
							.collect::<Vec<_>>()
							.join(", ");

						format!("{}<{}>", user_type_name, type_arguments)
					}

					UserTypeKind::Union { shape } => {
						if explicit_generic_parameters_len == 0 {
							return user_type_name.to_owned();
						}

						let specialization = &shape.specializations[specialization_index];
						let type_arguments = specialization.type_arguments.ids[0..explicit_generic_parameters_len].to_vec();
						drop(user_type);
						let type_arguments = type_arguments
							.iter()
							.map(|argument| {
								self.internal_type_name(
									function_store,
									_module_path,
									argument.item,
									debug_generics,
									debug_type_ids,
								)
							})
							.collect::<Vec<_>>()
							.join(", ");

						format!("{}<{}>", user_type_name, type_arguments)
					}
				}
			}

			TypeEntryKind::Pointer { type_id, mutable } => {
				let inner = self.internal_type_name(function_store, _module_path, type_id, debug_generics, debug_type_ids);
				match mutable {
					true => format!("*mut {}", inner),
					false => format!("*{}", inner),
				}
			}

			TypeEntryKind::Slice(Slice { type_id, mutable }) => {
				let inner = self.internal_type_name(function_store, _module_path, type_id, debug_generics, debug_type_ids);
				match mutable {
					true => format!("[]mut {}", inner),
					false => format!("[]{}", inner),
				}
			}

			TypeEntryKind::UserTypeGeneric { shape_index, generic_index, .. } => {
				let user_type = &self.user_types.read()[shape_index];
				let user_type = user_type.read();
				let generic = &user_type.generic_parameters.parameters()[generic_index];

				if debug_generics {
					format!("UserTypeGeneric {shape_index} {generic_index} {}", generic.name.item)
				} else {
					generic.name.item.to_owned()
				}
			}

			TypeEntryKind::FunctionGeneric { function_shape_index, generic_index, .. } => {
				if let Some(function_store) = function_store {
					let shapes = function_store.shapes.read();
					let shape = shapes[function_shape_index].as_ref().unwrap();
					let shape = shape.read();
					let generic = &shape.generic_parameters.parameters()[generic_index];

					if debug_generics {
						format!("FunctionGeneric {function_shape_index} {generic_index} {}", generic.name.item)
					} else {
						generic.name.item.to_owned()
					}
				} else {
					if debug_generics {
						format!("FunctionGeneric {function_shape_index} {generic_index}")
					} else {
						"FunctionGeneric".to_owned()
					}
				}
			}

			TypeEntryKind::TraitGeneric { trait_shape_index, generic_index, .. } => {
				let traits = self.traits.read();
				let shape = traits[trait_shape_index].as_ref().unwrap().read();
				let generic = &shape.generic_parameters.parameters()[generic_index];

				if debug_generics {
					format!("TraitGeneric {trait_shape_index} {generic_index} {}", generic.name.item)
				} else {
					generic.name.item.to_owned()
				}
			}
		}
	}
}
