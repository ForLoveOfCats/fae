use crate::frontend::error::Messages;
use crate::frontend::function_store::FunctionStore;
use crate::frontend::ir::{
	DecimalValue, Expression, ExpressionKind, GenericParameter, GenericUsage, SliceMutableToImmutable, TypeArguments,
};
use crate::frontend::root_layers::RootLayers;
use crate::frontend::span::Span;
use crate::frontend::symbols::{Symbol, SymbolKind, Symbols};
use crate::frontend::tree::{self, Node};

#[derive(Debug, Clone, Copy)]
pub struct TypeId {
	entry: u32,
}

impl TypeId {
	pub fn new(entry: u32) -> TypeId {
		TypeId { entry }
	}

	pub fn is_any_collapse(self, type_store: &TypeStore) -> bool {
		type_store.direct_match(self, type_store.any_collapse_type_id)
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
			NumericKind::USize,
			NumericKind::F32,
			NumericKind::F64,
		];

		Some(BUFFER[self.index() - type_store.i8_type_id.index()])
	}

	pub fn is_pointer(self, type_store: &TypeStore) -> bool {
		let entry = type_store.type_entries[self.index()];
		matches!(entry.kind, TypeEntryKind::Pointer { .. })
	}

	pub fn is_primative(self, type_store: &TypeStore) -> bool {
		let entry = type_store.type_entries[self.entry as usize];
		match entry.kind {
			TypeEntryKind::BuiltinType { .. } | TypeEntryKind::Pointer { .. } => true,
			TypeEntryKind::UserType { .. } | TypeEntryKind::Slice(_) => false,
			TypeEntryKind::UserTypeGeneric { .. } => unreachable!(),
			TypeEntryKind::FunctionGeneric { .. } => unreachable!(),
		}
	}

	pub fn as_struct<'a, 's>(self, type_store: &'s TypeStore<'a>) -> Option<&'s Struct<'a>> {
		let entry = type_store.type_entries[self.entry as usize];
		if let TypeEntryKind::UserType { shape_index, specialization_index } = entry.kind {
			let shape = &type_store.user_types[shape_index];
			#[allow(irrefutable_let_patterns)] // TODO: Remove once enums are added
			if let UserTypeKind::Struct { shape } = &shape.kind {
				return Some(&shape.specializations[specialization_index]);
			}
		}

		None
	}

	pub fn as_slice(self, type_store: &TypeStore) -> Option<Slice> {
		let entry = type_store.type_entries[self.entry as usize];
		if let TypeEntryKind::Slice(slice) = entry.kind {
			return Some(slice);
		}

		None
	}

	pub fn index(self) -> usize {
		self.entry as usize
	}
}

#[derive(Debug, Clone, Copy)]
pub struct Layout {
	pub size: i64,
	pub alignment: i64,
}

#[derive(Debug)]
pub struct StructShape<'a> {
	pub name: &'a str,
	pub generics: Vec<GenericParameter<'a>>,

	pub been_filled: bool,
	pub fields: Vec<Node<FieldShape<'a>>>,

	pub specializations: Vec<Struct<'a>>,
}

impl<'a> StructShape<'a> {
	pub fn new(name: &'a str, generics: Vec<GenericParameter<'a>>) -> Self {
		StructShape {
			name,
			generics,
			been_filled: false,
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

#[derive(Debug, Clone)]
pub struct Struct<'a> {
	pub type_id: TypeId,
	pub type_arguments: Vec<TypeId>,
	pub been_filled: bool,
	pub fields: Vec<Field<'a>>,
	pub layout: Option<Layout>,
}

#[derive(Debug, Clone, Copy)]
pub struct Field<'a> {
	pub span: Option<Span>,
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
			NumericKind::USize => Layout { size: 8, alignment: 8 },
			NumericKind::F32 => Layout { size: 4, alignment: 4 },
			NumericKind::F64 => Layout { size: 8, alignment: 8 },
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimativeKind {
	AnyCollapse,
	Void,

	UntypedInteger,
	UntypedDecimal,

	Bool,

	Numeric(NumericKind),
}

impl PrimativeKind {
	pub fn name(self) -> &'static str {
		match self {
			PrimativeKind::AnyCollapse => "any collapse",
			PrimativeKind::Void => "void",
			PrimativeKind::UntypedInteger => "untyped integer",
			PrimativeKind::UntypedDecimal => "untyped decimal",
			PrimativeKind::Bool => "bool",
			PrimativeKind::Numeric(numeric) => numeric.name(),
		}
	}

	pub fn layout(self) -> Layout {
		match self {
			PrimativeKind::AnyCollapse => unreachable!(),
			PrimativeKind::Void => Layout { size: 0, alignment: 0 },
			PrimativeKind::UntypedInteger => unreachable!(),
			PrimativeKind::UntypedDecimal => unreachable!(),
			PrimativeKind::Bool => Layout { size: 1, alignment: 1 },
			PrimativeKind::Numeric(numeric) => numeric.layout(),
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

			TypeEntryKind::Pointer { type_id, .. } | TypeEntryKind::Slice(Slice { type_id, .. }) => {
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
	Slice(Slice),
	UserTypeGeneric { shape_index: usize, generic_index: usize },
	FunctionGeneric { function_shape_index: usize, generic_index: usize },
}

#[derive(Debug, Clone, Copy)]
pub struct Slice {
	pub type_id: TypeId,
	pub mutable: bool,
}

#[derive(Debug, Clone, Copy)]
pub struct SliceDescription {
	pub entry: u32, // First immutable, then mutable directly following
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

	any_collapse_type_id: TypeId,
	void_type_id: TypeId,

	bool_type_id: TypeId,

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

	usize_type_id: TypeId,

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
				let symbol = Symbol { name, kind, span: None };
				primative_type_symbols.push(symbol);
			}

			type_id
		};

		let any_collapse_type_id = push_primative(None, PrimativeKind::AnyCollapse);
		let void_type_id = push_primative(Some("void"), PrimativeKind::Void);

		let bool_type_id = push_primative(Some("bool"), PrimativeKind::Bool);

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

		let usize_type_id = push_primative(Some("usize"), PrimativeKind::Numeric(NumericKind::USize));

		let f32_type_id = push_primative(Some("f32"), PrimativeKind::Numeric(NumericKind::F32));
		let f64_type_id = push_primative(Some("f64"), PrimativeKind::Numeric(NumericKind::F64));

		let mut type_store = TypeStore {
			primative_type_symbols,
			type_entries,
			slice_descriptions: Vec::new(),
			user_types: Vec::new(),
			user_type_generate_order: Vec::new(),
			any_collapse_type_id,
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
			usize_type_id,
			f32_type_id,
			f64_type_id,
			string_type_id: TypeId { entry: 0 },
		};

		// This is a hack, consider moving the saved type ids into a different struct
		let string_type_id = type_store.slice_of(u8_type_id, false);
		type_store.string_type_id = string_type_id;

		type_store
	}

	pub fn any_collapse_type_id(&self) -> TypeId {
		self.any_collapse_type_id
	}

	pub fn void_type_id(&self) -> TypeId {
		self.void_type_id
	}

	pub fn bool_type_id(&self) -> TypeId {
		self.bool_type_id
	}

	pub fn integer_type_id(&self) -> TypeId {
		self.integer_type_id
	}

	pub fn decimal_type_id(&self) -> TypeId {
		self.decimal_type_id
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

	pub fn usize_type_id(&self) -> TypeId {
		self.usize_type_id
	}

	pub fn string_type_id(&self) -> TypeId {
		self.string_type_id
	}

	pub fn direct_match(&self, a: TypeId, b: TypeId) -> bool {
		a.entry == b.entry
	}

	pub fn type_list_direct_match(&self, a: &[TypeId], b: &[TypeId]) -> bool {
		if a.len() != b.len() {
			return false;
		}

		for (a, b) in a.iter().zip(b.iter()) {
			if !self.direct_match(*a, *b) {
				return false;
			}
		}

		true
	}

	pub fn collapse_fair(
		&self,
		messages: &mut Messages<'a>,
		function_store: &FunctionStore,
		a: &mut Expression<'a>,
		b: &mut Expression<'a>,
	) -> Option<TypeId> {
		if a.type_id.entry == b.type_id.entry {
			return Some(a.type_id);
		}

		// if either are any collapse then collapse to the other, even if it is also any collapse
		// if both are untyped numbers then we know they are different, collapse to the decimal
		// if either type is an untyped number we know the other isn't, collapse to the other type

		if a.type_id.entry == self.any_collapse_type_id.entry {
			return Some(b.type_id);
		} else if b.type_id.entry == self.any_collapse_type_id.entry {
			return Some(a.type_id);
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
				true => Some(self.decimal_type_id),
				false => None,
			};
		}

		if a_number {
			assert!(!b_number);
			let collapsed = self.collapse_to(messages, function_store, b.type_id, a)?;
			return match collapsed {
				true => Some(b.type_id),
				false => None,
			};
		}

		if b_number {
			assert!(!a_number);
			let collapsed = self.collapse_to(messages, function_store, a.type_id, b)?;
			return match collapsed {
				true => Some(a.type_id),
				false => None,
			};
		}

		None
	}

	pub fn collapse_to(
		&self,
		messages: &mut Messages<'a>,
		function_store: &FunctionStore,
		to: TypeId,
		from: &mut Expression<'a>,
	) -> Option<bool> {
		if to.entry == from.type_id.entry {
			return Some(true);
		}

		// any collapse -> anything else
		// untyped integer -> signed of large enough | unsigned of large enough if not negative | float of large enough | decimal if small enough
		// untyped decimal -> float of large enough
		// mutable reference -> immutable reference
		// mutable slice -> immutable slice

		if from.type_id.entry == self.any_collapse_type_id.entry {
			// From any collapse
			// No need to convert anything, this only gets introduced in case of error so we know we won't codegen
			return Some(true);
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
					return None;
				}

				let type_id = self.decimal_type_id;
				let mutable = from.mutable;
				let returns = from.returns;
				let kind = ExpressionKind::DecimalValue(DecimalValue::new(value as f64, span));
				*from = Expression { span: from.span, type_id, mutable, returns, kind };
				return Some(true);
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
					return None;
				}

				// constant integer -> float of large enough
				from_value.collapse(to);
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
						return None;
					}
				} else {
					let max_value = i128::pow(2, bit_count - 1) - 1;
					if value > max_value {
						let name = self.type_name(function_store, &[], to);
						let error = error!("Constant integer {value} is too large to be represented as {name}");
						messages.message(error.span(span));
						return None;
					}
				}

				// constant integer -> signed of large enough
				from_value.collapse(to);
				return Some(true);
			}

			if to_unsigned {
				if value.is_negative() {
					let name = self.type_name(function_store, &[], to);
					let error = error!("Constant integer {value} is negative and so cannot be represented as {name}",);
					messages.message(error.span(span));
					return None;
				}

				let max_value = i128::pow(2, bit_count) - 1;
				if value > max_value {
					let name = self.type_name(function_store, &[], to);
					let error = error!("Constant integer {value} is too large to be represented as {name}");
					messages.message(error.span(span));
					return None;
				}

				// constant integer -> unsigned of large enough if not negative
				from_value.collapse(to);
				return Some(true);
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
						return None;
					}
				}

				// constant decimal -> float of large enough
				from_value.collapse(to);
				return Some(true);
			}
		}

		// mutable reference -> immutable reference
		let to_entry = self.type_entries[to.index()];
		let from_entry = self.type_entries[from.type_id.index()];
		if let TypeEntryKind::Pointer { type_id: to, mutable: to_mutable } = to_entry.kind {
			if let TypeEntryKind::Pointer { type_id: from, mutable: from_mutable } = from_entry.kind {
				if to.entry == from.entry && from_mutable && !to_mutable {
					return Some(true);
				}
			}
		}

		// mutable slice -> immutable slice
		let to_entry = self.type_entries[to.index()];
		let from_entry = self.type_entries[from.type_id.index()];
		if let TypeEntryKind::Slice(Slice { type_id: to_type_id, mutable: to_mutable }) = to_entry.kind {
			if let TypeEntryKind::Slice(Slice { type_id: from_type_id, mutable: from_mutable }) = from_entry.kind {
				if to_type_id.entry == from_type_id.entry && from_mutable && !to_mutable {
					if let ExpressionKind::ArrayLiteral(literal) = &mut from.kind {
						// See `get_or_create_reference_entries`, mutable slice directly follows immutable slice of same type
						// Back up one entry to turn the array's slice from mutable to immutable
						literal.type_id.entry -= 1;
						return Some(true);
					}

					// TODO: This replace is a dumb solution
					let expression = std::mem::replace(from, Expression::any_collapse(self, Span::unusable()));
					let returns = expression.returns;
					let type_id = TypeId { entry: expression.type_id.entry - 1 };
					let conversion = Box::new(SliceMutableToImmutable { type_id, expression });
					let kind = ExpressionKind::SliceMutableToImmutable(conversion);
					*from = Expression { span: from.span, type_id, mutable: false, returns, kind };
					return Some(true);
				}
			}
		}

		Some(false)
	}

	fn get_or_create_reference_entries(&mut self, type_id: TypeId) -> u32 {
		let entry = &self.type_entries[type_id.index()];
		let generic_poisoned = entry.generic_poisoned;
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

		let kind = TypeEntryKind::Slice(Slice { type_id, mutable: false });
		let description = SliceDescription {
			entry: self.type_entries.len() as u32,
			sliced_type_id: type_id,
		};
		self.type_entries.push(TypeEntry::new(self, kind));
		if !generic_poisoned {
			self.slice_descriptions.push(description);
		}

		let kind = TypeEntryKind::Slice(Slice { type_id, mutable: true });
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

	// TODO: Dear god I need anonymous structs
	// (TypeId, mutable)
	pub fn pointed_to(&self, type_id: TypeId) -> Option<(TypeId, bool)> {
		let entry = self.type_entries[type_id.index()];
		match entry.kind {
			TypeEntryKind::Pointer { type_id, mutable } => Some((type_id, mutable)),
			TypeEntryKind::BuiltinType { kind: PrimativeKind::AnyCollapse } => Some((self.any_collapse_type_id, false)),
			_ => None,
		}
	}

	// (TypeId, mutable)
	pub fn sliced_of(&self, type_id: TypeId) -> Option<(TypeId, bool)> {
		let entry = self.type_entries[type_id.index()];
		match entry.kind {
			TypeEntryKind::Slice(Slice { type_id, mutable }) => Some((type_id, mutable)),
			TypeEntryKind::BuiltinType { kind: PrimativeKind::AnyCollapse } => Some((self.any_collapse_type_id, false)),
			_ => None,
		}
	}

	pub fn register_type(&mut self, name: &'a str, kind: UserTypeKind<'a>, span: Span, module_path: &'a [String]) -> Symbol<'a> {
		// Type entry gets added during specialization
		let shape_index = self.user_types.len();
		self.user_types.push(UserType { span, module_path, kind });

		let kind = SymbolKind::Type { shape_index };
		Symbol { name, kind, span: Some(span) }
	}

	pub fn register_function_generic(&mut self, function_shape_index: usize, generic_index: usize) -> TypeId {
		let entry = self.type_entries.len() as u32;
		let kind = TypeEntryKind::FunctionGeneric { function_shape_index, generic_index };
		self.type_entries.push(TypeEntry::new(self, kind));
		TypeId { entry }
	}

	pub fn calculate_layout(&mut self, type_id: TypeId) {
		let entry = self.type_entries[type_id.index()];
		match entry.kind {
			TypeEntryKind::UserType { shape_index, specialization_index } => {
				let calculated_layout = match &self.user_types[shape_index].kind {
					UserTypeKind::Struct { shape } => {
						let specialization = &shape.specializations[specialization_index];
						if specialization.layout.is_some() {
							return;
						}

						// Belch
						let field_types: Vec<_> = specialization.fields.iter().map(|f| f.type_id).collect();

						let mut size = 0;
						let mut max_field_alignment = 0;

						for type_id in field_types {
							self.calculate_layout(type_id);
							let field_layout = self.type_layout(type_id);

							if field_layout.alignment != 0 {
								size += size % field_layout.alignment;
							}

							size += field_layout.size;
							max_field_alignment = max_field_alignment.max(field_layout.alignment);
						}

						if size != 0 && size % max_field_alignment != 0 {
							size = (size / max_field_alignment) * max_field_alignment + max_field_alignment;
						}

						if !entry.generic_poisoned {
							let description = UserTypeSpecializationDescription { shape_index, specialization_index };
							self.user_type_generate_order.push(description);
						}

						Layout { size, alignment: max_field_alignment }
					}
				};

				match &mut self.user_types[shape_index].kind {
					UserTypeKind::Struct { shape } => {
						shape.specializations[specialization_index].layout = Some(calculated_layout)
					}
				}
			}

			_ => {}
		}
	}

	pub fn type_layout(&self, type_id: TypeId) -> Layout {
		match self.type_entries[type_id.index()].kind {
			TypeEntryKind::BuiltinType { kind } => kind.layout(),

			TypeEntryKind::UserType { shape_index, specialization_index } => match &self.user_types[shape_index].kind {
				UserTypeKind::Struct { shape } => {
					let specialization = &shape.specializations[specialization_index];
					specialization
						.layout
						.expect("should have called `calculate_layout` before `type_layout`")
				}
			},

			TypeEntryKind::Pointer { .. } => Layout { size: 8, alignment: 8 },

			TypeEntryKind::Slice(_) => Layout { size: 16, alignment: 16 },

			// TODO: These are probably wrong, take care to make sure this doesn't break size_of in generic functions
			TypeEntryKind::UserTypeGeneric { .. } => Layout { size: 0, alignment: 0 },
			TypeEntryKind::FunctionGeneric { .. } => Layout { size: 0, alignment: 0 },
		}
	}

	pub fn find_user_type_dependency_chain(&self, from: TypeId, to: TypeId) -> Option<Vec<UserTypeChainLink<'a>>> {
		let entry = self.type_entries[from.index()];
		let (shape_index, specialization_index) = match entry.kind {
			TypeEntryKind::UserType { shape_index, specialization_index } => (shape_index, specialization_index),
			_ => return None,
		};

		let user_type = &self.user_types[shape_index];
		match &user_type.kind {
			UserTypeKind::Struct { shape } => {
				let specialization = &shape.specializations[specialization_index];
				for field in &specialization.fields {
					if self.direct_match(field.type_id, to) {
						let link = UserTypeChainLink {
							user_type: specialization.type_id,
							field_name: field.name,
							field_span: field.span.expect("User type fields should always have a span"),
						};
						return Some(vec![link]);
					}

					if let Some(mut chain) = self.find_user_type_dependency_chain(field.type_id, to) {
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
		}

		None
	}

	pub fn lookup_type(
		&mut self,
		messages: &mut Messages<'a>,
		function_store: &mut FunctionStore<'a>,
		module_path: &'a [String],
		generic_usages: &mut Vec<GenericUsage>,
		root_layers: &RootLayers<'a>,
		symbols: &Symbols<'a>,
		function_initial_symbols_len: usize,
		parsed_type: &Node<tree::Type<'a>>,
	) -> Option<TypeId> {
		let (path_segments, type_arguments) = match &parsed_type.item {
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
					pointee,
				)?;
				return Some(self.slice_of(id, *mutable));
			}

			tree::Type::Path { path_segments, type_arguments } => (path_segments, type_arguments),
		};

		assert!(!path_segments.item.segments.is_empty());
		let symbol = symbols.lookup_symbol(messages, root_layers, self, function_initial_symbols_len, &path_segments.item)?;

		let shape_index = match symbol.kind {
			SymbolKind::BuiltinType { type_id } => {
				if !type_arguments.is_empty() {
					messages.message(error!("Builtin types do not accept type arguments").span(parsed_type.span));
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
				let generic = &generics.parameters()[generic_index];
				return Some(generic.generic_type_id);
			}

			_ => {
				messages.message(error!("Symbol {:?} is not a type", symbol.name).span(path_segments.span));
				return None;
			}
		};

		let mut type_args = Vec::with_capacity(type_arguments.len());
		for argument in type_arguments {
			let id = self.lookup_type(
				messages,
				function_store,
				module_path,
				generic_usages,
				root_layers,
				symbols,
				function_initial_symbols_len,
				argument,
			)?;
			type_args.push(id);
		}

		let invoke_span = Some(parsed_type.span);
		self.get_or_add_shape_specialization(
			messages,
			function_store,
			module_path,
			generic_usages,
			shape_index,
			invoke_span,
			type_args,
		)
	}

	pub fn get_or_add_shape_specialization(
		&mut self,
		messages: &mut Messages<'a>,
		function_store: &FunctionStore<'a>,
		module_path: &'a [String],
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
			messages.message(
				error!(
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
			if self.type_list_direct_match(&existing.type_arguments, &type_arguments) {
				return Some(existing.type_id);
			}
		}

		let type_arguments_generic_poisoned = type_arguments.iter().any(|id| self.type_entries[id.index()].generic_poisoned);

		let mut fields = Vec::with_capacity(shape.fields.len());
		for field in &shape.fields {
			fields.push(Field {
				span: Some(field.span),
				name: field.item.name,
				type_id: field.item.field_type,
			});
		}

		for field in &mut fields {
			field.type_id = self.specialize_with_user_type_generics(
				messages,
				function_store,
				module_path,
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
		let been_filled = shape.been_filled;
		let specialization = Struct {
			type_id,
			type_arguments: type_arguments.clone(),
			been_filled,
			fields,
			layout: None,
		};
		shape.specializations.push(specialization);

		let entry = TypeEntry::new(self, TypeEntryKind::UserType { shape_index, specialization_index });
		self.type_entries.push(entry);

		if type_arguments_generic_poisoned {
			let usage = GenericUsage::UserType { type_arguments, shape_index };
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
		module_path: &'a [String],
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
						let any_user_type_generic = specialization
							.type_arguments
							.iter()
							.any(|t| matches!(self.type_entries[t.index()].kind, TypeEntryKind::UserTypeGeneric { .. }));

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
							function_store,
							module_path,
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

			TypeEntryKind::UserTypeGeneric { shape_index, generic_index } => {
				assert_eq!(type_shape_index, *shape_index);
				type_arguments[*generic_index]
			}

			TypeEntryKind::FunctionGeneric { .. } => unreachable!(),
		}
	}

	pub fn specialize_with_function_generics(
		&mut self,
		messages: &mut Messages<'a>,
		function_store: &FunctionStore<'a>,
		module_path: &'a [String],
		generic_usages: &mut Vec<GenericUsage>,
		function_shape_index: usize,
		function_type_arguments: &TypeArguments,
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
						let any_function_generic = specialization
							.type_arguments
							.iter()
							.any(|t| matches!(self.type_entries[t.index()].kind, TypeEntryKind::FunctionGeneric { .. }));

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
									new_struct_type_arguments.push(function_type_arguments.ids()[*generic_index]);
								}

								_ => new_struct_type_arguments.push(struct_type_argument),
							}
						}

						self.get_or_add_shape_specialization(
							messages,
							function_store,
							module_path,
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

			TypeEntryKind::UserTypeGeneric { .. } => unreachable!(),

			TypeEntryKind::FunctionGeneric { function_shape_index: shape_index, generic_index } => {
				assert_eq!(function_shape_index, *shape_index);
				function_type_arguments.ids()[*generic_index]
			}
		}
	}

	pub fn type_name(&self, function_store: &FunctionStore, module_path: &'a [String], type_id: TypeId) -> String {
		format!("`{}`", self.internal_type_name(function_store, module_path, type_id))
	}

	// TODO: Use module path to have or not have paths for local types?
	pub fn internal_type_name(&self, function_store: &FunctionStore, _module_path: &'a [String], type_id: TypeId) -> String {
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
							.map(|a| self.internal_type_name(function_store, _module_path, *a))
							.collect::<Vec<_>>()
							.join(", ");

						format!("{}<{}>", shape.name, type_arguments)
					}
				}
			}

			TypeEntryKind::Pointer { type_id, mutable } => {
				let inner = self.internal_type_name(function_store, _module_path, type_id);
				match mutable {
					true => format!("&mut {}", inner),
					false => format!("&{}", inner),
				}
			}

			TypeEntryKind::Slice(Slice { type_id, mutable }) => {
				let inner = self.internal_type_name(function_store, _module_path, type_id);
				match mutable {
					true => format!("[]mut {}", inner),
					false => format!("[]{}", inner),
				}
			}

			TypeEntryKind::FunctionGeneric { function_shape_index, generic_index } => {
				let shape = &function_store.shapes[function_shape_index];
				let generic = &shape.generics.parameters()[generic_index];
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