use crate::type_store::{NumericKind, PrimativeKind, TypeEntryKind, TypeId, TypeStore};

pub enum ParemeterClass {
	Integer,
	SSE,
	SSEUp,
	X87,
	X87Up,
	ComplexX87,
	Memory,
}

pub fn classify_parameter_type(type_store: &TypeStore, type_id: TypeId) -> ParemeterClass {
	let entry = type_store.type_entries[type_id.index()];

	match entry.kind {
		TypeEntryKind::BuiltinType { kind } => match kind {
			PrimativeKind::Bool => ParemeterClass::Integer,

			PrimativeKind::Numeric(kind) => match kind {
				NumericKind::I8
				| NumericKind::I16
				| NumericKind::I32
				| NumericKind::I64
				| NumericKind::U8
				| NumericKind::U16
				| NumericKind::U32
				| NumericKind::U64
				| NumericKind::USize => ParemeterClass::Integer,

				NumericKind::F32 | NumericKind::F64 => ParemeterClass::SSE,
			},

			PrimativeKind::AnyCollapse | PrimativeKind::Void | PrimativeKind::UntypedInteger | PrimativeKind::UntypedDecimal => {
				unreachable!()
			}
		},

		TypeEntryKind::UserType { shape_index, specialization_index } => todo!(),

		TypeEntryKind::Pointer { .. } => ParemeterClass::Integer,

		TypeEntryKind::Slice(_) => todo!(),

		TypeEntryKind::UserTypeGeneric { .. } | TypeEntryKind::FunctionGeneric { .. } => unreachable!(),
	}
}
