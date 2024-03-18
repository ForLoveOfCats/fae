use crate::type_store::{TypeEntryKind, TypeId, TypeStore};

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
		TypeEntryKind::BuiltinType { .. } => ParemeterClass::Integer,

		TypeEntryKind::UserType { shape_index, specialization_index } => todo!(),

		TypeEntryKind::Pointer { .. } => ParemeterClass::Integer,

		TypeEntryKind::Slice(_) => todo!(),

		TypeEntryKind::UserTypeGeneric { .. } | TypeEntryKind::FunctionGeneric { .. } => unreachable!(),
	}
}
