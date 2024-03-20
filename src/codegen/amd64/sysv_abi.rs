use crate::type_store::{NumericKind, PrimativeKind, TypeEntryKind, TypeId, TypeStore, UserTypeKind};

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

		TypeEntryKind::UserType { shape_index, specialization_index } => {
			let layout = type_store.type_layout(type_id);

			// ABI: If the size of an object is larger than eight eightbytes, or it contains unaligned fields, it has class MEMORY.
			if layout.size > 8 * 8 {
				return ParemeterClass::Memory;
			}

			// ABI: If the size of the aggregate exceeds a single eightbyte, each is classified separately. Each eightbyte gets
			// initialized to class NO_CLASS.

			let user_type = &type_store.user_types[shape_index];
			let shape = match &user_type.kind {
				UserTypeKind::Struct { shape } => shape,
			};
			let specialization = &shape.specializations[specialization_index];

			for field in &specialization.fields {
				let field_class = classify_parameter_type(type_store, field.type_id);
			}

			todo!()
		}

		TypeEntryKind::Pointer { .. } => ParemeterClass::Integer,

		TypeEntryKind::Slice(_) => todo!(),

		TypeEntryKind::UserTypeGeneric { .. } | TypeEntryKind::FunctionGeneric { .. } => unreachable!(),
	}
}
