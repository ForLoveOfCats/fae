use crate::codegen::classification::{Class, ClassKind, Classifier};
use crate::frontend::type_store::{NumericKind, PrimitiveKind, TypeEntryKind, TypeId, TypeStore};

#[allow(unused)]
pub struct WindowsClassifier {
	class: Class,
}

impl Classifier for WindowsClassifier {
	fn new() -> Self {
		WindowsClassifier { class: Class::default() }
	}

	// Thanks to the Zig selfhost compiler for directing me to the appropriate documentation!
	// https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention
	fn classify_type<'a>(&'a mut self, type_store: &mut TypeStore, type_id: TypeId) -> &'a [Class] {
		let entry = type_store.type_entries.get(type_id);
		self.class = match entry.kind {
			TypeEntryKind::BuiltinType { kind, .. } => match kind {
				PrimitiveKind::Numeric(numeric_kind) => match numeric_kind {
					NumericKind::I8 | NumericKind::U8 => Class { kind: ClassKind::Integer, size: 1 },
					NumericKind::I16 | NumericKind::U16 => Class { kind: ClassKind::Integer, size: 2 },
					NumericKind::I32 | NumericKind::U32 => Class { kind: ClassKind::Integer, size: 4 },
					NumericKind::I64 | NumericKind::U64 | NumericKind::ISize | NumericKind::USize => {
						Class { kind: ClassKind::Integer, size: 8 }
					}

					NumericKind::F32 => Class { kind: ClassKind::SSE, size: 4 },
					NumericKind::F64 => Class { kind: ClassKind::SSE, size: 8 },
				},

				PrimitiveKind::Bool => Class { kind: ClassKind::Boolean, size: 1 },

				PrimitiveKind::String | PrimitiveKind::StringMut | PrimitiveKind::FormatString => {
					Class { kind: ClassKind::Memory, size: 8 }
				}

				PrimitiveKind::AnyCollapse | PrimitiveKind::NoReturn | PrimitiveKind::Void | PrimitiveKind::UntypedNumber => {
					unreachable!("{:?}", entry.kind);
				}
			},

			TypeEntryKind::UserType { .. } => {
				let size = type_store.type_layout(type_id).size;

				if size > 8 {
					Class { kind: ClassKind::Memory, size: 8 }
				} else {
					// > Any argument that doesn't fit in 8 bytes, or isn't 1, 2, 4, or 8 bytes, must be passed by reference.
					assert!(size == 1 || size == 2 || size == 4 || size == 8);
					Class { kind: ClassKind::Integer, size: size as u8 }
				}
			}

			TypeEntryKind::Pointer { .. } => Class { kind: ClassKind::Pointer, size: 8 },

			TypeEntryKind::Array(_) => {
				let size = type_store.type_layout(type_id).size;

				if size > 8 {
					Class { kind: ClassKind::Memory, size: 8 }
				} else {
					// > Any argument that doesn't fit in 8 bytes, or isn't 1, 2, 4, or 8 bytes, must be passed by reference.
					assert!(size == 1 || size == 2 || size == 4 || size == 8);
					Class { kind: ClassKind::Integer, size: size as u8 }
				}
			}

			TypeEntryKind::Slice(_) => Class { kind: ClassKind::Memory, size: 8 },

			TypeEntryKind::Module
			| TypeEntryKind::Type
			| TypeEntryKind::UserTypeGeneric { .. }
			| TypeEntryKind::FunctionGeneric { .. }
			| TypeEntryKind::TraitGeneric { .. } => unreachable!(),
		};

		std::slice::from_ref(&self.class)
	}
}
