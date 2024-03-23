use crate::type_store::{NumericKind, PrimativeKind, TypeEntryKind, TypeId, TypeStore, UserTypeKind};

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Class {
	Integer,
	SSE,
	SSEUp,
	X87,
	X87Up,
	ComplexX87,
	Memory,
	NoClass,
}

#[inline]
pub fn classification_buffer() -> [Class; 8] {
	[
		Class::NoClass,
		Class::NoClass,
		Class::NoClass,
		Class::NoClass,
		Class::NoClass,
		Class::NoClass,
		Class::NoClass,
		Class::NoClass,
	]
}

// Huge thanks to the Zig selfhost compiler for making the spec algorithm make sense
// It's technically more efficient to pass the buffer around by value but slices are so convenient
pub fn classify_type<'buf>(type_store: &TypeStore, buffer: &'buf mut [Class; 8], type_id: TypeId) -> &'buf [Class] {
	let entry = type_store.type_entries[type_id.index()];

	match entry.kind {
		TypeEntryKind::BuiltinType { kind } => match kind {
			PrimativeKind::Bool => {
				buffer[0] = Class::Integer;
				return &buffer[..1];
			}

			PrimativeKind::Numeric(kind) => match kind {
				NumericKind::I8
				| NumericKind::I16
				| NumericKind::I32
				| NumericKind::I64
				| NumericKind::U8
				| NumericKind::U16
				| NumericKind::U32
				| NumericKind::U64
				| NumericKind::USize => {
					buffer[0] = Class::Integer;
					return &buffer[..1];
				}

				NumericKind::F32 | NumericKind::F64 => {
					buffer[0] = Class::SSE;
					return &buffer[..1];
				}
			},

			PrimativeKind::AnyCollapse | PrimativeKind::Void | PrimativeKind::UntypedInteger | PrimativeKind::UntypedDecimal => {
				unreachable!()
			}
		},

		TypeEntryKind::UserType { shape_index, specialization_index } => {
			let aggregate_layout = type_store.type_layout(type_id);

			// 1. If the size of an object is larger than eight eightbytes, or it contains unaligned fields, it has class MEMORY.
			// Fae does not currently support unaligned/packed struct fields, so we don't need to worry about that
			if aggregate_layout.size > 8 * 8 {
				buffer[0] = Class::Memory;
				return &buffer[..1];
			}

			let user_type = &type_store.user_types[shape_index];
			let shape = match &user_type.kind {
				UserTypeKind::Struct { shape } => shape,
			};
			let specialization = &shape.specializations[specialization_index];

			// 2. If the size of the aggregate exceeds a single eightbyte, each is classified separately. Each eightbyte gets
			// initialized to class NO_CLASS.
			// 3. Each field of an object is classified recursively so that always two fields 18 are considered. The resulting
			// class is calculated according to the classes of the fields in the eightbyte:

			let mut combine_size = 0;
			let mut buffer_index = 0;

			for field in &specialization.fields {
				let field_layout = type_store.type_layout(field.type_id);

				let mut field_buffer = classification_buffer();
				let field_classes = classify_type(type_store, &mut field_buffer, field.type_id);
				assert!(field_classes[0] != Class::NoClass, "{:?}", field_classes[0]);

				if field_layout.size + combine_size <= 8 {
					// Combine with prior fields to make an eightbyte

					// (a) If both classes are equal, this is the resulting class.
					if buffer[buffer_index] == field_classes[0] {
						assert_eq!(field_classes.len(), 1, "{}", field_classes.len()) // Is this correct?
					}
					// (b) If one of the classes is NO_CLASS, the resulting class is the other class.
					else if buffer[buffer_index] == Class::NoClass {
						buffer[buffer_index] = field_classes[0];
					}
					// (c) If one of the classes is MEMORY, the result is the MEMORY class.
					else if buffer[buffer_index] == Class::Memory || field_classes[0] == Class::Memory {
						buffer[buffer_index] = Class::Memory;
					}
					// (d) If one of the classes is INTEGER, the result is the INTEGER.
					else if buffer[buffer_index] == Class::Integer || field_classes[0] == Class::Integer {
						buffer[buffer_index] = Class::Integer;
					}
					// (e) If one of the classes is X87, X87UP, COMPLEX_X87 class, MEMORY is used as class.
					else if buffer[buffer_index] == Class::X87
						|| buffer[buffer_index] == Class::X87Up
						|| buffer[buffer_index] == Class::ComplexX87
						|| field_classes[0] == Class::X87
						|| field_classes[0] == Class::X87Up
						|| field_classes[0] == Class::ComplexX87
					{
						buffer[buffer_index] = Class::Memory;
					}
					// (f) Otherwise class SSE is used.
					buffer[buffer_index] = Class::SSE;

					combine_size += field_layout.size;
					assert!(combine_size <= 8, "{combine_size}");
					if combine_size == 8 {
						combine_size = 0;
						buffer_index += 1;
					}
				} else {
					// To large to combine

					if combine_size > 0 {
						buffer_index += 1;
					}

					let result_slice = &mut buffer[buffer_index..buffer_index + field_classes.len()];
					result_slice.copy_from_slice(&field_classes);
					buffer_index += field_classes.len();

					assert!(field_layout.size <= 8, "{}", field_layout.size);
					combine_size = field_layout.size;
					if combine_size > 0 {
						buffer_index -= 1;
					}
				}
			}

			// 5. Then a post merger cleanup is done:

			let mut contains_sse_up = false;
			for (index, &class) in buffer.iter().enumerate() {
				// (a) If one of the classes is MEMORY, the whole argument is passed in memory.
				if class == Class::Memory {
					buffer[0] = Class::Memory;
					return &buffer[..1];
				}

				// (b) If X87UP is not preceded by X87, the whole argument is passed in memory.
				if class == Class::X87Up && index > 0 && buffer[index - 1] != Class::X87 {
					buffer[0] = Class::Memory;
					return &buffer[..1];
				}

				if class == Class::SSEUp {
					contains_sse_up = true;
				}
			}

			// (c) If the size of the aggregate exceeds two eightbytes and the first eightbyte isn’t SSE or any
			// other eightbyte isn’t SSEUP, the whole argument is passed in memory.
			let first_eightbyte_sse = buffer[0] == Class::SSE;
			if aggregate_layout.size > 16 && (!first_eightbyte_sse || !contains_sse_up) {
				buffer[0] = Class::Memory;
				return &buffer[..1];
			}

			// (d) If SSEUP is not preceded by SSE or SSEUP, it is converted to SSE.
			for index in 0..buffer.len() {
				let has_preceeding = index > 0;
				if buffer[index] == Class::SSEUp && has_preceeding {
					let preceding = buffer[index - 1];
					if preceding == Class::SSE || preceding == Class::SSEUp {
						buffer[index] = Class::SSE;
					}
				}
			}

			let mut contents_len = buffer.len();
			for (index, &class) in buffer.iter().enumerate() {
				if class == Class::NoClass {
					contents_len = index + 1;
					break;
				}
			}

			return &buffer[..contents_len];
		}

		TypeEntryKind::Pointer { .. } => {
			buffer[0] = Class::Integer;
			return &buffer[..1];
		}

		TypeEntryKind::Slice(_) => {
			buffer[0] = Class::Integer;
			buffer[1] = Class::Integer;
			return &buffer[..2];
		}

		TypeEntryKind::UserTypeGeneric { .. } | TypeEntryKind::FunctionGeneric { .. } => unreachable!(),
	}
}
