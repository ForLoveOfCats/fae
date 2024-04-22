use crate::frontend::type_store::{NumericKind, PrimativeKind, TypeEntryKind, TypeId, TypeStore, UserTypeKind};

#[derive(Debug, Clone, Copy)]
pub struct Class {
	pub kind: ClassKind,
	pub size: u8,
}

impl std::default::Default for Class {
	fn default() -> Self {
		Class { kind: ClassKind::NoClass, size: 0 }
	}
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClassKind {
	Integer,
	Boolean,
	Pointer,
	SSE,
	SSECombine,
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
		Class::default(),
		Class::default(),
		Class::default(),
		Class::default(),
		Class::default(),
		Class::default(),
		Class::default(),
		Class::default(),
	]
}

// Huge thanks to the Zig selfhost compiler for making the spec algorithm make sense
pub fn classify_type<'buf>(type_store: &TypeStore, buffer: &'buf mut [Class; 8], type_id: TypeId) -> &'buf mut [Class] {
	let entry = type_store.type_entries[type_id.index()];

	match entry.kind {
		TypeEntryKind::BuiltinType { kind } => match kind {
			PrimativeKind::Bool => {
				buffer[0] = Class { kind: ClassKind::Boolean, size: 1 };
				return &mut buffer[..1];
			}

			PrimativeKind::Numeric(kind) => {
				let class = match kind {
					NumericKind::I8 | NumericKind::U8 => Class { kind: ClassKind::Integer, size: 1 },
					NumericKind::I16 | NumericKind::U16 => Class { kind: ClassKind::Integer, size: 2 },
					NumericKind::I32 | NumericKind::U32 => Class { kind: ClassKind::Integer, size: 4 },
					NumericKind::I64 | NumericKind::U64 | NumericKind::ISize | NumericKind::USize => {
						Class { kind: ClassKind::Integer, size: 8 }
					}

					NumericKind::F32 => Class { kind: ClassKind::SSE, size: 4 },
					NumericKind::F64 => Class { kind: ClassKind::SSE, size: 8 },
				};

				buffer[0] = class;
				return &mut buffer[..1];
			}

			PrimativeKind::AnyCollapse | PrimativeKind::Void | PrimativeKind::UntypedInteger | PrimativeKind::UntypedDecimal => {
				unreachable!("{:?}", entry.kind);
			}
		},

		TypeEntryKind::UserType { shape_index, specialization_index } => {
			let aggregate_layout = type_store.type_layout(type_id);

			// 1. If the size of an object is larger than eight eightbytes, or it contains unaligned fields, it has class MEMORY.
			// Fae does not currently support unaligned/packed struct fields, so we don't need to worry about that
			if aggregate_layout.size > 8 * 8 {
				buffer[0] = Class { kind: ClassKind::Memory, size: 8 };
				return &mut buffer[..1];
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
				if field_layout.size <= 0 {
					continue;
				}

				let mut field_buffer = classification_buffer();
				let field_classes = classify_type(type_store, &mut field_buffer, field.type_id);
				assert!(field_classes[0].kind != ClassKind::NoClass, "{:?}", field_classes[0]);
				if field_classes[0].kind == ClassKind::Boolean {
					field_classes[0].kind = ClassKind::Integer;
				}

				if field_layout.size + combine_size <= 8 {
					// Combine with prior fields to make an eightbyte

					// (a) If both classes are equal, this is the resulting class.
					if buffer[buffer_index].kind == field_classes[0].kind {
						assert_eq!(field_classes.len(), 1, "{}", field_classes.len());
						if buffer[buffer_index].kind == ClassKind::SSE {
							// Doesn't get confused by SSEUP as that can never be less than 8 bytes
							buffer[buffer_index].kind = ClassKind::SSECombine;
						}
						buffer[buffer_index].size += field_classes[0].size;
					}
					// (b) If one of the classes is NO_CLASS, the resulting class is the other class.
					else if buffer[buffer_index].kind == ClassKind::NoClass {
						buffer[buffer_index] = field_classes[0];
					}
					// (c) If one of the classes is MEMORY, the result is the MEMORY class.
					else if buffer[buffer_index].kind == ClassKind::Memory || field_classes[0].kind == ClassKind::Memory {
						buffer[buffer_index] = Class { kind: ClassKind::Memory, size: 8 };
					}
					// (d) If one of the classes is INTEGER, the result is the INTEGER.
					else if buffer[buffer_index].kind == ClassKind::Integer || field_classes[0].kind == ClassKind::Integer {
						assert_eq!(field_classes.len(), 1);
						let size = buffer[buffer_index].size + field_classes[0].size;
						buffer[buffer_index] = Class { kind: ClassKind::Integer, size };
					}
					// (e) If one of the classes is X87, X87UP, COMPLEX_X87 class, MEMORY is used as class.
					else if buffer[buffer_index].kind == ClassKind::X87
						|| buffer[buffer_index].kind == ClassKind::X87Up
						|| buffer[buffer_index].kind == ClassKind::ComplexX87
						|| field_classes[0].kind == ClassKind::X87
						|| field_classes[0].kind == ClassKind::X87Up
						|| field_classes[0].kind == ClassKind::ComplexX87
					{
						buffer[buffer_index] = Class { kind: ClassKind::Memory, size: 8 };
					}
					// (f) Otherwise class SSE is used.
					else {
						// A pointer is already 8 bytes so we should never see one flow through this if-else ladder
						assert_ne!(buffer[buffer_index].kind, ClassKind::Pointer);
						assert_ne!(field_classes[0].kind, ClassKind::Pointer);
						buffer[buffer_index] = Class { kind: ClassKind::SSE, size: field_layout.size as u8 };
					}

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
					result_slice.copy_from_slice(field_classes);
					buffer_index += field_classes.len();

					if field_layout.size > 8 {
						combine_size = 0;
					} else {
						combine_size = field_layout.size;
						if combine_size > 0 {
							buffer_index -= 1;
						}
					}
				}
			}

			// 5. Then a post merger cleanup is done:

			let mut contains_sse_up = false;
			for (index, &class) in buffer.iter().enumerate() {
				// (a) If one of the classes is MEMORY, the whole argument is passed in memory.
				if class.kind == ClassKind::Memory {
					buffer[0] = Class { kind: ClassKind::Memory, size: 8 };
					return &mut buffer[..1];
				}

				// (b) If X87UP is not preceded by X87, the whole argument is passed in memory.
				if class.kind == ClassKind::X87Up && index > 0 && buffer[index - 1].kind != ClassKind::X87 {
					buffer[0] = Class { kind: ClassKind::Memory, size: 8 };
					return &mut buffer[..1];
				}

				contains_sse_up |= class.kind == ClassKind::SSEUp;
			}

			// (c) If the size of the aggregate exceeds two eightbytes and the first eightbyte isn’t SSE or any
			// other eightbyte isn’t SSEUP, the whole argument is passed in memory.
			let first_eightbyte_sse = buffer[0].kind == ClassKind::SSE;
			if aggregate_layout.size > 16 && (!first_eightbyte_sse || !contains_sse_up) {
				buffer[0] = Class { kind: ClassKind::Memory, size: 8 };
				return &mut buffer[..1];
			}

			// (d) If SSEUP is not preceded by SSE or SSEUP, it is converted to SSE.
			for index in 0..buffer.len() {
				let has_preceeding = index > 0;
				if buffer[index].kind == ClassKind::SSEUp && has_preceeding {
					let preceding = buffer[index - 1];
					if !(preceding.kind == ClassKind::SSE || preceding.kind == ClassKind::SSEUp) {
						buffer[index] = Class { kind: ClassKind::SSE, size: buffer[index].size };
					}
				}
			}

			let mut contents_len = buffer.len();
			for (index, &class) in buffer.iter().enumerate() {
				if class.kind == ClassKind::NoClass {
					contents_len = index;
					break;
				}
			}

			return &mut buffer[..contents_len];
		}

		TypeEntryKind::Pointer { .. } => {
			buffer[0] = Class { kind: ClassKind::Pointer, size: 8 };
			return &mut buffer[..1];
		}

		TypeEntryKind::Slice(_) => {
			buffer[0] = Class { kind: ClassKind::Pointer, size: 8 };
			buffer[1] = Class { kind: ClassKind::Integer, size: 8 };
			return &mut buffer[..2];
		}

		TypeEntryKind::UserTypeGeneric { .. } | TypeEntryKind::FunctionGeneric { .. } => unreachable!(),
	}
}
