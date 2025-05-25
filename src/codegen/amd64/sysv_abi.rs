use crate::frontend::type_store::{Array, Layout, NumericKind, PrimativeKind, TypeEntryKind, TypeId, TypeStore, UserTypeKind};

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
pub fn classify_type<'buf>(type_store: &mut TypeStore, buffer: &'buf mut [Class; 8], type_id: TypeId) -> &'buf mut [Class] {
	let entry = type_store.type_entries.get(type_id);

	match entry.kind {
		TypeEntryKind::BuiltinType { kind, .. } => match kind {
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

			PrimativeKind::Bool => {
				buffer[0] = Class { kind: ClassKind::Boolean, size: 1 };
				return &mut buffer[..1];
			}

			PrimativeKind::String | PrimativeKind::StringMut | PrimativeKind::FormatString => {
				buffer[0] = Class { kind: ClassKind::Pointer, size: 8 };
				buffer[1] = Class { kind: ClassKind::Integer, size: 8 };
				return &mut buffer[..2];
			}

			PrimativeKind::AnyCollapse | PrimativeKind::NoReturn | PrimativeKind::Void | PrimativeKind::UntypedNumber => {
				unreachable!("{:?}", entry.kind);
			}
		},

		TypeEntryKind::UserType { shape_index, specialization_index, .. } => {
			let aggregate_layout = type_store.type_layout(type_id);

			// 1. If the size of an object is larger than eight eightbytes, or it contains unaligned fields, it has class MEMORY.
			// Fae does not currently support unaligned/packed struct fields, so we don't need to worry about that
			if aggregate_layout.size > 8 * 8 {
				buffer[0] = Class { kind: ClassKind::Memory, size: 8 };
				return &mut buffer[..1];
			}

			let user_type = type_store.user_types.read()[shape_index].clone();
			let user_type = user_type.read();

			match &user_type.kind {
				UserTypeKind::Struct { shape } => {
					let specialization = &shape.specializations[specialization_index];

					// 2. If a C++ object is non-trivial for the purpose of calls, as specified in the C++ ABI, it is passed by
					// invisible reference (the object is replaced in the parameter list by a pointer that has class INTEGER).
					// 3. If the size of the aggregate exceeds a single eightbyte, each is classified separately. Each eightbyte gets
					// initialized to class NO_CLASS.
					// 4. Each field of an object is classified recursively so that always two fields 18 are considered. The resulting
					// class is calculated according to the classes of the fields in the eightbyte:
					classify_merge_fields(type_store, buffer, specialization.fields.iter().map(|f| f.type_id));

					// 5. Then a post merger cleanup is done:
					return post_merge_cleanup(aggregate_layout, buffer);
				}

				UserTypeKind::Enum { .. } => {
					if aggregate_layout.size > 8 {
						buffer[0] = Class { kind: ClassKind::Memory, size: 8 };
						return &mut buffer[..1];
					} else {
						let size = if 0 <= aggregate_layout.size && aggregate_layout.size <= 8 {
							aggregate_layout.size as u8
						} else {
							unreachable!("{:?}", aggregate_layout);
						};
						buffer[0] = Class { kind: ClassKind::Integer, size };
						return &mut buffer[..1];
					}
				}

				UserTypeKind::Union { shape, .. } => {
					let specialization = &shape.specializations[specialization_index];

					let mut output_len = 0;
					for field in specialization.fields.iter() {
						let mut field_buffer = classification_buffer();
						let field_classes = classify_type(type_store, &mut field_buffer, field.type_id);
						output_len = output_len.max(field_classes.len());

						for (index, field_class) in field_classes.iter().enumerate() {
							buffer[index] = merge_classes_union(buffer[index], *field_class);
						}
					}

					return post_merge_cleanup(aggregate_layout, buffer);
				}
			}
		}

		TypeEntryKind::Pointer { .. } => {
			buffer[0] = Class { kind: ClassKind::Pointer, size: 8 };
			return &mut buffer[..1];
		}

		TypeEntryKind::Array(Array { item_type_id, length, .. }) => {
			let array_layout = type_store.type_layout(type_id);
			classify_merge_fields(type_store, buffer, (0..length).map(|_| item_type_id));
			return post_merge_cleanup(array_layout, buffer);
		}

		TypeEntryKind::Slice(_) => {
			buffer[0] = Class { kind: ClassKind::Pointer, size: 8 };
			buffer[1] = Class { kind: ClassKind::Integer, size: 8 };
			return &mut buffer[..2];
		}

		TypeEntryKind::Module
		| TypeEntryKind::Type
		| TypeEntryKind::UserTypeGeneric { .. }
		| TypeEntryKind::FunctionGeneric { .. }
		| TypeEntryKind::TraitGeneric { .. } => unreachable!(),
	}
}

fn classify_merge_fields<I: Iterator<Item = TypeId>>(type_store: &mut TypeStore, buffer: &mut [Class; 8], field_type_ids: I) {
	let mut combine_size = 0;
	let mut buffer_index = 0;

	for field_type_id in field_type_ids {
		let field_layout = type_store.type_layout(field_type_id);
		if field_layout.size <= 0 {
			continue;
		}

		let mut field_buffer = classification_buffer();
		let field_classes = classify_type(type_store, &mut field_buffer, field_type_id);
		assert!(field_classes[0].kind != ClassKind::NoClass, "{:?}", field_classes[0]);
		if field_classes[0].kind == ClassKind::Boolean {
			field_classes[0].kind = ClassKind::Integer;
		}

		if field_layout.size + combine_size <= 8 {
			// Combine with prior fields to make an eightbyte

			assert_eq!(field_classes.len(), 1);
			buffer[buffer_index] = merge_classes_struct(buffer[buffer_index], field_classes[0]);

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
}

fn merge_classes_struct(mut a: Class, b: Class) -> Class {
	// (a) If both classes are equal, this is the resulting class.
	if a.kind == b.kind {
		if a.kind == ClassKind::SSE {
			// Doesn't get confused by SSEUP as that can never be less than 8 bytes
			a.kind = ClassKind::SSECombine;
		}
		a.size += b.size;
		assert!(a.size <= 8, "{:?}", a.size);
		return a;
	}
	// (b) If one of the classes is NO_CLASS, the resulting class is the other class.
	else if a.kind == ClassKind::NoClass {
		return b;
	}
	// (c) If one of the classes is MEMORY, the result is the MEMORY class.
	else if a.kind == ClassKind::Memory || b.kind == ClassKind::Memory {
		return Class { kind: ClassKind::Memory, size: 8 };
	}
	// (d) If one of the classes is INTEGER, the result is the INTEGER.
	else if a.kind == ClassKind::Integer || b.kind == ClassKind::Integer {
		let size = a.size + b.size;
		assert!(size <= 8, "{size:?}");
		return Class { kind: ClassKind::Integer, size };
	}
	// (e) If one of the classes is X87, X87UP, COMPLEX_X87 class, MEMORY is used as class.
	else if a.kind == ClassKind::X87
		|| a.kind == ClassKind::X87Up
		|| a.kind == ClassKind::ComplexX87
		|| b.kind == ClassKind::X87
		|| b.kind == ClassKind::X87Up
		|| b.kind == ClassKind::ComplexX87
	{
		return Class { kind: ClassKind::Memory, size: 8 };
	}
	// (f) Otherwise class SSE is used.
	else {
		// A pointer is already 8 bytes so we should never see one flow through this if-else ladder
		assert_ne!(a.kind, ClassKind::Pointer);
		assert_ne!(b.kind, ClassKind::Pointer);
		let size = a.size + b.size;
		assert!(size <= 8, "{size:?}");
		return Class { kind: ClassKind::SSE, size };
	}
}

fn merge_classes_union(mut a: Class, b: Class) -> Class {
	// (a) If both classes are equal, this is the resulting class.
	if a.kind == b.kind {
		if a.kind == ClassKind::SSE {
			// Doesn't get confused by SSEUP as that can never be less than 8 bytes
			a.kind = ClassKind::SSECombine;
		}
		a.size = a.size.max(b.size);
		assert!(a.size <= 8, "{:?}", a.size);
		return a;
	}
	// (b) If one of the classes is NO_CLASS, the resulting class is the other class.
	else if a.kind == ClassKind::NoClass {
		return b;
	}
	// (c) If one of the classes is MEMORY, the result is the MEMORY class.
	else if a.kind == ClassKind::Memory || b.kind == ClassKind::Memory {
		return Class { kind: ClassKind::Memory, size: 8 };
	}
	// (d) If one of the classes is INTEGER, the result is the INTEGER.
	else if a.kind == ClassKind::Integer || b.kind == ClassKind::Integer {
		let size = a.size.max(b.size);
		assert!(size <= 8, "{size:?}");
		return Class { kind: ClassKind::Integer, size };
	}
	// (e) If one of the classes is X87, X87UP, COMPLEX_X87 class, MEMORY is used as class.
	else if a.kind == ClassKind::X87
		|| a.kind == ClassKind::X87Up
		|| a.kind == ClassKind::ComplexX87
		|| b.kind == ClassKind::X87
		|| b.kind == ClassKind::X87Up
		|| b.kind == ClassKind::ComplexX87
	{
		return Class { kind: ClassKind::Memory, size: 8 };
	}
	// (f) Otherwise class SSE is used.
	else {
		// A pointer is already 8 bytes so we should never see one flow through this if-else ladder
		assert_ne!(a.kind, ClassKind::Pointer);
		assert_ne!(b.kind, ClassKind::Pointer);
		let size = a.size.max(b.size);
		assert!(size <= 8, "{size:?}");
		return Class { kind: ClassKind::SSE, size };
	}
}

fn post_merge_cleanup<'buf>(aggregate_layout: Layout, buffer: &'buf mut [Class; 8]) -> &'buf mut [Class] {
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
