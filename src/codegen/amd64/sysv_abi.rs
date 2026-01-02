use crate::frontend::type_store::{Array, Layout, NumericKind, PrimitiveKind, TypeEntryKind, TypeId, TypeStore, UserTypeKind};

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

// Note: It is counterintuitive that this array has space for *nine* classes when
// there should never be more than eight. However the algorithm as implemented
// below can use up to one extra as a temporary spot to put a class which is about
// to be combined, bringing the total classes back down to eight.
#[inline]
pub fn classification_buffer() -> [Class; 9] {
	Default::default()
}

pub fn classify_type<'buf>(
	type_store: &mut TypeStore,
	buffer: &'buf mut [Class; 9],
	buffer_index: usize,
	type_id: TypeId,
) -> usize {
	let result = classify_type_internal(type_store, buffer, buffer_index, type_id);
	assert!(result.new_classes_len <= 8);
	result.new_classes_len
}

#[derive(Debug)]
pub struct ClassifyResult {
	pub new_classes_len: usize,
	pub itself_may_be_combined: bool,
}

// Huge thanks to the Zig selfhost compiler for making the spec algorithm make sense
fn classify_type_internal<'buf>(
	type_store: &mut TypeStore,
	buffer: &'buf mut [Class; 9],
	buffer_index: usize,
	type_id: TypeId,
) -> ClassifyResult {
	let old_length = buffer_index;
	let entry = type_store.type_entries.get(type_id);

	match entry.kind {
		TypeEntryKind::BuiltinType { kind, .. } => match kind {
			PrimitiveKind::Numeric(kind) => {
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

				buffer[buffer_index] = class;
				return ClassifyResult { new_classes_len: 1, itself_may_be_combined: true };
			}

			PrimitiveKind::Bool => {
				buffer[buffer_index] = Class { kind: ClassKind::Boolean, size: 1 };
				return ClassifyResult { new_classes_len: 1, itself_may_be_combined: true };
			}

			PrimitiveKind::String | PrimitiveKind::StringMut | PrimitiveKind::FormatString => {
				buffer[buffer_index] = Class { kind: ClassKind::Pointer, size: 8 };
				buffer[buffer_index + 1] = Class { kind: ClassKind::Integer, size: 8 };
				return ClassifyResult { new_classes_len: 2, itself_may_be_combined: false };
			}

			PrimitiveKind::AnyCollapse | PrimitiveKind::NoReturn | PrimitiveKind::Void | PrimitiveKind::UntypedNumber => {
				unreachable!("{:?}", entry.kind);
			}
		},

		TypeEntryKind::UserType { shape_index, specialization_index, .. } => {
			let aggregate_layout = type_store.type_layout(type_id);

			// 1. If the size of an object is larger than eight eightbytes, or it contains unaligned fields, it has class MEMORY.
			// Fae does not currently support unaligned/packed struct fields, so we don't need to worry about that
			if aggregate_layout.size > 8 * 8 {
				buffer[buffer_index] = Class { kind: ClassKind::Memory, size: 8 };
				return ClassifyResult { new_classes_len: 1, itself_may_be_combined: false };
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
					classify_merge_fields(type_store, buffer, buffer_index, specialization.fields.iter().map(|f| f.type_id));

					// 5. Then a post merger cleanup is done:
					let new_classes_len = post_merge_cleanup(aggregate_layout, buffer, old_length);
					return ClassifyResult { new_classes_len, itself_may_be_combined: false };
				}

				UserTypeKind::Enum { .. } => {
					if aggregate_layout.size > 8 {
						buffer[buffer_index] = Class { kind: ClassKind::Memory, size: 8 };
						return ClassifyResult { new_classes_len: 1, itself_may_be_combined: false };
					} else {
						let size = if 0 <= aggregate_layout.size && aggregate_layout.size <= 8 {
							aggregate_layout.size as u8
						} else {
							unreachable!("{:?}", aggregate_layout);
						};
						buffer[buffer_index] = Class { kind: ClassKind::Integer, size };
						return ClassifyResult { new_classes_len: 1, itself_may_be_combined: true };
					}
				}

				UserTypeKind::Union { shape, .. } => {
					let specialization = &shape.specializations[specialization_index];

					let mut output_len = 0;
					for field in specialization.fields.iter() {
						let mut field_buffer = classification_buffer();
						let result = classify_type_internal(type_store, &mut field_buffer, 0, field.type_id);
						output_len = output_len.max(result.new_classes_len);

						for (index, field_class) in field_buffer[..result.new_classes_len].iter().enumerate() {
							buffer[buffer_index + index] = merge_classes_union(buffer[buffer_index + index], *field_class);
						}
					}

					let new_classes_len = post_merge_cleanup(aggregate_layout, buffer, old_length);
					return ClassifyResult { new_classes_len, itself_may_be_combined: false };
				}
			}
		}

		TypeEntryKind::Pointer { .. } => {
			buffer[buffer_index] = Class { kind: ClassKind::Pointer, size: 8 };
			return ClassifyResult { new_classes_len: 1, itself_may_be_combined: false };
		}

		TypeEntryKind::Array(Array { item_type_id, length, .. }) => {
			let array_layout = type_store.type_layout(type_id);
			if array_layout.size > 8 * 8 {
				buffer[buffer_index] = Class { kind: ClassKind::Memory, size: 8 };
				return ClassifyResult { new_classes_len: 1, itself_may_be_combined: false };
			}

			classify_merge_fields(type_store, buffer, buffer_index, (0..length).map(|_| item_type_id));
			let new_classes_len = post_merge_cleanup(array_layout, buffer, old_length);
			return ClassifyResult { new_classes_len, itself_may_be_combined: false };
		}

		TypeEntryKind::Slice(_) => {
			buffer[buffer_index] = Class { kind: ClassKind::Pointer, size: 8 };
			buffer[buffer_index + 1] = Class { kind: ClassKind::Integer, size: 8 };
			return ClassifyResult { new_classes_len: 2, itself_may_be_combined: false };
		}

		TypeEntryKind::Module
		| TypeEntryKind::Type
		| TypeEntryKind::UserTypeGeneric { .. }
		| TypeEntryKind::FunctionGeneric { .. }
		| TypeEntryKind::TraitGeneric { .. } => unreachable!(),
	}
}

fn classify_merge_fields<I: Iterator<Item = TypeId>>(
	type_store: &mut TypeStore,
	buffer: &mut [Class; 9],
	mut buffer_index: usize,
	field_type_ids: I,
) {
	let mut combine_size = 0;
	if buffer_index > 0 {
		let final_class = buffer[buffer_index - 1];
		combine_size = final_class.size as i64;
	}

	for field_type_id in field_type_ids {
		let field_layout = type_store.type_layout(field_type_id);
		if field_layout.size <= 0 {
			continue;
		}

		let result = classify_type_internal(type_store, buffer, buffer_index, field_type_id);

		// This is counter-intuitive but the nested type can entirely merge itself into
		// an already existing item in the buffer, leaving the index class untouched
		if buffer[buffer_index].kind == ClassKind::NoClass {
			continue;
		}

		if buffer[buffer_index].kind == ClassKind::Boolean {
			buffer[buffer_index].kind = ClassKind::Integer;
		}

		if result.itself_may_be_combined && combine_size > 0 && field_layout.size + combine_size <= 8 {
			// Combine with prior fields to make an eightbyte

			assert_eq!(result.new_classes_len, 1);
			buffer[buffer_index - 1] = merge_classes_struct(buffer[buffer_index - 1], buffer[buffer_index]);
			buffer[buffer_index] = Class::default();
			let result_class = buffer[buffer_index - 1];

			combine_size += field_layout.size;
			assert!(combine_size <= 8, "{combine_size}");
			assert!(result_class.size <= 8, "{}", result_class.size);
			if combine_size == 8 {
				combine_size = 0;
			}
		} else {
			// To large to combine

			buffer_index += result.new_classes_len;
			let final_class = buffer[buffer_index - 1];

			if final_class.size >= 8 {
				combine_size = 0;
			} else {
				combine_size = final_class.size as i64;
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

fn post_merge_cleanup<'buf>(aggregate_layout: Layout, buffer: &'buf mut [Class; 9], old_length: usize) -> usize {
	let mut contains_sse_up = false;
	for (index, &class) in buffer.iter().enumerate() {
		// (a) If one of the classes is MEMORY, the whole argument is passed in memory.
		if class.kind == ClassKind::Memory {
			buffer[0] = Class { kind: ClassKind::Memory, size: 8 };
			return 1;
		}

		// (b) If X87UP is not preceded by X87, the whole argument is passed in memory.
		if class.kind == ClassKind::X87Up && index > 0 && buffer[index - 1].kind != ClassKind::X87 {
			buffer[0] = Class { kind: ClassKind::Memory, size: 8 };
			return 1;
		}

		contains_sse_up |= class.kind == ClassKind::SSEUp;
	}

	// (c) If the size of the aggregate exceeds two eightbytes and the first eightbyte isn’t SSE or any
	// other eightbyte isn’t SSEUP, the whole argument is passed in memory.
	let first_eightbyte_sse = buffer[0].kind == ClassKind::SSE;
	if aggregate_layout.size > 16 && (!first_eightbyte_sse || !contains_sse_up) {
		buffer[0] = Class { kind: ClassKind::Memory, size: 8 };
		return 1;
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

	contents_len - old_length
}
