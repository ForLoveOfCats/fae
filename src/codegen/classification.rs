use crate::frontend::type_store::{TypeId, TypeStore};

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

pub trait Classifier {
	fn new() -> Self;

	fn classify_type<'a>(&'a mut self, type_store: &mut TypeStore, type_id: TypeId) -> &'a [Class];
}
