use crate::codegen::ir::{IrModule, PrimativeSize};

#[derive(Debug, Clone, Copy, Default)]
pub struct Intermediate {
	pub index: u16,
}

impl Intermediate {
	pub fn index(self) -> usize {
		self.index as usize
	}

	pub fn primative_size(self, module: &IrModule) -> PrimativeSize {
		module.current_function().intermediate_slots[self.index()]
			.primative_size
			.unwrap()
	}

	pub fn display(self, module: &IrModule) -> FmtDisplayIntermediate {
		FmtDisplayIntermediate {
			intermediate: self,
			borrowed_metadata: module.current_function().intermediate_slots.as_slice(),
		}
	}
}

pub struct FmtDisplayIntermediate<'a> {
	intermediate: Intermediate,
	borrowed_metadata: &'a [IntermediateMetadata],
}

impl<'a> std::fmt::Display for FmtDisplayIntermediate<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		let metadata = self.borrowed_metadata[self.intermediate.index()];
		write!(f, ":{}_{}", self.intermediate.index, metadata.primative_size.unwrap())
	}
}

#[derive(Debug, Clone, Copy)]
pub struct IntermediateMetadata {
	pub primative_size: Option<PrimativeSize>,
}
