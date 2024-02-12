use crate::codegen::ir::{IrModule, PrimativeSize};

#[derive(Debug, Clone, Copy, Default)]
pub struct MemorySlot {
	pub index: u16,
}

impl MemorySlot {
	pub fn index(self) -> usize {
		self.index as usize
	}

	pub fn primative_size(self, module: &IrModule) -> PrimativeSize {
		module.current_function().memory_slots[self.index()].primative_size.unwrap()
	}

	pub fn display(self, module: &IrModule) -> FmtDisplayMemorySlot {
		FmtDisplayMemorySlot {
			slot: self,
			borrowed_metadata: module.current_function().memory_slots.as_slice(),
		}
	}
}

pub struct FmtDisplayMemorySlot<'a> {
	pub slot: MemorySlot,
	pub borrowed_metadata: &'a [MemorySlotMetadata],
}

impl<'a> std::fmt::Display for FmtDisplayMemorySlot<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		let metadata = self.borrowed_metadata[self.slot.index()];
		write!(f, ":{}_{}", self.slot.index, metadata.primative_size.unwrap())
	}
}

#[derive(Debug, Clone, Copy)]
pub struct MemorySlotMetadata {
	pub primative_size: Option<PrimativeSize>,
	// Used by the optimizer to track if the previously inspected (in reverse function order) operation
	// to this memory slot was a write instead of a read
	pub next_forward_operation_is_write: bool,
}
