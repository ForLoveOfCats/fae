use llvm_sys::core::{LLVMGetCurrentDebugLocation2, LLVMSetCurrentDebugLocation2};
use llvm_sys::debuginfo::LLVMDIBuilderCreateDebugLocation;
use llvm_sys::prelude::{LLVMBuilderRef, LLVMContextRef, LLVMMetadataRef};

use crate::frontend::span::DebugLocation;

pub struct DebugScope {
	builder: LLVMBuilderRef,
	previous_location: LLVMMetadataRef,
}

impl DebugScope {
	pub fn new(
		context: LLVMContextRef,
		builder: LLVMBuilderRef,
		scope: LLVMMetadataRef,
		debug_location: DebugLocation,
	) -> DebugScope {
		let previous_location = unsafe { LLVMGetCurrentDebugLocation2(builder) };

		unsafe {
			// LLVM and DWARF calls this the "column" but testing shows that it actually
			// wants the byte offset into the line to avoid issues with tabulators. I'm very
			// saddened by the continued use of this heavily overloaded term :(
			let column = debug_location.offset_in_line;

			let line = debug_location.line;
			let inlined_at = std::ptr::null_mut();
			let location = LLVMDIBuilderCreateDebugLocation(context, line, column, scope, inlined_at);
			LLVMSetCurrentDebugLocation2(builder, location);
		}

		DebugScope { builder, previous_location }
	}
}

impl Drop for DebugScope {
	fn drop(&mut self) {
		unsafe { LLVMSetCurrentDebugLocation2(self.builder, self.previous_location) };
	}
}
