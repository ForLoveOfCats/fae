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
			let line = debug_location.line;
			let column = debug_location.column;
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
