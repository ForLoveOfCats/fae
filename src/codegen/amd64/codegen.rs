use crate::codegen::amd64::assembler::{Assembler, Register64};
use crate::codegen::literal::Literal32;
use crate::error::Messages;
use crate::ir::FunctionId;
use crate::type_store::TypeStore;
use crate::validator::FunctionStore;

pub fn generate<'a>(
	messages: &mut Messages<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &mut FunctionStore<'a>,
	assembler: &mut Assembler,
) {
	let main = function_store.main.unwrap();
	let mut function_generate_queue = vec![main];
	while let Some(function_id) = function_generate_queue.pop() {
		generate_function(messages, type_store, function_store, assembler, function_id);
	}
}

pub fn generate_function<'a>(
	messages: &mut Messages<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &mut FunctionStore<'a>,
	assembler: &mut Assembler,
	function_id: FunctionId,
) {
	let shape = &mut function_store.shapes[function_id.function_shape_index];
	assert!(shape.extern_attribute.is_none(), "{:?}", shape.extern_attribute);
	let specialization = &mut shape.specializations[function_id.specialization_index];

	assert!(!specialization.been_generated);
	specialization.been_generated = true;

	let shape = &function_store.shapes[function_id.function_shape_index];
	let specialization = &shape.specializations[function_id.specialization_index];

	for type_argument in specialization.type_arguments.ids() {
		let entry = &type_store.type_entries[type_argument.index()];
		if entry.generic_poisoned {
			return;
		}
	}

	assembler.note("Function prelude");
	assembler.push_register64(Register64::Rbp);
	assembler.move_register64_to_register64(Register64::Rsp, Register64::Rbp);
	assembler.sub_literal32_to_register64(Literal32::from(32), Register64::Rsp); // TODO: Calculate stack usage
}
