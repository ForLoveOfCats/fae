use std::ops::Range;

use super::ir::{FunctionData, Instruction, InstructionKind, IrModule, MemorySlot};

// TODO: Pass closure capable of reserving registers for calling convention
pub fn optimize(module: &mut IrModule, register_count: u8) {
	module.current_function = 0;
	module_forward_pass(module, register_count);
	module.optimized = true;
}

fn module_forward_pass(module: &mut IrModule, register_count: u8) {
	let mut instruction_index = 0;

	while instruction_index < module.instructions.len() {
		let instruction = module.instructions[instruction_index];
		let initial_instruction_index = instruction_index;

		if let InstructionKind::Function { function } = instruction.kind {
			module.current_function = function.index;
			instruction_index = function_forward_pass(module, instruction_index + 1);
		} else {
			unreachable!();
		}

		let range = initial_instruction_index..instruction_index;
		function_reverse_pass(module, range);
	}
}

fn function_forward_pass(module: &mut IrModule, mut instruction_index: usize) -> usize {
	mark_first_layer_unused_memory_slots(module);

	while instruction_index < module.instructions.len() {
		let instruction = module.instructions[instruction_index];
		if let InstructionKind::Function { .. } = instruction.kind {
			break;
		}

		instruction_index += 1;
	}

	instruction_index
}

fn mark_first_layer_unused_memory_slots(module: &mut IrModule) {
	let current_function = module.current_function_mut();
	for slot in &mut current_function.memory_slots {
		if slot.is_unused() {
			slot.location = None;
		}
	}
}

fn function_reverse_pass(module: &mut IrModule, range: Range<usize>) {
	// inlined `current_function_mut` to satiate borrow checker
	let current_function = &mut module.functions[module.current_function as usize];

	for instruction_index in range.rev() {
		let instruction = &mut module.instructions[instruction_index];
		mark_sources_unused_if_destination_unused(current_function, instruction);
		prune_instruction_if_destination_unused(current_function, instruction);
	}
}

fn mark_sources_unused_if_destination_unused(current_function: &mut FunctionData, instruction: &Instruction) {
	let Some(destination) = instruction.destination() else {
		return;
	};

	if current_function.memory_slots[destination.index()].is_unused() {
		let mut slots_buffer = [MemorySlot::default(); 2];
		let source_slots = instruction.source_slots(&mut slots_buffer);

		for source_slot in source_slots {
			let slot_metadata = &mut current_function.memory_slots[source_slot.index()];
			slot_metadata.reads = slot_metadata.reads.saturating_sub(1);

			if slot_metadata.is_unused() {
				slot_metadata.location = None;
			}
		}
	}
}

fn prune_instruction_if_destination_unused(current_function: &FunctionData, instruction: &mut Instruction) {
	if let Some(destination) = instruction.destination() {
		let slot_metadata = current_function.memory_slots[destination.index()];
		instruction.removed = slot_metadata.is_unused();
	}
}
