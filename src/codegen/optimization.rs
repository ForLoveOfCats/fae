use super::ir::{Instruction, IrModule, MemorySlot};

// TODO: Pass closure capable of reserving registers for calling convention
pub fn optimize(module: &mut IrModule, register_count: u8) {
	module.current_function = 0;
	forward_pass(module, register_count);
	module.optimized = true;
}

fn forward_pass(module: &mut IrModule, register_count: u8) {
	for instruction_index in 0..module.instructions.len() {
		let instruction = module.instructions[instruction_index];

		match instruction {
			Instruction::Function { function } => {
				module.current_function = function.index;
				mark_first_layer_unused_memory_slots(module);
				continue;
			}

			_ => {}
		}

		mark_sources_unused_if_destination_unused(module, instruction);
	}
}

fn mark_first_layer_unused_memory_slots(module: &mut IrModule) {
	let current_function = module.current_function_mut();
	for slot in &mut current_function.memory_slots {
		if slot.is_unused() {
			slot.location = None;
		}
	}
}

fn mark_sources_unused_if_destination_unused(module: &mut IrModule, instruction: Instruction) {
	let current_function = module.current_function_mut();
	let Some(destination) = instruction.destination() else {
		return;
	};

	if current_function.memory_slots[destination.index()].is_unused() {
		let mut slots_buffer = [MemorySlot::default(); 2];
		let source_slots = instruction.source_slots(&mut slots_buffer);

		for source_slot in source_slots {
			let slot = &mut current_function.memory_slots[source_slot.index()];
			slot.reads -= 1;

			if slot.is_unused() {
				slot.location = None;
			}
		}
	}
}
