use std::ops::Range;

use crate::codegen::ir::{FlowControlId, Source};

use super::ir::{FunctionData, Instruction, InstructionKind, IrModule, MemorySlot};

pub fn optimize(module: &mut IrModule) {
	module.current_function = 0;
	module_forward_pass(module);
	module.optimized = true;
}

fn module_forward_pass(module: &mut IrModule) {
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
	while instruction_index < module.instructions.len() {
		let instruction = module.instructions[instruction_index];
		if let InstructionKind::Function { .. } = instruction.kind {
			break;
		}

		instruction_index += 1;
	}

	instruction_index
}

struct FlowControlStackEntry {
	id: FlowControlId,
	index: usize,
	live_instructions: usize,
}

fn function_reverse_pass(module: &mut IrModule, range: Range<usize>) {
	// inlined `current_function_mut` to satiate borrow checker
	let current_function = &mut module.functions[module.current_function as usize];

	let mut flow_control_stack = Vec::new();

	for instruction_index in range.rev() {
		let instruction = &mut module.instructions[instruction_index];
		mark_sources_unused_if_destination_unused(current_function, instruction);
		mark_instruction_removed_unneeded(current_function, instruction, &flow_control_stack);
		maintain_flow_control_stack(&mut module.instructions, instruction_index, &mut flow_control_stack);
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
			slot_metadata.decrement_reads();
		}
	}
}

fn mark_instruction_removed_unneeded(
	current_function: &mut FunctionData,
	instruction: &mut Instruction,
	flow_control_stack: &[FlowControlStackEntry],
) {
	if let Some(destination) = instruction.destination() {
		let destination_metadata = current_function.memory_slots[destination.index()];
		instruction.removed = destination_metadata.is_unused();
		return;
	}

	let (id, conditional) = match instruction.kind {
		InstructionKind::Branch { id, conditional } | InstructionKind::While { id, conditional } => (id, conditional),
		_ => return,
	};

	let stack_entry = flow_control_stack.last().unwrap();
	assert_eq!(stack_entry.id, id);

	if stack_entry.live_instructions <= 0 {
		instruction.removed = true;

		if let Source::MemorySlot(slot) = conditional {
			let slot_metadata = &mut current_function.memory_slots[slot.index()];
			slot_metadata.decrement_reads()
		}
	}
}

fn maintain_flow_control_stack(
	instructions: &mut [Instruction],
	instruction_index: usize,
	flow_control_stack: &mut Vec<FlowControlStackEntry>,
) {
	let instruction = instructions[instruction_index];
	match instruction.kind {
		InstructionKind::End { id } => {
			let entry = FlowControlStackEntry { id, index: instruction_index, live_instructions: 0 };
			flow_control_stack.push(entry);
			return;
		}

		InstructionKind::Branch { id, .. } | InstructionKind::While { id, .. } => {
			let stack_entry = flow_control_stack.pop().unwrap();
			assert_eq!(stack_entry.id, id);

			if instruction.removed {
				let end = &mut instructions[stack_entry.index];
				match end.kind {
					InstructionKind::End { .. } => end.removed = true,
					_ => unreachable!(),
				}
			}

			return;
		}

		_ => {}
	}

	if let Some(stack_entry) = flow_control_stack.last_mut() {
		if !instruction.removed {
			stack_entry.live_instructions += 1;
		}
	}
}
