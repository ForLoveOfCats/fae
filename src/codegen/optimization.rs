use std::ops::Range;

use crate::codegen::ir::{Destination, FlowControlId, FunctionData, Instruction, InstructionKind, IrModule, Source};
use crate::codegen::memory_slot::MemorySlot;

pub fn optimize(module: &mut IrModule) -> Tracker {
	let mut tracker = Tracker { overwritten_slot_writes_removals: 0 };

	module.current_function = 0;
	module_forward_pass(module, &mut tracker);
	module.optimized = true;

	tracker
}

#[derive(Debug)]
pub struct Tracker {
	overwritten_slot_writes_removals: u64,
}

fn module_forward_pass(module: &mut IrModule, tracker: &mut Tracker) {
	let mut instruction_index = 0;

	while instruction_index < module.instructions.len() {
		let instruction = module.instructions[instruction_index];
		let initial_instruction_index = instruction_index + 1;

		if let InstructionKind::Function { function } = instruction.kind {
			module.current_function = function.index;
			instruction_index = function_forward_pass(module, instruction_index + 1);
		} else {
			unreachable!();
		}

		let range = initial_instruction_index..instruction_index;
		function_reverse_pass(module, range, tracker);
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

fn function_reverse_pass(module: &mut IrModule, range: Range<usize>, tracker: &mut Tracker) {
	// inlined `current_function_mut` to satiate borrow checker
	let current_function = &mut module.functions[module.current_function as usize];

	let mut flow_control_stack = Vec::new();

	for instruction_index in range.rev() {
		let instruction = &mut module.instructions[instruction_index];
		mark_removed_if_overwritten_slot_write(current_function, instruction, tracker);
		maintain_flow_control_stack(&mut module.instructions, instruction_index, &mut flow_control_stack);
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

// Marks the instruction as removed if the destinatation is guarenteed to be overwritten
// with something else before the next read to that memory slot. Maintains the state it
// need to track to recognize this
fn mark_removed_if_overwritten_slot_write(
	current_function: &mut FunctionData,
	instruction: &mut Instruction,
	tracker: &mut Tracker,
) {
	if instruction.forces_proceeding_writes() {
		for slot in &mut current_function.memory_slots {
			slot.next_forward_operation_is_write = false;
		}
	}

	let Some(Destination::MemorySlot(destination)) = instruction.destination() else {
		return;
	};

	let metadata = &mut current_function.memory_slots[destination.index()];
	if metadata.next_forward_operation_is_write {
		instruction.removed = true;
		tracker.overwritten_slot_writes_removals += 1;
	}
	metadata.next_forward_operation_is_write = true;

	let mut source_slots_buffer = [MemorySlot::default(); 2];
	let source_slots = instruction.source_slots(&mut source_slots_buffer);
	for source_slot in source_slots {
		current_function.memory_slots[source_slot.index()].next_forward_operation_is_write = false;
	}
}
