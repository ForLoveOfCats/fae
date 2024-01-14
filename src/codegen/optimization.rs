use super::ir::{Instruction, IrModule};

pub fn optimize(module: &mut IrModule) {
	module.current_function = 0;

	for instruction_index in 0..module.instructions.len() {
		match module.instructions[instruction_index] {
			Instruction::Function { function } => {
				module.current_function = function.index;

				// TODO: Slots only read by instructions writing to unread slots will not get optimized out with this method
				// At least it'll catch unused structure field moves. Reverse and go from the end to catch everything perhaps?
				let current_function = module.current_function_mut();
				for slot in &mut current_function.memory_slots {
					if slot.reads <= 0 {
						slot.location = None;
					}
				}
			}

			Instruction::Label { label } => {}

			Instruction::Branch { conditional, else_label } => {}

			Instruction::Move8 { value, destination } => {}

			Instruction::Move16 { value, destination } => {}

			Instruction::Move32 { value, destination } => {}

			Instruction::Move64 { value, destination } => {}

			Instruction::Add { kind, left, right, destination } => {}

			Instruction::Subtract { kind, left, right, destination } => {}

			Instruction::Multiply { kind, left, right, destination } => {}

			Instruction::Divide { kind, left, right, destination } => {}
		}
	}

	module.optimized = true;
}
