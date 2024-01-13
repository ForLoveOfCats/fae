use super::ssa::{Instruction, SsaModule};

pub fn optimize(module: &mut SsaModule) {
	module.current_function = 0;

	for instruction in &mut module.instructions {
		match instruction {
			Instruction::Phi { sources, destination } => {}

			Instruction::Function { function } => {
				module.current_function = function.index;
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
