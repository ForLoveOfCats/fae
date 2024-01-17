use crate::codegen::intermediate::*;
use crate::type_store::NumericKind;

#[derive(Debug, Clone, Copy)]
pub struct Instruction {
	pub kind: InstructionKind,
	pub removed: bool,
}

#[derive(Debug, Clone, Copy)]
pub enum InstructionKind {
	Function {
		function: Function,
	},

	Branch {
		id: FlowControlId,
		conditional: Source,
	},

	While {
		id: FlowControlId,
		conditional: Source,
	},

	End {
		id: FlowControlId,
	},

	Move8 {
		value: Intermediate8,
		destination: MemorySlot,
	},

	Move16 {
		value: Intermediate16,
		destination: MemorySlot,
	},

	Move32 {
		value: Intermediate32,
		destination: MemorySlot,
	},

	Move64 {
		value: Intermediate64,
		destination: MemorySlot,
	},

	Add {
		kind: NumericKind,
		left: Source,
		right: Source,
		destination: MemorySlot,
	},

	Subtract {
		kind: NumericKind,
		left: Source,
		right: Source,
		destination: MemorySlot,
	},

	Multiply {
		kind: NumericKind,
		left: Source,
		right: Source,
		destination: MemorySlot,
	},

	Divide {
		kind: NumericKind,
		left: Source,
		right: Source,
		destination: MemorySlot,
	},

	ForceUsed {
		slot: MemorySlot,
	},
}

impl Instruction {
	pub fn source_slots(self, slots: &mut [MemorySlot; 2]) -> &[MemorySlot] {
		let mut index = 0;
		let mut push_source = |source: Source| {
			if let Source::MemorySlot(slot) = source {
				slots[index] = slot;
				index += 1;
			}
		};

		match self.kind {
			InstructionKind::Function { .. }
			| InstructionKind::End { .. }
			| InstructionKind::Move8 { .. }
			| InstructionKind::Move16 { .. }
			| InstructionKind::Move32 { .. }
			| InstructionKind::Move64 { .. } => return &[],

			InstructionKind::Branch { conditional, .. } => {
				push_source(conditional);
			}

			InstructionKind::While { conditional, .. } => {
				push_source(conditional);
			}

			InstructionKind::Add { left, right, .. } => {
				push_source(left);
				push_source(right);
			}

			InstructionKind::Subtract { left, right, .. } => {
				push_source(left);
				push_source(right);
			}

			InstructionKind::Multiply { left, right, .. } => {
				push_source(left);
				push_source(right);
			}

			InstructionKind::Divide { left, right, .. } => {
				push_source(left);
				push_source(right);
			}

			InstructionKind::ForceUsed { slot } => {
				slots[index] = slot;
				index += 1;
			}
		}

		let slots = &slots[0..index];
		slots
	}

	pub fn destination(self) -> Option<MemorySlot> {
		match self.kind {
			InstructionKind::Function { .. }
			| InstructionKind::Branch { .. }
			| InstructionKind::While { .. }
			| InstructionKind::End { .. } => None,

			InstructionKind::Move8 { destination, .. } => Some(destination),
			InstructionKind::Move16 { destination, .. } => Some(destination),
			InstructionKind::Move32 { destination, .. } => Some(destination),
			InstructionKind::Move64 { destination, .. } => Some(destination),
			InstructionKind::Add { destination, .. } => Some(destination),
			InstructionKind::Subtract { destination, .. } => Some(destination),
			InstructionKind::Multiply { destination, .. } => Some(destination),
			InstructionKind::Divide { destination, .. } => Some(destination),
			InstructionKind::ForceUsed { slot } => Some(slot),
		}
	}
}

#[derive(Debug, Clone, Copy)]
pub enum Source {
	Intermediate8(Intermediate8),
	Intermediate16(Intermediate16),
	Intermediate32(Intermediate32),
	Intermediate64(Intermediate64),
	MemorySlot(MemorySlot),
}

impl Source {
	pub fn display(self, module: &IrModule) -> FmtDisplaySource {
		FmtDisplaySource {
			source: self,
			borrowed_metadata: module.current_function().memory_slots.as_slice(),
		}
	}
}

impl From<MemorySlot> for Source {
	fn from(value: MemorySlot) -> Source {
		Source::MemorySlot(value)
	}
}

impl From<Intermediate8> for Source {
	fn from(value: Intermediate8) -> Source {
		Source::Intermediate8(value)
	}
}

impl From<Intermediate16> for Source {
	fn from(value: Intermediate16) -> Source {
		Source::Intermediate16(value)
	}
}

impl From<Intermediate32> for Source {
	fn from(value: Intermediate32) -> Source {
		Source::Intermediate32(value)
	}
}

impl From<Intermediate64> for Source {
	fn from(value: Intermediate64) -> Source {
		Source::Intermediate64(value)
	}
}

pub struct FmtDisplaySource<'a> {
	source: Source,
	borrowed_metadata: &'a [SlotMetadata],
}

// This is a damn mess, belch
impl<'a> std::fmt::Display for FmtDisplaySource<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self.source {
			Source::Intermediate8(intermediate) => write!(f, "{intermediate}"),
			Source::Intermediate16(intermediate) => write!(f, "{intermediate}"),
			Source::Intermediate32(intermediate) => write!(f, "{intermediate}"),
			Source::Intermediate64(intermediate) => write!(f, "{intermediate}"),
			Source::MemorySlot(slot) => FmtDisplayMemorySlot { slot, borrowed_metadata: self.borrowed_metadata }.fmt(f),
		}
	}
}

#[derive(Debug, Clone, Copy, Default)]
pub struct MemorySlot {
	pub index: u16,
}

impl MemorySlot {
	pub fn new(index: u16) -> MemorySlot {
		MemorySlot { index }
	}

	pub fn index(self) -> usize {
		self.index as usize
	}

	pub fn display(self, module: &IrModule) -> FmtDisplayMemorySlot {
		FmtDisplayMemorySlot {
			slot: self,
			borrowed_metadata: module.current_function().memory_slots.as_slice(),
		}
	}
}

pub struct FmtDisplayMemorySlot<'a> {
	slot: MemorySlot,
	borrowed_metadata: &'a [SlotMetadata],
}

impl<'a> std::fmt::Display for FmtDisplayMemorySlot<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		let metadata = self.borrowed_metadata[self.slot.index()];
		match metadata.location {
			Some(Location::Stack) => write!(f, "${}", self.slot.index),
			Some(Location::Register(reg)) => write!(f, "%{reg}"),
			None => write!(f, "_"),
		}
	}
}

#[derive(Debug, Clone, Copy)]
pub struct SlotMetadata {
	pub reads: u32,
	pub force_used: bool,
	pub location: Option<Location>,
}

impl SlotMetadata {
	pub fn is_unused(self) -> bool {
		self.reads <= 0 && !self.force_used
	}
}

#[derive(Debug, Clone, Copy)]
pub struct Label {
	index: u32,
}

impl std::fmt::Display for Label {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, ".{}", self.index)
	}
}

#[derive(Debug, Clone, Copy)]
pub struct FlowControlId {
	index: u32,
}

impl std::fmt::Display for FlowControlId {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "^{}", self.index)
	}
}

#[derive(Debug, Clone, Copy)]
pub enum Location {
	Stack,
	Register(u8),
}

#[derive(Debug, Clone, Copy)]
pub struct Function {
	pub index: u32,
}

impl std::fmt::Display for Function {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "#{}", self.index)
	}
}

#[derive(Debug)]
pub struct FunctionData {
	pub next_label: u32,
	pub next_control_flow_id: u32,
	pub memory_slots: Vec<SlotMetadata>,
}

#[derive(Debug)]
pub struct IrModule {
	pub instructions: Vec<Instruction>,
	pub next_function: u32,
	pub current_function: u32,
	pub functions: Vec<FunctionData>,
	pub optimized: bool,
}

impl IrModule {
	pub fn new() -> IrModule {
		IrModule {
			instructions: Vec::new(),
			next_function: 0,
			current_function: 0,
			functions: Vec::new(),
			optimized: false,
		}
	}

	pub fn instruction_count(&self) -> usize {
		self.instructions.len()
	}

	pub fn next_function(&mut self) -> Function {
		self.current_function = self.next_function;
		let data = FunctionData {
			next_label: 0,
			next_control_flow_id: 0,
			memory_slots: Vec::new(),
		};
		self.functions.push(data);

		let function = Function { index: self.next_function };

		self.next_function += 1;
		function
	}

	pub fn current_function(&self) -> &FunctionData {
		&self.functions[self.current_function as usize]
	}

	pub fn current_function_mut(&mut self) -> &mut FunctionData {
		&mut self.functions[self.current_function as usize]
	}

	// pub fn next_label(&mut self) -> Label {
	// 	let current_function = self.current_function_mut();
	// 	let label = Label { index: current_function.next_label };
	// 	current_function.next_label += 1;
	// 	label
	// }

	pub fn next_control_flow_id(&mut self) -> FlowControlId {
		let current_function = self.current_function_mut();
		let label = FlowControlId { index: current_function.next_control_flow_id };
		current_function.next_control_flow_id += 1;
		label
	}

	pub fn next_memory_slot(&mut self) -> MemorySlot {
		let current_function = self.current_function_mut();
		let index = current_function.memory_slots.len() as u16;
		let metadata = SlotMetadata { reads: 0, force_used: false, location: Some(Location::Stack) };
		current_function.memory_slots.push(metadata);
		MemorySlot { index }
	}

	pub fn start_function(&mut self) -> Function {
		let function = self.next_function();
		self.push(InstructionKind::Function { function });
		function
	}

	pub fn push(&mut self, kind: InstructionKind) {
		let instruction = Instruction { kind, removed: false };
		let current_function = self.current_function_mut();

		let mut source_slots_buffer = [MemorySlot::default(); 2];
		let source_slots = instruction.source_slots(&mut source_slots_buffer);
		let destination = instruction.destination();

		for source_slot in source_slots {
			if let Some(destination) = destination {
				if source_slot.index == destination.index {
					continue;
				}
			}

			current_function.memory_slots[source_slot.index()].reads += 1;
		}

		if let InstructionKind::ForceUsed { slot } = instruction.kind {
			current_function.memory_slots[slot.index()].force_used = true;
		}

		self.instructions.push(instruction);
	}

	// conditional value must be a single byte, uses C style numeric truthiness
	// returns the id, insert as End at the end of the loop block
	pub fn push_branch(&mut self, conditional: impl Into<Source>) -> FlowControlId {
		let conditional = conditional.into();
		let id = self.next_control_flow_id();
		self.push(InstructionKind::Branch { id, conditional });
		id
	}

	// conditional value must be a single byte, uses C style numeric truthiness
	// returns the id, insert as End at the end of the loop block
	pub fn push_while_loop(&mut self, conditional: impl Into<Source>) -> FlowControlId {
		let conditional = conditional.into();
		let id = self.next_control_flow_id();
		self.push(InstructionKind::While { id, conditional });
		id
	}

	pub fn debug_dump(&mut self) {
		self.current_function = 0;

		if self.optimized {
			println!("Optimized IR Module:");
		} else {
			println!("Unoptimized IR Module:");
		}

		let mut current_line_number = 1;
		let line_number = |current_line_number: &mut u32| {
			print!("{current_line_number:3}| ");
			*current_line_number += 1;
		};

		let mut indent_level = 1;
		let indent = |indent_level: u32, current_line_number: &mut u32, instruction: Instruction| {
			line_number(current_line_number);

			if instruction.removed {
				print!("✕");
			}

			for _ in 0..indent_level {
				print!("  ");
			}
		};

		for (index, &instruction) in self.instructions.iter().enumerate() {
			match instruction.kind {
				InstructionKind::Function { function } => {
					self.current_function = function.index;
					if index != 0 {
						println!();
					}
					line_number(&mut current_line_number);
					println!("Function {function}");
					indent_level = 1
				}

				InstructionKind::Branch { id, conditional } => {
					indent(indent_level, &mut current_line_number, instruction);
					println!("Branch {} else → {id}", conditional.display(self));
					indent_level += 1;
				}

				InstructionKind::While { conditional, id } => {
					indent(indent_level, &mut current_line_number, instruction);
					println!("While {}: {id}", conditional.display(self));
					indent_level += 1;
				}

				InstructionKind::End { id } => {
					indent_level -= 1;
					indent(indent_level, &mut current_line_number, instruction);
					println!("End {id}");
				}

				InstructionKind::Move8 { value, destination } => {
					indent(indent_level, &mut current_line_number, instruction);
					println!("{} = Move8 {value}", destination.display(self));
				}

				InstructionKind::Move16 { value, destination } => {
					indent(indent_level, &mut current_line_number, instruction);
					println!("{} = Move16 {value}", destination.display(self));
				}

				InstructionKind::Move32 { value, destination } => {
					indent(indent_level, &mut current_line_number, instruction);
					println!("{} = Move32 {value}", destination.display(self));
				}

				InstructionKind::Move64 { value, destination } => {
					indent(indent_level, &mut current_line_number, instruction);
					println!("{} = Move64 {value}", destination.display(self));
				}

				InstructionKind::Add { kind, left, right, destination } => {
					indent(indent_level, &mut current_line_number, instruction);
					println!(
						"{} = Add<{kind}> {}, {}",
						destination.display(self),
						left.display(self),
						right.display(self),
					);
				}

				InstructionKind::Subtract { kind, left, right, destination } => {
					indent(indent_level, &mut current_line_number, instruction);
					println!(
						"{} = Subtract<{kind}> {}, {}",
						destination.display(self),
						left.display(self),
						right.display(self),
					);
				}

				InstructionKind::Multiply { kind, left, right, destination } => {
					indent(indent_level, &mut current_line_number, instruction);
					println!(
						"{} = Multiply<{kind}> {}, {}",
						destination.display(self),
						left.display(self),
						right.display(self),
					);
				}

				InstructionKind::Divide { kind, left, right, destination } => {
					indent(indent_level, &mut current_line_number, instruction);
					println!(
						"{} = Divide<{kind}> {}, {}",
						destination.display(self),
						left.display(self),
						right.display(self),
					);
				}

				InstructionKind::ForceUsed { slot } => {
					indent(indent_level, &mut current_line_number, instruction);
					println!("ForceUsed {}", slot.display(self));
				}
			}
		}
	}
}
