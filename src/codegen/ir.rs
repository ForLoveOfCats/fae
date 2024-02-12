use crate::codegen::literal::*;
use crate::type_store::NumericKind;

use crate::codegen::intermediate::IntermediateMetadata;
use crate::codegen::memory_slot::{FmtDisplayMemorySlot, MemorySlot, MemorySlotMetadata};

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
		value: Literal8,
		destination: Destination,
	},

	Move16 {
		value: Literal16,
		destination: Destination,
	},

	Move32 {
		value: Literal32,
		destination: Destination,
	},

	Move64 {
		value: Literal64,
		destination: Destination,
	},

	Add {
		kind: NumericKind,
		left: Source,
		right: Source,
		destination: Destination,
	},

	Subtract {
		kind: NumericKind,
		left: Source,
		right: Source,
		destination: Destination,
	},

	Multiply {
		kind: NumericKind,
		left: Source,
		right: Source,
		destination: Destination,
	},

	Divide {
		kind: NumericKind,
		left: Source,
		right: Source,
		destination: Destination,
	},

	Call {
		function: Function,
	},
}

impl Instruction {
	pub fn check_sizes_match(self, module: &IrModule) {
		match self.kind {
			InstructionKind::Function { .. } | InstructionKind::End { .. } | InstructionKind::Call { .. } => {}

			InstructionKind::Branch { conditional, .. } => assert_eq!(conditional.primative_size(module), PrimativeSize::Ps8),
			InstructionKind::While { conditional, .. } => assert_eq!(conditional.primative_size(module), PrimativeSize::Ps8),

			InstructionKind::Move8 { destination, .. } => assert_eq!(destination.primative_size(module), PrimativeSize::Ps8),
			InstructionKind::Move16 { destination, .. } => assert_eq!(destination.primative_size(module), PrimativeSize::Ps16),
			InstructionKind::Move32 { destination, .. } => assert_eq!(destination.primative_size(module), PrimativeSize::Ps32),
			InstructionKind::Move64 { destination, .. } => assert_eq!(destination.primative_size(module), PrimativeSize::Ps64),

			InstructionKind::Add { kind, left, right, destination } => {
				let primative_size = kind.ir_primative_size();
				assert_eq!(primative_size, left.primative_size(module));
				assert_eq!(primative_size, right.primative_size(module));
				assert_eq!(primative_size, destination.primative_size(module));
			}

			InstructionKind::Subtract { kind, left, right, destination } => {
				let primative_size = kind.ir_primative_size();
				assert_eq!(primative_size, left.primative_size(module));
				assert_eq!(primative_size, right.primative_size(module));
				assert_eq!(primative_size, destination.primative_size(module));
			}

			InstructionKind::Multiply { kind, left, right, destination } => {
				let primative_size = kind.ir_primative_size();
				assert_eq!(primative_size, left.primative_size(module));
				assert_eq!(primative_size, right.primative_size(module));
				assert_eq!(primative_size, destination.primative_size(module));
			}

			InstructionKind::Divide { kind, left, right, destination } => {
				let primative_size = kind.ir_primative_size();
				assert_eq!(primative_size, left.primative_size(module));
				assert_eq!(primative_size, right.primative_size(module));
				assert_eq!(primative_size, destination.primative_size(module));
			}
		}
	}

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
			| InstructionKind::Move64 { .. }
			| InstructionKind::Call { .. } => return &[],

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
		}

		let slots = &slots[0..index];
		slots
	}

	pub fn destination(self) -> Option<Destination> {
		match self.kind {
			InstructionKind::Function { .. }
			| InstructionKind::Branch { .. }
			| InstructionKind::While { .. }
			| InstructionKind::End { .. }
			| InstructionKind::Call { .. } => None,

			InstructionKind::Move8 { destination, .. } => Some(destination),
			InstructionKind::Move16 { destination, .. } => Some(destination),
			InstructionKind::Move32 { destination, .. } => Some(destination),
			InstructionKind::Move64 { destination, .. } => Some(destination),
			InstructionKind::Add { destination, .. } => Some(destination),
			InstructionKind::Subtract { destination, .. } => Some(destination),
			InstructionKind::Multiply { destination, .. } => Some(destination),
			InstructionKind::Divide { destination, .. } => Some(destination),
		}
	}

	pub fn forces_proceeding_writes(self) -> bool {
		match self.kind {
			InstructionKind::Function { .. } => unreachable!(),

			InstructionKind::Branch { .. }
			| InstructionKind::While { .. }
			| InstructionKind::End { .. }
			| InstructionKind::Call { .. } => true,

			InstructionKind::Move8 { .. }
			| InstructionKind::Move16 { .. }
			| InstructionKind::Move32 { .. }
			| InstructionKind::Move64 { .. }
			| InstructionKind::Add { .. }
			| InstructionKind::Subtract { .. }
			| InstructionKind::Multiply { .. }
			| InstructionKind::Divide { .. } => false,
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimativeSize {
	Ps8,
	Ps16,
	Ps32,
	Ps64,
}

impl std::fmt::Display for PrimativeSize {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let value = match self {
			PrimativeSize::Ps8 => 8,
			PrimativeSize::Ps16 => 16,
			PrimativeSize::Ps32 => 32,
			PrimativeSize::Ps64 => 64,
		};

		write!(f, "{value}")
	}
}

#[derive(Debug, Clone, Copy)]
pub enum Source {
	Literal8(Literal8),
	Literal16(Literal16),
	Literal32(Literal32),
	Literal64(Literal64),
	MemorySlot(MemorySlot),
}

impl Source {
	pub fn primative_size(self, module: &IrModule) -> PrimativeSize {
		match self {
			Source::Literal8(_) => PrimativeSize::Ps8,
			Source::Literal16(_) => PrimativeSize::Ps16,
			Source::Literal32(_) => PrimativeSize::Ps32,
			Source::Literal64(_) => PrimativeSize::Ps64,
			Source::MemorySlot(slot) => slot.primative_size(module),
		}
	}

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

impl From<Literal8> for Source {
	fn from(value: Literal8) -> Source {
		Source::Literal8(value)
	}
}

impl From<Literal16> for Source {
	fn from(value: Literal16) -> Source {
		Source::Literal16(value)
	}
}

impl From<Literal32> for Source {
	fn from(value: Literal32) -> Source {
		Source::Literal32(value)
	}
}

impl From<Literal64> for Source {
	fn from(value: Literal64) -> Source {
		Source::Literal64(value)
	}
}

pub struct FmtDisplaySource<'a> {
	source: Source,
	borrowed_metadata: &'a [MemorySlotMetadata],
}

// This is a damn mess, belch
impl<'a> std::fmt::Display for FmtDisplaySource<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self.source {
			Source::Literal8(literal) => write!(f, "{literal}"),
			Source::Literal16(literal) => write!(f, "{literal}"),
			Source::Literal32(literal) => write!(f, "{literal}"),
			Source::Literal64(literal) => write!(f, "{literal}"),
			Source::MemorySlot(slot) => FmtDisplayMemorySlot { slot, borrowed_metadata: self.borrowed_metadata }.fmt(f),
		}
	}
}

#[derive(Debug, Clone, Copy)]
pub enum Destination {
	MemorySlot(MemorySlot),
}

impl Destination {
	pub fn primative_size(self, module: &IrModule) -> PrimativeSize {
		match self {
			Destination::MemorySlot(slot) => slot.primative_size(module),
		}
	}

	pub fn display(self, module: &IrModule) -> FmtDisplayDestination {
		FmtDisplayDestination {
			destination: self,
			borrowed_metadata: module.current_function().memory_slots.as_slice(),
		}
	}
}

impl From<MemorySlot> for Destination {
	fn from(value: MemorySlot) -> Destination {
		Destination::MemorySlot(value)
	}
}

pub struct FmtDisplayDestination<'a> {
	destination: Destination,
	borrowed_metadata: &'a [MemorySlotMetadata],
}

// This is a damn mess, belch
impl<'a> std::fmt::Display for FmtDisplayDestination<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self.destination {
			Destination::MemorySlot(slot) => FmtDisplayMemorySlot { slot, borrowed_metadata: self.borrowed_metadata }.fmt(f),
		}
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FlowControlId {
	index: u32,
}

impl std::fmt::Display for FlowControlId {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "^{}", self.index)
	}
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
	pub memory_slots: Vec<MemorySlotMetadata>,
	pub intermediate_slots: Vec<IntermediateMetadata>,
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
			intermediate_slots: Vec::new(),
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
		let metadata = MemorySlotMetadata { primative_size: None, next_forward_operation_is_write: false };
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
		instruction.check_sizes_match(self);

		// let mut source_slots_buffer = [MemorySlot::default(); 2];
		// let source_slots = instruction.source_slots(&mut source_slots_buffer);
		// let destination = instruction.destination();

		// for source_slot in source_slots {
		// 	if let Some(destination) = destination {
		// 		if source_slot.index == destination.index {
		// 			continue;
		// 		}
		// 	}
		// }

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

		let line_number = |index: usize| {
			print!("{index:3}|");
		};

		let mut indent_level = 1;
		let indent = |indent_level: u32, index: usize, instruction: Instruction| {
			line_number(index);

			if instruction.removed {
				print!("✕| ");
			} else {
				print!(" | ");
			}

			for _ in 0..indent_level {
				print!("    ");
			}
		};

		for (index, &instruction) in self.instructions.iter().enumerate() {
			match instruction.kind {
				InstructionKind::Function { function } => {
					self.current_function = function.index;
					if index != 0 {
						println!();
					}
					line_number(index);
					println!(" | Function {function}");
					indent_level = 1
				}

				InstructionKind::Branch { id, conditional } => {
					indent(indent_level, index, instruction);
					println!("Branch {} else → {id}", conditional.display(self));
					indent_level += 1;
				}

				InstructionKind::While { conditional, id } => {
					indent(indent_level, index, instruction);
					println!("While {}: {id}", conditional.display(self));
					indent_level += 1;
				}

				InstructionKind::End { id } => {
					indent_level -= 1;
					indent(indent_level, index, instruction);
					println!("End {id}");
				}

				InstructionKind::Move8 { value, destination } => {
					indent(indent_level, index, instruction);
					println!("{} = Move8 {value}", destination.display(self));
				}

				InstructionKind::Move16 { value, destination } => {
					indent(indent_level, index, instruction);
					println!("{} = Move16 {value}", destination.display(self));
				}

				InstructionKind::Move32 { value, destination } => {
					indent(indent_level, index, instruction);
					println!("{} = Move32 {value}", destination.display(self));
				}

				InstructionKind::Move64 { value, destination } => {
					indent(indent_level, index, instruction);
					println!("{} = Move64 {value}", destination.display(self));
				}

				InstructionKind::Add { kind, left, right, destination } => {
					indent(indent_level, index, instruction);
					println!(
						"{} = Add<{kind}> {}, {}",
						destination.display(self),
						left.display(self),
						right.display(self),
					);
				}

				InstructionKind::Subtract { kind, left, right, destination } => {
					indent(indent_level, index, instruction);
					println!(
						"{} = Subtract<{kind}> {}, {}",
						destination.display(self),
						left.display(self),
						right.display(self),
					);
				}

				InstructionKind::Multiply { kind, left, right, destination } => {
					indent(indent_level, index, instruction);
					println!(
						"{} = Multiply<{kind}> {}, {}",
						destination.display(self),
						left.display(self),
						right.display(self),
					);
				}

				InstructionKind::Divide { kind, left, right, destination } => {
					indent(indent_level, index, instruction);
					println!(
						"{} = Divide<{kind}> {}, {}",
						destination.display(self),
						left.display(self),
						right.display(self),
					);
				}

				InstructionKind::Call { function } => {
					indent(indent_level, index, instruction);
					println!("Call {function}");
				}
			}
		}
	}
}
