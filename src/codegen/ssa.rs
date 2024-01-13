use crate::codegen::intermediate::*;
use crate::type_store::NumericKind;

#[derive(Debug)]
pub enum Instruction {
	Phi {
		sources: Vec<Binding>, // TODO: Use a smallvec
		destination: Binding,
	},

	Function {
		function: Function,
	},

	Label {
		label: Label,
	},

	Branch {
		conditional: Binding,
		else_label: Label,
	},

	Move8 {
		value: Intermediate8,
		destination: Binding,
	},

	Move16 {
		value: Intermediate16,
		destination: Binding,
	},

	Move32 {
		value: Intermediate32,
		destination: Binding,
	},

	Move64 {
		value: Intermediate64,
		destination: Binding,
	},

	Add {
		kind: NumericKind,
		left: Binding,
		right: Binding,
		destination: Binding,
	},

	Subtract {
		kind: NumericKind,
		left: Binding,
		right: Binding,
		destination: Binding,
	},

	Multiply {
		kind: NumericKind,
		left: Binding,
		right: Binding,
		destination: Binding,
	},

	Divide {
		kind: NumericKind,
		left: Binding,
		right: Binding,
		destination: Binding,
	},
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Binding {
	pub index: u16,
}

impl Binding {
	pub fn new(index: u16) -> Binding {
		Binding { index }
	}

	pub fn display(self, module: &SsaModule) -> FmtDisplayBinding {
		FmtDisplayBinding {
			binding: self,
			borrowed_metadata: module.current_function().bindings.as_slice(),
		}
	}
}

pub struct FmtDisplayBinding<'a> {
	binding: Binding,
	borrowed_metadata: &'a [BindingMetadata],
}

impl<'a> std::fmt::Display for FmtDisplayBinding<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		let metadata = self.borrowed_metadata[self.binding.index as usize];

		if let Some(memory_slot) = metadata.memory_slot {
			write!(f, "[{}]", memory_slot)
		} else {
			write!(f, "${}", self.binding.index)
		}
	}
}

#[derive(Debug, Clone, Copy)]
pub struct Function {
	pub index: u32,
}

impl std::fmt::Display for Function {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "`{}", self.index)
	}
}

#[derive(Debug, Clone, Copy)]
pub struct Label {
	index: u32,
}

impl std::fmt::Display for Label {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "#{}", self.index)
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BindingMetadata {
	pub reads: u32,
	pub phi_target: Option<u16>,
	pub memory_slot: Option<u16>,
}

#[derive(Debug)]
pub struct FunctionData {
	next_label: u32,
	bindings: Vec<BindingMetadata>,
}

#[derive(Debug)]
pub struct SsaModule {
	pub instructions: Vec<Instruction>,
	pub next_function: u32,
	pub current_function: u32,
	pub functions: Vec<FunctionData>,
	pub optimized: bool,
}

impl SsaModule {
	pub fn new() -> SsaModule {
		SsaModule {
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
		self.functions.push(FunctionData { next_label: 0, bindings: Vec::new() });

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

	pub fn next_label(&mut self) -> Label {
		let current_function = self.current_function_mut();
		let label = Label { index: current_function.next_label };
		current_function.next_label += 1;
		label
	}

	pub fn next_binding(&mut self) -> Binding {
		let current_function = self.current_function_mut();
		let index = current_function.bindings.len() as u16;
		let metadata = BindingMetadata { reads: 0, phi_target: None, memory_slot: None };
		current_function.bindings.push(metadata);
		Binding { index }
	}

	pub fn push_phi(&mut self, sources: Vec<Binding>) -> Binding {
		assert!(sources.len() >= 2, "{sources:?}");
		let destination = self.next_binding();
		let current_function = self.current_function_mut();

		for source in &sources {
			let phi_target = &mut current_function.bindings[source.index as usize].phi_target;
			assert_eq!(*phi_target, None);
			*phi_target = Some(destination.index);
		}

		self.instructions.push(Instruction::Phi { sources, destination });
		destination
	}

	pub fn start_function(&mut self) -> Function {
		let function = self.next_function();
		self.instructions.push(Instruction::Function { function });
		function
	}

	// conditional value must be a single byte, uses C style numeric truthiness
	// returns the else-branch, insert at the end of the branch block
	pub fn push_branch(&mut self, conditional: Binding) -> Label {
		let else_label = self.next_label();
		self.instructions.push(Instruction::Branch { conditional, else_label });
		else_label
	}

	pub fn push_label(&mut self, label: Label) {
		self.instructions.push(Instruction::Label { label });
	}

	pub fn push_move_8(&mut self, value: impl Into<Intermediate8>) -> Binding {
		let value = value.into();
		let destination = self.next_binding();
		self.instructions.push(Instruction::Move8 { value, destination });
		destination
	}

	pub fn push_move_16(&mut self, value: impl Into<Intermediate16>) -> Binding {
		let value = value.into();
		let destination = self.next_binding();
		self.instructions.push(Instruction::Move16 { value, destination });
		destination
	}

	pub fn push_move_32(&mut self, value: impl Into<Intermediate32>) -> Binding {
		let value = value.into();
		let destination = self.next_binding();
		self.instructions.push(Instruction::Move32 { value, destination });
		destination
	}

	pub fn push_move_64(&mut self, value: impl Into<Intermediate64>) -> Binding {
		let value = value.into();
		let destination = self.next_binding();
		self.instructions.push(Instruction::Move64 { value, destination });
		destination
	}

	pub fn push_add(&mut self, kind: NumericKind, left: Binding, right: Binding) -> Binding {
		let destination = self.next_binding();
		self.instructions.push(Instruction::Add { kind, left, right, destination });
		destination
	}

	pub fn push_subtract(&mut self, kind: NumericKind, left: Binding, right: Binding) -> Binding {
		let destination = self.next_binding();
		self.instructions
			.push(Instruction::Subtract { kind, left, right, destination });
		destination
	}

	pub fn push_multiply(&mut self, kind: NumericKind, left: Binding, right: Binding) -> Binding {
		let destination = self.next_binding();
		self.instructions
			.push(Instruction::Multiply { kind, left, right, destination });
		destination
	}

	pub fn push_divide(&mut self, kind: NumericKind, left: Binding, right: Binding) -> Binding {
		let destination = self.next_binding();
		self.instructions.push(Instruction::Divide { kind, left, right, destination });
		destination
	}

	pub fn debug_dump(&mut self) {
		self.current_function = 0;

		if self.optimized {
			println!("Optimized SSA Module:");
		} else {
			println!("Unoptimized SSA Module:");
		}
		let indent = || print!("    ");
		let function_label_indent = || print!("  ");

		for (index, instruction) in self.instructions.iter().enumerate() {
			match instruction {
				Instruction::Phi { sources, destination } => {
					indent();
					print!("{} = Phi ", destination.display(self));
					for (index, source) in sources.iter().enumerate() {
						print!("{}", source.display(self));
						if index + 1 < sources.len() {
							print!(", ");
						}
					}
					println!();
				}

				Instruction::Function { function } => {
					self.current_function = function.index;
					if index != 0 {
						println!("\n");
					}
					function_label_indent();
					println!("Function {function}");
				}

				Instruction::Label { label } => {
					function_label_indent();
					println!("Label {label}");
				}

				Instruction::Branch { conditional, else_label } => {
					indent();
					println!("Branch {} else → {else_label}", conditional.display(self));
				}

				Instruction::Move8 { value, destination } => {
					indent();
					println!("{} = Move8 {value}", destination.display(self));
				}

				Instruction::Move16 { value, destination } => {
					indent();
					println!("{} = Move16 {value}", destination.display(self));
				}

				Instruction::Move32 { value, destination } => {
					indent();
					println!("{} = Move32 {value}", destination.display(self));
				}

				Instruction::Move64 { value, destination } => {
					indent();
					println!("{} = Move64 {value}", destination.display(self));
				}

				Instruction::Add { kind, left, right, destination } => {
					indent();
					println!(
						"{} = Add<{kind}> {}, {}",
						destination.display(self),
						left.display(self),
						right.display(self),
					);
				}

				Instruction::Subtract { kind, left, right, destination } => {
					indent();
					println!(
						"{} = Subtract<{kind}> {}, {}",
						destination.display(self),
						left.display(self),
						right.display(self),
					);
				}

				Instruction::Multiply { kind, left, right, destination } => {
					indent();
					println!(
						"{} = Multiply<{kind}> {}, {}",
						destination.display(self),
						left.display(self),
						right.display(self),
					);
				}

				Instruction::Divide { kind, left, right, destination } => {
					indent();
					println!(
						"{} = Divide<{kind}> {}, {}",
						destination.display(self),
						left.display(self),
						right.display(self),
					);
				}
			}
		}
	}
}
