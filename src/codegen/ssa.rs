use std::num::NonZeroU32;

use crate::codegen::intermediate::*;
use crate::type_store::NumericKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Binding {
	pub index: u32,
}

impl Binding {
	pub fn new(index: u32) -> Binding {
		Binding { index }
	}
}

impl std::fmt::Display for Binding {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "${}", self.index)
	}
}

#[derive(Debug)]
pub struct BindingMetadata {
	pub reads: u32,
	// to have any bindings to phi, the result binding cannot possibly be 0
	pub phi_target: Option<NonZeroU32>, // size == sizeof(u32) with niche
}

#[derive(Debug, Clone, Copy)]
pub struct Function {
	index: u32,
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

#[derive(Debug)]
pub struct SsaModule {
	instructions: Vec<Instruction>,
	next_function: u32,
	next_label: u32,
	bindings: Vec<BindingMetadata>,
}

impl SsaModule {
	pub fn new() -> SsaModule {
		SsaModule {
			instructions: Vec::new(),
			next_function: 0,
			next_label: 0,
			bindings: Vec::new(),
		}
	}

	pub fn instruction_count(&self) -> usize {
		self.instructions.len()
	}

	pub fn next_function(&mut self) -> Function {
		let function = Function { index: self.next_function };
		self.next_function += 1;
		function
	}

	pub fn next_label(&mut self) -> Label {
		let label = Label { index: self.next_label };
		self.next_label += 1;
		label
	}

	pub fn next_binding(&mut self) -> Binding {
		let index = self.bindings.len() as u32;
		self.bindings.push(BindingMetadata { reads: 0, phi_target: None });
		Binding { index }
	}

	pub fn push_phi(&mut self, sources: Vec<Binding>) -> Binding {
		assert!(sources.len() >= 2, "{sources:?}");
		let destination = self.next_binding();

		for source in &sources {
			let phi_target = &mut self.bindings[source.index as usize].phi_target;
			assert_eq!(*phi_target, None);
			*phi_target = Some(NonZeroU32::new(destination.index).unwrap());
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
}

impl std::fmt::Display for SsaModule {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		let indent = |f: &mut std::fmt::Formatter| write!(f, "  ");

		for (index, instruction) in self.instructions.iter().enumerate() {
			match instruction {
				Instruction::Phi { sources, destination } => {
					indent(f)?;
					write!(f, "{destination} = Phi ")?;
					for (index, source) in sources.iter().enumerate() {
						write!(f, "{source}")?;
						if index + 1 < sources.len() {
							write!(f, ", ")?;
						}
					}
					writeln!(f)?;
				}

				Instruction::Function { function } => {
					if index != 0 {
						writeln!(f, "\n")?;
					}
					writeln!(f, "Function {function}")?;
				}

				Instruction::Label { label } => writeln!(f, "Label {label}")?,

				Instruction::Branch { conditional, else_label } => {
					indent(f)?;
					writeln!(f, "Branch {conditional} else → {else_label}")?;
				}

				Instruction::Move8 { value, destination } => {
					indent(f)?;
					writeln!(f, "{destination} = Move8 {value}")?;
				}

				Instruction::Move16 { value, destination } => {
					indent(f)?;
					writeln!(f, "{destination} = Move16 {value}")?;
				}

				Instruction::Move32 { value, destination } => {
					indent(f)?;
					writeln!(f, "{destination} = Move32 {value}")?;
				}

				Instruction::Move64 { value, destination } => {
					indent(f)?;
					writeln!(f, "{destination} = Move64 {value}")?;
				}

				Instruction::Add { kind, left, right, destination } => {
					indent(f)?;
					writeln!(f, "{destination} = Add<{kind}> {left}, {right}")?;
				}

				Instruction::Subtract { kind, left, right, destination } => {
					indent(f)?;
					writeln!(f, "{destination} = Subtract<{kind}> {left}, {right}")?;
				}

				Instruction::Multiply { kind, left, right, destination } => {
					indent(f)?;
					writeln!(f, "{destination} = Multiply<{kind}> {left}, {right}")?;
				}

				Instruction::Divide { kind, left, right, destination } => {
					indent(f)?;
					writeln!(f, "{destination} = Divide<{kind}> {left}, {right}")?;
				}
			}
		}

		Ok(())
	}
}
