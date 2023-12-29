#![allow(unused)] // TODO: Remove after demo

use std::num::NonZeroU32;

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
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_fmt(format_args!("'{}", self.index))
	}
}

#[derive(Debug)]
pub struct BindingMetadata {
	pub reads: u32,
	// to have any bindings to phi, the result binding cannot possibly be 0
	pub phi_target: Option<NonZeroU32>, // size == sizeof(u32) with niche
}

#[derive(Debug)]
pub enum Instruction {
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

	Phi {
		sources: Vec<Binding>, // TODO: Use a smallvec
		destination: Binding,
	},
}

#[derive(Debug)]
pub struct SsaModule {
	instructions: Vec<Instruction>,
	bindings: Vec<BindingMetadata>,
}

impl SsaModule {
	pub fn new() -> SsaModule {
		SsaModule { instructions: Vec::new(), bindings: Vec::new() }
	}

	pub fn instruction_count(&self) -> usize {
		self.instructions.len()
	}

	pub fn next_binding(&mut self) -> Binding {
		let index = self.bindings.len() as u32;
		self.bindings.push(BindingMetadata { reads: 0, phi_target: None });
		Binding { index }
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

	pub fn push_phi(&mut self, sources: Vec<Binding>) -> Binding {
		let destination = self.next_binding();
		for source in &sources {
			let phi_target = &mut self.bindings[source.index as usize].phi_target;
			assert_eq!(*phi_target, None);
			*phi_target = Some(NonZeroU32::new(destination.index).unwrap());
		}
		self.instructions.push(Instruction::Phi { sources, destination });
		destination
	}
}

impl std::fmt::Display for SsaModule {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		for instruction in &self.instructions {
			match instruction {
				Instruction::Add { kind, left, right, destination } => {
					writeln!(f, "{destination} = Add<{kind}>({left}, {right})")?;
				}

				Instruction::Subtract { kind, left, right, destination } => {
					writeln!(f, "{destination} = Subtract<{kind}>({left}, {right})")?;
				}

				Instruction::Multiply { kind, left, right, destination } => {
					writeln!(f, "{destination} = Multiply<{kind}>({left}, {right})")?;
				}

				Instruction::Divide { kind, left, right, destination } => {
					writeln!(f, "{destination} = Divide<{kind}>({left}, {right})")?;
				}

				Instruction::Phi { sources, destination } => {
					write!(f, "{destination} = Phi(")?;
					for (index, source) in sources.iter().enumerate() {
						write!(f, "{source}")?;
						if index + 1 < sources.len() {
							write!(f, ", ")?;
						}
					}
					writeln!(f, ")")?;
				}
			}
		}

		Ok(())
	}
}
