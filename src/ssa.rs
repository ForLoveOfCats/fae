use crate::type_store::NumericKind;

#[derive(Debug)]
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
}

#[derive(Debug)]
pub struct SsaModule {
	pub instructions: Vec<Instruction>,
}

impl SsaModule {
	pub fn new() -> SsaModule {
		SsaModule { instructions: Vec::new() }
	}

	pub fn push(&mut self, instruction: Instruction) {
		self.instructions.push(instruction);
	}
}

impl std::fmt::Display for SsaModule {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		for instruction in &self.instructions {
			match instruction {
				Instruction::Add { kind, left, right, destination } => {
					writeln!(f, "Add<{kind}> {left} • {right} → {destination}")?
				}

				Instruction::Subtract { kind, left, right, destination } => {
					writeln!(f, "Subtract<{kind}> {left} • {right} → {destination}")?
				}

				Instruction::Multiply { kind, left, right, destination } => {
					writeln!(f, "Multiply<{kind}> {left} • {right} → {destination}")?
				}

				Instruction::Divide { kind, left, right, destination } => {
					writeln!(f, "Divide<{kind}> {left} • {right} → {destination}")?
				}
			}
		}

		Ok(())
	}
}
