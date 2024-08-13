use crate::frontend::type_store::TypeId;

#[derive(Debug, Clone, Copy)]
pub struct YieldTarget {
	pub type_id: Option<TypeId>,
	pub outer_loop_index: Option<usize>,
}

#[derive(Debug)]
pub struct YieldTargets {
	pub starting_index: usize,
	pub targets: Vec<YieldTarget>,
}

impl YieldTargets {
	pub fn new() -> YieldTargets {
		YieldTargets { starting_index: 0, targets: Vec::new() }
	}

	pub fn overall_len(&self) -> usize {
		self.targets.len()
	}

	pub fn push(&mut self, type_id: Option<TypeId>, outer_loop_index: Option<usize>) -> usize {
		let index = self.targets.len() - self.starting_index;
		self.targets.push(YieldTarget { type_id, outer_loop_index });
		index
	}

	pub fn get(&self, index: usize) -> YieldTarget {
		self.targets.get(index + self.starting_index).copied().unwrap()
	}

	pub fn get_mut(&mut self, index: usize) -> &mut YieldTarget {
		self.targets.get_mut(index + self.starting_index).unwrap()
	}
}
