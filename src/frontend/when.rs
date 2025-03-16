use crate::frontend::error::Messages;
use crate::frontend::project::TargetPlatform;
use crate::frontend::tree::{Block, Node, WhenElseChain};

#[derive(Debug)]
pub struct WhenContext {
	pub target_platform: TargetPlatform,
	pub release_mode: bool,
	pub provide_main: bool,
	pub in_compiler_test: bool,
}

impl WhenContext {
	pub fn evaluate_when<'a, 'b>(&self, messages: &mut Messages, chain: &'b WhenElseChain<'a>) -> Option<&'b Node<Block<'a>>> {
		for entry in chain.entries {
			if self.validate_when_condition(messages, entry.condition) {
				return Some(&entry.body);
			}
		}

		if let Some(body) = &chain.else_body {
			return Some(body);
		}

		None
	}

	fn validate_when_condition<'a>(&self, messages: &mut Messages, condition: Node<&'a str>) -> bool {
		match condition.item {
			"PlatformLinux" => self.target_platform == TargetPlatform::Linux,

			"PlatformDarwin" => self.target_platform == TargetPlatform::Darwin,

			"DebugBuild" => !self.release_mode,

			"ReleaseBuild" => self.release_mode,

			"ProvideMain" => self.provide_main,

			"InCompilerTest" => self.in_compiler_test,

			_ => {
				let error = error!("Unknown `when` condition {:?}", condition.item);
				messages.message(error.span(condition.span));
				false
			}
		}
	}
}
