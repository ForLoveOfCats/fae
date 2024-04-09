use crate::frontend::error::Messages;
use crate::frontend::ir::FunctionId;
use crate::frontend::span::Span;

#[derive(Debug)]
pub struct LangItems {
	pub slice_bound_check_failure: Option<FunctionId>,
}

impl LangItems {
	pub fn new() -> LangItems {
		LangItems { slice_bound_check_failure: None }
	}

	pub fn register_lang_function(&mut self, messages: &mut Messages, function_id: FunctionId, lang_name: &str, span: Span) {
		match lang_name {
			"slice_bound_check_failure" => self.slice_bound_check_failure = Some(function_id),

			_ => {
				let error = error!("Unknown lang item function {lang_name:?}");
				messages.message(error.span(span));
			}
		}
	}
}
