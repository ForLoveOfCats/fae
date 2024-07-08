use crate::frontend::error::Messages;
use crate::frontend::ir::FunctionId;
use crate::frontend::span::Span;
use crate::frontend::type_store::TypeId;

#[derive(Debug)]
pub struct LangItems {
	pub range_type: Option<TypeId>,

	pub slice_bound_check_failure: Option<FunctionId>,
}

impl LangItems {
	pub fn new() -> LangItems {
		LangItems { range_type: None, slice_bound_check_failure: None }
	}

	pub fn register_lang_type(&mut self, messages: &mut Messages, type_id: TypeId, lang_name: &str, span: Span) {
		match lang_name {
			"range_type" => self.range_type = Some(type_id),

			_ => {
				let error = error!("Unknown lang item type {lang_name:?}");
				messages.message(error.span(span));
			}
		}
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
