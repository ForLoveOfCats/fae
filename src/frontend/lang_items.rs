use crate::frontend::error::Messages;
use crate::frontend::ir::FunctionId;
use crate::frontend::span::Span;
use crate::frontend::type_store::TypeId;

#[derive(Debug)]
pub struct LangItems {
	pub range_type: Option<TypeId>,
	pub format_string_item_type: Option<TypeId>,

	pub slice_index_out_of_bounds: Option<FunctionId>,
	pub slice_range_inverted: Option<FunctionId>,
	pub slice_range_start_out_of_bounds: Option<FunctionId>,
	pub slice_range_end_out_of_bounds: Option<FunctionId>,

	pub for_range_inverted: Option<FunctionId>,
}

impl LangItems {
	pub fn new() -> LangItems {
		LangItems {
			range_type: None,
			format_string_item_type: None,

			slice_index_out_of_bounds: None,
			slice_range_inverted: None,
			slice_range_start_out_of_bounds: None,
			slice_range_end_out_of_bounds: None,

			for_range_inverted: None,
		}
	}

	pub fn register_lang_type(&mut self, messages: &mut Messages, type_id: TypeId, lang_name: &str, span: Span) {
		match lang_name {
			"range_type" => self.range_type = Some(type_id),

			"format_string_item_type" => self.format_string_item_type = Some(type_id),

			_ => {
				let error = error!("Unknown lang item type {lang_name:?}");
				messages.message(error.span(span));
			}
		}
	}

	pub fn register_lang_function(&mut self, messages: &mut Messages, function_id: FunctionId, lang_name: &str, span: Span) {
		match lang_name {
			"slice_index_out_of_bounds" => self.slice_index_out_of_bounds = Some(function_id),
			"slice_range_inverted" => self.slice_range_inverted = Some(function_id),
			"slice_range_start_out_of_bounds" => self.slice_range_start_out_of_bounds = Some(function_id),
			"slice_range_end_out_of_bounds" => self.slice_range_end_out_of_bounds = Some(function_id),

			"for_range_inverted" => self.for_range_inverted = Some(function_id),

			_ => {
				let error = error!("Unknown lang item function {lang_name:?}");
				messages.message(error.span(span));
			}
		}
	}
}
