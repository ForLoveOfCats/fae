use std::collections::hash_map;

use rustc_hash::FxHashMap;

use crate::frontend::error::Messages;
use crate::frontend::root_layers::RootLayers;
use crate::frontend::span::Span;
use crate::frontend::tree::{ExternAttribute, PathSegments};
use crate::frontend::type_store::{TypeId, TypeStore};

#[derive(Debug, Copy, Clone)]
pub struct Symbol<'a> {
	pub name: &'a str,
	pub kind: SymbolKind,
	pub span: Option<Span>,
	pub used: bool,
}

#[derive(Debug, Clone, Copy)]
pub enum SymbolKind {
	BuiltinType { type_id: TypeId },
	Type { shape_index: usize },
	UserTypeGeneric { shape_index: usize, generic_index: usize },
	FunctionGeneric { function_shape_index: usize, generic_index: usize },
	Function { function_shape_index: usize },
	Const { constant_index: usize },
	Static { static_index: usize },
	Let { readable_index: usize },
	Mut { readable_index: usize },
}

impl std::fmt::Display for SymbolKind {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		let name = match self {
			SymbolKind::BuiltinType { .. } => "a built in type",
			SymbolKind::Type { .. } => "a type",
			SymbolKind::UserTypeGeneric { .. } => "a type generic parameter",
			SymbolKind::FunctionGeneric { .. } => "a function generic parameter",
			SymbolKind::Function { .. } => "a function",
			SymbolKind::Const { .. } => "a constant",
			SymbolKind::Static { .. } => "a static",
			SymbolKind::Let { .. } => "an immutable binding",
			SymbolKind::Mut { .. } => "a mutable binding",
		};

		f.write_str(name)
	}
}

#[derive(Debug, Clone)]
pub struct Symbols<'a> {
	pub scopes: Vec<FxHashMap<&'a str, Symbol<'a>>>,
}

impl<'a> Symbols<'a> {
	pub fn new() -> Self {
		Symbols { scopes: Vec::new() }
	}

	pub fn child_scope<'s>(&'s mut self) -> SymbolsScope<'a, 's> {
		self.scopes.push(FxHashMap::default());
		SymbolsScope { symbols: self }
	}

	pub fn push_symbol(&mut self, messages: &mut Messages, function_initial_scope_count: usize, mut symbol: Symbol<'a>) {
		let can_shadow = matches!(
			symbol.kind,
			SymbolKind::Let { .. }
				| SymbolKind::Mut { .. }
				| SymbolKind::FunctionGeneric { .. }
				| SymbolKind::UserTypeGeneric { .. }
		);

		if !can_shadow {
			if let Some(found) = self.find_local_symbol_matching_name(function_initial_scope_count, symbol.name, false) {
				// `symbol.span` should only be None for builtin types, yes it's a hack, shush
				messages.message(
					error!("Duplicate symbol `{}`", symbol.name)
						.span_if_some(symbol.span)
						.note_if_some(found.span, "Original symbol here"),
				);
			}
		}

		if symbol.name.starts_with('_') {
			symbol.used = true;
		}

		// Pushing the symbol even if duplicate is probably has less error virality
		self.scopes.last_mut().unwrap().insert(symbol.name, symbol);
	}

	pub fn push_imported_symbol(
		&mut self,
		messages: &mut Messages,
		function_initial_scope_count: usize,
		mut symbol: Symbol<'a>,
		import_span: Option<Span>,
		is_prelude: bool,
	) {
		if let Some(found) = self.find_local_symbol_matching_name(function_initial_scope_count, symbol.name, false) {
			messages.message(
				error!("Import conflicts with existing symbol `{}`", found.name)
					.span_if_some(import_span)
					.note_if_some(found.span, "Existing symbol here"),
			);
		} else {
			symbol.used = is_prelude;
			if let Some(import_span) = import_span {
				symbol.span = Some(import_span);
			}

			self.scopes.last_mut().unwrap().insert(symbol.name, symbol);
		}
	}

	fn find_local_symbol_matching_name(
		&mut self,
		function_initial_scope_count: usize,
		name: &str,
		mark_used: bool,
	) -> Option<Symbol<'a>> {
		let mut scope_index = self.scopes.len();
		for scope in self.scopes.iter_mut().rev() {
			scope_index -= 1;

			let Some(symbol) = scope.get_mut(name) else {
				continue;
			};

			symbol.used |= mark_used;

			if scope_index < function_initial_scope_count {
				match symbol.kind {
					SymbolKind::Function { .. }
					| SymbolKind::Type { .. }
					| SymbolKind::Const { .. }
					| SymbolKind::Static { .. }
					| SymbolKind::BuiltinType { .. } => {}

					_ => break,
				}
			}

			return Some(*symbol);
		}

		None
	}

	pub fn lookup_symbol(
		&mut self,
		messages: &mut Messages,
		root_layers: &RootLayers<'a>,
		type_store: &TypeStore<'a>,
		function_initial_scope_count: usize,
		path: &PathSegments<'a>,
	) -> Option<Symbol<'a>> {
		if let [segment] = path.segments {
			let name = segment.item;

			let primatives = &type_store.primative_type_symbols;
			if let Some(&found) = primatives.iter().find(|symbol| symbol.name == name) {
				return Some(found);
			}

			if let Some(found) = self.find_local_symbol_matching_name(function_initial_scope_count, name, true) {
				return Some(found);
			}

			messages.message(error!("No symbol `{name}` in the current scope").span(segment.span));
			None
		} else {
			root_layers.lookup_path_symbol(messages, path)
		}
	}
}

#[derive(Debug)]
pub struct SymbolsScope<'a, 'b> {
	pub symbols: &'b mut Symbols<'a>,
}

impl<'a, 'b> SymbolsScope<'a, 'b> {
	pub fn child_scope<'s>(&'s mut self) -> SymbolsScope<'a, 's> {
		self.symbols.scopes.push(FxHashMap::default());
		SymbolsScope { symbols: self.symbols }
	}

	pub fn report_unused(&self, messages: &mut Messages<'a>) {
		let scope = self.symbols.scopes.last().unwrap();
		report_unused(scope, messages);
	}
}

pub fn report_unused<'a>(scope: &FxHashMap<&'a str, Symbol<'a>>, messages: &mut Messages<'a>) {
	// TODO: At this point we should just switch back to linear symbol lookup
	// if we're going to have to iterate through them all at the end of each
	// scope anyway which can be super expensive with a hashmap
	for symbol in scope.values() {
		if !symbol.used {
			let warning = warning!("Unused symbol `{}`", symbol.name);
			messages.message(warning.span_if_some(symbol.span));
		}
	}
}

impl<'a, 'b> Drop for SymbolsScope<'a, 'b> {
	fn drop(&mut self) {
		self.symbols.scopes.pop();
	}
}

#[derive(Debug, Clone, Copy)]
pub struct ExternLocation<'a> {
	pub span: Span,
	pub module_path: &'a [String],
}

#[derive(Debug)]
pub struct Externs<'a> {
	pub externs: FxHashMap<String, Vec<ExternLocation<'a>>>,
}

impl<'a> Externs<'a> {
	pub fn new() -> Self {
		Externs { externs: FxHashMap::default() }
	}

	pub fn push(&mut self, messages: &mut Messages<'a>, name: &str, span: Span) {
		let location = ExternLocation { span, module_path: messages.module_path() };

		match self.externs.entry(name.to_string()) {
			hash_map::Entry::Occupied(mut occupied) => {
				occupied.get_mut().push(location);
			}

			hash_map::Entry::Vacant(entry) => {
				entry.insert(vec![location]);
			}
		}
	}
}

#[derive(Debug, Clone, Copy)]
pub struct Static<'a> {
	pub name: &'a str,
	pub type_id: TypeId,
	pub extern_attribute: Option<ExternAttribute<'a>>,
}

#[derive(Debug)]
pub struct Statics<'a> {
	pub statics: Vec<Static<'a>>,
}

impl<'a> Statics<'a> {
	pub fn new() -> Self {
		Statics { statics: Vec::new() }
	}

	pub fn push(&mut self, name: &'a str, type_id: TypeId, extern_attribute: Option<ExternAttribute<'a>>) -> usize {
		let index = self.statics.len();
		let instance = Static { name, type_id, extern_attribute };
		self.statics.push(instance);
		index
	}
}

#[derive(Debug, Clone, Copy)]
pub struct Readable<'a> {
	pub name: &'a str,
	pub type_id: TypeId,
	pub kind: ReadableKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReadableKind {
	Let,
	Mut,
}

#[derive(Debug)]
pub struct Readables<'a> {
	pub starting_index: usize,
	pub readables: Vec<Readable<'a>>,
}

impl<'a> Readables<'a> {
	pub fn new() -> Self {
		Readables { starting_index: 0, readables: Vec::new() }
	}

	pub fn overall_len(&self) -> usize {
		self.readables.len()
	}

	pub fn push(&mut self, name: &'a str, type_id: TypeId, kind: ReadableKind) -> usize {
		let index = self.readables.len() - self.starting_index;
		self.readables.push(Readable { name, type_id, kind });
		index
	}

	pub fn get(&mut self, index: usize) -> Option<Readable<'a>> {
		self.readables.get(index + self.starting_index).copied()
	}
}
