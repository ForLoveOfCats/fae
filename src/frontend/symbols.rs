use std::collections::{hash_map, HashMap};

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
	pub symbols: Vec<Symbol<'a>>,
}

impl<'a> Symbols<'a> {
	pub fn new() -> Self {
		Symbols { symbols: Vec::new() }
	}

	pub fn len(&self) -> usize {
		self.symbols.len()
	}

	pub fn is_empty(&self) -> bool {
		self.symbols.is_empty()
	}

	pub fn duplicate(&mut self, other: &Symbols<'a>) {
		self.symbols.clear();
		self.symbols.extend_from_slice(&other.symbols);
	}

	pub fn child_scope<'s>(&'s mut self) -> SymbolsScope<'a, 's> {
		SymbolsScope { initial_symbols_len: self.len(), symbols: self }
	}

	pub fn push_symbol(&mut self, messages: &mut Messages, function_initial_symbols_len: usize, symbol: Symbol<'a>) {
		let can_shadow = matches!(
			symbol.kind,
			SymbolKind::Let { .. }
				| SymbolKind::Mut { .. }
				| SymbolKind::FunctionGeneric { .. }
				| SymbolKind::UserTypeGeneric { .. }
		);

		if !can_shadow {
			if let Some(found) = self.find_local_symbol_matching_name(function_initial_symbols_len, symbol.name) {
				// `symbol.span` should only be None for builtin types, yes it's a hack, shush
				messages.message(
					error!("Duplicate symbol `{}`", symbol.name)
						.span_if_some(symbol.span)
						.note_if_some(found.span, "Original symbol here"),
				);
			}
		}

		// Pushing the symbol even if duplicate seems to be a better failure state for following errors
		self.symbols.push(symbol);
	}

	pub fn push_imported_symbol(
		&mut self,
		messages: &mut Messages,
		function_initial_symbols_len: usize,
		symbol: Symbol<'a>,
		import_span: Option<Span>,
	) {
		if let Some(found) = self.find_local_symbol_matching_name(function_initial_symbols_len, symbol.name) {
			messages.message(
				error!("Import conflicts with existing symbol `{}`", found.name)
					.span_if_some(import_span)
					.note_if_some(found.span, "Existing symbol here"),
			);
		} else {
			self.symbols.push(symbol);
		}
	}

	fn find_local_symbol_matching_name(&self, function_initial_symbols_len: usize, name: &str) -> Option<Symbol<'a>> {
		let mut index = self.symbols.len();
		for &symbol in self.symbols.iter().rev() {
			index -= 1;

			if symbol.name == name {
				if index < function_initial_symbols_len {
					match symbol.kind {
						SymbolKind::Function { .. }
						| SymbolKind::Type { .. }
						| SymbolKind::Const { .. }
						| SymbolKind::Static { .. }
						| SymbolKind::BuiltinType { .. } => {}

						_ => break,
					}
				}

				return Some(symbol);
			}
		}

		None
	}

	pub fn lookup_symbol(
		&self,
		messages: &mut Messages,
		root_layers: &RootLayers<'a>,
		type_store: &TypeStore<'a>,
		function_initial_symbols_len: usize,
		path: &PathSegments<'a>,
	) -> Option<Symbol<'a>> {
		let segments = match path {
			PathSegments::Path { segments } => Some(segments.as_slice()),
			PathSegments::MainModule { .. } => return root_layers.lookup_path_symbol(messages, path),
		};

		if let Some([segment]) = segments {
			let name = segment.item;

			let primatives = &type_store.primative_type_symbols;
			if let Some(&found) = primatives.iter().find(|symbol| symbol.name == name) {
				return Some(found);
			}

			if let Some(found) = self.find_local_symbol_matching_name(function_initial_symbols_len, name) {
				return Some(found);
			}

			messages.message(error!("No symbol `{name}` in the current scope").span(segment.span));
			None
		} else {
			root_layers.lookup_path_symbol(messages, path)
		}
	}
}

pub struct SymbolsScope<'a, 'b> {
	pub initial_symbols_len: usize,
	pub symbols: &'b mut Symbols<'a>,
}

impl<'a, 'b> Drop for SymbolsScope<'a, 'b> {
	fn drop(&mut self) {
		self.symbols.symbols.truncate(self.initial_symbols_len);
	}
}

#[derive(Debug)]
pub struct Externs {
	pub externs: HashMap<String, Span>,
}

impl Externs {
	pub fn new() -> Externs {
		Externs { externs: HashMap::new() }
	}

	pub fn push(&mut self, messages: &mut Messages, name: &str, span: Span) {
		match self.externs.entry(name.to_string()) {
			hash_map::Entry::Occupied(occupied) => {
				let error = error!("Duplicate extern declaration {name:?}").span(span);
				messages.message(error.note(note!(*occupied.get(), "Other declaration here")));
			}

			hash_map::Entry::Vacant(entry) => {
				entry.insert(span);
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
