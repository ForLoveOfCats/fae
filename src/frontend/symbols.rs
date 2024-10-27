use std::collections::hash_map;

use rustc_hash::FxHashMap;

use crate::frontend::error::Messages;
use crate::frontend::root_layers::{layer_for_module_path_under_root, RootLayer, RootLayers};
use crate::frontend::span::Span;
use crate::frontend::tree::{ExternAttribute, Node, PathSegments};
use crate::frontend::type_store::{TypeId, TypeStore, UserTypeKind};
use crate::lock::RwLock;
use crate::reference::Ref;

#[derive(Debug, Clone)]
pub struct Symbol<'a> {
	pub name: &'a str,
	pub kind: SymbolKind<'a>,
	pub span: Option<Span>,
	pub used: bool,
	pub imported: bool,
}

#[derive(Debug, Clone)]
pub enum SymbolKind<'a> {
	BuiltinType { type_id: TypeId, methods_index: usize },
	Type { shape_index: usize, methods_index: usize },
	UserTypeGeneric { shape_index: usize, generic_index: usize },
	FunctionGeneric { function_shape_index: usize, generic_index: usize },
	Function { function_shape_index: usize },
	Const { constant_index: usize },
	Static { static_index: usize },
	Let { readable_index: usize },
	Mut { readable_index: usize },
	Module { layer: Ref<RwLock<RootLayer<'a>>> },
}

impl<'a> std::fmt::Display for SymbolKind<'a> {
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
			SymbolKind::Module { .. } => "an imported module",
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

	pub fn child_scope<'s>(&'s mut self) -> SymbolsScope<'a, 's> {
		let initial_symbols_length = self.symbols.len();
		SymbolsScope { symbols: self, initial_symbols_length }
	}

	pub fn push_symbol(&mut self, messages: &mut Messages, function_initial_symbols_length: usize, mut symbol: Symbol<'a>) {
		let can_shadow = matches!(
			symbol.kind,
			SymbolKind::Let { .. }
				| SymbolKind::Mut { .. }
				| SymbolKind::FunctionGeneric { .. }
				| SymbolKind::UserTypeGeneric { .. }
		);

		if !can_shadow {
			if let Some(found) = self.find_local_symbol_matching_name(function_initial_symbols_length, symbol.name, false) {
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
		self.symbols.push(symbol);
	}

	pub fn push_imported_symbol(
		&mut self,
		messages: &mut Messages,
		function_initial_symbols_length: usize,
		mut symbol: Symbol<'a>,
		import_span: Option<Span>,
		is_prelude: bool,
	) {
		if let Some(found) = self.find_local_symbol_matching_name(function_initial_symbols_length, symbol.name, false) {
			messages.message(
				error!("Import conflicts with existing symbol `{}`", found.name)
					.span_if_some(import_span)
					.note_if_some(found.span, "Existing symbol here"),
			);
		} else {
			symbol.used = is_prelude;
			symbol.imported = true;
			if let Some(import_span) = import_span {
				symbol.span = Some(import_span);
			}

			self.symbols.push(symbol);
		}
	}

	fn find_local_symbol_matching_name(
		&mut self,
		function_initial_symbols_length: usize,
		name: &str,
		mark_used: bool,
	) -> Option<Symbol<'a>> {
		let symbols_length = self.symbols.len();

		for (iteration_index, symbol) in self.symbols.iter_mut().rev().enumerate() {
			let symbol_index = symbols_length - iteration_index;

			if symbol.name != name {
				continue;
			}

			symbol.used |= mark_used;

			if symbol_index < function_initial_symbols_length {
				match symbol.kind {
					SymbolKind::Function { .. }
					| SymbolKind::Type { .. }
					| SymbolKind::Const { .. }
					| SymbolKind::Static { .. }
					| SymbolKind::BuiltinType { .. }
					| SymbolKind::Module { .. } => {}

					_ => break,
				}
			}

			return Some(symbol.clone());
		}

		None
	}

	pub fn lookup_symbol_by_name(
		&mut self,
		messages: &mut Messages,
		root_layers: &RootLayers<'a>,
		type_store: &TypeStore<'a>,
		function_initial_symbols_length: usize,
		name: Node<&'a str>,
	) -> Option<Symbol<'a>> {
		let primatives = &type_store.primative_type_symbols;
		if let Some(found) = primatives.iter().find(|symbol| symbol.name == name.item) {
			return Some(found.clone());
		}

		if let Some(found) = self.find_local_symbol_matching_name(function_initial_symbols_length, name.item, true) {
			return Some(found);
		}

		if let Some(layer) = root_layers.layer_for_module_name(name) {
			let kind = SymbolKind::Module { layer };
			let span = Some(name.span);
			let name = name.item;
			let symbol = Symbol { name, kind, span, used: false, imported: false };
			return Some(symbol);
		}

		let error = error!("No symbol `{}` in the current scope", name.item);
		messages.message(error.span(name.span));
		None
	}

	pub fn lookup_path_symbol(
		&mut self,
		messages: &mut Messages,
		root_layers: &RootLayers<'a>,
		type_store: &TypeStore<'a>,
		function_initial_symbols_length: usize,
		path: &PathSegments<'a>,
	) -> Option<Symbol<'a>> {
		if let [segment] = path.segments {
			let name = segment.item;
			let primatives = &type_store.primative_type_symbols;
			if let Some(found) = primatives.iter().find(|symbol| symbol.name == name) {
				return Some(found.clone());
			}
		}

		assert!(!path.segments.is_empty());

		if let [first, ..] = path.segments {
			let Some(found) = self.find_local_symbol_matching_name(function_initial_symbols_length, first.item, true) else {
				if path.segments.len() <= 1 {
					let error = error!("No symbol `{}` in the current scope", first.item);
					messages.message(error.span(first.span));
					return None;
				}

				return root_layers.lookup_path_symbol(messages, path);
			};

			if path.segments.len() == 1 {
				return Some(found);
			}
			let second = path.segments[1];

			if let SymbolKind::Module { mut layer } = found.kind {
				let segments = &path.segments[1..];
				if segments.len() > 1 {
					layer = layer_for_module_path_under_root(Some(messages), layer, &segments[..segments.len() - 1])?;
				}

				let mut lock = layer.write();
				return lock.lookup_root_symbol(messages, &[*segments.last().unwrap()]);
			} else if let SymbolKind::Type { shape_index, .. } = found.kind {
				let lock = type_store.user_types.read()[shape_index].clone();
				let guard = lock.read();
				let shape = match &guard.kind {
					UserTypeKind::Enum { shape } => shape,

					UserTypeKind::Struct { .. } => {
						let error = error!("Cannot path sub-access `{}` as it is a struct", first.item);
						messages.message(error.span(first.span + second.span));
						return None;
					}
				};

				let Some(variant_shape) = shape.variant_shapes.iter().find(|s| s.name == second.item) else {
					let error = error!("No variant `{}` on enum `{}`", second.item, first.item);
					messages.message(error.span(second.span));
					return None;
				};

				if let [_, _, third, ..] = path.segments {
					let error = error!("Cannot path sub-access `{}` as it is an enum variant", second.item);
					messages.message(error.span(second.span + third.span));
					return None;
				}

				let kind = SymbolKind::Type {
					shape_index: variant_shape.struct_shape_index,
					methods_index: variant_shape.methods_index,
				};
				let span = Some(variant_shape.span);
				let symbol = Symbol { name: second.item, kind, span, used: true, imported: false };
				return Some(symbol);
			}

			let error = error!("Cannot path sub-access `{}` as it is {}", first.item, found.kind);
			messages.message(error.span(first.span + second.span));
			return None;
		}

		unreachable!();
	}
}

#[derive(Debug)]
pub struct SymbolsScope<'a, 'b> {
	pub initial_symbols_length: usize,
	pub symbols: &'b mut Symbols<'a>,
}

impl<'a, 'b> SymbolsScope<'a, 'b> {
	pub fn child_scope<'s>(&'s mut self) -> SymbolsScope<'a, 's> {
		let initial_symbols_length = self.symbols.symbols.len();
		SymbolsScope { symbols: self.symbols, initial_symbols_length }
	}

	pub fn report_unused(&self, messages: &mut Messages<'a>) {
		let scope = &self.symbols.symbols[self.initial_symbols_length..];
		report_unused(scope, messages);
	}
}

pub fn report_unused<'a>(scope: &[Symbol<'a>], messages: &mut Messages<'a>) {
	for symbol in scope {
		if !symbol.used {
			let warning = warning!("Unused symbol `{}`", symbol.name);
			messages.message(warning.span_if_some(symbol.span));
		}
	}
}

impl<'a, 'b> Drop for SymbolsScope<'a, 'b> {
	fn drop(&mut self) {
		self.symbols.symbols.truncate(self.initial_symbols_length);
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
	pub is_pointer_access_mutable: bool,
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

	pub fn push(&mut self, name: &'a str, type_id: TypeId, kind: ReadableKind, is_pointer_access_mutable: bool) -> usize {
		let index = self.readables.len() - self.starting_index;
		let readable = Readable { name, type_id, kind, is_pointer_access_mutable };
		self.readables.push(readable);
		index
	}

	pub fn get(&self, index: usize) -> Option<Readable<'a>> {
		self.readables.get(index + self.starting_index).copied()
	}
}
