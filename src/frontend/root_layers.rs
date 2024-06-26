use rustc_hash::FxHashMap;

use crate::frontend::error::Messages;
use crate::frontend::symbols::{Symbol, Symbols};
use crate::frontend::tree::{Node, PathSegments};
use crate::lock::RwLock;
use crate::reference::Ref;

#[derive(Debug)]
pub struct RootLayers<'a> {
	// pub layers: FxHashMap<&'a str, Ref<RwLock<RootLayer<'a>>>>,
	root: Ref<RwLock<RootLayer<'a>>>,
	pub root_name: String,
}

impl<'a> RootLayers<'a> {
	pub fn new(root_name: String) -> Self {
		RootLayers { root: Ref::new(RwLock::new(RootLayer::new(""))), root_name }
	}

	pub fn layer_for_path(&self, messages: &mut Messages, path: &PathSegments<'a>) -> Option<Ref<RwLock<RootLayer<'a>>>> {
		self.layer_for_module_path(messages, &path.segments)
	}

	fn layer_for_module_path(&self, messages: &mut Messages, segments: &[Node<&'a str>]) -> Option<Ref<RwLock<RootLayer<'a>>>> {
		assert!(!segments.is_empty());
		let mut next = self.root.clone();

		for (piece_index, piece) in segments.iter().enumerate() {
			let guard = next.read();
			let layer = match guard.children.get(piece.item) {
				Some(layer) => layer.clone(),

				None => {
					messages.message(error!("Cannot find module layer for path segment").span(piece.span));
					return None;
				}
			};

			if piece_index + 1 == segments.len() {
				return Some(layer);
			}

			drop(guard);
			next = layer;
		}

		unreachable!()
	}

	pub fn create_module_path(&self, path: &'a [String]) -> Ref<RwLock<RootLayer<'a>>> {
		assert!(!path.is_empty());

		let mut next = self.root.clone();
		for (piece_index, piece) in path.iter().enumerate() {
			let mut guard = next.write();
			let layer = guard
				.children
				.entry(piece.as_str())
				.or_insert(Ref::new(RwLock::new(RootLayer::new(piece))))
				.clone();

			if piece_index + 1 == path.len() {
				return layer;
			}

			drop(guard);
			next = layer;
		}

		unreachable!();
	}

	pub fn lookup_module_path(&self, path: &'a [String]) -> Ref<RwLock<RootLayer<'a>>> {
		assert!(!path.is_empty());

		let mut next = self.root.clone();
		for (piece_index, piece) in path.iter().enumerate() {
			let guard = next.read();
			let layer = guard.children.get(piece.as_str()).unwrap().clone();

			if piece_index + 1 == path.len() {
				return layer;
			}

			drop(guard);
			next = layer;
		}

		unreachable!();
	}

	pub fn lookup_path_symbol(&self, messages: &mut Messages, path: &PathSegments<'a>) -> Option<Symbol<'a>> {
		let segments = path.segments;
		assert!(segments.len() > 1);
		let layer = self.layer_for_module_path(messages, &segments[..segments.len() - 1])?;
		let lock = layer.read();
		lock.lookup_root_symbol(messages, &[*segments.last().unwrap()])
	}
}

#[derive(Debug)]
pub struct RootLayer<'a> {
	pub name: &'a str,
	pub children: FxHashMap<&'a str, Ref<RwLock<RootLayer<'a>>>>,
	pub symbols: Symbols<'a>,
	pub importable_types_index: usize,
	pub importable_functions_index: usize,
	pub importable_consts_index: usize,
	pub importable_statics_index: usize,
}

impl<'a> RootLayer<'a> {
	fn new(name: &'a str) -> Self {
		RootLayer {
			name,
			children: FxHashMap::default(),
			symbols: Symbols::new(),
			importable_types_index: usize::MAX,
			importable_functions_index: usize::MAX,
			importable_consts_index: usize::MAX,
			importable_statics_index: usize::MAX,
		}
	}

	fn lookup_root_symbol(&self, messages: &mut Messages, segments: &[Node<&'a str>]) -> Option<Symbol<'a>> {
		assert_eq!(segments.len(), 1);

		let segment = &segments[0];
		let name = segment.item;
		let found = self.symbols.scopes.iter().find_map(|scope| scope.get(name));

		if found.is_none() {
			messages.message(error!("No symbol `{name}` in root of module `{}`", self.name).span(segment.span));
		}
		found.copied()
	}

	pub fn importable_types(&self) -> &FxHashMap<&'a str, Symbol<'a>> {
		&self.symbols.scopes[self.importable_types_index]
	}

	pub fn importable_functions(&self) -> &FxHashMap<&'a str, Symbol<'a>> {
		&self.symbols.scopes[self.importable_functions_index]
	}

	pub fn importable_consts(&self) -> &FxHashMap<&'a str, Symbol<'a>> {
		&self.symbols.scopes[self.importable_consts_index]
	}

	pub fn importable_statics(&self) -> &FxHashMap<&'a str, Symbol<'a>> {
		&self.symbols.scopes[self.importable_statics_index]
	}
}
