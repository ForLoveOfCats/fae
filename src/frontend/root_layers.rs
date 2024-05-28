use std::ops::Range;

use rustc_hash::FxHashMap;

use crate::frontend::error::Messages;
use crate::frontend::symbols::{Symbol, Symbols};
use crate::frontend::tree::{Node, PathSegments};

#[derive(Debug)]
pub struct RootLayers<'a> {
	pub layers: FxHashMap<&'a str, RootLayer<'a>>,
	pub root_name: String,
	pub root_layer_index: usize,
}

impl<'a> RootLayers<'a> {
	pub fn new(root_name: String) -> Self {
		RootLayers {
			layers: FxHashMap::default(),
			root_name,
			root_layer_index: usize::MAX,
		}
	}

	pub fn layer_for_path(&self, messages: &mut Messages, path: &PathSegments<'a>) -> Option<&RootLayer<'a>> {
		self.layer_for_module_path(messages, &path.segments)
	}

	fn layer_for_module_path(&self, messages: &mut Messages, segments: &[Node<&'a str>]) -> Option<&RootLayer<'a>> {
		assert!(!segments.is_empty());
		let mut layers = &self.layers;

		for (piece_index, piece) in segments.iter().enumerate() {
			let layer = match layers.get(piece.item) {
				Some(layer) => layer,

				None => {
					messages.message(error!("Cannot find module layer for path segment").span(piece.span));
					return None;
				}
			};

			if piece_index + 1 == segments.len() {
				return Some(layer);
			}
			layers = &layer.children;
		}

		unreachable!()
	}

	pub fn create_module_path(&mut self, path: &'a [String]) -> &mut RootLayer<'a> {
		assert!(!path.is_empty());
		if let [piece] = &path {
			if *piece == self.root_name {
				return self.layers.entry(piece.as_str()).or_insert(RootLayer::new(piece));
			}
		}

		let mut layers = &mut self.layers;
		for (piece_index, piece) in path.iter().enumerate() {
			let layer = layers.entry(piece.as_str()).or_insert(RootLayer::new(piece));

			if piece_index + 1 == path.len() {
				return layer;
			}
			layers = &mut layer.children;
		}

		unreachable!();
	}

	pub fn lookup_path_symbol(&self, messages: &mut Messages, path: &PathSegments<'a>) -> Option<Symbol<'a>> {
		let segments = path.segments.as_slice();
		assert!(segments.len() > 1);
		let layer = self.layer_for_module_path(messages, &segments[..segments.len() - 1])?;
		layer.lookup_root_symbol(messages, &[*segments.last().unwrap()])
	}
}

#[derive(Debug)]
pub struct RootLayer<'a> {
	pub name: &'a str,
	pub children: FxHashMap<&'a str, RootLayer<'a>>,
	pub symbols: Symbols<'a>,
	pub importable_types_range: Range<usize>,
	pub importable_functions_range: Range<usize>,
	pub importable_consts_range: Range<usize>,
	pub importable_statics_range: Range<usize>,
}

impl<'a> RootLayer<'a> {
	fn new(name: &'a str) -> Self {
		RootLayer {
			name,
			children: FxHashMap::default(),
			symbols: Symbols::new(),
			importable_types_range: 0..0,
			importable_functions_range: 0..0,
			importable_consts_range: 0..0,
			importable_statics_range: 0..0,
		}
	}

	fn lookup_root_symbol(&self, messages: &mut Messages, segments: &[Node<&'a str>]) -> Option<Symbol<'a>> {
		assert_eq!(segments.len(), 1);

		let segment = &segments[0];
		let name = segment.item;
		let found = self.symbols.symbols.iter().find(|symbol| symbol.name == name);

		if found.is_none() {
			messages.message(error!("No symbol `{name}` in root of module `{}`", self.name).span(segment.span));
		}
		found.copied()
	}

	pub fn importable_types(&self) -> &[Symbol<'a>] {
		&self.symbols.symbols[self.importable_types_range.clone()]
	}

	pub fn importable_functions(&self) -> &[Symbol<'a>] {
		&self.symbols.symbols[self.importable_functions_range.clone()]
	}

	pub fn importable_consts(&self) -> &[Symbol<'a>] {
		&self.symbols.symbols[self.importable_consts_range.clone()]
	}

	pub fn importable_statics(&self) -> &[Symbol<'a>] {
		&self.symbols.symbols[self.importable_statics_range.clone()]
	}
}
