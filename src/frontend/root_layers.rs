use std::ops::Range;

use rustc_hash::FxHashMap;

use crate::frontend::error::Messages;
use crate::frontend::symbols::{Symbol, Symbols};
use crate::frontend::tree::{Node, PathSegments};
use crate::lock::RwLock;
use crate::reference::Ref;

#[derive(Debug)]
pub struct RootLayers<'a> {
	root: Ref<RwLock<RootLayer<'a>>>,
	pub root_name: String,
}

impl<'a> RootLayers<'a> {
	pub fn new(root_name: String) -> Self {
		RootLayers { root: Ref::new(RwLock::new(RootLayer::new(""))), root_name }
	}

	pub fn layer_for_path(&self, messages: Option<&mut Messages>, path: &PathSegments<'a>) -> Option<Ref<RwLock<RootLayer<'a>>>> {
		self.layer_for_module_path(messages, &path.segments)
	}

	pub fn layer_for_module_path(
		&self,
		messages: Option<&mut Messages>,
		segments: &[Node<&'a str>],
	) -> Option<Ref<RwLock<RootLayer<'a>>>> {
		layer_for_module_path_under_root(messages, self.root.clone(), segments)
	}

	pub fn layer_for_module_name(&self, name: Node<&'a str>) -> Option<Ref<RwLock<RootLayer<'a>>>> {
		let guard = self.root.read();
		if let Some(layer) = guard.children.get(name.item) {
			return Some(layer.clone());
		}

		None
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
		let layer = self.layer_for_module_path(Some(messages), &segments[..segments.len() - 1])?;
		let mut lock = layer.write();
		lock.lookup_root_symbol(messages, &[*segments.last().unwrap()])
	}
}

#[derive(Debug)]
pub struct RootLayer<'a> {
	pub name: &'a str,
	pub children: FxHashMap<&'a str, Ref<RwLock<RootLayer<'a>>>>,
	pub symbols: Symbols<'a>,
	pub importable_traits_range: Range<usize>,
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
			importable_traits_range: usize::MAX - 1..usize::MAX,
			importable_types_range: usize::MAX - 1..usize::MAX,
			importable_functions_range: usize::MAX - 1..usize::MAX,
			importable_consts_range: usize::MAX - 1..usize::MAX,
			importable_statics_range: usize::MAX - 1..usize::MAX,
		}
	}

	pub fn lookup_root_symbol(&mut self, messages: &mut Messages, segments: &[Node<&'a str>]) -> Option<Symbol<'a>> {
		assert_eq!(segments.len(), 1);

		let segment = &segments[0];
		let name = segment.item;
		let found = self.symbols.symbols.iter_mut().find(|symbol| symbol.name == name);

		if let Some(found) = found {
			if found.imported == false {
				found.used = true;
				return Some(found.clone());
			}
		}

		messages.message(error!("No symbol `{name}` in root of module `{}`", self.name).span(segment.span));
		None
	}
}

pub fn layer_for_module_path_under_root<'a>(
	messages: Option<&mut Messages>,
	root: Ref<RwLock<RootLayer<'a>>>,
	segments: &[Node<&'a str>],
) -> Option<Ref<RwLock<RootLayer<'a>>>> {
	assert!(!segments.is_empty());
	let mut next = root;

	for (piece_index, piece) in segments.iter().enumerate() {
		let guard = next.read();
		let layer = match guard.children.get(piece.item) {
			Some(layer) => layer.clone(),

			None => {
				if let Some(messages) = messages {
					messages.message(error!("Cannot find module for path segment").span(piece.span));
				}
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
