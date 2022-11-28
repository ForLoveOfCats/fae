use crate::error::*;
use crate::mir::*;
use crate::tree;
use crate::tree::PathSegments;

struct Context<'a, 'b, 'p> {
	file_layers: &'b FileLayers<'a>,
	current_layer: &'b FileLayer<'a>,
	type_store: &'b TypeStore<'a>,
	scope: Scope<'a, 'p>,
}

impl<'a, 'b, 'p> Context<'a, 'b, 'p> {
	fn child_scope<'s>(&'s mut self) -> Context<'a, 'b, 's> {
		Context {
			file_layers: &*self.file_layers,
			current_layer: &*self.current_layer,
			type_store: &*self.type_store,
			scope: self.scope.child(),
		}
	}
}

pub struct FileLayers<'a> {
	layers: Vec<FileLayer<'a>>,
}

impl<'a> FileLayers<'a> {
	pub fn new() -> Self {
		FileLayers { layers: Vec::new() }
	}

	fn layer_for_module_path(
		&self,
		messages: &mut Messages,
		path: &[tree::Node<&'a str>],
	) -> Option<&FileLayer<'a>> {
		assert!(path.len() > 0);
		let mut layers = &self.layers;

		for (piece_index, piece) in path.iter().enumerate() {
			let layer = match layers.iter().position(|x| x.name == piece.node) {
				Some(index) => &layers[index],

				None => {
					messages.error(
						message!("Cannot find module layer for path segment").span(piece.span),
					);
					return None;
				}
			};

			let last_piece = piece_index + 1 == path.len();
			if last_piece {
				return Some(layer);
			}

			layers = &layer.children;
		}

		unreachable!()
	}

	fn layer_or_create_for_module_path(&mut self, path: &[String]) -> &mut FileLayer<'a> {
		assert!(path.len() > 0);
		let mut layers = &mut self.layers;

		for (piece_index, piece) in path.iter().enumerate() {
			let layer = match layers.iter().position(|x| x.name == *piece) {
				Some(index) => &mut layers[index],

				None => {
					layers.push(FileLayer::new(piece));
					layers.last_mut().unwrap()
				}
			};

			let last_piece = piece_index + 1 == path.len();
			if last_piece {
				return layer;
			}

			layers = &mut layer.children;
		}

		unreachable!()
	}

	fn lookup_path_symbol(
		&self,
		messages: &mut Messages,
		segments: &[tree::Node<&'a str>],
	) -> Option<&Symbol> {
		assert!(!segments.is_empty());
		let layer = self.layer_for_module_path(messages, &segments)?;
		layer.lookup_symbol(messages, self, &[*segments.last().unwrap()])
	}
}

#[derive(Debug)]
pub struct FileLayer<'a> {
	name: String,
	children: Vec<FileLayer<'a>>,

	scope_symbols: Vec<Symbol<'a>>,
	root_symbols: Vec<Symbol<'a>>,
}

impl<'a> FileLayer<'a> {
	fn new(name: &str) -> Self {
		FileLayer {
			name: name.to_owned(),
			children: Vec::new(),

			scope_symbols: Vec::new(),
			root_symbols: Vec::new(),
		}
	}

	fn lookup_symbol<'b>(
		&'b self,
		messages: &mut Messages,
		file_layers: &'b FileLayers<'a>,
		segments: &[tree::Node<&'a str>],
	) -> Option<&Symbol> {
		assert!(!segments.is_empty());

		if segments.len() == 1 {
			let segment = &segments[0];
			let name = segment.node;
			let found = self.root_symbols.iter().find(|symbol| symbol.name == name);

			if found.is_none() {
				messages
					.error(message!("No symbol in scope with name {name:?}").span(segment.span));
			}
			found
		} else {
			file_layers.lookup_path_symbol(messages, segments)
		}
	}
}

#[derive(Debug, Default, Clone, Copy)]
struct FrameState {
	symbols_len: usize,
}

#[derive(Debug)]
struct Scope<'a, 'p> {
	initial_state: FrameState,
	symbols: &'p mut Vec<Symbol<'a>>,
}

impl<'a, 'p> Drop for Scope<'a, 'p> {
	fn drop(&mut self) {
		self.symbols.truncate(self.initial_state.symbols_len);
	}
}

impl<'a, 'p> Scope<'a, 'p> {
	fn child<'s>(&'s mut self) -> Scope<'a, 's> {
		Scope {
			initial_state: FrameState {
				symbols_len: self.symbols.len(),
			},
			symbols: &mut *self.symbols,
		}
	}
}
pub struct TypeStore<'a> {
	concrete_types: Vec<Type<'a>>,

	void_type_id: TypeId,
	reference_concrete_index: usize,
	slice_concrete_index: usize,
}

impl<'a> TypeStore<'a> {
	pub fn new() -> Self {
		//This two-phase initialization is unfortunate
		let mut store = TypeStore {
			concrete_types: Vec::new(),
			void_type_id: TypeId {
				concrete_index: 0,
				specialization_index: 0,
			},
			reference_concrete_index: 0,
			slice_concrete_index: 0,
		};

		store.register_builtin_type("void", TypeKind::Primative);
		store.void_type_id = TypeId {
			concrete_index: store.concrete_types.len() - 1,
			specialization_index: 0,
		};

		//These are just to park the concrete index so we can generate valid TypeIds for them
		store.register_builtin_type("&", TypeKind::Primative);
		store.reference_concrete_index = store.concrete_types.len() - 1;
		store.register_builtin_type("[]", TypeKind::Primative);
		store.slice_concrete_index = store.concrete_types.len() - 1;

		store.register_builtin_type("i8", TypeKind::Primative);
		store.register_builtin_type("i16", TypeKind::Primative);
		store.register_builtin_type("i32", TypeKind::Primative);
		store.register_builtin_type("i64", TypeKind::Primative);

		store.register_builtin_type("u8", TypeKind::Primative);
		store.register_builtin_type("u16", TypeKind::Primative);
		store.register_builtin_type("u32", TypeKind::Primative);
		store.register_builtin_type("u64", TypeKind::Primative);

		store.register_builtin_type("f16", TypeKind::Primative);
		store.register_builtin_type("f32", TypeKind::Primative);
		store.register_builtin_type("f64", TypeKind::Primative);

		store
	}

	#[must_use = "Logical error to call without handling resulting symbol"]
	pub fn register_type(&mut self, name: &'a str, kind: TypeKind<'a>) -> Symbol<'a> {
		self.concrete_types.push(Type {
			name: name.to_string(),
			kind,
			specialization: Vec::new(),
		});

		let concrete_index = self.concrete_types.len() - 1;
		let symbol_kind = SymbolKind::Type { concrete_index };
		Symbol { name, kind: symbol_kind }
	}

	fn register_builtin_type(&mut self, name: &'a str, kind: TypeKind<'a>) {
		let symbol = self.register_type(name, kind);
	}

	fn lookup_type(
		&mut self,
		messages: &mut Messages,
		file_layers: &FileLayers,
		current_layer: &FileLayer,
		parsed_type: &tree::Type,
	) -> Option<TypeId> {
		let (segments, arguments) = match parsed_type {
			tree::Type::Void => return Some(self.void_type_id),

			tree::Type::Reference(inner) => {
				let inner_id =
					self.lookup_type(messages, file_layers, current_layer, &inner.node)?;
				let concrete_index = self.reference_concrete_index;
				let concrete = &mut self.concrete_types[concrete_index];
				let specialization_index = concrete.get_or_add_specialization(vec![inner_id]);
				return Some(TypeId {
					concrete_index,
					specialization_index,
				});
			}

			tree::Type::Slice(inner) => {
				let inner_id =
					self.lookup_type(messages, file_layers, current_layer, &inner.node)?;
				let concrete_index = self.slice_concrete_index;
				let concrete = &mut self.concrete_types[concrete_index];
				let specialization_index = concrete.get_or_add_specialization(vec![inner_id]);
				return Some(TypeId {
					concrete_index,
					specialization_index,
				});
			}

			tree::Type::Path { segments, arguments } => (segments, arguments),
		};

		assert!(!segments.segments.is_empty());
		let symbol = current_layer.lookup_symbol(messages, file_layers, &segments.segments)?;

		let concrete_index = match symbol.kind {
			SymbolKind::Type { concrete_index } => concrete_index,

			_ => {
				messages.error(
					message!("Symbol {:?} is not a type", symbol.name)
						.span(segments.segments.last().unwrap().span),
				);
				return None;
			}
		};

		let mut type_args = Vec::with_capacity(arguments.len());
		for argument in arguments {
			type_args.push(self.lookup_type(
				messages,
				file_layers,
				current_layer,
				&argument.node,
			)?);
		}

		let concrete = &mut self.concrete_types[concrete_index];
		let specialization_index = concrete.get_or_add_specialization(type_args);

		Some(TypeId {
			concrete_index,
			specialization_index,
		})
	}
}
