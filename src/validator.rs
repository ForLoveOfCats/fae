use crate::error::*;
use crate::mir::*;
use crate::span::Span;
use crate::tree;

pub struct Context<'a, 'b, 'p> {
	messages: &'b mut Messages<'a>,
	file_layers: &'b FileLayers<'a>,
	current_layer: &'b FileLayer<'a>,
	type_store: &'b mut TypeStore<'a>,
	scope: Scope<'a, 'p>,
}

impl<'a, 'b, 'p> Context<'a, 'b, 'p> {
	fn child_scope<'s>(&'s mut self) -> Context<'a, 's, 's> {
		Context {
			messages: &mut *self.messages,
			file_layers: &*self.file_layers,
			current_layer: &*self.current_layer,
			type_store: &mut *self.type_store,
			scope: self.scope.child(),
		}
	}

	fn lookup_symbol(&mut self, segments: &[tree::Node<&'a str>]) -> Option<&Symbol<'a>> {
		assert!(!segments.is_empty());

		if segments.len() == 1 {
			let segment = &segments[0];
			let name = segment.node;
			let found = self.scope.symbols.iter().find(|symbol| symbol.name == name);

			if found.is_none() {
				self.messages.error(
					message!("No symbol named {name:?} in the current scope").span(segment.span),
				);
			}
			found
		} else {
			self.file_layers.lookup_path_symbol(self.messages, segments)
		}
	}
}

#[derive(Debug, Clone)]
pub struct FileLayers<'a> {
	layers: Vec<FileLayer<'a>>,
}

impl<'a> FileLayers<'a> {
	pub fn build(messages: &mut Messages, parsed_files: &'a [tree::File<'a>]) -> Option<Self> {
		let mut file_layers = FileLayers { layers: Vec::new() };

		for file in parsed_files {
			file_layers.create_module_path(messages, file);
		}

		Some(file_layers)
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

	fn create_module_path(&mut self, messages: &mut Messages, file: &'a tree::File<'a>) {
		assert!(file.module_path.len() > 0);
		let mut layers = &mut self.layers;

		for (piece_index, piece) in file.module_path.iter().enumerate() {
			let layer = match layers.iter().position(|x| x.name == *piece) {
				Some(index) => &mut layers[index],

				None => {
					layers.push(FileLayer::new(piece, file));
					layers.last_mut().unwrap()
				}
			};

			let last_piece = piece_index + 1 == file.module_path.len();
			if last_piece {
				if layer.block.is_some() {
					messages.error(message!(
						"Duplicate module with path {:?}",
						file.module_path
					));
				} else {
					layer.block = Some(&file.block);
				}
			}

			layers = &mut layer.children;
		}
	}

	fn lookup_path_symbol(
		&self,
		messages: &mut Messages,
		segments: &[tree::Node<&'a str>],
	) -> Option<&Symbol<'a>> {
		assert!(!segments.is_empty());
		let layer = self.layer_for_module_path(messages, &segments)?;
		layer.lookup_root_symbol(messages, &[*segments.last().unwrap()])
	}
}

#[derive(Debug, Clone)]
pub struct FileLayer<'a> {
	name: &'a str,
	file: &'a tree::File<'a>,

	children: Vec<FileLayer<'a>>,
	block: Option<&'a tree::Block<'a>>,

	root_symbols: Vec<Symbol<'a>>,
}

impl<'a> FileLayer<'a> {
	fn new(name: &'a str, file: &'a tree::File<'a>) -> Self {
		FileLayer {
			name,
			file,
			children: Vec::new(),
			block: None,
			root_symbols: Vec::new(),
		}
	}

	fn lookup_root_symbol<'b>(
		&'b self,
		messages: &mut Messages,
		segments: &[tree::Node<&'a str>],
	) -> Option<&Symbol<'a>> {
		assert_eq!(segments.len(), 1);

		let segment = &segments[0];
		let name = segment.node;
		let found = self.root_symbols.iter().find(|symbol| symbol.name == name);

		if found.is_none() {
			messages.error(
				message!("No symbol named {name:?} in root of module {:?}", self.name)
					.span(segment.span),
			);
		}
		found
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

	fn push_symbol(&mut self, messages: &mut Messages, symbol: Symbol<'a>) {
		let name = symbol.name;
		if let Some(found) = self.symbols.iter().find(|s| s.name == name) {
			//symbol.span should only be None for builtin types, yes it's a hack, shush
			messages.error(
				message!("Duplicate symbol {name:?}")
					.span_if_some(symbol.span)
					.note_if_some("Original symbol here", found.span, found.file_index),
			);
		} else {
			self.symbols.push(symbol);
		}
	}
}

pub struct TypeStore<'a> {
	concrete_types: Vec<Type<'a>>,

	builtin_type_symbols: Vec<Symbol<'a>>,

	void_type_id: TypeId,
	reference_concrete_index: usize,
	slice_concrete_index: usize,
}

impl<'a> TypeStore<'a> {
	pub fn new() -> Self {
		//This two-phase initialization is unfortunate
		let mut store = TypeStore {
			concrete_types: Vec::new(),
			builtin_type_symbols: Vec::new(),
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

		//Garbage names to just park the concrete index so we can generate TypeIds for these two
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
	pub fn register_type(
		&mut self,
		name: &'a str,
		kind: TypeKind<'a>,
		span: Option<Span>,
		file_index: Option<usize>,
	) -> Symbol<'a> {
		self.concrete_types.push(Type {
			name: name.to_string(),
			kind,
			specialization: Vec::new(),
		});

		let concrete_index = self.concrete_types.len() - 1;
		let kind = SymbolKind::Type { concrete_index };
		Symbol { name, kind, span, file_index }
	}

	fn register_builtin_type(&mut self, name: &'a str, kind: TypeKind<'a>) {
		let symbol = self.register_type(name, kind, None, None);
		self.builtin_type_symbols.push(symbol);
	}

	fn lookup_type(
		&mut self,
		parsed_type: &tree::Type<'a>,
		cx: &mut Context<'a, '_, '_>,
	) -> Option<TypeId> {
		let (segments, arguments) = match parsed_type {
			tree::Type::Void => return Some(self.void_type_id),

			tree::Type::Reference(inner) => {
				let inner_id = self.lookup_type(&inner.node, cx)?;
				let concrete_index = self.reference_concrete_index;
				let concrete = &mut self.concrete_types[concrete_index];
				let specialization_index = concrete.get_or_add_specialization(vec![inner_id]);
				return Some(TypeId {
					concrete_index,
					specialization_index,
				});
			}

			tree::Type::Slice(inner) => {
				let inner_id = self.lookup_type(&inner.node, cx)?;
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
		let symbol = cx.lookup_symbol(&segments.segments)?;

		let concrete_index = match symbol.kind {
			SymbolKind::Type { concrete_index } => concrete_index,

			_ => {
				let name = symbol.name;
				cx.messages.error(
					message!("Symbol {name:?} is not a type")
						.span(segments.segments.last().unwrap().span),
				);
				return None;
			}
		};

		let mut type_args = Vec::with_capacity(arguments.len());
		for argument in arguments {
			type_args.push(self.lookup_type(&argument.node, cx)?);
		}

		let concrete = &mut self.concrete_types[concrete_index];
		let specialization_index = concrete.get_or_add_specialization(type_args);

		Some(TypeId {
			concrete_index,
			specialization_index,
		})
	}
}

pub fn validate_file_layers<'a>(
	messages: &mut Messages,
	file_layers: &mut FileLayers<'a>,
	type_store: &mut TypeStore<'a>,
) {
	fill_root_scopes(messages, file_layers, type_store);
	resolve_root_scope_inports(messages, file_layers, type_store);
}

fn fill_root_scopes<'a>(
	messages: &mut Messages,
	file_layers: &mut FileLayers<'a>,
	type_store: &mut TypeStore<'a>,
) {
	fn handle_layers_fill<'a>(
		messages: &mut Messages,
		file_layers: &mut [FileLayer<'a>],
		type_store: &mut TypeStore<'a>,
	) {
		for layer in file_layers {
			assert_eq!(layer.root_symbols.len(), 0);

			handle_layers_fill(messages, &mut layer.children, type_store);

			let block = match layer.block {
				Some(block) => block,
				_ => continue,
			};
			messages.set_current_file_index(layer.file.source_file.index);

			let mut symbols = type_store.builtin_type_symbols.to_vec();
			let mut scope = Scope {
				//The initial state doesn't matter, we aren't going to drop this scope
				initial_state: FrameState { symbols_len: 0 },
				symbols: &mut symbols,
			};

			let index = layer.file.source_file.index;
			fill_block_scope(messages, block, true, type_store, &mut scope, index);

			std::mem::forget(scope); //Avoid cleaning up symbols
			layer.root_symbols = symbols;
		}
	}

	handle_layers_fill(messages, &mut file_layers.layers, type_store);
}

fn resolve_root_scope_inports<'a>(
	messages: &mut Messages,
	file_layers: &mut FileLayers<'a>,
	type_store: &mut TypeStore<'a>,
) {
	let cloned_layers = file_layers.clone();

	fn handle_layers_imports<'a>(
		messages: &mut Messages,
		file_layers: &mut [FileLayer<'a>],
		type_store: &mut TypeStore<'a>,
		cloned_layers: &FileLayers<'a>,
	) {
		for layer in file_layers {
			handle_layers_imports(messages, &mut layer.children, type_store, cloned_layers);

			let block = match layer.block {
				Some(block) => block,
				_ => continue,
			};
			messages.set_current_file_index(layer.file.source_file.index);

			for statement in &block.statements {
				let using_statement = match statement {
					tree::Statement::Using(using_statement) => using_statement,
					_ => continue,
				};

				let path = &using_statement.node.path_segments.node.segments;
				let found = match cloned_layers.layer_for_module_path(messages, path) {
					Some(found) => found,
					_ => continue,
				};

				if found.root_symbols.is_empty() {
					continue;
				}

				let symbols = &found.root_symbols[type_store.builtin_type_symbols.len()..];
				for symbol in symbols {
					let name = symbol.name;

					if let Some(found) = layer.root_symbols.iter().find(|s| s.name == name) {
						messages.error(
							message!("Import of duplicate symbol {name:?}")
								.span(using_statement.span)
								.note_if_some("Original symbol here", found.span, found.file_index)
								.note_if_some(
									"Duplicate symbol here",
									symbol.span,
									symbol.file_index,
								),
						);
					} else {
						layer.root_symbols.push(symbol.clone());
					}
				}
			}
		}
	}

	let layers = &mut file_layers.layers;
	handle_layers_imports(messages, layers, type_store, &cloned_layers);
}

fn fill_block_scope<'a>(
	messages: &mut Messages,
	block: &tree::Block<'a>,
	is_root: bool,
	type_store: &mut TypeStore<'a>,
	scope: &mut Scope<'a, '_>,
	file_index: usize,
) {
	let file_index = Some(file_index);

	for statement in &block.statements {
		if is_root {
			match statement {
				tree::Statement::Expression(..)
				| tree::Statement::Block(..)
				| tree::Statement::Let(..)
				| tree::Statement::Mut(..)
				| tree::Statement::Return(..) => messages.error(
					message!("Disallowed statement kind in root scope").span(statement.span()),
				),

				tree::Statement::Using(..)
				| tree::Statement::Struct(..)
				| tree::Statement::Function(..)
				| tree::Statement::Const(..) => {}
			}
		}

		match statement {
			tree::Statement::Using(..) => {} //Skip

			tree::Statement::Struct(statement) => {
				let name = statement.name.node;
				//Start off with no fields, they will be added during the next pre-pass
				let kind = TypeKind::Struct { fields: Vec::new() };
				let span = Some(statement.name.span);
				let symbol = type_store.register_type(name, kind, span, file_index);
				scope.push_symbol(messages, symbol);
			}

			tree::Statement::Function(statement) => {
				//Start off with no parameters, they will be added during the next pre-pass
				let kind = SymbolKind::Function { parameters: Vec::new() };
				let symbol = Symbol {
					name: statement.name.node,
					kind,
					span: Some(statement.name.span),
					file_index,
				};
				scope.push_symbol(messages, symbol);
			}

			tree::Statement::Const(statement) => {
				//Type id will get filled in during the next pre-pass
				let kind = SymbolKind::Const { type_id: TypeId::invalid() };
				let symbol = Symbol {
					name: statement.node.name.node,
					kind,
					span: Some(statement.span),
					file_index,
				};
				scope.push_symbol(messages, symbol);
			}

			_ => {}
		}
	}
}
