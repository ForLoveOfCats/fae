use crate::error::*;
use crate::mir::*;
use crate::span::Span;
use crate::tree;

pub struct Context<'a, 'b, 'p> {
	messages: &'b mut Messages<'a>,
	root_layers: &'b RootLayers<'a>,
	type_store: &'b mut TypeStore<'a>,
	scope: Scope<'a, 'p>,
}

impl<'a, 'b, 'p> Context<'a, 'b, 'p> {
	fn child_scope<'s>(&'s mut self) -> Context<'a, 's, 's> {
		Context {
			messages: &mut *self.messages,
			root_layers: &*self.root_layers,
			type_store: &mut *self.type_store,
			scope: self.scope.child(),
		}
	}

	fn lookup_symbol(&mut self, segments: &[tree::Node<&'a str>]) -> Option<Symbol<'a>> {
		self.scope
			.lookup_symbol(self.messages, self.root_layers, self.type_store, segments)
	}
}

pub struct RootLayers<'a> {
	layers: Vec<RootLayer<'a>>,
}

impl<'a> RootLayers<'a> {
	pub fn new() -> Self {
		RootLayers { layers: Vec::new() }
	}

	fn layer_for_module_path(&self, messages: &mut Messages, path: &[tree::Node<&'a str>]) -> Option<&RootLayer<'a>> {
		assert!(path.len() > 0);
		let mut layers = &self.layers;

		for (piece_index, piece) in path.iter().enumerate() {
			let layer = match layers.iter().position(|x| x.name == piece.item) {
				Some(index) => &layers[index],

				None => {
					messages.error(message!("Cannot find module layer for path segment").span(piece.span));
					return None;
				}
			};

			if piece_index + 1 == path.len() {
				return Some(layer);
			}
			layers = &layer.children;
		}

		unreachable!()
	}

	fn create_module_path(&mut self, path: &'a [String]) -> &mut RootLayer<'a> {
		assert!(path.len() > 0);
		let mut layers = &mut self.layers;

		for (piece_index, piece) in path.iter().enumerate() {
			let layer = match layers.iter().position(|x| x.name == *piece) {
				Some(index) => &mut layers[index],

				None => {
					layers.push(RootLayer::new(piece));
					layers.last_mut().unwrap()
				}
			};

			if piece_index + 1 == path.len() {
				return layer;
			}
			layers = &mut layer.children;
		}

		unreachable!();
	}

	fn lookup_path_symbol(&self, messages: &mut Messages, segments: &[tree::Node<&'a str>]) -> Option<Symbol<'a>> {
		assert!(!segments.is_empty());
		let layer = self.layer_for_module_path(messages, &segments)?;
		layer.lookup_root_symbol(messages, &[*segments.last().unwrap()])
	}
}

pub struct RootLayer<'a> {
	name: &'a str,
	children: Vec<RootLayer<'a>>,
	symbols: Vec<Symbol<'a>>,
	importable_types_len: usize,
	imported_types_len: usize,
	importable_functions_len: usize,
}

impl<'a> RootLayer<'a> {
	fn new(name: &'a str) -> Self {
		RootLayer {
			name,
			children: Vec::new(),
			symbols: Vec::new(),
			importable_types_len: 0,
			imported_types_len: 0,
			importable_functions_len: 0,
		}
	}

	fn lookup_root_symbol(&self, messages: &mut Messages, segments: &[tree::Node<&'a str>]) -> Option<Symbol<'a>> {
		assert_eq!(segments.len(), 1);

		let segment = &segments[0];
		let name = segment.item;
		let found = self.symbols.iter().find(|symbol| symbol.name == name);

		if found.is_none() {
			messages.error(message!("No symbol named {name:?} in root of module {:?}", self.name).span(segment.span));
		}
		found.copied()
	}

	fn importable_types(&self, type_store: &TypeStore<'a>) -> &[Symbol<'a>] {
		&self.symbols[0..self.importable_types_len]
	}

	fn importable_functions(&self, type_store: &TypeStore<'a>) -> &[Symbol<'a>] {
		let types_len = self.importable_types_len + self.imported_types_len;
		&self.symbols[types_len..types_len + self.importable_functions_len]
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
			initial_state: FrameState { symbols_len: self.symbols.len() },
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

	fn lookup_symbol(
		&self,
		messages: &mut Messages,
		root_layers: &RootLayers<'a>,
		type_store: &TypeStore<'a>,
		segments: &[tree::Node<&'a str>],
	) -> Option<Symbol<'a>> {
		assert!(!segments.is_empty());

		if let [segment] = segments {
			let name = segment.item;

			let primatives = &type_store.primative_type_symbols;
			if let Some(&found) = primatives.iter().find(|symbol| symbol.name == name) {
				return Some(found);
			}

			if let Some(&found) = self.symbols.iter().find(|symbol| symbol.name == name) {
				return Some(found);
			}

			messages.error(message!("No symbol {name:?} in the current scope").span(segment.span));
			None
		} else {
			root_layers.lookup_path_symbol(messages, segments)
		}
	}
}

struct Primatives {
	next_index: usize,
	primatives: Vec<PrimativeType>,
}

impl Primatives {
	fn new(initial_index: usize) -> Primatives {
		Primatives { next_index: initial_index, primatives: Vec::new() }
	}

	fn push<'a>(&mut self, name: &'static str, kind: PrimativeKind) -> Symbol<'a> {
		let type_id = TypeId { index: self.next_type_index(), specialization: 0 };
		self.primatives.push(PrimativeType { name, kind, type_id });
		let kind = SymbolKind::BuiltinType { type_index: type_id.index };
		Symbol { name, kind, span: None, file_index: None }
	}

	fn next_type_index(&mut self) -> usize {
		let index = self.next_index;
		self.next_index += 1;
		index
	}
}

pub struct TypeStore<'a> {
	primatives: Primatives,
	primative_type_symbols: Vec<Symbol<'a>>,

	user_types: Vec<UserType<'a>>,

	void_type_id: TypeId,
	reference_type_index: usize,
	slice_type_index: usize,
}

impl<'a> TypeStore<'a> {
	pub fn new() -> Self {
		let mut primative_type_symbols = Vec::new();
		let mut primatives = Primatives::new(u32::MAX as usize);

		primative_type_symbols.push(primatives.push("void", PrimativeKind::Void));
		let void_type_id = TypeId { index: u32::MAX as usize, specialization: 0 };

		primative_type_symbols.push(primatives.push("i8", PrimativeKind::I8));
		primative_type_symbols.push(primatives.push("i16", PrimativeKind::I16));
		primative_type_symbols.push(primatives.push("i32", PrimativeKind::I32));
		primative_type_symbols.push(primatives.push("i64", PrimativeKind::I64));

		primative_type_symbols.push(primatives.push("u8", PrimativeKind::U8));
		primative_type_symbols.push(primatives.push("u16", PrimativeKind::U16));
		primative_type_symbols.push(primatives.push("u32", PrimativeKind::U32));
		primative_type_symbols.push(primatives.push("u64", PrimativeKind::U64));

		primative_type_symbols.push(primatives.push("f16", PrimativeKind::F16));
		primative_type_symbols.push(primatives.push("f32", PrimativeKind::F32));
		primative_type_symbols.push(primatives.push("f64", PrimativeKind::F64));

		let reference_type_index = primatives.next_type_index();
		let slice_type_index = primatives.next_type_index();

		TypeStore {
			primatives,
			primative_type_symbols,
			user_types: Vec::new(),
			void_type_id,
			reference_type_index,
			slice_type_index,
		}
	}

	#[must_use]
	fn register_type(
		&mut self,
		name: &'a str,
		kind: UserTypeKind<'a>,
		span: Span,
		file_index: Option<usize>,
	) -> Symbol<'a> {
		self.user_types.push(UserType { span, kind });

		let type_index = self.user_types.len() - 1;
		let kind = SymbolKind::Type { type_index };
		Symbol { name, kind, span: Some(span), file_index }
	}

	fn lookup_type(
		&mut self,
		messages: &mut Messages,
		root_layers: &mut RootLayers<'a>,
		scope: &Scope<'a, '_>,
		parsed_type: &tree::Type<'a>,
	) -> Option<TypeId> {
		let (segments, arguments) = match parsed_type {
			tree::Type::Void => return Some(self.void_type_id),

			tree::Type::Reference(inner) => {
				let inner_id = self.lookup_type(messages, root_layers, scope, &inner.item)?;
				assert!(inner_id.index < u32::MAX as usize, "{}", inner_id.index);
				assert!(inner_id.specialization < u32::MAX as usize, "{}", inner_id.specialization);

				let type_index = self.reference_type_index;
				let concrete_index = inner_id.index | inner_id.specialization << 4 * 8;
				return Some(TypeId { index: type_index, specialization: concrete_index });
			}

			tree::Type::Slice(inner) => {
				let inner_id = self.lookup_type(messages, root_layers, scope, &inner.item)?;
				assert!(inner_id.index < u32::MAX as usize, "{}", inner_id.index);
				assert!(inner_id.specialization < u32::MAX as usize, "{}", inner_id.specialization);

				let type_index = self.slice_type_index;
				let concrete_index = inner_id.index | inner_id.specialization << 4 * 8;
				return Some(TypeId { index: type_index, specialization: concrete_index });
			}

			tree::Type::Path { segments, arguments } => (segments, arguments),
		};

		assert!(!segments.segments.is_empty());
		let symbol = scope.lookup_symbol(messages, root_layers, self, &segments.segments)?;

		let type_index = match symbol.kind {
			SymbolKind::BuiltinType { type_index } => {
				if !arguments.is_empty() {
					let span = segments.segments.last().unwrap().span;
					messages.error(message!("Builtin types do not accept type arguments").span(span));
					return None;
				}

				let index = type_index - u32::MAX as usize;
				return Some(self.primatives.primatives[index].type_id);
			}

			SymbolKind::Type { type_index } => type_index,

			_ => {
				let span = segments.segments.last().unwrap().span;
				messages.error(message!("Symbol {:?} is not a type", symbol.name).span(span));
				return None;
			}
		};

		let mut type_args = Vec::with_capacity(arguments.len());
		for argument in arguments {
			type_args.push(self.lookup_type(messages, root_layers, scope, &argument.item)?);
		}

		let user_type = &mut self.user_types[type_index];
		let concrete_index = match &mut user_type.kind {
			UserTypeKind::Struct { shape } => shape.get_or_add_specialization(messages, user_type.span, type_args)?,
		};

		Some(TypeId { index: type_index, specialization: concrete_index })
	}
}

pub struct FunctionStore<'a> {
	shapes: Vec<FunctionShape<'a>>,
}

impl<'a> FunctionStore<'a> {
	pub fn new() -> Self {
		FunctionStore { shapes: Vec::new() }
	}

	fn register_shape(&mut self, shape: FunctionShape<'a>) -> usize {
		let index = self.shapes.len();
		self.shapes.push(shape);
		index
	}
}

pub fn validate_roots<'a>(
	messages: &mut Messages,
	root_layers: &mut RootLayers<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &mut FunctionStore<'a>,
	parsed_files: &[tree::File<'a>],
) {
	create_and_fill_root_types(messages, root_layers, type_store, parsed_files);
	resolve_root_type_inports(messages, root_layers, type_store, parsed_files);
	create_root_functions(messages, root_layers, type_store, function_store, parsed_files);
}

fn create_and_fill_root_types<'a>(
	messages: &mut Messages,
	root_layers: &mut RootLayers<'a>,
	type_store: &mut TypeStore<'a>,
	parsed_files: &[tree::File<'a>],
) {
	for parsed_file in parsed_files {
		let layer = root_layers.create_module_path(&parsed_file.module_path);
		assert_eq!(layer.symbols.len(), 0);

		let block = &parsed_file.block;
		let index = parsed_file.source_file.index;
		messages.set_current_file_index(index);

		let mut symbols = Vec::new();
		let mut scope = Scope {
			//The initial state doesn't matter, we aren't going to drop this scope
			initial_state: FrameState { symbols_len: 0 },
			symbols: &mut symbols,
		};

		create_block_types(messages, type_store, &mut scope, block, true, index);
		let symbols_len = scope.symbols.len();

		std::mem::forget(scope); //Avoid cleaning up symbols
		layer.importable_types_len = symbols_len;
		layer.symbols = symbols;
	}
}

fn resolve_root_type_inports<'a>(
	messages: &mut Messages,
	root_layers: &mut RootLayers<'a>,
	type_store: &mut TypeStore<'a>,
	parsed_files: &[tree::File<'a>],
) {
	for parsed_file in parsed_files {
		let block = &parsed_file.block;
		let index = parsed_file.source_file.index;
		messages.set_current_file_index(index);

		let count = resolve_block_type_imports(messages, root_layers, type_store, block, &parsed_file.module_path);

		let layer = root_layers.create_module_path(&parsed_file.module_path);
		layer.imported_types_len = count;
	}
}

fn create_root_functions<'a>(
	messages: &mut Messages,
	root_layers: &mut RootLayers<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &mut FunctionStore<'a>,
	parsed_files: &[tree::File<'a>],
) {
	for parsed_file in parsed_files {
		let block = &parsed_file.block;
		let index = parsed_file.source_file.index;
		messages.set_current_file_index(index);

		//Yuck
		let mut symbols = root_layers.create_module_path(&parsed_file.module_path).symbols.clone();
		let old_symbols_len = symbols.len();
		let mut scope = Scope {
			initial_state: FrameState { symbols_len: 0 },
			symbols: &mut symbols,
		};

		create_block_functions(messages, root_layers, type_store, function_store, &mut scope, block, index);

		std::mem::forget(scope);
		let layer = root_layers.create_module_path(&parsed_file.module_path);
		layer.importable_functions_len = symbols.len() - old_symbols_len;
		layer.symbols = symbols;
	}
}

//Returns imported count, TODO use named return in Fae
#[must_use]
fn resolve_block_type_imports<'a>(
	messages: &mut Messages,
	root_layers: &mut RootLayers<'a>,
	type_store: &mut TypeStore<'a>,
	block: &tree::Block<'a>,
	module_path: &'a [String],
) -> usize {
	let mut imported_count = 0;

	for statement in &block.statements {
		let using_statement = match statement {
			tree::Statement::Using(using_statement) => using_statement,
			_ => continue,
		};

		let path = &using_statement.item.path_segments.item.segments;
		let found = match root_layers.layer_for_module_path(messages, path) {
			Some(found) if found.symbols.is_empty() => continue,
			Some(found) => found,
			_ => continue,
		};

		let symbols = found.importable_types(type_store).to_vec(); //Yuck
		let layer = root_layers.create_module_path(module_path);

		for symbol in symbols {
			let name = symbol.name;

			if let Some(found) = layer.symbols.iter().find(|s| s.name == name) {
				messages.error(
					message!("Import of duplicate symbol {name:?}")
						.span(using_statement.span)
						.note_if_some("Original symbol here", found.span, found.file_index)
						.note_if_some("Duplicate symbol here", symbol.span, symbol.file_index),
				);
			} else {
				layer.symbols.push(symbol.clone());
				imported_count += 1;
			}
		}
	}

	imported_count
}

fn create_block_types<'a>(
	messages: &mut Messages,
	type_store: &mut TypeStore<'a>,
	scope: &mut Scope<'a, '_>,
	block: &tree::Block<'a>,
	is_root: bool,
	file_index: usize,
) {
	for statement in &block.statements {
		if is_root {
			match statement {
				tree::Statement::Expression(..)
				| tree::Statement::Block(..)
				| tree::Statement::Let(..)
				| tree::Statement::Mut(..)
				| tree::Statement::Return(..) => {
					messages.error(
						message!("{} is not allowed in a root scope", statement.name_and_article())
							.span(statement.span()),
					);
					continue;
				}

				tree::Statement::Using(..)
				| tree::Statement::Struct(..)
				| tree::Statement::Function(..)
				| tree::Statement::Const(..) => {}
			}
		}

		if let tree::Statement::Struct(statement) = statement {
			// let name = statement.name.item;
			//Start off with no fields, they will be added during the next pre-pass
			//so that all types exist in order to populate field types
			// let kind = TypeKind::Struct { fields: Vec::new() };
			// let span = Some(statement.name.span);
			// let symbol = type_store.register_type(name, kind, span, Some(file_index));
			// scope.push_symbol(messages, symbol);
		}
	}
}

fn fill_block_types<'a>(
	messages: &mut Messages,
	type_store: &mut TypeStore<'a>,
	scope: &mut Scope<'a, '_>,
	block: &tree::Block<'a>,
	is_root: bool,
	file_index: usize,
) {
}

fn create_block_functions<'a>(
	messages: &mut Messages,
	root_layers: &mut RootLayers<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &mut FunctionStore<'a>,
	scope: &mut Scope<'a, '_>,
	block: &tree::Block<'a>,
	file_index: usize,
) {
	for statement in &block.statements {
		if let tree::Statement::Function(statement) = statement {
			let parsed_type = &statement.parsed_type.item;
			let single_sement = parsed_type.as_single_segment();

			let mut generics = statement.generics.iter().enumerate();
			let generic = generics.find(|(_, g)| single_sement == Some(g.item));
			let return_type = if let Some(generic) = generic {
				GenericOrTypeId::Generic { index: generic.0 }
			} else {
				match type_store.lookup_type(messages, root_layers, scope, &parsed_type) {
					Some(id) => GenericOrTypeId::TypeId { id },
					None => continue,
				}
			};

			let mut parameters = Vec::new();
			for parameter in &statement.parameters {
				let parsed_type = &parameter.item.parsed_type.item;
				let single_sement = parsed_type.as_single_segment();

				let mut generics = statement.generics.iter().enumerate();
				let generic = generics.find(|(_, g)| single_sement == Some(g.item));
				let param_type = if let Some(generic) = generic {
					GenericOrTypeId::Generic { index: generic.0 }
				} else {
					match type_store.lookup_type(messages, root_layers, scope, &parsed_type) {
						Some(id) => GenericOrTypeId::TypeId { id },
						None => continue,
					}
				};

				let name = parameter.item.name.item;
				parameters.push(ParameterShape { name, param_type });
			}

			let name = statement.name.item;
			let generics = statement.generics.clone();
			let shape = FunctionShape::new(name, generics, parameters, return_type);
			let shape_index = function_store.register_shape(shape);

			let kind = SymbolKind::Function { shape_index };
			let symbol = Symbol {
				name: statement.name.item,
				kind,
				span: Some(statement.name.span),
				file_index: Some(file_index),
			};
			scope.push_symbol(messages, symbol);
		}
	}
}
