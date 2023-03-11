use crate::error::*;
use crate::mir::*;
use crate::span::Span;
use crate::tree;

#[derive(Debug)]
pub struct Context<'a, 'b> {
	file_index: usize,
	messages: &'b mut Messages<'a>,

	type_store: &'b mut TypeStore<'a>,
	function_store: &'b mut FunctionStore<'a>,
	root_layers: &'b RootLayers<'a>,

	initial_symbols_len: usize,
	symbols: &'b mut Symbols<'a>,
}

impl<'a, 'b> Drop for Context<'a, 'b> {
	fn drop(&mut self) {
		self.symbols.symbols.truncate(self.initial_symbols_len);
	}
}

impl<'a, 'b> Context<'a, 'b> {
	fn child_scope<'s>(&'s mut self) -> Context<'a, 's> {
		Context {
			file_index: self.file_index,
			messages: &mut *self.messages,

			type_store: &mut *self.type_store,
			function_store: &mut *self.function_store,
			root_layers: &*self.root_layers,

			initial_symbols_len: self.symbols.len(),
			symbols: &mut *self.symbols,
		}
	}

	fn push_symbol(&mut self, messages: &mut Messages, symbol: Symbol<'a>) {
		self.symbols.push_symbol(messages, symbol);
	}

	fn lookup_symbol(
		&self,
		messages: &mut Messages,
		type_store: &TypeStore<'a>,
		segments: &[tree::Node<&'a str>],
	) -> Option<Symbol<'a>> {
		self.symbols
			.lookup_symbol(messages, &self.root_layers, type_store, segments)
	}
}

#[derive(Debug)]
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

#[derive(Debug, Clone)]
pub struct Symbols<'a> {
	symbols: Vec<Symbol<'a>>,
}

impl<'a> Symbols<'a> {
	fn new() -> Self {
		Symbols { symbols: Vec::new() }
	}

	fn len(&self) -> usize {
		self.symbols.len()
	}

	fn is_empty(&self) -> bool {
		self.symbols.is_empty()
	}

	fn duplicate(&mut self, other: &Symbols<'a>) {
		self.symbols.clear();
		self.symbols.extend_from_slice(&other.symbols);
	}

	// TODO: Use named return in Fae
	fn push_symbol(&mut self, messages: &mut Messages, symbol: Symbol<'a>) -> bool {
		if let Some(found) = self.symbols.iter().find(|s| s.name == symbol.name) {
			// `symbol.span` should only be None for builtin types, yes it's a hack, shush
			messages.error(
				message!("Duplicate symbol {:?}", symbol.name)
					.span_if_some(symbol.span)
					.note_if_some("Original symbol here", found.span, found.file_index),
			);
			false
		} else {
			self.symbols.push(symbol);
			true
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

#[derive(Debug)]
pub struct RootLayer<'a> {
	name: &'a str,
	children: Vec<RootLayer<'a>>,
	symbols: Symbols<'a>,
	importable_types_len: usize,
	imported_types_len: usize,
	importable_functions_len: usize,
}

impl<'a> RootLayer<'a> {
	fn new(name: &'a str) -> Self {
		RootLayer {
			name,
			children: Vec::new(),
			symbols: Symbols::new(),
			importable_types_len: 0,
			imported_types_len: 0,
			importable_functions_len: 0,
		}
	}

	fn lookup_root_symbol(&self, messages: &mut Messages, segments: &[tree::Node<&'a str>]) -> Option<Symbol<'a>> {
		assert_eq!(segments.len(), 1);

		let segment = &segments[0];
		let name = segment.item;
		let found = self.symbols.symbols.iter().find(|symbol| symbol.name == name);

		if found.is_none() {
			messages.error(message!("No symbol named {name:?} in root of module {:?}", self.name).span(segment.span));
		}
		found.copied()
	}

	fn importable_types(&self) -> &[Symbol<'a>] {
		&self.symbols.symbols[0..self.importable_types_len]
	}

	fn importable_functions(&self) -> &[Symbol<'a>] {
		let types_len = self.importable_types_len + self.imported_types_len;
		&self.symbols.symbols[types_len..types_len + self.importable_functions_len]
	}
}

#[derive(Debug)]
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

#[derive(Debug)]
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
		root_layers: &RootLayers<'a>,
		symbols: &Symbols<'a>,
		parsed_type: &tree::Type<'a>,
	) -> Option<TypeId> {
		let (segments, arguments) = match parsed_type {
			tree::Type::Void => return Some(self.void_type_id),

			tree::Type::Reference(inner) => {
				let inner_id = self.lookup_type(messages, root_layers, symbols, &inner.item)?;
				assert!(inner_id.index < u32::MAX as usize, "{}", inner_id.index);
				assert!(inner_id.specialization < u32::MAX as usize, "{}", inner_id.specialization);

				let type_index = self.reference_type_index;
				let concrete_index = inner_id.index | inner_id.specialization << 4 * 8;
				return Some(TypeId { index: type_index, specialization: concrete_index });
			}

			tree::Type::Slice(inner) => {
				let inner_id = self.lookup_type(messages, root_layers, symbols, &inner.item)?;
				assert!(inner_id.index < u32::MAX as usize, "{}", inner_id.index);
				assert!(inner_id.specialization < u32::MAX as usize, "{}", inner_id.specialization);

				let type_index = self.slice_type_index;
				let concrete_index = inner_id.index | inner_id.specialization << 4 * 8;
				return Some(TypeId { index: type_index, specialization: concrete_index });
			}

			tree::Type::Path { segments, arguments } => (segments, arguments),
		};

		assert!(!segments.segments.is_empty());
		let symbol = symbols.lookup_symbol(messages, root_layers, self, &segments.segments)?;

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
			type_args.push(self.lookup_type(messages, root_layers, symbols, &argument.item)?);
		}

		let user_type = &mut self.user_types[type_index];
		let concrete_index = match &mut user_type.kind {
			UserTypeKind::Struct { shape } => shape.get_or_add_specialization(messages, user_type.span, type_args)?,
		};

		Some(TypeId { index: type_index, specialization: concrete_index })
	}
}

#[derive(Debug)]
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
	messages: &mut Messages<'a>,
	root_layers: &mut RootLayers<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &mut FunctionStore<'a>,
	parsed_files: &[tree::File<'a>],
) {
	create_and_fill_root_types(messages, root_layers, type_store, parsed_files);
	resolve_root_type_imports(messages, root_layers, parsed_files);

	create_root_functions(messages, root_layers, type_store, function_store, parsed_files);
	resolve_root_function_imports(messages, root_layers, parsed_files);

	let mut symbols = Symbols::new();
	for parsed_file in parsed_files {
		let file_index = parsed_file.source_file.index;
		messages.set_current_file_index(file_index);

		let layer = root_layers.create_module_path(&parsed_file.module_path);
		symbols.duplicate(&layer.symbols);

		let context = Context {
			file_index,
			messages,
			type_store,
			function_store,
			root_layers,
			initial_symbols_len: symbols.len(),
			symbols: &mut symbols,
		};

		validate_block(context, &parsed_file.block, true);
	}
}

fn validate_block<'a>(context: Context<'a, '_>, block: &tree::Block<'a>, is_root: bool) {
	// Root blocks already have had this done so imports can resolve, don't do it again
	if !is_root {
		create_block_types(context.messages, context.type_store, context.symbols, block, false, context.file_index);
		fill_block_types(context.messages, context.type_store, context.root_layers, context.symbols, block);
		_ = resolve_block_type_imports(context.messages, context.root_layers, context.symbols, block);

		create_block_functions(
			context.messages,
			context.type_store,
			context.function_store,
			context.root_layers,
			context.symbols,
			block,
			context.file_index,
		);
		resolve_block_function_imports(context.messages, context.root_layers, context.symbols, block);
	}

	for statement in &block.statements {
		match statement {
			tree::Statement::Expression(..) if !is_root => unimplemented!("tree::Statement::Expression"),
			tree::Statement::Block(..) if !is_root => unimplemented!("tree::Statement::Block"),
			tree::Statement::Let(..) if !is_root => unimplemented!("tree::Statement::Let"),
			tree::Statement::Mut(..) if !is_root => unimplemented!("tree::Statement::Mut"),
			tree::Statement::Return(..) if !is_root => unimplemented!("tree::Statement::Return"),

			tree::Statement::Expression(..)
			| tree::Statement::Block(..)
			| tree::Statement::Let(..)
			| tree::Statement::Mut(..)
			| tree::Statement::Return(..) => {
				// is_root is true, we've already emitted a message at the root pre-process layer, skip
			}

			tree::Statement::Using(..) => {}

			tree::Statement::Struct(..) => unimplemented!("tree::Statement::Struct"),
			tree::Statement::Function(..) => unimplemented!("tree::Statement::Function"),
			tree::Statement::Const(..) => unimplemented!("tree::Statement::Const"),
		}
	}
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

		create_block_types(messages, type_store, &mut layer.symbols, block, true, index);
		layer.importable_types_len = layer.symbols.len();
	}

	for parsed_file in parsed_files {
		let layer = root_layers.create_module_path(&parsed_file.module_path);

		let block = &parsed_file.block;
		let index = parsed_file.source_file.index;
		messages.set_current_file_index(index);

		// Yuck, I do not like this
		let mut symbols = layer.symbols.clone();
		fill_block_types(messages, type_store, root_layers, &mut symbols, block);
		root_layers.create_module_path(&parsed_file.module_path).symbols = symbols;
	}
}

fn resolve_root_type_imports<'a>(
	messages: &mut Messages,
	root_layers: &mut RootLayers<'a>,
	parsed_files: &[tree::File<'a>],
) {
	for parsed_file in parsed_files {
		let layer = root_layers.create_module_path(&parsed_file.module_path);

		let block = &parsed_file.block;
		let index = parsed_file.source_file.index;
		messages.set_current_file_index(index);

		// Yuck, I do not like this
		let mut symbols = layer.symbols.clone();
		let count = resolve_block_type_imports(messages, root_layers, &mut symbols, block);
		let layer = root_layers.create_module_path(&parsed_file.module_path);
		layer.imported_types_len = count;
		layer.symbols = symbols;
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

		//Yuck, I do not like this
		let mut symbols = root_layers.create_module_path(&parsed_file.module_path).symbols.clone();
		let old_symbols_len = symbols.len();

		create_block_functions(messages, type_store, function_store, root_layers, &mut symbols, block, index);

		let layer = root_layers.create_module_path(&parsed_file.module_path);
		layer.importable_functions_len = symbols.len() - old_symbols_len;
		layer.symbols = symbols;
	}
}

fn resolve_root_function_imports<'a>(
	messages: &mut Messages,
	root_layers: &mut RootLayers<'a>,
	parsed_files: &[tree::File<'a>],
) {
	for parsed_file in parsed_files {
		let layer = root_layers.create_module_path(&parsed_file.module_path);

		let block = &parsed_file.block;
		let index = parsed_file.source_file.index;
		messages.set_current_file_index(index);

		// Yuck, I do not like this
		let mut symbols = layer.symbols.clone();
		resolve_block_function_imports(messages, root_layers, &mut symbols, block);
		root_layers.create_module_path(&parsed_file.module_path).symbols = symbols;
	}
}

//Returns imported count, TODO use named return in Fae
#[must_use]
fn resolve_block_type_imports<'a>(
	messages: &mut Messages,
	root_layers: &RootLayers<'a>,
	symbols: &mut Symbols<'a>,
	block: &tree::Block<'a>,
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

		for &symbol in found.importable_types() {
			if symbols.push_symbol(messages, symbol) {
				imported_count += 1;
			}
		}
	}

	imported_count
}

fn resolve_block_function_imports<'a>(
	messages: &mut Messages,
	root_layers: &RootLayers<'a>,
	symbols: &mut Symbols<'a>,
	block: &tree::Block<'a>,
) {
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

		for &symbol in found.importable_functions() {
			symbols.push_symbol(messages, symbol);
		}
	}
}

fn create_block_types<'a>(
	messages: &mut Messages,
	type_store: &mut TypeStore<'a>,
	symbols: &mut Symbols<'a>,
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
			//Start off with no fields, they will be added during the next pre-pass
			//so that all types exist in order to populate field types
			let shape = StructShape {
				name: statement.name.item,
				generics: statement.generics.clone(),
				fields: Vec::new(),
				concrete: Vec::new(),
			};

			let name = statement.name.item;
			let kind = UserTypeKind::Struct { shape };
			let span = statement.name.span;
			let symbol = type_store.register_type(name, kind, span, Some(file_index));
			symbols.push_symbol(messages, symbol);
		}
	}
}

fn fill_block_types<'a>(
	messages: &mut Messages,
	type_store: &mut TypeStore<'a>,
	root_layers: &RootLayers<'a>,
	symbols: &mut Symbols<'a>,
	block: &tree::Block<'a>,
) {
	for statement in &block.statements {
		if let tree::Statement::Struct(statement) = statement {
			let name = statement.name.item;
			let symbol = symbols.symbols.iter().find(|symbol| symbol.name == name).unwrap();
			let type_index = match symbol.kind {
				SymbolKind::Type { type_index } => type_index,
				_ => unreachable!("{:?}", symbol.kind),
			};

			for field in &statement.fields {
				let generic_type = match &field.parsed_type.item {
					tree::Type::Path { segments, arguments } if segments.len() == 1 && arguments.is_empty() => {
						let segment = segments.segments[0];

						let user_type = &type_store.user_types[type_index];
						let struct_shape = match &user_type.kind {
							UserTypeKind::Struct { shape } => shape,
							_ => unreachable!("{:?}", user_type.kind),
						};

						let index = struct_shape
							.generics
							.iter()
							.enumerate()
							.find(|(_, &g)| g.item == segment.item)
							.map(|(i, _)| i);

						match index {
							Some(index) => Some(GenericOrTypeId::Generic { index }),
							None => None,
						}
					}

					_ => None,
				};

				let field_type = match generic_type {
					Some(field_type) => field_type,

					None => match type_store.lookup_type(messages, root_layers, symbols, &field.parsed_type.item) {
						Some(id) => GenericOrTypeId::TypeId { id },
						None => return,
					},
				};

				let field_shape = FieldShape { name: field.name.item, field_type };
				let span = field.name.span + field.parsed_type.span;
				let node = tree::Node::new(field_shape, span);

				let user_type = &mut type_store.user_types[type_index];
				match &mut user_type.kind {
					UserTypeKind::Struct { shape } => shape.fields.push(node),
					_ => unreachable!("{:?}", user_type.kind),
				};
			}
		}
	}
}

fn create_block_functions<'a>(
	messages: &mut Messages,
	type_store: &mut TypeStore<'a>,
	function_store: &mut FunctionStore<'a>,
	root_layers: &RootLayers<'a>,
	symbols: &mut Symbols<'a>,
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
				match type_store.lookup_type(messages, root_layers, symbols, &parsed_type) {
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
					match type_store.lookup_type(messages, root_layers, symbols, &parsed_type) {
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
			symbols.push_symbol(messages, symbol);
		}
	}
}
