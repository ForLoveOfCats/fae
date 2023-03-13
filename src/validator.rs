use crate::error::*;
use crate::ir::*;
use crate::span::Span;
use crate::tree;

#[derive(Debug)]
pub struct Context<'a, 'b> {
	file_index: usize,
	module_path: &'a [String],

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
			module_path: &*self.module_path,

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

	fn lookup_symbol(&mut self, segments: &[tree::Node<&'a str>]) -> Option<Symbol<'a>> {
		self.symbols
			.lookup_symbol(self.messages, self.root_layers, self.type_store, segments)
	}

	fn lookup_type(&mut self, parsed_type: &tree::Type<'a>) -> Option<TypeId> {
		self.type_store
			.lookup_type(self.messages, self.root_layers, self.symbols, parsed_type)
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
	fn new() -> Primatives {
		Primatives { next_index: 0, primatives: Vec::new() }
	}

	fn len(&self) -> usize {
		self.next_index
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

	u64_type_id: TypeId,
	string_type_id: TypeId,
}

impl<'a> TypeStore<'a> {
	pub fn new() -> Self {
		let mut primative_type_symbols = Vec::new();
		let mut primatives = Primatives::new();

		let void_type_id = TypeId { index: primatives.next_index, specialization: 0 };
		primative_type_symbols.push(primatives.push("void", PrimativeKind::Void));

		primative_type_symbols.push(primatives.push("i8", PrimativeKind::I8));
		primative_type_symbols.push(primatives.push("i16", PrimativeKind::I16));
		primative_type_symbols.push(primatives.push("i32", PrimativeKind::I32));
		primative_type_symbols.push(primatives.push("i64", PrimativeKind::I64));

		let u8_type_index = primatives.next_index;
		primative_type_symbols.push(primatives.push("u8", PrimativeKind::U8));
		primative_type_symbols.push(primatives.push("u16", PrimativeKind::U16));
		primative_type_symbols.push(primatives.push("u32", PrimativeKind::U32));
		let u64_type_index = primatives.next_index;
		primative_type_symbols.push(primatives.push("u64", PrimativeKind::U64));

		primative_type_symbols.push(primatives.push("f16", PrimativeKind::F16));
		primative_type_symbols.push(primatives.push("f32", PrimativeKind::F32));
		primative_type_symbols.push(primatives.push("f64", PrimativeKind::F64));

		let reference_type_index = primatives.next_type_index();
		let slice_type_index = primatives.next_type_index();
		let u64_type_id = TypeId { index: u64_type_index, specialization: 0 };
		let string_type_id = TypeId { index: slice_type_index, specialization: u8_type_index };

		TypeStore {
			primatives,
			primative_type_symbols,
			user_types: Vec::new(),
			void_type_id,
			reference_type_index,
			slice_type_index,
			u64_type_id,
			string_type_id,
		}
	}

	#[must_use]
	fn register_type(
		&mut self,
		name: &'a str,
		kind: UserTypeKind<'a>,
		span: Span,
		file_index: Option<usize>,
		module_path: &'a [String],
	) -> Symbol<'a> {
		let type_index = self.user_types.len() + self.primatives.len();
		assert!(type_index < u32::MAX as usize, "{type_index}");
		self.user_types.push(UserType { span, module_path, kind });
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

				let index = self.reference_type_index;
				let specialization = inner_id.index | inner_id.specialization << 4 * 8;
				return Some(TypeId { index, specialization });
			}

			tree::Type::Slice(inner) => {
				let inner_id = self.lookup_type(messages, root_layers, symbols, &inner.item)?;
				dbg!(self.type_name(&[], inner_id));
				assert!(inner_id.index < u32::MAX as usize, "{}", inner_id.index);
				assert!(inner_id.specialization < u32::MAX as usize, "{}", inner_id.specialization);

				let index = self.slice_type_index;
				let specialization = inner_id.index | inner_id.specialization << 4 * 8;
				return Some(TypeId { index, specialization });
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

				return Some(self.primatives.primatives[type_index].type_id);
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

		let user_type = &mut self.user_types[type_index - self.primatives.len()];
		let concrete_index = match &mut user_type.kind {
			UserTypeKind::Struct { shape } => shape.get_or_add_specialization(messages, user_type.span, type_args)?,
		};

		Some(TypeId { index: type_index, specialization: concrete_index })
	}

	fn type_name(&self, module_path: &'a [String], type_id: TypeId) -> String {
		if type_id.index == self.reference_type_index {
			let index = 0xFFFFFFFF & type_id.specialization;
			let specialization = type_id.specialization >> 4 * 8;
			let type_id = TypeId { index, specialization };
			return format!("&{}", self.type_name(module_path, type_id));
		}

		if type_id.index == self.slice_type_index {
			let index = 0xFFFFFFFF & type_id.specialization;
			let specialization = type_id.specialization >> 4 * 8;
			let type_id = TypeId { index, specialization };
			return format!("&[{}]", self.type_name(module_path, type_id));
		}

		if type_id.index >= self.primatives.len() {
			let user_type = &self.user_types[type_id.index - self.primatives.len()];
			match &user_type.kind {
				UserTypeKind::Struct { shape } => {
					let mut type_module_path = user_type.module_path;
					for index in 0..module_path.len().min(module_path.len()) {
						if user_type.module_path[index] == module_path[index] {
							type_module_path = &type_module_path[1..];
						}
					}

					let type_module_path = if user_type.module_path.is_empty() {
						String::new()
					} else {
						format!("{}::", user_type.module_path.join("::"))
					};

					let specialization = &shape.concrete[type_id.specialization];
					let generics = if specialization.type_arguments.is_empty() {
						String::new()
					} else {
						let mut generics = String::from("[");
						for &type_argument in &specialization.type_arguments {
							generics.push_str(&self.type_name(module_path, type_argument));
						}
						generics.push(']');
						generics
					};

					format!("{type_module_path}{}{generics}", shape.name)
				}
			}
		} else {
			assert!(type_id.specialization == 0, "{}", type_id.specialization);
			self.primatives.primatives[type_id.index].name.to_string()
		}
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

pub fn validate<'a>(
	messages: &mut Messages<'a>,
	root_layers: &mut RootLayers<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &mut FunctionStore<'a>,
	parsed_files: &[tree::File<'a>],
) -> Vec<Block<'a>> {
	create_and_fill_root_types(messages, root_layers, type_store, parsed_files);
	resolve_root_type_imports(messages, root_layers, parsed_files);

	create_root_functions(messages, root_layers, type_store, function_store, parsed_files);
	resolve_root_function_imports(messages, root_layers, parsed_files);

	let mut blocks = Vec::new();

	let mut symbols = Symbols::new();
	for parsed_file in parsed_files {
		let file_index = parsed_file.source_file.index;
		messages.set_current_file_index(file_index);
		let module_path = parsed_file.module_path;

		let layer = root_layers.create_module_path(&parsed_file.module_path);
		symbols.duplicate(&layer.symbols);

		let context = Context {
			file_index,
			module_path,
			messages,
			type_store,
			function_store,
			root_layers,
			initial_symbols_len: symbols.len(),
			symbols: &mut symbols,
		};

		blocks.push(validate_block(context, &parsed_file.block, true));
	}

	blocks
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

		create_block_types(messages, type_store, &mut layer.symbols, parsed_file.module_path, block, true, index);
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
	module_path: &'a [String],
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
			let symbol = type_store.register_type(name, kind, span, Some(file_index), module_path);
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

						let user_type = &type_store.user_types[type_index - type_store.primatives.len()];
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

				let user_type = &mut type_store.user_types[type_index - type_store.primatives.len()];
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

fn validate_block<'a>(mut context: Context<'a, '_>, block: &tree::Block<'a>, is_root: bool) -> Block<'a> {
	// Root blocks already have had this done so imports can resolve, don't do it again
	if !is_root {
		create_block_types(
			context.messages,
			context.type_store,
			context.symbols,
			context.module_path,
			block,
			false,
			context.file_index,
		);
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

	let mut statements = Vec::with_capacity(block.statements.len());

	for statement in &block.statements {
		match statement {
			tree::Statement::Expression(..) if !is_root => unimplemented!("tree::Statement::Expression"),
			tree::Statement::Block(..) if !is_root => unimplemented!("tree::Statement::Block"),

			tree::Statement::Using(..) | tree::Statement::Struct(..) => {
				// Already handled as a pre-pass
			}

			tree::Statement::Function(..) => unimplemented!("tree::Statement::Function"),

			tree::Statement::Const(statement) => {
				let validated = Box::new(validate_const(&mut context, statement));
				statements.push(Statement {
					type_id: validated.type_id,
					kind: StatementKind::Const(validated),
				});
			}

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
		}
	}

	// TODO: Add `give` keywork and support block expressions
	let type_id = context.type_store.void_type_id;
	Block { type_id, statements }
}

fn validate_const<'a>(context: &mut Context<'a, '_>, statement: &tree::Node<tree::Const<'a>>) -> Const<'a> {
	let explicit_type = match &statement.item.parsed_type {
		Some(parsed_type) => context.lookup_type(&parsed_type.item),
		None => None,
	};
	let expression = validate_expression(context, &statement.item.expression.item);
	let type_id = explicit_type.unwrap_or(expression.type_id);

	if let Some(explicit_type) = explicit_type {
		if explicit_type != expression.type_id {
			context.messages.error(
				message!(
					"Const type mismatch between explicit type `{}` and expression type `{}`",
					context.type_store.type_name(context.module_path, explicit_type),
					context.type_store.type_name(context.module_path, expression.type_id),
				)
				.span(statement.span),
			);
		}
	}

	Const { name: statement.item.name.item, type_id, expression }
}

fn validate_expression<'a>(context: &mut Context<'a, '_>, expression: &tree::Expression<'a>) -> Expression<'a> {
	match expression {
		tree::Expression::Block(block) => {
			let validated_block = validate_block(context.child_scope(), &block, false);
			Expression {
				type_id: validated_block.type_id,
				kind: ExpressionKind::Block(validated_block),
			}
		}

		tree::Expression::IntegerLiteral(literal) => Expression {
			type_id: context.type_store.u64_type_id,
			kind: ExpressionKind::IntegerLiteral(IntegerLiteral { value: literal.value.item }),
		},

		tree::Expression::FloatLiteral(_) => unimplemented!("tree::Expression::FloatLiteral"),
		tree::Expression::CharLiteral(_) => unimplemented!("tree::Expression::CharLiteral"),

		tree::Expression::StringLiteral(literal) => Expression {
			type_id: context.type_store.string_type_id,
			kind: ExpressionKind::StringLiteral(StringLiteral { value: literal.value.item }),
		},

		tree::Expression::StructLiteral(_) => unimplemented!("tree::Expression::StructLiteral"),
		tree::Expression::Call(_) => unimplemented!("tree::Expression::Call"),
		tree::Expression::Read(_) => unimplemented!("tree::Expression::Read"),
		tree::Expression::UnaryOperation(_) => unimplemented!("tree::Expression::UnaryOperation"),
		tree::Expression::BinaryOperation(_) => unimplemented!("tree::Expression::BinaryOperation"),
	}
}
