use crate::error::*;
use crate::ir::*;
use crate::span::Span;
use crate::tree::{self, BinaryOperator, PathSegments};

#[derive(Debug)]
pub struct Context<'a, 'b> {
	file_index: usize,
	module_path: &'a [String],

	messages: &'b mut Messages<'a>,

	type_store: &'b mut TypeStore<'a>,
	function_store: &'b mut FunctionStore<'a>,
	root_layers: &'b RootLayers<'a>,

	readables: &'b mut Readables<'a>,

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

			readables: &mut *self.readables,

			initial_symbols_len: self.symbols.len(),
			symbols: &mut *self.symbols,
		}
	}

	fn error(&mut self, message: Message) {
		self.messages.error(message);
	}

	fn push_symbol(&mut self, symbol: Symbol<'a>) {
		self.symbols.push_symbol(self.messages, symbol);
	}

	fn push_readable(&mut self, name: tree::Node<&'a str>, type_id: TypeId, kind: ReadableKind) -> usize {
		let readable_index = self.readables.push(name.item, type_id, kind);
		self.push_readable_with_index(name, kind, readable_index);
		readable_index
	}

	fn push_readable_with_index(&mut self, name: tree::Node<&'a str>, kind: ReadableKind, readable_index: usize) {
		let span = Some(name.span);
		let name = name.item;

		let kind = match kind {
			ReadableKind::Const => SymbolKind::Const { readable_index },
			ReadableKind::Let => SymbolKind::Let { readable_index },
			ReadableKind::Mut => SymbolKind::Mut { readable_index },
		};

		self.push_symbol(Symbol { name, kind, span, file_index: Some(self.file_index) })
	}

	fn lookup_symbol(&mut self, path: &PathSegments<'a>) -> Option<Symbol<'a>> {
		self.symbols
			.lookup_symbol(self.messages, self.root_layers, self.type_store, path)
	}

	fn lookup_type(&mut self, parsed_type: &tree::Type<'a>) -> Option<TypeId> {
		self.type_store
			.lookup_type(self.messages, self.root_layers, self.symbols, parsed_type)
	}

	fn type_name(&self, type_id: TypeId) -> String {
		self.type_store.type_name(self.module_path, type_id)
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

	fn layer_for_module_path(&self, messages: &mut Messages, path: &PathSegments) -> Option<&RootLayer<'a>> {
		assert!(path.len() > 0);
		let mut layers = &self.layers;

		for (piece_index, piece) in path.segments.iter().enumerate() {
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

	fn lookup_path_symbol(&self, messages: &mut Messages, path: &PathSegments<'a>) -> Option<Symbol<'a>> {
		assert!(!path.is_empty());
		let layer = self.layer_for_module_path(messages, &path)?;
		layer.lookup_root_symbol(messages, &[*path.segments.last().unwrap()])
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
		// TODO: Allow duplicate symbole when symbol is variable
		if let Some(found) = self.symbols.iter().find(|s| s.name == symbol.name) {
			// `symbol.span` should only be None for builtin types, yes it's a hack, shush
			messages.error(
				message!("Duplicate symbol {:?}", symbol.name)
					.span_if_some(symbol.span)
					.note_if_some(found.span, found.file_index, "Original symbol here"),
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
		path: &PathSegments<'a>,
	) -> Option<Symbol<'a>> {
		assert!(!path.is_empty());

		if let [segment] = path.segments.as_slice() {
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
			root_layers.lookup_path_symbol(messages, path)
		}
	}
}

#[derive(Debug, Clone)]
pub struct Readables<'a> {
	readables: Vec<Readable<'a>>,
}

impl<'a> Readables<'a> {
	fn new() -> Self {
		Readables { readables: Vec::new() }
	}

	pub fn push(&mut self, name: &'a str, type_id: TypeId, kind: ReadableKind) -> usize {
		let index = self.readables.len();
		self.readables.push(Readable { name, type_id, kind });
		index
	}

	fn get(&mut self, index: usize) -> Option<Readable<'a>> {
		self.readables.get(index).copied()
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
	slice_specializations: Vec<usize>,

	pub u32_type_id: TypeId,
	pub u64_type_id: TypeId,
	pub f64_type_id: TypeId,
	pub string_type_id: TypeId,
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
		let u32_type_index = primatives.next_index;
		primative_type_symbols.push(primatives.push("u32", PrimativeKind::U32));
		let u64_type_index = primatives.next_index;
		primative_type_symbols.push(primatives.push("u64", PrimativeKind::U64));

		primative_type_symbols.push(primatives.push("f16", PrimativeKind::F16));
		primative_type_symbols.push(primatives.push("f32", PrimativeKind::F32));
		let f64_type_index = primatives.next_index;
		primative_type_symbols.push(primatives.push("f64", PrimativeKind::F64));

		let reference_type_index = primatives.next_type_index();
		let slice_type_index = primatives.next_type_index();
		let u32_type_id = TypeId { index: u32_type_index, specialization: 0 };
		let u64_type_id = TypeId { index: u64_type_index, specialization: 0 };
		let f64_type_id = TypeId { index: f64_type_index, specialization: 0 };
		let string_type_id = TypeId { index: slice_type_index, specialization: u8_type_index };

		TypeStore {
			primatives,
			primative_type_symbols,
			user_types: Vec::new(),
			void_type_id,
			reference_type_index,
			slice_type_index,
			slice_specializations: Vec::new(),
			u32_type_id,
			u64_type_id,
			f64_type_id,
			string_type_id,
		}
	}

	pub fn primative_len(&self) -> usize {
		self.primatives.len()
	}

	pub fn user_types(&self) -> &[UserType] {
		&self.user_types
	}

	pub fn slice_type_index(&self) -> usize {
		self.slice_type_index
	}

	pub fn slice_specializations(&self) -> &[usize] {
		&self.slice_specializations
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
				assert!(inner_id.index < u32::MAX as usize, "{}", inner_id.index);
				assert!(inner_id.specialization < u32::MAX as usize, "{}", inner_id.specialization);

				let index = self.slice_type_index;
				let specialization = inner_id.index | inner_id.specialization << 4 * 8;
				self.slice_specializations.push(specialization);
				return Some(TypeId { index, specialization });
			}

			tree::Type::Path { segments, arguments } => (segments, arguments),
		};

		assert!(!segments.segments.is_empty());
		let symbol = symbols.lookup_symbol(messages, root_layers, self, &segments)?;

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

	pub fn type_name(&self, module_path: &'a [String], type_id: TypeId) -> String {
		if type_id.index == self.reference_type_index {
			let type_id = Self::unpack_ref_slice_specialization(type_id.specialization);
			return format!("`&{}`", self.type_name(module_path, type_id));
		}

		if type_id.index == self.slice_type_index() {
			let type_id = Self::unpack_ref_slice_specialization(type_id.specialization);
			return format!("`&[{}]`", self.type_name(module_path, type_id));
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

					format!("`{type_module_path}{}{generics}`", shape.name)
				}
			}
		} else {
			assert!(type_id.specialization == 0, "{}", type_id.specialization);
			format!("`{}`", self.primatives.primatives[type_id.index].name)
		}
	}

	pub fn is_reference(&self, type_id: TypeId) -> bool {
		type_id.index == self.reference_type_index
	}

	pub fn unpack_ref_slice_specialization(specialization: usize) -> TypeId {
		let index = 0xFFFFFFFF & specialization;
		let specialization = specialization >> 4 * 8;
		TypeId { index, specialization }
	}

	pub fn primative(&self, type_id: TypeId) -> Option<PrimativeType> {
		self.primatives.primatives.get(type_id.index).copied()
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

	pub fn shapes(&self) -> &[FunctionShape] {
		&self.shapes
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
	parsed_files: &'a [tree::File<'a>],
) {
	create_and_fill_root_types(messages, root_layers, type_store, parsed_files);
	resolve_root_type_imports(messages, root_layers, parsed_files);

	create_root_functions(messages, root_layers, type_store, function_store, parsed_files);
	resolve_root_function_imports(messages, root_layers, parsed_files);

	let mut readables = Readables::new();
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
			readables: &mut readables,
			initial_symbols_len: symbols.len(),
			symbols: &mut symbols,
		};

		validate_block(context, &parsed_file.block, true);
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
	parsed_files: &'a [tree::File<'a>],
) {
	for parsed_file in parsed_files {
		let block = &parsed_file.block;
		let index = parsed_file.source_file.index;
		messages.set_current_file_index(index);

		//Yuck, I do not like this
		let mut symbols = root_layers.create_module_path(&parsed_file.module_path).symbols.clone();
		let old_symbols_len = symbols.len();

		create_block_functions(
			messages,
			type_store,
			function_store,
			root_layers,
			&mut symbols,
			parsed_file.module_path,
			block,
			index,
		);

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

		let path = &using_statement.item.path_segments.item;
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

		let path = &using_statement.item.path_segments.item;
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
				| tree::Statement::Binding(..)
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
			let shape = StructShape::new(statement.name.item, statement.generics.clone());
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
			let type_index = symbols
				.symbols
				.iter()
				.find_map(|symbol| {
					if let SymbolKind::Type { type_index } = symbol.kind {
						if symbol.name == statement.name.item {
							return Some(type_index);
						}
					}
					None
				})
				.unwrap();

			for field in &statement.fields {
				let generic_type = match &field.parsed_type.item {
					tree::Type::Path { segments, arguments } if segments.len() == 1 && arguments.is_empty() => {
						let segment = segments.segments[0];

						let user_type = &type_store.user_types[type_index - type_store.primatives.len()];
						let struct_shape = match &user_type.kind {
							UserTypeKind::Struct { shape } => shape,
							// _ => unreachable!("{:?}", user_type.kind),
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
					// _ => unreachable!("{:?}", user_type.kind),
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
	module_path: &'a [String],
	block: &'a tree::Block<'a>,
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
				let parameter_type = if let Some(generic) = generic {
					GenericOrTypeId::Generic { index: generic.0 }
				} else {
					match type_store.lookup_type(messages, root_layers, symbols, &parsed_type) {
						Some(id) => GenericOrTypeId::TypeId { id },
						None => continue,
					}
				};

				let name = parameter.item.name;
				let is_mutable = parameter.item.is_mutable;
				parameters.push(ParameterShape { name, parameter_type, is_mutable });
			}

			let name = statement.name;
			let generics = statement.generics.clone();
			let block = &statement.block.item;
			let shape = FunctionShape::new(name, module_path, file_index, generics, parameters, return_type, block);

			let name = statement.name.item;
			let shape_index = function_store.register_shape(shape);
			let kind = SymbolKind::Function { shape_index };
			let span = Some(statement.name.span);
			let symbol = Symbol { name, kind, span, file_index: Some(file_index) };
			symbols.push_symbol(messages, symbol);
		}
	}
}

fn validate_block<'a>(mut context: Context<'a, '_>, block: &'a tree::Block<'a>, is_root: bool) -> Block<'a> {
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
			context.module_path,
			block,
			context.file_index,
		);
		resolve_block_function_imports(context.messages, context.root_layers, context.symbols, block);
	}

	let mut statements = Vec::with_capacity(block.statements.len());

	for statement in &block.statements {
		match statement {
			tree::Statement::Expression(..)
			| tree::Statement::Block(..)
			| tree::Statement::Binding(..)
			| tree::Statement::Return(..)
				if is_root => {} // `is_root` is true, then we've already emitted a message in the root pre-process step, skip

			tree::Statement::Expression(statement) => {
				let expression = match validate_expression(&mut context, &statement) {
					Some(expression) => expression,
					None => continue,
				};

				let type_id = expression.type_id;
				let kind = StatementKind::Expression(expression);
				statements.push(Statement { type_id, kind });
			}

			tree::Statement::Block(..) => unimplemented!("tree::Statement::Block"),

			tree::Statement::Using(..) => {}

			tree::Statement::Struct(statement) => validate_non_generic_struct(&mut context, statement),

			tree::Statement::Function(statement) => validate_non_generic_function(&mut context, statement),

			tree::Statement::Const(statement) => {
				let validated = match validate_const(&mut context, statement) {
					Some(validated) => validated,
					None => continue,
				};

				let type_id = validated.type_id;
				let kind = StatementKind::Const(Box::new(validated));
				statements.push(Statement { type_id, kind });
			}

			tree::Statement::Binding(statement) => {
				let validated = match validate_binding(&mut context, statement) {
					Some(validated) => validated,
					None => continue,
				};

				let type_id = validated.type_id;
				let kind = StatementKind::Binding(Box::new(validated));
				statements.push(Statement { type_id, kind });
			}

			tree::Statement::Return(statement) => {
				let expression = statement.item.expression.as_ref();
				let span = match &expression {
					Some(expression) => statement.span + expression.span,
					None => statement.span,
				};

				let expression = expression.and_then(|expression| validate_expression(&mut context, expression));
				let type_id = match &expression {
					Some(expression) => expression.type_id,
					None => context.type_store.void_type_id,
				};

				let boxed_return = Box::new(Return { span, expression });
				let kind = StatementKind::Return(boxed_return);
				statements.push(Statement { type_id, kind })
			}
		}
	}

	// TODO: Add `give` keywork and support block expressions
	let type_id = context.type_store.void_type_id;
	Block { type_id, statements }
}

fn validate_non_generic_struct<'a>(context: &mut Context<'a, '_>, statement: &'a tree::Struct<'a>) {
	if statement.generics.is_empty() {
		let type_index = context
			.symbols
			.symbols
			.iter()
			.find_map(|symbol| {
				if let SymbolKind::Type { type_index } = symbol.kind {
					if symbol.name == statement.name.item {
						return Some(type_index);
					}
				}
				None
			})
			.unwrap();

		let primative_len = context.type_store.primatives.len();
		let user_type = &mut context.type_store.user_types[type_index - primative_len];
		let shape = match &mut user_type.kind {
			UserTypeKind::Struct { shape } => shape,
		};

		shape.get_or_add_specialization(context.messages, Span::zero(), Vec::new());
	}
}

fn validate_non_generic_function<'a>(context: &mut Context<'a, '_>, statement: &'a tree::Function<'a>) {
	if statement.generics.is_empty() {
		let shape_index = context
			.symbols
			.symbols
			.iter()
			.find_map(|symbol| {
				if let SymbolKind::Function { shape_index } = symbol.kind {
					if symbol.name == statement.name.item {
						return Some(shape_index);
					}
				}
				None
			})
			.unwrap();

		validate_function(context, shape_index, statement.name.span, Vec::new(), &[]);
	}
}

struct ValidatedFunction {
	type_id: TypeId,
	function_id: FunctionId,
}

fn validate_function<'a>(
	context: &mut Context<'a, '_>,
	shape_index: usize,
	invoke_span: Span,
	type_arguments: Vec<TypeId>,
	arguments: &[Expression],
) -> Option<ValidatedFunction> {
	let shape = &mut context.function_store.shapes[shape_index];
	let name_span = shape.name.span;
	let tree_block = shape.block;
	let index = shape.get_or_add_specialization(context.messages, context.readables, invoke_span, type_arguments)?;

	let specialized = &shape.concrete[index];
	let type_id = specialized.return_type;
	if specialized.block.is_some() {
		// This specialization has already been validated
		let function_id = FunctionId { shape_index, specialization_index: index };
		return Some(ValidatedFunction { type_id, function_id });
	}

	let parameters = specialized.parameters.clone();
	if arguments.len() != parameters.len() {
		let message = message!("Expected {} arguments but got {}", parameters.len(), arguments.len());
		context.error(message.span(invoke_span));
	}

	for (index, parameter) in parameters.iter().enumerate() {
		if let Some(argument) = arguments.get(index) {
			if parameter.type_id != argument.type_id {
				context.error(
					message!(
						"Argument type mismatch, expected {} but got {}",
						context.type_name(parameter.type_id),
						context.type_name(argument.type_id)
					)
					.span(argument.span),
				);
			}
		}
	}

	let mut child = context.child_scope();
	for parameter in &parameters {
		let kind = match parameter.is_mutable {
			true => ReadableKind::Mut,
			false => ReadableKind::Let,
		};
		child.push_readable_with_index(parameter.name, kind, parameter.readable_index);
	}

	let block = validate_block(child, tree_block, false);
	let traced = trace_return(context, &block);

	// Don't love this chunk of logic
	if type_id == context.type_store.void_type_id {
		// Expects void return
		if let Some(traced) = traced {
			if traced.type_id != context.type_store.void_type_id {
				context.error(message!("Return of value from void function").span(traced.span));
				return None;
			}
		}
	} else {
		// Expects non-void return
		if let Some(traced) = traced {
			if traced.type_id != type_id {
				context.error(message!(
					"Function expects return type of {} but body returns type {}",
					context.type_name(type_id),
					context.type_name(traced.type_id),
				));
				return None;
			}
		} else {
			context.error(message!("Not all code paths return a value").span(name_span));
			return None;
		}
	}

	let shape = &mut context.function_store.shapes[shape_index];
	let concrete = &mut shape.concrete[index];
	assert!(concrete.block.is_none());
	concrete.block = Some(block);

	let function_id = FunctionId { shape_index, specialization_index: index };
	Some(ValidatedFunction { type_id, function_id })
}

struct TracedReturn {
	span: Span,
	type_id: TypeId,
}

// TODO: Update once flow control gets added
// See https://github.com/ForLoveOfCats/Mountain/blob/OriginalC/compiler/validator.c#L1007
fn trace_return(context: &Context, block: &Block) -> Option<TracedReturn> {
	for statement in &block.statements {
		if let StatementKind::Return(statement) = &statement.kind {
			let type_id = match &statement.expression {
				Some(expression) => expression.type_id,
				None => context.type_store.void_type_id,
			};

			let span = statement.span;
			return Some(TracedReturn { span, type_id });
		}
	}

	None
}

fn validate_const<'a>(context: &mut Context<'a, '_>, statement: &'a tree::Node<tree::Const<'a>>) -> Option<Const<'a>> {
	let explicit_type = match &statement.item.parsed_type {
		Some(parsed_type) => context.lookup_type(&parsed_type.item),
		None => None,
	};
	let expression = validate_expression(context, &statement.item.expression)?;
	let type_id = explicit_type.unwrap_or(expression.type_id);

	if let Some(explicit_type) = explicit_type {
		if explicit_type != expression.type_id {
			context.error(
				message!(
					"Const type mismatch between explicit type {} and expression type {}",
					context.type_store.type_name(context.module_path, explicit_type),
					context.type_store.type_name(context.module_path, expression.type_id),
				)
				.span(statement.span),
			);
		}
	}

	Some(Const { name: statement.item.name.item, type_id, expression })
}

fn validate_binding<'a>(
	context: &mut Context<'a, '_>,
	statement: &'a tree::Node<tree::Binding<'a>>,
) -> Option<Binding<'a>> {
	let expression = validate_expression(context, &statement.item.expression)?;
	if let Some(parsed_type) = &statement.item.parsed_type {
		let type_id = context.lookup_type(&parsed_type.item)?;
		if type_id != expression.type_id {
			context.error(
				message!(
					"Expression type {} mismatch with explicit binding type {}",
					context.type_name(expression.type_id),
					context.type_name(type_id)
				)
				.span(statement.item.expression.span),
			);
			return None;
		}
	};

	let is_mutable = statement.item.is_mutable;
	let kind = match is_mutable {
		true => ReadableKind::Mut,
		false => ReadableKind::Let,
	};
	let readable_index = context.push_readable(statement.item.name, expression.type_id, kind);

	let name = statement.item.name.item;
	let type_id = expression.type_id;
	Some(Binding { name, type_id, expression, readable_index, is_mutable })
}

fn validate_expression<'a>(
	context: &mut Context<'a, '_>,
	expression: &'a tree::Node<tree::Expression<'a>>,
) -> Option<Expression<'a>> {
	let span = expression.span;

	let expression = match &expression.item {
		tree::Expression::Block(block) => {
			let validated_block = validate_block(context.child_scope(), &block, false);
			let type_id = validated_block.type_id;
			Expression { span, type_id, kind: ExpressionKind::Block(validated_block) }
		}

		tree::Expression::IntegerLiteral(literal) => {
			let kind = ExpressionKind::IntegerLiteral(IntegerLiteral { value: literal.value.item });
			Expression { span, type_id: context.type_store.u64_type_id, kind }
		}

		tree::Expression::FloatLiteral(literal) => {
			let kind = ExpressionKind::FloatLiteral(FloatLiteral { value: literal.value.item });
			Expression { span, type_id: context.type_store.f64_type_id, kind }
		}

		tree::Expression::CodepointLiteral(literal) => {
			let kind = ExpressionKind::CodepointLiteral(CodepointLiteral { value: literal.value.item });
			Expression { span, type_id: context.type_store.u32_type_id, kind }
		}

		tree::Expression::StringLiteral(literal) => {
			let kind = ExpressionKind::StringLiteral(StringLiteral { value: literal.value.item });
			Expression { span, type_id: context.type_store.string_type_id, kind }
		}

		tree::Expression::StructLiteral(_) => unimplemented!("tree::Expression::StructLiteral"),

		tree::Expression::Call(call) => {
			let symbol = context.lookup_symbol(&call.path_segments.item)?;
			let name = symbol.name;
			let shape_index = match symbol.kind {
				SymbolKind::Function { shape_index } => shape_index,

				kind => {
					context.error(message!("Cannot call {kind}").span(call.path_segments.span));
					return None;
				}
			};

			let mut type_arguments = Vec::new();
			for type_argument in &call.type_arguments {
				let type_id = context.lookup_type(&type_argument)?;
				type_arguments.push(type_id);
			}

			let mut arguments = Vec::new();
			for argument in &call.arguments {
				let expression = validate_expression(context, argument)?;
				arguments.push(expression);
			}

			let validated = validate_function(context, shape_index, expression.span, type_arguments, &arguments)?;
			let kind = ExpressionKind::Call(Call { name, function_id: validated.function_id, arguments });
			Expression { span, type_id: validated.type_id, kind }
		}

		tree::Expression::Read(read) => {
			let symbol = context.lookup_symbol(&read.path_segments.item)?;
			let readable_index = match symbol.kind {
				SymbolKind::Let { readable_index } | SymbolKind::Mut { readable_index } => readable_index,

				kind => {
					context.error(message!("Cannot read value from {kind}").span(read.path_segments.span));
					return None;
				}
			};

			let readable = context.readables.get(readable_index)?;
			let kind = ExpressionKind::Read(Read { name: readable.name, readable_index });
			Expression { span, type_id: readable.type_id, kind }
		}

		tree::Expression::UnaryOperation(_) => unimplemented!("tree::Expression::UnaryOperation"),

		tree::Expression::BinaryOperation(operation) => {
			let op = operation.op.item;
			let left = validate_expression(context, &operation.left)?;
			let right = validate_expression(context, &operation.right)?;

			if left.type_id != right.type_id {
				context.error(
					message!("{} type mismatch", op.name())
						.span(span)
						.note(note!(
							operation.left.span,
							context.file_index,
							"Left type {}",
							context.type_name(left.type_id)
						))
						.note(note!(
							operation.right.span,
							context.file_index,
							"Right type {}",
							context.type_name(right.type_id)
						)),
				);
				return None;
			}

			let type_id = match op {
				BinaryOperator::Assign => context.type_store.void_type_id,
				_ => left.type_id,
			};

			let operation = Box::new(BinaryOperation { op, left, right });
			let kind = ExpressionKind::BinaryOperation(operation);
			Expression { span, type_id, kind }
		}
	};

	Some(expression)
}
