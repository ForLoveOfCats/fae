use std::rc::Rc;

use crate::error::*;
use crate::ir::*;
use crate::span::Span;
use crate::tree::Node;
use crate::tree::{self, BinaryOperator, PathSegments};
use crate::type_store::*;

#[derive(Debug)]
pub struct Context<'a, 'b> {
	file_index: usize,
	module_path: &'a [String],

	messages: &'b mut Messages<'a>,

	type_store: &'b mut TypeStore<'a>,
	function_store: &'b mut FunctionStore<'a>,
	function_generic_usages: &'b mut Vec<GenericUsage>,

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
			function_generic_usages: &mut *self.function_generic_usages,

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

		let span = Some(name.span);
		let name = name.item;
		let kind = match kind {
			ReadableKind::Const => SymbolKind::Const { readable_index },
			ReadableKind::Let => SymbolKind::Let { readable_index },
			ReadableKind::Mut => SymbolKind::Mut { readable_index },
		};

		self.push_symbol(Symbol { name, kind, span, file_index: Some(self.file_index) });
		readable_index
	}

	fn lookup_symbol(&mut self, path: &PathSegments<'a>) -> Option<Symbol<'a>> {
		self.symbols
			.lookup_symbol(self.messages, self.root_layers, self.type_store, path)
	}

	fn lookup_type(&mut self, parsed_type: &Node<tree::Type<'a>>) -> Option<TypeId> {
		self.type_store.lookup_type(
			self.messages,
			self.function_store,
			self.function_generic_usages,
			self.root_layers,
			self.symbols,
			parsed_type,
		)
	}

	fn type_name(&self, type_id: TypeId) -> String {
		self.type_store
			.type_name(self.function_store, self.module_path, type_id)
	}

	fn collapse_to(&mut self, to: TypeId, from: &mut Expression<'a>) -> Option<bool> {
		self.type_store.collapse_to(self.messages, to, from)
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

	fn child_scope<'s>(&'s mut self) -> SymbolsScope<'a, 's> {
		SymbolsScope { initial_symbols_len: self.len(), symbols: self }
	}

	// TODO: Use named return in Fae
	fn push_symbol(&mut self, messages: &mut Messages, symbol: Symbol<'a>) -> bool {
		// TODO: Allow duplicate symbol when symbol is variable
		if let Some(found) = self.symbols.iter().rev().find(|s| s.name == symbol.name) {
			// `symbol.span` should only be None for builtin types, yes it's a hack, shush
			messages.error(
				message!("Duplicate symbol `{}`", symbol.name)
					.span_if_some(symbol.span)
					.note_if_some(found.span, found.file_index, "Original symbol here"),
			);
			false
		} else {
			self.symbols.push(symbol);
			true
		}
	}

	pub fn lookup_symbol(
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

			if let Some(&found) = self.symbols.iter().rev().find(|symbol| symbol.name == name) {
				return Some(found);
			}

			messages.error(message!("No symbol `{name}` in the current scope").span(segment.span));
			None
		} else {
			root_layers.lookup_path_symbol(messages, path)
		}
	}
}

struct SymbolsScope<'a, 'b> {
	initial_symbols_len: usize,
	symbols: &'b mut Symbols<'a>,
}

impl<'a, 'b> Drop for SymbolsScope<'a, 'b> {
	fn drop(&mut self) {
		self.symbols.symbols.truncate(self.initial_symbols_len);
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
			messages.error(message!("No symbol `{name}` in root of module `{}`", self.name).span(segment.span));
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
pub struct FunctionStore<'a> {
	pub shapes: Vec<FunctionShape<'a>>,

	// Need to have a copy of each shape's generic parameters around before
	// the shape has been fully constructed so signature types can be looked up
	pub generics: Vec<Vec<GenericParameter<'a>>>,

	pub main: Option<FunctionId>,
}

impl<'a> FunctionStore<'a> {
	pub fn new() -> Self {
		FunctionStore { shapes: Vec::new(), generics: Vec::new(), main: None }
	}

	fn get_specialization(
		&self,
		type_store: &TypeStore<'a>,
		function_shape_index: usize,
		type_arguments: &[TypeId],
	) -> Option<FunctionSpecializationResult> {
		let shape = &self.shapes[function_shape_index];
		for (specialization_index, existing) in shape.specializations.iter().enumerate() {
			if type_store.type_arguments_direct_equal(&existing.type_arguments, &type_arguments) {
				return Some(FunctionSpecializationResult { specialization_index, return_type: existing.return_type });
			}
		}

		None
	}

	fn get_or_add_specialization(
		&mut self,
		messages: &mut Messages,
		type_store: &mut TypeStore<'a>,
		generic_usages: &mut Vec<GenericUsage>,
		function_shape_index: usize,
		invoke_span: Option<Span>,
		type_arguments: Vec<TypeId>,
	) -> Option<FunctionSpecializationResult> {
		let shape = &self.shapes[function_shape_index];
		if shape.generics.len() != type_arguments.len() {
			let error = message!("Expected {} type arguments, got {}", shape.generics.len(), type_arguments.len());
			messages.error(error.span_if_some(invoke_span));
			return None;
		}

		if let Some(result) = self.get_specialization(type_store, function_shape_index, &type_arguments) {
			return Some(result);
		}

		let shape = &mut self.shapes[function_shape_index];
		let type_arguments_generic_poisoned = type_arguments
			.iter()
			.any(|id| type_store.type_entries[id.index()].generic_poisoned);

		let parameters = shape
			.parameters
			.iter()
			.map(|parameter| {
				let type_id = type_store.specialize_with_function_generics(
					messages,
					generic_usages,
					function_shape_index,
					&type_arguments,
					parameter.type_id,
				);

				let is_mutable = parameter.is_mutable;
				let readable_index = parameter.readable_index;
				Parameter { name: parameter.name, type_id, readable_index, is_mutable }
			})
			.collect::<Vec<_>>();

		let return_type = type_store.specialize_with_function_generics(
			messages,
			generic_usages,
			function_shape_index,
			&type_arguments,
			shape.return_type,
		);

		let specialization_index = shape.specializations.len();
		let concrete = Function {
			type_arguments: type_arguments.clone(),
			parameters,
			return_type,
			been_queued: false,
			been_generated: false,
		};
		shape.specializations.push(concrete);

		if type_arguments_generic_poisoned {
			let kind = GenericUsageKind::Function { function_shape_index };
			let usage = GenericUsage { type_arguments, kind };
			generic_usages.push(usage)
		} else {
			let function_type_arguments = type_arguments;

			for generic_usage in shape.generic_usages.clone() {
				let mut type_arguments = Vec::with_capacity(generic_usage.type_arguments.len());
				for &type_argument in &generic_usage.type_arguments {
					let type_id = type_store.specialize_with_function_generics(
						messages,
						generic_usages,
						function_shape_index,
						&function_type_arguments,
						type_argument,
					);

					type_arguments.push(type_id);
				}

				match generic_usage.kind {
					GenericUsageKind::UserType { shape_index } => {
						type_store.get_or_add_shape_specialization(
							messages,
							generic_usages,
							shape_index,
							None,
							type_arguments,
						);
					}

					GenericUsageKind::Function { function_shape_index } => {
						self.get_or_add_specialization(
							messages,
							type_store,
							generic_usages,
							function_shape_index,
							invoke_span,
							type_arguments,
						);
					}
				}
			}
		}

		Some(FunctionSpecializationResult { specialization_index, return_type })
	}

	pub fn specialize_with_function_generics(
		&self,
		messages: &mut Messages<'a>,
		type_store: &mut TypeStore<'a>,
		function_id: FunctionId,
		caller_shape_index: usize,
		caller_type_arguments: &[TypeId],
	) -> FunctionId {
		let shape = &self.shapes[function_id.function_shape_index];
		let specialization = &shape.specializations[function_id.specialization_index];
		if specialization.type_arguments.is_empty() {
			return function_id;
		}

		let generic_poisoned = specialization
			.type_arguments
			.iter()
			.any(|id| type_store.type_entries[id.index()].generic_poisoned);
		if !generic_poisoned {
			return function_id;
		}

		let mut type_arguments = Vec::with_capacity(specialization.type_arguments.len());
		let mut generic_usages = Vec::new();
		for &type_argument in &specialization.type_arguments {
			let type_id = type_store.specialize_with_function_generics(
				messages,
				&mut generic_usages,
				caller_shape_index,
				caller_type_arguments,
				type_argument,
			);

			assert!(generic_usages.is_empty());
			type_arguments.push(type_id);
		}

		let result = self
			.get_specialization(type_store, function_id.function_shape_index, &type_arguments)
			.unwrap();
		assert!(generic_usages.is_empty());

		FunctionId {
			function_shape_index: function_id.function_shape_index,
			specialization_index: result.specialization_index,
		}
	}
}

pub fn validate<'a>(
	messages: &mut Messages<'a>,
	root_layers: &mut RootLayers<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &mut FunctionStore<'a>,
	parsed_files: &'a [tree::File<'a>],
) {
	let mut function_generic_usages = Vec::new();

	create_and_fill_root_types(
		messages,
		type_store,
		function_store,
		&mut function_generic_usages,
		root_layers,
		parsed_files,
	);
	resolve_root_type_imports(messages, root_layers, parsed_files);

	let mut readables = Readables::new();
	create_root_functions(
		messages,
		root_layers,
		type_store,
		function_store,
		&mut function_generic_usages,
		&mut readables,
		parsed_files,
	);
	resolve_root_function_imports(messages, root_layers, parsed_files);

	assert!(function_generic_usages.is_empty());
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
			function_generic_usages: &mut function_generic_usages,
			root_layers,
			readables: &mut readables,
			initial_symbols_len: symbols.len(),
			symbols: &mut symbols,
		};

		validate_block(context, &parsed_file.block, true);
	}

	if function_store.main.is_none() {
		messages.error(message!("Missing main function"));
	}
}

fn create_and_fill_root_types<'a>(
	messages: &mut Messages,
	type_store: &mut TypeStore<'a>,
	function_store: &mut FunctionStore<'a>,
	generic_usages: &mut Vec<GenericUsage>,
	root_layers: &mut RootLayers<'a>,
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
		fill_block_types(messages, type_store, function_store, generic_usages, root_layers, &mut symbols, block);
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
	generic_usages: &mut Vec<GenericUsage>,
	readables: &mut Readables<'a>,
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
			generic_usages,
			root_layers,
			readables,
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

			let shape_index = symbols.len();
			let mut generics = Vec::new();
			for (generic_index, &generic) in statement.generics.iter().enumerate() {
				let entry = type_store.type_entries.len() as u32;
				let kind = TypeEntryKind::UserTypeGeneric { shape_index, generic_index };
				type_store.type_entries.push(TypeEntry::new(type_store, kind));
				let generic_type_id = TypeId::new(entry);
				generics.push(GenericParameter { name: generic, generic_type_id });
			}

			let shape = StructShape::new(statement.name.item, generics);
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
	function_store: &mut FunctionStore<'a>,
	generic_usages: &mut Vec<GenericUsage>,
	root_layers: &RootLayers<'a>,
	symbols: &mut Symbols<'a>,
	block: &tree::Block<'a>,
) {
	for statement in &block.statements {
		if let tree::Statement::Struct(statement) = statement {
			let shape_index = symbols
				.symbols
				.iter()
				.rev()
				.find_map(|symbol| {
					if let SymbolKind::Type { shape_index } = symbol.kind {
						if symbol.name == statement.name.item {
							return Some(shape_index);
						}
					}
					None
				})
				.unwrap();

			let shape = match &type_store.user_types[shape_index].kind {
				UserTypeKind::Struct { shape } => shape,
			};

			let scope = symbols.child_scope();
			for (generic_index, generic) in shape.generics.iter().enumerate() {
				let kind = SymbolKind::UserTypeGeneric { shape_index, generic_index };
				let symbol = Symbol {
					name: generic.name.item,
					kind,
					span: Some(generic.name.span),
					file_index: Some(messages.current_file_index()),
				};
				scope.symbols.push_symbol(messages, symbol);
			}

			for field in &statement.fields {
				let field_type = match type_store.lookup_type(
					messages,
					function_store,
					generic_usages,
					root_layers,
					scope.symbols,
					&field.parsed_type,
				) {
					Some(type_id) => type_id,
					None => return,
				};

				let field_shape = FieldShape { name: field.name.item, field_type };
				let span = field.name.span + field.parsed_type.span;
				let node = tree::Node::new(field_shape, span);

				let user_type = &mut type_store.user_types[shape_index];
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
	generic_usages: &mut Vec<GenericUsage>,
	root_layers: &RootLayers<'a>,
	readables: &mut Readables<'a>,
	symbols: &mut Symbols<'a>,
	module_path: &'a [String],
	block: &'a tree::Block<'a>,
	file_index: usize,
) {
	for statement in &block.statements {
		if let tree::Statement::Function(statement) = statement {
			let scope = symbols.child_scope();
			let function_shape_index = function_store.shapes.len();

			let mut generics = Vec::new();
			for (generic_index, &generic) in statement.generics.iter().enumerate() {
				let generic_type_id = type_store.register_function_generic(function_shape_index, generic_index);
				generics.push(GenericParameter { name: generic, generic_type_id });

				let kind = SymbolKind::FunctionGeneric { function_shape_index, generic_index };
				let symbol = Symbol {
					name: generic.item,
					kind,
					span: Some(generic.span),
					file_index: Some(messages.current_file_index()),
				};
				scope.symbols.push_symbol(messages, symbol);
			}

			function_store.generics.push(generics.clone());

			let parsed_type = &statement.parsed_type;
			let return_type = type_store.lookup_type(
				messages,
				function_store,
				generic_usages,
				root_layers,
				scope.symbols,
				&parsed_type,
			);

			let return_type = match return_type {
				Some(type_id) => type_id,
				None => continue,
			};

			let mut parameters = Vec::new();
			for parameter in &statement.parameters {
				let type_id = type_store.lookup_type(
					messages,
					function_store,
					generic_usages,
					root_layers,
					scope.symbols,
					&parameter.item.parsed_type,
				);

				let type_id = match type_id {
					Some(type_id) => type_id,
					None => continue,
				};

				let is_mutable = parameter.item.is_mutable;
				let readable_kind = match is_mutable {
					false => ReadableKind::Let,
					true => ReadableKind::Mut,
				};

				let name = parameter.item.name.item;
				let readable_index = readables.push(name, type_id, readable_kind);

				let name = parameter.item.name;
				parameters.push(ParameterShape { name, type_id, is_mutable, readable_index });
			}

			drop(scope);
			let shape = FunctionShape::new(statement.name, module_path, file_index, generics, parameters, return_type);
			function_store.shapes.push(shape);

			let name = statement.name.item;
			let kind = SymbolKind::Function { function_shape_index };
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
		fill_block_types(
			context.messages,
			context.type_store,
			context.function_store,
			context.function_generic_usages,
			context.root_layers,
			context.symbols,
			block,
		);
		_ = resolve_block_type_imports(context.messages, context.root_layers, context.symbols, block);

		create_block_functions(
			context.messages,
			context.type_store,
			context.function_store,
			context.function_generic_usages,
			context.root_layers,
			context.readables,
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

			tree::Statement::Struct(..) => {}

			tree::Statement::Function(statement) => validate_function(&mut context, statement),

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
					None => context.type_store.void_type_id(),
				};

				let boxed_return = Box::new(Return { span, expression });
				let kind = StatementKind::Return(boxed_return);
				statements.push(Statement { type_id, kind })
			}
		}
	}

	// TODO: Add `give` keywork and support block expressions
	// Make sure to disallow in root of function
	let type_id = context.type_store.void_type_id();
	Block { type_id, statements }
}

fn validate_function<'a>(context: &mut Context<'a, '_>, statement: &'a tree::Function<'a>) {
	let function_shape_index = context.symbols.symbols.iter().rev().find_map(|symbol| {
		if let SymbolKind::Function { function_shape_index: shape_index } = symbol.kind {
			if symbol.name == statement.name.item {
				return Some(shape_index);
			}
		}
		None
	});

	let function_shape_index = match function_shape_index {
		Some(function_shape_index) => function_shape_index,
		None => return,
	};

	let mut scope = context.child_scope();
	let initial_generic_usages_len = scope.function_generic_usages.len();

	let generics = &scope.function_store.shapes[function_shape_index].generics.clone();
	for (generic_index, generic) in generics.into_iter().enumerate() {
		let kind = SymbolKind::FunctionGeneric { function_shape_index, generic_index };
		let symbol = Symbol {
			name: generic.name.item,
			kind,
			span: Some(generic.name.span),
			file_index: Some(scope.file_index),
		};
		scope.push_symbol(symbol);
	}

	for (index, parameter) in statement.parameters.iter().enumerate() {
		let span = parameter.span;
		let parameter = &parameter.item;

		let shape = &scope.function_store.shapes[function_shape_index];
		let parameter_shape = &shape.parameters[index];

		let readable_index = parameter_shape.readable_index;
		let kind = match parameter.is_mutable {
			false => SymbolKind::Let { readable_index },
			true => SymbolKind::Mut { readable_index },
		};

		let name = parameter.name.item;
		let file_index = Some(scope.file_index);
		scope.push_symbol(Symbol { name, kind, span: Some(span), file_index });
	}

	let type_id = scope.lookup_type(&statement.parsed_type);
	let mut block = validate_block(scope, &statement.block.item, false);
	if let Some(type_id) = type_id {
		block.type_id = type_id;
	}

	if trace_return(context, block.type_id, &mut block) == TracedReturn::NotCovered {
		let error = message!("Not all code paths for function `{}` return a value", statement.name.item);
		context.error(error.span(statement.name.span));
	}

	let shape = &mut context.function_store.shapes[function_shape_index];
	assert!(shape.block.is_none());
	shape.block = Some(Rc::new(block));

	let generic_usages = context.function_generic_usages[initial_generic_usages_len..].to_vec();
	context.function_generic_usages.truncate(initial_generic_usages_len);

	if !generic_usages.is_empty() && !shape.specializations.is_empty() {
		for specialization in shape.specializations.clone() {
			for generic_usage in &generic_usages {
				let mut type_arguments = Vec::with_capacity(generic_usage.type_arguments.len());
				for &type_argument in &generic_usage.type_arguments {
					let type_id = context.type_store.specialize_with_function_generics(
						context.messages,
						&mut Vec::new(),
						function_shape_index,
						&specialization.type_arguments,
						type_argument,
					);

					type_arguments.push(type_id);
				}

				match generic_usage.kind {
					GenericUsageKind::UserType { shape_index } => {
						context.type_store.get_or_add_shape_specialization(
							context.messages,
							context.function_generic_usages,
							shape_index,
							None,
							type_arguments,
						);
					}

					GenericUsageKind::Function { function_shape_index } => {
						context.function_store.get_or_add_specialization(
							context.messages,
							context.type_store,
							context.function_generic_usages,
							function_shape_index,
							None,
							type_arguments,
						);
					}
				}
			}
		}
	}

	let shape = &mut context.function_store.shapes[function_shape_index];
	assert!(shape.generic_usages.is_empty());
	shape.generic_usages = generic_usages;

	if shape.is_main {
		let result = context.function_store.get_or_add_specialization(
			context.messages,
			context.type_store,
			context.function_generic_usages,
			function_shape_index,
			None,
			Vec::new(),
		);

		if let Some(result) = result {
			if context.function_store.main.is_some() {
				let message = message!("Duplicate main function, this should be impossible");
				context.error(message.span(statement.name.span));
				return;
			}

			let specialization_index = result.specialization_index;
			let function_id = FunctionId { function_shape_index, specialization_index };
			context.function_store.main = Some(function_id);
		}
	}
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum TracedReturn {
	Covered,
	NotCovered,
}

// TODO: Update once flow control gets added
// See https://github.com/ForLoveOfCats/Mountain/blob/OriginalC/compiler/validator.c#L1007
fn trace_return<'a>(context: &mut Context<'a, '_>, return_type: TypeId, block: &mut Block<'a>) -> TracedReturn {
	for statement in &mut block.statements {
		let StatementKind::Return(statement) = &mut statement.kind else {
			continue;
		};

		let matches = match &mut statement.expression {
			Some(expression) => match context.collapse_to(return_type, expression) {
				Some(matches) => matches,
				None => continue,
			},

			None => return_type.is_void(context.type_store),
		};

		if !matches {
			let expected = context.type_name(return_type);
			let got = match &statement.expression {
				Some(expression) => context.type_name(expression.type_id),
				None => context.type_name(context.type_store.void_type_id()),
			};
			let error = message!("Expected return type of {expected}, got {got}");
			context.error(error.span(statement.span));
		}

		return TracedReturn::Covered;
	}

	if return_type.is_void(context.type_store) {
		return TracedReturn::Covered;
	}

	TracedReturn::NotCovered
}

fn validate_const<'a>(context: &mut Context<'a, '_>, statement: &'a tree::Node<tree::Const<'a>>) -> Option<Const<'a>> {
	let explicit_type = match &statement.item.parsed_type {
		Some(parsed_type) => context.lookup_type(&parsed_type),
		None => None,
	};
	let mut expression = validate_expression(context, &statement.item.expression)?;
	let type_id = explicit_type.unwrap_or(expression.type_id);

	if let Some(explicit_type) = explicit_type {
		if !context
			.type_store
			.collapse_to(context.messages, explicit_type, &mut expression)?
		{
			context.error(
				message!(
					"Const type mismatch between explicit type {} and expression type {}",
					context.type_name(explicit_type),
					context.type_name(expression.type_id),
				)
				.span(statement.span),
			);
			return None;
		}
	}

	Some(Const { name: statement.item.name.item, type_id, expression })
}

fn validate_binding<'a>(
	context: &mut Context<'a, '_>,
	statement: &'a tree::Node<tree::Binding<'a>>,
) -> Option<Binding<'a>> {
	let mut expression = validate_expression(context, &statement.item.expression)?;
	if let Some(parsed_type) = &statement.item.parsed_type {
		let explicit_type = context.lookup_type(&parsed_type)?;
		if !context.collapse_to(explicit_type, &mut expression)? {
			context.error(
				message!(
					"Expression type {} mismatch with explicit binding type {}",
					context.type_name(expression.type_id),
					context.type_name(explicit_type)
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

	match &expression.item {
		tree::Expression::Block(block) => {
			let validated_block = validate_block(context.child_scope(), &block, false);
			let type_id = validated_block.type_id;
			return Some(Expression { span, type_id, kind: ExpressionKind::Block(validated_block) });
		}

		tree::Expression::IntegerLiteral(literal) => {
			let value = IntegerValue::new(literal.value.item, literal.value.span);
			let kind = ExpressionKind::IntegerValue(value);
			return Some(Expression { span, type_id: context.type_store.integer_type_id(), kind });
		}

		tree::Expression::FloatLiteral(literal) => {
			let value = DecimalValue::new(literal.value.item, literal.value.span);
			let kind = ExpressionKind::DecimalValue(value);
			return Some(Expression { span, type_id: context.type_store.decimal_type_id(), kind });
		}

		tree::Expression::CodepointLiteral(literal) => {
			let kind = ExpressionKind::CodepointLiteral(CodepointLiteral { value: literal.value.item });
			return Some(Expression { span, type_id: context.type_store.u32_type_id(), kind });
		}

		tree::Expression::StringLiteral(literal) => {
			let kind = ExpressionKind::StringLiteral(StringLiteral { value: literal.value.item });
			return Some(Expression { span, type_id: context.type_store.string_type_id(), kind });
		}

		tree::Expression::StructLiteral(literal) => {
			let type_id = context.lookup_type(&literal.parsed_type)?;
			let type_entry = &context.type_store.type_entries[type_id.index()];
			let (shape_index, specialization_index) = match &type_entry.kind {
				TypeEntryKind::UserType { shape_index, specialization_index } => (*shape_index, *specialization_index),

				_ => {
					let name = context.type_name(type_id);
					let message = message!("Cannot construct type {name} like a struct as it is not a struct");
					context.error(message.span(literal.parsed_type.span));
					return None;
				}
			};

			let user_type = &mut context.type_store.user_types[shape_index];
			let shape = match &mut user_type.kind {
				UserTypeKind::Struct { shape } => shape,
			};

			// Hate this clone
			let fields = shape.specializations[specialization_index].fields.clone();
			let mut fields = fields.iter();

			let mut field_initializers = Vec::new();

			for intializer in &literal.initializer.item.field_initializers {
				let mut expression = validate_expression(context, &intializer.expression)?;

				let field = match fields.next() {
					Some(field) => field,

					None => {
						context.error(message!("Unexpected extra field initalizer").span(intializer.name.span));
						continue;
					}
				};

				if field.name != intializer.name.item {
					context.error(
						message!(
							"Expected initalizer for field `{}`, got `{}` instead",
							field.name,
							intializer.name.item,
						)
						.span(intializer.name.span),
					);
					continue;
				}

				if !context.collapse_to(field.type_id, &mut expression)? {
					context.error(
						message!(
							"Field intializer type mismatch, expected {} but got {} instead",
							context.type_name(field.type_id),
							context.type_name(expression.type_id),
						)
						.span(intializer.name.span + expression.span),
					);
					continue;
				}

				field_initializers.push(FieldInitializer { expression });
			}

			let kind = ExpressionKind::StructLiteral(StructLiteral { type_id, field_initializers });
			return Some(Expression { span, type_id, kind });
		}

		tree::Expression::Call(call) => {
			let symbol = context.lookup_symbol(&call.path_segments.item)?;
			let name = symbol.name;
			let function_shape_index = match symbol.kind {
				SymbolKind::Function { function_shape_index } => function_shape_index,

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

			let results = context.function_store.get_or_add_specialization(
				context.messages,
				context.type_store,
				context.function_generic_usages,
				function_shape_index,
				Some(span),
				type_arguments,
			)?;
			let FunctionSpecializationResult { specialization_index, return_type } = results;

			let mut arguments = Vec::new();
			for argument in &call.arguments {
				let expression = validate_expression(context, argument)?;
				arguments.push(expression);
			}

			let shape = &context.function_store.shapes[function_shape_index];
			let specialization = &shape.specializations[specialization_index];

			// Don't bail immediately with type mismatch, we want to check every argument and the argument count
			let mut arguments_type_mismatch = false;
			for (index, argument) in arguments.iter_mut().enumerate() {
				let parameter = match specialization.parameters.get(index) {
					Some(parameter) => parameter,
					None => break,
				};

				if !context
					.type_store
					.collapse_to(context.messages, parameter.type_id, argument)?
				{
					let error = message!(
						"Expected argument of type {}, got {}",
						context.type_name(parameter.type_id),
						context.type_name(argument.type_id)
					);
					context.messages.error(error.span(argument.span));
					arguments_type_mismatch = true;
				}
			}

			if arguments.len() != specialization.parameters.len() {
				let error = message!("Expected {} arguments, got {}", specialization.parameters.len(), arguments.len());
				context.error(error.span(span));
				return None;
			}

			if arguments_type_mismatch {
				return None;
			}

			let function_id = FunctionId { function_shape_index, specialization_index };
			let kind = ExpressionKind::Call(Call { name, function_id, arguments });
			return Some(Expression { span, type_id: return_type, kind });
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
			let read = Read {
				name: readable.name,
				type_id: readable.type_id,
				readable_index,
			};

			let kind = ExpressionKind::Read(read);
			return Some(Expression { span, type_id: readable.type_id, kind });
		}

		tree::Expression::UnaryOperation(operation) => {
			let op = match operation.op.item {
				tree::UnaryOperator::Negate => UnaryOperator::Negate,
			};

			let mut expression = validate_expression(context, &operation.expression)?;
			match (op, &mut expression.kind) {
				(UnaryOperator::Negate, ExpressionKind::IntegerValue(value)) => {
					value.negate(context.messages, span);
					expression.span = expression.span + span;
					return Some(expression);
				}

				_ => {}
			}

			let type_id = expression.type_id;
			let kind = ExpressionKind::UnaryOperation(Box::new(UnaryOperation { op, expression }));
			return Some(Expression { span, type_id, kind });
		}

		tree::Expression::BinaryOperation(operation) => {
			let op = operation.op.item;

			let mut left = validate_expression(context, &operation.left)?;
			let mut right = validate_expression(context, &operation.right)?;
			let collapsed = context
				.type_store
				.collapse_fair(context.messages, &mut left, &mut right);

			let Some(collapsed) = collapsed else {
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
			};

			if let Some(constant_math_result) = perform_constant_math(context, &left, &right, op) {
				return Some(constant_math_result);
			}

			let type_id = match op {
				BinaryOperator::Assign => context.type_store.void_type_id(),
				_ => collapsed,
			};

			let operation = Box::new(BinaryOperation { op, left, right });
			let kind = ExpressionKind::BinaryOperation(operation);
			return Some(Expression { span, type_id, kind });
		}
	};
}

fn perform_constant_math<'a>(
	context: &mut Context<'a, '_>,
	left: &Expression,
	right: &Expression,
	op: BinaryOperator,
) -> Option<Expression<'a>> {
	if let ExpressionKind::IntegerValue(left) = left.kind {
		let right = match &right.kind {
			ExpressionKind::IntegerValue(right) => *right,
			kind => unreachable!("{kind:?}"),
		};

		let value = match op {
			BinaryOperator::Assign => return None,
			BinaryOperator::Add => left.add(context.messages, right)?,
			BinaryOperator::Sub => left.sub(context.messages, right)?,
			BinaryOperator::Mul => left.mul(context.messages, right)?,
			BinaryOperator::Div => left.div(context.messages, right)?,
		};

		let kind = ExpressionKind::IntegerValue(value);
		let type_id = context.type_store.integer_type_id();
		return Some(Expression { span: value.span(), type_id, kind });
	}

	if let ExpressionKind::DecimalValue(left) = left.kind {
		let right = match &right.kind {
			ExpressionKind::DecimalValue(right) => *right,
			kind => unreachable!("{kind:?}"),
		};

		let value = match op {
			BinaryOperator::Assign => return None,
			BinaryOperator::Add => left.add(right),
			BinaryOperator::Sub => left.sub(right),
			BinaryOperator::Mul => left.mul(right),
			BinaryOperator::Div => left.div(right),
		};

		let kind = ExpressionKind::DecimalValue(value);
		let type_id = context.type_store.decimal_type_id();
		return Some(Expression { span: value.span(), type_id, kind });
	}

	None
}
