use std::ops::Range;
use std::rc::Rc;

use crate::error::*;
use crate::ir::*;
use crate::span::Span;
use crate::tree::Node;
use crate::tree::{self, BinaryOperator, PathSegments};
use crate::type_store::*;

#[derive(Debug)]
pub struct Context<'a, 'b, 'c> {
	file_index: usize,
	module_path: &'a [String],

	messages: &'b mut Messages<'a>,

	c_include_store: &'b mut CIncludeStore<'a>,
	type_store: &'b mut TypeStore<'a>,
	function_store: &'b mut FunctionStore<'a>,
	function_generic_usages: &'b mut Vec<GenericUsage>,

	root_layers: &'b RootLayers<'a>,

	constants: &'b mut Vec<ConstantValue<'a>>,
	readables: &'b mut Readables<'a>,

	initial_symbols_len: usize,
	function_initial_symbols_len: usize,
	symbols: &'b mut Symbols<'a>,

	generic_parameters: &'c GenericParameters<'a>,
}

impl<'a, 'b, 'c> Drop for Context<'a, 'b, 'c> {
	fn drop(&mut self) {
		self.symbols.symbols.truncate(self.initial_symbols_len);
	}
}

impl<'a, 'b, 'c> Context<'a, 'b, 'c> {
	fn child_scope<'s>(&'s mut self) -> Context<'a, 's, 'c> {
		Context {
			file_index: self.file_index,
			module_path: self.module_path,

			messages: self.messages,

			c_include_store: self.c_include_store,
			type_store: self.type_store,
			function_store: self.function_store,
			function_generic_usages: self.function_generic_usages,

			root_layers: self.root_layers,

			constants: self.constants,
			readables: self.readables,

			initial_symbols_len: self.symbols.len(),
			function_initial_symbols_len: self.function_initial_symbols_len,
			symbols: self.symbols,

			generic_parameters: self.generic_parameters,
		}
	}

	fn child_scope_with_generic_parameters<'s, 't>(
		&'s mut self,
		generic_parameters: &'t GenericParameters<'a>,
	) -> Context<'a, 's, 't> {
		Context {
			file_index: self.file_index,
			module_path: self.module_path,

			messages: self.messages,

			c_include_store: self.c_include_store,
			type_store: self.type_store,
			function_store: self.function_store,
			function_generic_usages: self.function_generic_usages,

			root_layers: self.root_layers,

			constants: self.constants,
			readables: self.readables,

			initial_symbols_len: self.symbols.len(),
			function_initial_symbols_len: self.function_initial_symbols_len,
			symbols: self.symbols,

			generic_parameters,
		}
	}

	fn message(&mut self, message: Message) {
		self.messages.message(message);
	}

	fn push_symbol(&mut self, symbol: Symbol<'a>) {
		self.symbols
			.push_symbol(self.messages, self.function_initial_symbols_len, symbol);
	}

	fn push_readable(&mut self, name: tree::Node<&'a str>, type_id: TypeId, kind: ReadableKind) -> usize {
		let readable_index = self.readables.push(name.item, type_id, kind);

		let span = Some(name.span);
		let name = name.item;
		let kind = match kind {
			ReadableKind::Let => SymbolKind::Let { readable_index },
			ReadableKind::Mut => SymbolKind::Mut { readable_index },
		};

		self.push_symbol(Symbol { name, kind, span });
		readable_index
	}

	fn lookup_symbol(&mut self, path: &PathSegments<'a>) -> Option<Symbol<'a>> {
		self.symbols
			.lookup_symbol(self.messages, self.root_layers, self.type_store, self.function_initial_symbols_len, path)
	}

	fn lookup_type(&mut self, parsed_type: &Node<tree::Type<'a>>) -> Option<TypeId> {
		self.type_store.lookup_type(
			self.messages,
			self.function_store,
			self.module_path,
			self.function_generic_usages,
			self.root_layers,
			self.symbols,
			self.function_initial_symbols_len,
			parsed_type,
		)
	}

	fn type_name(&self, type_id: TypeId) -> String {
		self.type_store.type_name(self.function_store, self.module_path, type_id)
	}

	fn collapse_to(&mut self, to: TypeId, from: &mut Expression<'a>) -> Option<bool> {
		self.type_store.collapse_to(self.messages, to, from)
	}
}

#[derive(Debug)]
pub struct RootLayers<'a> {
	layers: Vec<RootLayer<'a>>,
	root_name: String,
}

impl<'a> RootLayers<'a> {
	pub fn new(root_name: String) -> Self {
		RootLayers { layers: Vec::new(), root_name }
	}

	fn layer_for_module_path(&self, messages: &mut Messages, path: &[Node<&'a str>]) -> Option<&RootLayer<'a>> {
		assert!(!path.is_empty());
		let mut layers = &self.layers;

		for (piece_index, piece) in path.iter().enumerate() {
			let layer = match layers.iter().position(|x| x.name == piece.item) {
				Some(index) => &layers[index],

				None => {
					messages.message(error!("Cannot find module layer for path segment").span(piece.span));
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
		assert!(!path.is_empty());
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
		assert!(path.len() > 1);
		let layer = self.layer_for_module_path(messages, &path.segments[..path.len() - 1])?;
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

	fn push_symbol(&mut self, messages: &mut Messages, function_initial_symbol_len: usize, symbol: Symbol<'a>) {
		// TODO: Allow duplicate symbol when symbol is variable
		if let Some(found) = self.find_local_symbol_matching_name(function_initial_symbol_len, symbol.name) {
			// `symbol.span` should only be None for builtin types, yes it's a hack, shush
			messages.message(
				error!("Duplicate symbol `{}`", symbol.name)
					.span_if_some(symbol.span)
					.note_if_some(found.span, "Original symbol here"),
			);
		} else {
			self.symbols.push(symbol);
		}
	}

	fn push_imported_symbol(
		&mut self,
		messages: &mut Messages,
		function_initial_symbol_len: usize,
		symbol: Symbol<'a>,
		import_span: Span,
	) {
		if let Some(found) = self.find_local_symbol_matching_name(function_initial_symbol_len, symbol.name) {
			messages.message(
				error!("Import conflicts with local symbol `{}`", found.name)
					.span(import_span)
					.note_if_some(found.span, "Local symbol here"),
			);
		} else {
			self.symbols.push(symbol);
		}
	}

	fn find_local_symbol_matching_name(&self, function_initial_symbol_len: usize, name: &str) -> Option<Symbol<'a>> {
		let mut index = self.symbols.len();
		for &symbol in self.symbols.iter().rev() {
			index -= 1;

			if symbol.name == name {
				if index < function_initial_symbol_len {
					match symbol.kind {
						SymbolKind::Function { .. }
						| SymbolKind::Type { .. }
						| SymbolKind::Const { .. }
						| SymbolKind::BuiltinType { .. } => {}

						_ => break,
					}
				}

				return Some(symbol);
			}
		}

		None
	}

	pub fn lookup_symbol(
		&self,
		messages: &mut Messages,
		root_layers: &RootLayers<'a>,
		type_store: &TypeStore<'a>,
		function_initial_symbol_len: usize,
		path: &PathSegments<'a>,
	) -> Option<Symbol<'a>> {
		assert!(!path.is_empty());

		if let [segment] = path.segments.as_slice() {
			let name = segment.item;

			let primatives = &type_store.primative_type_symbols;
			if let Some(&found) = primatives.iter().find(|symbol| symbol.name == name) {
				return Some(found);
			}

			if let Some(found) = self.find_local_symbol_matching_name(function_initial_symbol_len, name) {
				return Some(found);
			}

			messages.message(error!("No symbol `{name}` in the current scope").span(segment.span));
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
	importable_types_range: Range<usize>,
	importable_functions_range: Range<usize>,
	importable_consts_range: Range<usize>,
}

impl<'a> RootLayer<'a> {
	fn new(name: &'a str) -> Self {
		RootLayer {
			name,
			children: Vec::new(),
			symbols: Symbols::new(),
			importable_types_range: 0..0,
			importable_functions_range: 0..0,
			importable_consts_range: 0..0,
		}
	}

	fn lookup_root_symbol(&self, messages: &mut Messages, segments: &[tree::Node<&'a str>]) -> Option<Symbol<'a>> {
		assert_eq!(segments.len(), 1);

		let segment = &segments[0];
		let name = segment.item;
		let found = self.symbols.symbols.iter().find(|symbol| symbol.name == name);

		if found.is_none() {
			messages.message(error!("No symbol `{name}` in root of module `{}`", self.name).span(segment.span));
		}
		found.copied()
	}

	fn importable_types(&self) -> &[Symbol<'a>] {
		&self.symbols.symbols[self.importable_types_range.clone()]
	}

	fn importable_functions(&self) -> &[Symbol<'a>] {
		&self.symbols.symbols[self.importable_functions_range.clone()]
	}

	fn importable_consts(&self) -> &[Symbol<'a>] {
		&self.symbols.symbols[self.importable_consts_range.clone()]
	}
}

#[derive(Debug)]
pub struct CIncludeStore<'a> {
	pub includes: Vec<CInclude<'a>>,
}

impl<'a> CIncludeStore<'a> {
	pub fn new() -> Self {
		CIncludeStore { includes: Vec::new() }
	}

	fn push_system(&mut self, include: &'a str) {
		let include = CInclude::System(include);
		if self.includes.contains(&include) {
			return;
		}

		self.includes.push(include);
	}
}

#[derive(Debug, PartialEq, Eq)]
pub enum CInclude<'a> {
	System(&'a str),
}

#[derive(Debug)]
pub struct FunctionStore<'a> {
	pub shapes: Vec<FunctionShape<'a>>,

	// Need to have a copy of each shape's generic parameters around before
	// the shape has been fully constructed so signature types can be looked up
	pub generics: Vec<GenericParameters<'a>>,

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
		type_arguments: &TypeArguments,
	) -> Option<FunctionSpecializationResult> {
		let shape = &self.shapes[function_shape_index];
		for (specialization_index, existing) in shape.specializations.iter().enumerate() {
			if existing.type_arguments.matches(type_arguments, type_store) {
				return Some(FunctionSpecializationResult { specialization_index, return_type: existing.return_type });
			}
		}

		None
	}

	pub fn get_or_add_specialization(
		&mut self,
		messages: &mut Messages<'a>,
		type_store: &mut TypeStore<'a>,
		module_path: &'a [String],
		generic_usages: &mut Vec<GenericUsage>,
		function_shape_index: usize,
		invoke_span: Option<Span>,
		type_arguments: TypeArguments,
	) -> Option<FunctionSpecializationResult> {
		let shape = &self.shapes[function_shape_index];

		if shape.generics.explicit_len() != type_arguments.explicit_len() {
			let expected = shape.generics.explicit_len();
			let got = type_arguments.explicit_len();
			let error = error!("Expected {expected} type arguments, got {got}");
			messages.message(error.span_if_some(invoke_span));
			return None;
		}

		// TODO: Should this be an ICE instead?
		if shape.generics.implicit_len() != type_arguments.implicit_len() {
			let expected = shape.generics.implicit_len();
			let got = type_arguments.implicit_len();
			let error = error!("Expected {expected} implicit type arguments, got {got}");
			messages.message(error.span_if_some(invoke_span));
			return None;
		}

		if let Some(result) = self.get_specialization(type_store, function_shape_index, &type_arguments) {
			return Some(result);
		}

		let shape = &self.shapes[function_shape_index];
		let type_arguments_generic_poisoned = type_arguments
			.ids()
			.iter()
			.any(|id| type_store.type_entries[id.index()].generic_poisoned);

		let parameters = shape
			.parameters
			.iter()
			.map(|parameter| {
				let type_id = type_store.specialize_with_function_generics(
					messages,
					self,
					module_path,
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
			self,
			module_path,
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
		let shape = &mut self.shapes[function_shape_index];
		shape.specializations.push(concrete);

		if type_arguments_generic_poisoned {
			let usage = GenericUsage::Function { type_arguments, function_shape_index };
			generic_usages.push(usage)
		} else {
			let function_type_arguments = type_arguments;

			for generic_usage in shape.generic_usages.clone() {
				generic_usage.apply_specialization(
					messages,
					type_store,
					self,
					module_path,
					generic_usages,
					function_shape_index,
					&function_type_arguments,
					invoke_span,
				);
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
		caller_type_arguments: &TypeArguments,
	) -> FunctionId {
		let shape = &self.shapes[function_id.function_shape_index];
		let specialization = &shape.specializations[function_id.specialization_index];
		if specialization.type_arguments.is_empty() {
			return function_id;
		}

		let generic_poisoned = specialization
			.type_arguments
			.ids()
			.iter()
			.any(|id| type_store.type_entries[id.index()].generic_poisoned);
		if !generic_poisoned {
			return function_id;
		}

		let mut generic_usages = Vec::new();
		let mut type_arguments = specialization.type_arguments.clone();
		type_arguments.specialize_with_function_generics(
			messages,
			type_store,
			self,
			shape.module_path,
			&mut generic_usages,
			caller_shape_index,
			caller_type_arguments,
		);

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
	c_include_store: &mut CIncludeStore<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &mut FunctionStore<'a>,
	parsed_files: &'a [tree::File<'a>],
) {
	let mut function_generic_usages = Vec::new();
	let mut constants = Vec::new();

	create_root_types(messages, type_store, root_layers, parsed_files);
	resolve_root_type_imports(messages, root_layers, parsed_files);
	fill_root_types(messages, type_store, function_store, root_layers, parsed_files);

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
	function_generic_usages.clear();

	let mut symbols = Symbols::new();
	validate_root_consts(
		messages,
		root_layers,
		c_include_store,
		type_store,
		function_store,
		&mut function_generic_usages,
		&mut constants,
		&mut readables,
		parsed_files,
		&mut symbols,
	);

	for parsed_file in parsed_files {
		let file_index = parsed_file.source_file.index;
		let module_path = parsed_file.module_path;

		let layer = root_layers.create_module_path(module_path);
		symbols.duplicate(&layer.symbols);

		let blank_generic_parameters = GenericParameters::new_from_explicit(Vec::new());
		let context = Context {
			file_index,
			module_path,
			messages,
			c_include_store,
			type_store,
			function_store,
			function_generic_usages: &mut function_generic_usages,
			root_layers,
			constants: &mut constants,
			readables: &mut readables,
			initial_symbols_len: symbols.len(),
			function_initial_symbols_len: symbols.len(),
			symbols: &mut symbols,
			generic_parameters: &blank_generic_parameters,
		};

		validate_block(context, &parsed_file.block, true);
	}

	if function_store.main.is_none() {
		messages.message(error!("Missing main function"));
	}
}

fn create_root_types<'a>(
	messages: &mut Messages,
	type_store: &mut TypeStore<'a>,
	root_layers: &mut RootLayers<'a>,
	parsed_files: &[tree::File<'a>],
) {
	for parsed_file in parsed_files {
		let layer = root_layers.create_module_path(parsed_file.module_path);
		assert_eq!(layer.symbols.len(), 0);

		let block = &parsed_file.block;
		create_block_types(messages, type_store, &mut layer.symbols, 0, parsed_file.module_path, block, true);

		layer.importable_types_range = 0..layer.symbols.len();
	}
}

fn resolve_root_type_imports<'a>(messages: &mut Messages, root_layers: &mut RootLayers<'a>, parsed_files: &[tree::File<'a>]) {
	for parsed_file in parsed_files {
		let layer = root_layers.create_module_path(parsed_file.module_path);
		let mut symbols = layer.symbols.clone(); // Belch

		let block = &parsed_file.block;
		resolve_block_type_imports(messages, root_layers, &mut symbols, 0, block);

		root_layers.create_module_path(parsed_file.module_path).symbols = symbols;
	}
}

fn fill_root_types<'a>(
	messages: &mut Messages<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &mut FunctionStore<'a>,
	root_layers: &mut RootLayers<'a>,
	parsed_files: &[tree::File<'a>],
) {
	for parsed_file in parsed_files {
		let layer = root_layers.create_module_path(parsed_file.module_path);
		let mut symbols = layer.symbols.clone(); // Belch

		// TODO: This is definitely wrong
		let mut generic_usages = Vec::new();

		fill_block_types(
			messages,
			type_store,
			function_store,
			&mut generic_usages,
			root_layers,
			&mut symbols,
			parsed_file.module_path,
			0,
			&parsed_file.block,
		);

		let layer = root_layers.create_module_path(parsed_file.module_path);
		layer.symbols = symbols;
	}
}

fn create_root_functions<'a>(
	messages: &mut Messages<'a>,
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

		//Yuck, I do not like this
		let mut symbols = root_layers.create_module_path(parsed_file.module_path).symbols.clone();
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
			&GenericParameters::new_from_explicit(Vec::new()),
			block,
			index,
		);

		let layer = root_layers.create_module_path(parsed_file.module_path);
		layer.importable_functions_range = old_symbols_len..symbols.len();
		layer.symbols = symbols;
	}
}

fn validate_root_consts<'a>(
	messages: &mut Messages<'a>,
	root_layers: &mut RootLayers<'a>,
	c_include_store: &mut CIncludeStore<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &mut FunctionStore<'a>,
	function_generic_usages: &mut Vec<GenericUsage>,
	constants: &mut Vec<ConstantValue<'a>>,
	readables: &mut Readables<'a>,
	parsed_files: &'a [tree::File<'a>],
	symbols: &mut Symbols<'a>,
) {
	for parsed_file in parsed_files {
		let file_index = parsed_file.source_file.index;
		let module_path = parsed_file.module_path;

		let layer = root_layers.create_module_path(parsed_file.module_path);
		symbols.duplicate(&layer.symbols);
		let old_symbols_len = symbols.len();

		let blank_generic_parameters = GenericParameters::new_from_explicit(Vec::new());
		let mut context = Context {
			file_index,
			module_path,
			messages,
			c_include_store,
			type_store,
			function_store,
			function_generic_usages,
			root_layers,
			constants,
			readables,
			initial_symbols_len: symbols.len(),
			function_initial_symbols_len: symbols.len(),
			symbols,
			generic_parameters: &blank_generic_parameters,
		};

		validate_block_consts(&mut context, &parsed_file.block);
		std::mem::forget(context);

		let layer = root_layers.create_module_path(parsed_file.module_path);
		layer.importable_consts_range = old_symbols_len..symbols.len();
		layer.symbols.duplicate(symbols);
	}
}

fn resolve_block_type_imports<'a>(
	messages: &mut Messages,
	root_layers: &RootLayers<'a>,
	symbols: &mut Symbols<'a>,
	function_initial_symbols_len: usize,
	block: &tree::Block<'a>,
) {
	for statement in &block.statements {
		let import_statement = match statement {
			tree::Statement::Import(import_statement) => import_statement,
			_ => continue,
		};
		let names = &import_statement.item.symbol_names;

		let path = &import_statement.item.path_segments;
		let layer = match root_layers.layer_for_module_path(messages, &path.segments) {
			Some(found) if found.symbols.is_empty() => continue,
			Some(found) => found,
			_ => continue,
		};

		for &importing in layer.importable_types() {
			if let Some(name) = names.iter().find(|n| n.item == importing.name) {
				symbols.push_imported_symbol(messages, function_initial_symbols_len, importing, name.span);
			}
		}
	}
}

fn resolve_block_non_type_imports<'a>(
	messages: &mut Messages,
	root_layers: &RootLayers<'a>,
	symbols: &mut Symbols<'a>,
	function_initial_symbols_len: usize,
	block: &tree::Block<'a>,
) {
	for statement in &block.statements {
		let import_statement = match statement {
			tree::Statement::Import(import_statement) => import_statement,
			_ => continue,
		};
		let names = &import_statement.item.symbol_names;

		let path = &import_statement.item.path_segments;
		let layer = match root_layers.layer_for_module_path(messages, &path.segments) {
			Some(found) if found.symbols.is_empty() => continue,
			Some(found) => found,
			_ => continue,
		};

		for &importing in layer.importable_functions() {
			if let Some(name) = names.iter().find(|n| n.item == importing.name) {
				symbols.push_imported_symbol(messages, function_initial_symbols_len, importing, name.span);
			}
		}

		for &importing in layer.importable_consts() {
			if let Some(name) = names.iter().find(|n| n.item == importing.name) {
				symbols.push_imported_symbol(messages, function_initial_symbols_len, importing, name.span);
			}
		}
	}
}

fn create_block_types<'a>(
	messages: &mut Messages,
	type_store: &mut TypeStore<'a>,
	symbols: &mut Symbols<'a>,
	function_initial_symbols_len: usize,
	module_path: &'a [String],
	block: &tree::Block<'a>,
	is_root: bool,
) {
	for statement in &block.statements {
		if is_root {
			match statement {
				tree::Statement::Expression(..)
				| tree::Statement::Block(..)
				| tree::Statement::Binding(..)
				| tree::Statement::Return(..) => {
					messages.message(
						error!("{} is not allowed in a root scope", statement.name_and_article()).span(statement.span()),
					);
					continue;
				}

				tree::Statement::Import(..)
				| tree::Statement::Struct(..)
				| tree::Statement::Function(..)
				| tree::Statement::Const(..)
				| tree::Statement::CIncludeSystem(..) => {}
			}
		}

		if let tree::Statement::Struct(statement) = statement {
			//Start off with no fields, they will be added during the next pre-pass
			//so that all types exist in order to populate field types

			let shape_index = type_store.user_types.len();
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
			let symbol = type_store.register_type(name, kind, span, module_path);
			symbols.push_symbol(messages, function_initial_symbols_len, symbol);
		}
	}
}

fn fill_block_types<'a>(
	messages: &mut Messages<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &mut FunctionStore<'a>,
	generic_usages: &mut Vec<GenericUsage>,
	root_layers: &RootLayers<'a>,
	symbols: &mut Symbols<'a>,
	module_path: &'a [String],
	function_initial_symbols_len: usize,
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

			let shape = match &mut type_store.user_types[shape_index].kind {
				UserTypeKind::Struct { shape } => shape,
			};

			let scope = symbols.child_scope();
			for (generic_index, generic) in shape.generics.iter().enumerate() {
				let kind = SymbolKind::UserTypeGeneric { shape_index, generic_index };
				let symbol = Symbol { name: generic.name.item, kind, span: Some(generic.name.span) };
				scope.symbols.push_symbol(messages, function_initial_symbols_len, symbol);
			}

			let mut fields = Vec::with_capacity(statement.fields.len());
			for field in &statement.fields {
				let field_type = match type_store.lookup_type(
					messages,
					function_store,
					module_path,
					generic_usages,
					root_layers,
					scope.symbols,
					function_initial_symbols_len,
					&field.parsed_type,
				) {
					Some(type_id) => type_id,
					None => type_store.any_collapse_type_id(),
				};

				let field_shape = FieldShape { name: field.name.item, field_type };
				let span = field.name.span + field.parsed_type.span;
				let node = tree::Node::new(field_shape, span);
				fields.push(node);
			}

			match &mut type_store.user_types[shape_index].kind {
				UserTypeKind::Struct { shape } => {
					shape.fields = fields;
					shape.been_filled = true;

					if !shape.specializations.is_empty() {
						fill_pre_existing_user_type_specializations(
							messages,
							type_store,
							function_store,
							generic_usages,
							module_path,
							shape_index,
						);
					}
				}
			}
		}
	}
}

fn fill_pre_existing_user_type_specializations<'a>(
	messages: &mut Messages<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	generic_usages: &mut Vec<GenericUsage>,
	module_path: &'a [String],
	shape_index: usize,
) {
	let user_type = &mut type_store.user_types[shape_index];
	let span = user_type.span;
	let shape = match &mut user_type.kind {
		UserTypeKind::Struct { shape } => shape,
	};

	let mut fields = Vec::with_capacity(shape.fields.len());
	for field in &shape.fields {
		fields.push(Field {
			span: Some(field.span),
			name: field.item.name,
			type_id: field.item.field_type,
		});
	}

	let mut specializations = shape.specializations.clone(); // Belch
	for specialization in &mut specializations {
		let mut fields = fields.clone();
		for field in &mut fields {
			field.type_id = type_store.specialize_with_user_type_generics(
				messages,
				function_store,
				module_path,
				generic_usages,
				shape_index,
				&specialization.type_arguments,
				field.type_id,
			);
		}

		assert!(!specialization.been_filled);
		specialization.been_filled;
		assert_eq!(specialization.fields.len(), 0);
		specialization.fields = fields;
	}

	match &mut type_store.user_types[shape_index].kind {
		UserTypeKind::Struct { shape } => {
			shape.specializations = specializations;
		}
	}

	let mut type_ids = Vec::new();
	match &type_store.user_types[shape_index].kind {
		UserTypeKind::Struct { shape } => {
			type_ids.reserve(shape.specializations.len());

			for specialization in &shape.specializations {
				let type_id = specialization.type_id;
				let chain = type_store.find_user_type_dependency_chain(type_id, type_id);
				if let Some(chain) = chain {
					report_cyclic_user_type(messages, type_store, function_store, module_path, type_id, chain, span);
				} else {
					type_ids.push(type_id);
				}
			}
		}
	}

	for type_id in type_ids {
		type_store.type_size(type_id);
	}
}

pub fn report_cyclic_user_type<'a>(
	messages: &mut Messages<'a>,
	type_store: &TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	module_path: &'a [String],
	type_id: TypeId,
	chain: Vec<UserTypeChainLink<'a>>,
	invoke_span: Span,
) {
	let name = type_store.type_name(function_store, module_path, type_id);
	let mut error = error!("Cyclic user type {}", name).span(invoke_span);

	for link in chain {
		error = error.note(note!(
			link.field_span,
			"Via field `{}` in {}",
			link.field_name,
			type_store.type_name(function_store, module_path, link.user_type)
		));
	}

	messages.message(error);
}

fn create_block_functions<'a>(
	messages: &mut Messages<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &mut FunctionStore<'a>,
	generic_usages: &mut Vec<GenericUsage>,
	root_layers: &RootLayers<'a>,
	readables: &mut Readables<'a>,
	symbols: &mut Symbols<'a>,
	module_path: &'a [String],
	enclosing_generic_parameters: &GenericParameters<'a>,
	block: &'a tree::Block<'a>,
	file_index: usize,
) {
	for statement in &block.statements {
		if let tree::Statement::Function(statement) = statement {
			let scope = symbols.child_scope();
			let function_initial_symbols_len = scope.symbols.len();
			let function_shape_index = function_store.shapes.len();

			let mut explicit_generics = Vec::new();
			for (generic_index, &generic) in statement.generics.iter().enumerate() {
				let generic_type_id = type_store.register_function_generic(function_shape_index, generic_index);
				explicit_generics.push(GenericParameter { name: generic, generic_type_id });

				let kind = SymbolKind::FunctionGeneric { function_shape_index, generic_index };
				let symbol = Symbol { name: generic.item, kind, span: Some(generic.span) };
				scope.symbols.push_symbol(messages, function_initial_symbols_len, symbol);
			}

			let explicit_generics_len = explicit_generics.len();
			let mut generics = GenericParameters::new_from_explicit(explicit_generics);
			for (index, parent_parameter) in enclosing_generic_parameters.parameters().iter().enumerate() {
				let generic_index = explicit_generics_len + index;
				let generic_type_id = type_store.register_function_generic(function_shape_index, generic_index);
				let parameter = GenericParameter { name: parent_parameter.name, generic_type_id };
				generics.push_implicit(parameter);

				let kind = SymbolKind::FunctionGeneric { function_shape_index, generic_index };
				let span = Some(parent_parameter.name.span);
				let symbol = Symbol { name: parent_parameter.name.item, kind, span };
				scope.symbols.push_symbol(messages, function_initial_symbols_len, symbol);
			}

			function_store.generics.push(generics.clone());

			let return_type = if let Some(parsed_type) = &statement.parsed_type {
				let type_id = type_store.lookup_type(
					messages,
					function_store,
					module_path,
					generic_usages,
					root_layers,
					scope.symbols,
					function_initial_symbols_len,
					parsed_type,
				);

				let return_type = match type_id {
					Some(type_id) => type_id,
					None => continue,
				};

				if return_type.is_void(type_store) {
					let warning = warning!("`void` return type can be omitted");
					messages.message(warning.span(parsed_type.span));
				}

				return_type
			} else {
				type_store.void_type_id()
			};

			let mut parameters = Vec::new();
			for parameter in &statement.parameters {
				let type_id = type_store.lookup_type(
					messages,
					function_store,
					module_path,
					generic_usages,
					root_layers,
					scope.symbols,
					function_initial_symbols_len,
					&parameter.item.parsed_type,
				);

				let type_id = match type_id {
					Some(type_id) => type_id,
					None => type_store.any_collapse_type_id(),
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

			let name = statement.name;
			let is_main = module_path == [root_layers.root_name.as_str()] && name.item == "main";
			let shape = FunctionShape::new(
				name,
				module_path,
				file_index,
				is_main,
				generics,
				statement.extern_name,
				parameters,
				return_type,
			);
			function_store.shapes.push(shape);

			let kind = SymbolKind::Function { function_shape_index };
			let span = Some(statement.name.span);
			let symbol = Symbol { name: name.item, kind, span };
			symbols.push_symbol(messages, function_initial_symbols_len, symbol);
		}
	}
}

fn validate_block_consts<'a>(context: &mut Context<'a, '_, '_>, block: &'a tree::Block<'a>) {
	for statement in &block.statements {
		if let tree::Statement::Const(statement) = statement {
			validate_const(context, statement);
		}
	}
}

fn validate_block<'a>(mut context: Context<'a, '_, '_>, block: &'a tree::Block<'a>, is_root: bool) -> Block<'a> {
	if !is_root {
		create_block_types(
			context.messages,
			context.type_store,
			context.symbols,
			context.function_initial_symbols_len,
			context.module_path,
			block,
			false,
		);

		resolve_block_type_imports(
			context.messages,
			context.root_layers,
			context.symbols,
			context.function_initial_symbols_len,
			block,
		);

		fill_block_types(
			context.messages,
			context.type_store,
			context.function_store,
			context.function_generic_usages,
			context.root_layers,
			context.symbols,
			context.module_path,
			context.function_initial_symbols_len,
			block,
		);
	}

	resolve_block_non_type_imports(
		context.messages,
		context.root_layers,
		context.symbols,
		context.function_initial_symbols_len,
		block,
	);

	if !is_root {
		create_block_functions(
			context.messages,
			context.type_store,
			context.function_store,
			context.function_generic_usages,
			context.root_layers,
			context.readables,
			context.symbols,
			context.module_path,
			context.generic_parameters,
			block,
			context.file_index,
		);
	}

	if !is_root {
		validate_block_consts(&mut context, block);
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
				let expression = validate_expression(&mut context, statement);
				let type_id = expression.type_id;
				let kind = StatementKind::Expression(expression);
				statements.push(Statement { type_id, kind });
			}

			tree::Statement::Block(statement) => {
				let scope = context.child_scope();
				let block = validate_block(scope, &statement.item, false);
				let type_id = block.type_id;
				let kind = StatementKind::Block(block);
				statements.push(Statement { type_id, kind })
			}

			tree::Statement::Import(..) => {}

			tree::Statement::Struct(..) => {}

			tree::Statement::Function(statement) => validate_function(&mut context, statement),

			tree::Statement::Const(..) => {}

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

				let expression = expression.map(|expression| validate_expression(&mut context, expression));
				let type_id = match &expression {
					Some(expression) => expression.type_id,
					None => context.type_store.void_type_id(),
				};

				let boxed_return = Box::new(Return { span, expression });
				let kind = StatementKind::Return(boxed_return);
				statements.push(Statement { type_id, kind })
			}

			tree::Statement::CIncludeSystem(statement) => {
				context.c_include_store.push_system(statement.item);
			}
		}
	}

	// TODO: Add `give` keywork and support block expressions
	// Make sure to disallow in root of function
	let type_id = context.type_store.void_type_id();
	Block { type_id, statements }
}

fn validate_function<'a>(context: &mut Context<'a, '_, '_>, statement: &'a tree::Function<'a>) {
	if statement.extern_name.is_some() {
		// Note: Signature has already been checked in `create_block_functions`
		return;
	}

	let function_shape_index = context.symbols.symbols.iter().rev().find_map(|symbol| {
		if let SymbolKind::Function { function_shape_index } = symbol.kind {
			if symbol.name == statement.name.item {
				return Some(function_shape_index);
			}
		}
		None
	});

	let Some(function_shape_index) = function_shape_index else {
		return;
	};

	let generics = context.function_store.shapes[function_shape_index].generics.clone();
	let mut scope = context.child_scope_with_generic_parameters(&generics);
	scope.function_initial_symbols_len = scope.symbols.len();
	let initial_generic_usages_len = scope.function_generic_usages.len();

	for (generic_index, generic) in generics.parameters().iter().enumerate() {
		let kind = SymbolKind::FunctionGeneric { function_shape_index, generic_index };
		let symbol = Symbol { name: generic.name.item, kind, span: Some(generic.name.span) };
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
		scope.push_symbol(Symbol { name, kind, span: Some(span) });
	}

	let type_id = scope.function_store.shapes[function_shape_index].return_type;
	let block = statement.block.as_ref().unwrap();
	let mut block = validate_block(scope, &block.item, false);

	if trace_return(context, type_id, &mut block) == TracedReturn::NotCovered {
		let error = error!("Not all code paths for function `{}` return a value", statement.name.item);
		context.message(error.span(statement.name.span));
	}

	let shape = &mut context.function_store.shapes[function_shape_index];
	assert!(shape.block.is_none());
	shape.block = Some(Rc::new(block));

	let mut generic_usages = context.function_generic_usages[initial_generic_usages_len..].to_vec();
	context.function_generic_usages.truncate(initial_generic_usages_len);

	if !generic_usages.is_empty() && !shape.specializations.is_empty() {
		for specialization in shape.specializations.clone() {
			for generic_usage in generic_usages.clone().iter() {
				generic_usage.apply_specialization(
					context.messages,
					context.type_store,
					context.function_store,
					context.module_path,
					&mut generic_usages,
					function_shape_index,
					&specialization.type_arguments,
					None,
				);
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
			context.module_path,
			context.function_generic_usages,
			function_shape_index,
			None,
			TypeArguments::new_from_explicit(Vec::new()),
		);

		if let Some(result) = result {
			if context.function_store.main.is_some() {
				let message = error!("Duplicate main function");
				context.message(message.span(statement.name.span));
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
fn trace_return<'a>(context: &mut Context<'a, '_, '_>, return_type: TypeId, block: &mut Block<'a>) -> TracedReturn {
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
			let error = error!("Expected return type of {expected}, got {got}");
			context.message(error.span(statement.span));
		}

		return TracedReturn::Covered;
	}

	if return_type.is_void(context.type_store) {
		return TracedReturn::Covered;
	}

	TracedReturn::NotCovered
}

fn validate_const<'a>(context: &mut Context<'a, '_, '_>, statement: &'a tree::Node<tree::Const<'a>>) -> Option<()> {
	let explicit_type = match &statement.item.parsed_type {
		Some(parsed_type) => context.lookup_type(parsed_type),
		None => None,
	};

	let mut expression = validate_expression(context, &statement.item.expression);
	if let Some(explicit_type) = explicit_type {
		if !context
			.type_store
			.collapse_to(context.messages, explicit_type, &mut expression)?
		{
			context.message(
				error!(
					"Const type mismatch between explicit type {} and expression type {}",
					context.type_name(explicit_type),
					context.type_name(expression.type_id),
				)
				.span(statement.span),
			);
			return None;
		}
	}

	let value = match &expression.kind {
		ExpressionKind::IntegerValue(value) => ConstantValue::IntegerValue(value.value()),
		ExpressionKind::DecimalValue(value) => ConstantValue::DecimalValue(value.value()),
		ExpressionKind::StringLiteral(literal) => ConstantValue::StringLiteral(literal.value.clone()),
		ExpressionKind::CodepointLiteral(literal) => ConstantValue::CodepointLiteral(literal.value),

		kind => {
			let name = kind.name_with_article();
			context.message(error!("Cannot have {name} as a const expression").span(expression.span));
			return None;
		}
	};

	let constant_index = context.constants.len();
	context.constants.push(value);

	let name = statement.item.name.item;
	let kind = SymbolKind::Const { constant_index };
	let span = Some(expression.span);
	let symbol = Symbol { name, kind, span };
	context.push_symbol(symbol);

	Some(())
}

fn validate_binding<'a>(context: &mut Context<'a, '_, '_>, statement: &'a tree::Node<tree::Binding<'a>>) -> Option<Binding<'a>> {
	let mut expression = validate_expression(context, &statement.item.expression);

	let type_id = match &statement.item.parsed_type {
		Some(parsed_type) => {
			let explicit_type = context.lookup_type(parsed_type)?;
			if !context.collapse_to(explicit_type, &mut expression)? {
				let expected = context.type_name(explicit_type);
				let got = context.type_name(expression.type_id);
				let err = error!("Expected {expected} but got expression with type {got}");
				context.message(err.span(statement.item.expression.span));
				return None;
			}

			explicit_type
		}

		_ => expression.type_id,
	};

	if type_id.is_void(context.type_store) {
		context.message(error!("Cannot create binding of `void`").span(statement.span));
	} else if type_id.is_untyped_integer(context.type_store) {
		context.message(error!("Cannot create binding of untyped integer").span(statement.span));
	} else if type_id.is_untyped_decimal(context.type_store) {
		context.message(error!("Cannot create binding of untyped decimal").span(statement.span));
	}

	let is_mutable = statement.item.is_mutable;
	let kind = match is_mutable {
		true => ReadableKind::Mut,
		false => ReadableKind::Let,
	};
	let readable_index = context.push_readable(statement.item.name, type_id, kind);

	let name = statement.item.name.item;
	Some(Binding { name, type_id, expression, readable_index, is_mutable })
}

fn validate_expression<'a>(
	context: &mut Context<'a, '_, '_>,
	expression: &'a tree::Node<tree::Expression<'a>>,
) -> Expression<'a> {
	let span = expression.span;

	match &expression.item {
		tree::Expression::Block(block) => validate_block_expression(context, block, span),
		tree::Expression::IntegerLiteral(literal) => validate_integer_literal(context, literal, span),
		tree::Expression::FloatLiteral(literal) => validate_float_literal(context, literal, span),
		tree::Expression::CodepointLiteral(literal) => validate_codepoint_literal(context, literal, span),
		tree::Expression::StringLiteral(literal) => validate_string_literal(context, literal, span),
		tree::Expression::StructLiteral(literal) => validate_struct_literal(context, literal, span),
		tree::Expression::Call(call) => validate_call(context, call, span),
		tree::Expression::Read(read) => validate_read(context, read, span),
		tree::Expression::FieldRead(field_read) => validate_field_read(context, field_read, span),
		tree::Expression::UnaryOperation(operation) => validate_unary_operation(context, operation, span),
		tree::Expression::BinaryOperation(operation) => validate_binary_operation(context, operation, span),
	}
}

fn validate_block_expression<'a>(context: &mut Context<'a, '_, '_>, block: &'a tree::Block<'a>, span: Span) -> Expression<'a> {
	let validated_block = validate_block(context.child_scope(), block, false);
	let type_id = validated_block.type_id;
	let kind = ExpressionKind::Block(validated_block);
	Expression { span, type_id, mutable: true, kind }
}

fn validate_integer_literal<'a>(context: &mut Context<'a, '_, '_>, literal: &tree::IntegerLiteral, span: Span) -> Expression<'a> {
	let value = IntegerValue::new(literal.value.item, literal.value.span);
	let kind = ExpressionKind::IntegerValue(value);
	let type_id = context.type_store.integer_type_id();
	Expression { span, type_id, mutable: true, kind }
}

fn validate_float_literal<'a>(context: &mut Context<'a, '_, '_>, literal: &tree::FloatLiteral, span: Span) -> Expression<'a> {
	let value = DecimalValue::new(literal.value.item, literal.value.span);
	let kind = ExpressionKind::DecimalValue(value);
	let type_id = context.type_store.decimal_type_id();
	Expression { span, type_id, mutable: true, kind }
}

fn validate_codepoint_literal<'a>(
	context: &mut Context<'a, '_, '_>,
	literal: &tree::CodepointLiteral,
	span: Span,
) -> Expression<'a> {
	let kind = ExpressionKind::CodepointLiteral(CodepointLiteral { value: literal.value.item });
	let type_id = context.type_store.u32_type_id();
	Expression { span, type_id, mutable: true, kind }
}

fn validate_string_literal<'a>(
	context: &mut Context<'a, '_, '_>,
	literal: &tree::StringLiteral<'a>,
	span: Span,
) -> Expression<'a> {
	let kind = ExpressionKind::StringLiteral(StringLiteral { value: literal.value.item.clone() });
	let type_id = context.type_store.string_type_id();
	Expression { span, type_id, mutable: true, kind }
}

fn validate_struct_literal<'a>(
	context: &mut Context<'a, '_, '_>,
	literal: &'a tree::StructLiteral<'a>,
	span: Span,
) -> Expression<'a> {
	let type_id = match context.lookup_type(&literal.parsed_type) {
		Some(type_id) => type_id,
		None => return Expression::any_collapse(context.type_store, span),
	};

	let type_entry = &context.type_store.type_entries[type_id.index()];
	let (shape_index, specialization_index) = match &type_entry.kind {
		TypeEntryKind::UserType { shape_index, specialization_index } => (*shape_index, *specialization_index),

		_ => {
			let name = context.type_name(type_id);
			let message = error!("Cannot construct type {name} like a struct as it is not a struct");
			context.message(message.span(literal.parsed_type.span));
			return Expression::any_collapse(context.type_store, span);
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
		let mut expression = validate_expression(context, &intializer.expression);

		let field = match fields.next() {
			Some(field) => field,

			None => {
				context.message(error!("Unexpected extra field initalizer").span(intializer.name.span));
				continue;
			}
		};

		if field.name != intializer.name.item {
			context.message(
				error!("Expected initalizer for field `{}`, got `{}` instead", field.name, intializer.name.item,)
					.span(intializer.name.span),
			);
		}

		if !context.collapse_to(field.type_id, &mut expression).unwrap_or(true) {
			// Avoids a silly error message when something happend in the field definition, causing it to
			// have `AnyCollapse` as its type, leading to an "Expected `AnyCollapse` got `_`" error
			if !field.type_id.is_any_collapse(context.type_store) {
				context.message(
					error!(
						"Field intializer type mismatch, expected {} but got {} instead",
						context.type_name(field.type_id),
						context.type_name(expression.type_id),
					)
					.span(intializer.name.span + expression.span),
				);
			}
		}

		field_initializers.push(FieldInitializer { expression });
	}

	let user_type = &context.type_store.user_types[shape_index];
	let shape = match &user_type.kind {
		UserTypeKind::Struct { shape } => shape,
	};
	let specialization = &shape.specializations[specialization_index];
	if field_initializers.len() < specialization.fields.len() {
		context.message(
			error!(
				"Too few field initializers, expected {}, but only found {}",
				specialization.fields.len(),
				field_initializers.len(),
			)
			.span(literal.initializer.span),
		);
	}

	let kind = ExpressionKind::StructLiteral(StructLiteral { type_id, field_initializers });
	Expression { span, type_id, mutable: true, kind }
}

fn validate_call<'a>(context: &mut Context<'a, '_, '_>, call: &'a tree::Call<'a>, span: Span) -> Expression<'a> {
	let symbol = match context.lookup_symbol(&call.path_segments.item) {
		Some(symbol) => symbol,
		None => return Expression::any_collapse(context.type_store, span),
	};

	let name = symbol.name;
	let function_shape_index = match symbol.kind {
		SymbolKind::Function { function_shape_index } => function_shape_index,

		kind => {
			context.message(error!("Cannot call {kind}").span(call.path_segments.span));
			return Expression::any_collapse(context.type_store, span);
		}
	};

	let mut type_argument_lookup_errored = false;
	let mut explicit_arguments = Vec::new();
	for type_argument in &call.type_arguments {
		if let Some(type_id) = context.lookup_type(type_argument) {
			explicit_arguments.push(type_id);
		} else {
			type_argument_lookup_errored = true;
		}
	}

	if type_argument_lookup_errored {
		return Expression::any_collapse(context.type_store, span);
	}

	let mut type_arguments = TypeArguments::new_from_explicit(explicit_arguments);
	let shape = &context.function_store.shapes[function_shape_index];
	if shape.generics.implicit_len() != 0 {
		// The only functions with implicit generic parameters are inner functions, and if we have it
		// in scope then that means it must be somewhere within ourselves or our function parent chain
		for parameter in context.generic_parameters.parameters() {
			type_arguments.push_implicit(parameter.generic_type_id);
		}
	}

	let results = context.function_store.get_or_add_specialization(
		context.messages,
		context.type_store,
		context.module_path,
		context.function_generic_usages,
		function_shape_index,
		Some(span),
		type_arguments,
	);
	let FunctionSpecializationResult { specialization_index, return_type } = match results {
		Some(results) => results,
		None => return Expression::any_collapse(context.type_store, span),
	};

	let mut arguments = Vec::with_capacity(call.arguments.len());
	for argument in &call.arguments {
		arguments.push(validate_expression(context, argument));
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

		let collapsed = context.type_store.collapse_to(context.messages, parameter.type_id, argument);
		if !collapsed.unwrap_or(true) {
			let error = error!(
				"Expected argument of type {}, got {}",
				context.type_name(parameter.type_id),
				context.type_name(argument.type_id)
			);
			context.messages.message(error.span(argument.span));
			arguments_type_mismatch = true;
		}
	}

	if arguments.len() != specialization.parameters.len() {
		let error = error!("Expected {} arguments, got {}", specialization.parameters.len(), arguments.len());
		context.message(error.span(span));
		return Expression::any_collapse(context.type_store, span);
	}

	if arguments_type_mismatch {
		return Expression::any_collapse(context.type_store, span);
	}

	let function_id = FunctionId { function_shape_index, specialization_index };
	let kind = ExpressionKind::Call(Call { name, function_id, arguments });
	Expression { span, type_id: return_type, mutable: true, kind }
}

fn validate_read<'a>(context: &mut Context<'a, '_, '_>, read: &tree::Read<'a>, span: Span) -> Expression<'a> {
	let symbol = match context.lookup_symbol(&read.path_segments.item) {
		Some(symbol) => symbol,
		None => return Expression::any_collapse(context.type_store, span),
	};

	let readable_index = match symbol.kind {
		SymbolKind::Let { readable_index } | SymbolKind::Mut { readable_index } => readable_index,

		SymbolKind::Const { constant_index } => {
			let constant = &context.constants[constant_index];
			let (kind, type_id) = match constant {
				ConstantValue::IntegerValue(value) => {
					let kind = ExpressionKind::IntegerValue(IntegerValue::new(*value, span));
					(kind, context.type_store.integer_type_id())
				}

				ConstantValue::DecimalValue(value) => {
					let kind = ExpressionKind::DecimalValue(DecimalValue::new(*value, span));
					(kind, context.type_store.decimal_type_id())
				}

				ConstantValue::CodepointLiteral(value) => {
					let kind = ExpressionKind::CodepointLiteral(CodepointLiteral { value: *value });
					(kind, context.type_store.u32_type_id())
				}

				ConstantValue::StringLiteral(value) => {
					let kind = ExpressionKind::StringLiteral(StringLiteral { value: value.clone() });
					(kind, context.type_store.string_type_id())
				}
			};

			return Expression { span, type_id, mutable: false, kind };
		}

		kind => {
			context.message(error!("Cannot read value from {kind}").span(read.path_segments.span));
			return Expression::any_collapse(context.type_store, span);
		}
	};

	let readable = match context.readables.get(readable_index) {
		Some(readable) => readable,
		None => return Expression::any_collapse(context.type_store, span),
	};

	let mutable = readable.kind == ReadableKind::Mut;
	let read = Read {
		name: readable.name,
		type_id: readable.type_id,
		readable_index,
	};

	let kind = ExpressionKind::Read(read);
	Expression { span, type_id: readable.type_id, mutable, kind }
}

fn validate_field_read<'a>(context: &mut Context<'a, '_, '_>, field_read: &'a tree::FieldRead<'a>, span: Span) -> Expression<'a> {
	let base = validate_expression(context, &field_read.base);
	if base.type_id.is_any_collapse(context.type_store) {
		return Expression::any_collapse(context.type_store, span);
	}

	// Dumb hack to store fields array in outer scope so a slice can be taken
	let slice_fields;

	let fields: &[Field] = if let Some(as_struct) = base.type_id.as_struct(context.type_store) {
		&as_struct.fields
	} else if let Some(as_slice) = base.type_id.as_slice(context.type_store) {
		slice_fields = [
			Field {
				span: None,
				name: "pointer",
				type_id: context.type_store.pointer_to(as_slice.type_id, false),
			},
			Field {
				span: None,
				name: "len",
				type_id: context.type_store.i64_type_id(),
			},
		];
		&slice_fields
	} else {
		let error = error!("Cannot access field on {}", base.kind.name_with_article());
		context.message(error.span(span));
		return Expression::any_collapse(context.type_store, span);
	};

	let mut fields = fields.iter().enumerate();
	let Some((field_index, field)) = fields.find(|f| f.1.name == field_read.name.item) else {
		let type_name = context.type_name(base.type_id);
		let error = error!("No field `{}` on {}", field_read.name.item, type_name);
		context.message(error.span(field_read.name.span));
		return Expression::any_collapse(context.type_store, span);
	};

	let mutable = base.mutable;
	let field_read = FieldRead { base, name: field.name, type_id: field.type_id, field_index };
	let kind = ExpressionKind::FieldRead(Box::new(field_read));
	Expression { span, type_id: field.type_id, mutable, kind }
}

fn validate_unary_operation<'a>(
	context: &mut Context<'a, '_, '_>,
	operation: &'a tree::UnaryOperation<'a>,
	span: Span,
) -> Expression<'a> {
	let op = match operation.op.item {
		tree::UnaryOperator::Negate => UnaryOperator::Negate,
	};

	let mut expression = validate_expression(context, &operation.expression);
	match (op, &mut expression.kind) {
		(UnaryOperator::Negate, ExpressionKind::IntegerValue(value)) => {
			value.negate(context.messages, span);
			expression.span = expression.span + span;
			return expression;
		}

		(UnaryOperator::Negate, ExpressionKind::DecimalValue(value)) => {
			value.negate(span);
			expression.span = expression.span + span;
			return expression;
		}

		_ => {}
	}

	let type_id = expression.type_id;
	let kind = ExpressionKind::UnaryOperation(Box::new(UnaryOperation { op, expression }));
	Expression { span, type_id, mutable: true, kind }
}

fn validate_binary_operation<'a>(
	context: &mut Context<'a, '_, '_>,
	operation: &'a tree::BinaryOperation<'a>,
	span: Span,
) -> Expression<'a> {
	let op = operation.op.item;

	let mut left = validate_expression(context, &operation.left);
	let mut right = validate_expression(context, &operation.right);
	let collapsed = context.type_store.collapse_fair(context.messages, &mut left, &mut right);

	let Some(collapsed) = collapsed else {
		context.message(
			error!("{} type mismatch", op.name())
				.span(span)
				.note(note!(operation.left.span, "Left type {}", context.type_name(left.type_id)))
				.note(note!(operation.right.span, "Right type {}", context.type_name(right.type_id))),
		);
		return Expression::any_collapse(context.type_store, span);
	};

	if let Some(constant_math_result) = perform_constant_math(context, &left, &right, op) {
		return constant_math_result;
	}

	if op == BinaryOperator::Assign {
		if let ExpressionKind::Read(read) = &left.kind {
			let readable = match context.readables.get(read.readable_index) {
				Some(readable) => readable,
				None => return Expression::any_collapse(context.type_store, span),
			};

			if readable.kind != ReadableKind::Mut {
				context.message(error!("Cannot assign to immutable binding `{}`", read.name).span(left.span));
			}
		} else if let ExpressionKind::FieldRead(_) = &left.kind {
			if !left.mutable {
				context.message(error!("Cannot assign to field of immutable object").span(left.span));
			}
		} else {
			context.message(error!("Cannot assign to {}", left.kind.name_with_article()).span(left.span));
		}
	}

	let type_id = match op {
		BinaryOperator::Assign => context.type_store.void_type_id(),

		_ if matches!(left.kind, ExpressionKind::AnyCollapse) || matches!(right.kind, ExpressionKind::AnyCollapse) => {
			context.type_store.any_collapse_type_id()
		}

		_ => collapsed,
	};

	let operation = Box::new(BinaryOperation { op, left, right, type_id });
	let kind = ExpressionKind::BinaryOperation(operation);
	Expression { span, type_id, mutable: true, kind }
}

fn perform_constant_math<'a>(
	context: &mut Context<'a, '_, '_>,
	left: &Expression,
	right: &Expression,
	op: BinaryOperator,
) -> Option<Expression<'a>> {
	if let ExpressionKind::IntegerValue(left) = left.kind {
		let right = match &right.kind {
			ExpressionKind::IntegerValue(right) => *right,
			ExpressionKind::DecimalValue(..) => unreachable!(),
			_ => return None,
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
		return Some(Expression { span: value.span(), type_id, mutable: false, kind });
	}

	if let ExpressionKind::DecimalValue(left) = left.kind {
		let right = match &right.kind {
			ExpressionKind::DecimalValue(right) => *right,
			ExpressionKind::IntegerValue(..) => unreachable!(),
			_ => return None,
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
		return Some(Expression { span: value.span(), type_id, mutable: false, kind });
	}

	None
}
