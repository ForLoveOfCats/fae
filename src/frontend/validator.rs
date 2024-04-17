use std::rc::Rc;

use crate::cli_arguments::CliArguments;
use crate::frontend::error::*;
use crate::frontend::function_store::FunctionStore;
use crate::frontend::ir::*;
use crate::frontend::lang_items::LangItems;
use crate::frontend::root_layers::RootLayers;
use crate::frontend::span::Span;
use crate::frontend::symbols::{Externs, ReadableKind, Readables, Statics, Symbol, SymbolKind, Symbols};
use crate::frontend::tree::Node;
use crate::frontend::tree::{self, BinaryOperator, PathSegments};
use crate::frontend::type_store::*;

#[derive(Debug)]
pub struct Context<'a, 'b, 'c> {
	pub cli_arguments: &'a CliArguments,

	pub file_index: usize,
	pub module_path: &'a [String],

	pub messages: &'b mut Messages<'a>,

	pub type_store: &'b mut TypeStore<'a>,
	pub function_store: &'b mut FunctionStore<'a>,
	pub function_generic_usages: &'b mut Vec<GenericUsage>,

	pub root_layers: &'b RootLayers<'a>,

	pub lang_items: &'b mut LangItems,
	pub externs: &'b mut Externs,
	pub constants: &'b mut Vec<ConstantValue<'a>>,
	pub statics: &'b mut Statics<'a>,
	pub initial_readables_starting_index: usize,
	pub initial_readables_overall_len: usize,
	pub readables: &'b mut Readables<'a>,

	pub initial_symbols_len: usize,
	pub function_initial_symbols_len: usize,
	pub symbols: &'b mut Symbols<'a>,

	pub return_type: Option<TypeId>,
	pub generic_parameters: &'c GenericParameters<'a>,
}

impl<'a, 'b, 'c> Drop for Context<'a, 'b, 'c> {
	fn drop(&mut self) {
		self.readables.readables.truncate(self.initial_readables_overall_len);
		self.readables.starting_index = self.initial_readables_starting_index;
		self.symbols.symbols.truncate(self.initial_symbols_len);
	}
}

impl<'a, 'b, 'c> Context<'a, 'b, 'c> {
	fn child_scope<'s>(&'s mut self) -> Context<'a, 's, 'c> {
		Context {
			cli_arguments: self.cli_arguments,

			file_index: self.file_index,
			module_path: self.module_path,

			messages: self.messages,

			type_store: self.type_store,
			function_store: self.function_store,
			function_generic_usages: self.function_generic_usages,

			root_layers: self.root_layers,

			lang_items: self.lang_items,
			externs: self.externs,
			constants: self.constants,
			statics: self.statics,
			initial_readables_starting_index: self.readables.starting_index,
			initial_readables_overall_len: self.readables.overall_len(),
			readables: self.readables,

			initial_symbols_len: self.symbols.len(),
			function_initial_symbols_len: self.function_initial_symbols_len,
			symbols: self.symbols,

			return_type: self.return_type,
			generic_parameters: self.generic_parameters,
		}
	}

	fn child_scope_for_function<'s, 't>(
		&'s mut self,
		return_type: TypeId,
		generic_parameters: &'t GenericParameters<'a>,
	) -> Context<'a, 's, 't> {
		let initial_readables_starting_index = self.readables.starting_index;
		self.readables.starting_index = self.readables.overall_len();

		Context {
			cli_arguments: self.cli_arguments,

			file_index: self.file_index,
			module_path: self.module_path,

			messages: self.messages,

			type_store: self.type_store,
			function_store: self.function_store,
			function_generic_usages: self.function_generic_usages,

			root_layers: self.root_layers,

			lang_items: self.lang_items,
			externs: self.externs,
			constants: self.constants,
			statics: self.statics,
			initial_readables_starting_index,
			initial_readables_overall_len: self.readables.overall_len(),
			readables: self.readables,

			initial_symbols_len: self.symbols.len(),
			function_initial_symbols_len: self.function_initial_symbols_len,
			symbols: self.symbols,

			return_type: Some(return_type),
			generic_parameters,
		}
	}

	pub fn message(&mut self, message: Message) {
		self.messages.message(message);
	}

	pub fn push_symbol(&mut self, symbol: Symbol<'a>) {
		self.symbols
			.push_symbol(self.messages, self.function_initial_symbols_len, symbol);
	}

	pub fn push_readable(&mut self, name: tree::Node<&'a str>, type_id: TypeId, kind: ReadableKind) -> usize {
		let readable_index = self.readables.push(name.item, type_id, kind);

		let span = Some(name.span);
		let name = name.item;
		let kind = match kind {
			ReadableKind::Let => SymbolKind::Let { readable_index },
			ReadableKind::Mut => SymbolKind::Mut { readable_index },
		};

		if name != "_" {
			self.push_symbol(Symbol { name, kind, span });
		}

		readable_index
	}

	pub fn lookup_symbol(&mut self, path: &PathSegments<'a>) -> Option<Symbol<'a>> {
		self.symbols
			.lookup_symbol(self.messages, self.root_layers, self.type_store, self.function_initial_symbols_len, path)
	}

	pub fn lookup_type(&mut self, parsed_type: &Node<tree::Type<'a>>) -> Option<TypeId> {
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

	pub fn type_name(&self, type_id: TypeId) -> String {
		self.type_store.type_name(self.function_store, self.module_path, type_id)
	}

	pub fn collapse_fair(&mut self, a: &mut Expression<'a>, b: &mut Expression<'a>) -> Option<TypeId> {
		self.type_store.collapse_fair(self.messages, self.function_store, a, b)
	}

	pub fn collapse_to(&mut self, to: TypeId, from: &mut Expression<'a>) -> Option<bool> {
		self.type_store.collapse_to(self.messages, self.function_store, to, from)
	}
}

pub fn validate<'a>(
	cli_arguments: &'a CliArguments,
	messages: &mut Messages<'a>,
	lang_items: &mut LangItems,
	root_layers: &mut RootLayers<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &mut FunctionStore<'a>,
	statics: &mut Statics<'a>,
	parsed_files: &'a [tree::File<'a>],
) {
	let mut function_generic_usages = Vec::new();
	let mut externs = Externs::new();
	let mut constants = Vec::new();

	create_root_types(messages, root_layers, type_store, parsed_files);
	resolve_root_type_imports(messages, root_layers, parsed_files);
	fill_root_types(messages, type_store, function_store, root_layers, parsed_files);

	let mut readables = Readables::new();
	create_root_functions(
		messages,
		root_layers,
		type_store,
		function_store,
		&mut function_generic_usages,
		&mut externs,
		&mut readables,
		parsed_files,
	);
	function_generic_usages.clear();

	let mut symbols = Symbols::new();
	validate_root_consts(
		cli_arguments,
		messages,
		lang_items,
		root_layers,
		type_store,
		function_store,
		&mut function_generic_usages,
		&mut externs,
		&mut constants,
		statics,
		&mut readables,
		parsed_files,
		&mut symbols,
	);

	for parsed_file in parsed_files {
		let file_index = parsed_file.source_file.index;
		let module_path = parsed_file.module_path;

		let layer = root_layers.create_module_path(module_path);
		symbols.duplicate(&layer.symbols);

		readables.starting_index = 0;
		readables.readables.clear();

		let blank_generic_parameters = GenericParameters::new_from_explicit(Vec::new());
		let context = Context {
			cli_arguments,
			file_index,
			module_path,
			messages,
			type_store,
			function_store,
			function_generic_usages: &mut function_generic_usages,
			root_layers,
			lang_items,
			externs: &mut externs,
			constants: &mut constants,
			statics,
			initial_readables_starting_index: readables.starting_index,
			initial_readables_overall_len: readables.overall_len(),
			readables: &mut readables,
			initial_symbols_len: symbols.len(),
			function_initial_symbols_len: symbols.len(),
			symbols: &mut symbols,
			return_type: None,
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
	root_layers: &mut RootLayers<'a>,
	type_store: &mut TypeStore<'a>,
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

		let module_path = parsed_file.module_path;
		let block = &parsed_file.block;
		resolve_block_type_imports(messages, root_layers, &mut symbols, module_path, 0, block, true);

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
	externs: &mut Externs,
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
			root_layers,
			type_store,
			function_store,
			generic_usages,
			externs,
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
	cli_arguments: &'a CliArguments,
	messages: &mut Messages<'a>,
	lang_items: &mut LangItems,
	root_layers: &mut RootLayers<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &mut FunctionStore<'a>,
	function_generic_usages: &mut Vec<GenericUsage>,
	externs: &mut Externs,
	constants: &mut Vec<ConstantValue<'a>>,
	statics: &mut Statics<'a>,
	readables: &mut Readables<'a>,
	parsed_files: &'a [tree::File<'a>],
	symbols: &mut Symbols<'a>,
) {
	for parsed_file in parsed_files {
		let file_index = parsed_file.source_file.index;
		let module_path = parsed_file.module_path;

		let layer = root_layers.create_module_path(parsed_file.module_path);
		symbols.duplicate(&layer.symbols);

		readables.starting_index = 0;
		readables.readables.clear();

		let blank_generic_parameters = GenericParameters::new_from_explicit(Vec::new());
		let mut context = Context {
			cli_arguments,
			file_index,
			module_path,
			messages,
			type_store,
			function_store,
			function_generic_usages,
			root_layers,
			lang_items,
			externs,
			constants,
			statics,
			initial_readables_starting_index: readables.starting_index,
			initial_readables_overall_len: readables.overall_len(),
			readables,
			initial_symbols_len: symbols.len(),
			function_initial_symbols_len: symbols.len(),
			symbols,
			return_type: None,
			generic_parameters: &blank_generic_parameters,
		};

		let old_symbols_len = context.symbols.len();
		validate_block_consts(&mut context, &parsed_file.block);
		assert_eq!(context.readables.overall_len(), 0);
		assert_eq!(context.readables.starting_index, 0);
		let importable_consts_range = old_symbols_len..context.symbols.len();

		let old_symbols_len = context.symbols.len();
		validate_block_statics(&mut context, &parsed_file.block);
		assert_eq!(context.readables.overall_len(), 0);
		assert_eq!(context.readables.starting_index, 0);
		let importable_statics_range = old_symbols_len..context.symbols.len();

		std::mem::forget(context);

		let layer = root_layers.create_module_path(parsed_file.module_path);
		layer.importable_consts_range = importable_consts_range;
		layer.importable_statics_range = importable_statics_range;
		layer.symbols.duplicate(symbols);
	}
}

fn resolve_block_type_imports<'a>(
	messages: &mut Messages,
	root_layers: &RootLayers<'a>,
	symbols: &mut Symbols<'a>,
	module_path: &'a [String],
	function_initial_symbols_len: usize,
	block: &tree::Block<'a>,
	is_root: bool,
) {
	if is_root && !matches!(module_path, [a, b] if a == "fae" && b == "prelude") {
		let path = PathSegments::Path {
			segments: vec![Node::new("fae", Span::unusable()), Node::new("prelude", Span::unusable())],
		};
		resolve_import_for_block_types(messages, root_layers, symbols, function_initial_symbols_len, &path, None);
	}

	for statement in &block.statements {
		let import_statement = match statement {
			tree::Statement::Import(import_statement) => import_statement,
			_ => continue,
		};

		let path = &import_statement.item.path_segments;
		let names = Some(import_statement.item.symbol_names.as_slice());
		resolve_import_for_block_types(messages, root_layers, symbols, function_initial_symbols_len, path, names);
	}
}

fn resolve_import_for_block_types<'a>(
	messages: &mut Messages,
	root_layers: &RootLayers<'a>,
	symbols: &mut Symbols<'a>,
	function_initial_symbols_len: usize,
	path: &PathSegments<'a>,
	names: Option<&[Node<&'a str>]>,
) {
	let layer = match root_layers.layer_for_path(messages, path) {
		Some(found) if found.symbols.is_empty() => return,
		Some(found) => found,
		_ => return,
	};

	if let Some(names) = names {
		for &importing in layer.importable_types() {
			if let Some(name) = names.iter().find(|n| n.item == importing.name) {
				symbols.push_imported_symbol(messages, function_initial_symbols_len, importing, Some(name.span));
			}
		}
	} else {
		for &importing in layer.importable_types() {
			symbols.push_imported_symbol(messages, function_initial_symbols_len, importing, None);
		}
	}
}

fn resolve_block_non_type_imports<'a>(
	messages: &mut Messages,
	root_layers: &RootLayers<'a>,
	symbols: &mut Symbols<'a>,
	module_path: &'a [String],
	function_initial_symbols_len: usize,
	block: &tree::Block<'a>,
	is_root: bool,
) {
	if is_root && !matches!(module_path, [a, b] if a == "fae" && b == "prelude") {
		let path = PathSegments::Path {
			segments: vec![Node::new("fae", Span::unusable()), Node::new("prelude", Span::unusable())],
		};
		resolve_import_for_block_non_types(messages, root_layers, symbols, function_initial_symbols_len, &path, None);
	}

	for statement in &block.statements {
		let import_statement = match statement {
			tree::Statement::Import(import_statement) => import_statement,
			_ => continue,
		};

		let path = &import_statement.item.path_segments;
		let names = Some(import_statement.item.symbol_names.as_slice());
		resolve_import_for_block_non_types(messages, root_layers, symbols, function_initial_symbols_len, path, names);
	}
}

fn resolve_import_for_block_non_types<'a>(
	messages: &mut Messages,
	root_layers: &RootLayers<'a>,
	symbols: &mut Symbols<'a>,
	function_initial_symbols_len: usize,
	path: &PathSegments<'a>,
	names: Option<&[Node<&'a str>]>,
) {
	let layer = match root_layers.layer_for_path(messages, path) {
		Some(found) if found.symbols.is_empty() => return,
		Some(found) => found,
		_ => return,
	};

	if let Some(names) = names {
		for &importing in layer.importable_functions() {
			if let Some(name) = names.iter().find(|n| n.item == importing.name) {
				symbols.push_imported_symbol(messages, function_initial_symbols_len, importing, Some(name.span));
			}
		}

		for &importing in layer.importable_consts() {
			if let Some(name) = names.iter().find(|n| n.item == importing.name) {
				symbols.push_imported_symbol(messages, function_initial_symbols_len, importing, Some(name.span));
			}
		}

		for &importing in layer.importable_statics() {
			if let Some(name) = names.iter().find(|n| n.item == importing.name) {
				symbols.push_imported_symbol(messages, function_initial_symbols_len, importing, Some(name.span));
			}
		}
	} else {
		// TODO: Add asterisk syntax for importing all items in a scope

		for &importing in layer.importable_functions() {
			symbols.push_imported_symbol(messages, function_initial_symbols_len, importing, None);
		}

		for &importing in layer.importable_consts() {
			symbols.push_imported_symbol(messages, function_initial_symbols_len, importing, None);
		}

		for &importing in layer.importable_statics() {
			symbols.push_imported_symbol(messages, function_initial_symbols_len, importing, None);
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
				| tree::Statement::While(..)
				| tree::Statement::Binding(..)
				| tree::Statement::Return(..) => {
					let error = error!("{} is not allowed in a root scope", statement.name_and_article());
					messages.message(error.span(statement.span()));
					continue;
				}

				tree::Statement::Import(..)
				| tree::Statement::Struct(..)
				| tree::Statement::Function(..)
				| tree::Statement::Const(..)
				| tree::Statement::Static(..) => {}
			}
		} else {
			match statement {
				tree::Statement::Static(..) => {
					let error = error!("{} is only allowed in a root scope", statement.name_and_article());
					messages.message(error.span(statement.span()));
					continue;
				}

				_ => {}
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
		type_store.calculate_layout(type_id);
	}
}

fn report_cyclic_user_type<'a>(
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
	root_layers: &RootLayers<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &mut FunctionStore<'a>,
	generic_usages: &mut Vec<GenericUsage>,
	externs: &mut Externs,
	readables: &mut Readables<'a>,
	symbols: &mut Symbols<'a>,
	module_path: &'a [String],
	enclosing_generic_parameters: &GenericParameters<'a>,
	block: &'a tree::Block<'a>,
	file_index: usize,
) {
	for statement in &block.statements {
		if let tree::Statement::Function(statement) = statement {
			let original_readables_starting_index = readables.starting_index;
			let original_readables_overall_len = readables.overall_len();
			readables.starting_index = readables.overall_len();

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
			for (index, parameter) in statement.parameters.parameters.iter().enumerate() {
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
				assert_eq!(readable_index, index);

				let name = parameter.item.name;
				parameters.push(ParameterShape { name, type_id, is_mutable, readable_index });
			}

			drop(scope);

			let in_std = module_path.first().map(|s| s == "fae").unwrap_or(false);
			if !in_std {
				if let Some(intrinsic_attribute) = statement.intrinsic_attribute {
					let error = error!("Function may not be annotated as an intrinsic outside the standard library");
					messages.message(error.span(intrinsic_attribute.span));
				}
			}

			let c_varargs = statement.parameters.c_varargs.is_some();
			if let Some(varargs_span) = statement.parameters.c_varargs {
				if statement.extern_attribute.is_none() {
					let error = error!("Function must be an extern to accept C varargs");
					messages.message(error.span(varargs_span));
				}
			}

			let name = statement.name;
			if let Some(Node { item: extern_attribute, .. }) = statement.extern_attribute {
				externs.push(messages, extern_attribute.name, name.span);
			}

			let is_main = module_path == [root_layers.root_name.as_str()] && name.item == "main";
			let shape = FunctionShape::new(
				name,
				module_path,
				file_index,
				is_main,
				generics,
				statement.extern_attribute,
				statement.export_attribute,
				statement.intrinsic_attribute,
				statement.lang_attribute,
				parameters,
				c_varargs,
				return_type,
			);
			function_store.shapes.push(shape);

			let kind = SymbolKind::Function { function_shape_index };
			let span = Some(statement.name.span);
			let symbol = Symbol { name: name.item, kind, span };
			symbols.push_symbol(messages, function_initial_symbols_len, symbol);

			readables.readables.truncate(original_readables_overall_len);
			readables.starting_index = original_readables_starting_index;
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

fn validate_block_statics<'a>(context: &mut Context<'a, '_, '_>, block: &'a tree::Block<'a>) {
	for statement in &block.statements {
		if let tree::Statement::Static(statement) = statement {
			validate_static(context, statement);
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
			is_root,
		);

		resolve_block_type_imports(
			context.messages,
			context.root_layers,
			context.symbols,
			context.module_path,
			context.function_initial_symbols_len,
			block,
			is_root,
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
		context.module_path,
		context.function_initial_symbols_len,
		block,
		is_root,
	);

	if !is_root {
		create_block_functions(
			context.messages,
			context.root_layers,
			context.type_store,
			context.function_store,
			context.function_generic_usages,
			context.externs,
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

	let mut returns = false;
	let mut statements = Vec::with_capacity(block.statements.len());

	for statement in &block.statements {
		match statement {
			tree::Statement::Expression(..)
			| tree::Statement::Block(..)
			| tree::Statement::While(..)
			| tree::Statement::Binding(..)
			| tree::Statement::Return(..)
				if is_root => {} // `is_root` is true, then we've already emitted a message in the root pre-process step, skip

			tree::Statement::Expression(statement) => {
				let expression = validate_expression(&mut context, statement);
				returns |= expression.returns;

				let kind = StatementKind::Expression(expression);
				statements.push(Statement { kind });
			}

			tree::Statement::Block(statement) => {
				let scope = context.child_scope();
				let block = validate_block(scope, &statement.item, false);
				let kind = StatementKind::Block(block);
				statements.push(Statement { kind })
			}

			tree::Statement::While(statement) => {
				let statement = validate_while_statement(&mut context, statement);
				let kind = StatementKind::While(statement);
				statements.push(Statement { kind })
			}

			tree::Statement::Import(..) => {}

			tree::Statement::Struct(..) => {}

			tree::Statement::Function(statement) => validate_function(&mut context, statement),

			tree::Statement::Const(..) => {}

			tree::Statement::Static(..) => {}

			tree::Statement::Binding(statement) => {
				let validated = match validate_binding(&mut context, statement) {
					Some(validated) => validated,
					None => continue,
				};

				let kind = StatementKind::Binding(Box::new(validated));
				statements.push(Statement { kind });
			}

			tree::Statement::Return(statement) => {
				returns |= true;

				let expression = statement.item.expression.as_ref();
				let span = match &expression {
					Some(expression) => statement.span + expression.span,
					None => statement.span,
				};

				let mut expression = expression.map(|expression| validate_expression(&mut context, expression));
				if let Some(expression) = &mut expression {
					let return_type = context.return_type.unwrap();
					if let Some(false) = context.collapse_to(return_type, expression) {
						let expected = context.type_name(return_type);
						let got = context.type_name(expression.type_id);
						let error = error!("Expected return type of {expected}, got {got}");
						context.message(error.span(statement.span));
					}
				}

				let boxed_return = Box::new(Return { span, expression });
				let kind = StatementKind::Return(boxed_return);
				statements.push(Statement { kind })
			}
		}
	}

	// TODO: Add `give` keywork and support block expressions
	// Make sure to disallow in root of function
	let type_id = context.type_store.void_type_id();
	Block { type_id, returns, statements }
}

fn validate_function<'a>(context: &mut Context<'a, '_, '_>, statement: &'a tree::Function<'a>) {
	if statement.extern_attribute.is_some() || statement.intrinsic_attribute.is_some() {
		// Note: Signature has already been checked in `create_block_functions`
		return;
	}

	// TODO: Rip this out, like yesterday
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

	let return_type = context.function_store.shapes[function_shape_index].return_type;
	let generics = context.function_store.shapes[function_shape_index].generics.clone();
	let mut scope = context.child_scope_for_function(return_type, &generics);
	scope.function_initial_symbols_len = scope.symbols.len();
	let initial_generic_usages_len = scope.function_generic_usages.len();

	for (generic_index, generic) in generics.parameters().iter().enumerate() {
		let kind = SymbolKind::FunctionGeneric { function_shape_index, generic_index };
		let symbol = Symbol { name: generic.name.item, kind, span: Some(generic.name.span) };
		scope.push_symbol(symbol);
	}

	for (index, parameter) in statement.parameters.parameters.iter().enumerate() {
		let span = parameter.span;
		let parameter = &parameter.item;

		let shape = &scope.function_store.shapes[function_shape_index];
		let parameter_shape = &shape.parameters[index];

		let stored_readable_index = parameter_shape.readable_index;
		assert_eq!(stored_readable_index, index);
		let kind = match parameter.is_mutable {
			false => ReadableKind::Let,
			true => ReadableKind::Mut,
		};
		let readable_index = scope.readables.push(parameter.name.item, parameter_shape.type_id, kind);
		assert_eq!(readable_index, stored_readable_index);
		let kind = match parameter.is_mutable {
			false => SymbolKind::Let { readable_index },
			true => SymbolKind::Mut { readable_index },
		};

		let name = parameter.name.item;
		scope.push_symbol(Symbol { name, kind, span: Some(span) });
	}

	let tree_block = &statement.block.as_ref().unwrap().item;
	let block = validate_block(scope, tree_block, false);

	if !return_type.is_void(context.type_store) && !block.returns {
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

	let type_parameters_iter = shape.generics.explicit_parameters().iter();
	let type_parameter_span = type_parameters_iter.fold(None, |sum, p| match sum {
		Some(sum) => Some(sum + p.name.span),
		None => Some(p.name.span),
	});

	if shape.is_main {
		assert_eq!(shape.generics.implicit_len(), 0);
		let has_return_type = !shape.return_type.is_void(context.type_store);

		let export_span = shape.export_attribute.map(|a| a.span);

		let parameters = statement.parameters.parameters.iter();
		let parameter_span = parameters.fold(None, |sum, p| match sum {
			Some(sum) => Some(sum + p.span),
			None => Some(p.span),
		});

		if let Some(export_span) = export_span {
			let message = error!("Main function may not have an export attribute");
			context.message(message.span(export_span));
		}

		if let Some(type_parameter_span) = type_parameter_span {
			let message = error!("Main function may not have any generic type parameters");
			context.message(message.span(type_parameter_span));
		}

		if let Some(parameter_span) = parameter_span {
			let message = error!("Main function may not have any parameters");
			context.message(message.span(parameter_span));
		}

		if has_return_type {
			let message = error!("Main function may not have a non-void return type");
			let node = statement.parsed_type.as_ref().unwrap(); // Must have parsed type to have non-void
			context.message(message.span(node.span));
			return;
		}

		if type_parameter_span.is_some() {
			// Not part of main `if` block as to keep relative order of messages
			return;
		}

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
	} else if shape.export_attribute.is_some() {
		if let Some(type_parameter_span) = type_parameter_span {
			let message = error!("Exported function may not have any generic type parameters");
			context.message(message.span(type_parameter_span));
		}

		context.function_store.get_or_add_specialization(
			context.messages,
			context.type_store,
			context.module_path,
			context.function_generic_usages,
			function_shape_index,
			None,
			TypeArguments::new_from_explicit(Vec::new()),
		);
	} else if let Some(lang_attribute) = &shape.lang_attribute {
		let lang_name = lang_attribute.item.name;
		let lang_span = lang_attribute.span;

		if let Some(type_parameter_span) = type_parameter_span {
			let message = error!("Lang item function may not have any generic type parameters");
			context.message(message.span(type_parameter_span));
		}

		let result = context.function_store.get_or_add_specialization(
			context.messages,
			context.type_store,
			context.module_path,
			context.function_generic_usages,
			function_shape_index,
			None,
			TypeArguments::new_from_explicit(Vec::new()),
		);

		if let Some(FunctionSpecializationResult { specialization_index, .. }) = result {
			let function_id = FunctionId { function_shape_index, specialization_index };
			context
				.lang_items
				.register_lang_function(context.messages, function_id, lang_name, lang_span);
		}
	}
}

fn validate_const<'a>(context: &mut Context<'a, '_, '_>, statement: &'a tree::Node<tree::Const<'a>>) -> Option<()> {
	let explicit_type = match &statement.item.parsed_type {
		Some(parsed_type) => context.lookup_type(parsed_type),
		None => None,
	};

	let mut expression = validate_expression(context, &statement.item.expression);
	if let Some(explicit_type) = explicit_type {
		if !context.collapse_to(explicit_type, &mut expression)? {
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
			let name = kind.name();
			context.message(error!("Cannot have a runtime {name} as a const expression").span(expression.span));
			return None;
		}
	};

	let constant_index = context.constants.len();
	context.constants.push(value);

	let name = statement.item.name.item;
	let kind = SymbolKind::Const { constant_index };
	let span = Some(statement.span + expression.span);
	let symbol = Symbol { name, kind, span };
	context.push_symbol(symbol);

	Some(())
}

fn validate_static<'a>(context: &mut Context<'a, '_, '_>, statement: &'a tree::Node<tree::Static<'a>>) -> Option<()> {
	if let Some(extern_attribute) = statement.item.extern_attribute {
		let name = extern_attribute.item.name;
		context.externs.push(context.messages, name, statement.span);
	} else {
		let error = error!("Static definition must have extern attribute");
		context.message(error.span(statement.span));
	}

	let type_id = context.lookup_type(&statement.item.parsed_type)?;
	let layout = context.type_store.type_layout(type_id);
	if layout.size <= 0 {
		let error = error!("Static definition cannot have a zero-sized type");
		context.message(error.span(statement.span));
	}

	let name = statement.item.name.item;
	let extern_attribute = statement.item.extern_attribute.map(|n| n.item);
	let index = context.statics.push(name, type_id, extern_attribute);

	let kind = SymbolKind::Static { static_index: index };
	let span = Some(statement.span);
	context.push_symbol(Symbol { name, kind, span });

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

	if type_id.is_untyped_integer(context.type_store) {
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

pub fn validate_expression<'a>(
	context: &mut Context<'a, '_, '_>,
	expression: &'a tree::Node<tree::Expression<'a>>,
) -> Expression<'a> {
	let span = expression.span;

	match &expression.item {
		tree::Expression::Block(block) => validate_block_expression(context, block, span),
		tree::Expression::If(if_expression) => validate_if_expression(context, if_expression, span),
		tree::Expression::IntegerLiteral(literal) => validate_integer_literal(context, literal, span),
		tree::Expression::FloatLiteral(literal) => validate_float_literal(context, literal, span),
		tree::Expression::BooleanLiteral(literal) => validate_bool_literal(context, *literal, span),
		tree::Expression::CodepointLiteral(literal) => validate_codepoint_literal(context, literal, span),
		tree::Expression::StringLiteral(literal) => validate_string_literal(context, literal, span),
		tree::Expression::ArrayLiteral(literal) => validate_array_literal(context, literal, span),
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
	let returns = validated_block.returns;
	let type_id = validated_block.type_id;
	let kind = ExpressionKind::Block(validated_block);
	Expression { span, type_id, mutable: true, returns, kind }
}

fn validate_if_expression<'a>(context: &mut Context<'a, '_, '_>, if_expression: &'a tree::If<'a>, span: Span) -> Expression<'a> {
	let mut scope = context.child_scope();
	let condition = validate_expression(&mut scope, &if_expression.condition);
	let body = validate_block(scope, &if_expression.body.item, false);
	let type_id = body.type_id; // TODO: Wrong, needs else-if/else
	let returns = condition.returns; // The body is not guarenteed to run, therefore cannot indicate termination
	let kind = ExpressionKind::If(Box::new(If { type_id, condition, body }));
	Expression { span, type_id, mutable: true, returns, kind }
}

fn validate_while_statement<'a>(context: &mut Context<'a, '_, '_>, statement: &'a Node<tree::While<'a>>) -> While<'a> {
	let mut scope = context.child_scope();
	let condition = validate_expression(&mut scope, &statement.item.condition);
	let body = validate_block(scope, &statement.item.body.item, false);
	While { condition, body }
}

fn validate_integer_literal<'a>(context: &mut Context<'a, '_, '_>, literal: &tree::IntegerLiteral, span: Span) -> Expression<'a> {
	let value = IntegerValue::new(literal.value.item, literal.value.span);
	let kind = ExpressionKind::IntegerValue(value);
	let type_id = context.type_store.integer_type_id();
	Expression { span, type_id, mutable: true, returns: false, kind }
}

fn validate_float_literal<'a>(context: &mut Context<'a, '_, '_>, literal: &tree::FloatLiteral, span: Span) -> Expression<'a> {
	let value = DecimalValue::new(literal.value.item, literal.value.span);
	let kind = ExpressionKind::DecimalValue(value);
	let type_id = context.type_store.decimal_type_id();
	Expression { span, type_id, mutable: true, returns: false, kind }
}

fn validate_bool_literal<'a>(context: &mut Context<'a, '_, '_>, literal: bool, span: Span) -> Expression<'a> {
	let type_id = context.type_store.bool_type_id();
	let kind = ExpressionKind::BooleanLiteral(literal);
	Expression { span, type_id, mutable: true, returns: false, kind }
}

fn validate_codepoint_literal<'a>(
	context: &mut Context<'a, '_, '_>,
	literal: &tree::CodepointLiteral,
	span: Span,
) -> Expression<'a> {
	let kind = ExpressionKind::CodepointLiteral(CodepointLiteral { value: literal.value.item });
	let type_id = context.type_store.u32_type_id();
	Expression { span, type_id, mutable: true, returns: false, kind }
}

fn validate_string_literal<'a>(
	context: &mut Context<'a, '_, '_>,
	literal: &tree::StringLiteral<'a>,
	span: Span,
) -> Expression<'a> {
	let kind = ExpressionKind::StringLiteral(StringLiteral { value: literal.value.item.clone() });
	let type_id = context.type_store.string_type_id();
	Expression { span, type_id, mutable: true, returns: false, kind }
}

fn validate_array_literal<'a>(
	context: &mut Context<'a, '_, '_>,
	literal: &'a tree::ArrayLiteral<'a>,
	span: Span,
) -> Expression<'a> {
	let mut returns = false;
	let mut expressions = Vec::with_capacity(literal.expressions.len());
	for expression in &literal.expressions {
		let expression = validate_expression(context, expression);
		returns |= expression.returns;
		expressions.push(expression);
	}

	let pointee_type_id = expressions.first().expect("TODO: Support item-less array literal").type_id;
	for expression in &mut expressions {
		let collapsed = context.collapse_to(pointee_type_id, expression);
		if !collapsed.unwrap_or(true) {
			let error = error!(
				"Type mismatch for array entry, expected {} but got {}",
				context.type_name(pointee_type_id),
				context.type_name(expression.type_id)
			);
			context.message(error.span(expression.span));
		}
	}

	let type_id = context.type_store.slice_of(pointee_type_id, true);
	let literal = ArrayLiteral { type_id, pointee_type_id, expressions };
	let kind = ExpressionKind::ArrayLiteral(literal);
	Expression { span, type_id, mutable: true, returns, kind }
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

	let mut returns = false;
	let mut field_initializers = Vec::new();

	for intializer in &literal.initializer.item.field_initializers {
		let mut expression = validate_expression(context, &intializer.expression);
		returns |= expression.returns;

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
	Expression { span, type_id, mutable: true, returns, kind }
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

	let mut returns = false;
	let mut arguments = Vec::with_capacity(call.arguments.len());
	for argument in &call.arguments {
		let argument = validate_expression(context, argument);
		returns |= argument.returns;
		arguments.push(argument);
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

		let collapsed = context
			.type_store
			.collapse_to(context.messages, context.function_store, parameter.type_id, argument)
			.unwrap_or(true);

		if !collapsed {
			let error = error!(
				"Expected argument of type {}, got {}",
				context.type_name(parameter.type_id),
				context.type_name(argument.type_id)
			);
			context.messages.message(error.span(argument.span));
			arguments_type_mismatch = true;
		}
	}

	if shape.c_varargs {
		if arguments.len() < specialization.parameters.len() {
			let error = error!("Expected at least {} arguments, got {}", specialization.parameters.len(), arguments.len());
			context.message(error.span(span));
			return Expression::any_collapse(context.type_store, span);
		}

		let mut vararg_error = false;
		let remaining_arguments = &arguments[specialization.parameters.len()..];
		for argument in remaining_arguments {
			let is_integer = argument.type_id.is_untyped_integer(context.type_store);
			let is_decimal = argument.type_id.is_untyped_decimal(context.type_store);
			if is_integer || is_decimal {
				let error = error!("Cannot pass untyped numeral as vararg, try casting it to a concrete type first");
				context.message(error.span(argument.span));
				vararg_error = true;
			}
		}

		if vararg_error {
			return Expression::any_collapse(context.type_store, span);
		}
	} else if arguments.len() != specialization.parameters.len() {
		let error = error!("Expected {} arguments, got {}", specialization.parameters.len(), arguments.len());
		context.message(error.span(span));
		return Expression::any_collapse(context.type_store, span);
	}

	if arguments_type_mismatch {
		return Expression::any_collapse(context.type_store, span);
	}

	let function_id = FunctionId { function_shape_index, specialization_index };
	let kind = ExpressionKind::Call(Call { span, name, function_id, arguments });
	Expression { span, type_id: return_type, mutable: true, returns, kind }
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

			return Expression { span, type_id, mutable: false, returns: false, kind };
		}

		SymbolKind::Static { static_index } => {
			let static_instance = &context.statics.statics[static_index];
			let static_read = StaticRead {
				name: static_instance.name,
				type_id: static_instance.type_id,
				static_index,
			};

			let type_id = static_instance.type_id;
			let kind = ExpressionKind::StaticRead(static_read);
			return Expression { span, type_id, mutable: false, returns: false, kind };
		}

		SymbolKind::BuiltinType { type_id } if type_id.is_void(context.type_store) => {
			return Expression::void(context.type_store, span);
		}

		kind => {
			context.message(error!("Cannot read value from {kind}").span(read.path_segments.span));
			return Expression::any_collapse(context.type_store, span);
		}
	};

	let Some(readable) = context.readables.get(readable_index) else {
		panic!("Symbol pointed to unknown readable index {readable_index}, {read:?}");
	};

	let mutable = readable.kind == ReadableKind::Mut;
	let read = Read {
		name: readable.name,
		type_id: readable.type_id,
		readable_index,
	};

	let type_id = readable.type_id;
	let kind = ExpressionKind::Read(read);
	Expression { span, type_id, mutable, returns: false, kind }
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
				type_id: context.type_store.pointer_to(as_slice.type_id, as_slice.mutable),
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

	let type_id = field.type_id;
	let mutable = base.mutable;
	let field_read = FieldRead { base, name: field.name, type_id, field_index };
	let kind = ExpressionKind::FieldRead(Box::new(field_read));
	Expression { span, type_id, mutable, returns: false, kind }
}

fn validate_unary_operation<'a>(
	context: &mut Context<'a, '_, '_>,
	operation: &'a tree::UnaryOperation<'a>,
	span: Span,
) -> Expression<'a> {
	let mut expression = validate_expression(context, &operation.expression);
	let op = match &operation.op.item {
		tree::UnaryOperator::Negate => UnaryOperator::Negate,
		tree::UnaryOperator::Invert => UnaryOperator::Invert,
		tree::UnaryOperator::AddressOf => UnaryOperator::AddressOf,
		tree::UnaryOperator::AddressOfMut => UnaryOperator::AddressOfMut,
		tree::UnaryOperator::Dereference => UnaryOperator::Dereference,

		tree::UnaryOperator::Cast { parsed_type } => {
			return validate_cast(context, expression, parsed_type, span);
		}

		tree::UnaryOperator::Index { index_expression } => {
			return validate_bracket_index(context, expression, index_expression, span);
		}
	};

	match (&op, &mut expression.kind) {
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

		(UnaryOperator::Invert, ExpressionKind::BooleanLiteral(value)) => {
			*value = !*value;
			expression.span = expression.span + span;
			return expression;
		}

		_ => {}
	}

	let type_id = expression.type_id;
	let returns = expression.returns;

	if matches!(op, UnaryOperator::AddressOf | UnaryOperator::AddressOfMut) {
		if type_id.is_untyped_integer(context.type_store) || type_id.is_untyped_decimal(context.type_store) {
			let error = error!("Cannot take address of untyped numeral, try casting it to a concrete type first");
			context.message(error.span(span));
			return Expression::any_collapse(context.type_store, span);
		}
	}

	match op {
		UnaryOperator::Negate => {
			if !type_id.is_numeric(context.type_store) {
				let error = error!("Cannot negate {} as it is not a numeric type", context.type_name(type_id));
				context.message(error.span(span));
				return Expression::any_collapse(context.type_store, span);
			}

			let kind = ExpressionKind::UnaryOperation(Box::new(UnaryOperation { op, type_id, expression }));
			return Expression { span, type_id, mutable: true, returns, kind };
		}

		UnaryOperator::Invert => {
			if !context.type_store.direct_match(type_id, context.type_store.bool_type_id()) {
				if !type_id.is_any_collapse(context.type_store) {
					let error = error!("Cannot invert {} as it is not a boolean", context.type_name(type_id));
					context.message(error.span(span));
				}
				return Expression::any_collapse(context.type_store, span);
			}

			let kind = ExpressionKind::UnaryOperation(Box::new(UnaryOperation { op, type_id, expression }));
			return Expression { span, type_id, mutable: true, returns, kind };
		}

		UnaryOperator::AddressOf => {
			let type_id = context.type_store.pointer_to(type_id, false);
			let kind = ExpressionKind::UnaryOperation(Box::new(UnaryOperation { op, type_id, expression }));
			return Expression { span, type_id, mutable: true, returns, kind };
		}

		UnaryOperator::AddressOfMut => {
			if !expression.mutable {
				let error = error!("Cannot take mutable address of immutable value");
				context.message(error.span(span));
				return Expression::any_collapse(context.type_store, span);
			}

			let type_id = context.type_store.pointer_to(type_id, true);
			let kind = ExpressionKind::UnaryOperation(Box::new(UnaryOperation { op, type_id, expression }));
			return Expression { span, type_id, mutable: true, returns, kind };
		}

		UnaryOperator::Dereference => {
			let Some((type_id, mutable)) = context.type_store.pointed_to(type_id) else {
				let error = error!("Cannot dereference {} as it is not a pointer", context.type_name(type_id));
				context.message(error.span(span));
				return Expression::any_collapse(context.type_store, span);
			};

			let kind = ExpressionKind::UnaryOperation(Box::new(UnaryOperation { op, type_id, expression }));
			return Expression { span, type_id, mutable, returns, kind };
		}

		UnaryOperator::Cast { .. } => unreachable!(),
		UnaryOperator::Index { .. } => unreachable!(),
	}
}

fn validate_cast<'a>(
	context: &mut Context<'a, '_, '_>,
	mut expression: Expression<'a>,
	parsed_type: &Node<tree::Type<'a>>,
	span: Span,
) -> Expression<'a> {
	let any_collapse = context.type_store.any_collapse_type_id();
	let from_type_id = expression.type_id;
	let to_type_id = context.lookup_type(parsed_type).unwrap_or(any_collapse);

	if context.type_store.direct_match(from_type_id, to_type_id) {
		let warning = warning!("Unnecessary cast from {} to itself", context.type_name(from_type_id));
		context.message(warning.span(span));
	} else if from_type_id.is_pointer(context.type_store) && to_type_id.is_pointer(context.type_store) {
		let (from_pointed, from_mutable) = context.type_store.pointed_to(from_type_id).unwrap();
		let (to_pointed, to_mutable) = context.type_store.pointed_to(to_type_id).unwrap();

		if to_mutable && !from_mutable {
			let error = error!(
				"Cannot cast from immutable pointer {} to mutable pointer {}",
				context.type_name(from_type_id),
				context.type_name(to_type_id)
			);
			context.message(error.span(span));
		} else if context.type_store.direct_match(from_pointed, to_pointed) {
			// If they were not the same type id, yet they point to the same type, then we must be casting from mut to non-mut
			let warning = warning!(
				"Unnecessary cast from mutable pointer {} to immutable pointer {}",
				context.type_name(from_type_id),
				context.type_name(to_type_id)
			);
			context.message(warning.span(span));
		}
	}

	let from_numeric = from_type_id.is_numeric(context.type_store);
	let to_numeric = to_type_id.is_numeric(context.type_store);
	let from_pointer = from_type_id.is_pointer(context.type_store);
	let to_pointer = to_type_id.is_pointer(context.type_store);

	if from_numeric && to_numeric {
		let from_untyped_integer = from_type_id.is_untyped_integer(context.type_store);
		let from_untyped_decimal = from_type_id.is_untyped_decimal(context.type_store);
		let from_untyped = from_untyped_integer || from_untyped_decimal;

		let to_untyped_integer = to_type_id.is_untyped_integer(context.type_store);
		let to_untyped_decimal = to_type_id.is_untyped_decimal(context.type_store);
		let to_untyped = to_untyped_integer || to_untyped_decimal;

		if from_untyped && !to_untyped {
			context.collapse_to(to_type_id, &mut expression);
		}
	} else if from_pointer && to_pointer {
	} else if from_pointer && to_numeric {
	} else if from_numeric && to_pointer {
		if from_type_id.is_untyped_decimal(context.type_store) {
			let error = error!("Cannot cast untyped decimal to a pointer");
			context.message(error.span(span));
		} else {
			let is_i64 = context
				.type_store
				.direct_match(from_type_id, context.type_store.i64_type_id());
			let is_u64 = context
				.type_store
				.direct_match(from_type_id, context.type_store.u64_type_id());
			let is_usize = context
				.type_store
				.direct_match(from_type_id, context.type_store.usize_type_id());
			let is_untyped_integer = from_type_id.is_untyped_integer(context.type_store);

			if !is_i64 && !is_u64 && !is_usize && !is_untyped_integer {
				let error = error!("Cannot cast {} to a pointer as it is too small", context.type_name(from_type_id));
				context.message(error.span(span));
			} else if from_type_id.is_untyped_integer(context.type_store) {
				context.collapse_to(context.type_store.usize_type_id(), &mut expression);
			}
		}
	} else {
		let error = if from_numeric || from_pointer {
			assert!(!to_numeric);
			assert!(!to_pointer);
			let to = context.type_name(to_type_id);
			error!("Cannot cast to a value which is not numeric or a pointer")
				.note(note!(parsed_type.span, "Attempted to cast to type {to}"))
		} else if to_numeric || to_pointer {
			assert!(!from_numeric);
			assert!(!from_pointer);
			let from = context.type_name(from_type_id);
			error!("Cannot cast from a value which is not numeric or a pointer")
				.note(note!(expression.span, "Attempted to cast from type {from}"))
		} else {
			let to = context.type_name(to_type_id);
			let from = context.type_name(from_type_id);
			error!("Cannot cast between values which are not numeric or a pointer")
				.note(note!(expression.span, "Attempted to cast from type {from}"))
				.note(note!(parsed_type.span, "Attempted to cast to type {to}"))
		};
		context.message(error.span(span));
	}

	let type_id = to_type_id;
	let returns = expression.returns;
	let op = UnaryOperator::Cast { type_id };
	let kind = ExpressionKind::UnaryOperation(Box::new(UnaryOperation { op, type_id, expression }));
	Expression { span, type_id, mutable: true, returns, kind }
}

fn validate_bracket_index<'a>(
	context: &mut Context<'a, '_, '_>,
	expression: Expression<'a>,
	index_expression: &'a Node<tree::Expression<'a>>,
	span: Span,
) -> Expression<'a> {
	let mut index_expression = validate_expression(context, index_expression);
	let (type_id, mutable) = match context.type_store.sliced_of(expression.type_id) {
		Some((type_id, mutable)) => (type_id, mutable),

		None => {
			// TODO: Once arrays have an actual type this needs to account for that
			let error = error!("Cannot index on a value of type {}", context.type_name(expression.type_id));
			context.message(error.span(span));
			(context.type_store.any_collapse_type_id(), true)
		}
	};

	let i64_type_id = context.type_store.i64_type_id();
	let collapsed = context.collapse_to(i64_type_id, &mut index_expression);
	if !collapsed.unwrap_or(true) {
		let error = error!("Cannot index by a value of {}", context.type_name(index_expression.type_id));
		context.message(error.span(index_expression.span));
	}

	let returns = expression.returns || index_expression.returns;
	let op = UnaryOperator::Index { index_expression };
	let kind = ExpressionKind::UnaryOperation(Box::new(UnaryOperation { op, type_id, expression }));
	Expression { span, type_id, mutable, returns, kind }
}

fn validate_binary_operation<'a>(
	context: &mut Context<'a, '_, '_>,
	operation: &'a tree::BinaryOperation<'a>,
	span: Span,
) -> Expression<'a> {
	let op = operation.op.item;

	let mut left = validate_expression(context, &operation.left);
	let mut right = validate_expression(context, &operation.right);
	let collapsed = context.collapse_fair(&mut left, &mut right);

	let Some(collapsed) = collapsed else {
		context.message(
			error!("{} type mismatch", op.name())
				.span(span)
				.note(note!(operation.left.span, "Left type {}", context.type_name(left.type_id)))
				.note(note!(operation.right.span, "Right type {}", context.type_name(right.type_id))),
		);
		return Expression::any_collapse(context.type_store, span);
	};

	if let Some(constant_operation) = perform_constant_binary_operation(context, &left, &right, op) {
		return constant_operation;
	}

	if op == BinaryOperator::Assign {
		if let ExpressionKind::Read(read) = &left.kind {
			let readable = match context.readables.get(read.readable_index) {
				Some(readable) => readable,
				None => return Expression::any_collapse(context.type_store, span),
			};

			if readable.kind != ReadableKind::Mut {
				context.message(error!("Cannot assign to immutable binding `{}`", read.name).span(span));
			}
		} else if let ExpressionKind::FieldRead(_) = &left.kind {
			if !left.mutable {
				context.message(error!("Cannot assign to field of immutable object").span(span));
			}
		} else if matches!(&left.kind, ExpressionKind::UnaryOperation(op) if matches!(op.as_ref(), UnaryOperation { op: UnaryOperator::Dereference, .. }))
		{
			if !left.mutable {
				context.message(error!("Cannot assign immutable memory location").span(span));
			}
		} else if matches!(&left.kind, ExpressionKind::UnaryOperation(op) if matches!(op.as_ref(), UnaryOperation { op: UnaryOperator::Index { .. }, .. }))
		{
			if !left.mutable {
				context.message(error!("Cannot assign to index of immutable slice").span(span));
			}
		} else {
			context.message(error!("Cannot assign to {}", left.kind.name_with_article()).span(span));
		}
	}

	match op {
		BinaryOperator::Assign => {}

		BinaryOperator::Add
		| BinaryOperator::AddAssign
		| BinaryOperator::Sub
		| BinaryOperator::SubAssign
		| BinaryOperator::Mul
		| BinaryOperator::MulAssign
		| BinaryOperator::Div
		| BinaryOperator::DivAssign
		| BinaryOperator::Modulo
		| BinaryOperator::ModuloAssign => {
			if matches!(op, BinaryOperator::Modulo | BinaryOperator::ModuloAssign) {
				if !left.type_id.is_integer(context.type_store) {
					let found = context.type_name(left.type_id);
					let error = error!("Cannot perform modulo on non-integer type {found}");
					context.message(error.span(span));
					return Expression::any_collapse(context.type_store, span);
				}
			} else if !left.type_id.is_numeric(context.type_store) {
				let found = context.type_name(left.type_id);
				let error = error!("Cannot perform arithmetic on non-numerical type {found}");
				context.message(error.span(span));
				return Expression::any_collapse(context.type_store, span);
			}
		}

		BinaryOperator::BitshiftLeft
		| BinaryOperator::BitshiftLeftAssign
		| BinaryOperator::BitshiftRight
		| BinaryOperator::BitshiftRightAssign => {
			if !left.type_id.is_numeric(context.type_store) {
				let found = context.type_name(left.type_id);
				let error = error!("Cannot perform bitshift on non-numerical type {found}");
				context.message(error.span(span));
				return Expression::any_collapse(context.type_store, span);
			}
		}

		BinaryOperator::Equals | BinaryOperator::NotEquals => {
			if !left.type_id.is_primative(context.type_store) {
				let found = context.type_name(left.type_id);
				let error = error!("Cannot perform equality comparison on non-primative type {found}");
				context.message(error.span(span));
				return Expression::any_collapse(context.type_store, span);
			}
		}

		BinaryOperator::GreaterThan
		| BinaryOperator::GreaterThanEquals
		| BinaryOperator::LessThan
		| BinaryOperator::LessThanEquals => {
			if !left.type_id.is_numeric(context.type_store) {
				let found = context.type_name(left.type_id);
				let error = error!("Cannot perform comparison on non-numerical type {found}");
				context.message(error.span(span));
				return Expression::any_collapse(context.type_store, span);
			}
		}

		BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr => {
			let boolean = context.type_store.bool_type_id();
			if !context.type_store.direct_match(left.type_id, boolean) {
				let found = context.type_name(left.type_id);
				let error = error!("Cannot perform logical operation on non-boolean type {found}");
				context.message(error.span(span));
				return Expression::any_collapse(context.type_store, span);
			}
		}
	}

	let type_id = match op {
		BinaryOperator::Assign
		| BinaryOperator::AddAssign
		| BinaryOperator::SubAssign
		| BinaryOperator::MulAssign
		| BinaryOperator::DivAssign
		| BinaryOperator::ModuloAssign
		| BinaryOperator::BitshiftLeftAssign
		| BinaryOperator::BitshiftRightAssign => context.type_store.void_type_id(),

		BinaryOperator::Equals
		| BinaryOperator::NotEquals
		| BinaryOperator::GreaterThan
		| BinaryOperator::GreaterThanEquals
		| BinaryOperator::LessThan
		| BinaryOperator::LessThanEquals => context.type_store.bool_type_id(),

		_ if matches!(left.kind, ExpressionKind::AnyCollapse) || matches!(right.kind, ExpressionKind::AnyCollapse) => {
			context.type_store.any_collapse_type_id()
		}

		_ => collapsed,
	};

	let returns = left.returns || right.returns;
	let operation = Box::new(BinaryOperation { op, left, right, type_id });
	let kind = ExpressionKind::BinaryOperation(operation);
	Expression { span, type_id, mutable: true, returns, kind }
}

fn perform_constant_binary_operation<'a>(
	context: &mut Context<'a, '_, '_>,
	left_expression: &Expression,
	right_expression: &Expression,
	op: BinaryOperator,
) -> Option<Expression<'a>> {
	if let ExpressionKind::IntegerValue(left) = &left_expression.kind {
		let right = match &right_expression.kind {
			ExpressionKind::IntegerValue(right) => *right,
			ExpressionKind::DecimalValue(..) => unreachable!(),
			_ => return None,
		};

		let value = match op {
			BinaryOperator::Add => left.add(context.messages, right)?,
			BinaryOperator::Sub => left.sub(context.messages, right)?,
			BinaryOperator::Mul => left.mul(context.messages, right)?,
			BinaryOperator::Div => left.div(context.messages, right)?,
			BinaryOperator::Modulo => left.modulo(context.messages, right)?,
			_ => return None,
		};

		let span = value.span();
		let kind = ExpressionKind::IntegerValue(value);
		let type_id = context.type_store.integer_type_id();
		return Some(Expression { span, type_id, mutable: true, returns: false, kind });
	}

	if let ExpressionKind::DecimalValue(left) = &left_expression.kind {
		let right = match &right_expression.kind {
			ExpressionKind::DecimalValue(right) => *right,
			ExpressionKind::IntegerValue(..) => unreachable!(),
			_ => return None,
		};

		let value = match op {
			BinaryOperator::Add => left.add(right),
			BinaryOperator::Sub => left.sub(right),
			BinaryOperator::Mul => left.mul(right),
			BinaryOperator::Div => left.div(right),
			_ => return None,
		};

		let span = value.span();
		let kind = ExpressionKind::DecimalValue(value);
		let type_id = context.type_store.decimal_type_id();
		return Some(Expression { span, type_id, mutable: true, returns: false, kind });
	}

	if let ExpressionKind::BooleanLiteral(left) = &left_expression.kind {
		let right = match &right_expression.kind {
			ExpressionKind::BooleanLiteral(right) => *right,
			_ => return None,
		};

		let value = match op {
			BinaryOperator::LogicalAnd => *left && right,
			BinaryOperator::LogicalOr => *left || right,
			_ => return None,
		};

		let span = left_expression.span + right_expression.span;
		let type_id = context.type_store.bool_type_id();
		let kind = ExpressionKind::BooleanLiteral(value);
		return Some(Expression { span, type_id, mutable: true, returns: false, kind });
	}

	None
}
