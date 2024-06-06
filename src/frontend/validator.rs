use std::sync::Arc;

use bumpalo_herd::Herd;
use rustc_hash::FxHashMap;

use crate::cli::CliArguments;
use crate::frontend::error::*;
use crate::frontend::function_store::FunctionStore;
use crate::frontend::ir::*;
use crate::frontend::lang_items::LangItems;
use crate::frontend::root_layers::RootLayers;
use crate::frontend::span::Span;
use crate::frontend::symbols::SymbolsScope;
use crate::frontend::symbols::{Externs, ReadableKind, Readables, Statics, Symbol, SymbolKind, Symbols};
use crate::frontend::tree::{self, BinaryOperator, EnumInitializer, FieldAttribute, MethodKind, PathSegments};
use crate::frontend::tree::{MethodAttribute, Node};
use crate::frontend::type_store::*;
use crate::lock::RwLock;

#[derive(Debug)]
pub struct Context<'a, 'b, 'c> {
	pub cli_arguments: &'a CliArguments,
	pub herd_member: &'b bumpalo_herd::Member<'a>,

	pub file_index: usize,
	pub module_path: &'a [String],

	pub next_scope_index: &'b mut usize,
	pub scope_index: usize,

	pub messages: &'b mut Messages<'a>,

	pub type_shape_indicies: &'b mut Vec<usize>,

	pub type_store: &'b TypeStore<'a>,
	pub function_store: &'b FunctionStore<'a>,
	pub function_generic_usages: &'b mut Vec<GenericUsage>,

	pub root_layers: &'b RootLayers<'a>,

	pub lang_items: &'b RwLock<LangItems>,
	pub externs: &'b RwLock<Externs>,
	pub constants: &'b RwLock<Vec<ConstantValue<'a>>>,
	pub statics: &'b RwLock<Statics<'a>>,
	pub initial_readables_starting_index: usize,
	pub initial_readables_overall_len: usize,
	pub readables: &'b mut Readables<'a>,

	pub initial_local_function_shape_indicies_len: usize,
	pub local_function_shape_indicies: &'b mut Vec<usize>,

	pub function_initial_scope_count: usize,
	pub symbols_scope: SymbolsScope<'a, 'b>,

	pub next_loop_index: usize,
	pub current_loop_index: Option<usize>,

	pub can_is_bind: bool,
	pub expected_type: Option<TypeId>,

	pub return_type: Option<TypeId>,
	pub generic_parameters: &'c GenericParameters<'a>,
	pub method_base_index: Option<usize>,
}

impl<'a, 'b, 'c> Drop for Context<'a, 'b, 'c> {
	fn drop(&mut self) {
		self.readables.readables.truncate(self.initial_readables_overall_len);
		self.readables.starting_index = self.initial_readables_starting_index;
	}
}

impl<'a, 'b, 'c> Context<'a, 'b, 'c> {
	fn child_scope<'s>(&'s mut self) -> Context<'a, 's, 'c> {
		let scope_index = *self.next_scope_index;
		*self.next_scope_index += 1;

		Context {
			cli_arguments: self.cli_arguments,
			herd_member: self.herd_member,

			file_index: self.file_index,
			module_path: self.module_path,

			next_scope_index: self.next_scope_index,
			scope_index,

			messages: self.messages,

			type_shape_indicies: self.type_shape_indicies,

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

			initial_local_function_shape_indicies_len: self.local_function_shape_indicies.len(),
			local_function_shape_indicies: self.local_function_shape_indicies,

			function_initial_scope_count: self.function_initial_scope_count,
			symbols_scope: self.symbols_scope.child_scope(),

			next_loop_index: self.next_loop_index,
			current_loop_index: self.current_loop_index,

			can_is_bind: false,
			expected_type: self.expected_type,

			return_type: self.return_type,
			generic_parameters: self.generic_parameters,
			method_base_index: self.method_base_index,
		}
	}

	fn child_scope_for_function<'s, 't>(
		&'s mut self,
		return_type: TypeId,
		generic_parameters: &'t GenericParameters<'a>,
	) -> Context<'a, 's, 't> {
		let scope_index = *self.next_scope_index;
		*self.next_scope_index += 1;

		let initial_readables_starting_index = self.readables.starting_index;
		self.readables.starting_index = self.readables.overall_len();

		Context {
			cli_arguments: self.cli_arguments,
			herd_member: self.herd_member,

			file_index: self.file_index,
			module_path: self.module_path,

			next_scope_index: self.next_scope_index,
			scope_index,

			messages: self.messages,

			type_shape_indicies: self.type_shape_indicies,

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

			initial_local_function_shape_indicies_len: self.local_function_shape_indicies.len(),
			local_function_shape_indicies: self.local_function_shape_indicies,

			function_initial_scope_count: self.function_initial_scope_count,
			symbols_scope: self.symbols_scope.child_scope(),

			next_loop_index: 0,
			current_loop_index: None,

			can_is_bind: false,
			expected_type: None,

			return_type: Some(return_type),
			generic_parameters,
			method_base_index: self.method_base_index,
		}
	}

	pub fn message(&mut self, message: Message) {
		self.messages.message(message);
	}

	pub fn push_symbol(&mut self, symbol: Symbol<'a>) {
		self.symbols_scope
			.symbols
			.push_symbol(self.messages, self.function_initial_scope_count, symbol);
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
		self.symbols_scope.symbols.lookup_symbol(
			self.messages,
			self.root_layers,
			self.type_store,
			self.function_initial_scope_count,
			path,
		)
	}

	pub fn lookup_type(&mut self, parsed_type: &Node<tree::Type<'a>>) -> Option<TypeId> {
		self.type_store.lookup_type(
			self.messages,
			self.function_store,
			self.module_path,
			self.function_generic_usages,
			self.root_layers,
			self.symbols_scope.symbols,
			self.function_initial_scope_count,
			self.generic_parameters,
			parsed_type,
		)
	}

	pub fn get_enum_variant(&mut self, base: TypeId, name: Node<&'a str>) -> Option<TypeId> {
		self.type_store
			.get_enum_variant(self.messages, self.function_store, self.module_path, base, name)
	}

	pub fn local_function_shape_index(&self, index: usize) -> usize {
		self.local_function_shape_indicies[self.initial_local_function_shape_indicies_len + index]
	}

	pub fn type_name(&self, type_id: TypeId) -> String {
		self.type_store.type_name(self.function_store, self.module_path, type_id)
	}

	pub fn collapse_fair(&mut self, a: &mut Expression<'a>, b: &mut Expression<'a>) -> Result<TypeId, ()> {
		self.type_store.collapse_fair(self.messages, self.function_store, a, b)
	}

	pub fn collapse_to(&mut self, to: TypeId, from: &mut Expression<'a>) -> Result<bool, ()> {
		self.type_store.collapse_to(self.messages, self.function_store, to, from)
	}
}

pub fn validate<'a>(
	cli_arguments: &'a CliArguments,
	herd: &'a Herd,
	messages: &mut Messages<'a>,
	lang_items: &RwLock<LangItems>,
	root_layers: &mut RootLayers<'a>,
	type_store: &TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	statics: &RwLock<Statics<'a>>,
	parsed_files: &'a [tree::File<'a>],
) {
	let herd_member = herd.get();
	let mut function_generic_usages = Vec::new();
	let externs = RwLock::new(Externs::new());
	let mut constants = RwLock::new(Vec::new());

	let mut type_shape_indicies = Vec::with_capacity(parsed_files.len());
	for _ in 0..parsed_files.len() {
		type_shape_indicies.push(Vec::new());
	}

	create_root_types(messages, type_store, root_layers, parsed_files, type_shape_indicies.as_mut_slice());
	resolve_root_type_imports(cli_arguments, &herd_member, messages, root_layers, parsed_files);
	fill_root_types(
		messages,
		type_store,
		function_store,
		root_layers,
		parsed_files,
		type_shape_indicies.as_mut_slice(),
	);

	let mut local_function_shape_indicies = Vec::new();
	for _ in 0..parsed_files.len() {
		local_function_shape_indicies.push(Vec::new());
	}

	let mut readables = Readables::new();
	create_root_functions(
		messages,
		lang_items,
		root_layers,
		type_store,
		function_store,
		&mut function_generic_usages,
		&externs,
		&mut readables,
		parsed_files,
		&mut local_function_shape_indicies,
	);
	function_generic_usages.clear();

	validate_root_consts(
		cli_arguments,
		&herd_member,
		messages,
		lang_items,
		root_layers,
		type_store,
		function_store,
		&mut function_generic_usages,
		&externs,
		&constants,
		statics,
		&mut readables,
		parsed_files,
		type_shape_indicies.as_mut_slice(),
	);

	validate_root_statics(
		cli_arguments,
		&herd_member,
		messages,
		lang_items,
		root_layers,
		type_store,
		function_store,
		&mut function_generic_usages,
		&externs,
		&mut constants,
		statics,
		&mut readables,
		parsed_files,
		type_shape_indicies.as_mut_slice(),
	);

	assert_eq!(function_generic_usages.len(), 0);

	let root_layers: &RootLayers = root_layers;
	let type_store: &TypeStore = type_store;

	let parsed_files = parking_lot::Mutex::new((parsed_files.iter(), local_function_shape_indicies.into_iter()));

	std::thread::scope(|scope| {
		for _ in 0..6 {
			let parsed_files = &parsed_files;
			let mut messages = messages.fork();
			let externs = &externs;
			let constants = &constants;

			scope.spawn(move || {
				let herd_member = herd.get();

				let mut type_shape_indicies = Vec::new();
				let mut function_generic_usages = Vec::new();
				let mut readables = Readables::new();

				loop {
					let mut parsed_files = parsed_files.lock();
					let parsed_file = parsed_files.0.next();
					let local_function_shape_indicies = parsed_files.1.next();
					drop(parsed_files);
					let Some(parsed_file) = parsed_file else {
						break;
					};
					let mut local_function_shape_indicies = local_function_shape_indicies.unwrap();

					let file_index = parsed_file.source_file.index;
					let module_path = parsed_file.module_path;

					let layer = root_layers.lookup_module_path(module_path);
					let mut symbols = layer.symbols.clone();

					readables.starting_index = 0;
					readables.readables.clear();

					let mut next_scope_index = 1;
					let blank_generic_parameters = GenericParameters::new_from_explicit(Vec::new());
					let context = Context {
						cli_arguments,
						herd_member: &herd_member,
						file_index,
						module_path,
						next_scope_index: &mut next_scope_index,
						scope_index: 0,
						messages: &mut messages,
						type_shape_indicies: &mut type_shape_indicies,
						type_store,
						function_store,
						function_generic_usages: &mut function_generic_usages,
						root_layers,
						lang_items,
						externs,
						constants,
						statics,
						initial_readables_starting_index: 0,
						initial_readables_overall_len: 0,
						readables: &mut readables,
						initial_local_function_shape_indicies_len: 0,
						local_function_shape_indicies: &mut local_function_shape_indicies,
						function_initial_scope_count: symbols.scopes.len(),
						symbols_scope: symbols.child_scope(),
						next_loop_index: 0,
						current_loop_index: None,
						can_is_bind: false,
						expected_type: None,
						return_type: None,
						generic_parameters: &blank_generic_parameters,
						method_base_index: None,
					};

					validate_block(context, &parsed_file.block, true);

					function_generic_usages.clear();
				}
			});
		}
	});

	if function_store.main.read().is_none() {
		let error = error!("Project has no main function, is it missing or in the wrong file for project name?");
		messages.message(error);
	}
}

fn create_root_types<'a>(
	messages: &mut Messages,
	type_store: &TypeStore<'a>,
	root_layers: &mut RootLayers<'a>,
	parsed_files: &[tree::File<'a>],
	type_shape_indicies: &mut [Vec<usize>],
) {
	for parsed_file in parsed_files.iter() {
		let file_index = parsed_file.source_file.index;
		let type_shape_indicies = &mut type_shape_indicies[file_index];
		let layer = root_layers.create_module_path(parsed_file.module_path);

		let blank_generic_parameters = GenericParameters::new_from_explicit(Vec::new());
		let block = &parsed_file.block;
		let scope_id = ScopeId { file_index, scope_index: 0 };

		let importable_types_index = layer.symbols.scopes.len();
		assert_eq!(importable_types_index, 0);
		layer.symbols.scopes.push(FxHashMap::default());

		create_block_types(
			messages,
			type_store,
			&mut layer.symbols,
			0,
			parsed_file.module_path,
			&blank_generic_parameters,
			block,
			scope_id,
			true,
			type_shape_indicies,
		);

		layer.importable_types_index = importable_types_index;
	}
}

fn resolve_root_type_imports<'a>(
	cli_arguments: &CliArguments,
	herd_member: &bumpalo_herd::Member<'a>,
	messages: &mut Messages,
	root_layers: &mut RootLayers<'a>,
	parsed_files: &[tree::File<'a>],
) {
	for parsed_file in parsed_files {
		let layer = root_layers.create_module_path(parsed_file.module_path);
		let mut symbols = layer.symbols.clone(); // Belch

		let module_path = parsed_file.module_path;
		let block = &parsed_file.block;
		resolve_block_type_imports(
			cli_arguments,
			herd_member,
			messages,
			root_layers,
			&mut symbols,
			module_path,
			0,
			block,
			true,
		);

		root_layers.create_module_path(parsed_file.module_path).symbols = symbols;
	}
}

fn fill_root_types<'a>(
	messages: &mut Messages<'a>,
	type_store: &TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	root_layers: &mut RootLayers<'a>,
	parsed_files: &[tree::File<'a>],
	type_shape_indicies: &mut [Vec<usize>],
) {
	for parsed_file in parsed_files {
		let layer = root_layers.create_module_path(parsed_file.module_path);
		let mut symbols = layer.symbols.clone(); // Belch
		let type_shape_indicies = &mut type_shape_indicies[parsed_file.source_file.index];

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
			type_shape_indicies,
		);

		let layer = root_layers.create_module_path(parsed_file.module_path);
		layer.symbols = symbols;
	}
}

fn create_root_functions<'a>(
	messages: &mut Messages<'a>,
	lang_items: &RwLock<LangItems>,
	root_layers: &mut RootLayers<'a>,
	type_store: &TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	generic_usages: &mut Vec<GenericUsage>,
	externs: &RwLock<Externs>,
	readables: &mut Readables<'a>,
	parsed_files: &'a [tree::File<'a>],
	local_function_shape_indicies: &mut Vec<Vec<usize>>,
) {
	for (iteration_index, parsed_file) in parsed_files.iter().enumerate() {
		let block = &parsed_file.block;

		let file_index = parsed_file.source_file.index;
		let scope_id = ScopeId { file_index, scope_index: 0 };

		//Yuck, I do not like this
		let mut symbols = root_layers.create_module_path(parsed_file.module_path).symbols.clone();

		let importable_functions_index = symbols.scopes.len();
		assert_eq!(importable_functions_index, 1);
		symbols.scopes.push(FxHashMap::default());

		create_block_functions(
			messages,
			lang_items,
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
			&mut local_function_shape_indicies[iteration_index],
			scope_id,
		);

		let layer = root_layers.create_module_path(parsed_file.module_path);
		layer.importable_functions_index = importable_functions_index;
		layer.symbols = symbols;
	}
}

fn validate_root_consts<'a>(
	cli_arguments: &'a CliArguments,
	herd_member: &bumpalo_herd::Member<'a>,
	messages: &mut Messages<'a>,
	lang_items: &RwLock<LangItems>,
	root_layers: &mut RootLayers<'a>,
	type_store: &TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	function_generic_usages: &mut Vec<GenericUsage>,
	externs: &RwLock<Externs>,
	constants: &RwLock<Vec<ConstantValue<'a>>>,
	statics: &RwLock<Statics<'a>>,
	readables: &mut Readables<'a>,
	parsed_files: &'a [tree::File<'a>],
	type_shape_indicies: &mut [Vec<usize>],
) {
	for parsed_file in parsed_files {
		let file_index = parsed_file.source_file.index;
		let module_path = parsed_file.module_path;
		let type_shape_indicies = &mut type_shape_indicies[file_index];

		let layer = root_layers.create_module_path(parsed_file.module_path);
		let mut symbols = layer.symbols.clone();

		readables.starting_index = 0;
		readables.readables.clear();

		let importable_consts_index = symbols.scopes.len();
		assert_eq!(importable_consts_index, 2);

		let mut next_scope_index = 1;
		let blank_generic_parameters = GenericParameters::new_from_explicit(Vec::new());
		let mut context = Context {
			cli_arguments,
			herd_member,
			file_index,
			module_path,
			next_scope_index: &mut next_scope_index,
			scope_index: 0,
			messages,
			type_shape_indicies,
			type_store,
			function_store,
			function_generic_usages,
			root_layers,
			lang_items,
			externs,
			constants,
			statics,
			initial_local_function_shape_indicies_len: 0,
			local_function_shape_indicies: &mut Vec::new(),
			initial_readables_starting_index: readables.starting_index,
			initial_readables_overall_len: readables.overall_len(),
			readables,
			function_initial_scope_count: symbols.scopes.len(),
			symbols_scope: symbols.child_scope(),
			next_loop_index: 0,
			current_loop_index: None,
			can_is_bind: false,
			expected_type: None,
			return_type: None,
			generic_parameters: &blank_generic_parameters,
			method_base_index: None,
		};

		validate_block_consts(&mut context, &parsed_file.block);
		assert_eq!(context.readables.overall_len(), 0);
		assert_eq!(context.readables.starting_index, 0);

		std::mem::forget(context);
		assert_eq!(next_scope_index, 1);

		let layer = root_layers.create_module_path(parsed_file.module_path);
		layer.importable_consts_index = importable_consts_index;
		layer.symbols = symbols;
	}
}

fn validate_root_statics<'a>(
	cli_arguments: &'a CliArguments,
	herd_member: &bumpalo_herd::Member<'a>,
	messages: &mut Messages<'a>,
	lang_items: &RwLock<LangItems>,
	root_layers: &mut RootLayers<'a>,
	type_store: &TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	function_generic_usages: &mut Vec<GenericUsage>,
	externs: &RwLock<Externs>,
	constants: &RwLock<Vec<ConstantValue<'a>>>,
	statics: &RwLock<Statics<'a>>,
	readables: &mut Readables<'a>,
	parsed_files: &'a [tree::File<'a>],
	type_shape_indicies: &mut [Vec<usize>],
) {
	for parsed_file in parsed_files {
		let file_index = parsed_file.source_file.index;
		let module_path = parsed_file.module_path;
		let type_shape_indicies = &mut type_shape_indicies[file_index];

		let layer = root_layers.create_module_path(parsed_file.module_path);
		let mut symbols = layer.symbols.clone();

		readables.starting_index = 0;
		readables.readables.clear();

		let importable_statics_index = symbols.scopes.len();
		assert_eq!(importable_statics_index, 3);

		let mut next_scope_index = 1;
		let blank_generic_parameters = GenericParameters::new_from_explicit(Vec::new());
		let mut context = Context {
			cli_arguments,
			herd_member,
			file_index,
			module_path,
			next_scope_index: &mut next_scope_index,
			scope_index: 0,
			messages,
			type_shape_indicies,
			type_store,
			function_store,
			function_generic_usages,
			root_layers,
			lang_items,
			externs,
			constants,
			statics,
			initial_local_function_shape_indicies_len: 0,
			local_function_shape_indicies: &mut Vec::new(),
			initial_readables_starting_index: readables.starting_index,
			initial_readables_overall_len: readables.overall_len(),
			readables,
			function_initial_scope_count: symbols.scopes.len(),
			symbols_scope: symbols.child_scope(),
			next_loop_index: 0,
			current_loop_index: None,
			can_is_bind: false,
			expected_type: None,
			return_type: None,
			generic_parameters: &blank_generic_parameters,
			method_base_index: None,
		};

		validate_block_statics(&mut context, &parsed_file.block);
		assert_eq!(context.readables.overall_len(), 0);
		assert_eq!(context.readables.starting_index, 0);

		std::mem::forget(context);
		assert_eq!(next_scope_index, 1);

		let layer = root_layers.create_module_path(parsed_file.module_path);
		layer.importable_statics_index = importable_statics_index;
		layer.symbols = symbols;
	}
}

fn resolve_block_type_imports<'a>(
	cli_arguments: &CliArguments,
	herd_member: &bumpalo_herd::Member<'a>,
	messages: &mut Messages,
	root_layers: &RootLayers<'a>,
	symbols: &mut Symbols<'a>,
	module_path: &'a [String],
	function_initial_scope_count: usize,
	block: &tree::Block<'a>,
	is_root: bool,
) {
	if is_root && cli_arguments.std_enabled && !matches!(module_path, [a, b] if a == "fae" && b == "prelude") {
		let segments = herd_member.alloc([Node::new("fae", Span::unusable()), Node::new("prelude", Span::unusable())]);
		let path = PathSegments { segments };
		resolve_import_for_block_types(messages, root_layers, symbols, function_initial_scope_count, &path, None);
	}

	for statement in block.statements {
		let import_statement = match statement {
			tree::Statement::Import(import_statement) => import_statement,
			_ => continue,
		};

		let path = &import_statement.item.path_segments;
		let names = Some(import_statement.item.symbol_names);
		resolve_import_for_block_types(messages, root_layers, symbols, function_initial_scope_count, path, names);
	}
}

fn resolve_import_for_block_types<'a>(
	messages: &mut Messages,
	root_layers: &RootLayers<'a>,
	symbols: &mut Symbols<'a>,
	function_initial_scope_count: usize,
	path: &PathSegments<'a>,
	names: Option<&[Node<&'a str>]>,
) {
	let Some(layer) = root_layers.layer_for_path(messages, path) else {
		return;
	};

	if let Some(names) = names {
		let importable_types = layer.importable_types();
		for name in names {
			if let Some(&importing) = importable_types.get(name.item) {
				symbols.push_imported_symbol(messages, function_initial_scope_count, importing, Some(name.span));
			}
		}
	} else {
		for &importing in layer.importable_types().values() {
			symbols.push_imported_symbol(messages, function_initial_scope_count, importing, None);
		}
	}
}

fn resolve_block_non_type_imports<'a>(
	cli_arguments: &CliArguments,
	herd_member: &bumpalo_herd::Member<'a>,
	messages: &mut Messages,
	root_layers: &RootLayers<'a>,
	symbols: &mut Symbols<'a>,
	module_path: &'a [String],
	function_initial_scope_count: usize,
	block: &tree::Block<'a>,
	is_root: bool,
) {
	if is_root && cli_arguments.std_enabled && !matches!(module_path, [a, b] if a == "fae" && b == "prelude") {
		let segments = herd_member.alloc([Node::new("fae", Span::unusable()), Node::new("prelude", Span::unusable())]);
		let path = PathSegments { segments };
		resolve_import_for_block_non_types(messages, root_layers, symbols, function_initial_scope_count, &path, None);
	}

	for statement in block.statements {
		let import_statement = match statement {
			tree::Statement::Import(import_statement) => import_statement,
			_ => continue,
		};

		let path = &import_statement.item.path_segments;
		let names = Some(import_statement.item.symbol_names);
		resolve_import_for_block_non_types(messages, root_layers, symbols, function_initial_scope_count, path, names);
	}
}

fn resolve_import_for_block_non_types<'a>(
	messages: &mut Messages,
	root_layers: &RootLayers<'a>,
	symbols: &mut Symbols<'a>,
	function_initial_scope_count: usize,
	path: &PathSegments<'a>,
	names: Option<&[Node<&'a str>]>,
) {
	let Some(layer) = root_layers.layer_for_path(messages, path) else {
		return;
	};

	if let Some(names) = names {
		let importable_functions = layer.importable_functions();
		let importable_consts = layer.importable_consts();
		let importable_statics = layer.importable_statics();

		for name in names {
			if let Some(&importing) = importable_functions.get(name.item) {
				symbols.push_imported_symbol(messages, function_initial_scope_count, importing, Some(name.span));
			} else if let Some(&importing) = importable_consts.get(name.item) {
				symbols.push_imported_symbol(messages, function_initial_scope_count, importing, Some(name.span));
			} else if let Some(&importing) = importable_statics.get(name.item) {
				symbols.push_imported_symbol(messages, function_initial_scope_count, importing, Some(name.span));
			}
		}
	} else {
		// TODO: Add asterisk syntax for importing all items in a scope

		for &importing in layer.importable_functions().values() {
			symbols.push_imported_symbol(messages, function_initial_scope_count, importing, None);
		}

		for &importing in layer.importable_consts().values() {
			symbols.push_imported_symbol(messages, function_initial_scope_count, importing, None);
		}

		for &importing in layer.importable_statics().values() {
			symbols.push_imported_symbol(messages, function_initial_scope_count, importing, None);
		}
	}
}

fn create_block_types<'a>(
	messages: &mut Messages,
	type_store: &TypeStore<'a>,
	symbols: &mut Symbols<'a>,
	function_initial_scope_count: usize,
	module_path: &'a [String],
	enclosing_generic_parameters: &GenericParameters<'a>,
	block: &tree::Block<'a>,
	scope_id: ScopeId,
	is_root: bool,
	type_shape_indicies: &mut Vec<usize>,
) {
	for statement in block.statements {
		if is_root {
			match statement {
				tree::Statement::Expression(..)
				| tree::Statement::Block(..)
				| tree::Statement::While(..)
				| tree::Statement::Binding(..)
				| tree::Statement::Break(..)
				| tree::Statement::Continue(..)
				| tree::Statement::Return(..) => {
					let error = error!("{} is not allowed in a root scope", statement.name_and_article());
					messages.message(error.span(statement.span()));
					continue;
				}

				tree::Statement::Import(..)
				| tree::Statement::Struct(..)
				| tree::Statement::Enum(..)
				| tree::Statement::Function(..)
				| tree::Statement::Const(..)
				| tree::Statement::Static(..) => {}
			}
		} else {
			if let tree::Statement::Static(..) = statement {
				let error = error!("{} is only allowed in a root scope", statement.name_and_article());
				messages.message(error.span(statement.span()));
				continue;
			}
		}

		if let tree::Statement::Struct(statement) = statement {
			let shape_index = create_block_struct(
				messages,
				type_store,
				symbols,
				function_initial_scope_count,
				module_path,
				enclosing_generic_parameters,
				scope_id,
				statement,
			);
			type_shape_indicies.push(shape_index);
		} else if let tree::Statement::Enum(statement) = statement {
			let shape_index = create_block_enum(
				messages,
				type_store,
				symbols,
				function_initial_scope_count,
				module_path,
				enclosing_generic_parameters,
				scope_id,
				statement,
			);
			type_shape_indicies.push(shape_index);
		}
	}
}

fn create_block_struct<'a>(
	messages: &mut Messages,
	type_store: &TypeStore<'a>,
	symbols: &mut Symbols<'a>,
	function_initial_scope_count: usize,
	module_path: &'a [String],
	enclosing_generic_parameters: &GenericParameters<'a>,
	scope_id: ScopeId,
	statement: &tree::Struct<'a>,
) -> usize {
	//Start off with no fields, they will be added during the next pre-pass
	//so that all types exist in order to populate field types

	let capacity = statement.generics.len() + enclosing_generic_parameters.parameters().len();
	let mut explicit_generics = Vec::with_capacity(capacity);

	let mut type_entries = type_store.type_entries.write();
	let mut user_types = type_store.user_types.write();

	let shape_index = user_types.len();
	for (generic_index, &generic) in statement.generics.iter().enumerate() {
		let generic_type_id = TypeStore::register_user_type_generic(&mut type_entries, &user_types, shape_index, generic_index);
		explicit_generics.push(GenericParameter { name: generic, generic_type_id });
	}

	let explicit_generics_len = explicit_generics.len();
	let mut generic_parameters = GenericParameters::new_from_explicit(explicit_generics);
	for (index, parent_parameter) in enclosing_generic_parameters.parameters().iter().enumerate() {
		let generic_index = explicit_generics_len + index;
		let generic_type_id = TypeStore::register_user_type_generic(&mut type_entries, &user_types, shape_index, generic_index);
		let parameter = GenericParameter { name: parent_parameter.name, generic_type_id };
		generic_parameters.push_implicit(parameter);
	}

	let name = statement.name.item;
	let shape = StructShape::new(statement.name.item, None, None, false);
	let kind = UserTypeKind::Struct { shape };
	let span = statement.name.span;
	let shape_index = TypeStore::register_type(&mut user_types, name, generic_parameters, kind, module_path, scope_id, span);
	let kind = SymbolKind::Type { shape_index };
	let symbol = Symbol { name, kind, span: Some(span) };
	symbols.push_symbol(messages, function_initial_scope_count, symbol);
	shape_index
}

fn create_block_enum<'a>(
	messages: &mut Messages,
	type_store: &TypeStore<'a>,
	symbols: &mut Symbols<'a>,
	function_initial_scope_count: usize,
	module_path: &'a [String],
	enclosing_generic_parameters: &GenericParameters<'a>,
	scope_id: ScopeId,
	statement: &tree::Enum<'a>,
) -> usize {
	let capacity = statement.generics.len() + enclosing_generic_parameters.parameters().len();
	let mut explicit_generics = Vec::with_capacity(capacity);

	let mut type_entries = type_store.type_entries.write();
	let mut user_types = type_store.user_types.write();

	let enum_shape_index = user_types.len() + statement.variants.len();
	for (generic_index, &generic) in statement.generics.iter().enumerate() {
		let generic_type_id =
			TypeStore::register_user_type_generic(&mut type_entries, &user_types, enum_shape_index, generic_index);
		explicit_generics.push(GenericParameter { name: generic, generic_type_id });
	}

	let explicit_generics_len = explicit_generics.len();
	let mut generic_parameters = GenericParameters::new_from_explicit(explicit_generics);
	for (index, parent_parameter) in enclosing_generic_parameters.parameters().iter().enumerate() {
		let generic_index = explicit_generics_len + index;
		let generic_type_id =
			TypeStore::register_user_type_generic(&mut type_entries, &user_types, enum_shape_index, generic_index);
		let parameter = GenericParameter { name: parent_parameter.name, generic_type_id };
		generic_parameters.push_implicit(parameter);
	}

	if statement.variants.len() > 256 {
		let name = statement.name.item;
		let error = error!("Enum `{name}` has more than 256 variants, this is currently unsupported and disallowed");
		messages.message(error.span(statement.name.span));
	}

	let mut variants = Vec::new();
	let mut variants_by_name = FxHashMap::default();

	for (variant_index, variant) in statement.variants.iter().enumerate() {
		let struct_shape_index = user_types.len();

		let mut variant_explicit_generics = Vec::with_capacity(generic_parameters.explicit_len());
		for (generic_index, enum_explicit_generic) in generic_parameters.explicit_parameters().iter().enumerate() {
			let generic_type_id =
				TypeStore::register_user_type_generic(&mut type_entries, &user_types, struct_shape_index, generic_index);
			variant_explicit_generics.push(GenericParameter { name: enum_explicit_generic.name, generic_type_id });
		}

		let explicit_generics_len = variant_explicit_generics.len();
		let mut variant_generic_parameters = GenericParameters::new_from_explicit(variant_explicit_generics);
		for (index, enum_parameter) in generic_parameters.implicit_parameters().iter().enumerate() {
			let generic_index = explicit_generics_len + index;
			let generic_type_id =
				TypeStore::register_user_type_generic(&mut type_entries, &user_types, struct_shape_index, generic_index);
			let parameter = GenericParameter { name: enum_parameter.name, generic_type_id };
			variant_generic_parameters.push_implicit(parameter);
		}

		let (name, is_transparent) = match variant {
			tree::EnumVariant::StructLike(struct_like) => (struct_like.name, false),
			tree::EnumVariant::Transparent(transparent) => (transparent.name, true),
		};

		let span = name.span;
		let name = name.item;

		let shape = StructShape::new(name, Some(enum_shape_index), Some(variant_index), is_transparent);
		let kind = UserTypeKind::Struct { shape };
		TypeStore::register_type(&mut user_types, name, variant_generic_parameters, kind, module_path, scope_id, span);

		let variant_shape = EnumVariantShape {
			name,
			span,
			variant_index,
			struct_shape_index,
			is_transparent,
		};
		variants.push(variant_shape);
		variants_by_name.insert(name, variant_index);
	}

	let name = statement.name.item;
	let shape = EnumShape::new(variants, variants_by_name);
	let kind = UserTypeKind::Enum { shape };
	let span = statement.name.span;
	assert_eq!(user_types.len(), enum_shape_index);
	let shape_index = TypeStore::register_type(&mut user_types, name, generic_parameters, kind, module_path, scope_id, span);
	let kind = SymbolKind::Type { shape_index };
	let symbol = Symbol { name, kind, span: Some(span) };
	symbols.push_symbol(messages, function_initial_scope_count, symbol);
	shape_index
}

fn fill_block_types<'a>(
	messages: &mut Messages<'a>,
	type_store: &TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	generic_usages: &mut Vec<GenericUsage>,
	root_layers: &RootLayers<'a>,
	symbols: &mut Symbols<'a>,
	module_path: &'a [String],
	function_initial_scope_count: usize,
	block: &tree::Block<'a>,
	type_shape_indicies: &mut Vec<usize>,
) {
	let mut shape_index_iter = type_shape_indicies.iter();

	for statement in block.statements {
		if let tree::Statement::Struct(statement) = statement {
			fill_block_struct(
				messages,
				type_store,
				function_store,
				generic_usages,
				root_layers,
				symbols,
				module_path,
				function_initial_scope_count,
				statement,
				*shape_index_iter.next().unwrap(),
			);
		}

		if let tree::Statement::Enum(statement) = statement {
			fill_block_enum(
				messages,
				type_store,
				function_store,
				generic_usages,
				root_layers,
				symbols,
				module_path,
				function_initial_scope_count,
				statement,
				*shape_index_iter.next().unwrap(),
			);
		}
	}

	type_shape_indicies.clear();
}

fn fill_block_struct<'a>(
	messages: &mut Messages<'a>,
	type_store: &TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	generic_usages: &mut Vec<GenericUsage>,
	root_layers: &RootLayers<'a>,
	symbols: &mut Symbols<'a>,
	module_path: &'a [String],
	function_initial_scope_count: usize,
	statement: &tree::Struct<'a>,
	shape_index: usize,
) {
	let user_types = type_store.user_types.read();
	let user_type = &user_types[shape_index];
	let scope = symbols.child_scope();

	for (generic_index, generic) in user_type.generic_parameters.parameters().iter().enumerate() {
		let kind = SymbolKind::UserTypeGeneric { shape_index, generic_index };
		let symbol = Symbol { name: generic.name.item, kind, span: Some(generic.name.span) };
		scope.symbols.push_symbol(messages, function_initial_scope_count, symbol);
	}
	drop(user_types);

	let blank_generic_parameters = GenericParameters::new_from_explicit(Vec::new());
	let mut fields = Vec::with_capacity(statement.fields.len());

	for field in statement.fields {
		let field_type = match type_store.lookup_type(
			messages,
			function_store,
			module_path,
			generic_usages,
			root_layers,
			scope.symbols,
			function_initial_scope_count,
			&blank_generic_parameters,
			&field.parsed_type,
		) {
			Some(type_id) => type_id,
			None => type_store.any_collapse_type_id(),
		};

		let field_shape = FieldShape {
			name: field.name.item,
			field_type,
			attribute: field.attribute,
			read_only: field.read_only,
		};
		let span = field.name.span + field.parsed_type.span;
		let node = Node::new(field_shape, span);
		fields.push(node);
	}

	let mut user_types = type_store.user_types.write();
	match &mut user_types[shape_index].kind {
		UserTypeKind::Struct { shape } => {
			shape.fields = fields;
			assert!(!shape.been_filled);
			shape.been_filled = true;

			let has_specializations = !shape.specializations.is_empty();
			drop(user_types);

			if has_specializations {
				fill_pre_existing_struct_specializations(
					messages,
					type_store,
					function_store,
					generic_usages,
					module_path,
					shape_index,
				);
			}
		}

		UserTypeKind::Enum { .. } => unreachable!(),
	}
}

fn fill_block_enum<'a>(
	messages: &mut Messages<'a>,
	type_store: &TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	generic_usages: &mut Vec<GenericUsage>,
	root_layers: &RootLayers<'a>,
	symbols: &mut Symbols<'a>,
	module_path: &'a [String],
	function_initial_scope_count: usize,
	statement: &tree::Enum<'a>,
	enum_shape_index: usize,
) {
	let user_types = type_store.user_types.read();
	let user_type = &user_types[enum_shape_index];
	let scope = symbols.child_scope();

	for (generic_index, generic) in user_type.generic_parameters.parameters().iter().enumerate() {
		let kind = SymbolKind::UserTypeGeneric { shape_index: enum_shape_index, generic_index };
		let symbol = Symbol { name: generic.name.item, kind, span: Some(generic.name.span) };
		scope.symbols.push_symbol(messages, function_initial_scope_count, symbol);
	}
	drop(user_types);

	let blank_generic_parameters = GenericParameters::new_from_explicit(Vec::new());
	let mut shared_fields = Vec::with_capacity(statement.shared_fields.len());

	for shared_field in statement.shared_fields {
		let field_type = match type_store.lookup_type(
			messages,
			function_store,
			module_path,
			generic_usages,
			root_layers,
			scope.symbols,
			function_initial_scope_count,
			&blank_generic_parameters,
			&shared_field.parsed_type,
		) {
			Some(type_id) => type_id,
			None => type_store.any_collapse_type_id(),
		};

		let field_shape = FieldShape {
			name: shared_field.name.item,
			field_type,
			attribute: shared_field.attribute,
			read_only: shared_field.read_only,
		};
		let span = shared_field.name.span + shared_field.parsed_type.span;
		let node = Node::new(field_shape, span);
		shared_fields.push(node);
	}

	let user_types = type_store.user_types.read();
	let user_type = &user_types[enum_shape_index];
	let shape = match &user_type.kind {
		UserTypeKind::Enum { shape } => shape,
		UserTypeKind::Struct { .. } => unreachable!(),
	};

	// Yuck
	let mut variant_shapes = shape.variant_shapes.clone();
	drop(user_types);

	for variant_shape in &mut variant_shapes {
		fill_struct_like_enum_variant(
			messages,
			type_store,
			function_store,
			generic_usages,
			root_layers,
			scope.symbols,
			module_path,
			function_initial_scope_count,
			&variant_shape,
			statement,
		);
	}

	let mut user_types = type_store.user_types.write();
	match &mut user_types[enum_shape_index].kind {
		UserTypeKind::Struct { .. } => unreachable!(),

		UserTypeKind::Enum { shape } => {
			assert!(shape.shared_fields.is_empty());
			shape.shared_fields = shared_fields;
			shape.variant_shapes = variant_shapes;
			assert!(!shape.been_filled);
			shape.been_filled = true;

			let has_specializations = !shape.specializations.is_empty();
			drop(user_types);

			if has_specializations {
				fill_pre_existing_enum_specializations(
					messages,
					type_store,
					function_store,
					generic_usages,
					module_path,
					enum_shape_index,
				);
			}
		}
	}
}

fn fill_struct_like_enum_variant<'a>(
	messages: &mut Messages<'a>,
	type_store: &TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	generic_usages: &mut Vec<GenericUsage>,
	root_layers: &RootLayers<'a>,
	symbols: &mut Symbols<'a>,
	module_path: &'a [String],
	function_initial_scope_count: usize,
	variant_shape: &EnumVariantShape<'a>,
	statement: &tree::Enum<'a>,
) {
	let scope = symbols.child_scope();
	let tree_variant = &statement.variants[variant_shape.variant_index];

	let user_types = type_store.user_types.read();
	let struct_shape = &user_types[variant_shape.struct_shape_index];
	for (generic_index, generic) in struct_shape.generic_parameters.parameters().iter().enumerate() {
		let kind = SymbolKind::UserTypeGeneric { shape_index: variant_shape.struct_shape_index, generic_index };
		let symbol = Symbol { name: generic.name.item, kind, span: Some(generic.name.span) };
		scope.symbols.push_symbol(messages, function_initial_scope_count, symbol);
	}
	drop(user_types);

	let mut fields = Vec::new();

	let blank_generic_parameters = GenericParameters::new_from_explicit(Vec::new());
	for shared_field in statement.shared_fields {
		let field_type = match type_store.lookup_type(
			messages,
			function_store,
			module_path,
			generic_usages,
			root_layers,
			scope.symbols,
			function_initial_scope_count,
			&blank_generic_parameters,
			&shared_field.parsed_type,
		) {
			Some(type_id) => type_id,
			None => type_store.any_collapse_type_id(),
		};

		let field_shape = FieldShape {
			name: shared_field.name.item,
			field_type,
			attribute: shared_field.attribute,
			read_only: shared_field.read_only,
		};
		let span = shared_field.name.span + shared_field.parsed_type.span;
		let node = Node::new(field_shape, span);
		fields.push(node);
	}

	let blank_generic_parameters = GenericParameters::new_from_explicit(Vec::new());
	match tree_variant {
		tree::EnumVariant::StructLike(struct_like) => {
			for field in struct_like.fields {
				let field_type = match type_store.lookup_type(
					messages,
					function_store,
					module_path,
					generic_usages,
					root_layers,
					scope.symbols,
					function_initial_scope_count,
					&blank_generic_parameters,
					&field.parsed_type,
				) {
					Some(type_id) => type_id,
					None => type_store.any_collapse_type_id(),
				};

				let field_shape = FieldShape {
					name: field.name.item,
					field_type,
					attribute: field.attribute,
					read_only: field.read_only,
				};
				let span = field.name.span + field.parsed_type.span;
				let node = Node::new(field_shape, span);
				fields.push(node);
			}
		}

		tree::EnumVariant::Transparent(transparent) => {
			let field_type = match type_store.lookup_type(
				messages,
				function_store,
				module_path,
				generic_usages,
				root_layers,
				scope.symbols,
				function_initial_scope_count,
				&blank_generic_parameters,
				&transparent.parsed_type,
			) {
				Some(type_id) => type_id,
				None => type_store.any_collapse_type_id(),
			};

			let field_shape = FieldShape {
				name: transparent.name.item,
				field_type,
				attribute: None,
				read_only: false, // TODO: Let the user control this
			};
			let span = transparent.parsed_type.span;
			let node = Node::new(field_shape, span);
			fields.push(node);
		}
	}

	let mut user_types = type_store.user_types.write();
	match &mut user_types[variant_shape.struct_shape_index].kind {
		UserTypeKind::Struct { shape } => {
			shape.fields = fields;
			assert!(!shape.been_filled);
			shape.been_filled = true;
		}

		UserTypeKind::Enum { .. } => unreachable!(),
	}
}

fn fill_pre_existing_struct_specializations<'a>(
	messages: &mut Messages<'a>,
	type_store: &TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	generic_usages: &mut Vec<GenericUsage>,
	module_path: &'a [String],
	shape_index: usize,
) {
	let user_types = type_store.user_types.read();
	let user_type = &user_types[shape_index];
	let span = user_type.span;
	let shape = match &user_type.kind {
		UserTypeKind::Struct { shape } => shape,
		kind => unreachable!("{kind:?}"),
	};

	let mut fields = Vec::with_capacity(shape.fields.len());
	for field in &shape.fields {
		fields.push(Field {
			span: Some(field.span),
			name: field.item.name,
			type_id: field.item.field_type,
			attribute: field.item.attribute,
			read_only: field.item.read_only,
		});
	}

	let mut specializations = shape.specializations.clone(); // Belch
	drop(user_types);

	for specialization in &mut specializations {
		let mut fields = fields.clone();
		for field in &mut fields {
			field.type_id = type_store.specialize_with_user_type_generics(
				messages,
				function_store,
				module_path,
				generic_usages,
				shape_index,
				specialization.type_arguments.clone(),
				field.type_id,
			);
		}

		assert!(!specialization.been_filled);
		specialization.been_filled;
		assert_eq!(specialization.fields.len(), 0);
		specialization.fields = fields;
	}

	let mut user_types = type_store.user_types.write();
	match &mut user_types[shape_index].kind {
		UserTypeKind::Struct { shape } => {
			shape.specializations = specializations;
		}

		kind => unreachable!("{kind:?}"),
	}
	drop(user_types);

	let type_entries = type_store.type_entries.read();
	let user_types = type_store.user_types.read();
	let mut type_ids = Vec::new();
	match &user_types[shape_index].kind {
		UserTypeKind::Struct { shape } => {
			type_ids.reserve(shape.specializations.len());

			for specialization in &shape.specializations {
				let type_id = specialization.type_id;
				let chain = TypeStore::find_user_type_dependency_chain(&type_entries, &user_types, type_id, type_id);
				if let Some(chain) = chain {
					report_cyclic_user_type(messages, type_store, function_store, module_path, type_id, chain, span);
				} else {
					type_ids.push(type_id);
				}
			}
		}

		kind => unreachable!("{kind:?}"),
	}

	drop(user_types);
	drop(type_entries);

	for type_id in type_ids {
		type_store.calculate_layout(type_id);
	}
}

fn fill_pre_existing_enum_specializations<'a>(
	messages: &mut Messages<'a>,
	type_store: &TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	generic_usages: &mut Vec<GenericUsage>,
	module_path: &'a [String],
	shape_index: usize,
) {
	let user_types = type_store.user_types.read();
	let user_type = &user_types[shape_index];
	let span = user_type.span;
	let shape = match &user_type.kind {
		UserTypeKind::Enum { shape } => shape,
		kind => unreachable!("{kind:?}"),
	};

	let mut specializations = shape.specializations.clone(); // Belch
	drop(user_types);

	for specialization in &mut specializations {
		assert!(!specialization.been_filled);
		specialization.been_filled;
		assert_eq!(specialization.variants_by_name.len(), 0);

		for field in &mut specialization.shared_fields {
			field.type_id = type_store.specialize_with_user_type_generics(
				messages,
				function_store,
				module_path,
				generic_usages,
				shape_index,
				specialization.type_arguments.clone(),
				field.type_id,
			);
		}

		for variant in &mut specialization.variants {
			variant.type_id = type_store.specialize_with_user_type_generics(
				messages,
				function_store,
				module_path,
				generic_usages,
				shape_index,
				specialization.type_arguments.clone(),
				variant.type_id,
			);
		}
	}

	let mut user_types = type_store.user_types.write();
	match &mut user_types[shape_index].kind {
		UserTypeKind::Enum { shape } => {
			shape.specializations = specializations;
		}

		kind => unreachable!("{kind:?}"),
	}
	drop(user_types);

	let type_entries = type_store.type_entries.read();
	let user_types = type_store.user_types.read();
	let mut type_ids = Vec::new();
	match &user_types[shape_index].kind {
		UserTypeKind::Enum { shape } => {
			type_ids.reserve(shape.specializations.len());

			for specialization in &shape.specializations {
				let type_id = specialization.type_id;
				let chain = TypeStore::find_user_type_dependency_chain(&type_entries, &user_types, type_id, type_id);
				if let Some(chain) = chain {
					report_cyclic_user_type(messages, type_store, function_store, module_path, type_id, chain, span);
				} else {
					type_ids.push(type_id);
				}
			}
		}

		kind => unreachable!("{kind:?}"),
	}

	drop(user_types);
	drop(type_entries);

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
			type_store.type_name(&function_store, module_path, link.user_type)
		));
	}

	messages.message(error);
}

fn create_block_functions<'a>(
	messages: &mut Messages<'a>,
	lang_items: &RwLock<LangItems>,
	root_layers: &RootLayers<'a>,
	type_store: &TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	generic_usages: &mut Vec<GenericUsage>,
	externs: &RwLock<Externs>,
	readables: &mut Readables<'a>,
	symbols: &mut Symbols<'a>,
	module_path: &'a [String],
	enclosing_generic_parameters: &GenericParameters<'a>,
	block: &'a tree::Block<'a>,
	local_function_shape_indicies: &mut Vec<usize>,
	scope_id: ScopeId,
) {
	let original_generic_usages_len = generic_usages.len();

	for statement in block.statements {
		if let tree::Statement::Function(statement) = statement {
			let original_readables_starting_index = readables.starting_index;
			let original_readables_overall_len = readables.overall_len();
			readables.starting_index = readables.overall_len();

			let method_base_shape_index = statement.method_attribute.as_ref().and_then(|attribute| {
				method_base_shape_index(messages, root_layers, type_store, symbols, symbols.scopes.len(), attribute)
			});
			let base_shape_generics = method_base_shape_index.map(|method_base_shape_index| {
				let base_shape = &type_store.user_types.read()[method_base_shape_index];
				base_shape.generic_parameters.clone() // I am sad
			});

			let function_initial_scope_count = symbols.scopes.len();
			let scope = symbols.child_scope();
			let mut function_store_shapes = function_store.shapes.write();
			let mut function_store_generics = function_store.generics.write();
			let function_shape_index = function_store_shapes.len();

			let capacity = statement.generics.len()
				+ enclosing_generic_parameters.parameters().len()
				+ base_shape_generics.as_ref().map(|p| p.parameters().len()).unwrap_or(0);
			let mut explicit_generics = Vec::with_capacity(capacity);

			for (generic_index, &generic) in statement.generics.iter().enumerate() {
				let generic_type_id = type_store.register_function_generic(function_shape_index, generic_index);
				explicit_generics.push(GenericParameter { name: generic, generic_type_id });

				let kind = SymbolKind::FunctionGeneric { function_shape_index, generic_index };
				let symbol = Symbol { name: generic.item, kind, span: Some(generic.span) };
				scope.symbols.push_symbol(messages, function_initial_scope_count, symbol);
			}

			let explicit_generics_len = explicit_generics.len();
			let mut generic_parameters = GenericParameters::new_from_explicit(explicit_generics);
			for (index, parent_parameter) in enclosing_generic_parameters.parameters().iter().enumerate() {
				let generic_index = explicit_generics_len + index;
				let generic_type_id = type_store.register_function_generic(function_shape_index, generic_index);
				let parameter = GenericParameter { name: parent_parameter.name, generic_type_id };
				generic_parameters.push_implicit(parameter);

				let kind = SymbolKind::FunctionGeneric { function_shape_index, generic_index };
				let span = Some(parent_parameter.name.span);
				let symbol = Symbol { name: parent_parameter.name.item, kind, span };
				scope.symbols.push_symbol(messages, function_initial_scope_count, symbol);
			}

			let mut base_type_arguments;
			let implicit_generics_len = generic_parameters.implicit_len();
			if let Some(base_shape_generics) = &base_shape_generics {
				base_type_arguments = TypeArguments::new_from_explicit(Vec::new());

				for (index, base_type_parameter) in base_shape_generics.parameters().iter().enumerate() {
					let generic_index = explicit_generics_len + implicit_generics_len + index;
					let generic_type_id = type_store.register_function_generic(function_shape_index, generic_index);
					let parameter = GenericParameter { name: base_type_parameter.name, generic_type_id };
					generic_parameters.push_method_base(parameter);
					base_type_arguments.ids.push(parameter.generic_type_id);

					let kind = SymbolKind::FunctionGeneric { function_shape_index, generic_index };
					let span = Some(base_type_parameter.name.span);
					let symbol = Symbol { name: base_type_parameter.name.item, kind, span };
					scope.symbols.push_symbol(messages, function_initial_scope_count, symbol);
				}

				base_type_arguments.explicit_len = base_shape_generics.explicit_len();
				base_type_arguments.implicit_len = base_shape_generics.implicit_len();
				base_type_arguments.method_base_len = base_shape_generics.method_base_len();
			} else {
				base_type_arguments = TypeArguments::new_from_explicit(Vec::new());
			}

			function_store_generics.push(generic_parameters.clone());
			drop(function_store_generics);

			let return_type = if let Some(parsed_type) = &statement.parsed_type {
				let type_id = type_store.lookup_type(
					messages,
					&function_store,
					module_path,
					generic_usages,
					root_layers,
					scope.symbols,
					function_initial_scope_count,
					enclosing_generic_parameters,
					parsed_type,
				);

				let return_type = match type_id {
					Some(type_id) => type_id,
					None => type_store.any_collapse_type_id(),
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
			if let Some(method_attribute) = &statement.method_attribute {
				let mutable = match method_attribute.item.kind {
					MethodKind::ImmutableSelf => Some(false),
					MethodKind::MutableSelf => Some(true),
					MethodKind::Static => None,
				};

				let method_base_shape_index = method_base_shape_index.unwrap();

				if let Some(mutable) = mutable {
					let base_type = type_store.get_or_add_shape_specialization(
						messages,
						&function_store,
						module_path,
						generic_usages,
						method_base_shape_index,
						Some(method_attribute.span),
						base_type_arguments,
					);
					let type_id = match base_type {
						Some(base_type) => type_store.pointer_to(base_type, mutable),
						None => type_store.any_collapse_type_id(),
					};

					let readable_index = readables.push("self", type_id, ReadableKind::Let);
					assert_eq!(readable_index, 0);
					parameters.push(ParameterShape { type_id, is_mutable: false, readable_index });
				}
			}

			let maybe_self = parameters.len();
			for (index, parameter) in statement.parameters.parameters.iter().enumerate() {
				let type_id = type_store.lookup_type(
					messages,
					&function_store,
					module_path,
					generic_usages,
					root_layers,
					scope.symbols,
					function_initial_scope_count,
					enclosing_generic_parameters,
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
				assert_eq!(readable_index, index + maybe_self);

				parameters.push(ParameterShape { type_id, is_mutable, readable_index });
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
				// TODO: Detect duplicate externs
				externs.write().push(messages, extern_attribute.name, name.span);
			}

			let root_name = root_layers.root_name.as_str();
			let is_main = statement.method_attribute.is_none() && module_path == [root_name] && name.item == "main";

			let shape = FunctionShape {
				name,
				module_path,
				file_index: scope_id.file_index,
				is_main,
				generic_parameters,
				extern_attribute: statement.extern_attribute,
				export_attribute: statement.export_attribute,
				intrinsic_attribute: statement.intrinsic_attribute,
				lang_attribute: statement.lang_attribute,
				method_base_index: method_base_shape_index,
				parameters,
				c_varargs,
				return_type,
				block: None,
				generic_usages: Vec::new(),
				specializations_by_type_arguments: FxHashMap::default(),
				specializations: Vec::new(),
			};
			function_store_shapes.push(shape);
			drop(function_store_shapes);

			local_function_shape_indicies.push(function_shape_index);

			if let Some(method_attribute) = &statement.method_attribute {
				if let Some(shape_index) = method_base_shape_index {
					let base_scope_id = type_store.user_types.read()[shape_index].scope_id;
					if scope_id != base_scope_id {
						let error = error!("Method must be defined in the same scope as the self type");
						messages.message(error.span(statement.name.span));
					} else {
						let info = MethodInfo {
							span: statement.name.span,
							function_shape_index,
							kind: method_attribute.item.kind,
						};

						let methods = &mut type_store.user_types.write()[shape_index].methods;
						methods.insert(statement.name.item, info);
					}
				}
			} else {
				if let Some(lang_attribute) = &statement.lang_attribute {
					let lang_name = lang_attribute.item.name;
					let lang_span = lang_attribute.span;
					let function_id = FunctionId { function_shape_index, specialization_index: 0 };
					lang_items
						.write()
						.register_lang_function(messages, function_id, lang_name, lang_span);
				}

				let kind = SymbolKind::Function { function_shape_index };
				let span = Some(statement.name.span);
				let symbol = Symbol { name: name.item, kind, span };
				symbols.push_symbol(messages, function_initial_scope_count, symbol);
			}

			readables.readables.truncate(original_readables_overall_len);
			readables.starting_index = original_readables_starting_index;
		}
	}

	generic_usages.truncate(original_generic_usages_len);
}

fn method_base_shape_index<'a>(
	messages: &mut Messages<'a>,
	root_layers: &RootLayers<'a>,
	type_store: &TypeStore<'a>,
	symbols: &mut Symbols<'a>,
	function_initial_scope_count: usize,
	method_attribute: &'a Node<MethodAttribute>,
) -> Option<usize> {
	let symbol = symbols.lookup_symbol(
		messages,
		root_layers,
		type_store,
		function_initial_scope_count,
		&method_attribute.item.base_type.item,
	)?;

	match symbol.kind {
		SymbolKind::Type { shape_index } => Some(shape_index),

		_ => {
			let error = error!("Method declaration self type specifier must be a type");
			messages.message(error.span(method_attribute.item.base_type.span));
			return None;
		}
	}
}

fn validate_block_consts<'a>(context: &mut Context<'a, '_, '_>, block: &'a tree::Block<'a>) {
	for statement in block.statements {
		if let tree::Statement::Const(statement) = statement {
			validate_const(context, statement);
		}
	}
}

fn validate_block_statics<'a>(context: &mut Context<'a, '_, '_>, block: &'a tree::Block<'a>) {
	for statement in block.statements {
		if let tree::Statement::Static(statement) = statement {
			validate_static(context, statement);
		}
	}
}

fn validate_block<'a>(mut context: Context<'a, '_, '_>, block: &'a tree::Block<'a>, is_root: bool) -> Block<'a> {
	let scope_id = ScopeId {
		file_index: context.file_index,
		scope_index: context.scope_index,
	};

	if !is_root {
		create_block_types(
			context.messages,
			context.type_store,
			context.symbols_scope.symbols,
			context.function_initial_scope_count,
			context.module_path,
			context.generic_parameters,
			block,
			scope_id,
			is_root,
			context.type_shape_indicies,
		);

		resolve_block_type_imports(
			context.cli_arguments,
			context.herd_member,
			context.messages,
			context.root_layers,
			context.symbols_scope.symbols,
			context.module_path,
			context.function_initial_scope_count,
			block,
			is_root,
		);

		fill_block_types(
			context.messages,
			context.type_store,
			context.function_store,
			context.function_generic_usages,
			context.root_layers,
			context.symbols_scope.symbols,
			context.module_path,
			context.function_initial_scope_count,
			block,
			context.type_shape_indicies,
		);
	}

	resolve_block_non_type_imports(
		context.cli_arguments,
		context.herd_member,
		context.messages,
		context.root_layers,
		context.symbols_scope.symbols,
		context.module_path,
		context.function_initial_scope_count,
		block,
		is_root,
	);

	if !is_root {
		create_block_functions(
			context.messages,
			context.lang_items,
			context.root_layers,
			context.type_store,
			context.function_store,
			context.function_generic_usages,
			context.externs,
			context.readables,
			context.symbols_scope.symbols,
			context.module_path,
			context.generic_parameters,
			block,
			context.local_function_shape_indicies,
			scope_id,
		);
	}

	if !is_root {
		validate_block_consts(&mut context, block);
	}

	let mut returns = false;
	let mut statements = Vec::with_capacity(block.statements.len());

	for statement in block.statements {
		context.expected_type = None;

		match statement {
			tree::Statement::Expression(..)
			| tree::Statement::Block(..)
			| tree::Statement::While(..)
			| tree::Statement::Binding(..)
			| tree::Statement::Break(..)
			| tree::Statement::Continue(..)
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
				returns |= block.returns;
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

			tree::Statement::Enum(..) => {}

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

			tree::Statement::Break(statement) => {
				let statement = if let Some(loop_index) = context.current_loop_index {
					Break { loop_index }
				} else {
					let error = error!("Cannot break when outside a loop");
					context.message(error.span(statement.span));
					Break { loop_index: 0 }
				};

				let kind = StatementKind::Break(statement);
				statements.push(Statement { kind });
			}

			tree::Statement::Continue(statement) => {
				let statement = if let Some(loop_index) = context.current_loop_index {
					Continue { loop_index }
				} else {
					let error = error!("Cannot continue when outside a loop");
					context.message(error.span(statement.span));
					Continue { loop_index: 0 }
				};

				let kind = StatementKind::Continue(statement);
				statements.push(Statement { kind });
			}

			tree::Statement::Return(statement) => {
				returns |= true;

				let expression = statement.item.expression.as_ref();
				let mut expression = expression.map(|expression| {
					let mut scope = context.child_scope();
					scope.expected_type = scope.return_type;
					validate_expression(&mut scope, expression)
				});

				if let Some(expression) = &mut expression {
					let return_type = context.return_type.unwrap();
					if let Ok(false) = context.collapse_to(return_type, expression) {
						let expected = context.type_name(return_type);
						let got = context.type_name(expression.type_id);
						let error = error!("Expected return type of {expected}, got {got}");
						context.message(error.span(statement.span));
					}
				}

				let boxed_return = Box::new(Return { expression });
				let kind = StatementKind::Return(boxed_return);
				statements.push(Statement { kind });
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

	let function_shape_index = context.local_function_shape_index(statement.index_in_block);
	let function_store_shapes = context.function_store.shapes.read();
	let return_type = function_store_shapes[function_shape_index].return_type;
	let generics = function_store_shapes[function_shape_index].generic_parameters.clone();
	drop(function_store_shapes);

	let function_initial_scope_count = context.symbols_scope.symbols.scopes.len();
	let mut scope = context.child_scope_for_function(return_type, &generics);
	scope.function_initial_scope_count = function_initial_scope_count;
	let initial_generic_usages_len = scope.function_generic_usages.len();

	for (generic_index, generic) in generics.parameters().iter().enumerate() {
		let kind = SymbolKind::FunctionGeneric { function_shape_index, generic_index };
		let symbol = Symbol { name: generic.name.item, kind, span: Some(generic.name.span) };
		scope.push_symbol(symbol);
	}

	let function_store_shapes = scope.function_store.shapes.read();

	let mut maybe_self = 0;
	if let Some(method_attribute) = &statement.method_attribute {
		let shape = &function_store_shapes[function_shape_index];
		scope.method_base_index = shape.method_base_index;

		let is_method = match method_attribute.item.kind {
			MethodKind::ImmutableSelf | MethodKind::MutableSelf => true,
			MethodKind::Static => false,
		};

		if is_method {
			let type_id = shape.parameters[0].type_id;
			let readable_index = scope.readables.push("self", type_id, ReadableKind::Let);
			let kind = SymbolKind::Let { readable_index };
			let symbol = Symbol { name: "self", kind, span: None };

			scope.push_symbol(symbol);
			maybe_self = 1;
		}
	}

	for (index, parameter) in statement.parameters.parameters.iter().enumerate() {
		let span = parameter.span;
		let parameter = &parameter.item;

		let shape = &function_store_shapes[function_shape_index];
		let parameter_shape = &shape.parameters[index + maybe_self];
		let stored_readable_index = parameter_shape.readable_index;
		let parameter_shape_type_id = parameter_shape.type_id;

		assert_eq!(stored_readable_index, index + maybe_self);
		let kind = match parameter.is_mutable {
			false => ReadableKind::Let,
			true => ReadableKind::Mut,
		};
		let readable_index = scope.readables.push(parameter.name.item, parameter_shape_type_id, kind);
		assert_eq!(readable_index, stored_readable_index);
		let kind = match parameter.is_mutable {
			false => SymbolKind::Let { readable_index },
			true => SymbolKind::Mut { readable_index },
		};

		let name = parameter.name.item;
		scope.push_symbol(Symbol { name, kind, span: Some(span) });
	}

	drop(function_store_shapes);

	let tree_block = &statement.block.as_ref().unwrap().item;
	let block = validate_block(scope, tree_block, false);

	if !return_type.is_void(context.type_store) && !block.returns {
		let error = error!("Not all code paths for function `{}` return a value", statement.name.item);
		context.message(error.span(statement.name.span));
	}

	let mut function_store_shapes = context.function_store.shapes.write();
	let shape = &mut function_store_shapes[function_shape_index];
	assert!(shape.block.is_none());
	shape.block = Some(Arc::new(block));

	let mut generic_usages = context.function_generic_usages[initial_generic_usages_len..].to_vec();
	context.function_generic_usages.truncate(initial_generic_usages_len);

	if !generic_usages.is_empty() && !shape.specializations.is_empty() {
		let specializations = shape.specializations.clone();
		drop(function_store_shapes);

		for specialization in specializations {
			if specialization.generic_poisoned {
				continue;
			}

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
	} else {
		drop(function_store_shapes);
	}

	let mut function_store_shapes = context.function_store.shapes.write();
	let shape = &mut function_store_shapes[function_shape_index];
	assert!(shape.generic_usages.is_empty());
	shape.generic_usages = generic_usages;

	let type_parameters_iter = shape.generic_parameters.explicit_parameters().iter();
	let type_parameter_span = type_parameters_iter.fold(None, |sum, p| match sum {
		Some(sum) => Some(sum + p.name.span),
		None => Some(p.name.span),
	});

	if shape.is_main {
		assert_eq!(shape.generic_parameters.implicit_len(), 0);
		let has_return_type = !shape.return_type.is_void(context.type_store);

		let export_span = shape.export_attribute.map(|a| a.span);
		drop(function_store_shapes);

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
			TypeArguments::new_from_explicit(Vec::new()),
			None,
		);

		if let Some(result) = result {
			let mut main = context.function_store.main.write();
			if main.is_some() {
				// TODO: Store all duplicat main functions, print at end sorted so the messages
				// have a consistant order independent of file order
				let message = error!("Duplicate main function");
				context.message(message.span(statement.name.span));
				return;
			}

			let specialization_index = result.specialization_index;
			let function_id = FunctionId { function_shape_index, specialization_index };
			*main = Some(function_id);
		}
	} else if statement.method_attribute.is_some() {
		let extern_span = shape.extern_attribute.map(|a| a.span);
		let export_span = shape.export_attribute.map(|a| a.span);
		drop(function_store_shapes);

		if let Some(extern_span) = extern_span {
			let message = error!("Method may not have an extern attribute");
			context.message(message.span(extern_span));
		}

		if let Some(export_span) = export_span {
			let message = error!("Method may not have an export attribute");
			context.message(message.span(export_span));
		}
	} else if statement.export_attribute.is_some() {
		drop(function_store_shapes);
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
			TypeArguments::new_from_explicit(Vec::new()),
			None,
		);
	} else if shape.lang_attribute.is_some() {
		drop(function_store_shapes);
		if let Some(type_parameter_span) = type_parameter_span {
			let message = error!("Lang item function may not have any generic type parameters");
			context.message(message.span(type_parameter_span));
		}

		context.function_store.get_or_add_specialization(
			context.messages,
			context.type_store,
			context.module_path,
			context.function_generic_usages,
			function_shape_index,
			TypeArguments::new_from_explicit(Vec::new()),
			None,
		);
	}
}

fn validate_const<'a>(context: &mut Context<'a, '_, '_>, statement: &'a tree::Node<tree::Const<'a>>) -> Option<()> {
	let explicit_type = match &statement.item.parsed_type {
		Some(parsed_type) => context.lookup_type(parsed_type),
		None => None,
	};

	let mut expression = validate_expression(context, &statement.item.expression);
	if let Some(explicit_type) = explicit_type {
		if !context.collapse_to(explicit_type, &mut expression).ok()? {
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

	let mut constants = context.constants.write();
	let constant_index = constants.len();
	constants.push(value);
	drop(constants);

	let name = statement.item.name.item;
	let kind = SymbolKind::Const { constant_index };
	let span = Some(statement.span + expression.span);
	let symbol = Symbol { name, kind, span };
	context.push_symbol(symbol);

	Some(())
}

fn validate_static<'a>(context: &mut Context<'a, '_, '_>, statement: &'a tree::Node<tree::Static<'a>>) -> Option<()> {
	if let Some(extern_attribute) = statement.item.extern_attribute {
		// TODO: Detect duplicate externs
		let name = extern_attribute.item.name;
		context.externs.write().push(context.messages, name, statement.span);
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
	let index = context.statics.write().push(name, type_id, extern_attribute);

	let kind = SymbolKind::Static { static_index: index };
	let span = Some(statement.span);
	context.push_symbol(Symbol { name, kind, span });

	Some(())
}

fn validate_binding<'a>(context: &mut Context<'a, '_, '_>, statement: &'a tree::Node<tree::Binding<'a>>) -> Option<Binding<'a>> {
	let explicit_type = match &statement.item.parsed_type {
		Some(parsed_type) => context.lookup_type(parsed_type),
		None => None,
	};

	let mut scope = context.child_scope();
	scope.expected_type = explicit_type;
	let mut expression = validate_expression(&mut scope, &statement.item.expression);
	drop(scope);

	let mut type_id = match &statement.item.parsed_type {
		Some(_) => {
			// Make sure we still take this path even if the parsed type lookup failed
			let explicit_type = explicit_type?;

			if !context.collapse_to(explicit_type, &mut expression).ok()? {
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
		type_id = context.type_store.any_collapse_type_id();
	} else if type_id.is_untyped_decimal(context.type_store) {
		context.message(error!("Cannot create binding of untyped decimal").span(statement.span));
		type_id = context.type_store.any_collapse_type_id();
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
	// This function is a bit overzealous in setting/resetting `can_is_bind` but that's fine
	// I'd rather be safe than make a mistake and miss a necessary case

	let span = expression.span;
	let original_can_is_bind = context.can_is_bind;

	match &expression.item {
		tree::Expression::Block(block) => {
			context.expected_type = None;
			context.can_is_bind = false;
			let expression = validate_block_expression(context, block, span);
			context.can_is_bind = original_can_is_bind;
			expression
		}

		tree::Expression::IfElseChain(chain_expression) => {
			context.expected_type = None;
			context.can_is_bind = false;
			let expression = validate_if_else_chain_expression(context, chain_expression, span);
			context.can_is_bind = original_can_is_bind;
			expression
		}

		tree::Expression::Match(match_expression) => {
			context.expected_type = None;
			context.can_is_bind = false;
			let expression = validate_match_expression(context, match_expression, span);
			context.can_is_bind = original_can_is_bind;
			expression
		}

		tree::Expression::IntegerLiteral(literal) => validate_integer_literal(context, literal, span),

		tree::Expression::FloatLiteral(literal) => validate_float_literal(context, literal, span),

		tree::Expression::BooleanLiteral(literal) => validate_bool_literal(context, *literal, span),

		tree::Expression::CodepointLiteral(literal) => validate_codepoint_literal(context, literal, span),

		tree::Expression::ByteCodepointLiteral(literal) => validate_byte_codepoint_literal(context, literal, span),

		tree::Expression::StringLiteral(literal) => validate_string_literal(context, literal, span),

		tree::Expression::ArrayLiteral(literal) => {
			context.expected_type = None;
			context.can_is_bind = false;
			let expression = validate_array_literal(context, literal, span);
			context.can_is_bind = original_can_is_bind;
			expression
		}

		tree::Expression::StructLiteral(literal) => {
			context.expected_type = None;
			context.can_is_bind = false;
			let expression = validate_struct_literal(context, literal, span);
			context.can_is_bind = original_can_is_bind;
			expression
		}

		tree::Expression::Call(call) => {
			context.expected_type = None;
			context.can_is_bind = false;
			let expression = validate_call(context, call, span);
			context.can_is_bind = original_can_is_bind;
			expression
		}

		tree::Expression::MethodCall(method_call) => {
			context.expected_type = None;
			context.can_is_bind = false;
			let expression = validate_method_call(context, method_call, span);
			context.can_is_bind = original_can_is_bind;
			expression
		}

		tree::Expression::Read(read) => {
			context.expected_type = None;
			context.can_is_bind = false;
			let expression = validate_read(context, read, span);
			context.can_is_bind = original_can_is_bind;
			expression
		}

		tree::Expression::DotAcccess(dot_access) => {
			context.expected_type = None;
			context.can_is_bind = false;
			let expression = validate_dot_access(context, dot_access, span);
			context.can_is_bind = original_can_is_bind;
			expression
		}

		tree::Expression::InferredEnum(inferred_enum) => validate_inferred_enum(context, inferred_enum, span),

		tree::Expression::UnaryOperation(operation) => {
			context.expected_type = None;
			context.can_is_bind = false;
			let expression = validate_unary_operation(context, operation, span);
			context.can_is_bind = original_can_is_bind;
			expression
		}

		tree::Expression::BinaryOperation(operation) => {
			context.expected_type = None;
			// Modifies `can_is_bind` flag internally
			let expression = validate_binary_operation(context, operation, span);
			context.can_is_bind = original_can_is_bind;
			expression
		}

		tree::Expression::CheckIs(check) => validate_check_is(context, check, span),
	}
}

fn validate_block_expression<'a>(context: &mut Context<'a, '_, '_>, block: &'a tree::Block<'a>, span: Span) -> Expression<'a> {
	let validated_block = validate_block(context.child_scope(), block, false);
	let returns = validated_block.returns;
	let type_id = validated_block.type_id;
	let kind = ExpressionKind::Block(validated_block);
	Expression { span, type_id, is_mutable: true, returns, kind }
}

fn validate_if_else_chain_expression<'a>(
	context: &mut Context<'a, '_, '_>,
	chain_expression: &'a tree::IfElseChain<'a>,
	span: Span,
) -> Expression<'a> {
	let mut type_id = None;
	let mut check_body_type_id = |context: &mut Context, body: &Block, span: Span| {
		if let Some(type_id) = type_id {
			// TODO: Make sure that future `give` statement performs expression collapse
			if !context.type_store.direct_match(type_id, body.type_id) {
				let expected = context.type_name(type_id);
				let found = context.type_name(body.type_id);
				let error = error!("If-else chain body type mismatch, expected {expected} but found {found}");
				context.message(error.span(span));
			}
		} else {
			type_id = Some(body.type_id);
		}
	};

	let mut entries = Vec::with_capacity(chain_expression.entries.len());
	let mut first_condition_returns = false;
	let mut all_if_bodies_return = true;

	for (index, entry) in chain_expression.entries.iter().enumerate() {
		let mut scope = context.child_scope();
		scope.can_is_bind = true;
		let condition = validate_expression(&mut scope, &entry.condition);
		scope.can_is_bind = false;
		if index == 0 && condition.returns {
			first_condition_returns = true;
		}

		let body = validate_block(scope, &entry.body.item, false);
		all_if_bodies_return &= body.returns;
		check_body_type_id(context, &body, entry.body.span);

		let entry = IfElseChainEntry { condition, body };
		entries.push(entry);
	}

	let mut else_returns = false;
	let else_body = if let Some(else_body) = &chain_expression.else_body {
		let scope = context.child_scope();
		let body = validate_block(scope, &else_body.item, false);
		else_returns = body.returns;
		Some(body)
	} else {
		None
	};

	let type_id = type_id.unwrap();
	let returns = (all_if_bodies_return && else_returns) || first_condition_returns;
	let chain = IfElseChain { type_id, entries, else_body };
	let kind = ExpressionKind::IfElseChain(Box::new(chain));
	Expression { span, type_id, is_mutable: true, returns, kind }
}

fn validate_match_expression<'a>(
	context: &mut Context<'a, '_, '_>,
	match_expression: &'a tree::Match<'a>,
	span: Span,
) -> Expression<'a> {
	let expression = validate_expression(context, &match_expression.expression);
	if expression.type_id.is_any_collapse(context.type_store) {
		return Expression::any_collapse(context.type_store, span);
	}

	let (expression_type_id, is_mutable) = match expression.type_id.as_pointed(context.type_store) {
		Some(as_pointer) => (as_pointer.type_id, as_pointer.mutable),
		None => (expression.type_id, expression.is_mutable),
	};

	let enum_entry = context.type_store.type_entries.read()[expression_type_id.index()];
	let user_types = context.type_store.user_types.read();
	let enum_specialization = match enum_entry.kind {
		TypeEntryKind::UserType { shape_index, specialization_index } => match &user_types[shape_index].kind {
			UserTypeKind::Struct { .. } => None,
			UserTypeKind::Enum { shape } => Some(&shape.specializations[specialization_index]),
		},

		_ => None,
	};

	let Some(enum_specialization) = enum_specialization else {
		let found = context.type_name(expression_type_id);
		let error = error!("Cannot match on type {found} as it is not an enum");
		context.messages.message(error.span(expression.span));
		return Expression::any_collapse(context.type_store, span);
	};

	let mut encountered_variants = Vec::<Option<Span>>::with_capacity(enum_specialization.variants_by_name.len());
	for _ in 0..enum_specialization.variants_by_name.len() {
		encountered_variants.push(None);
	}
	drop(user_types);

	let mut arms_returns = true;
	let mut arms = Vec::new();

	for arm in match_expression.arms {
		let mut scope = context.child_scope();

		let enum_entry = scope.type_store.type_entries.read()[expression_type_id.index()];
		let user_types = scope.type_store.user_types.read();

		let enum_specialization = match enum_entry.kind {
			TypeEntryKind::UserType { shape_index, specialization_index } => match &user_types[shape_index].kind {
				UserTypeKind::Struct { .. } => unreachable!(),
				UserTypeKind::Enum { shape } => &shape.specializations[specialization_index],
			},

			kind => unreachable!("{kind:?}"),
		};

		let mut variant_infos = Vec::with_capacity(arm.variant_names.len());

		for variant_name in arm.variant_names {
			let (variant_type_id, variant_index) = match enum_specialization.variants_by_name.get(variant_name.item) {
				Some(&variant_index) => {
					let variant = &enum_specialization.variants[variant_index];
					(variant.type_id, variant_index)
				}

				None => {
					let found = scope.type_name(expression_type_id);
					let error = error!("No variant `{}` found on enum {found}", variant_name.item);
					scope.messages.message(error.span(variant_name.span));
					continue;
				}
			};

			let info = VariantInfo { type_id: variant_type_id, variant_index };
			variant_infos.push(info);

			if let Some(existing) = encountered_variants[variant_index] {
				let found = scope.type_name(variant_type_id);
				let error = error!("Duplicate match arm for enum variant {found}");
				let noted = error.note(note!(existing, "Existing arm here"));
				scope.messages.message(noted.span(variant_name.span));
			}
			encountered_variants[variant_index] = Some(variant_name.span);
		}

		drop(user_types);

		let binding_name = if let Some(binding_name) = arm.binding_name {
			Some(binding_name)
		} else if let ExpressionKind::Read(read) = &expression.kind {
			if arm.variant_names.len() == 1 {
				Some(Node::new(read.name, expression.span))
			} else {
				None
			}
		} else {
			None
		};

		let binding = if let Some(binding_name) = binding_name {
			assert_eq!(arm.variant_names.len(), 1);

			let type_id = if let Some(info) = variant_infos.last() {
				let type_entries = scope.type_store.type_entries.read();
				let user_types = scope.type_store.user_types.read();
				let (shape, specialization) = info.type_id.as_struct(&type_entries, &user_types).unwrap();
				if shape.is_transparent_variant {
					assert_eq!(specialization.fields.len(), 1);
					specialization.fields.first().unwrap().type_id
				} else {
					info.type_id
				}
			} else {
				scope.type_store.any_collapse_type_id()
			};

			let kind = match is_mutable {
				true => ReadableKind::Mut,
				false => ReadableKind::Let,
			};

			let readable_index = scope.push_readable(binding_name, type_id, kind);
			Some(CheckIsResultBinding { type_id, readable_index, is_mutable })
		} else {
			None
		};

		let block = validate_block(scope.child_scope(), &arm.block.item, false);
		arms_returns &= block.returns;

		let arm = MatchArm { binding, block, variant_infos };
		arms.push(arm);
	}

	let else_arm = if let Some(else_arm) = &match_expression.else_arm {
		let block = validate_block(context.child_scope(), &else_arm.block.item, false);
		Some(block)
	} else {
		None
	};

	let enum_entry = context.type_store.type_entries.read()[expression_type_id.index()];
	let user_types = context.type_store.user_types.read();
	let enum_specialization = match enum_entry.kind {
		TypeEntryKind::UserType { shape_index, specialization_index } => match &user_types[shape_index].kind {
			UserTypeKind::Struct { .. } => unreachable!(),
			UserTypeKind::Enum { shape } => &shape.specializations[specialization_index],
		},

		kind => unreachable!("{kind:?}"),
	};

	let mut all_variants_covered = true;
	for (variant_index, variant) in enum_specialization.variants.iter().enumerate() {
		if encountered_variants[variant_index].is_none() {
			all_variants_covered = false;
			if else_arm.is_some() {
				continue;
			}

			let missing = context.type_name(variant.type_id);
			let error = error!("Missing match arm for enum variant {missing}");
			context.messages.message(error.span(span));
		}
	}

	drop(user_types);

	if let Some(else_arm) = &match_expression.else_arm {
		if all_variants_covered {
			let warning = warning!("Match expression else arm will never execute, all enum variants are already covered");
			context.message(warning.span(else_arm.else_span));
		}
	}

	// TODO: When adding `give` make sure to infer match type from arms
	let type_id = context.type_store.void_type_id();
	let returns = expression.returns | arms_returns;
	let match_expression = Box::new(Match { expression, arms, else_arm });
	let kind = ExpressionKind::Match(match_expression);
	Expression { span, type_id, is_mutable, returns, kind }
}

fn validate_while_statement<'a>(context: &mut Context<'a, '_, '_>, statement: &'a Node<tree::While<'a>>) -> While<'a> {
	let mut scope = context.child_scope();
	let condition = validate_expression(&mut scope, &statement.item.condition);
	scope.current_loop_index = Some(scope.next_loop_index);
	scope.next_loop_index += 1;
	let body = validate_block(scope, &statement.item.body.item, false);
	While { condition, body }
}

fn validate_integer_literal<'a>(context: &mut Context<'a, '_, '_>, literal: &tree::IntegerLiteral, span: Span) -> Expression<'a> {
	let value = IntegerValue::new(literal.value.item, literal.value.span);
	let kind = ExpressionKind::IntegerValue(value);
	let type_id = context.type_store.integer_type_id();
	Expression { span, type_id, is_mutable: true, returns: false, kind }
}

fn validate_float_literal<'a>(context: &mut Context<'a, '_, '_>, literal: &tree::FloatLiteral, span: Span) -> Expression<'a> {
	let value = DecimalValue::new(literal.value.item, literal.value.span);
	let kind = ExpressionKind::DecimalValue(value);
	let type_id = context.type_store.decimal_type_id();
	Expression { span, type_id, is_mutable: true, returns: false, kind }
}

fn validate_bool_literal<'a>(context: &mut Context<'a, '_, '_>, literal: bool, span: Span) -> Expression<'a> {
	let type_id = context.type_store.bool_type_id();
	let kind = ExpressionKind::BooleanLiteral(literal);
	Expression { span, type_id, is_mutable: true, returns: false, kind }
}

fn validate_codepoint_literal<'a>(
	context: &mut Context<'a, '_, '_>,
	literal: &tree::CodepointLiteral,
	span: Span,
) -> Expression<'a> {
	let kind = ExpressionKind::CodepointLiteral(CodepointLiteral { value: literal.value.item });
	let type_id = context.type_store.u32_type_id();
	Expression { span, type_id, is_mutable: true, returns: false, kind }
}

fn validate_byte_codepoint_literal<'a>(
	context: &mut Context<'a, '_, '_>,
	literal: &tree::ByteCodepointLiteral,
	span: Span,
) -> Expression<'a> {
	let kind = ExpressionKind::ByteCodepointLiteral(ByteCodepointLiteral { value: literal.value.item });
	let type_id = context.type_store.u8_type_id();
	Expression { span, type_id, is_mutable: true, returns: false, kind }
}

fn validate_string_literal<'a>(
	context: &mut Context<'a, '_, '_>,
	literal: &tree::StringLiteral<'a>,
	span: Span,
) -> Expression<'a> {
	let kind = ExpressionKind::StringLiteral(StringLiteral { value: literal.value.item.clone() });
	let type_id = context.type_store.string_type_id();
	Expression { span, type_id, is_mutable: true, returns: false, kind }
}

fn validate_array_literal<'a>(
	context: &mut Context<'a, '_, '_>,
	literal: &'a tree::ArrayLiteral<'a>,
	span: Span,
) -> Expression<'a> {
	let mut returns = false;
	let mut expressions = Vec::with_capacity(literal.expressions.len());
	for expression in literal.expressions {
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
	Expression { span, type_id, is_mutable: true, returns, kind }
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

	let type_entry = context.type_store.type_entries.read()[type_id.index()];
	let user_types = context.type_store.user_types.read();
	let indices = match &type_entry.kind {
		&TypeEntryKind::UserType { shape_index, specialization_index } => {
			let user_type = &user_types[shape_index];
			match &user_type.kind {
				UserTypeKind::Struct { .. } => Some((shape_index, specialization_index)),
				_ => None,
			}
		}

		_ => None,
	};

	let Some((shape_index, specialization_index)) = indices else {
		let name = context.type_name(type_id);
		let message = error!("Cannot construct type {name} like a struct as it is not a struct");
		context.messages.message(message.span(literal.parsed_type.span));
		return Expression::any_collapse(context.type_store, span);
	};

	let user_type = &user_types[shape_index];
	let shape = match &user_type.kind {
		UserTypeKind::Struct { shape } => shape,
		kind => unreachable!("{kind:?}"),
	};

	// Hate this clone
	let fields = shape.specializations[specialization_index].fields.clone();
	drop(user_types);

	let mut returns = false;
	let field_initializers = validate_struct_initializer(
		context,
		shape_index,
		specialization_index,
		type_id,
		&literal.initializer,
		&fields,
		&mut returns,
	);

	let kind = ExpressionKind::StructLiteral(StructLiteral { type_id, field_initializers });
	Expression { span, type_id, is_mutable: true, returns, kind }
}

fn validate_struct_initializer<'a>(
	context: &mut Context<'a, '_, '_>,
	shape_index: usize,
	specialization_index: usize,
	type_id: TypeId,
	initializer: &'a Node<tree::StructInitializer<'a>>,
	fields: &[Field<'a>],
	returns: &mut bool,
) -> Vec<FieldInitializer<'a>> {
	let mut fields = fields.iter();
	let mut field_initializers = Vec::new();

	for intializer in initializer.item.field_initializers {
		let field = fields.next();

		let mut scope = context.child_scope();
		scope.expected_type = field.map(|f| f.type_id);
		let mut expression = validate_expression(&mut scope, &intializer.expression);
		drop(scope);

		*returns |= expression.returns;

		let field = match field {
			Some(field) => field,

			None => {
				context.message(error!("Unexpected extra field initalizer").span(intializer.name.span));
				continue;
			}
		};

		if field.name != intializer.name.item {
			let error = error!("Expected initalizer for field `{}`, got `{}` instead", field.name, intializer.name.item);
			context.message(error.span(intializer.name.span));
		}

		let name = field.name;
		let is_private = matches!(field.attribute, Some(Node { item: FieldAttribute::Private, .. }));
		let is_readable = matches!(field.attribute, Some(Node { item: FieldAttribute::Readable, .. }));
		let external_access = if let Some(method_base_index) = context.method_base_index {
			method_base_index != shape_index
		} else {
			true
		};

		if is_private && external_access {
			let on = context.type_name(type_id);
			let error = error!("Cannot publicly initialize private field `{name}` on type {on}",);
			context.message(error.span(intializer.name.span + expression.span));
		}

		if is_readable && external_access {
			let on = context.type_name(type_id);
			let error = error!("Cannot publicly initialize readable field `{name}` on type {on}",);
			context.message(error.span(intializer.name.span + expression.span));
		}

		if !context.collapse_to(field.type_id, &mut expression).unwrap_or(true) {
			// Avoids a silly error message when something happend in the field definition, causing it to
			// have `AnyCollapse` as its type, leading to an "Expected `AnyCollapse` got `_`" error
			if !field.type_id.is_any_collapse(context.type_store) {
				let expected = context.type_name(field.type_id);
				let found = context.type_name(expression.type_id);
				let error = error!("Field intializer type mismatch, expected {expected} but got {found} instead");
				context.message(error.span(intializer.name.span + expression.span));
			}
		}

		field_initializers.push(FieldInitializer { expression });
	}

	let user_types = context.type_store.user_types.read();
	let user_type = &user_types[shape_index];
	let shape = match &user_type.kind {
		UserTypeKind::Struct { shape } => shape,
		kind => unreachable!("{kind:?}"),
	};
	let specialization = &shape.specializations[specialization_index];
	if field_initializers.len() < specialization.fields.len() {
		let error = error!(
			"Too few field initializers, expected {}, but only found {}",
			specialization.fields.len(),
			field_initializers.len(),
		);
		context.messages.message(error.span(initializer.span));
	}

	field_initializers
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
	for type_argument in call.type_arguments {
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
	let function_store_shapes = context.function_store.shapes.read();
	let shape = &function_store_shapes[function_shape_index];
	if shape.generic_parameters.implicit_len() != 0 {
		// The only functions with implicit generic parameters are inner functions, and if we have it
		// in scope then that means it must be somewhere within ourselves or our function parent chain
		let count = shape.generic_parameters.implicit_len();
		for parameter in &context.generic_parameters.parameters()[0..count] {
			type_arguments.push_implicit(parameter.generic_type_id);
		}
	}
	drop(function_store_shapes);

	let result = context.function_store.get_or_add_specialization(
		context.messages,
		context.type_store,
		context.module_path,
		context.function_generic_usages,
		function_shape_index,
		type_arguments,
		Some(span),
	);
	let FunctionSpecializationResult { specialization_index, return_type } = match result {
		Some(results) => results,
		None => return Expression::any_collapse(context.type_store, span),
	};

	let mut returns = false;
	let mut arguments = Vec::with_capacity(call.arguments.len());
	for (index, argument) in call.arguments.iter().enumerate() {
		let mut scope = context.child_scope();

		let function_store_shapes = scope.function_store.shapes.read();
		let shape = &function_store_shapes[function_shape_index];
		let specialization = &shape.specializations[specialization_index];
		scope.expected_type = match specialization.parameters.get(index) {
			Some(parameter) => Some(parameter.type_id),
			None => None,
		};
		drop(function_store_shapes);

		let argument = validate_expression(&mut scope, argument);
		returns |= argument.returns;
		arguments.push(argument);
	}

	let function_store_shapes = context.function_store.shapes.read();
	let shape = &function_store_shapes[function_shape_index];
	let specialization = &shape.specializations[specialization_index];
	let specialization_parameters = specialization.parameters.clone();
	let has_c_varargs = shape.c_varargs;
	drop(function_store_shapes);

	// Don't bail immediately with type mismatch, we want to check every argument and the argument count
	let mut arguments_type_mismatch = false;
	for (index, argument) in arguments.iter_mut().enumerate() {
		let parameter = match specialization_parameters.get(index) {
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

	if has_c_varargs {
		if arguments.len() < specialization_parameters.len() {
			let error = error!("Expected at least {} arguments, got {}", specialization_parameters.len(), arguments.len());
			context.message(error.span(span));
			return Expression::any_collapse(context.type_store, span);
		}

		let mut vararg_error = false;
		let remaining_arguments = &arguments[specialization_parameters.len()..];
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
	} else if arguments.len() != specialization_parameters.len() {
		let error = error!("Expected {} arguments, got {}", specialization_parameters.len(), arguments.len());
		context.message(error.span(span));
		return Expression::any_collapse(context.type_store, span);
	}

	if arguments_type_mismatch {
		return Expression::any_collapse(context.type_store, span);
	}

	if return_type.is_noreturn(context.type_store) {
		returns = true;
	}

	let function_id = FunctionId { function_shape_index, specialization_index };
	let kind = ExpressionKind::Call(Call { span, name, function_id, arguments });
	Expression { span, type_id: return_type, is_mutable: true, returns, kind }
}

fn get_method_function_specialization<'a>(
	context: &mut Context<'a, '_, '_>,
	call_type_arguments: &[Node<tree::Type<'a>>],
	function_shape_index: usize,
	base_shape_index: usize,
	base_specialization_index: usize,
	span: Span,
) -> Option<FunctionSpecializationResult> {
	let mut type_argument_lookup_errored = false;
	let mut explicit_arguments = Vec::new();
	for type_argument in call_type_arguments {
		if let Some(type_id) = context.lookup_type(type_argument) {
			explicit_arguments.push(type_id);
		} else {
			type_argument_lookup_errored = true;
		}
	}

	if type_argument_lookup_errored {
		return None;
	}

	let mut type_arguments = TypeArguments::new_from_explicit(explicit_arguments);
	let function_store_shapes = context.function_store.shapes.read();
	let shape = &function_store_shapes[function_shape_index];
	if shape.generic_parameters.implicit_len() != 0 {
		// The only functions with implicit generic parameters are inner functions, and if we have it
		// in scope then that means it must be somewhere within ourselves or our function parent chain
		let count = shape.generic_parameters.implicit_len();
		for parameter in &context.generic_parameters.parameters()[0..count] {
			type_arguments.push_implicit(parameter.generic_type_id);
		}
	}
	drop(function_store_shapes);

	let user_types = context.type_store.user_types.read();
	let user_type = &user_types[base_shape_index];
	let method_base_arguments = match &user_type.kind {
		UserTypeKind::Struct { shape } => shape.specializations[base_specialization_index].type_arguments.ids.as_slice(),
		UserTypeKind::Enum { shape } => shape.specializations[base_specialization_index].type_arguments.ids.as_slice(),
	};

	for &base_argument in method_base_arguments {
		type_arguments.push_method_base(base_argument);
	}

	drop(user_types);

	context.function_store.get_or_add_specialization(
		context.messages,
		context.type_store,
		context.module_path,
		context.function_generic_usages,
		function_shape_index,
		type_arguments,
		Some(span),
	)
}

struct MethodArgumentsResult<'a> {
	returns: bool,
	arguments: Vec<Expression<'a>>,
}

fn validate_method_arguments<'a>(
	context: &mut Context<'a, '_, '_>,
	call_arguments: &'a [Node<tree::Expression<'a>>],
	function_shape_index: usize,
	function_specialization_index: usize,
	span: Span,
	is_static: bool,
) -> Option<MethodArgumentsResult<'a>> {
	let maybe_self = if is_static { 0 } else { 1 };

	let mut returns = false;
	let mut arguments = Vec::with_capacity(call_arguments.len());
	for (index, argument) in call_arguments.iter().enumerate() {
		let mut scope = context.child_scope();

		let function_store_shapes = scope.function_store.shapes.read();
		let shape = &function_store_shapes[function_shape_index];
		let specialization = &shape.specializations[function_specialization_index];
		scope.expected_type = match specialization.parameters.get(index + maybe_self) {
			Some(parameter) => Some(parameter.type_id),
			None => None,
		};
		drop(function_store_shapes);

		let argument = validate_expression(&mut scope, argument);
		returns |= argument.returns;
		arguments.push(argument);
	}

	let function_store_shapes = context.function_store.shapes.read();
	let shape = &function_store_shapes[function_shape_index];
	let specialization = &shape.specializations[function_specialization_index];
	let specialization_parameters = specialization.parameters.clone();
	drop(function_store_shapes);

	// Don't bail immediately with type mismatch, we want to check every argument and the argument count
	let mut arguments_type_mismatch = false;
	for (index, argument) in arguments.iter_mut().enumerate() {
		let parameter = match specialization_parameters.get(index + maybe_self) {
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

	if arguments.len() != specialization_parameters.len() - maybe_self {
		let expected = specialization_parameters.len() - maybe_self;
		let error = error!("Expected {expected} arguments, got {}", arguments.len());
		context.message(error.span(span));
		return None;
	}

	if arguments_type_mismatch {
		return None;
	}

	Some(MethodArgumentsResult { returns, arguments })
}

fn validate_method_call<'a>(
	context: &mut Context<'a, '_, '_>,
	method_call: &'a tree::MethodCall<'a>,
	span: Span,
) -> Expression<'a> {
	let base = validate_expression(context, &method_call.base);
	if let ExpressionKind::Type(base_type) = base.kind {
		return validate_static_method_call(context, method_call, base_type, span);
	}

	let entry = context.type_store.type_entries.read()[base.type_id.index()];
	let (base_shape_index, base_specialization_index, base_mutable) = match entry.kind {
		TypeEntryKind::UserType { shape_index, specialization_index } => (shape_index, specialization_index, base.is_mutable),

		TypeEntryKind::Pointer { type_id, mutable } => {
			let entry = context.type_store.type_entries.read()[type_id.index()];
			let TypeEntryKind::UserType { shape_index, specialization_index } = entry.kind else {
				let found = context.type_name(base.type_id);
				let error = error!("Cannot call method on type {found}");
				context.message(error.span(span));
				return Expression::any_collapse(context.type_store, span);
			};

			(shape_index, specialization_index, mutable)
		}

		TypeEntryKind::BuiltinType { kind } if kind == PrimativeKind::AnyCollapse => {
			return Expression::any_collapse(context.type_store, span);
		}

		_ => {
			let found = context.type_name(base.type_id);
			let error = error!("Cannot call method on type {found}");
			context.message(error.span(span));
			return Expression::any_collapse(context.type_store, span);
		}
	};

	let user_types = context.type_store.user_types.read();
	let user_type = &user_types[base_shape_index];
	let method_info = match user_type.methods.get(method_call.name.item) {
		Some(method_info) => method_info,

		None => {
			let name = method_call.name.item;
			let found = context.type_name(base.type_id);
			let error = error!("No method {name} on type {found}");
			context.messages.message(error.span(span));
			return Expression::any_collapse(context.type_store, span);
		}
	};

	let function_shape_index = method_info.function_shape_index;
	let mutable_self = match method_info.kind {
		MethodKind::ImmutableSelf => false,
		MethodKind::MutableSelf => true,

		MethodKind::Static => {
			let name = method_call.name.item;
			let error = error!("Cannot call static method `{name}` on an object instance");
			context.messages.message(error.span(span));
			return Expression::any_collapse(context.type_store, span);
		}
	};

	drop(user_types);

	if mutable_self && !base_mutable {
		let name = method_call.name.item;
		let error = error!("Cannot call mutable method `{name}` on an immutable object");
		context.message(error.span(span));
	}

	let result = get_method_function_specialization(
		context,
		&method_call.type_arguments,
		function_shape_index,
		base_shape_index,
		base_specialization_index,
		span,
	);
	let FunctionSpecializationResult { specialization_index, return_type } = match result {
		Some(results) => results,
		None => return Expression::any_collapse(context.type_store, span),
	};

	let Some(MethodArgumentsResult { mut returns, arguments }) =
		validate_method_arguments(context, &method_call.arguments, function_shape_index, specialization_index, span, false)
	else {
		return Expression::any_collapse(context.type_store, span);
	};

	let function_id = FunctionId { function_shape_index, specialization_index };
	let method_call = MethodCall {
		base,
		mutable_self,
		span,
		name: method_call.name.item,
		function_id,
		arguments,
	};

	if return_type.is_noreturn(context.type_store) {
		returns = true;
	}

	let kind = ExpressionKind::MethodCall(Box::new(method_call));
	Expression { span, type_id: return_type, is_mutable: true, returns, kind }
}

fn validate_static_method_call<'a>(
	context: &mut Context<'a, '_, '_>,
	method_call: &'a tree::MethodCall<'a>,
	base_type_id: TypeId,
	span: Span,
) -> Expression<'a> {
	let entry = context.type_store.type_entries.read()[base_type_id.index()];
	let (base_shape_index, base_specialization_index) = match entry.kind {
		TypeEntryKind::UserType { shape_index, specialization_index } => (shape_index, specialization_index),

		TypeEntryKind::BuiltinType { kind } if kind == PrimativeKind::AnyCollapse => {
			return Expression::any_collapse(context.type_store, span);
		}

		_ => {
			let found = context.type_name(base_type_id);
			let error = error!("Cannot call static method on type {found}");
			context.message(error.span(span));
			return Expression::any_collapse(context.type_store, span);
		}
	};

	// Holy mother of indentation
	let user_types = context.type_store.user_types.read();
	let user_type = &user_types[base_shape_index];
	match &user_type.kind {
		UserTypeKind::Enum { shape } => {
			let specialization = &shape.specializations[base_specialization_index];
			if let Some(&variant_index) = specialization.variants_by_name.get(method_call.name.item) {
				let variant = specialization.variants[variant_index];
				drop(user_types);
				if variant.is_transparent {
					if !method_call.type_arguments.is_empty() {
						let span = method_call
							.type_arguments
							.iter()
							.fold(method_call.type_arguments.first().unwrap().span, |a, b| a + b.span);

						let name = method_call.name.item;
						let error = error!("Cannot construct transparent variant enum `{name}` with type arguments");
						context.messages.message(error.span(span));
					}

					if method_call.arguments.len() != 1 {
						let name = method_call.name.item;
						let count = method_call.arguments.len();
						let error = error!("Transparent variant enum `{name}` must be constructed with one value, found {count}");
						context.messages.message(error.span(span));
						return Expression::any_collapse(context.type_store, span);
					}

					let expression = method_call.arguments.first().unwrap();

					let type_entries = context.type_store.type_entries.read();
					let user_types = context.type_store.user_types.read();
					let (_, as_struct) = variant.type_id.as_struct(&type_entries, &user_types).unwrap();
					let expected_type_id = as_struct.fields.first().unwrap().type_id;
					drop(user_types);
					drop(type_entries);

					let mut returns = false;
					let Some(field_initializers) =
						validate_transparent_variant_initializer(context, &mut returns, expression, expected_type_id)
					else {
						return Expression::any_collapse(context.type_store, span);
					};

					let type_id = variant.type_id;
					let literal = StructLiteral { type_id, field_initializers };
					let kind = ExpressionKind::StructLiteral(literal);
					return Expression { span, type_id, is_mutable: true, returns, kind };
				}
			} else {
				drop(user_types);
			}
		}

		_ => drop(user_types),
	};

	let user_types = context.type_store.user_types.read();
	let user_type = &user_types[base_shape_index];
	let method_info = match user_type.methods.get(method_call.name.item) {
		Some(method_info) => method_info,

		None => {
			let name = method_call.name.item;
			let found = context.type_name(base_type_id);
			let error = error!("No static method {name} on type {found}");
			context.messages.message(error.span(span));
			return Expression::any_collapse(context.type_store, span);
		}
	};

	match method_info.kind {
		MethodKind::ImmutableSelf | MethodKind::MutableSelf => {
			let name = method_call.name.item;
			let error = error!("Cannot call instance method `{name}` statically");
			context.messages.message(error.span(span));
			return Expression::any_collapse(context.type_store, span);
		}

		MethodKind::Static => {}
	};
	let function_shape_index = method_info.function_shape_index;
	drop(user_types);

	let result = get_method_function_specialization(
		context,
		&method_call.type_arguments,
		function_shape_index,
		base_shape_index,
		base_specialization_index,
		span,
	);
	let FunctionSpecializationResult { specialization_index, return_type } = match result {
		Some(results) => results,
		None => return Expression::any_collapse(context.type_store, span),
	};

	let Some(MethodArgumentsResult { mut returns, arguments }) =
		validate_method_arguments(context, &method_call.arguments, function_shape_index, specialization_index, span, true)
	else {
		return Expression::any_collapse(context.type_store, span);
	};

	let function_id = FunctionId { function_shape_index, specialization_index };
	let call = Call { span, name: method_call.name.item, function_id, arguments };

	if return_type.is_noreturn(context.type_store) {
		returns = true;
	}

	let kind = ExpressionKind::Call(call);
	Expression { span, type_id: return_type, is_mutable: true, returns, kind }
}

fn validate_read<'a>(context: &mut Context<'a, '_, '_>, read: &tree::Read<'a>, span: Span) -> Expression<'a> {
	let symbol = match context.lookup_symbol(&read.path_segments.item) {
		Some(symbol) => symbol,
		None => return Expression::any_collapse(context.type_store, span),
	};

	fn disallow_type_arguments(context: &mut Context, read: &tree::Read, span: Span, label: &str) {
		if read.type_arguments.len() > 0 {
			let error = error!("Type arguments not permitted on {label}");
			context.message(error.span(span));
		}
	}

	let readable_index = match symbol.kind {
		SymbolKind::Let { readable_index } | SymbolKind::Mut { readable_index } => readable_index,

		SymbolKind::Const { constant_index } => {
			disallow_type_arguments(context, read, span, "a const read");

			let constant = &context.constants.read()[constant_index];
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

			return Expression { span, type_id, is_mutable: false, returns: false, kind };
		}

		SymbolKind::Static { static_index } => {
			disallow_type_arguments(context, read, span, "a static read");

			let static_instance = &context.statics.read().statics[static_index];
			let static_read = StaticRead {
				name: static_instance.name,
				type_id: static_instance.type_id,
				static_index,
			};

			let type_id = static_instance.type_id;
			let kind = ExpressionKind::StaticRead(static_read);
			return Expression { span, type_id, is_mutable: false, returns: false, kind };
		}

		SymbolKind::BuiltinType { type_id } => {
			disallow_type_arguments(context, read, span, "a builtin type");

			if type_id.is_void(context.type_store) {
				return Expression::void(context.type_store, span);
			} else {
				let kind = ExpressionKind::Type(type_id);
				return Expression { span, type_id, is_mutable: false, returns: false, kind };
			}
		}

		SymbolKind::Type { shape_index } => {
			let Some(type_id) = context.type_store.get_or_add_shape_specialization_in_scope(
				context.messages,
				context.function_store,
				context.module_path,
				context.function_generic_usages,
				context.root_layers,
				context.symbols_scope.symbols,
				shape_index,
				Some(span),
				context.function_initial_scope_count,
				context.generic_parameters,
				&read.type_arguments,
			) else {
				return Expression::any_collapse(context.type_store, span);
			};

			let kind = ExpressionKind::Type(type_id);
			return Expression { span, type_id, is_mutable: false, returns: false, kind };
		}

		kind => {
			context.message(error!("Cannot read value from {kind}").span(read.path_segments.span));
			return Expression::any_collapse(context.type_store, span);
		}
	};

	let Some(readable) = context.readables.get(readable_index) else {
		// dbg!(&context.readables);
		panic!("Symbol pointed to unknown readable index {readable_index}, {read:?}");
	};

	let is_mutable = readable.kind == ReadableKind::Mut;
	let read = Read {
		name: readable.name,
		type_id: readable.type_id,
		readable_index,
	};

	let type_id = readable.type_id;
	let kind = ExpressionKind::Read(read);
	Expression { span, type_id, is_mutable, returns: false, kind }
}

fn validate_dot_access<'a>(context: &mut Context<'a, '_, '_>, dot_access: &'a tree::DotAccess<'a>, span: Span) -> Expression<'a> {
	let base = validate_expression(context, &dot_access.base);
	if base.type_id.is_any_collapse(context.type_store) {
		return Expression::any_collapse(context.type_store, span);
	}

	if let ExpressionKind::Type(base) = base.kind {
		return validate_dot_access_enum_literal(context, base, dot_access, span);
	}

	let (type_id, mutable) = match base.type_id.as_pointed(context.type_store) {
		Some(as_pointer) => (as_pointer.type_id, as_pointer.mutable),
		None => (base.type_id, base.is_mutable),
	};

	let type_entries = context.type_store.type_entries.read();
	let user_types = context.type_store.user_types.read();

	// Dumb hack to store fields array in outer scope so a slice can be taken
	let slice_fields;
	let str_fields;

	let mut external_access = true;
	let fields: &[Field] = if let Some((_, as_struct)) = type_id.as_struct(&type_entries, &user_types) {
		if let Some(method_base_index) = context.method_base_index {
			external_access = method_base_index != as_struct.shape_index;
		}
		&as_struct.fields
	} else if let Some(as_enum) = type_id.as_enum(&type_entries, &user_types) {
		if let Some(method_base_index) = context.method_base_index {
			external_access = method_base_index != as_enum.shape_index;
		}
		&as_enum.shared_fields
	} else if let Some(as_slice) = type_id.as_slice(&type_entries) {
		drop(user_types);
		drop(type_entries);
		slice_fields = [
			Field {
				span: None,
				name: "pointer",
				type_id: context.type_store.pointer_to(as_slice.type_id, as_slice.mutable),
				attribute: None,
				read_only: true,
			},
			Field {
				span: None,
				name: "length",
				type_id: context.type_store.isize_type_id(),
				attribute: None,
				read_only: true,
			},
		];
		&slice_fields
	} else if type_id.is_string(context.type_store) {
		drop(user_types);
		drop(type_entries);
		let u8_type_id = context.type_store.u8_type_id();
		str_fields = [
			Field {
				span: None,
				name: "pointer",
				type_id: context.type_store.pointer_to(u8_type_id, false),
				attribute: None,
				read_only: true,
			},
			Field {
				span: None,
				name: "length",
				type_id: context.type_store.isize_type_id(),
				attribute: None,
				read_only: true,
			},
			Field {
				span: None,
				name: "bytes",
				type_id: context.type_store.u8_slice_type_id(),
				attribute: None,
				read_only: true,
			},
		];
		&str_fields
	} else {
		let on = base.kind.name_with_article();
		let found = context.type_name(base.type_id);
		let error = error!("Cannot access field on {on} of type {found}");
		context.messages.message(error.span(span));
		return Expression::any_collapse(context.type_store, span);
	};

	// TODO: Hashmapify this linear lookup
	let mut fields = fields.iter().enumerate();
	let Some((field_index, field)) = fields.find(|f| f.1.name == dot_access.name.item) else {
		let type_name = context.type_name(base.type_id);
		let error = error!("No field `{}` on {}", dot_access.name.item, type_name);
		context.messages.message(error.span(dot_access.name.span));
		return Expression::any_collapse(context.type_store, span);
	};

	let is_private = matches!(field.attribute, Some(Node { item: FieldAttribute::Private, .. }));
	let is_readable = matches!(field.attribute, Some(Node { item: FieldAttribute::Readable, .. }));
	let is_read_only = field.read_only;

	if external_access && is_private {
		let type_name = context.type_name(base.type_id);
		let error = error!("Cannot publicly access private field `{}` on type {}", dot_access.name.item, type_name);
		context.messages.message(error.span(dot_access.name.span));
		return Expression::any_collapse(context.type_store, span);
	}

	let type_id = field.type_id;
	let (is_mutable, reason) = if external_access && is_readable {
		(false, Some(FieldReadImmutableReason::Readable))
	} else if is_read_only {
		(false, Some(FieldReadImmutableReason::ReadOnly))
	} else {
		(mutable, None)
	};

	let field_read = FieldRead {
		base,
		name: field.name,
		type_id,
		field_index,
		immutable_reason: reason,
	};
	let kind = ExpressionKind::FieldRead(Box::new(field_read));
	Expression { span, type_id, is_mutable, returns: false, kind }
}

fn validate_inferred_enum<'a>(
	context: &mut Context<'a, '_, '_>,
	inferred_enum: &'a tree::InferredEnum<'a>,
	span: Span,
) -> Expression<'a> {
	let Some(expected_type) = context.expected_type else {
		let error = error!("Cannot infer enum type for variant, there is no specific expected type in this context");
		context.message(error.span(span));
		return Expression::any_collapse(context.type_store, span);
	};

	let type_entries = context.type_store.type_entries.read();
	let user_types = context.type_store.user_types.read();
	let Some(as_enum) = expected_type.as_enum(&type_entries, &user_types) else {
		let error = error!("Cannot infer enum type for variant, the expected type is not an enum");
		context.messages.message(error.span(span));
		return Expression::any_collapse(context.type_store, span);
	};

	let Some(&variant_index) = as_enum.variants_by_name.get(inferred_enum.name.item) else {
		let expected = context.type_name(as_enum.type_id);
		let error = error!("Expected enum {expected} has no variant named `{}`", inferred_enum.name.item);
		context.messages.message(error.span(inferred_enum.name.span));
		return Expression::any_collapse(context.type_store, span);
	};

	let variant = as_enum.variants[variant_index];
	let type_id = variant.type_id;
	drop(user_types);
	drop(type_entries);

	let mut returns = false;
	let Some(field_initializers) =
		validate_enum_initializer(context, inferred_enum.name.item, type_id, &mut returns, inferred_enum.initializer, span)
	else {
		return Expression::any_collapse(context.type_store, span);
	};

	let literal = StructLiteral { type_id, field_initializers };
	let kind = ExpressionKind::StructLiteral(literal);
	Expression { span, type_id, is_mutable: true, returns, kind }
}

fn validate_dot_access_enum_literal<'a>(
	context: &mut Context<'a, '_, '_>,
	base: TypeId,
	dot_access: &'a tree::DotAccess<'a>,
	span: Span,
) -> Expression<'a> {
	let Some(variant_type_id) = context.get_enum_variant(base, dot_access.name) else {
		return Expression::any_collapse(context.type_store, span);
	};

	let mut returns = false;
	let Some(field_initializers) = validate_enum_initializer(
		context,
		dot_access.name.item,
		variant_type_id,
		&mut returns,
		dot_access.enum_initializer,
		span,
	) else {
		return Expression::any_collapse(context.type_store, span);
	};

	let literal = StructLiteral { type_id: variant_type_id, field_initializers };
	let kind = ExpressionKind::StructLiteral(literal);
	Expression {
		span,
		type_id: variant_type_id,
		is_mutable: true,
		returns,
		kind,
	}
}

fn validate_enum_initializer<'a>(
	context: &mut Context<'a, '_, '_>,
	variant_name: &str,
	variant_type_id: TypeId,
	returns: &mut bool,
	initializer: Option<&'a tree::EnumInitializer<'a>>,
	span: Span,
) -> Option<Vec<FieldInitializer<'a>>> {
	let entry = context.type_store.type_entries.read()[variant_type_id.index()];
	let (variant_shape_index, variant_specialization_index) = match entry.kind {
		TypeEntryKind::UserType { shape_index, specialization_index } => (shape_index, specialization_index),
		_ => unreachable!(),
	};

	let user_types = context.type_store.user_types.read();
	let user_type = &user_types[variant_shape_index];
	let fields = match &user_type.kind {
		UserTypeKind::Struct { shape } => {
			if shape.is_transparent_variant {
				let Some(initializer) = initializer else {
					let error = error!("Cannot construct transparent enum variant `{variant_name}` without an initializer");
					context.messages.message(error.span(span));
					return None;
				};

				let expression = match initializer {
					EnumInitializer::Transparent { expression } => expression,

					EnumInitializer::StructLike { .. } => {
						context.messages.message(
							error!("Cannot construct transparent enum variant `{variant_name}` like a struck-like enum variant")
								.span(span),
						);
						return None;
					}
				};

				let expected_type_id = shape.specializations[variant_specialization_index]
					.fields
					.first()
					.unwrap()
					.type_id;

				drop(user_types);
				return validate_transparent_variant_initializer(context, returns, expression, expected_type_id);
			}

			// Uggg
			shape.specializations[variant_specialization_index].fields.clone()
		}

		UserTypeKind::Enum { .. } => unreachable!(),
	};

	drop(user_types);

	if let Some(initializer) = initializer {
		if fields.is_empty() {
			let error = match initializer {
				EnumInitializer::StructLike { .. } => {
					error!("Cannot construct enum variant `{variant_name}` with field initializers")
				}
				EnumInitializer::Transparent { .. } => {
					error!("Cannot construct enum variant `{variant_name}` with a transparent initializer")
				}
			};
			context.message(error.span(span));
			return None;
		}

		let struct_initializer = match initializer {
			EnumInitializer::StructLike { struct_initializer } => struct_initializer,

			EnumInitializer::Transparent { .. } => {
				let error = error!("Cannot construct struck-like enum variant `{variant_name}` like a transparent enum variant");
				context.message(error.span(span));
				return None;
			}
		};

		Some(validate_struct_initializer(
			context,
			variant_shape_index,
			variant_specialization_index,
			variant_type_id,
			struct_initializer,
			&fields,
			returns,
		))
	} else if !fields.is_empty() {
		let name = variant_name;
		let error = error!(
			"Cannot construct struck-like enum variant `{}` without providing fields initializers",
			name
		);
		context.message(error.span(span));
		return None;
	} else {
		Some(Vec::new())
	}
}

fn validate_transparent_variant_initializer<'a>(
	context: &mut Context<'a, '_, '_>,
	returns: &mut bool,
	expression: &'a Node<tree::Expression<'a>>,
	expected_type_id: TypeId,
) -> Option<Vec<FieldInitializer<'a>>> {
	let mut scope = context.child_scope();
	scope.expected_type = Some(expected_type_id);
	let mut expression = validate_expression(&mut scope, expression);
	drop(scope);

	*returns |= expression.returns;

	if !context.collapse_to(expected_type_id, &mut expression).unwrap_or(true) {
		if !expected_type_id.is_any_collapse(context.type_store) {
			let expected = context.type_name(expected_type_id);
			let found = context.type_name(expression.type_id);
			let error = error!("Transparent enum variant initializer type mismatch, expected {expected} but got {found} instead");
			context.message(error.span(expression.span));
			return None;
		}
	}

	let initializer = FieldInitializer { expression };
	Some(vec![initializer])
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
			expression.span += span;
			return expression;
		}

		(UnaryOperator::Negate, ExpressionKind::DecimalValue(value)) => {
			value.negate(span);
			expression.span += span;
			return expression;
		}

		(UnaryOperator::Invert, ExpressionKind::BooleanLiteral(value)) => {
			*value = !*value;
			expression.span += span;
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
			return Expression { span, type_id, is_mutable: true, returns, kind };
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
			return Expression { span, type_id, is_mutable: true, returns, kind };
		}

		UnaryOperator::AddressOf => {
			let type_id = context.type_store.pointer_to(type_id, false);
			let kind = ExpressionKind::UnaryOperation(Box::new(UnaryOperation { op, type_id, expression }));
			return Expression { span, type_id, is_mutable: true, returns, kind };
		}

		UnaryOperator::AddressOfMut => {
			if !expression.is_mutable {
				let error = error!("Cannot take mutable address of immutable value");
				context.message(error.span(span));
				return Expression::any_collapse(context.type_store, span);
			}

			let type_id = context.type_store.pointer_to(type_id, true);
			let kind = ExpressionKind::UnaryOperation(Box::new(UnaryOperation { op, type_id, expression }));
			return Expression { span, type_id, is_mutable: true, returns, kind };
		}

		UnaryOperator::Dereference => {
			let Some((type_id, is_mutable)) = context.type_store.pointed_to(type_id) else {
				let error = error!("Cannot dereference {} as it is not a pointer", context.type_name(type_id));
				context.message(error.span(span));
				return Expression::any_collapse(context.type_store, span);
			};

			let kind = ExpressionKind::UnaryOperation(Box::new(UnaryOperation { op, type_id, expression }));
			return Expression { span, type_id, is_mutable, returns, kind };
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
			_ = context.collapse_to(to_type_id, &mut expression);
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
			let is_isize = context
				.type_store
				.direct_match(from_type_id, context.type_store.isize_type_id());
			let is_usize = context
				.type_store
				.direct_match(from_type_id, context.type_store.usize_type_id());
			let is_untyped_integer = from_type_id.is_untyped_integer(context.type_store);

			if !is_i64 && !is_u64 && !is_isize && !is_usize && !is_untyped_integer {
				let error = error!("Cannot cast {} to a pointer as it is too small", context.type_name(from_type_id));
				context.message(error.span(span));
			} else if from_type_id.is_untyped_integer(context.type_store) {
				_ = context.collapse_to(context.type_store.usize_type_id(), &mut expression);
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
	Expression { span, type_id, is_mutable: true, returns, kind }
}

fn validate_bracket_index<'a>(
	context: &mut Context<'a, '_, '_>,
	expression: Expression<'a>,
	index_expression: &'a Node<tree::Expression<'a>>,
	span: Span,
) -> Expression<'a> {
	let mut index_expression = validate_expression(context, index_expression);

	let (type_id, is_mutable) = if let Some(sliced) = context.type_store.sliced_of(expression.type_id) {
		sliced
	} else if expression.type_id.is_string(context.type_store) {
		(context.type_store.u8_type_id(), false)
	} else {
		let error = error!("Cannot index on a value of type {}", context.type_name(expression.type_id));
		context.message(error.span(span));
		(context.type_store.any_collapse_type_id(), true)
	};

	let isize_type_id = context.type_store.isize_type_id();
	let collapsed = context.collapse_to(isize_type_id, &mut index_expression);
	if !collapsed.unwrap_or(true) {
		let error = error!("Cannot index by a value of {}", context.type_name(index_expression.type_id));
		context.message(error.span(index_expression.span));
	}

	let returns = expression.returns || index_expression.returns;
	let op = UnaryOperator::Index { index_expression };
	let kind = ExpressionKind::UnaryOperation(Box::new(UnaryOperation { op, type_id, expression }));
	Expression { span, type_id, is_mutable, returns, kind }
}

fn validate_binary_operation<'a>(
	context: &mut Context<'a, '_, '_>,
	operation: &'a tree::BinaryOperation<'a>,
	span: Span,
) -> Expression<'a> {
	let op = operation.op.item;

	let original_can_is_bind = context.can_is_bind;
	if op != BinaryOperator::LogicalAnd {
		context.can_is_bind = false;
	}
	let mut left = validate_expression(context, &operation.left);
	let mut right = validate_expression(context, &operation.right);
	context.can_is_bind = original_can_is_bind;

	let collapsed = context.collapse_fair(&mut left, &mut right);
	let Ok(collapsed) = collapsed else {
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

	if !left.type_id.is_any_collapse(context.type_store) {
		match op {
			BinaryOperator::Assign => {}

			BinaryOperator::Modulo | BinaryOperator::ModuloAssign => {
				if !left.type_id.is_integer(context.type_store) {
					let found = context.type_name(left.type_id);
					let error = error!("Cannot perform modulo on non-integer type {found}");
					context.message(error.span(span));
					return Expression::any_collapse(context.type_store, span);
				}
			}

			BinaryOperator::BitwiseAnd
			| BinaryOperator::BitwiseAndAssign
			| BinaryOperator::BitwiseOr
			| BinaryOperator::BitwiseOrAssign
			| BinaryOperator::BitwiseXor
			| BinaryOperator::BitwiseXorAssign => {
				if !left.type_id.is_bool(context.type_store) && !left.type_id.is_integer(context.type_store) {
					let found = context.type_name(left.type_id);
					let error = error!("Cannot perform bitwise operation on {found}, type must be integer or boolean");
					context.message(error.span(span));
					return Expression::any_collapse(context.type_store, span);
				}
			}

			BinaryOperator::Add
			| BinaryOperator::AddAssign
			| BinaryOperator::Sub
			| BinaryOperator::SubAssign
			| BinaryOperator::Mul
			| BinaryOperator::MulAssign
			| BinaryOperator::Div
			| BinaryOperator::DivAssign => {
				if !left.type_id.is_numeric(context.type_store) {
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
				if !left.type_id.is_integer(context.type_store) {
					let found = context.type_name(left.type_id);
					let error = error!("Cannot perform bitshift on non-integer type {found}");
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
	}

	let type_id = match op {
		BinaryOperator::Assign
		| BinaryOperator::AddAssign
		| BinaryOperator::SubAssign
		| BinaryOperator::MulAssign
		| BinaryOperator::DivAssign
		| BinaryOperator::ModuloAssign
		| BinaryOperator::BitshiftLeftAssign
		| BinaryOperator::BitshiftRightAssign => {
			if let ExpressionKind::Read(read) = &left.kind {
				let readable = match context.readables.get(read.readable_index) {
					Some(readable) => readable,
					None => return Expression::any_collapse(context.type_store, span),
				};

				if readable.kind != ReadableKind::Mut {
					context.message(error!("Cannot assign to immutable binding `{}`", read.name).span(span));
				}
			} else if let ExpressionKind::FieldRead(field_read) = &left.kind {
				if !left.is_mutable {
					let name = field_read.name;
					let base = context.type_name(field_read.base.type_id);
					let error = match field_read.immutable_reason {
						Some(FieldReadImmutableReason::Readable) => {
							error!("Cannot publicly assign to readable field `{name}` on type {base}")
						}
						Some(FieldReadImmutableReason::ReadOnly) => {
							error!("Cannot assign to readonly field `{name}` on type {base}")
						}
						None => error!("Cannot assign to field of immutable object"),
					};

					context.message(error.span(span));
				}
			} else if matches!(&left.kind, ExpressionKind::UnaryOperation(op) if matches!(op.as_ref(), UnaryOperation { op: UnaryOperator::Dereference, .. }))
			{
				if !left.is_mutable {
					context.message(error!("Cannot assign immutable memory location").span(span));
				}
			} else if matches!(&left.kind, ExpressionKind::UnaryOperation(op) if matches!(op.as_ref(), UnaryOperation { op: UnaryOperator::Index { .. }, .. }))
			{
				if !left.is_mutable {
					context.message(error!("Cannot assign to index of immutable slice").span(span));
				}
			} else if !matches!(left.kind, ExpressionKind::AnyCollapse) {
				context.message(error!("Cannot assign to {}", left.kind.name_with_article()).span(span));
			}

			context.type_store.void_type_id()
		}

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
	Expression { span, type_id, is_mutable: true, returns, kind }
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
			BinaryOperator::BitwiseAnd => left.bitwise_and(right),
			BinaryOperator::BitwiseOr => left.bitwise_or(right),
			BinaryOperator::BitwiseXor => left.bitwise_xor(right),
			_ => return None,
		};

		let span = value.span();
		let kind = ExpressionKind::IntegerValue(value);
		let type_id = context.type_store.integer_type_id();
		return Some(Expression { span, type_id, is_mutable: true, returns: false, kind });
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
		return Some(Expression { span, type_id, is_mutable: true, returns: false, kind });
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
		return Some(Expression { span, type_id, is_mutable: true, returns: false, kind });
	}

	None
}

fn validate_check_is<'a>(context: &mut Context<'a, '_, '_>, check: &'a tree::CheckIs<'a>, span: Span) -> Expression<'a> {
	let left = validate_expression(context, &check.left);
	if left.type_id.is_any_collapse(context.type_store) {
		return Expression::any_collapse(context.type_store, span);
	}

	let (left_type_id, is_mutable) = match left.type_id.as_pointed(context.type_store) {
		Some(as_pointer) => (as_pointer.type_id, as_pointer.mutable),
		None => (left.type_id, left.is_mutable),
	};

	let enum_entry = context.type_store.type_entries.read()[left_type_id.index()];
	let user_types = context.type_store.user_types.read();
	let enum_specialization = match enum_entry.kind {
		TypeEntryKind::UserType { shape_index, specialization_index } => match &user_types[shape_index].kind {
			UserTypeKind::Struct { .. } => None,
			UserTypeKind::Enum { shape } => Some(&shape.specializations[specialization_index]),
		},

		_ => None,
	};

	let Some(enum_specialization) = enum_specialization else {
		let found = context.type_name(left_type_id);
		let error = error!("Cannot check is on type {found} as it is not an enum");
		context.messages.message(error.span(left.span));
		return Expression::any_collapse(context.type_store, span);
	};

	let mut encountered_variants = Vec::<Option<Span>>::with_capacity(enum_specialization.variants_by_name.len());
	for _ in 0..enum_specialization.variants_by_name.len() {
		encountered_variants.push(None);
	}

	let mut variant_infos = Vec::with_capacity(check.variant_names.len());

	for variant_name in check.variant_names {
		let (variant_type_id, variant_index) = match enum_specialization.variants_by_name.get(variant_name.item) {
			Some(&variant_index) => {
				let variant = &enum_specialization.variants[variant_index];
				(variant.type_id, variant_index)
			}

			None => {
				let found = context.type_name(left_type_id);
				let error = error!("No variant `{}` found on enum {found}", variant_name.item);
				context.messages.message(error.span(variant_name.span));
				continue;
			}
		};

		let info = VariantInfo { type_id: variant_type_id, variant_index };
		variant_infos.push(info);

		if let Some(existing) = encountered_variants[variant_index] {
			let found = context.type_name(variant_type_id);
			let warning = warning!("Duplicate enum variant {found} in list to check is");
			let noted = warning.note(note!(existing, "Existing variant entry here"));
			context.messages.message(noted.span(variant_name.span));
		}
		encountered_variants[variant_index] = Some(variant_name.span);
	}

	drop(user_types);

	let binding_name = if !context.can_is_bind {
		if let Some(binding_name) = check.binding_name {
			// TODO: Figure out how to explain this to the user better
			let error = error!("Cannot bind `is` check to a name in the current context");
			context.message(error.span(binding_name.span));
		}
		None
	} else if let Some(binding_name) = check.binding_name {
		Some(binding_name)
	} else if let ExpressionKind::Read(read) = &left.kind {
		if check.variant_names.len() == 1 {
			Some(Node::new(read.name, left.span))
		} else {
			None
		}
	} else {
		None
	};

	let binding = if let Some(binding_name) = binding_name {
		assert_eq!(check.variant_names.len(), 1);

		let type_id = if let Some(info) = variant_infos.last() {
			let type_entries = context.type_store.type_entries.read();
			let user_types = context.type_store.user_types.read();
			let (shape, specialization) = info.type_id.as_struct(&type_entries, &user_types).unwrap();
			if shape.is_transparent_variant {
				assert_eq!(specialization.fields.len(), 1);
				specialization.fields.first().unwrap().type_id
			} else {
				info.type_id
			}
		} else {
			context.type_store.any_collapse_type_id()
		};

		let kind = match is_mutable {
			true => ReadableKind::Mut,
			false => ReadableKind::Let,
		};

		let readable_index = context.push_readable(binding_name, type_id, kind);
		Some(CheckIsResultBinding { type_id, readable_index, is_mutable })
	} else {
		None
	};

	let type_id = context.type_store.bool_type_id();
	let returns = left.returns;
	let check_is = CheckIs { left, binding, variant_infos };
	let kind = ExpressionKind::CheckIs(Box::new(check_is));
	Expression { span, type_id, is_mutable: true, returns, kind }
}
