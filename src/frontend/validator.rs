use bumpalo_herd::Herd;
use rustc_hash::FxHashMap;

use crate::cli::CliArguments;
use crate::frontend::error::*;
use crate::frontend::function_store::{FunctionStore, MethodBaseType};
use crate::frontend::ir::*;
use crate::frontend::lang_items::LangItems;
use crate::frontend::root_layers::{RootLayer, RootLayers};
use crate::frontend::span::Span;
use crate::frontend::symbols::{Externs, ReadableKind, Readables, Statics, Symbol, SymbolKind, Symbols, SymbolsScope};
use crate::frontend::tree::{self, BinaryOperator, FieldAttribute, MethodKind, Node, PathSegments};
use crate::frontend::type_store::*;
use crate::frontend::when::WhenContext;
use crate::frontend::yield_targets::YieldTargets;
use crate::lock::{ReadGuard, RwLock};
use crate::reference::{Ref, SliceRef};

#[derive(Debug)]
pub struct Context<'a, 'b, 'c> {
	pub cli_arguments: &'a CliArguments,
	pub herd_member: &'b bumpalo_herd::Member<'a>,

	pub file_index: u32,
	pub module_path: &'a [String],
	pub parsed_files: &'b [tree::File<'a>],

	pub when_context: &'b WhenContext,

	pub next_scope_index: &'b mut usize,
	pub scope_index: usize,

	pub messages: &'b mut Messages<'a>,

	pub type_shape_indicies: &'b mut Vec<usize>,
	pub trait_shape_indicies: &'b mut Vec<usize>,

	pub type_store: &'b mut TypeStore<'a>,
	pub function_store: &'b FunctionStore<'a>,
	pub function_generic_usages: &'b mut Vec<GenericUsage>,

	pub root_layers: &'b RootLayers<'a>,

	pub lang_items: &'b RwLock<LangItems>,
	pub externs: &'b RwLock<Externs<'a>>,
	pub constants: &'b RwLock<Vec<ConstantValue<'a>>>,
	pub statics: &'b RwLock<Statics<'a>>,

	pub initial_readables_starting_index: usize,
	pub initial_readables_overall_len: usize,
	pub readables: &'b mut Readables<'a>,

	pub initial_yield_targets_starting_index: usize,
	pub initial_yield_targets_overall_len: usize,
	pub yield_targets: &'b mut YieldTargets,
	pub current_yield_target_index: Option<usize>,

	pub initial_local_function_shape_indicies_len: usize,
	pub local_function_shape_indicies: &'b mut Vec<usize>,

	pub function_initial_symbols_length: usize,
	pub symbols_scope: SymbolsScope<'a, 'b>,

	pub next_loop_index: usize,
	pub current_loop_index: Option<usize>,

	pub can_is_bind: bool,
	pub expected_type: Option<TypeId>,

	pub return_type: Option<TypeId>,
	pub generic_parameters: &'c GenericParameters<'a>,
	pub method_base_index: Option<usize>,
	pub is_extension_method: bool,
}

impl<'a, 'b, 'c> Drop for Context<'a, 'b, 'c> {
	fn drop(&mut self) {
		self.readables.readables.truncate(self.initial_readables_overall_len);
		self.readables.starting_index = self.initial_readables_starting_index;

		self.yield_targets.targets.truncate(self.initial_yield_targets_overall_len);
		self.yield_targets.starting_index = self.initial_yield_targets_starting_index;
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
			parsed_files: self.parsed_files,

			when_context: self.when_context,

			next_scope_index: self.next_scope_index,
			scope_index,

			messages: self.messages,

			type_shape_indicies: self.type_shape_indicies,
			trait_shape_indicies: self.trait_shape_indicies,

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

			initial_yield_targets_starting_index: self.yield_targets.starting_index,
			initial_yield_targets_overall_len: self.yield_targets.overall_len(),
			yield_targets: self.yield_targets,
			current_yield_target_index: self.current_yield_target_index,

			initial_local_function_shape_indicies_len: self.local_function_shape_indicies.len(),
			local_function_shape_indicies: self.local_function_shape_indicies,

			function_initial_symbols_length: self.function_initial_symbols_length, // TODO: Wrong?
			symbols_scope: self.symbols_scope.child_scope(),

			next_loop_index: self.next_loop_index,
			current_loop_index: self.current_loop_index,

			can_is_bind: false,
			expected_type: self.expected_type,

			return_type: self.return_type,
			generic_parameters: self.generic_parameters,
			method_base_index: self.method_base_index,
			is_extension_method: self.is_extension_method,
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

		let initial_yield_targets_starting_index = self.yield_targets.starting_index;
		self.yield_targets.starting_index = self.yield_targets.overall_len();

		let function_initial_symbols_length = self.symbols_scope.symbols.symbols.len();

		Context {
			cli_arguments: self.cli_arguments,
			herd_member: self.herd_member,

			file_index: self.file_index,
			module_path: self.module_path,
			parsed_files: self.parsed_files,

			when_context: self.when_context,

			next_scope_index: self.next_scope_index,
			scope_index,

			messages: self.messages,

			type_shape_indicies: self.type_shape_indicies,
			trait_shape_indicies: self.trait_shape_indicies,

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

			initial_yield_targets_starting_index,
			initial_yield_targets_overall_len: self.yield_targets.overall_len(),
			yield_targets: self.yield_targets,
			current_yield_target_index: None,

			initial_local_function_shape_indicies_len: self.local_function_shape_indicies.len(),
			local_function_shape_indicies: self.local_function_shape_indicies,

			function_initial_symbols_length,
			symbols_scope: self.symbols_scope.child_scope(),

			next_loop_index: 0,
			current_loop_index: None,

			can_is_bind: false,
			expected_type: None,

			return_type: Some(return_type),
			generic_parameters,
			method_base_index: None,
			is_extension_method: false,
		}
	}

	pub fn message(&mut self, message: Message) {
		self.messages.message(message);
	}

	pub fn push_symbol(&mut self, symbol: Symbol<'a>) {
		self.symbols_scope
			.symbols
			.push_symbol(self.messages, self.function_initial_symbols_length, symbol);
	}

	pub fn push_readable(
		&mut self,
		name: tree::Node<&'a str>,
		type_id: TypeId,
		kind: ReadableKind,
		used: bool,
		is_pointer_access_mutable: bool,
	) -> usize {
		let readable_index = self.readables.push(name.item, type_id, kind, is_pointer_access_mutable);

		let span = Some(name.span);
		let name = name.item;
		let kind = match kind {
			ReadableKind::Let => SymbolKind::Let { readable_index },
			ReadableKind::Mut => SymbolKind::Mut { readable_index },
		};

		if name != "_" {
			self.push_symbol(Symbol { name, kind, span, used, imported: false });
		}

		readable_index
	}

	pub fn lookup_symbol_by_name(&mut self, name: Node<&'a str>) -> Option<Symbol<'a>> {
		self.symbols_scope.symbols.lookup_symbol_by_name(
			self.messages,
			self.root_layers,
			self.type_store,
			self.function_initial_symbols_length,
			name,
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
			self.function_initial_symbols_length,
			self.generic_parameters,
			parsed_type,
		)
	}

	pub fn check_is_external_access(&self, shape_index: usize) -> bool {
		let type_allowed_access = if let Some(method_base_index) = self.method_base_index {
			method_base_index == shape_index || {
				let lock = self.type_store.user_types.read()[shape_index].clone();
				let user_type = lock.read();
				match &user_type.kind {
					UserTypeKind::Struct { shape } => {
						// Check if the method base is the enum or union that the struct is a variant of
						shape.parent_kind != StructParentKind::None && shape.parent_shape_index == method_base_index
					}

					UserTypeKind::Enum { shape } => {
						// Check if the method base is a variant of the enum
						shape
							.variant_shapes
							.iter()
							.find(|s| s.struct_shape_index == method_base_index)
							.is_some()
					}

					UserTypeKind::Union { shape } => {
						// Check if the method base is a variant of the union
						shape
							.variant_shapes
							.iter()
							.find(|s| s.struct_shape_index == method_base_index)
							.is_some()
					}
				}
			}
		} else {
			false
		};

		let internal_access = type_allowed_access && !self.is_extension_method;
		!internal_access
	}

	pub fn local_function_shape_index(&self, index: usize) -> usize {
		self.local_function_shape_indicies[self.initial_local_function_shape_indicies_len + index]
	}

	pub fn type_name(&mut self, type_id: TypeId) -> String {
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
	when_context: &WhenContext,
	root_messages: &mut RootMessages<'a>,
	lang_items: &RwLock<LangItems>,
	root_layers: &RootLayers<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	externs: &RwLock<Externs<'a>>,
	statics: &RwLock<Statics<'a>>,
	parsed_files: &'a [tree::File<'a>],
) {
	let mut trait_shape_indicies = Vec::with_capacity(parsed_files.len());
	for _ in 0..parsed_files.len() {
		trait_shape_indicies.push(RwLock::new(Vec::new()));
	}

	let mut type_shape_indicies = Vec::with_capacity(parsed_files.len());
	for _ in 0..parsed_files.len() {
		type_shape_indicies.push(RwLock::new(Vec::new()));
	}

	let mut local_function_shape_indicies = Vec::new();
	for _ in 0..parsed_files.len() {
		local_function_shape_indicies.push(RwLock::new(Vec::new()));
	}

	let root_messages = RwLock::new(root_messages);

	let parsed_files_iter = RwLock::new(parsed_files.iter());

	let thread_count = if cli_arguments.parallel_validator { 6 } else { 1 };
	let finish_step_barrier = std::sync::Barrier::new(thread_count);
	let replenish_iter_barrier = std::sync::Barrier::new(thread_count);
	let wait_on_barriers_and_replenish_iter = || {
		if finish_step_barrier.wait().is_leader() {
			*parsed_files_iter.write() = parsed_files.iter();
		}

		replenish_iter_barrier.wait();
	};

	let constants = RwLock::new(Vec::new());

	std::thread::scope(|scope| {
		for _ in 0..thread_count {
			scope.spawn(|| {
				set_thread_name!("validator thread");

				let mut type_store = type_store.clone();
				let herd_member = herd.get();
				let mut function_generic_usages = Vec::new();

				create_root_traits(
					when_context,
					&root_messages,
					root_layers,
					&mut type_store,
					&function_store,
					&mut function_generic_usages,
					&parsed_files_iter,
					&trait_shape_indicies,
				);
				function_generic_usages.clear();

				wait_on_barriers_and_replenish_iter();

				resolve_root_trait_imports(
					cli_arguments,
					&herd_member,
					when_context,
					&root_messages,
					root_layers,
					&parsed_files_iter,
				);

				wait_on_barriers_and_replenish_iter();

				create_root_types(
					when_context,
					&root_messages,
					root_layers,
					lang_items,
					&mut type_store,
					function_store,
					&parsed_files_iter,
					&type_shape_indicies,
				);

				wait_on_barriers_and_replenish_iter();

				resolve_root_type_imports(
					cli_arguments,
					&herd_member,
					when_context,
					&root_messages,
					root_layers,
					&parsed_files_iter,
				);

				wait_on_barriers_and_replenish_iter();

				fill_root_traits(
					when_context,
					&root_messages,
					root_layers,
					lang_items,
					&mut type_store,
					function_store,
					&parsed_files_iter,
					&trait_shape_indicies,
				);

				wait_on_barriers_and_replenish_iter();

				let mut readables = Readables::new();
				create_root_functions(
					when_context,
					&root_messages,
					lang_items,
					root_layers,
					&mut type_store,
					function_store,
					&mut function_generic_usages,
					&externs,
					&mut readables,
					&parsed_files_iter,
					&local_function_shape_indicies,
				);
				function_generic_usages.clear();

				wait_on_barriers_and_replenish_iter();

				fill_root_types(
					when_context,
					&root_messages,
					root_layers,
					lang_items,
					&mut type_store,
					function_store,
					&parsed_files_iter,
					&type_shape_indicies,
				);

				wait_on_barriers_and_replenish_iter();

				let mut yield_targets = YieldTargets::new();
				validate_root_consts(
					cli_arguments,
					&herd_member,
					when_context,
					&root_messages,
					lang_items,
					root_layers,
					&mut type_store,
					function_store,
					&mut function_generic_usages,
					&externs,
					&constants,
					statics,
					&mut readables,
					&mut yield_targets,
					parsed_files,
					&parsed_files_iter,
					&type_shape_indicies,
					&trait_shape_indicies,
				);

				wait_on_barriers_and_replenish_iter();

				validate_root_statics(
					cli_arguments,
					&herd_member,
					when_context,
					&root_messages,
					lang_items,
					root_layers,
					&mut type_store,
					function_store,
					&mut function_generic_usages,
					&externs,
					&constants,
					statics,
					&mut readables,
					&mut yield_targets,
					parsed_files,
					&parsed_files_iter,
					&type_shape_indicies,
					&trait_shape_indicies,
				);
				assert_eq!(function_generic_usages.len(), 0);

				wait_on_barriers_and_replenish_iter();

				deep_pass(
					cli_arguments,
					&herd_member,
					when_context,
					&root_messages,
					lang_items,
					root_layers,
					&mut type_store,
					function_store,
					&mut function_generic_usages,
					&externs,
					&constants,
					statics,
					&mut readables,
					&mut yield_targets,
					parsed_files,
					&parsed_files_iter,
					&type_shape_indicies,
					&trait_shape_indicies,
					&local_function_shape_indicies,
				);

				if !when_context.in_compiler_test && !cfg!(feature = "measure-lock-contention") {
					std::mem::forget(type_store);
				}
			});
		}
	});

	if let Some(main) = *function_store.main.read() {
		let shapes = function_store.shapes.read();
		let shape = shapes[main.function_shape_index].as_ref().unwrap().read();
		root_messages.write().mark_main_found(shape.name.span);
	}
}

fn deep_pass<'a>(
	cli_arguments: &'a CliArguments,
	herd_member: &bumpalo_herd::Member<'a>,
	when_context: &WhenContext,
	root_messages: &RwLock<&mut RootMessages<'a>>,
	lang_items: &RwLock<LangItems>,
	root_layers: &RootLayers<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	function_generic_usages: &mut Vec<GenericUsage>,
	externs: &RwLock<Externs<'a>>,
	constants: &RwLock<Vec<ConstantValue<'a>>>,
	statics: &RwLock<Statics<'a>>,
	readables: &mut Readables<'a>,
	yield_targets: &mut YieldTargets,
	parsed_files: &'a [tree::File<'a>],
	parsed_files_iter: &RwLock<std::slice::Iter<'a, tree::File<'a>>>,
	type_shape_indicies: &[RwLock<Vec<usize>>],
	trait_shape_indicies: &[RwLock<Vec<usize>>],
	local_function_shape_indicies: &[RwLock<Vec<usize>>],
) {
	loop {
		let mut guard = parsed_files_iter.write();
		let Some(parsed_file) = guard.next() else {
			return;
		};
		drop(guard);

		let file_index = parsed_file.source_file.index as usize;
		let mut type_shape_indicies = type_shape_indicies[file_index].write();
		let mut trait_shape_indicies = trait_shape_indicies[file_index].write();
		let mut local_function_shape_indicies = local_function_shape_indicies[file_index].write();

		let module_path = parsed_file.module_path;

		let layer = root_layers.lookup_module_path(module_path);
		let mut symbols = layer.read().symbols.clone();

		readables.starting_index = 0;
		readables.readables.clear();

		yield_targets.starting_index = 0;
		yield_targets.targets.clear();

		let mut next_scope_index = 1;
		let blank_generic_parameters = GenericParameters::new_from_explicit(Vec::new());
		let mut messages = Messages::new(parsed_file.module_path);
		let context = Context {
			cli_arguments,
			herd_member: &herd_member,
			file_index: parsed_file.source_file.index,
			module_path,
			parsed_files,
			when_context,
			next_scope_index: &mut next_scope_index,
			scope_index: 0,
			messages: &mut messages,
			type_shape_indicies: &mut type_shape_indicies,
			trait_shape_indicies: &mut trait_shape_indicies,
			type_store,
			function_store,
			function_generic_usages,
			root_layers,
			lang_items,
			externs,
			constants: &constants,
			statics,
			initial_readables_starting_index: 0,
			initial_readables_overall_len: 0,
			readables,
			initial_yield_targets_starting_index: 0,
			initial_yield_targets_overall_len: 0,
			yield_targets,
			current_yield_target_index: None,
			initial_local_function_shape_indicies_len: 0,
			local_function_shape_indicies: &mut local_function_shape_indicies,
			function_initial_symbols_length: symbols.symbols.len(),
			symbols_scope: symbols.child_scope(),
			next_loop_index: 0,
			current_loop_index: None,
			can_is_bind: false,
			expected_type: None,
			return_type: None,
			generic_parameters: &blank_generic_parameters,
			method_base_index: None,
			is_extension_method: false,
		};

		validate_block(context, &parsed_file.block, true);

		function_generic_usages.clear();
		layer.write().symbols = symbols;

		if messages.any_messages() {
			root_messages.write().add_messages_if_any(messages);
		}
	}
}

fn create_root_traits<'a>(
	when_context: &WhenContext,
	root_messages: &RwLock<&mut RootMessages<'a>>,
	root_layers: &RootLayers<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	generic_usages: &mut Vec<GenericUsage>,
	parsed_files: &RwLock<std::slice::Iter<tree::File<'a>>>,
	trait_shape_indicies: &[RwLock<Vec<usize>>],
) {
	loop {
		let mut guard = parsed_files.write();
		let Some(parsed_file) = guard.next() else {
			return;
		};
		drop(guard);

		let file_index = parsed_file.source_file.index;
		let mut trait_shape_indicies = trait_shape_indicies[file_index as usize].write();
		let layer = root_layers.create_module_path(parsed_file.module_path);
		let mut symbols = layer.read().symbols.clone();

		let importable_traits_start = symbols.symbols.len();
		assert_eq!(importable_traits_start, 0);

		let mut messages = Messages::new(parsed_file.module_path);

		create_block_traits(
			when_context,
			&mut messages,
			root_layers,
			type_store,
			function_store,
			&parsed_file.module_path,
			generic_usages,
			&mut symbols,
			0,
			&GenericParameters::new_from_explicit(Vec::new()),
			&parsed_file.block,
			true,
			&mut trait_shape_indicies,
		);

		let importable_traits_end = symbols.symbols.len();
		let mut layer_guard = layer.write();
		layer_guard.importable_traits_range = importable_traits_start..importable_traits_end;
		layer_guard.symbols = symbols;
		drop(layer_guard);

		if messages.any_messages() {
			root_messages.write().add_messages_if_any(messages);
		}
	}
}

fn create_root_types<'a>(
	when_context: &WhenContext,
	root_messages: &RwLock<&mut RootMessages<'a>>,
	root_layers: &RootLayers<'a>,
	lang_items: &RwLock<LangItems>,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	parsed_files: &RwLock<std::slice::Iter<tree::File<'a>>>,
	type_shape_indicies: &[RwLock<Vec<usize>>],
) {
	loop {
		let mut guard = parsed_files.write();
		let Some(parsed_file) = guard.next() else {
			return;
		};
		drop(guard);

		let file_index = parsed_file.source_file.index;
		let mut type_shape_indicies = type_shape_indicies[file_index as usize].write();
		let layer = root_layers.create_module_path(parsed_file.module_path);
		let mut symbols = layer.read().symbols.clone();

		let blank_generic_parameters = GenericParameters::new_from_explicit(Vec::new());
		let block = &parsed_file.block;
		let scope_id = ScopeId { file_index, scope_index: 0 };

		let importable_types_start = symbols.symbols.len();
		let mut messages = Messages::new(parsed_file.module_path);

		create_block_types(
			when_context,
			&mut messages,
			lang_items,
			root_layers,
			type_store,
			function_store,
			parsed_file.module_path,
			&mut symbols,
			&mut Vec::new(),
			0,
			&blank_generic_parameters,
			block,
			scope_id,
			&mut type_shape_indicies,
		);

		let importable_types_end = symbols.symbols.len();
		let mut layer_guard = layer.write();
		layer_guard.importable_types_range = importable_types_start..importable_types_end;
		layer_guard.symbols = symbols;
		drop(layer_guard);

		if messages.any_messages() {
			root_messages.write().add_messages_if_any(messages);
		}
	}
}

fn resolve_root_trait_imports<'a>(
	cli_arguments: &CliArguments,
	herd_member: &bumpalo_herd::Member<'a>,
	when_context: &WhenContext,
	root_messages: &RwLock<&mut RootMessages<'a>>,
	root_layers: &RootLayers<'a>,
	parsed_files: &RwLock<std::slice::Iter<tree::File<'a>>>,
) {
	loop {
		let mut guard = parsed_files.write();
		let Some(parsed_file) = guard.next() else {
			return;
		};
		drop(guard);

		let layer = root_layers.lookup_module_path(parsed_file.module_path);
		let mut symbols = layer.read().symbols.clone();

		let mut messages = Messages::new(parsed_file.module_path);
		let module_path = parsed_file.module_path;
		let block = &parsed_file.block;

		resolve_block_trait_imports(
			herd_member,
			when_context,
			&mut messages,
			root_layers,
			&mut symbols,
			module_path,
			0,
			block,
			cli_arguments.std_enabled,
		);

		layer.write().symbols = symbols;

		if messages.any_messages() {
			root_messages.write().add_messages_if_any(messages);
		}
	}
}

fn resolve_root_type_imports<'a>(
	cli_arguments: &CliArguments,
	herd_member: &bumpalo_herd::Member<'a>,
	when_context: &WhenContext,
	root_messages: &RwLock<&mut RootMessages<'a>>,
	root_layers: &RootLayers<'a>,
	parsed_files: &RwLock<std::slice::Iter<tree::File<'a>>>,
) {
	loop {
		let mut guard = parsed_files.write();
		let Some(parsed_file) = guard.next() else {
			return;
		};
		drop(guard);

		let layer = root_layers.lookup_module_path(parsed_file.module_path);
		let mut symbols = layer.read().symbols.clone();

		let mut messages = Messages::new(parsed_file.module_path);
		let module_path = parsed_file.module_path;
		let block = &parsed_file.block;

		resolve_block_type_imports(
			herd_member,
			when_context,
			&mut messages,
			root_layers,
			&mut symbols,
			module_path,
			0,
			block,
			cli_arguments.std_enabled,
		);

		layer.write().symbols = symbols;

		if messages.any_messages() {
			root_messages.write().add_messages_if_any(messages);
		}
	}
}

fn fill_root_traits<'a>(
	when_context: &WhenContext,
	root_messages: &RwLock<&mut RootMessages<'a>>,
	root_layers: &RootLayers<'a>,
	lang_items: &RwLock<LangItems>,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	parsed_files: &RwLock<std::slice::Iter<tree::File<'a>>>,
	trait_shape_indicies: &[RwLock<Vec<usize>>],
) {
	loop {
		let mut guard = parsed_files.write();
		let Some(parsed_file) = guard.next() else {
			return;
		};
		drop(guard);

		let file_index = parsed_file.source_file.index;
		let scope_id = ScopeId { file_index, scope_index: 0 };

		let layer = root_layers.lookup_module_path(parsed_file.module_path);
		let mut symbols = layer.read().symbols.clone();
		let mut trait_shape_indicies = trait_shape_indicies[parsed_file.source_file.index as usize].write();

		// TODO: This is definitely wrong
		let mut generic_usages = Vec::new();
		let mut messages = Messages::new(parsed_file.module_path);

		fill_block_traits(
			when_context,
			&mut messages,
			lang_items,
			type_store,
			function_store,
			&mut generic_usages,
			root_layers,
			&mut symbols,
			parsed_file.module_path,
			0,
			&GenericParameters::new_from_explicit(Vec::new()),
			&parsed_file.block,
			scope_id,
			&mut trait_shape_indicies.iter(),
		);
		trait_shape_indicies.clear();

		layer.write().symbols = symbols;

		if messages.any_messages() {
			root_messages.write().add_messages_if_any(messages);
		}
	}
}

fn fill_root_types<'a>(
	when_context: &WhenContext,
	root_messages: &RwLock<&mut RootMessages<'a>>,
	root_layers: &RootLayers<'a>,
	lang_items: &RwLock<LangItems>,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	parsed_files: &RwLock<std::slice::Iter<tree::File<'a>>>,
	type_shape_indicies: &[RwLock<Vec<usize>>],
) {
	loop {
		let mut guard = parsed_files.write();
		let Some(parsed_file) = guard.next() else {
			return;
		};
		drop(guard);

		let file_index = parsed_file.source_file.index;
		let scope_id = ScopeId { file_index, scope_index: 0 };

		let layer = root_layers.lookup_module_path(parsed_file.module_path);
		let mut symbols = layer.read().symbols.clone();
		let mut type_shape_indicies = type_shape_indicies[parsed_file.source_file.index as usize].write();

		// TODO: This is definitely wrong
		let mut generic_usages = Vec::new();
		let mut messages = Messages::new(parsed_file.module_path);

		fill_block_types(
			when_context,
			&mut messages,
			lang_items,
			type_store,
			function_store,
			&mut generic_usages,
			root_layers,
			&mut symbols,
			parsed_file.module_path,
			0,
			&GenericParameters::new_from_explicit(Vec::new()),
			&parsed_file.block,
			scope_id,
			&mut type_shape_indicies.iter(),
		);
		type_shape_indicies.clear();

		layer.write().symbols = symbols;

		if messages.any_messages() {
			root_messages.write().add_messages_if_any(messages);
		}
	}
}

fn create_root_functions<'a>(
	when_context: &WhenContext,
	root_messages: &RwLock<&mut RootMessages<'a>>,
	lang_items: &RwLock<LangItems>,
	root_layers: &RootLayers<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	generic_usages: &mut Vec<GenericUsage>,
	externs: &RwLock<Externs<'a>>,
	readables: &mut Readables<'a>,
	parsed_files: &RwLock<std::slice::Iter<'a, tree::File<'a>>>,
	local_function_shape_indicies: &[RwLock<Vec<usize>>],
) {
	loop {
		let mut guard = parsed_files.write();
		let Some(parsed_file) = guard.next() else {
			return;
		};
		drop(guard);
		let block = &parsed_file.block;

		let file_index = parsed_file.source_file.index;
		let scope_id = ScopeId { file_index, scope_index: 0 };

		let layer = root_layers.lookup_module_path(parsed_file.module_path);
		let mut symbols = layer.read().symbols.clone();
		let mut local_function_shape_indicies = local_function_shape_indicies[file_index as usize].write();

		let importable_functions_start = symbols.symbols.len();

		let mut messages = Messages::new(parsed_file.module_path);

		create_block_functions(
			when_context,
			&mut messages,
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
			&mut local_function_shape_indicies,
			scope_id,
		);

		let importable_functions_end = symbols.symbols.len();
		let mut layer_guard = layer.write();
		layer_guard.importable_functions_range = importable_functions_start..importable_functions_end;
		layer_guard.symbols = symbols;
		drop(layer_guard);

		if messages.any_messages() {
			root_messages.write().add_messages_if_any(messages);
		}
	}
}

fn validate_root_consts<'a>(
	cli_arguments: &'a CliArguments,
	herd_member: &bumpalo_herd::Member<'a>,
	when_context: &WhenContext,
	root_messages: &RwLock<&mut RootMessages<'a>>,
	lang_items: &RwLock<LangItems>,
	root_layers: &RootLayers<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	function_generic_usages: &mut Vec<GenericUsage>,
	externs: &RwLock<Externs<'a>>,
	constants: &RwLock<Vec<ConstantValue<'a>>>,
	statics: &RwLock<Statics<'a>>,
	readables: &mut Readables<'a>,
	yield_targets: &mut YieldTargets,
	parsed_files: &'a [tree::File<'a>],
	parsed_files_iter: &RwLock<std::slice::Iter<'a, tree::File<'a>>>,
	type_shape_indicies: &[RwLock<Vec<usize>>],
	trait_shape_indicies: &[RwLock<Vec<usize>>],
) {
	loop {
		let mut guard = parsed_files_iter.write();
		let Some(parsed_file) = guard.next() else {
			return;
		};
		drop(guard);

		let file_index = parsed_file.source_file.index as usize;
		let module_path = parsed_file.module_path;
		let mut type_shape_indicies = type_shape_indicies[file_index].write();
		let mut trait_shape_indicies = trait_shape_indicies[file_index].write();

		let layer = root_layers.lookup_module_path(parsed_file.module_path);
		let mut symbols = layer.read().symbols.clone();

		readables.starting_index = 0;
		readables.readables.clear();

		yield_targets.starting_index = 0;
		yield_targets.targets.clear();

		let importable_consts_start = symbols.symbols.len();

		let mut next_scope_index = 1;
		let blank_generic_parameters = GenericParameters::new_from_explicit(Vec::new());
		let mut messages = Messages::new(parsed_file.module_path);
		let mut context = Context {
			cli_arguments,
			herd_member,
			file_index: parsed_file.source_file.index,
			module_path,
			parsed_files,
			when_context,
			next_scope_index: &mut next_scope_index,
			scope_index: 0,
			messages: &mut messages,
			type_shape_indicies: &mut type_shape_indicies,
			trait_shape_indicies: &mut trait_shape_indicies,
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
			initial_yield_targets_starting_index: 0,
			initial_yield_targets_overall_len: 0,
			yield_targets,
			current_yield_target_index: None,
			function_initial_symbols_length: symbols.symbols.len(),
			symbols_scope: symbols.child_scope(),
			next_loop_index: 0,
			current_loop_index: None,
			can_is_bind: false,
			expected_type: None,
			return_type: None,
			generic_parameters: &blank_generic_parameters,
			method_base_index: None,
			is_extension_method: false,
		};

		validate_block_consts(&mut context, &parsed_file.block);
		assert_eq!(context.readables.overall_len(), 0);
		assert_eq!(context.readables.starting_index, 0);

		let importable_consts_end = context.symbols_scope.symbols.symbols.len();

		std::mem::forget(context);
		assert_eq!(next_scope_index, 1);

		let mut layer_guard = layer.write();
		layer_guard.importable_consts_range = importable_consts_start..importable_consts_end;
		layer_guard.symbols = symbols;
		drop(layer_guard);

		if messages.any_messages() {
			root_messages.write().add_messages_if_any(messages);
		}
	}
}

fn validate_root_statics<'a>(
	cli_arguments: &'a CliArguments,
	herd_member: &bumpalo_herd::Member<'a>,
	when_context: &WhenContext,
	root_messages: &RwLock<&mut RootMessages<'a>>,
	lang_items: &RwLock<LangItems>,
	root_layers: &RootLayers<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	function_generic_usages: &mut Vec<GenericUsage>,
	externs: &RwLock<Externs<'a>>,
	constants: &RwLock<Vec<ConstantValue<'a>>>,
	statics: &RwLock<Statics<'a>>,
	readables: &mut Readables<'a>,
	yield_targets: &mut YieldTargets,
	parsed_files: &'a [tree::File<'a>],
	parsed_files_iter: &RwLock<std::slice::Iter<'a, tree::File<'a>>>,
	type_shape_indicies: &[RwLock<Vec<usize>>],
	trait_shape_indicies: &[RwLock<Vec<usize>>],
) {
	loop {
		let mut guard = parsed_files_iter.write();
		let Some(parsed_file) = guard.next() else {
			return;
		};
		drop(guard);

		let file_index = parsed_file.source_file.index as usize;
		let module_path = parsed_file.module_path;
		let mut type_shape_indicies = type_shape_indicies[file_index].write();
		let mut trait_shape_indicies = trait_shape_indicies[file_index].write();

		let layer = root_layers.lookup_module_path(parsed_file.module_path);
		let mut symbols = layer.read().symbols.clone();

		readables.starting_index = 0;
		readables.readables.clear();

		yield_targets.starting_index = 0;
		yield_targets.targets.clear();

		let importable_statics_start = symbols.symbols.len();

		let mut next_scope_index = 1;
		let blank_generic_parameters = GenericParameters::new_from_explicit(Vec::new());
		let mut messages = Messages::new(parsed_file.module_path);
		let mut context = Context {
			cli_arguments,
			herd_member,
			file_index: parsed_file.source_file.index,
			module_path,
			parsed_files,
			when_context,
			next_scope_index: &mut next_scope_index,
			scope_index: 0,
			messages: &mut messages,
			type_shape_indicies: &mut type_shape_indicies,
			trait_shape_indicies: &mut trait_shape_indicies,
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
			initial_yield_targets_starting_index: 0,
			initial_yield_targets_overall_len: 0,
			yield_targets,
			current_yield_target_index: None,
			function_initial_symbols_length: symbols.symbols.len(),
			symbols_scope: symbols.child_scope(),
			next_loop_index: 0,
			current_loop_index: None,
			can_is_bind: false,
			expected_type: None,
			return_type: None,
			generic_parameters: &blank_generic_parameters,
			method_base_index: None,
			is_extension_method: false,
		};

		validate_block_statics(&mut context, &parsed_file.block);
		assert_eq!(context.readables.overall_len(), 0);
		assert_eq!(context.readables.starting_index, 0);

		let importable_statics_end = context.symbols_scope.symbols.symbols.len();

		std::mem::forget(context);
		assert_eq!(next_scope_index, 1);

		let mut layer_guard = layer.write();
		layer_guard.importable_statics_range = importable_statics_start..importable_statics_end;
		layer_guard.symbols = symbols;
		drop(layer_guard);

		if messages.any_messages() {
			root_messages.write().add_messages_if_any(messages);
		}
	}
}

fn resolve_block_trait_imports<'a>(
	herd_member: &bumpalo_herd::Member<'a>,
	when_context: &WhenContext,
	messages: &mut Messages,
	root_layers: &RootLayers<'a>,
	symbols: &mut Symbols<'a>,
	module_path: &'a [String],
	function_initial_symbols_length: usize,
	block: &tree::Block<'a>,
	should_import_prelude: bool,
) {
	if should_import_prelude && !matches!(module_path, [a, b] if a == "fae" && b == "prelude") {
		let segments = herd_member.alloc([Node::new("fae", Span::unusable()), Node::new("prelude", Span::unusable())]);
		let path = PathSegments { segments };
		resolve_import_for_block_traits(messages, root_layers, symbols, function_initial_symbols_length, &path, None, true);
	}

	for statement in block.statements {
		let import_statement = match statement {
			tree::Statement::Import(import_statement) => import_statement,

			tree::Statement::WhenElseChain(statement) => {
				if let Some(body) = when_context.evaluate_when(messages, &statement.item) {
					resolve_block_trait_imports(
						herd_member,
						when_context,
						messages,
						root_layers,
						symbols,
						module_path,
						function_initial_symbols_length,
						&body.item,
						false,
					)
				}
				continue;
			}

			_ => continue,
		};

		let path = &import_statement.item.path_segments;
		let names = Some(import_statement.item.symbol_names);
		resolve_import_for_block_traits(messages, root_layers, symbols, function_initial_symbols_length, path, names, false);
	}
}

fn resolve_block_type_imports<'a>(
	herd_member: &bumpalo_herd::Member<'a>,
	when_context: &WhenContext,
	messages: &mut Messages,
	root_layers: &RootLayers<'a>,
	symbols: &mut Symbols<'a>,
	module_path: &'a [String],
	function_initial_symbols_length: usize,
	block: &tree::Block<'a>,
	should_import_prelude: bool,
) {
	if should_import_prelude && !matches!(module_path, [a, b] if a == "fae" && b == "prelude") {
		let segments = herd_member.alloc([Node::new("fae", Span::unusable()), Node::new("prelude", Span::unusable())]);
		let path = PathSegments { segments };
		resolve_import_for_block_types(messages, root_layers, symbols, function_initial_symbols_length, &path, None, true);
	}

	for statement in block.statements {
		let import_statement = match statement {
			tree::Statement::Import(import_statement) => import_statement,

			tree::Statement::WhenElseChain(statement) => {
				if let Some(body) = when_context.evaluate_when(messages, &statement.item) {
					resolve_block_type_imports(
						herd_member,
						when_context,
						messages,
						root_layers,
						symbols,
						module_path,
						function_initial_symbols_length,
						&body.item,
						false,
					)
				}
				continue;
			}

			_ => continue,
		};

		let path = &import_statement.item.path_segments;
		let names = Some(import_statement.item.symbol_names);
		resolve_import_for_block_types(messages, root_layers, symbols, function_initial_symbols_length, path, names, false);
	}
}

fn resolve_import_for_block_traits<'a>(
	messages: &mut Messages,
	root_layers: &RootLayers<'a>,
	symbols: &mut Symbols<'a>,
	function_initial_symbols_length: usize,
	path: &PathSegments<'a>,
	names: Option<&[Node<&'a str>]>,
	is_prelude: bool,
) {
	let Some(layer) = root_layers.layer_for_path(None, path) else {
		return;
	};

	let layer_guard = layer.read();
	if layer_guard.symbols.symbols.is_empty() {
		return;
	}
	let importable_traits_range = layer_guard.importable_traits_range.clone();
	let source_symbols = layer_guard.symbols.clone();
	drop(layer_guard);

	if let Some(names) = names {
		let importable_traits = &source_symbols.symbols[importable_traits_range];
		for name in names {
			if let Some(importing) = importable_traits.iter().find(|i| i.name == name.item) {
				symbols.push_imported_symbol(
					messages,
					function_initial_symbols_length,
					importing.clone(),
					Some(name.span),
					is_prelude,
				);
			}
		}
	} else {
		for importing in &source_symbols.symbols[importable_traits_range] {
			symbols.push_imported_symbol(messages, function_initial_symbols_length, importing.clone(), None, is_prelude);
		}
	}
}

fn resolve_import_for_block_types<'a>(
	messages: &mut Messages,
	root_layers: &RootLayers<'a>,
	symbols: &mut Symbols<'a>,
	function_initial_symbols_length: usize,
	path: &PathSegments<'a>,
	names: Option<&[Node<&'a str>]>,
	is_prelude: bool,
) {
	let Some(layer) = root_layers.layer_for_path(None, path) else {
		return;
	};

	let layer_guard = layer.read();
	if layer_guard.symbols.symbols.is_empty() {
		return;
	}
	let importable_types_range = layer_guard.importable_types_range.clone();
	let source_symbols = layer_guard.symbols.clone();
	drop(layer_guard);

	if let Some(names) = names {
		let importable_types = &source_symbols.symbols[importable_types_range];
		for name in names {
			if let Some(importing) = importable_types.iter().find(|i| i.name == name.item) {
				symbols.push_imported_symbol(
					messages,
					function_initial_symbols_length,
					importing.clone(),
					Some(name.span),
					is_prelude,
				);
			}
		}
	} else {
		for importing in &source_symbols.symbols[importable_types_range] {
			symbols.push_imported_symbol(messages, function_initial_symbols_length, importing.clone(), None, is_prelude);
		}
	}
}

fn resolve_block_non_range_imports<'a>(
	herd_member: &bumpalo_herd::Member<'a>,
	messages: &mut Messages,
	root_layers: &RootLayers<'a>,
	symbols: &mut Symbols<'a>,
	module_path: &'a [String],
	function_initial_symbols_length: usize,
	block: &tree::Block<'a>,
	should_import_prelude: bool,
) {
	if should_import_prelude && !matches!(module_path, [a, b] if a == "fae" && b == "prelude") {
		let segments = herd_member.alloc([Node::new("fae", Span::unusable()), Node::new("prelude", Span::unusable())]);
		let path = PathSegments { segments };
		resolve_import_for_block_non_range(messages, root_layers, symbols, function_initial_symbols_length, &path, None, true);
	}

	for statement in block.statements {
		let import_statement = match statement {
			tree::Statement::Import(import_statement) => import_statement,
			_ => continue,
		};

		let path = &import_statement.item.path_segments;
		let names = Some(import_statement.item.symbol_names);
		resolve_import_for_block_non_range(messages, root_layers, symbols, function_initial_symbols_length, path, names, false);
	}
}

fn resolve_import_for_block_non_range<'a>(
	messages: &mut Messages,
	root_layers: &RootLayers<'a>,
	symbols: &mut Symbols<'a>,
	function_initial_symbols_length: usize,
	path: &PathSegments<'a>,
	names: Option<&[Node<&'a str>]>,
	is_prelude: bool,
) {
	let Some(layer) = root_layers.layer_for_path(Some(messages), path) else {
		return;
	};

	let layer_guard = layer.read();
	if layer_guard.symbols.symbols.is_empty() {
		let Some(names) = names else {
			let error = error!("Module does not contain importable symbols");
			messages.message(error.span(path.segments.last().unwrap().span));
			return;
		};

		for name in names {
			if let Some(layer) = layer_guard.children.get(name.item) {
				let span = Some(name.span);
				let name = name.item;
				let kind = SymbolKind::Module { layer: layer.clone() };
				let importing = Symbol { name, kind, span, used: false, imported: true };
				symbols.push_imported_symbol(messages, function_initial_symbols_length, importing, span, is_prelude);
			} else {
				let error = error!("No importable module `{}`", name.item);
				messages.message(error.span(name.span));
			}
		}

		return;
	}

	let importable_traits_range = layer_guard.importable_traits_range.clone();
	let importable_types_range = layer_guard.importable_types_range.clone();
	let importable_functions_range = layer_guard.importable_functions_range.clone();
	let importable_consts_range = layer_guard.importable_consts_range.clone();
	let importable_statics_range = layer_guard.importable_statics_range.clone();
	let source_symbols = layer_guard.symbols.clone();

	if let Some(names) = names {
		let importable_traits = &source_symbols.symbols[importable_traits_range];
		let importable_types = &source_symbols.symbols[importable_types_range];
		let importable_functions = &source_symbols.symbols[importable_functions_range];
		let importable_consts = &source_symbols.symbols[importable_consts_range];
		let importable_statics = &source_symbols.symbols[importable_statics_range];

		for name in names {
			if let Some(importing) = importable_functions.iter().find(|i| i.name == name.item) {
				symbols.push_imported_symbol(
					messages,
					function_initial_symbols_length,
					importing.clone(),
					Some(name.span),
					is_prelude,
				);
			} else if let Some(importing) = importable_consts.iter().find(|i| i.name == name.item) {
				symbols.push_imported_symbol(
					messages,
					function_initial_symbols_length,
					importing.clone(),
					Some(name.span),
					is_prelude,
				);
			} else if let Some(importing) = importable_statics.iter().find(|i| i.name == name.item) {
				symbols.push_imported_symbol(
					messages,
					function_initial_symbols_length,
					importing.clone(),
					Some(name.span),
					is_prelude,
				);
			} else if importable_traits.iter().find(|i| i.name == name.item).is_some() {
			} else if importable_types.iter().find(|i| i.name == name.item).is_some() {
			} else if let Some(layer) = layer_guard.children.get(name.item) {
				let span = Some(name.span);
				let name = name.item;
				let kind = SymbolKind::Module { layer: layer.clone() };
				let importing = Symbol { name, kind, span, used: false, imported: true };
				symbols.push_imported_symbol(messages, function_initial_symbols_length, importing, span, is_prelude);
			} else {
				let error = error!("Cannot find symbol `{}` to import", name.item);
				messages.message(error.span(name.span));
			}
		}
	} else {
		// TODO: Add asterisk syntax for importing all items in a scope

		for importing in &source_symbols.symbols[importable_functions_range] {
			symbols.push_imported_symbol(messages, function_initial_symbols_length, importing.clone(), None, is_prelude);
		}

		for importing in &source_symbols.symbols[importable_consts_range] {
			symbols.push_imported_symbol(messages, function_initial_symbols_length, importing.clone(), None, is_prelude);
		}

		for importing in &source_symbols.symbols[importable_statics_range] {
			symbols.push_imported_symbol(messages, function_initial_symbols_length, importing.clone(), None, is_prelude);
		}
	}
}

fn create_block_traits<'a>(
	when_context: &WhenContext,
	messages: &mut Messages<'a>,
	root_layers: &RootLayers<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	module_path: &'a [String],
	generic_usages: &mut Vec<GenericUsage>,
	symbols: &mut Symbols<'a>,
	function_initial_symbols_length: usize,
	enclosing_generic_parameters: &GenericParameters<'a>,
	block: &tree::Block<'a>,
	is_root: bool,
	trait_shape_indicies: &mut Vec<usize>,
) {
	for statement in block.statements {
		if is_root {
			match statement {
				tree::Statement::Expression(..)
				| tree::Statement::Block(..)
				| tree::Statement::IfElseChain(..)
				| tree::Statement::Match(..)
				| tree::Statement::While(..)
				| tree::Statement::For(..)
				| tree::Statement::Binding(..)
				| tree::Statement::Defer(..)
				| tree::Statement::Break(..)
				| tree::Statement::Continue(..)
				| tree::Statement::Yield(..)
				| tree::Statement::Return(..) => {
					let error = error!("{} is not allowed in a root scope", statement.name_and_article());
					messages.message(error.span(statement.span()));
					continue;
				}

				tree::Statement::Import(..)
				| tree::Statement::WhenElseChain(..)
				| tree::Statement::Struct(..)
				| tree::Statement::Enum(..)
				| tree::Statement::Union(..)
				| tree::Statement::Trait(..)
				| tree::Statement::Function(..)
				| tree::Statement::Const(..)
				| tree::Statement::Static(..) => {}
			}
		}

		if let tree::Statement::Trait(statement) = statement {
			let trait_shape_index = create_block_trait(
				messages,
				type_store,
				symbols,
				function_initial_symbols_length,
				enclosing_generic_parameters,
				statement,
			);
			trait_shape_indicies.push(trait_shape_index);
		} else if let tree::Statement::WhenElseChain(statement) = statement {
			if let Some(body) = when_context.evaluate_when(messages, &statement.item) {
				create_block_traits(
					when_context,
					messages,
					root_layers,
					type_store,
					function_store,
					module_path,
					generic_usages,
					symbols,
					function_initial_symbols_length,
					enclosing_generic_parameters,
					&body.item,
					is_root,
					trait_shape_indicies,
				);
			};
		}
	}
}

fn create_block_types<'a>(
	when_context: &WhenContext,
	messages: &mut Messages<'a>,
	lang_items: &RwLock<LangItems>,
	root_layers: &RootLayers<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	module_path: &'a [String],
	symbols: &mut Symbols<'a>,
	generic_usages: &mut Vec<GenericUsage>,
	function_initial_symbols_length: usize,
	enclosing_generic_parameters: &GenericParameters<'a>,
	block: &tree::Block<'a>,
	scope_id: ScopeId,
	type_shape_indicies: &mut Vec<usize>,
) {
	for statement in block.statements {
		match statement {
			tree::Statement::Struct(statement) => {
				let shape_index = create_block_struct(
					messages,
					lang_items,
					root_layers,
					type_store,
					function_store,
					module_path,
					symbols,
					generic_usages,
					function_initial_symbols_length,
					enclosing_generic_parameters,
					scope_id,
					statement,
				);
				type_shape_indicies.push(shape_index);
			}

			tree::Statement::Enum(statement) => {
				let shape_index = create_block_enum(
					messages,
					lang_items,
					root_layers,
					type_store,
					function_store,
					module_path,
					symbols,
					generic_usages,
					function_initial_symbols_length,
					enclosing_generic_parameters,
					scope_id,
					statement,
				);
				type_shape_indicies.push(shape_index);
			}

			tree::Statement::Union(statement) => {
				let shape_index = create_block_union(
					messages,
					root_layers,
					type_store,
					function_store,
					module_path,
					symbols,
					generic_usages,
					function_initial_symbols_length,
					enclosing_generic_parameters,
					scope_id,
					statement,
				);
				type_shape_indicies.push(shape_index);
			}

			tree::Statement::WhenElseChain(statement) => {
				if let Some(body) = when_context.evaluate_when(messages, &statement.item) {
					create_block_types(
						when_context,
						messages,
						lang_items,
						root_layers,
						type_store,
						function_store,
						module_path,
						symbols,
						generic_usages,
						function_initial_symbols_length,
						enclosing_generic_parameters,
						&body.item,
						scope_id,
						type_shape_indicies,
					);
				};
			}

			_ => (),
		}
	}
}

fn create_block_struct<'a>(
	messages: &mut Messages<'a>,
	lang_items: &RwLock<LangItems>,
	root_layers: &RootLayers<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	module_path: &'a [String],
	symbols: &mut Symbols<'a>,
	generic_usages: &mut Vec<GenericUsage>,
	function_initial_symbols_length: usize,
	enclosing_generic_parameters: &GenericParameters<'a>,
	scope_id: ScopeId,
	statement: &tree::Struct<'a>,
) -> usize {
	// Start off with no fields, they will be added during the next pre-pass
	// so that all types exist in order to populate field types

	let capacity = statement.generics.len() + enclosing_generic_parameters.parameters().len();
	let mut explicit_generics = Vec::with_capacity(capacity);

	let user_types = type_store.user_types.clone();
	let mut user_types = user_types.write(); // This write lock is quite unfortunate

	let shape_index = user_types.len();
	for (generic_index, generic) in statement.generics.iter().enumerate() {
		let (trait_ids, generic_constraints) = type_store.lookup_constraints(
			messages,
			root_layers,
			function_store,
			module_path,
			symbols,
			generic_usages,
			function_initial_symbols_length,
			generic.constraints,
			enclosing_generic_parameters,
		);

		let generic_type_id = type_store.register_user_type_generic(shape_index, generic_index, &trait_ids);
		explicit_generics.push(GenericParameter {
			name: generic.name,
			constraint_trait_ids: trait_ids,
			generic_constraints,
			generic_type_id,
		});
	}

	let explicit_generics_len = explicit_generics.len();
	let mut generic_parameters = GenericParameters::new_from_explicit(explicit_generics);
	for (index, parent_parameter) in enclosing_generic_parameters.parameters().iter().enumerate() {
		let generic_index = explicit_generics_len + index;
		let constraint_trait_ids = parent_parameter.constraint_trait_ids.clone();
		let generic_constraints = parent_parameter.generic_constraints.clone();
		let generic_type_id = type_store.register_user_type_generic(shape_index, generic_index, &constraint_trait_ids);
		generic_parameters.push_implicit(GenericParameter {
			name: parent_parameter.name,
			constraint_trait_ids,
			generic_constraints,
			generic_type_id,
		});
	}

	let name = statement.name.item;
	let shape = StructShape::new(statement.opaque, StructParentKind::None, 0, None, false);
	let kind = UserTypeKind::Struct { shape };
	let span = statement.name.span;
	let RegisterTypeResult { shape_index, methods_index } = TypeStore::register_type(
		&mut user_types,
		&type_store.method_collections,
		&type_store.implementations,
		name,
		generic_parameters,
		kind,
		scope_id,
		span,
	);
	drop(user_types);

	if let Some(lang_attribute) = statement.lang_attribute {
		let type_id = type_store
			.get_or_add_shape_specialization(
				messages,
				function_store,
				&[],
				&mut Vec::new(),
				&GenericParameters::new_from_explicit(Vec::new()),
				shape_index,
				None,
				Ref::new(TypeArguments::new_from_explicit(Vec::new())),
			)
			.unwrap();
		let lang_name = lang_attribute.item.name;
		lang_items.write().register_lang_type(messages, type_id, lang_name, span);
	}

	let kind = SymbolKind::UserType { shape_index, methods_index };
	let symbol = Symbol { name, kind, span: Some(span), used: true, imported: false };
	symbols.push_symbol(messages, function_initial_symbols_length, symbol);
	shape_index
}

fn create_block_enum<'a>(
	messages: &mut Messages<'a>,
	lang_items: &RwLock<LangItems>,
	root_layers: &RootLayers<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	module_path: &'a [String],
	symbols: &mut Symbols<'a>,
	generic_usages: &mut Vec<GenericUsage>,
	function_initial_symbols_length: usize,
	enclosing_generic_parameters: &GenericParameters<'a>,
	scope_id: ScopeId,
	statement: &tree::Enum<'a>,
) -> usize {
	let capacity = statement.generics.len() + enclosing_generic_parameters.parameters().len();
	let mut explicit_generics = Vec::with_capacity(capacity);

	let user_types = type_store.user_types.clone();
	let mut user_types = user_types.write(); // This write lock is quite unfortunate

	let enum_shape_index = user_types.len() + statement.variants.len();
	for (generic_index, generic) in statement.generics.iter().enumerate() {
		let (constraint_trait_ids, generic_constraints) = type_store.lookup_constraints(
			messages,
			root_layers,
			function_store,
			module_path,
			symbols,
			generic_usages,
			function_initial_symbols_length,
			generic.constraints,
			enclosing_generic_parameters,
		);
		let generic_type_id = type_store.register_user_type_generic(enum_shape_index, generic_index, &constraint_trait_ids);
		explicit_generics.push(GenericParameter {
			name: generic.name,
			constraint_trait_ids,
			generic_constraints,
			generic_type_id,
		});
	}

	let explicit_generics_len = explicit_generics.len();
	let mut generic_parameters = GenericParameters::new_from_explicit(explicit_generics);
	for (index, parent_parameter) in enclosing_generic_parameters.parameters().iter().enumerate() {
		let generic_index = explicit_generics_len + index;
		let constraint_trait_ids = parent_parameter.constraint_trait_ids.clone();
		let generic_constraints = parent_parameter.generic_constraints.clone();
		let generic_type_id = type_store.register_user_type_generic(enum_shape_index, generic_index, &constraint_trait_ids);
		generic_parameters.push_implicit(GenericParameter {
			name: parent_parameter.name,
			constraint_trait_ids,
			generic_constraints,
			generic_type_id,
		});
	}

	if statement.variants.len() > 256 {
		let name = statement.name.item;
		let error = error!("Enum `{name}` has more than 256 variants, this is currently unsupported and disallowed");
		messages.message(error.span(statement.name.span));
	}

	let mut variants: Vec<EnumVariantShape> = Vec::new();

	for (variant_index, variant) in statement.variants.iter().enumerate() {
		let struct_shape_index = user_types.len();

		let mut variant_explicit_generics = Vec::with_capacity(generic_parameters.explicit_len());
		for (generic_index, enum_explicit_generic) in generic_parameters.explicit_parameters().iter().enumerate() {
			let constraint_trait_ids = enum_explicit_generic.constraint_trait_ids.clone();
			let generic_constraints = enum_explicit_generic.generic_constraints.clone();
			let generic_type_id = type_store.register_user_type_generic(struct_shape_index, generic_index, &constraint_trait_ids);
			variant_explicit_generics.push(GenericParameter {
				name: enum_explicit_generic.name,
				constraint_trait_ids,
				generic_constraints,
				generic_type_id,
			});
		}

		let explicit_generics_len = variant_explicit_generics.len();
		let mut variant_generic_parameters = GenericParameters::new_from_explicit(variant_explicit_generics);
		for (index, enum_parameter) in generic_parameters.implicit_parameters().iter().enumerate() {
			let generic_index = explicit_generics_len + index;
			let constraint_trait_ids = enum_parameter.constraint_trait_ids.clone();
			let generic_constraints = enum_parameter.generic_constraints.clone();
			let generic_type_id = type_store.register_user_type_generic(struct_shape_index, generic_index, &constraint_trait_ids);
			let parameter = GenericParameter {
				name: enum_parameter.name,
				constraint_trait_ids,
				generic_constraints,
				generic_type_id,
			};
			variant_generic_parameters.push_implicit(parameter);
		}

		let (name, is_transparent) = match variant {
			tree::Variant::StructLike(struct_like) => (struct_like.name, false),
			tree::Variant::Transparent(transparent) => (transparent.name, true),
		};

		let span = name.span;
		let name = name.item;

		let shape = StructShape::new(false, StructParentKind::Enum, enum_shape_index, Some(variant_index), is_transparent);
		let kind = UserTypeKind::Struct { shape };
		let RegisterTypeResult { methods_index, .. } = TypeStore::register_type(
			&mut user_types,
			&type_store.method_collections,
			&type_store.implementations,
			name,
			variant_generic_parameters,
			kind,
			scope_id,
			span,
		);

		if let Some(existing) = variants.iter().find(|v| v.name == name) {
			let error = error!("Duplicate variant `{}` on enum `{}`", name, statement.name.item);
			let note = note!(existing.span, "Original variant here");
			messages.message(error.span(span).note(note))
		}

		let variant_shape = EnumVariantShape {
			name,
			span,
			variant_index,
			struct_shape_index,
			methods_index,
			is_transparent,
		};
		variants.push(variant_shape);
	}

	let name = statement.name.item;
	let shape = EnumShape::new(variants);
	let kind = UserTypeKind::Enum { shape };
	let span = statement.name.span;
	assert_eq!(user_types.len(), enum_shape_index);
	let RegisterTypeResult { shape_index, methods_index } = TypeStore::register_type(
		&mut user_types,
		&type_store.method_collections,
		&type_store.implementations,
		name,
		generic_parameters,
		kind,
		scope_id,
		span,
	);
	drop(user_types);

	if let Some(lang_attribute) = statement.lang_attribute {
		let type_id = type_store
			.get_or_add_shape_specialization(
				messages,
				function_store,
				&[],
				&mut Vec::new(),
				&GenericParameters::new_from_explicit(Vec::new()),
				shape_index,
				None,
				Ref::new(TypeArguments::new_from_explicit(Vec::new())),
			)
			.unwrap();
		let lang_name = lang_attribute.item.name;
		lang_items.write().register_lang_type(messages, type_id, lang_name, span);
	}

	let kind = SymbolKind::UserType { shape_index, methods_index };
	let symbol = Symbol { name, kind, span: Some(span), used: true, imported: false };
	symbols.push_symbol(messages, function_initial_symbols_length, symbol);
	shape_index
}

fn create_block_union<'a>(
	messages: &mut Messages<'a>,
	root_layers: &RootLayers<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	module_path: &'a [String],
	symbols: &mut Symbols<'a>,
	generic_usages: &mut Vec<GenericUsage>,
	function_initial_symbols_length: usize,
	enclosing_generic_parameters: &GenericParameters<'a>,
	scope_id: ScopeId,
	statement: &tree::Union<'a>,
) -> usize {
	let capacity = statement.generics.len() + enclosing_generic_parameters.parameters().len();
	let mut explicit_generics = Vec::with_capacity(capacity);

	let user_types = type_store.user_types.clone();
	let mut user_types = user_types.write(); // This write lock is quite unfortunate

	// TODO: Abstract and reuse this?
	let union_shape_index = user_types.len() + statement.variants.len();
	for (generic_index, generic) in statement.generics.iter().enumerate() {
		let (constraint_trait_ids, generic_constraints) = type_store.lookup_constraints(
			messages,
			root_layers,
			function_store,
			module_path,
			symbols,
			generic_usages,
			function_initial_symbols_length,
			generic.constraints,
			enclosing_generic_parameters,
		);
		let generic_type_id = type_store.register_user_type_generic(union_shape_index, generic_index, &constraint_trait_ids);
		explicit_generics.push(GenericParameter {
			name: generic.name,
			constraint_trait_ids,
			generic_constraints,
			generic_type_id,
		});
	}

	let explicit_generics_len = explicit_generics.len();
	let mut generic_parameters = GenericParameters::new_from_explicit(explicit_generics);
	for (index, parent_parameter) in enclosing_generic_parameters.parameters().iter().enumerate() {
		let generic_index = explicit_generics_len + index;
		let constraint_trait_ids = parent_parameter.constraint_trait_ids.clone();
		let generic_constraints = parent_parameter.generic_constraints.clone();
		let generic_type_id = type_store.register_user_type_generic(union_shape_index, generic_index, &constraint_trait_ids);
		generic_parameters.push_implicit(GenericParameter {
			name: parent_parameter.name,
			constraint_trait_ids,
			generic_constraints,
			generic_type_id,
		});
	}

	let mut variants: Vec<UnionVariantShape> = Vec::new();

	for (variant_index, variant) in statement.variants.iter().enumerate() {
		let struct_shape_index = user_types.len();

		let mut variant_explicit_generics = Vec::with_capacity(generic_parameters.explicit_len());
		for (generic_index, union_explicit_generic) in generic_parameters.explicit_parameters().iter().enumerate() {
			let constraint_trait_ids = union_explicit_generic.constraint_trait_ids.clone();
			let generic_constraints = union_explicit_generic.generic_constraints.clone();
			let generic_type_id = type_store.register_user_type_generic(struct_shape_index, generic_index, &constraint_trait_ids);
			variant_explicit_generics.push(GenericParameter {
				name: union_explicit_generic.name,
				constraint_trait_ids,
				generic_constraints,
				generic_type_id,
			});
		}

		let explicit_generics_len = variant_explicit_generics.len();
		let mut variant_generic_parameters = GenericParameters::new_from_explicit(variant_explicit_generics);
		for (index, union_parameter) in generic_parameters.implicit_parameters().iter().enumerate() {
			let generic_index = explicit_generics_len + index;
			let constraint_trait_ids = union_parameter.constraint_trait_ids.clone();
			let generic_constraints = union_parameter.generic_constraints.clone();
			let generic_type_id = type_store.register_user_type_generic(struct_shape_index, generic_index, &constraint_trait_ids);
			let parameter = GenericParameter {
				name: union_parameter.name,
				constraint_trait_ids,
				generic_constraints,
				generic_type_id,
			};
			variant_generic_parameters.push_implicit(parameter);
		}

		let (name, is_transparent) = match variant {
			tree::Variant::StructLike(struct_like) => (struct_like.name, false),
			tree::Variant::Transparent(transparent) => (transparent.name, true),
		};

		let span = name.span;
		let name = name.item;

		let shape = StructShape::new(false, StructParentKind::Union, union_shape_index, Some(variant_index), is_transparent);
		let kind = UserTypeKind::Struct { shape };
		let RegisterTypeResult { methods_index, .. } = TypeStore::register_type(
			&mut user_types,
			&type_store.method_collections,
			&type_store.implementations,
			name,
			variant_generic_parameters,
			kind,
			scope_id,
			span,
		);

		if let Some(existing) = variants.iter().find(|v| v.name == name) {
			let error = error!("Duplicate variant `{}` on union `{}`", name, statement.name.item);
			let note = note!(existing.span, "Original variant here");
			messages.message(error.span(span).note(note))
		}

		let variant_shape = UnionVariantShape {
			name,
			span,
			variant_index,
			struct_shape_index,
			methods_index,
			is_transparent,
		};
		variants.push(variant_shape);
	}

	let name = statement.name.item;
	let shape = UnionShape::new(variants);
	let kind = UserTypeKind::Union { shape };
	let span = statement.name.span;
	assert_eq!(user_types.len(), union_shape_index);
	let RegisterTypeResult { shape_index, methods_index } = TypeStore::register_type(
		&mut user_types,
		&type_store.method_collections,
		&type_store.implementations,
		name,
		generic_parameters,
		kind,
		scope_id,
		span,
	);
	drop(user_types);

	let kind = SymbolKind::UserType { shape_index, methods_index };
	let symbol = Symbol { name, kind, span: Some(span), used: true, imported: false };
	symbols.push_symbol(messages, function_initial_symbols_length, symbol);
	shape_index
}

fn fill_block_traits<'a>(
	when_context: &WhenContext,
	messages: &mut Messages<'a>,
	lang_items: &RwLock<LangItems>,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	generic_usages: &mut Vec<GenericUsage>,
	root_layers: &RootLayers<'a>,
	symbols: &mut Symbols<'a>,
	module_path: &'a [String],
	function_initial_symbols_length: usize,
	enclosing_generic_parameters: &GenericParameters<'a>,
	block: &tree::Block<'a>,
	scope_id: ScopeId,
	trait_index_iter: &mut std::slice::Iter<usize>,
) {
	for statement in block.statements {
		match statement {
			tree::Statement::Trait(statement) => {
				fill_block_trait(
					messages,
					root_layers,
					type_store,
					function_store,
					generic_usages,
					symbols,
					module_path,
					function_initial_symbols_length,
					enclosing_generic_parameters,
					statement,
					*trait_index_iter.next().unwrap(),
				);
			}

			tree::Statement::WhenElseChain(statement) => {
				if let Some(body) = when_context.evaluate_when(messages, &statement.item) {
					fill_block_traits(
						when_context,
						messages,
						lang_items,
						type_store,
						function_store,
						generic_usages,
						root_layers,
						symbols,
						module_path,
						function_initial_symbols_length,
						enclosing_generic_parameters,
						&body.item,
						scope_id,
						trait_index_iter,
					);
				}
			}

			_ => {}
		}
	}
}

fn fill_block_types<'a>(
	when_context: &WhenContext,
	messages: &mut Messages<'a>,
	lang_items: &RwLock<LangItems>,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	generic_usages: &mut Vec<GenericUsage>,
	root_layers: &RootLayers<'a>,
	symbols: &mut Symbols<'a>,
	module_path: &'a [String],
	function_initial_symbols_length: usize,
	enclosing_generic_parameters: &GenericParameters<'a>,
	block: &tree::Block<'a>,
	scope_id: ScopeId,
	shape_index_iter: &mut std::slice::Iter<usize>,
) {
	for statement in block.statements {
		match statement {
			tree::Statement::Struct(statement) => {
				fill_block_struct(
					messages,
					type_store,
					function_store,
					generic_usages,
					enclosing_generic_parameters,
					root_layers,
					symbols,
					module_path,
					function_initial_symbols_length,
					statement,
					*shape_index_iter.next().unwrap(),
				);
			}

			tree::Statement::Enum(statement) => {
				fill_block_enum(
					messages,
					type_store,
					function_store,
					generic_usages,
					enclosing_generic_parameters,
					root_layers,
					symbols,
					module_path,
					function_initial_symbols_length,
					statement,
					*shape_index_iter.next().unwrap(),
				);
			}

			tree::Statement::Union(statement) => {
				fill_block_union(
					messages,
					type_store,
					function_store,
					generic_usages,
					enclosing_generic_parameters,
					root_layers,
					symbols,
					module_path,
					function_initial_symbols_length,
					statement,
					*shape_index_iter.next().unwrap(),
				);
			}

			tree::Statement::WhenElseChain(statement) => {
				if let Some(body) = when_context.evaluate_when(messages, &statement.item) {
					fill_block_types(
						when_context,
						messages,
						lang_items,
						type_store,
						function_store,
						generic_usages,
						root_layers,
						symbols,
						module_path,
						function_initial_symbols_length,
						enclosing_generic_parameters,
						&body.item,
						scope_id,
						shape_index_iter,
					);
				}
			}

			_ => {}
		}
	}
}

fn create_block_trait<'a>(
	messages: &mut Messages<'a>,
	type_store: &mut TypeStore<'a>,
	symbols: &mut Symbols<'a>,
	function_initial_symbols_length: usize,
	enclosing_generic_parameters: &GenericParameters<'a>,
	statement: &tree::Trait<'a>,
) -> usize {
	let capacity = statement.generics.len() + enclosing_generic_parameters.parameters().len();
	let mut explicit_generics = Vec::with_capacity(capacity);

	let traits_lock = type_store.traits.clone();
	let mut traits = traits_lock.write();
	let trait_shape_index = TypeStore::pre_create_empty_trait_shape(&mut traits);
	drop(traits);

	for (generic_index, generic) in statement.generics.iter().enumerate() {
		if let Some(first) = generic.constraints.first() {
			let error = error!("TODO: constraints are not allowed on trait body generic");
			messages.message(error.span(first.span));
		}

		let constraint_trait_ids = SliceRef::new_empty();
		let generic_type_id = type_store.register_trait_generic(trait_shape_index, generic_index, false, &constraint_trait_ids);
		explicit_generics.push(GenericParameter {
			name: generic.name,
			constraint_trait_ids,
			generic_constraints: SliceRef::new_empty(),
			generic_type_id,
		});
	}

	let explicit_generics_len = explicit_generics.len();
	let mut generic_parameters = GenericParameters::new_from_explicit(explicit_generics);
	for (index, parent_parameter) in enclosing_generic_parameters.parameters().iter().enumerate() {
		let generic_index = explicit_generics_len + index;
		let constraint_trait_ids = parent_parameter.constraint_trait_ids.clone();
		let generic_constraints = parent_parameter.generic_constraints.clone();
		let generic_type_id = type_store.register_trait_generic(trait_shape_index, generic_index, false, &constraint_trait_ids);
		generic_parameters.push_implicit(GenericParameter {
			name: parent_parameter.name,
			constraint_trait_ids,
			generic_constraints,
			generic_type_id,
		});
	}

	let self_generic_index = generic_parameters.parameters().len();
	let self_generic_type_id =
		type_store.register_trait_generic(trait_shape_index, self_generic_index, true, &SliceRef::new_empty());
	generic_parameters.push_trait_self(GenericParameter {
		name: Node::new("Self", Span::unusable()), // TODO: Massive terrible hack
		constraint_trait_ids: SliceRef::new_empty(),
		generic_constraints: SliceRef::new_empty(),
		generic_type_id: self_generic_type_id,
	});

	let trait_shape = TraitShape::new(statement.name.item, generic_parameters);
	let mut traits = traits_lock.write();
	TypeStore::register_trait_shape(&mut traits, trait_shape, trait_shape_index);
	drop(traits);

	let symbol = Symbol {
		name: statement.name.item,
		kind: SymbolKind::Trait { trait_shape_index },
		span: Some(statement.name.span),
		used: false,
		imported: false,
	};
	symbols.push_symbol(messages, function_initial_symbols_length, symbol);

	trait_shape_index
}

fn fill_block_trait<'a>(
	messages: &mut Messages<'a>,
	root_layers: &RootLayers<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	generic_usages: &mut Vec<GenericUsage>,
	symbols: &mut Symbols<'a>,
	module_path: &'a [String],
	function_initial_symbols_length: usize,
	enclosing_generic_parameters: &GenericParameters<'a>,
	statement: &tree::Trait<'a>,
	trait_shape_index: usize,
) {
	let mut symbols = symbols.child_scope();

	let lock = type_store.traits.read()[trait_shape_index].clone();
	let trait_shape = lock.as_ref().unwrap().read(); // TODO: We acquire this twice in the same function, yuck

	let trait_shape_generic_parameters = trait_shape.generic_parameters.clone();
	for (generic_index, generic) in trait_shape_generic_parameters.parameters().iter().enumerate() {
		let kind = SymbolKind::TraitGeneric { trait_shape_index, generic_index };
		let symbol = Symbol {
			name: generic.name.item,
			kind,
			span: Some(generic.name.span),
			used: true,
			imported: false,
		};
		symbols.symbols.push_symbol(messages, function_initial_symbols_length, symbol);
	}

	let capacity = enclosing_generic_parameters.parameters().len() + trait_shape.generic_parameters.parameters().len();
	drop(trait_shape);

	let mut methods = Vec::new();
	for (trait_method_index, method) in statement.methods.iter().enumerate() {
		let symbols = symbols.child_scope();
		if let Some(varargs_span) = method.parameters.item.c_varargs {
			let message = error!("Trait method may not accept C varargs");
			messages.message(message.span(varargs_span));
		}

		let mut shapes = function_store.shapes.write();
		let fake_function_shape_index = shapes.len();
		shapes.push(None);
		drop(shapes);

		let explicit_generics_len = 0;
		let explicit_generics = Vec::with_capacity(capacity);
		let mut generic_parameters = GenericParameters::new_from_explicit(explicit_generics);

		for (index, parent_parameter) in enclosing_generic_parameters.parameters().iter().enumerate() {
			let generic_index = explicit_generics_len + index;
			let constraint_trait_ids = parent_parameter.constraint_trait_ids.clone();
			let generic_constraints = parent_parameter.generic_constraints.clone();
			let generic_type_id =
				type_store.register_function_generic(fake_function_shape_index, generic_index, false, 0, &constraint_trait_ids);
			generic_parameters.push_implicit(GenericParameter {
				name: parent_parameter.name,
				constraint_trait_ids,
				generic_constraints,
				generic_type_id,
			});

			let symbol = Symbol {
				name: parent_parameter.name.item,
				kind: SymbolKind::FunctionGeneric {
					function_shape_index: fake_function_shape_index,
					generic_index,
				},
				span: Some(parent_parameter.name.span),
				used: true,
				imported: false,
			};
			symbols.symbols.push_symbol(messages, function_initial_symbols_length, symbol);
		}

		let implicit_generics_len = generic_parameters.implicit_len();
		for (index, base_type_parameter) in trait_shape_generic_parameters.parameters().iter().enumerate() {
			let generic_index = explicit_generics_len + implicit_generics_len + index;
			let constraint_trait_ids = base_type_parameter.constraint_trait_ids.clone();
			let generic_constraints = base_type_parameter.generic_constraints.clone();
			let generic_type_id = type_store.register_function_generic(
				fake_function_shape_index,
				generic_index,
				index == 0,
				trait_method_index,
				&constraint_trait_ids,
			);
			generic_parameters.push_method_base(GenericParameter {
				name: base_type_parameter.name,
				constraint_trait_ids,
				generic_constraints,
				generic_type_id,
			});

			let symbol = Symbol {
				name: base_type_parameter.name.item,
				kind: SymbolKind::FunctionGeneric {
					function_shape_index: fake_function_shape_index,
					generic_index,
				},
				span: Some(base_type_parameter.name.span),
				used: true,
				imported: false,
			};
			symbols.symbols.push_symbol(messages, function_initial_symbols_length, symbol);
		}

		let mut function_store_generics = function_store.generics.write();
		assert_eq!(function_store_generics.len(), fake_function_shape_index);
		function_store_generics.push(generic_parameters.clone());
		drop(function_store_generics);

		let mut shape_parameters = Vec::new();
		shape_parameters.push(ParameterShape {
			span: Span::unusable(),
			label: None,
			type_id: type_store.any_collapse_type_id(), // TODO: Hack
			readable_index: usize::MAX,
			is_mutable: false,
		});

		let mut trait_parameters = Vec::new();
		for parameter in method.parameters.item.parameters {
			let type_id = type_store.lookup_type(
				messages,
				&function_store,
				module_path,
				generic_usages,
				root_layers,
				symbols.symbols,
				function_initial_symbols_length,
				&enclosing_generic_parameters,
				&parameter.item.parsed_type,
			);

			let type_id = match type_id {
				Some(type_id) => type_id,
				None => type_store.any_collapse_type_id(),
			};

			trait_parameters.push(TraitParameter { type_id });
			shape_parameters.push(ParameterShape {
				span: parameter.span,
				label: parameter.item.label,
				type_id,
				readable_index: usize::MAX, // TODO: Hack?
				is_mutable: false,
			});
		}

		let return_type = if let Some(parsed_type) = &method.parsed_type {
			let type_id = type_store.lookup_type(
				messages,
				function_store,
				module_path,
				generic_usages,
				root_layers,
				symbols.symbols,
				function_initial_symbols_length,
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

			Node::new(return_type, parsed_type.span)
		} else {
			Node::new(type_store.void_type_id(), method.name.span)
		};

		let shape = FunctionShape {
			name: method.name,
			module_path,
			is_main: false,
			trait_method_marker: Some(TraitMethodMarker { trait_shape_index, trait_method_index }),
			generic_parameters,
			extern_attribute: None,
			export_attribute: None,
			intrinsic_attribute: None,
			lang_attribute: None,
			method_base_index: None,
			parameters: Node::new(shape_parameters, method.parameters.span),
			c_varargs: None,
			return_type,
			block: None,
			generic_usages: SliceRef::new_empty(),
			specializations_by_type_arguments: FxHashMap::default(),
			specializations: Vec::new(),
		};

		let mut shapes = function_store.shapes.write();
		shapes[fake_function_shape_index] = Some(Ref::new(RwLock::new(shape)));
		drop(shapes);

		methods.push(TraitMethod {
			kind: method.kind,
			name: method.name,
			parameters: trait_parameters,
			return_type: return_type.item,
			fake_function_shape_index,
		});
	}

	let mut trait_shape = lock.as_ref().unwrap().write();

	let filling_lock = trait_shape.filling_lock.clone();
	let _filling_guard = filling_lock.lock();

	assert!(!trait_shape.been_filled);
	assert!(trait_shape.methods.is_empty());

	trait_shape.methods = methods;
	trait_shape.been_filled = true;

	let has_specialization = !trait_shape.specializations.is_empty();
	drop(trait_shape);

	if has_specialization {
		fill_pre_existing_trait_specializations(
			messages,
			type_store,
			function_store,
			generic_usages,
			enclosing_generic_parameters,
			module_path,
			trait_shape_index,
		);
	}
}

fn fill_pre_existing_trait_specializations<'a>(
	messages: &mut Messages<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	generic_usages: &mut Vec<GenericUsage>,
	enclosing_generic_parameters: &GenericParameters<'a>,
	module_path: &'a [String],
	trait_shape_index: usize,
) {
	// TODO: This is dumb, in order to pass in the type store the caller must close their locks on this state
	// just for us to lock them again. This is needlessly complex and expensive, fix it
	let lock = type_store.traits.read()[trait_shape_index].clone();
	let trait_shape = lock.as_ref().unwrap().read();

	let mut specializations = trait_shape.specializations.clone(); // Belch
	let methods = trait_shape.methods.clone();
	drop(trait_shape);

	for specialization in &mut specializations {
		let mut methods = methods.clone();
		for method in &mut methods {
			method.specialize_with_trait_generics(
				messages,
				type_store,
				function_store,
				module_path,
				generic_usages,
				enclosing_generic_parameters,
				trait_shape_index,
				&specialization.type_arguments,
			);
		}

		assert!(specialization.methods.is_empty());
		specialization.methods = SliceRef::from(methods);
		assert!(!specialization.been_filled);
		specialization.been_filled = true;
	}

	let mut trait_shape = lock.as_ref().unwrap().write();
	trait_shape.specializations = specializations;
}

fn fill_block_struct<'a>(
	messages: &mut Messages<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	generic_usages: &mut Vec<GenericUsage>,
	enclosing_generic_parameters: &GenericParameters<'a>,
	root_layers: &RootLayers<'a>,
	symbols: &mut Symbols<'a>,
	module_path: &'a [String],
	function_initial_symbols_length: usize,
	statement: &tree::Struct<'a>,
	shape_index: usize,
) {
	let lock = type_store.user_types.read()[shape_index].clone();
	let user_type = lock.read();
	let struct_name = user_type.name;

	let filling_lock = match &user_type.kind {
		UserTypeKind::Struct { shape } => shape.filling_lock.clone(),
		UserTypeKind::Enum { .. } => unreachable!("enum"),
		UserTypeKind::Union { .. } => unreachable!("union"),
	};
	let _filling_guard = filling_lock.lock();

	let scope = symbols.child_scope();

	for (generic_index, generic) in user_type.generic_parameters.parameters().iter().enumerate() {
		let kind = SymbolKind::UserTypeGeneric { shape_index, generic_index };
		let symbol = Symbol {
			name: generic.name.item,
			kind,
			span: Some(generic.name.span),
			used: true,
			imported: false,
		};
		scope.symbols.push_symbol(messages, function_initial_symbols_length, symbol);
	}
	drop(user_type);

	let blank_generic_parameters = GenericParameters::new_from_explicit(Vec::new());
	let mut fields: Vec<Node<FieldShape>> = Vec::with_capacity(statement.fields.len());

	for field in statement.fields {
		let field_type = match type_store.lookup_type(
			messages,
			function_store,
			module_path,
			generic_usages,
			root_layers,
			scope.symbols,
			function_initial_symbols_length,
			&blank_generic_parameters,
			&field.parsed_type,
		) {
			Some(type_id) => type_id,
			None => type_store.any_collapse_type_id(),
		};

		let span = field.name.span + field.parsed_type.span;
		if let Some(existing) = fields.iter().find(|f| f.item.name == field.name.item) {
			let error = error!("Duplicate field `{}` on struct `{}`", field.name.item, struct_name);
			let note = note!(existing.span, "Original field here");
			messages.message(error.span(span).note(note))
		}

		let field_shape = FieldShape {
			name: field.name.item,
			field_type,
			attribute: field.attribute,
			read_only: field.read_only,
		};
		let node = Node::new(field_shape, span);
		fields.push(node);
	}

	let mut user_type = lock.write();
	match &mut user_type.kind {
		UserTypeKind::Struct { shape } => {
			shape.fields = fields;
			assert!(!shape.been_filled);
			shape.been_filled = true;

			let has_specializations = !shape.specializations.is_empty();
			drop(user_type);

			if has_specializations {
				fill_pre_existing_struct_specializations(
					messages,
					type_store,
					function_store,
					generic_usages,
					enclosing_generic_parameters,
					module_path,
					shape_index,
				);
			}
		}

		UserTypeKind::Enum { .. } => unreachable!("enum"),
		UserTypeKind::Union { .. } => unreachable!("union"),
	}
}

fn fill_block_enum<'a>(
	messages: &mut Messages<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	generic_usages: &mut Vec<GenericUsage>,
	enclosing_generic_parameters: &GenericParameters<'a>,
	root_layers: &RootLayers<'a>,
	symbols: &mut Symbols<'a>,
	module_path: &'a [String],
	function_initial_symbols_length: usize,
	statement: &tree::Enum<'a>,
	enum_shape_index: usize,
) {
	let lock = type_store.user_types.read()[enum_shape_index].clone();
	let user_type = lock.read();
	let enum_name = user_type.name;

	let filling_lock = match &user_type.kind {
		UserTypeKind::Enum { shape } => shape.filling_lock.clone(),
		UserTypeKind::Struct { .. } => unreachable!("struct"),
		UserTypeKind::Union { .. } => unreachable!("union"),
	};
	let _filling_guard = filling_lock.lock();

	let scope = symbols.child_scope();

	for (generic_index, generic) in user_type.generic_parameters.parameters().iter().enumerate() {
		let kind = SymbolKind::UserTypeGeneric { shape_index: enum_shape_index, generic_index };
		let symbol = Symbol {
			name: generic.name.item,
			kind,
			span: Some(generic.name.span),
			used: true,
			imported: false,
		};
		scope.symbols.push_symbol(messages, function_initial_symbols_length, symbol);
	}
	drop(user_type);

	let blank_generic_parameters = GenericParameters::new_from_explicit(Vec::new());
	let mut shared_fields: Vec<Node<FieldShape>> = Vec::with_capacity(statement.shared_fields.len());

	for shared_field in statement.shared_fields {
		let field_type = match type_store.lookup_type(
			messages,
			function_store,
			module_path,
			generic_usages,
			root_layers,
			scope.symbols,
			function_initial_symbols_length,
			&blank_generic_parameters,
			&shared_field.parsed_type,
		) {
			Some(type_id) => type_id,
			None => type_store.any_collapse_type_id(),
		};

		let span = shared_field.name.span + shared_field.parsed_type.span;
		if let Some(existing) = shared_fields.iter().find(|f| f.item.name == shared_field.name.item) {
			let error = error!("Duplicate shared field `{}` on enum `{}`", shared_field.name.item, enum_name);
			let note = note!(existing.span, "Original shared field here");
			messages.message(error.span(span).note(note))
		}

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

	let user_type = lock.read();
	let shape = match &user_type.kind {
		UserTypeKind::Enum { shape } => shape,
		UserTypeKind::Struct { .. } => unreachable!("struct"),
		UserTypeKind::Union { .. } => unreachable!("union"),
	};

	// Yuck
	let mut variant_shapes = shape.variant_shapes.to_vec();
	drop(user_type);

	for variant_shape in &mut variant_shapes {
		fill_struct_like_enum_variant(
			messages,
			type_store,
			function_store,
			generic_usages,
			enclosing_generic_parameters,
			root_layers,
			scope.symbols,
			module_path,
			function_initial_symbols_length,
			&variant_shape,
			statement,
		);
	}

	let mut user_type = lock.write();
	match &mut user_type.kind {
		UserTypeKind::Enum { shape } => {
			assert!(shape.shared_fields.is_empty());
			shape.shared_fields = SliceRef::from(shared_fields);
			shape.variant_shapes = SliceRef::from(variant_shapes);
			assert!(!shape.been_filled);
			shape.been_filled = true;

			let has_specializations = !shape.specializations.is_empty();
			drop(user_type);

			if has_specializations {
				fill_pre_existing_enum_specializations(
					messages,
					type_store,
					function_store,
					generic_usages,
					enclosing_generic_parameters,
					module_path,
					enum_shape_index,
				);
			}
		}

		UserTypeKind::Struct { .. } => unreachable!("struct"),
		UserTypeKind::Union { .. } => unreachable!("union"),
	}
}

fn fill_struct_like_enum_variant<'a>(
	messages: &mut Messages<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	generic_usages: &mut Vec<GenericUsage>,
	enclosing_generic_parameters: &GenericParameters<'a>,
	root_layers: &RootLayers<'a>,
	symbols: &mut Symbols<'a>,
	module_path: &'a [String],
	function_initial_symbols_length: usize,
	variant_shape: &EnumVariantShape<'a>,
	statement: &tree::Enum<'a>,
) {
	let scope = symbols.child_scope();
	let tree_variant = &statement.variants[variant_shape.variant_index];

	let lock = type_store.user_types.read()[variant_shape.struct_shape_index].clone();
	let struct_shape = lock.read();
	for (generic_index, generic) in struct_shape.generic_parameters.parameters().iter().enumerate() {
		let kind = SymbolKind::UserTypeGeneric { shape_index: variant_shape.struct_shape_index, generic_index };
		let symbol = Symbol {
			name: generic.name.item,
			kind,
			span: Some(generic.name.span),
			used: true,
			imported: false,
		};
		scope.symbols.push_symbol(messages, function_initial_symbols_length, symbol);
	}
	drop(struct_shape);

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
			function_initial_symbols_length,
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
		tree::Variant::StructLike(struct_like) => {
			let variant_name = struct_like.name.item;

			for field in struct_like.fields {
				let field_type = match type_store.lookup_type(
					messages,
					function_store,
					module_path,
					generic_usages,
					root_layers,
					scope.symbols,
					function_initial_symbols_length,
					&blank_generic_parameters,
					&field.parsed_type,
				) {
					Some(type_id) => type_id,
					None => type_store.any_collapse_type_id(),
				};

				let span = field.name.span + field.parsed_type.span;
				if let Some(existing) = fields.iter().find(|f| f.item.name == field.name.item) {
					let error = error!("Duplicate field `{}` on enum variant struct `{}`", field.name.item, variant_name);
					let note = note!(existing.span, "Original field here");
					messages.message(error.span(span).note(note))
				}

				let field_shape = FieldShape {
					name: field.name.item,
					field_type,
					attribute: field.attribute,
					read_only: field.read_only,
				};
				let node = Node::new(field_shape, span);
				fields.push(node);
			}
		}

		tree::Variant::Transparent(transparent) => {
			let field_type = match type_store.lookup_type(
				messages,
				function_store,
				module_path,
				generic_usages,
				root_layers,
				scope.symbols,
				function_initial_symbols_length,
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
				read_only: false,
			};
			let span = transparent.parsed_type.span;
			let node = Node::new(field_shape, span);
			fields.push(node);
		}
	}

	let mut user_type = lock.write();
	match &mut user_type.kind {
		UserTypeKind::Struct { shape } => {
			shape.fields = fields;
			assert!(!shape.been_filled);
			shape.been_filled = true;

			let has_specializations = !shape.specializations.is_empty();
			drop(user_type);

			if has_specializations {
				fill_pre_existing_struct_specializations(
					messages,
					type_store,
					function_store,
					generic_usages,
					enclosing_generic_parameters,
					module_path,
					variant_shape.struct_shape_index,
				);
			}
		}

		UserTypeKind::Enum { .. } => unreachable!("enum"),
		UserTypeKind::Union { .. } => unreachable!("union"),
	}
}

fn fill_block_union<'a>(
	messages: &mut Messages<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	generic_usages: &mut Vec<GenericUsage>,
	enclosing_generic_parameters: &GenericParameters<'a>,
	root_layers: &RootLayers<'a>,
	symbols: &mut Symbols<'a>,
	module_path: &'a [String],
	function_initial_symbols_length: usize,
	statement: &tree::Union<'a>,
	union_shape_index: usize,
) {
	let lock = type_store.user_types.read()[union_shape_index].clone();
	let user_type = lock.read();

	let filling_lock = match &user_type.kind {
		UserTypeKind::Union { shape } => shape.filling_lock.clone(),
		UserTypeKind::Struct { .. } => unreachable!("struct"),
		UserTypeKind::Enum { .. } => unreachable!("enum"),
	};
	let _filling_guard = filling_lock.lock();

	let scope = symbols.child_scope();

	for (generic_index, generic) in user_type.generic_parameters.parameters().iter().enumerate() {
		let kind = SymbolKind::UserTypeGeneric { shape_index: union_shape_index, generic_index };
		let symbol = Symbol {
			name: generic.name.item,
			kind,
			span: Some(generic.name.span),
			used: true,
			imported: false,
		};
		scope.symbols.push_symbol(messages, function_initial_symbols_length, symbol);
	}
	drop(user_type);

	let user_type = lock.read();
	let shape = match &user_type.kind {
		UserTypeKind::Union { shape } => shape,
		UserTypeKind::Struct { .. } => unreachable!("struct"),
		UserTypeKind::Enum { .. } => unreachable!("enum"),
	};

	// Yuck
	let mut variant_shapes = shape.variant_shapes.to_vec();
	drop(user_type);

	for variant_shape in &mut variant_shapes {
		fill_union_variant(
			messages,
			type_store,
			function_store,
			generic_usages,
			enclosing_generic_parameters,
			root_layers,
			scope.symbols,
			module_path,
			function_initial_symbols_length,
			&variant_shape,
			statement,
		);
	}

	let mut user_type = lock.write();
	match &mut user_type.kind {
		UserTypeKind::Union { shape } => {
			shape.variant_shapes = SliceRef::from(variant_shapes);
			assert!(!shape.been_filled);
			shape.been_filled = true;

			let has_specializations = !shape.specializations.is_empty();
			drop(user_type);

			if has_specializations {
				fill_pre_existing_union_specializations(
					messages,
					type_store,
					function_store,
					generic_usages,
					enclosing_generic_parameters,
					module_path,
					union_shape_index,
				);
			}
		}

		UserTypeKind::Struct { .. } => unreachable!("struct"),
		UserTypeKind::Enum { .. } => unreachable!("enum"),
	}
}

fn fill_union_variant<'a>(
	messages: &mut Messages<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	generic_usages: &mut Vec<GenericUsage>,
	enclosing_generic_parameters: &GenericParameters<'a>,
	root_layers: &RootLayers<'a>,
	symbols: &mut Symbols<'a>,
	module_path: &'a [String],
	function_initial_symbols_length: usize,
	variant_shape: &UnionVariantShape<'a>,
	statement: &tree::Union<'a>,
) {
	let scope = symbols.child_scope();
	let tree_variant = &statement.variants[variant_shape.variant_index];

	let lock = type_store.user_types.read()[variant_shape.struct_shape_index].clone();
	let struct_shape = lock.read();
	for (generic_index, generic) in struct_shape.generic_parameters.parameters().iter().enumerate() {
		let kind = SymbolKind::UserTypeGeneric { shape_index: variant_shape.struct_shape_index, generic_index };
		let symbol = Symbol {
			name: generic.name.item,
			kind,
			span: Some(generic.name.span),
			used: true,
			imported: false,
		};
		scope.symbols.push_symbol(messages, function_initial_symbols_length, symbol);
	}
	drop(struct_shape);

	let mut fields: Vec<Node<FieldShape<'_>>> = Vec::new();

	let blank_generic_parameters = GenericParameters::new_from_explicit(Vec::new());
	match tree_variant {
		tree::Variant::StructLike(struct_like) => {
			let variant_name = struct_like.name.item;

			for field in struct_like.fields {
				let field_type = match type_store.lookup_type(
					messages,
					function_store,
					module_path,
					generic_usages,
					root_layers,
					scope.symbols,
					function_initial_symbols_length,
					&blank_generic_parameters,
					&field.parsed_type,
				) {
					Some(type_id) => type_id,
					None => type_store.any_collapse_type_id(),
				};

				let span = field.name.span + field.parsed_type.span;
				if let Some(existing) = fields.iter().find(|f| f.item.name == field.name.item) {
					let error = error!("Duplicate field `{}` on union variant struct `{}`", field.name.item, variant_name);
					let note = note!(existing.span, "Original field here");
					messages.message(error.span(span).note(note))
				}

				let field_shape = FieldShape {
					name: field.name.item,
					field_type,
					attribute: field.attribute,
					read_only: field.read_only,
				};
				let node = Node::new(field_shape, span);
				fields.push(node);
			}
		}

		tree::Variant::Transparent(transparent) => {
			let field_type = match type_store.lookup_type(
				messages,
				function_store,
				module_path,
				generic_usages,
				root_layers,
				scope.symbols,
				function_initial_symbols_length,
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
				read_only: false,
			};
			let span = transparent.parsed_type.span;
			let node = Node::new(field_shape, span);
			fields.push(node);
		}
	}

	let mut user_type = lock.write();
	match &mut user_type.kind {
		UserTypeKind::Struct { shape } => {
			shape.fields = fields;
			assert!(!shape.been_filled);
			shape.been_filled = true;

			let has_specializations = !shape.specializations.is_empty();
			drop(user_type);

			if has_specializations {
				fill_pre_existing_struct_specializations(
					messages,
					type_store,
					function_store,
					generic_usages,
					enclosing_generic_parameters,
					module_path,
					variant_shape.struct_shape_index,
				);
			}
		}

		UserTypeKind::Enum { .. } => unreachable!("enum"),
		UserTypeKind::Union { .. } => unreachable!("union"),
	}
}

fn fill_pre_existing_struct_specializations<'a>(
	messages: &mut Messages<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	generic_usages: &mut Vec<GenericUsage>,
	enclosing_generic_parameters: &GenericParameters<'a>,
	module_path: &'a [String],
	shape_index: usize,
) {
	let lock = type_store.user_types.read()[shape_index].clone();
	let user_type = lock.read();
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
	drop(user_type);

	for specialization in &mut specializations {
		let mut fields = fields.clone();
		for field in &mut fields {
			field.type_id = type_store.specialize_type_id_with_generics(
				messages,
				function_store,
				module_path,
				generic_usages,
				enclosing_generic_parameters,
				field.type_id,
				&specialization.type_arguments,
				TypeIdSpecializationSituation::UserType { user_type_shape_index: shape_index },
			);
		}

		assert!(!specialization.been_filled);
		assert_eq!(specialization.fields.len(), 0);
		specialization.fields = SliceRef::from(fields);
	}

	let mut user_type = lock.write();
	match &mut user_type.kind {
		UserTypeKind::Struct { shape } => {
			for (actual, updated) in shape.specializations.iter_mut().zip(specializations.into_iter()) {
				assert!(!actual.been_filled);
				actual.been_filled = true;

				assert_eq!(actual.fields.len(), 0);
				actual.fields = updated.fields;
			}
		}

		kind => unreachable!("{kind:?}"),
	}

	drop(user_type);
	let user_type = lock.read();

	match &user_type.kind {
		UserTypeKind::Struct { shape } => {
			let specializations = shape.specializations.clone();
			drop(user_type);

			for specialization in &specializations {
				let type_id = specialization.type_id;
				let chain = type_store.find_user_type_dependency_chain(type_id, type_id);
				if let Some(chain) = chain {
					report_cyclic_user_type(messages, type_store, function_store, module_path, type_id, chain, span);
				} else if let Some(layout) = specialization.layout {
					let name = lock.read().name;
					panic!("{name}: {:?}", layout);
				}
			}
		}

		kind => unreachable!("{kind:?}"),
	}
}

fn fill_pre_existing_enum_specializations<'a>(
	messages: &mut Messages<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	generic_usages: &mut Vec<GenericUsage>,
	enclosing_generic_parameters: &GenericParameters<'a>,
	module_path: &'a [String],
	shape_index: usize,
) {
	let lock = type_store.user_types.read()[shape_index].clone();
	let user_type = lock.read();
	let span = user_type.span;
	let shape = match &user_type.kind {
		UserTypeKind::Enum { shape } => shape,
		kind => unreachable!("{kind:?}"),
	};

	let shared_fields_shapes = shape.shared_fields.clone();
	let mut specializations = shape.specializations.clone(); // Belch
	drop(user_type);

	for specialization in &mut specializations {
		assert!(!specialization.been_filled);

		assert_eq!(specialization.shared_fields.len(), 1); // The tag field
		let mut shared_fields = Vec::with_capacity(shared_fields_shapes.len() + 1);
		shared_fields.extend_from_slice(&specialization.shared_fields);

		for field_shape_node in &mut shared_fields_shapes.iter() {
			let field_shape = &field_shape_node.item;

			let type_id = type_store.specialize_type_id_with_generics(
				messages,
				function_store,
				module_path,
				generic_usages,
				enclosing_generic_parameters,
				field_shape.field_type,
				&specialization.type_arguments,
				TypeIdSpecializationSituation::UserType { user_type_shape_index: shape_index },
			);

			let field = Field {
				span: Some(field_shape_node.span),
				name: field_shape.name,
				type_id,
				attribute: field_shape.attribute,
				read_only: field_shape.read_only,
			};

			shared_fields.push(field);
		}

		specialization.shared_fields = SliceRef::from(shared_fields);
	}

	let mut user_type = lock.write();
	match &mut user_type.kind {
		UserTypeKind::Enum { shape } => {
			for (actual, updated) in shape.specializations.iter_mut().zip(specializations.into_iter()) {
				assert!(!actual.been_filled);
				actual.been_filled = true;

				assert!(actual.layout.is_none());

				actual.shared_fields = updated.shared_fields;
			}
		}

		kind => unreachable!("{kind:?}"),
	}
	drop(user_type);

	let user_type = lock.read();
	match &user_type.kind {
		UserTypeKind::Enum { shape } => {
			let specializations: Vec<_> = shape.specializations.clone();
			drop(user_type);

			for specialization in specializations {
				let type_id = specialization.type_id;
				let chain = type_store.find_user_type_dependency_chain(type_id, type_id);
				if let Some(chain) = chain {
					report_cyclic_user_type(messages, type_store, function_store, module_path, type_id, chain, span);
				} else if let Some(layout) = specialization.layout {
					let name = lock.read().name;
					panic!("{name}: {:?}", layout);
				}
			}
		}

		kind => unreachable!("{kind:?}"),
	}
}

fn fill_pre_existing_union_specializations<'a>(
	messages: &mut Messages<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	generic_usages: &mut Vec<GenericUsage>,
	enclosing_generic_parameters: &GenericParameters<'a>,
	module_path: &'a [String],
	union_shape_index: usize,
) {
	let lock = type_store.user_types.read()[union_shape_index].clone();
	let user_type = lock.read();
	let span = user_type.span;
	let shape = match &user_type.kind {
		UserTypeKind::Union { shape } => shape,
		kind => unreachable!("{kind:?}"),
	};

	let variant_shapes = shape.variant_shapes.clone();
	let mut specializations = shape.specializations.clone(); // Belch
	drop(user_type);

	for specialization in &mut specializations {
		assert!(!specialization.been_filled);

		let (fields, variants, variants_by_name) = type_store.generate_union_variant_info(
			messages,
			function_store,
			module_path,
			generic_usages,
			enclosing_generic_parameters,
			union_shape_index,
			&specialization.type_arguments,
			&variant_shapes,
		);

		specialization.fields = SliceRef::from(fields);
		specialization.variants = SliceRef::from(variants);
		specialization.variants_by_name = Ref::new(variants_by_name);
	}

	let mut user_type = lock.write();
	match &mut user_type.kind {
		UserTypeKind::Union { shape } => {
			for (actual, updated) in shape.specializations.iter_mut().zip(specializations.into_iter()) {
				assert!(!actual.been_filled);
				actual.been_filled = true;

				assert!(actual.layout.is_none());

				actual.fields = updated.fields;
				actual.variants = updated.variants;
				actual.variants_by_name = updated.variants_by_name;
			}
		}

		kind => unreachable!("{kind:?}"),
	}
	drop(user_type);

	let user_type = lock.read();
	match &user_type.kind {
		UserTypeKind::Union { shape } => {
			let specializations: Vec<_> = shape.specializations.clone();
			drop(user_type);

			for specialization in specializations {
				let type_id = specialization.type_id;
				let chain = type_store.find_user_type_dependency_chain(type_id, type_id);
				if let Some(chain) = chain {
					report_cyclic_user_type(messages, type_store, function_store, module_path, type_id, chain, span);
				} else if let Some(layout) = specialization.layout {
					let name = lock.read().name;
					panic!("{name}: {:?}", layout);
				}
			}
		}

		kind => unreachable!("{kind:?}"),
	}
}

fn report_cyclic_user_type<'a>(
	messages: &mut Messages<'a>,
	type_store: &mut TypeStore<'a>,
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
	when_context: &WhenContext,
	messages: &mut Messages<'a>,
	lang_items: &RwLock<LangItems>,
	root_layers: &RootLayers<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	generic_usages: &mut Vec<GenericUsage>,
	externs: &RwLock<Externs<'a>>,
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

			let FunctionMethodInfo {
				builtin_method_base_type_id,
				methods_index,
				method_base_shape_index,
			} = determine_method_info(messages, root_layers, type_store, symbols, statement);

			let base_shape_generics = if statement.method_attribute.is_some() {
				if let Some(method_base_shape_index) = method_base_shape_index {
					Some(
						type_store.user_types.read()[method_base_shape_index]
							.read()
							.generic_parameters
							.clone(), // I am sad
					)
				} else {
					Some(GenericParameters::new_from_explicit(Vec::new()))
				}
			} else {
				None
			};

			let function_initial_symbols_length = symbols.symbols.len();
			let scope = symbols.child_scope();

			let mut function_store_shapes = function_store.shapes.write();
			let function_shape_index = function_store_shapes.len();
			function_store_shapes.push(None);
			drop(function_store_shapes);

			let capacity = statement.generics.len()
				+ enclosing_generic_parameters.parameters().len()
				+ base_shape_generics.as_ref().map(|p| p.parameters().len()).unwrap_or(0);
			let mut explicit_generics = Vec::with_capacity(capacity);

			for (generic_index, generic) in statement.generics.iter().enumerate() {
				let (constraint_trait_ids, generic_constraints) = type_store.lookup_constraints(
					messages,
					root_layers,
					function_store,
					module_path,
					scope.symbols,
					generic_usages,
					function_initial_symbols_length,
					generic.constraints,
					enclosing_generic_parameters,
				);
				let generic_type_id =
					type_store.register_function_generic(function_shape_index, generic_index, false, 0, &constraint_trait_ids);
				explicit_generics.push(GenericParameter {
					name: generic.name,
					constraint_trait_ids,
					generic_constraints,
					generic_type_id,
				});

				let kind = SymbolKind::FunctionGeneric { function_shape_index, generic_index };
				let symbol = Symbol {
					name: generic.name.item,
					kind,
					span: Some(generic.name.span),
					used: true,
					imported: false,
				};
				scope.symbols.push_symbol(messages, function_initial_symbols_length, symbol);

				// TODO(perf): This is such a terrible horrible performance killing hack
				let generic_parameters = GenericParameters::new_from_explicit(explicit_generics.clone());
				let mut function_store_generics = function_store.generics.write();
				if function_store_generics.len() == function_shape_index {
					function_store_generics.push(generic_parameters);
				} else {
					function_store_generics[function_shape_index] = generic_parameters;
				}
				drop(function_store_generics);
			}

			let explicit_generics_len = explicit_generics.len();
			let mut generic_parameters = GenericParameters::new_from_explicit(explicit_generics);
			if statement.method_attribute.is_none() {
				for (index, parent_parameter) in enclosing_generic_parameters.parameters().iter().enumerate() {
					let generic_index = explicit_generics_len + index;
					let constraint_trait_ids = parent_parameter.constraint_trait_ids.clone();
					let generic_constraints = parent_parameter.generic_constraints.clone();
					let generic_type_id = type_store.register_function_generic(
						function_shape_index,
						generic_index,
						false,
						0,
						&constraint_trait_ids,
					);
					generic_parameters.push_implicit(GenericParameter {
						name: parent_parameter.name,
						constraint_trait_ids,
						generic_constraints,
						generic_type_id,
					});

					let kind = SymbolKind::FunctionGeneric { function_shape_index, generic_index };
					let span = Some(parent_parameter.name.span);
					let symbol = Symbol {
						name: parent_parameter.name.item,
						kind,
						span,
						used: true,
						imported: false,
					};
					scope.symbols.push_symbol(messages, function_initial_symbols_length, symbol);
				}
			}

			let mut base_type_arguments;
			let implicit_generics_len = generic_parameters.implicit_len();
			if let Some(base_shape_generics) = &base_shape_generics {
				base_type_arguments = TypeArguments::new_from_explicit(Vec::new());

				for (index, base_type_parameter) in base_shape_generics.parameters().iter().enumerate() {
					let generic_index = explicit_generics_len + implicit_generics_len + index;
					let constraint_trait_ids = base_type_parameter.constraint_trait_ids.clone();
					let generic_constraints = base_type_parameter.generic_constraints.clone();
					let generic_type_id = type_store.register_function_generic(
						function_shape_index,
						generic_index,
						false,
						0,
						&constraint_trait_ids,
					);
					generic_parameters.push_method_base(GenericParameter {
						name: base_type_parameter.name,
						constraint_trait_ids,
						generic_constraints,
						generic_type_id,
					});
					base_type_arguments
						.ids
						.push(Node::new(generic_type_id, base_type_parameter.name.span));

					let kind = SymbolKind::FunctionGeneric { function_shape_index, generic_index };
					let span = Some(base_type_parameter.name.span);
					let symbol = Symbol {
						name: base_type_parameter.name.item,
						kind,
						span,
						used: true,
						imported: false,
					};
					scope.symbols.push_symbol(messages, function_initial_symbols_length, symbol);
				}

				base_type_arguments.explicit_len = base_shape_generics.explicit_len();
				base_type_arguments.implicit_len = base_shape_generics.implicit_len();
				base_type_arguments.method_base_len = base_shape_generics.method_base_len();
			} else {
				base_type_arguments = TypeArguments::new_from_explicit(Vec::new());
			}

			let mut function_store_generics = function_store.generics.write();
			if function_store_generics.len() == function_shape_index {
				function_store_generics.push(generic_parameters.clone());
			} else {
				function_store_generics[function_shape_index] = generic_parameters.clone();
			}
			drop(function_store_generics);

			let return_type = if let Some(parsed_type) = &statement.parsed_type {
				let type_id = type_store.lookup_type(
					messages,
					&function_store,
					module_path,
					generic_usages,
					root_layers,
					scope.symbols,
					function_initial_symbols_length,
					&generic_parameters,
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

				Node::new(return_type, parsed_type.span)
			} else {
				// TODO: This span isn't exactly ideal
				Node::new(type_store.void_type_id(), statement.name.span)
			};

			let mut parameters = Vec::new();
			if let Some(method_attribute) = &statement.method_attribute {
				let mutable = match method_attribute.item.kind.item {
					MethodKind::ImmutableSelf => Some(false),
					MethodKind::MutableSelf => Some(true),
					MethodKind::Static => None,
				};

				if let Some(mutable) = mutable {
					let type_id = if let Some(method_base_shape_index) = method_base_shape_index {
						assert!(builtin_method_base_type_id.is_none());

						let base_type = type_store.get_or_add_shape_specialization(
							messages,
							&function_store,
							module_path,
							generic_usages,
							enclosing_generic_parameters,
							method_base_shape_index,
							Some(method_attribute.span),
							Ref::new(base_type_arguments),
						);

						match base_type {
							Some(base_type) => type_store.pointer_to(base_type, mutable),
							None => type_store.any_collapse_type_id(),
						}
					} else if let Some(builtin_method_base_type_id) = builtin_method_base_type_id {
						type_store.pointer_to(builtin_method_base_type_id, mutable)
					} else {
						type_store.any_collapse_type_id()
					};

					let readable_index = readables.push("self", type_id, ReadableKind::Let, mutable);
					assert_eq!(readable_index, 0);
					parameters.push(ParameterShape {
						span: Span::unusable(),
						label: None,
						type_id,
						readable_index,
						is_mutable: false,
					});
				}
			}

			let maybe_self = parameters.len();
			for (index, parameter) in statement.parameters.item.parameters.iter().enumerate() {
				let type_id = type_store.lookup_type(
					messages,
					&function_store,
					module_path,
					generic_usages,
					root_layers,
					scope.symbols,
					function_initial_symbols_length,
					&generic_parameters,
					&parameter.item.parsed_type,
				);

				let type_id = match type_id {
					Some(type_id) => type_id,
					None => type_store.any_collapse_type_id(),
				};

				let readable_kind = match parameter.item.is_mutable {
					false => ReadableKind::Let,
					true => ReadableKind::Mut,
				};

				let name = parameter.item.name.item;
				let readable_index = readables.push(name, type_id, readable_kind, true);
				assert_eq!(readable_index, index + maybe_self);

				let span = parameter.span;
				let label = parameter.item.label;
				let is_mutable = parameter.item.is_mutable;
				parameters.push(ParameterShape { span, label, type_id, readable_index, is_mutable });
			}

			drop(scope);

			let in_std = module_path.first().map(|s| s == "fae").unwrap_or(false);
			if !in_std {
				if let Some(intrinsic_attribute) = statement.intrinsic_attribute {
					let error = error!("Function may not be annotated as an intrinsic outside the standard library");
					messages.message(error.span(intrinsic_attribute.span));
				}
			}

			let c_varargs = statement.parameters.item.c_varargs;
			if let Some(varargs_span) = c_varargs {
				if statement.extern_attribute.is_none() {
					let message = error!("Function must be an extern to accept C varargs");
					messages.message(message.span(varargs_span));
				}
			}

			let name = statement.name;
			if let Some(Node { item: extern_attribute, .. }) = statement.extern_attribute {
				externs.write().push(messages, extern_attribute.name, name.span);
			}

			let root_name = root_layers.root_name.as_str();
			let is_main = statement.method_attribute.is_none() && module_path == [root_name] && name.item == "main";

			let shape = FunctionShape {
				name,
				module_path,
				is_main,
				trait_method_marker: None,
				generic_parameters,
				extern_attribute: statement.extern_attribute,
				export_attribute: statement.export_attribute,
				intrinsic_attribute: statement.intrinsic_attribute,
				lang_attribute: statement.lang_attribute,
				method_base_index: method_base_shape_index,
				parameters: Node::new(parameters, statement.parameters.span),
				c_varargs,
				return_type,
				block: None,
				generic_usages: SliceRef::new_empty(),
				specializations_by_type_arguments: FxHashMap::default(),
				specializations: Vec::new(),
			};
			function_store.shapes.write()[function_shape_index] = Some(Ref::new(RwLock::new(shape)));

			local_function_shape_indicies.push(function_shape_index);

			if let Some(method_attribute) = &statement.method_attribute {
				if let Some(methods_index) = methods_index {
					let kind = method_attribute.item.kind;
					let info = MethodInfo { function_shape_index, kind };

					let collections = type_store.method_collections.read();
					let mut collection = collections[methods_index].write();

					// TODO: Detect duplicate methods
					let index = collection.methods.len();
					collection.methods.push(info);
					assert!(collection.methods_by_name.insert(statement.name.item, index).is_none());
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
				let symbol = Symbol { name: name.item, kind, span, used: true, imported: false };
				symbols.push_symbol(messages, function_initial_symbols_length, symbol);
			}

			readables.readables.truncate(original_readables_overall_len);
			readables.starting_index = original_readables_starting_index;
		} else if let tree::Statement::WhenElseChain(statement) = statement {
			if let Some(body) = when_context.evaluate_when(messages, &statement.item) {
				create_block_functions(
					when_context,
					messages,
					lang_items,
					root_layers,
					type_store,
					function_store,
					generic_usages,
					externs,
					readables,
					symbols,
					module_path,
					enclosing_generic_parameters,
					&body.item,
					local_function_shape_indicies,
					scope_id,
				);
			}
		}
	}

	generic_usages.truncate(original_generic_usages_len);
}

struct FunctionMethodInfo {
	builtin_method_base_type_id: Option<TypeId>,
	methods_index: Option<usize>,
	method_base_shape_index: Option<usize>,
}

fn determine_method_info<'a>(
	messages: &mut Messages<'a>,
	root_layers: &RootLayers<'a>,
	type_store: &mut TypeStore<'a>,
	symbols: &mut Symbols<'a>,
	statement: &tree::Function<'a>,
) -> FunctionMethodInfo {
	let Some(method_attribute) = &statement.method_attribute else {
		return FunctionMethodInfo {
			builtin_method_base_type_id: None,
			methods_index: None,
			method_base_shape_index: None,
		};
	};

	let Some(symbol) = symbols.lookup_path_symbol(
		messages,
		root_layers,
		type_store,
		symbols.symbols.len(),
		&method_attribute.item.base_type.item,
	) else {
		return FunctionMethodInfo {
			builtin_method_base_type_id: None,
			methods_index: None,
			method_base_shape_index: None,
		};
	};

	match &symbol.kind {
		SymbolKind::BuiltinType { type_id, methods_index } => FunctionMethodInfo {
			builtin_method_base_type_id: Some(*type_id),
			methods_index: Some(*methods_index),
			method_base_shape_index: None,
		},

		SymbolKind::UserType { shape_index, methods_index } => {
			if method_attribute.item.kind.item == MethodKind::Static {
				let lock = type_store.user_types.read()[*shape_index].clone();
				let user_type = lock.read();
				let base_name = user_type.name;

				if let UserTypeKind::Enum { shape } = &user_type.kind {
					let method_name = statement.name.item;
					if let Some(existing) = shape.variant_shapes.iter().find(|v| v.name == method_name) {
						let variant_name = existing.name;
						let error =
							error!("Static method `{method_name}` on enum `{base_name}` conflicts with variant `{variant_name}`");
						let note = note!(existing.span, "Variant `{variant_name}` here");
						messages.message(error.span(statement.name.span).note(note));
					}
				}
			}

			FunctionMethodInfo {
				builtin_method_base_type_id: None,
				methods_index: Some(*methods_index),
				method_base_shape_index: Some(*shape_index),
			}
		}

		_ => {
			let error = error!("Method declaration self type specifier must be a type");
			messages.message(error.span(method_attribute.item.base_type.span));
			return FunctionMethodInfo {
				builtin_method_base_type_id: None,
				methods_index: None,
				method_base_shape_index: None,
			};
		}
	}
}

fn validate_block_consts<'a>(context: &mut Context<'a, '_, '_>, block: &'a tree::Block<'a>) {
	for statement in block.statements {
		match statement {
			tree::Statement::Const(statement) => {
				validate_const(context, statement);
			}

			tree::Statement::WhenElseChain(statement) => {
				if let Some(body) = context.when_context.evaluate_when(context.messages, &statement.item) {
					validate_block_consts(context, &body.item);
				}
			}

			_ => {}
		}
	}
}

fn validate_block_statics<'a>(context: &mut Context<'a, '_, '_>, block: &'a tree::Block<'a>) {
	for statement in block.statements {
		match statement {
			tree::Statement::Static(statement) => {
				validate_static(context, statement);
			}

			tree::Statement::WhenElseChain(statement) => {
				if let Some(body) = context.when_context.evaluate_when(context.messages, &statement.item) {
					validate_block_statics(context, &body.item);
				}
			}

			_ => {}
		}
	}
}

fn validate_block<'a>(mut context: Context<'a, '_, '_>, block: &'a tree::Block<'a>, is_root: bool) -> Block<'a> {
	let scope_id = ScopeId {
		file_index: context.file_index,
		scope_index: context.scope_index,
	};

	let should_import_prelude = is_root && context.cli_arguments.std_enabled;
	let block = validate_block_in_context(&mut context, block, &mut 0, scope_id, is_root, should_import_prelude);
	if is_root {
		let scope = &context.symbols_scope.symbols.symbols;
		crate::frontend::symbols::report_unused(scope, context.messages);
	} else {
		context.symbols_scope.report_unused(context.messages);
	}

	block
}

fn validate_block_in_context<'a>(
	context: &mut Context<'a, '_, '_>,
	block: &'a tree::Block<'a>,
	index_in_block: &mut usize,
	scope_id: ScopeId,
	is_root: bool,
	should_import_prelude: bool,
) -> Block<'a> {
	if !is_root {
		create_block_traits(
			context.when_context,
			context.messages,
			context.root_layers,
			context.type_store,
			context.function_store,
			context.module_path,
			context.function_generic_usages,
			context.symbols_scope.symbols,
			context.function_initial_symbols_length,
			context.generic_parameters,
			block,
			is_root,
			context.trait_shape_indicies,
		);

		resolve_block_trait_imports(
			context.herd_member,
			context.when_context,
			context.messages,
			context.root_layers,
			context.symbols_scope.symbols,
			context.module_path,
			context.function_initial_symbols_length,
			block,
			should_import_prelude,
		);

		create_block_types(
			context.when_context,
			context.messages,
			context.lang_items,
			context.root_layers,
			context.type_store,
			context.function_store,
			context.module_path,
			context.symbols_scope.symbols,
			context.function_generic_usages,
			context.function_initial_symbols_length,
			context.generic_parameters,
			block,
			scope_id,
			context.type_shape_indicies,
		);

		resolve_block_type_imports(
			context.herd_member,
			context.when_context,
			context.messages,
			context.root_layers,
			context.symbols_scope.symbols,
			context.module_path,
			context.function_initial_symbols_length,
			block,
			should_import_prelude,
		);

		fill_block_traits(
			context.when_context,
			context.messages,
			context.lang_items,
			context.type_store,
			context.function_store,
			context.function_generic_usages,
			context.root_layers,
			context.symbols_scope.symbols,
			context.module_path,
			context.function_initial_symbols_length,
			context.generic_parameters,
			block,
			scope_id,
			&mut context.trait_shape_indicies.iter(),
		);

		fill_block_types(
			context.when_context,
			context.messages,
			context.lang_items,
			context.type_store,
			context.function_store,
			context.function_generic_usages,
			context.root_layers,
			context.symbols_scope.symbols,
			context.module_path,
			context.function_initial_symbols_length,
			context.generic_parameters,
			block,
			scope_id,
			&mut context.type_shape_indicies.iter(),
		);
		context.type_shape_indicies.clear();
	}

	resolve_block_non_range_imports(
		context.herd_member,
		context.messages,
		context.root_layers,
		context.symbols_scope.symbols,
		context.module_path,
		context.function_initial_symbols_length,
		block,
		should_import_prelude,
	);

	if !is_root {
		create_block_functions(
			context.when_context,
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
		validate_block_consts(context, block);
	}

	let mut yields = false;
	let mut returns = false;
	let mut statements = Vec::with_capacity(block.statements.len());

	for statement in block.statements {
		let statement = validate_statement(context, scope_id, statement, &mut yields, &mut returns, index_in_block, is_root);
		if let Some(statement) = statement {
			statements.push(statement);
		}
	}

	let type_id = if let Some(index) = context.current_yield_target_index {
		let any_collapse = context.type_store.any_collapse_type_id();
		context.yield_targets.get(index).type_id.unwrap_or(any_collapse)
	} else {
		context.type_store.void_type_id()
	};

	let yield_target_index = None;
	Block { type_id, yields, yield_target_index, returns, statements }
}

fn validate_statement<'a>(
	context: &mut Context<'a, '_, '_>,
	scope_id: ScopeId,
	statement: &'a tree::Statement<'a>,
	yields: &mut bool,
	returns: &mut bool,
	index_in_block: &mut usize,
	is_root: bool,
) -> Option<Statement<'a>> {
	context.expected_type = None;

	match statement {
		tree::Statement::Expression(..)
		| tree::Statement::Block(..)
		| tree::Statement::IfElseChain(..)
		| tree::Statement::Match(..)
		| tree::Statement::While(..)
		| tree::Statement::For(..)
		| tree::Statement::Binding(..)
		| tree::Statement::Defer(..)
		| tree::Statement::Break(..)
		| tree::Statement::Continue(..)
		| tree::Statement::Yield(..)
		| tree::Statement::Return(..)
			if is_root => {} // `is_root` is true, then we've already emitted a message in the root pre-process step, skip

		tree::Statement::Expression(statement) => {
			let mut expression = validate_expression(context, statement);
			expression.into_value(context);
			*yields |= expression.yields;
			*returns |= expression.returns;

			let kind = StatementKind::Expression(expression);
			return Some(Statement {
				kind,
				debug_location: statement.span.debug_location(context.parsed_files),
			});
		}

		tree::Statement::Block(statement) => {
			let scope = context.child_scope();
			let block = validate_block(scope, &statement.item, false);
			*yields |= block.yields;
			*returns |= block.returns;
			let kind = StatementKind::Block(block);
			return Some(Statement {
				kind,
				debug_location: statement.span.debug_location(context.parsed_files),
			});
		}

		tree::Statement::WhenElseChain(statement) => {
			if let Some(block) = validate_when(context, statement, index_in_block, scope_id, is_root) {
				*yields |= block.yields;
				*returns |= block.returns;
				let kind = StatementKind::When(block);
				return Some(Statement {
					kind,
					debug_location: statement.span.debug_location(context.parsed_files),
				});
			}
		}

		tree::Statement::IfElseChain(chain) => {
			let expression = validate_if_else_chain_expression(context, &chain.item, chain.span, None);
			*yields |= expression.yields;
			*returns |= expression.returns;

			let kind = StatementKind::Expression(expression);
			return Some(Statement {
				kind,
				debug_location: chain.span.debug_location(context.parsed_files),
			});
		}

		tree::Statement::Match(statement) => {
			let expression = validate_match_expression(context, &statement.item, statement.span, None);
			*yields |= expression.yields;
			*returns |= expression.returns;

			let kind = StatementKind::Expression(expression);
			return Some(Statement {
				kind,
				debug_location: statement.span.debug_location(context.parsed_files),
			});
		}

		tree::Statement::While(statement) => {
			let debug_location = statement.span.debug_location(context.parsed_files);
			let statement = validate_while_statement(context, statement);
			let kind = StatementKind::While(statement);
			return Some(Statement { kind, debug_location });
		}

		tree::Statement::For(statement) => {
			let debug_location = statement.span.debug_location(context.parsed_files);
			let statement = validate_for_statement(context, statement);
			let kind = StatementKind::For(statement);
			return Some(Statement { kind, debug_location });
		}

		tree::Statement::Import(..) => {}

		tree::Statement::Struct(..) => {}

		tree::Statement::Enum(..) => {}

		tree::Statement::Union(..) => {}

		tree::Statement::Trait(..) => {}

		tree::Statement::Function(statement) => {
			let index = *index_in_block;
			*index_in_block += 1;
			validate_function(context, statement, index, scope_id)
		}

		tree::Statement::Const(..) => {}

		tree::Statement::Static(..) => {}

		tree::Statement::Binding(statement) => {
			let validated = match validate_binding(context, statement) {
				Some(validated) => validated,
				None => return None,
			};

			let kind = StatementKind::Binding(validated);
			return Some(Statement {
				kind,
				debug_location: statement.span.debug_location(context.parsed_files),
			});
		}

		tree::Statement::Defer(statement) => {
			let span = statement.span;
			let debug_location = span.debug_location(context.parsed_files);

			let mut scope = context.child_scope();
			let scope_id = ScopeId { file_index: scope.file_index, scope_index: scope.scope_index };

			let statement = &statement.item.statement;
			let Some(statement) = validate_statement(&mut scope, scope_id, statement, &mut false, &mut false, &mut 0, false)
			else {
				return None;
			};

			if let StatementKind::Defer(_) = statement.kind {
				let error = error!("Deferring a lone defer statement is disallowed");
				scope.message(error.span(span));
				return None;
			} else if let StatementKind::Yield(_) = statement.kind {
				let error = error!("Deferring a lone yield statement is disallowed");
				scope.message(error.span(span));
				return None;
			}

			let boxed = Box::new(Defer { statement });
			let kind = StatementKind::Defer(boxed);
			return Some(Statement { kind, debug_location });
		}

		tree::Statement::Break(statement) => {
			let debug_location = statement.span.debug_location(context.parsed_files);

			let statement = if let Some(loop_index) = context.current_loop_index {
				if let Some(target_index) = context.current_yield_target_index {
					let target = context.yield_targets.get(target_index);
					if let Some(outer_loop_index) = target.outer_loop_index {
						if outer_loop_index <= loop_index {
							*yields = true;
						}
					}
				}

				Break { loop_index }
			} else {
				let error = error!("Cannot break when outside a loop");
				context.message(error.span(statement.span));
				Break { loop_index: 0 }
			};

			let kind = StatementKind::Break(statement);
			return Some(Statement { kind, debug_location });
		}

		tree::Statement::Continue(statement) => {
			let debug_location = statement.span.debug_location(context.parsed_files);

			let statement = if let Some(loop_index) = context.current_loop_index {
				if let Some(target_index) = context.current_yield_target_index {
					let target = context.yield_targets.get(target_index);
					if let Some(outer_loop_index) = target.outer_loop_index {
						if outer_loop_index <= loop_index {
							*yields = true;
						}
					}
				}

				Continue { loop_index }
			} else {
				let error = error!("Cannot continue when outside a loop");
				context.message(error.span(statement.span));
				Continue { loop_index: 0 }
			};

			let kind = StatementKind::Continue(statement);
			return Some(Statement { kind, debug_location });
		}

		tree::Statement::Yield(statement) => {
			*yields |= true;

			let debug_location = statement.span.debug_location(context.parsed_files);
			let statement = if let Some(yield_target_index) = context.current_yield_target_index {
				let original_expected_type = context.expected_type;
				context.expected_type = context.yield_targets.get(yield_target_index).type_id;
				let mut expression = validate_expression(context, &statement.item.expression);
				expression.into_value(context);
				context.expected_type = original_expected_type;

				let target_type = context.yield_targets.get_mut(yield_target_index);
				if let Some(expected) = target_type.type_id {
					if let Ok(false) = context.collapse_to(expected, &mut expression) {
						let expected = context.type_name(expected);
						let got = context.type_name(expression.type_id);
						let error = error!("Expected yield type of {expected}, got {got}");
						context.message(error.span(statement.span));
					}
				} else {
					target_type.type_id = Some(expression.type_id);
				}

				Yield { yield_target_index, expression }
			} else {
				let error = error!("Cannot yield when outside a block expression");
				context.message(error.span(statement.span));
				let expression = Expression::any_collapse(context.type_store, statement.item.expression.span);
				Yield { yield_target_index: 0, expression }
			};

			let kind = StatementKind::Yield(statement);
			return Some(Statement { kind, debug_location });
		}

		tree::Statement::Return(statement) => {
			// There are valid reasons for wanting to return within a block expression, in these
			// cases it should count as a yield to avoid errors if the returning codepath has no
			// yield statement
			*yields |= true;
			*returns |= true;

			let expression = statement.item.expression.as_ref();
			let mut expression = expression.map(|expression| {
				let mut scope = context.child_scope();
				scope.expected_type = scope.return_type;
				validate_expression(&mut scope, expression)
			});

			if let Some(expression) = &mut expression {
				expression.into_value(context);
				let return_type = context.return_type.unwrap();
				if let Ok(false) = context.collapse_to(return_type, expression) {
					let expected = context.type_name(return_type);
					let got = context.type_name(expression.type_id);
					let error = error!("Expected return type of {expected}, got {got}");
					context.message(error.span(statement.span));
				}
			}

			let kind = StatementKind::Return(Return { expression });
			return Some(Statement {
				kind,
				debug_location: statement.span.debug_location(context.parsed_files),
			});
		}
	}

	None
}

fn validate_when<'a>(
	context: &mut Context<'a, '_, '_>,
	statement: &'a Node<tree::WhenElseChain<'a>>,
	index_in_block: &mut usize,
	scope_id: ScopeId,
	is_root: bool,
) -> Option<Block<'a>> {
	if let Some(body) = context.when_context.evaluate_when(context.messages, &statement.item) {
		Some(validate_block_in_context(context, &body.item, index_in_block, scope_id, is_root, false))
	} else {
		None
	}
}

fn validate_function<'a>(
	context: &mut Context<'a, '_, '_>,
	statement: &'a tree::Function<'a>,
	index_in_block: usize,
	scope_id: ScopeId,
) {
	if statement.extern_attribute.is_some() || statement.intrinsic_attribute.is_some() {
		// Note: Signature has already been checked in `create_block_functions`
		return;
	}

	let enclosing_generic_parameters = context.generic_parameters;

	let function_shape_index = context.local_function_shape_index(index_in_block);
	let lock = context.function_store.shapes.read()[function_shape_index]
		.as_ref()
		.unwrap()
		.clone();
	let shape = lock.read();
	let return_type = shape.return_type;
	let generics = shape.generic_parameters.clone();

	let mut scope = context.child_scope_for_function(return_type.item, &generics);
	let initial_generic_usages_len = scope.function_generic_usages.len();

	for (generic_index, generic) in generics.parameters().iter().enumerate() {
		let kind = SymbolKind::FunctionGeneric { function_shape_index, generic_index };
		let symbol = Symbol {
			name: generic.name.item,
			kind,
			span: Some(generic.name.span),
			used: true,
			imported: false,
		};
		scope.push_symbol(symbol);
	}

	let mut maybe_self = 0;
	if let Some(method_attribute) = &statement.method_attribute {
		scope.method_base_index = shape.method_base_index;

		if let Some(shape_index) = shape.method_base_index {
			let lock = scope.type_store.user_types.read()[shape_index].clone();
			let guard = lock.read();
			let base_scope_id = guard.scope_id;
			scope.is_extension_method = scope_id != base_scope_id;
		}

		let (is_method, is_mutable) = match method_attribute.item.kind.item {
			MethodKind::ImmutableSelf => (true, false),
			MethodKind::MutableSelf => (true, true),
			MethodKind::Static => (false, false),
		};

		if is_method {
			let type_id = shape.parameters.item[0].type_id;
			let readable_index = scope.readables.push("self", type_id, ReadableKind::Let, is_mutable);
			let kind = SymbolKind::Let { readable_index };
			let symbol = Symbol { name: "self", kind, span: None, used: true, imported: false };

			scope.push_symbol(symbol);
			maybe_self = 1;
		}
	}

	for (index, parameter) in statement.parameters.item.parameters.iter().enumerate() {
		let span = parameter.item.name.span;
		let parameter = &parameter.item;

		let parameter_shape = &shape.parameters.item[index + maybe_self];
		let stored_readable_index = parameter_shape.readable_index;
		let parameter_shape_type_id = parameter_shape.type_id;

		assert_eq!(stored_readable_index, index + maybe_self);
		let kind = match parameter.is_mutable {
			false => ReadableKind::Let,
			true => ReadableKind::Mut,
		};
		let readable_index = scope.readables.push(parameter.name.item, parameter_shape_type_id, kind, true);
		assert_eq!(readable_index, stored_readable_index);
		let kind = match parameter.is_mutable {
			false => SymbolKind::Let { readable_index },
			true => SymbolKind::Mut { readable_index },
		};

		let name = parameter.name.item;
		scope.push_symbol(Symbol { name, kind, span: Some(span), used: false, imported: false });

		let previous = &statement.parameters.item.parameters[..index];
		if let Some(existing) = previous.iter().find(|f| f.item.name.item == name) {
			let error = error!("Duplicate parameter `{name}` of function `{}`", statement.name.item);
			let note = note!(existing.item.name.span, "Original parameter here");
			scope.message(error.span(span).note(note))
		}
	}

	drop(shape);

	let tree_block = &statement.block.as_ref().unwrap().item;
	let block = validate_block(scope, tree_block, false);

	if !return_type.item.is_void(context.type_store) && !block.returns {
		let error = error!("Not all code paths for function `{}` return a value", statement.name.item);
		context.message(error.span(statement.name.span));
	}

	let mut shape = lock.write();
	assert!(shape.block.is_none());
	shape.block = Some(Ref::new(block));

	let mut generic_usages = context.function_generic_usages[initial_generic_usages_len..].to_vec();
	context.function_generic_usages.truncate(initial_generic_usages_len);

	if !generic_usages.is_empty() && !shape.specializations.is_empty() {
		let specializations = shape.specializations.clone();
		drop(shape);

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
					enclosing_generic_parameters,
					function_shape_index,
					&specialization.type_arguments,
					None,
				);
			}
		}
	} else {
		drop(shape);
	}

	let mut shape = lock.write();
	assert!(shape.generic_usages.is_empty());
	shape.generic_usages = SliceRef::from(generic_usages);

	let type_parameters_iter = shape.generic_parameters.explicit_parameters().iter();
	let type_parameter_span = type_parameters_iter.fold(None, |sum, p| match sum {
		Some(sum) => Some(sum + p.name.span),
		None => Some(p.name.span),
	});

	if shape.is_main {
		assert_eq!(shape.generic_parameters.implicit_len(), 0);
		let has_return_type = !shape.return_type.item.is_void(context.type_store);

		let export_span = shape.export_attribute.map(|a| a.span);
		drop(shape);

		let parameters = statement.parameters.item.parameters.iter();
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
			enclosing_generic_parameters,
			function_shape_index,
			None,
			Ref::new(TypeArguments::new_from_explicit(Vec::new())),
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
		drop(shape);

		if let Some(extern_span) = extern_span {
			let message = error!("Method may not have an extern attribute");
			context.message(message.span(extern_span));
		}

		if let Some(export_span) = export_span {
			let message = error!("Method may not have an export attribute");
			context.message(message.span(export_span));
		}
	} else if statement.export_attribute.is_some() {
		drop(shape);
		if let Some(type_parameter_span) = type_parameter_span {
			let message = error!("Exported function may not have any generic type parameters");
			context.message(message.span(type_parameter_span));
		}

		context.function_store.get_or_add_specialization(
			context.messages,
			context.type_store,
			context.module_path,
			context.function_generic_usages,
			enclosing_generic_parameters,
			function_shape_index,
			None,
			Ref::new(TypeArguments::new_from_explicit(Vec::new())),
		);
	} else if shape.lang_attribute.is_some() {
		drop(shape);
		if let Some(type_parameter_span) = type_parameter_span {
			let message = error!("Lang item function may not have any generic type parameters");
			context.message(message.span(type_parameter_span));
		}

		context.function_store.get_or_add_specialization(
			context.messages,
			context.type_store,
			context.module_path,
			context.function_generic_usages,
			enclosing_generic_parameters,
			function_shape_index,
			None,
			Ref::new(TypeArguments::new_from_explicit(Vec::new())),
		);
	}
}

fn validate_const<'a>(context: &mut Context<'a, '_, '_>, statement: &'a tree::Node<tree::Const<'a>>) -> Option<()> {
	let explicit_type = match &statement.item.parsed_type {
		Some(parsed_type) => context.lookup_type(parsed_type),
		None => None,
	};

	let mut expression = validate_expression(context, &statement.item.expression);
	expression.into_value(context);
	if let Some(explicit_type) = explicit_type {
		if !context.collapse_to(explicit_type, &mut expression).ok()? {
			let explicit = context.type_name(explicit_type);
			let expression = context.type_name(expression.type_id);
			let error = error!("Const type mismatch between explicit type {explicit} and expression type {expression}");
			context.message(error.span(statement.span));
			return None;
		}
	}

	let value = match &expression.kind {
		ExpressionKind::NumberValue(value) => ConstantValue::NumberValue(*value),
		ExpressionKind::StringLiteral(literal) => ConstantValue::StringLiteral(literal.value.clone()),
		ExpressionKind::CodepointLiteral(literal) => ConstantValue::CodepointLiteral(literal.value),

		ExpressionKind::AnyCollapse => ConstantValue::AnyCollapse,

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
	let symbol = Symbol { name, kind, span, used: true, imported: false };
	context.push_symbol(symbol);

	Some(())
}

fn validate_static<'a>(context: &mut Context<'a, '_, '_>, statement: &'a tree::Node<tree::Static<'a>>) -> Option<()> {
	if let Some(extern_attribute) = statement.item.extern_attribute {
		let name = extern_attribute.item.name;
		context.externs.write().push(context.messages, name, statement.span);
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
	context.push_symbol(Symbol { name, kind, span, used: true, imported: false });

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
	expression.into_value(&mut scope);
	drop(scope);

	let mut type_id = match &statement.item.parsed_type {
		Some(_) => {
			// Make sure we still take this path even if the parsed type lookup failed
			let explicit_type = explicit_type.unwrap_or(context.type_store.any_collapse_type_id());

			if !context.collapse_to(explicit_type, &mut expression).ok()? {
				let expected = context.type_name(explicit_type);
				let got = context.type_name(expression.type_id);
				let error = error!("Expected {expected} but got expression with type {got}");
				context.message(error.span(statement.item.expression.span));
				context.type_store.any_collapse_type_id()
			} else {
				explicit_type
			}
		}

		_ => expression.type_id,
	};

	if type_id.is_untyped_number(context.type_store) {
		context.message(error!("Cannot create binding of untyped number").span(statement.span));
		type_id = context.type_store.any_collapse_type_id();
	} else if let ExpressionKind::ModuleLayer { .. } = &expression.kind {
		if !expression.type_id.is_any_collapse(context.type_store) {
			context.message(error!("Binding expression must be a value, found a module").span(statement.span));
		}
		type_id = context.type_store.any_collapse_type_id();
	} else if let ExpressionKind::Type { type_id: found, .. } = &expression.kind {
		if !expression.type_id.is_any_collapse(context.type_store) {
			let found = context.type_name(*found);
			context.message(error!("Binding expression must be a value, found type {found}").span(statement.span));
		}
		type_id = context.type_store.any_collapse_type_id();
	}

	let kind = match statement.item.is_mutable {
		true => ReadableKind::Mut,
		false => ReadableKind::Let,
	};
	let readable_index = context.push_readable(statement.item.name, type_id, kind, false, true);

	let name = statement.item.name.item;
	Some(Binding { name, type_id, expression, readable_index })
}

pub fn validate_expression<'a>(
	context: &mut Context<'a, '_, '_>,
	expression: &'a tree::Node<tree::Expression<'a>>,
) -> Expression<'a> {
	let span = expression.span;
	let original_can_is_bind = context.can_is_bind;

	match &expression.item {
		tree::Expression::Block(block) => {
			let mut scope = context.child_scope();
			scope.can_is_bind = false;
			let index = scope.yield_targets.push(scope.expected_type, scope.current_loop_index);
			scope.current_yield_target_index = Some(index);

			let expression = validate_block_expression(&mut scope, block, span, index);
			if !expression.yields {
				let error = error!("Not all code paths for block expression yield a value");
				scope.message(error.span(expression.span));
			}

			expression
		}

		tree::Expression::IfElseChain(chain_expression) => {
			let mut scope = context.child_scope();
			scope.can_is_bind = false;
			let index = scope.yield_targets.push(scope.expected_type, scope.current_loop_index);
			scope.current_yield_target_index = Some(index);

			let expression = validate_if_else_chain_expression(&mut scope, chain_expression, span, Some(index));
			if !expression.yields {
				let error = error!("Not all code paths for if-else expression yield a value");
				scope.message(error.span(expression.span));
			}

			expression
		}

		tree::Expression::Match(match_expression) => {
			let mut scope = context.child_scope();
			scope.can_is_bind = false;
			let index = scope.yield_targets.push(scope.expected_type, scope.current_loop_index);
			scope.current_yield_target_index = Some(index);

			let expression = validate_match_expression(&mut scope, match_expression, span, Some(index));
			if !expression.yields {
				let error = error!("Not all code paths for match expression yield a value");
				scope.message(error.span(expression.span));
			}

			expression
		}

		tree::Expression::NumberLiteral(literal) => validate_number_literal(context, literal, span),

		tree::Expression::BooleanLiteral(literal) => validate_bool_literal(context, *literal, span),

		tree::Expression::CodepointLiteral(literal) => validate_codepoint_literal(context, literal, span),

		tree::Expression::ByteCodepointLiteral(literal) => validate_byte_codepoint_literal(context, literal, span),

		tree::Expression::StringLiteral(literal) => validate_string_literal(context, literal, span),

		tree::Expression::FormatStringLiteral(literal) => validate_format_string_literal(context, literal, span),

		tree::Expression::ArrayLiteral(literal) => {
			context.expected_type = None;
			context.can_is_bind = false;
			let expression = validate_array_literal(context, literal, span);
			context.can_is_bind = original_can_is_bind;
			expression
		}

		tree::Expression::StructLiteral(literal) => {
			context.can_is_bind = false;
			let expression = validate_struct_literal(context, literal, span);
			context.can_is_bind = original_can_is_bind;
			expression
		}

		tree::Expression::Call(call) => {
			context.can_is_bind = false;
			let expression = validate_call(context, call, span);
			context.can_is_bind = original_can_is_bind;
			expression
		}

		tree::Expression::Read(read) => {
			context.can_is_bind = false;
			let expression = validate_read(context, read, span);
			context.can_is_bind = original_can_is_bind;
			expression
		}

		tree::Expression::DotAccess(dot_access) => {
			context.can_is_bind = false;
			let expression = validate_dot_access(context, dot_access, span);
			context.can_is_bind = original_can_is_bind;
			expression
		}

		tree::Expression::DotInfer(dot_infer) => {
			context.can_is_bind = false;
			let expression = validate_dot_infer(context, dot_infer, span);
			context.can_is_bind = original_can_is_bind;
			expression
		}

		tree::Expression::DotInferCall(dot_infer_call) => {
			context.can_is_bind = false;
			let expression = validate_dot_infer_call(context, dot_infer_call, span);
			context.can_is_bind = original_can_is_bind;
			expression
		}

		tree::Expression::UnaryOperation(operation) => {
			context.can_is_bind = false;
			let expression = validate_unary_operation(context, operation, span);
			context.can_is_bind = original_can_is_bind;
			expression
		}

		tree::Expression::BinaryOperation(operation) => {
			// Modifies `can_is_bind` flag internally
			let expression = validate_binary_operation(context, operation, span);
			context.can_is_bind = original_can_is_bind;
			expression
		}

		tree::Expression::CheckIs(check) => validate_check_is(context, check, span),
	}
}

fn validate_block_expression<'a>(
	context: &mut Context<'a, '_, '_>,
	block: &'a tree::Block<'a>,
	span: Span,
	yield_target_index: usize,
) -> Expression<'a> {
	let mut validated_block = validate_block(context.child_scope(), block, false);
	let yields = validated_block.yields;
	validated_block.yield_target_index = Some(yield_target_index);
	let returns = validated_block.returns;
	let type_id = validated_block.type_id;
	let kind = ExpressionKind::Block(validated_block);
	Expression {
		span,
		type_id,
		is_itself_mutable: true,
		is_pointer_access_mutable: true,
		yields,
		returns,
		kind,
		debug_location: span.debug_location(context.parsed_files),
	}
}

fn validate_if_else_chain_expression<'a>(
	context: &mut Context<'a, '_, '_>,
	chain_expression: &'a tree::IfElseChain<'a>,
	span: Span,
	yield_target_index: Option<usize>,
) -> Expression<'a> {
	let mut type_id = context.expected_type;
	let check_body_type_id = |context: &mut Context, type_id: &mut Option<TypeId>, body: &Block, span: Span| {
		if let Some(type_id) = type_id {
			if !context.type_store.direct_match(*type_id, body.type_id) {
				let expected = context.type_name(*type_id);
				let found = context.type_name(body.type_id);
				let error = error!("If-else chain body type mismatch, expected {expected} but found {found}");
				context.message(error.span(span));
			}
		} else {
			*type_id = Some(body.type_id);
		}
	};

	let mut entries = Vec::with_capacity(chain_expression.entries.len());

	let mut first_condition_yields = false;
	let mut all_if_bodies_yield = true;

	let mut first_condition_returns = false;
	let mut all_if_bodies_return = true;

	for (index, entry) in chain_expression.entries.iter().enumerate() {
		let mut scope = context.child_scope();
		scope.can_is_bind = true;
		let mut condition = validate_expression(&mut scope, &entry.condition);
		condition.into_value(&mut scope);
		scope.can_is_bind = false;
		if index == 0 && condition.yields {
			first_condition_yields = true;
		}
		if index == 0 && condition.returns {
			first_condition_returns = true;
		}

		if !condition.type_id.is_bool(scope.type_store) && !condition.type_id.is_any_collapse(scope.type_store) {
			let found = scope.type_name(condition.type_id);
			let error = error!("Expected `if` condition of type `bool`, found {found}");
			scope.message(error.span(condition.span));
		}

		let body = {
			let mut body_scope = scope.child_scope();
			body_scope.expected_type = type_id;

			let body = validate_block(body_scope, &entry.body.item, false);
			all_if_bodies_yield &= body.yields;
			all_if_bodies_return &= body.returns;

			check_body_type_id(&mut scope, &mut type_id, &body, entry.body.span);
			body
		};

		let entry = IfElseChainEntry { condition, body };
		entries.push(entry);
	}

	let mut else_yields = false;
	let mut else_returns = false;
	let else_body = if let Some(else_body) = &chain_expression.else_body {
		let scope = context.child_scope();
		let body = validate_block(scope, &else_body.item, false);
		else_yields = body.yields;
		else_returns = body.returns;
		Some(body)
	} else {
		None
	};

	let type_id = type_id.unwrap();
	let yields = (all_if_bodies_yield && else_yields) || first_condition_yields;
	let returns = (all_if_bodies_return && else_returns) || first_condition_returns;
	let chain = IfElseChain { type_id, yield_target_index, entries, else_body };
	let kind = ExpressionKind::IfElseChain(Box::new(chain));
	Expression {
		span,
		type_id,
		is_itself_mutable: true,
		is_pointer_access_mutable: true,
		yields,
		returns,
		kind,
		debug_location: span.debug_location(context.parsed_files),
	}
}

fn validate_match_expression<'a>(
	context: &mut Context<'a, '_, '_>,
	match_expression: &'a tree::Match<'a>,
	span: Span,
	yield_target_index: Option<usize>,
) -> Expression<'a> {
	let mut expression = validate_expression(context, &match_expression.expression);
	expression.into_value(context);
	if expression.type_id.is_any_collapse(context.type_store) {
		return Expression::any_collapse(context.type_store, span);
	}

	let (expression_type_id, is_mutable) = match expression.type_id.as_pointed(context.type_store) {
		Some(as_pointer) => (as_pointer.type_id, as_pointer.mutable),
		None => (expression.type_id, expression.is_itself_mutable),
	};

	let report_not_enum_error = |context: &mut Context| {
		let found = context.type_name(expression_type_id);
		let error = error!("Cannot match on type {found} as it is not an enum");
		context.messages.message(error.span(expression.span));
	};

	let enum_entry = context.type_store.type_entries.get(expression_type_id);
	let (shape_index, specialization_index) = match enum_entry.kind {
		TypeEntryKind::UserType { shape_index, specialization_index, .. } => (shape_index, specialization_index),

		_ => {
			report_not_enum_error(context);
			return Expression::any_collapse(context.type_store, span);
		}
	};

	let user_type = context.type_store.user_types.read()[shape_index].clone();
	let user_type = user_type.read();
	let enum_specialization = match &user_type.kind {
		UserTypeKind::Enum { shape } => &shape.specializations[specialization_index],

		UserTypeKind::Struct { .. } | UserTypeKind::Union { .. } => {
			report_not_enum_error(context);
			return Expression::any_collapse(context.type_store, span);
		}
	};

	let mut encountered_variants = Vec::<Option<Span>>::with_capacity(enum_specialization.variants_by_name.len());
	for _ in 0..enum_specialization.variants_by_name.len() {
		encountered_variants.push(None);
	}
	drop(user_type);

	let mut arms_yield = true;
	let mut arms_return = true;
	let mut arms = Vec::new();

	for arm in match_expression.arms {
		let mut scope = context.child_scope();
		let enum_entry = scope.type_store.type_entries.get(expression_type_id);

		let (shape_index, specialization_index) = match enum_entry.kind {
			TypeEntryKind::UserType { shape_index, specialization_index, .. } => (shape_index, specialization_index),
			kind => unreachable!("{kind:?}"),
		};

		let user_type = scope.type_store.user_types.read()[shape_index].clone();
		let user_type = user_type.read();
		let enum_specialization = match &user_type.kind {
			UserTypeKind::Enum { shape } => &shape.specializations[specialization_index],
			UserTypeKind::Struct { .. } => unreachable!("struct"),
			UserTypeKind::Union { .. } => unreachable!("union"),
		};

		let variants = enum_specialization.variants.clone();
		let variants_by_name = enum_specialization.variants_by_name.clone();
		drop(user_type);

		let mut variant_infos = Vec::with_capacity(arm.variant_names.len());

		for variant_name in arm.variant_names {
			let (variant_type_id, variant_index) = match variants_by_name.get(variant_name.item) {
				Some(&variant_index) => {
					let variant = &variants[variant_index];
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
				info.type_id
					.as_struct(&mut scope.type_store, |shape, specialization| {
						if shape.is_transparent_variant {
							assert_eq!(specialization.fields.len(), 1);
							specialization.fields.first().unwrap().type_id
						} else {
							info.type_id
						}
					})
					.unwrap()
			} else {
				scope.type_store.any_collapse_type_id()
			};

			let kind = match is_mutable {
				true => ReadableKind::Mut,
				false => ReadableKind::Let,
			};

			let readable_index = scope.push_readable(binding_name, type_id, kind, false, expression.is_pointer_access_mutable);
			Some(ResultBinding { type_id, readable_index })
		} else {
			None
		};

		let block = validate_block(scope.child_scope(), &arm.block.item, false);
		arms_yield &= block.yields;
		arms_return &= block.returns;

		let arm = MatchArm { binding, block, variant_infos };
		arms.push(arm);
	}

	let else_arm = if let Some(else_arm) = &match_expression.else_arm {
		let block = validate_block(context.child_scope(), &else_arm.block.item, false);
		Some(block)
	} else {
		None
	};

	let enum_entry = context.type_store.type_entries.get(expression_type_id);
	let (shape_index, specialization_index) = match enum_entry.kind {
		TypeEntryKind::UserType { shape_index, specialization_index, .. } => (shape_index, specialization_index),
		kind => unreachable!("{kind:?}"),
	};

	let user_type = context.type_store.user_types.read()[shape_index].clone();
	let user_type = user_type.read();
	let enum_specialization = match &user_type.kind {
		UserTypeKind::Enum { shape } => &shape.specializations[specialization_index],
		UserTypeKind::Struct { .. } => unreachable!("struct"),
		UserTypeKind::Union { .. } => unreachable!("union"),
	};
	let variants = enum_specialization.variants.clone();
	drop(user_type);

	let mut all_variants_covered = true;
	for (variant_index, variant) in variants.iter().enumerate() {
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

	if let Some(else_arm) = &match_expression.else_arm {
		if all_variants_covered {
			let warning = warning!("Match expression else arm will never execute, all enum variants are already covered");
			context.message(warning.span(else_arm.else_span));
		}
	}

	let type_id = if let Some(index) = context.current_yield_target_index {
		let any_collapse = context.type_store.any_collapse_type_id();
		context.yield_targets.get(index).type_id.unwrap_or(any_collapse)
	} else {
		context.type_store.void_type_id()
	};

	let yields = expression.yields | arms_yield;
	let returns = expression.returns | arms_return;
	let match_expression = Box::new(Match { type_id, yield_target_index, expression, arms, else_arm });
	let kind = ExpressionKind::Match(match_expression);
	Expression {
		span,
		type_id,
		is_itself_mutable: is_mutable,
		is_pointer_access_mutable: true,
		yields,
		returns,
		kind,
		debug_location: span.debug_location(context.parsed_files),
	}
}

fn validate_while_statement<'a>(context: &mut Context<'a, '_, '_>, statement: &'a Node<tree::While<'a>>) -> While<'a> {
	let mut scope = context.child_scope();
	scope.can_is_bind = true;

	let mut condition = validate_expression(&mut scope, &statement.item.condition);
	condition.into_value(&mut scope);
	if !condition.type_id.is_bool(scope.type_store) && !condition.type_id.is_any_collapse(scope.type_store) {
		let found = scope.type_name(condition.type_id);
		let error = error!("Expected `while` condition of type `bool`, found {found}");
		scope.message(error.span(condition.span));
	}

	scope.current_loop_index = Some(scope.next_loop_index);
	scope.next_loop_index += 1;
	let body = validate_block(scope, &statement.item.body.item, false);

	While { condition, body }
}

fn validate_for_statement<'a>(context: &mut Context<'a, '_, '_>, statement: &'a Node<tree::For<'a>>) -> For<'a> {
	let mut scope = context.child_scope();
	let any_collapse = scope.type_store.any_collapse_type_id();

	let mut initializer = validate_expression(&mut scope, &statement.item.initializer);
	initializer.into_value(&mut scope);
	let slice = initializer.type_id.as_slice(&mut scope.type_store.type_entries);
	let kind = if slice.is_some() {
		match statement.item.iteration_kind.item {
			tree::IterationKind::In => ForKind::InSlice,
			tree::IterationKind::Of => ForKind::OfSlice,
		}
	} else {
		let range_type_id = scope.lang_items.read().range_type.unwrap();
		if scope.type_store.direct_match(initializer.type_id, range_type_id) {
			if statement.item.iteration_kind.item == tree::IterationKind::Of {
				let error = error!("Cannot iterate over `Range` by pointer, use `in` instead");
				scope.message(error.span(statement.item.iteration_kind.span));
			}

			ForKind::Range
		} else {
			if !initializer.type_id.is_any_collapse(scope.type_store) {
				let found = scope.type_name(initializer.type_id);
				let error = error!("Cannot iterate over type {found}, expected a slice or `Range`");
				scope.message(error.span(initializer.span));
			}
			ForKind::AnyCollapse
		}
	};

	let item = match kind {
		ForKind::InSlice => {
			let slice = slice.unwrap();
			let type_id = slice.type_id;
			let readable_index = scope.push_readable(
				statement.item.item,
				type_id,
				ReadableKind::Let,
				false,
				initializer.is_pointer_access_mutable,
			);
			ResultBinding { type_id, readable_index }
		}

		ForKind::OfSlice => {
			let slice = slice.unwrap();
			let type_id = scope.type_store.pointer_to(slice.type_id, slice.mutable);
			let readable_index = scope.push_readable(
				statement.item.item,
				type_id,
				ReadableKind::Let,
				false,
				initializer.is_pointer_access_mutable,
			);
			ResultBinding { type_id, readable_index }
		}

		ForKind::Range => {
			let type_id = scope.type_store.isize_type_id();
			let readable_index = scope.push_readable(statement.item.item, type_id, ReadableKind::Let, false, true);
			ResultBinding { type_id, readable_index }
		}

		ForKind::AnyCollapse => {
			let readable_index = scope.push_readable(statement.item.item, any_collapse, ReadableKind::Mut, false, true);
			ResultBinding { type_id: any_collapse, readable_index }
		}
	};

	let index = if let Some(index) = statement.item.index {
		let type_id = scope.type_store.isize_type_id();
		let readable_index = scope.push_readable(index, type_id, ReadableKind::Let, false, true);
		Some(ResultBinding { type_id, readable_index })
	} else {
		None
	};

	let is_last = if let Some(is_last) = statement.item.is_last {
		if is_last.item == "_" {
			// TODO: This warning is weirdly worded, we can probably do better
			let warning = warning!("Unnecessary `for` \"is last\" binding");
			scope.message(warning.span(is_last.span));
		}

		let type_id = scope.type_store.bool_type_id();
		let readable_index = scope.push_readable(is_last, type_id, ReadableKind::Let, false, true);
		Some(ResultBinding { type_id, readable_index })
	} else {
		None
	};

	scope.current_loop_index = Some(scope.next_loop_index);
	scope.next_loop_index += 1;
	let body = validate_block(scope, &statement.item.body.item, false);

	For { kind, item, index, is_last, initializer, body }
}

fn validate_number_literal<'a>(context: &mut Context<'a, '_, '_>, literal: &tree::NumberLiteral, span: Span) -> Expression<'a> {
	let value = NumberValue::new(literal.value.item, literal.value.span);
	let kind = ExpressionKind::NumberValue(value);
	let type_id = context.type_store.number_type_id();
	Expression {
		span,
		type_id,
		is_itself_mutable: true,
		is_pointer_access_mutable: true,
		yields: false,
		returns: false,
		kind,
		debug_location: span.debug_location(context.parsed_files),
	}
}

fn validate_bool_literal<'a>(context: &mut Context<'a, '_, '_>, literal: bool, span: Span) -> Expression<'a> {
	let type_id = context.type_store.bool_type_id();
	let kind = ExpressionKind::BooleanLiteral(literal);
	Expression {
		span,
		type_id,
		is_itself_mutable: true,
		is_pointer_access_mutable: true,
		yields: false,
		returns: false,
		kind,
		debug_location: span.debug_location(context.parsed_files),
	}
}

fn validate_codepoint_literal<'a>(
	context: &mut Context<'a, '_, '_>,
	literal: &tree::CodepointLiteral,
	span: Span,
) -> Expression<'a> {
	let kind = ExpressionKind::CodepointLiteral(CodepointLiteral { value: literal.value.item });
	let type_id = context.type_store.u32_type_id();
	Expression {
		span,
		type_id,
		is_itself_mutable: true,
		is_pointer_access_mutable: true,
		yields: false,
		returns: false,
		kind,
		debug_location: span.debug_location(context.parsed_files),
	}
}

fn validate_byte_codepoint_literal<'a>(
	context: &mut Context<'a, '_, '_>,
	literal: &tree::ByteCodepointLiteral,
	span: Span,
) -> Expression<'a> {
	let kind = ExpressionKind::ByteCodepointLiteral(ByteCodepointLiteral { value: literal.value.item });
	let type_id = context.type_store.u8_type_id();
	Expression {
		span,
		type_id,
		is_itself_mutable: true,
		is_pointer_access_mutable: true,
		yields: false,
		returns: false,
		kind,
		debug_location: span.debug_location(context.parsed_files),
	}
}

fn validate_string_literal<'a>(
	context: &mut Context<'a, '_, '_>,
	literal: &tree::StringLiteral<'a>,
	span: Span,
) -> Expression<'a> {
	let kind = ExpressionKind::StringLiteral(StringLiteral { value: literal.value.clone() });
	let type_id = context.type_store.string_type_id();
	Expression {
		span,
		type_id,
		is_itself_mutable: false,
		is_pointer_access_mutable: true,
		yields: false,
		returns: false,
		kind,
		debug_location: span.debug_location(context.parsed_files),
	}
}

fn validate_format_string_literal<'a>(
	context: &mut Context<'a, '_, '_>,
	literal: &tree::FormatStringLiteral<'a>,
	span: Span,
) -> Expression<'a> {
	let mut items = Vec::with_capacity(literal.items.len());

	for item in literal.items {
		let item = match item {
			tree::FormatStringItem::Text(text) => FormatStringItem::Text(text.clone()),

			tree::FormatStringItem::Expression(expression) => {
				let mut expression = validate_expression(context, expression);
				expression.into_value(context);
				if !expression.type_id.is_formattable(context.type_store, &expression) {
					let found = context.type_name(expression.type_id);
					let error = error!("Cannot format expression of type {found}");
					// TODO: Add hint system
					// let hint = hint!("Allowed types: `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, `isize`, `usize`, `f32`, `f64`, `bool`, `str`, and `fstr`");
					context.message(error.span(expression.span));
				}

				FormatStringItem::Expression(expression)
			}
		};

		items.push(item);
	}

	let kind = ExpressionKind::FormatStringLiteral(FormatStringLiteral { items });
	let type_id = context.type_store.format_string_type_id();
	Expression {
		span,
		type_id,
		is_itself_mutable: false,
		is_pointer_access_mutable: true,
		yields: false,
		returns: false,
		kind,
		debug_location: span.debug_location(context.parsed_files),
	}
}

fn validate_array_literal<'a>(
	context: &mut Context<'a, '_, '_>,
	literal: &'a tree::ArrayLiteral<'a>,
	span: Span,
) -> Expression<'a> {
	let pointee_type_id = literal.parsed_type.as_ref().map(|parsed_type| {
		context
			.lookup_type(parsed_type)
			.unwrap_or(context.type_store.any_collapse_type_id())
	});
	context.expected_type = pointee_type_id;

	let mut yields = false;
	let mut returns = false;
	let mut expressions = Vec::with_capacity(literal.expressions.len());
	for expression in literal.expressions {
		let mut expression = validate_expression(context, expression);
		expression.into_value(context);
		yields |= expression.yields;
		returns |= expression.returns;
		expressions.push(expression);
	}

	let pointee_type_id = if let Some(pointee_type_id) = pointee_type_id {
		pointee_type_id
	} else if let Some(first) = expressions.first() {
		if first.type_id.is_untyped_number(context.type_store) {
			let error = error!("Cannot infer array type from untyped number first item");
			context.message(error.span(first.span));
			context.type_store.any_collapse_type_id()
		} else {
			first.type_id
		}
	} else {
		context.message(error!("Cannot infer array type from empty array").span(span));
		context.type_store.any_collapse_type_id()
	};

	for expression in &mut expressions {
		let collapsed = context.collapse_to(pointee_type_id, expression);
		if !collapsed.unwrap_or(true) && !pointee_type_id.is_any_collapse(context.type_store) {
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
	Expression {
		span,
		type_id,
		is_itself_mutable: true,
		is_pointer_access_mutable: true,
		yields,
		returns,
		kind,
		debug_location: span.debug_location(context.parsed_files),
	}
}

fn validate_struct_literal<'a>(
	context: &mut Context<'a, '_, '_>,
	literal: &'a tree::StructLiteral<'a>,
	span: Span,
) -> Expression<'a> {
	let base = validate_expression(context, &literal.base);
	let type_id = match base.kind {
		ExpressionKind::Type { type_id, .. } => type_id,

		ExpressionKind::AnyCollapse => return Expression::any_collapse(context.type_store, span),

		kind => {
			let error = error!("Cannot construct {} like a struct", kind.name_with_article());
			context.message(error.span(span));
			return Expression::any_collapse(context.type_store, span);
		}
	};

	let report_not_struct = |context: &mut Context| {
		let name = context.type_name(type_id);
		let message = error!("Cannot construct type {name} like a struct as it is not a struct");
		context.messages.message(message.span(base.span));
	};

	let type_entry = context.type_store.type_entries.get(type_id);
	let (shape_index, specialization_index) = match &type_entry.kind {
		&TypeEntryKind::UserType { shape_index, specialization_index, .. } => (shape_index, specialization_index),

		_ => {
			report_not_struct(context);
			return Expression::any_collapse(context.type_store, span);
		}
	};

	let user_type = context.type_store.user_types.read()[shape_index].clone();
	let user_type = user_type.read();
	let shape = match &user_type.kind {
		UserTypeKind::Struct { shape } => shape,

		_ => {
			report_not_struct(context);
			return Expression::any_collapse(context.type_store, span);
		}
	};

	if shape.opaque {
		drop(user_type);
		let name = context.type_name(type_id);
		let message = error!("Cannot construct type {name} as it is an opaque struct");
		context.messages.message(message.span(base.span));
		// TODO: Unfortunate that we cannot also emit errors for the expressions contained in the initalizer
		return Expression::any_collapse(context.type_store, span);
	}

	let specialization = &shape.specializations[specialization_index];
	if shape.parent_kind == StructParentKind::Union && specialization.fields.is_empty() {
		drop(user_type);
		let variant_name = context.type_name(type_id);
		let message = error!("Cannot construct enum variant {variant_name} with field initializers");
		context.message(message.span(span));
		return Expression::any_collapse(context.type_store, span);
	}

	if shape.is_transparent_variant {
		drop(user_type);
		let variant_name = context.type_name(type_id);
		let message = error!("Cannot construct transparent enum variant {variant_name} like a struck-like enum variant");
		context.message(message.span(span));
		return Expression::any_collapse(context.type_store, span);
	}

	// Hate this clone
	let fields = specialization.fields.clone();
	drop(user_type);

	let mut yields = false;
	let mut returns = false;
	let field_initializers = validate_struct_initializer(
		context,
		shape_index,
		specialization_index,
		type_id,
		&literal.initializer,
		&fields,
		&mut yields,
		&mut returns,
	);

	let kind = ExpressionKind::StructLiteral(StructLiteral { type_id, field_initializers });
	Expression {
		span,
		type_id,
		is_itself_mutable: true,
		is_pointer_access_mutable: true,
		yields,
		returns,
		kind,
		debug_location: span.debug_location(context.parsed_files),
	}
}

fn validate_struct_initializer<'a>(
	context: &mut Context<'a, '_, '_>,
	shape_index: usize,
	specialization_index: usize,
	type_id: TypeId,
	initializer: &'a Node<tree::StructInitializer<'a>>,
	fields: &[Field<'a>],
	yields: &mut bool,
	returns: &mut bool,
) -> Vec<FieldInitializer<'a>> {
	let mut fields = fields.iter();
	let mut field_initializers = Vec::new();

	for intializer in initializer.item.field_initializers {
		let field = fields.next();

		let mut scope = context.child_scope();
		scope.expected_type = field.map(|f| f.type_id);
		let mut expression = validate_expression(&mut scope, &intializer.expression);
		expression.into_value(&mut scope);
		drop(scope);

		*yields |= expression.yields;
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
		let is_internal = matches!(field.attribute, Some(Node { item: FieldAttribute::Internal, .. }));
		let is_readable = matches!(field.attribute, Some(Node { item: FieldAttribute::Readable, .. }));
		let external_access = context.check_is_external_access(shape_index);

		if is_internal && external_access {
			let on = context.type_name(type_id);
			let error = error!("Cannot publicly initialize internal field `{name}` on type {on}",);
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

	let user_type = context.type_store.user_types.read()[shape_index].clone();
	let user_type = user_type.read();
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

#[derive(Debug)]
enum NameOnBase<'a> {
	ModuleLayer(Ref<RwLock<RootLayer<'a>>>),

	Symbol(Symbol<'a>),

	Variant(Variant),

	Method(MethodInfo),

	Field {
		field_index: usize,
		is_itself_mutable: bool,
		is_pointer_access_mutable: bool,
		immutable_reason: Option<FieldReadImmutableReason>,
		field: Field<'a>,
	},
}

fn lookup_name_on_base<'a>(
	context: &mut Context<'a, '_, '_>,
	base: &mut Expression<'a>,
	name: Node<&'a str>,
	is_call: bool,
) -> Option<NameOnBase<'a>> {
	let base_type_id = match &base.kind {
		ExpressionKind::ModuleLayer(layer) => {
			let mut lock = layer.write();
			if let Some(child) = lock.children.get(name.item) {
				return Some(NameOnBase::ModuleLayer(child.clone()));
			}

			let symbol = lock.lookup_root_symbol(context.messages, &[name]);
			return symbol.map(|symbol| NameOnBase::Symbol(symbol));
		}

		ExpressionKind::Type { type_id } => *type_id,

		ExpressionKind::AnyCollapse => return None,

		_ => base.type_id,
	};

	let (type_id, is_itself_mutable, is_pointer_access_mutable) = match base_type_id.as_pointed(context.type_store) {
		Some(p) => (p.type_id, p.mutable, p.mutable && base.is_pointer_access_mutable),
		None => (base_type_id, base.is_itself_mutable, base.is_pointer_access_mutable),
	};
	let entry = context.type_store.type_entries.get(type_id);

	if is_call {
		if let Some(methods_index) = entry.kind.methods_index() {
			let collections = context.type_store.method_collections.read();
			let collection = collections[methods_index].read();
			if let Some(method_index) = collection.methods_by_name.get(name.item).copied() {
				let info = collection.methods[method_index];
				return Some(NameOnBase::Method(info));
			}
		}
	}

	base.into_value(context);

	match entry.kind {
		TypeEntryKind::UserType { shape_index, specialization_index, .. } => {
			let external_access = context.check_is_external_access(shape_index);
			let user_type = context.type_store.user_types.read()[shape_index].clone();
			let user_type = user_type.read();

			let fields = match &user_type.kind {
				UserTypeKind::Struct { shape } => shape.specializations[specialization_index].fields.clone(),

				UserTypeKind::Enum { shape } => {
					if let ExpressionKind::Type { .. } = base.kind {
						let variant = lookup_enum_variant(context, user_type, specialization_index, name, false);
						return variant.map(|variant| NameOnBase::Variant(variant));
					}

					shape.specializations[specialization_index].shared_fields.clone()
				}

				UserTypeKind::Union { shape } => {
					if let ExpressionKind::Type { .. } = base.kind {
						let variant = lookup_union_variant(context, user_type, specialization_index, name, false);
						return variant.map(|variant| NameOnBase::Variant(variant));
					}

					shape.specializations[specialization_index].fields.clone()
				}
			};
			drop(user_type);

			return lookup_field_in_fields(
				context,
				name,
				base_type_id,
				is_itself_mutable,
				is_pointer_access_mutable,
				&fields,
				external_access,
			);
		}

		_ => {}
	}

	if let Some(as_slice) = type_id.as_slice(&mut context.type_store.type_entries) {
		let slice_fields = &[
			Field {
				span: None,
				name: "pointer",
				type_id: context.type_store.pointer_to(as_slice.type_id, as_slice.mutable),
				attribute: None,
				read_only: false,
			},
			Field {
				span: None,
				name: "length",
				type_id: context.type_store.isize_type_id(),
				attribute: None,
				read_only: false,
			},
		];
		let is_pointer_access_mutable = is_pointer_access_mutable && as_slice.mutable;
		return lookup_field_in_fields(
			context,
			name,
			base.type_id,
			is_itself_mutable,
			is_pointer_access_mutable,
			slice_fields,
			false,
		);
	} else if type_id.is_string(context.type_store) {
		let u8_type_id = context.type_store.u8_type_id();
		let str_fields = &[
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
		return lookup_field_in_fields(
			context,
			name,
			base.type_id,
			is_itself_mutable,
			is_pointer_access_mutable,
			str_fields,
			false,
		);
	} else if type_id.is_format_string(context.type_store) {
		let item_type_id = context.lang_items.read().format_string_item_type.unwrap();
		let fstr_fields = &[Field {
			span: None,
			name: "items",
			type_id: context.type_store.slice_of(item_type_id, false),
			attribute: None,
			read_only: true,
		}];
		return lookup_field_in_fields(
			context,
			name,
			base.type_id,
			is_itself_mutable,
			is_pointer_access_mutable,
			fstr_fields,
			false,
		);
	} else if !base.type_id.is_any_collapse(context.type_store) {
		let on = base.kind.name();
		let error = error!("No such thing named `{}` on this {on}", name.item);
		context.message(error.span(base.span + name.span));
	}

	None
}

fn lookup_field_in_fields<'a>(
	context: &mut Context<'a, '_, '_>,
	name: Node<&'a str>,
	base_type_id: TypeId,
	is_itself_mutable: bool,
	is_pointer_access_mutable: bool,
	fields: &[Field<'a>],
	external_access: bool,
) -> Option<NameOnBase<'a>> {
	// TODO: Hashmapify this linear lookup
	let mut fields = fields.iter().enumerate();
	let Some((field_index, field)) = fields.find(|f| f.1.name == name.item) else {
		let type_name = context.type_name(base_type_id);
		let error = error!("No field `{}` on {}", name.item, type_name);
		context.messages.message(error.span(name.span));
		return None;
	};

	let is_internal = matches!(field.attribute, Some(Node { item: FieldAttribute::Internal, .. }));
	let is_readable = matches!(field.attribute, Some(Node { item: FieldAttribute::Readable, .. }));
	let is_read_only = field.read_only;

	if external_access && is_internal {
		let type_name = context.type_name(base_type_id);
		let error = error!("Cannot publicly access internal field `{}` on type {}", name.item, type_name);
		context.messages.message(error.span(name.span));
		return None;
	}

	let (is_itself_mutable, is_pointer_access_mutable, immutable_reason) = if external_access && is_readable {
		(false, true, Some(FieldReadImmutableReason::Readable))
	} else if is_read_only {
		(false, true, Some(FieldReadImmutableReason::ReadOnly))
	} else {
		(is_itself_mutable, is_pointer_access_mutable, None)
	};

	Some(NameOnBase::Field {
		field_index,
		is_itself_mutable,
		is_pointer_access_mutable,
		immutable_reason,
		field: *field,
	})
}

fn validate_call<'a>(context: &mut Context<'a, '_, '_>, call: &'a tree::Call<'a>, span: Span) -> Expression<'a> {
	let mut base = call.base.as_ref().map(|base| validate_expression(context, base));
	let on_base = if let Some(base) = base.as_mut() {
		let Some(on_base) = lookup_name_on_base(context, base, call.name, true) else {
			return Expression::any_collapse(context.type_store, span);
		};

		on_base
	} else {
		match context.lookup_symbol_by_name(call.name) {
			Some(symbol) => NameOnBase::Symbol(symbol),
			None => return Expression::any_collapse(context.type_store, span),
		}
	};

	match on_base {
		NameOnBase::Symbol(symbol) => validate_symbol_call(context, symbol, call, span),

		NameOnBase::Variant(variant) => {
			validate_transparent_variant_initialization(context, call.name.item, variant, call.arguments, span)
		}

		NameOnBase::Method(method_info) => {
			let base = base.unwrap();
			if let ExpressionKind::Type { type_id, .. } = base.kind {
				validate_static_method_call(context, method_info, base, type_id, call, span)
			} else {
				validate_method_call(context, method_info, base, call, span)
			}
		}

		NameOnBase::ModuleLayer(layer) => {
			let message = error!("Cannot call module `{}`", layer.read().name);
			context.message(message.span(call.name.span));
			return Expression::any_collapse(context.type_store, span);
		}

		NameOnBase::Field { field, .. } => {
			let message = error!("Cannot call field `{}`", field.name);
			context.message(message.span(call.name.span));
			return Expression::any_collapse(context.type_store, span);
		}
	}
}

fn validate_symbol_call<'a>(
	context: &mut Context<'a, '_, '_>,
	symbol: Symbol<'a>,
	call: &'a tree::Call<'a>,
	span: Span,
) -> Expression<'a> {
	let SymbolKind::Function { function_shape_index } = symbol.kind else {
		let error = error!("Cannot call `{}`, it is {}", symbol.name, symbol.kind);
		context.message(error.span(span));
		return Expression::any_collapse(context.type_store, span);
	};

	let mut type_argument_lookup_errored = false;
	let mut explicit_arguments = Vec::new();
	for type_argument in call.type_arguments {
		if let Some(type_id) = context.lookup_type(type_argument) {
			explicit_arguments.push(Node::new(type_id, type_argument.span));
		} else {
			type_argument_lookup_errored = true;
		}
	}

	if type_argument_lookup_errored {
		return Expression::any_collapse(context.type_store, span);
	}

	let lock = context.function_store.shapes.read()[function_shape_index]
		.as_ref()
		.unwrap()
		.clone();
	let shape = lock.read();

	if shape.intrinsic_attribute.is_some() {
		if symbol.name == "size_of" {
			if let Some(first) = explicit_arguments.first() {
				if first.item.is_opaque(context.type_store) {
					let name = context.type_name(first.item);
					let error = error!("Opaque struct {name} may not have its size calculated");
					context.message(error.span(span));
				}
			}
		}

		if symbol.name == "alignment_of" {
			if let Some(first) = explicit_arguments.first() {
				if first.item.is_opaque(context.type_store) {
					let name = context.type_name(first.item);
					let error = error!("Opaque struct {name} may not have its alignment calculated");
					context.message(error.span(span));
				}
			}
		}
	}

	let mut type_arguments = TypeArguments::new_from_explicit(explicit_arguments);
	if shape.generic_parameters.implicit_len() != 0 {
		// The only functions with implicit generic parameters are inner functions, and if we have it
		// in scope then that means it must be somewhere within ourselves or our function parent chain
		let count = shape.generic_parameters.implicit_len();
		for parameter in &context.generic_parameters.parameters()[0..count] {
			type_arguments.push_implicit(Node::new(parameter.generic_type_id, parameter.name.span));
		}
	}
	drop(shape);

	let result = context.function_store.get_or_add_specialization(
		context.messages,
		context.type_store,
		context.module_path,
		context.function_generic_usages,
		context.generic_parameters,
		function_shape_index,
		Some(span),
		Ref::new(type_arguments),
	);

	let specialization_index = result.as_ref().map(|result| result.specialization_index);
	let MethodArgumentsResult { mut yields, mut returns, arguments } =
		validate_function_arguments(context, &call.arguments, function_shape_index, specialization_index, span, false);

	let FunctionSpecializationResult { specialization_index, return_type } = match result {
		Some(results) => results,
		None => return Expression::any_collapse(context.type_store, span),
	};

	if return_type.is_noreturn(context.type_store) {
		yields = true;
		returns = true;
	}

	let function_id = FunctionId { function_shape_index, specialization_index };
	let kind = ExpressionKind::Call(Call { span, name: call.name.item, function_id, arguments });
	Expression {
		span,
		type_id: return_type,
		is_itself_mutable: true,
		is_pointer_access_mutable: true,
		yields,
		returns,
		kind,
		debug_location: span.debug_location(context.parsed_files),
	}
}

fn get_method_function_specialization<'a>(
	context: &mut Context<'a, '_, '_>,
	call_type_arguments: &[Node<tree::Type<'a>>],
	function_shape_index: usize,
	method_base_type: MethodBaseType,
	span: Span,
) -> Option<FunctionSpecializationResult> {
	let mut type_argument_lookup_errored = false;
	let mut explicit_arguments = Vec::new();
	for type_argument in call_type_arguments {
		if let Some(type_id) = context.lookup_type(type_argument) {
			explicit_arguments.push(Node::new(type_id, type_argument.span));
		} else {
			type_argument_lookup_errored = true;
		}
	}

	if type_argument_lookup_errored {
		return None;
	}

	context.function_store.get_method_function_specialization(
		context.messages,
		context.type_store,
		context.module_path,
		context.function_generic_usages,
		context.generic_parameters,
		explicit_arguments,
		function_shape_index,
		method_base_type,
		Some(span),
	)
}

struct MethodArgumentsResult<'a> {
	yields: bool,
	returns: bool,
	arguments: Vec<Expression<'a>>,
}

fn validate_function_arguments<'a>(
	context: &mut Context<'a, '_, '_>,
	call_arguments: &'a [tree::Argument<'a>],
	function_shape_index: usize,
	function_specialization_index: Option<usize>,
	span: Span,
	has_self: bool,
) -> MethodArgumentsResult<'a> {
	let maybe_self = if has_self { 1 } else { 0 };
	let lock = context.function_store.shapes.read()[function_shape_index]
		.as_ref()
		.unwrap()
		.clone();

	let mut yields = false;
	let mut returns = false;
	let mut arguments = Vec::with_capacity(call_arguments.len());
	for (index, argument) in call_arguments.iter().enumerate() {
		let parameter_index = index + maybe_self;
		let mut scope = context.child_scope();

		let shape = lock.read();
		let shape_parameter = shape.parameters.item.get(parameter_index);
		if let Some(parameter_label) = shape_parameter.and_then(|parameter| parameter.label) {
			if let Some(argument_label) = argument.label {
				if argument_label.item != parameter_label {
					let message = error!(
						"Expected parameter label `{parameter_label}` for argument, found `{}`",
						argument_label.item
					);
					scope.message(message.span(argument_label.span));
				}
			} else {
				let message = error!("Expected parameter label `{parameter_label}` for argument");
				scope.message(message.span(argument.expression.span));
			}
		} else if let Some(argument_label) = argument.label {
			if !matches!(&argument.expression.item, tree::Expression::Read(_)) {
				let message = error!("Expected no parameter label for argument");
				scope.message(message.span(argument_label.span));
			}
		}

		scope.expected_type = None;
		if let Some(function_specialization_index) = function_specialization_index {
			let specialization = &shape.specializations[function_specialization_index];
			if let Some(parameter) = specialization.parameters.get(parameter_index) {
				scope.expected_type = Some(parameter.type_id)
			}
		}
		drop(shape);

		let mut argument = validate_expression(&mut scope, &argument.expression);
		argument.into_value(&mut scope);
		yields |= argument.yields;
		returns |= argument.returns;
		arguments.push(argument);
	}

	let shape = lock.read();
	let overall_parameter_len = shape.parameters.item.len();
	let selfless_parameter_len = overall_parameter_len - maybe_self;
	let specialization_parameters = function_specialization_index.map(|index| shape.specializations[index].parameters.clone());
	let has_c_varargs = shape.c_varargs.is_some();
	drop(shape);

	if let Some(specialization_parameters) = specialization_parameters {
		for (index, argument) in arguments.iter_mut().enumerate() {
			let parameter_index = index + maybe_self;
			let parameter = match specialization_parameters.get(parameter_index) {
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
			}
		}
	}

	if has_c_varargs {
		if arguments.len() < selfless_parameter_len {
			let message = error!("Expected at least {} arguments, got {}", selfless_parameter_len, arguments.len());
			context.message(message.span(span));
		}

		let remaining_arguments = &arguments[selfless_parameter_len..];
		for argument in remaining_arguments {
			if argument.type_id.is_untyped_number(context.type_store) {
				let message = error!("Cannot pass untyped number as vararg, try casting it to a concrete type first");
				context.message(message.span(argument.span));
			}
		}
	} else if arguments.len() != selfless_parameter_len {
		let expected = selfless_parameter_len;
		let message = error!("Expected {expected} arguments, got {}", arguments.len());
		context.message(message.span(span));
	}

	MethodArgumentsResult { yields, returns, arguments }
}

pub fn get_method_base_type_for_generic<'a>(
	messages: &mut Messages<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	module_path: &'a [String],
	generic_usages: &mut Vec<GenericUsage>,
	generic_parameters: &GenericParameters<'a>,
	kind: &TypeEntryKind,
	generic_type_id: TypeId,
) -> MethodBaseType {
	let constraints = match kind {
		TypeEntryKind::UserTypeGeneric { shape_index, generic_index, .. } => {
			let user_types = type_store.user_types.read();
			let user_type = user_types[*shape_index].read();
			let parameter = &user_type.generic_parameters.parameters()[*generic_index];
			parameter.generic_constraints.clone()
		}

		TypeEntryKind::FunctionGeneric { function_shape_index, generic_index, .. } => {
			let generics = function_store.generics.read();
			let parameters = generics[*function_shape_index].parameters();
			parameters[*generic_index].generic_constraints.clone()
		}

		TypeEntryKind::TraitGeneric { trait_shape_index, generic_index, .. } => {
			let traits = type_store.traits.read();
			let trait_shape = traits[*trait_shape_index].as_ref().unwrap().read();
			let parameter = &trait_shape.generic_parameters.parameters()[*generic_index];
			parameter.generic_constraints.clone()
		}

		_ => unreachable!("{kind:?}"),
	};

	// TODO: It is unfortunate to build this every time, can we cache it?
	let mut trait_ids = Vec::with_capacity(constraints.len());
	for constraint in constraints.iter() {
		if let Some(specialized) = type_store.get_or_add_trait_shape_specialization(
			messages,
			function_store,
			module_path,
			generic_usages,
			generic_parameters,
			constraint.trait_shape_index,
			None,
			constraint.type_arguments.clone(),
		) {
			trait_ids.push(specialized);
		}
	}

	MethodBaseType::Trait {
		trait_ids: SliceRef::from(trait_ids),
		self_type_id: generic_type_id,
	}
}

fn validate_method_call<'a>(
	context: &mut Context<'a, '_, '_>,
	method_info: MethodInfo,
	base: Expression<'a>,
	call: &'a tree::Call<'a>,
	span: Span,
) -> Expression<'a> {
	context.expected_type = None;
	let entry = context.type_store.type_entries.get(base.type_id);
	let (method_base_type, base_mutable) = match entry.kind {
		TypeEntryKind::BuiltinType { kind, .. } if kind == PrimativeKind::AnyCollapse => {
			return Expression::any_collapse(context.type_store, span);
		}

		TypeEntryKind::BuiltinType { .. } => (MethodBaseType::Other, base.is_itself_mutable),

		TypeEntryKind::FunctionGeneric { .. } | TypeEntryKind::UserTypeGeneric { .. } | TypeEntryKind::TraitGeneric { .. } => {
			let method_base_type = get_method_base_type_for_generic(
				context.messages,
				context.type_store,
				context.function_store,
				context.module_path,
				context.function_generic_usages,
				context.generic_parameters,
				&entry.kind,
				base.type_id,
			);
			(method_base_type, base.is_itself_mutable)
		}

		TypeEntryKind::UserType { shape_index, specialization_index, .. } => {
			let method_base_type = MethodBaseType::UserType { shape_index, specialization_index };
			(method_base_type, base.is_itself_mutable)
		}

		TypeEntryKind::Pointer { type_id, mutable } => {
			let entry = context.type_store.type_entries.get(type_id);
			let method_base_type = match entry.kind {
				TypeEntryKind::UserType { shape_index, specialization_index, .. } => {
					MethodBaseType::UserType { shape_index, specialization_index }
				}

				TypeEntryKind::FunctionGeneric { .. }
				| TypeEntryKind::UserTypeGeneric { .. }
				| TypeEntryKind::TraitGeneric { .. } => get_method_base_type_for_generic(
					context.messages,
					context.type_store,
					context.function_store,
					context.module_path,
					context.function_generic_usages,
					context.generic_parameters,
					&entry.kind,
					type_id,
				),

				_ => MethodBaseType::Other,
			};

			(method_base_type, mutable && base.is_pointer_access_mutable)
		}

		TypeEntryKind::Module | TypeEntryKind::Type | TypeEntryKind::Slice(_) => {
			let found = context.type_name(base.type_id);
			let error = error!("Cannot call method on type {found}");
			context.message(error.span(span));
			return Expression::any_collapse(context.type_store, span);
		}
	};

	let name = call.name.item;
	let function_shape_index = method_info.function_shape_index;
	let mutable_self = match method_info.kind.item {
		MethodKind::ImmutableSelf => false,
		MethodKind::MutableSelf => true,

		MethodKind::Static => {
			let error = error!("Cannot call static method `{name}` on an object instance");
			context.messages.message(error.span(span));
			return Expression::any_collapse(context.type_store, span);
		}
	};

	if mutable_self && !base_mutable {
		let error = error!("Cannot call mutable method `{name}` on an immutable object");
		context.message(error.span(span));
	}

	let result = get_method_function_specialization(context, &call.type_arguments, function_shape_index, method_base_type, span);

	let specialization_index = result.as_ref().map(|result| result.specialization_index);
	let MethodArgumentsResult { mut yields, mut returns, arguments } =
		validate_function_arguments(context, &call.arguments, function_shape_index, specialization_index, span, true);

	let FunctionSpecializationResult { specialization_index, return_type } = match result {
		Some(results) => results,
		None => return Expression::any_collapse(context.type_store, span),
	};

	let function_id = FunctionId { function_shape_index, specialization_index };
	let method_call = MethodCall { base, function_id, arguments };

	if return_type.is_noreturn(context.type_store) {
		yields = true;
		returns = true;
	}

	let kind = ExpressionKind::MethodCall(Box::new(method_call));
	Expression {
		span,
		type_id: return_type,
		is_itself_mutable: true,
		is_pointer_access_mutable: true,
		yields,
		returns,
		kind,
		debug_location: span.debug_location(context.parsed_files),
	}
}

fn validate_static_method_call<'a>(
	context: &mut Context<'a, '_, '_>,
	method_info: MethodInfo,
	mut base_expression: Expression<'a>,
	base_type_id: TypeId,
	call: &'a tree::Call<'a>,
	span: Span,
) -> Expression<'a> {
	context.expected_type = None;
	let entry = context.type_store.type_entries.get(base_type_id);
	let method_base_type = match entry.kind {
		TypeEntryKind::BuiltinType { kind, .. } if kind == PrimativeKind::AnyCollapse => {
			return Expression::any_collapse(context.type_store, span);
		}

		TypeEntryKind::BuiltinType { .. } => MethodBaseType::Other,

		TypeEntryKind::UserType { shape_index, specialization_index, .. } => {
			MethodBaseType::UserType { shape_index, specialization_index }
		}

		// TODO: Allow static methods on traits
		_ => {
			let found = context.type_name(base_type_id);
			let error = error!("Cannot call static method on type {found}");
			context.message(error.span(span));
			return Expression::any_collapse(context.type_store, span);
		}
	};

	let name = call.name.item;
	match method_info.kind.item {
		MethodKind::ImmutableSelf | MethodKind::MutableSelf => {
			if base_expression.into_value(context) {
				return validate_method_call(context, method_info, base_expression, call, span);
			} else {
				let error = error!("Cannot call instance method `{name}` statically");
				context.messages.message(error.span(span));
				return Expression::any_collapse(context.type_store, span);
			}
		}

		MethodKind::Static => {}
	};
	let function_shape_index = method_info.function_shape_index;

	let result = get_method_function_specialization(context, &call.type_arguments, function_shape_index, method_base_type, span);

	let specialization_index = result.as_ref().map(|result| result.specialization_index);
	let MethodArgumentsResult { mut yields, mut returns, arguments } =
		validate_function_arguments(context, &call.arguments, function_shape_index, specialization_index, span, false);

	let FunctionSpecializationResult { specialization_index, return_type } = match result {
		Some(results) => results,
		None => return Expression::any_collapse(context.type_store, span),
	};

	let function_id = FunctionId { function_shape_index, specialization_index };
	let call = Call { span, name, function_id, arguments };

	if return_type.is_noreturn(context.type_store) {
		yields = true;
		returns = true;
	}

	let kind = ExpressionKind::Call(call);
	Expression {
		span,
		type_id: return_type,
		is_itself_mutable: true,
		is_pointer_access_mutable: true,
		yields,
		returns,
		kind,
		debug_location: span.debug_location(context.parsed_files),
	}
}

fn validate_read<'a>(context: &mut Context<'a, '_, '_>, read: &tree::Read<'a>, span: Span) -> Expression<'a> {
	let Some(symbol) = context.lookup_symbol_by_name(read.name) else {
		return Expression::any_collapse(context.type_store, span);
	};

	validate_symbol_read(context, symbol, read.type_arguments, span)
}

fn validate_dot_access<'a>(context: &mut Context<'a, '_, '_>, dot_access: &'a tree::DotAccess<'a>, span: Span) -> Expression<'a> {
	let mut base = validate_expression(context, &dot_access.base);
	let Some(on_base) = lookup_name_on_base(context, &mut base, dot_access.name, false) else {
		return Expression::any_collapse(context.type_store, span);
	};

	let has_type_arguments = !dot_access.type_arguments.is_empty();
	let disallow_type_arguments = |context: &mut Context, label: &str| {
		if has_type_arguments {
			let error = error!("Type arguments not permitted on {label}");
			context.message(error.span(span));
		}
	};

	match on_base {
		NameOnBase::ModuleLayer(layer) => {
			disallow_type_arguments(context, "a module");
			let kind = ExpressionKind::ModuleLayer(layer);
			return Expression {
				span,
				type_id: context.type_store.module_type_id(),
				is_itself_mutable: false,
				is_pointer_access_mutable: false,
				yields: false,
				returns: false,
				kind,
				debug_location: span.debug_location(context.parsed_files),
			};
		}

		NameOnBase::Symbol(symbol) => return validate_symbol_read(context, symbol, dot_access.type_arguments, span),

		NameOnBase::Variant(variant) => {
			disallow_type_arguments(context, "a variant");
			let kind = ExpressionKind::Type { type_id: variant.type_id };
			return Expression {
				span,
				type_id: context.type_store.type_type_id(),
				is_itself_mutable: false,
				is_pointer_access_mutable: false,
				yields: false,
				returns: false,
				kind,
				debug_location: span.debug_location(context.parsed_files),
			};
		}

		NameOnBase::Method(..) => {
			let message = error!("Cannot dot-access method `{}`", dot_access.name.item);
			context.message(message.span(dot_access.name.span));
			return Expression::any_collapse(context.type_store, span);
		}

		NameOnBase::Field {
			field_index,
			is_itself_mutable,
			is_pointer_access_mutable,
			immutable_reason,
			field,
		} => {
			disallow_type_arguments(context, "a field");

			let kind = ExpressionKind::FieldRead(Box::new(FieldRead {
				base,
				name: dot_access.name.item,
				field_index,
				immutable_reason,
			}));

			let field_entry = context.type_store.type_entries.get(field.type_id);
			let type_id = match field_entry.kind {
				TypeEntryKind::Pointer { type_id: pointed, .. } => {
					if is_pointer_access_mutable {
						field.type_id
					} else {
						context.type_store.pointer_to(pointed, false)
					}
				}

				TypeEntryKind::Slice(slice) => {
					if is_pointer_access_mutable {
						field.type_id
					} else {
						context.type_store.slice_of(slice.type_id, false)
					}
				}

				_ => field.type_id,
			};

			return Expression {
				span,
				type_id,
				is_itself_mutable,
				is_pointer_access_mutable,
				yields: false,
				returns: false,
				kind,
				debug_location: span.debug_location(context.parsed_files),
			};
		}
	}
}

fn validate_symbol_read<'a>(
	context: &mut Context<'a, '_, '_>,
	symbol: Symbol<'a>,
	type_arguments: &'a [Node<tree::Type<'a>>],
	span: Span,
) -> Expression<'a> {
	let has_type_arguments = !type_arguments.is_empty();
	let disallow_type_arguments = |context: &mut Context, label: &str| {
		if has_type_arguments {
			let error = error!("Type arguments not permitted on {label}");
			context.message(error.span(span));
		}
	};

	let readable_index = match symbol.kind {
		SymbolKind::Let { readable_index } | SymbolKind::Mut { readable_index } => readable_index,

		SymbolKind::Const { constant_index } => {
			disallow_type_arguments(context, "a const read");

			let constant = &context.constants.read()[constant_index];
			let (kind, type_id) = match constant {
				ConstantValue::NumberValue(value) => {
					let kind = ExpressionKind::NumberValue(*value);
					(kind, value.collapsed().unwrap_or(context.type_store.number_type_id()))
				}

				ConstantValue::CodepointLiteral(value) => {
					let kind = ExpressionKind::CodepointLiteral(CodepointLiteral { value: *value });
					(kind, context.type_store.u32_type_id())
				}

				ConstantValue::StringLiteral(value) => {
					let kind = ExpressionKind::StringLiteral(StringLiteral { value: value.clone() });
					(kind, context.type_store.string_type_id())
				}

				ConstantValue::AnyCollapse => return Expression::any_collapse(context.type_store, span),
			};

			return Expression {
				span,
				type_id,
				is_itself_mutable: false,
				is_pointer_access_mutable: true,
				yields: false,
				returns: false,
				kind,
				debug_location: span.debug_location(context.parsed_files),
			};
		}

		SymbolKind::Static { static_index } => {
			disallow_type_arguments(context, "a static read");

			let static_instance = &context.statics.read().statics[static_index];
			let static_read = StaticRead { static_index };

			let type_id = static_instance.type_id;
			let kind = ExpressionKind::StaticRead(static_read);
			return Expression {
				span,
				type_id,
				is_itself_mutable: false,
				is_pointer_access_mutable: true,
				yields: false,
				returns: false,
				kind,
				debug_location: span.debug_location(context.parsed_files),
			};
		}

		SymbolKind::BuiltinType { type_id, .. } => {
			disallow_type_arguments(context, "a builtin type");

			if type_id.is_void(context.type_store) {
				return Expression::void(context.type_store, context.parsed_files, span);
			} else {
				let kind = ExpressionKind::Type { type_id };
				return Expression {
					span,
					type_id: context.type_store.type_type_id(),
					is_itself_mutable: false,
					is_pointer_access_mutable: false,
					yields: false,
					returns: false,
					kind,
					debug_location: span.debug_location(context.parsed_files),
				};
			}
		}

		SymbolKind::UserType { shape_index, .. } => {
			let Some(type_id) = context.type_store.get_or_add_shape_specialization_in_scope(
				context.messages,
				context.function_store,
				context.module_path,
				context.function_generic_usages,
				context.root_layers,
				context.symbols_scope.symbols,
				shape_index,
				Some(span),
				context.function_initial_symbols_length,
				context.generic_parameters,
				type_arguments,
			) else {
				return Expression::any_collapse(context.type_store, span);
			};

			let kind = ExpressionKind::Type { type_id };
			return Expression {
				span,
				type_id: context.type_store.type_type_id(),
				is_itself_mutable: false,
				is_pointer_access_mutable: false,
				yields: false,
				returns: false,
				kind,
				debug_location: span.debug_location(context.parsed_files),
			};
		}

		SymbolKind::Module { layer } => {
			return Expression {
				span,
				type_id: context.type_store.module_type_id(),
				is_itself_mutable: false,
				is_pointer_access_mutable: false,
				yields: false,
				returns: false,
				kind: ExpressionKind::ModuleLayer(layer),
				debug_location: span.debug_location(context.parsed_files),
			};
		}

		kind => {
			context.message(error!("Cannot read value from {kind}").span(span));
			return Expression::any_collapse(context.type_store, span);
		}
	};

	let Some(readable) = context.readables.get(readable_index) else {
		panic!("Symbol pointed to unknown readable index {readable_index}, {symbol:?}");
	};

	let is_itself_mutable = readable.kind == ReadableKind::Mut;
	let is_pointer_access_mutable = readable.is_pointer_access_mutable;
	let read = Read { name: readable.name, readable_index };

	let type_id = readable.type_id;
	let kind = ExpressionKind::Read(read);
	Expression {
		span,
		type_id,
		is_itself_mutable,
		is_pointer_access_mutable,
		yields: false,
		returns: false,
		kind,
		debug_location: span.debug_location(context.parsed_files),
	}
}

fn infer_variant<'a>(context: &mut Context<'a, '_, '_>, name: Node<&'a str>, span: Span) -> Option<Variant> {
	let Some(expected_type) = context.expected_type else {
		let error = error!("Cannot infer base type for variant, there is no specific expected type in this context");
		context.message(error.span(span));
		return None;
	};

	let entry = context.type_store.type_entries.get(expected_type);
	if let TypeEntryKind::UserType { shape_index, specialization_index, .. } = entry.kind {
		let lock = context.type_store.user_types.read()[shape_index].clone();
		let user_type = lock.read();

		match &user_type.kind {
			UserTypeKind::Enum { .. } => return lookup_enum_variant(context, user_type, specialization_index, name, true),
			UserTypeKind::Union { .. } => return lookup_union_variant(context, user_type, specialization_index, name, true),
			_ => (),
		}
	}

	let error = error!("Cannot infer base type for variant, the expected type is not an enum or union");
	context.messages.message(error.span(span));
	None
}

fn lookup_enum_variant<'a>(
	context: &mut Context<'a, '_, '_>,
	user_type: ReadGuard<'_, UserType<'a>>,
	specialization_index: usize,
	name: Node<&'a str>,
	is_inferred: bool,
) -> Option<Variant> {
	let UserTypeKind::Enum { shape } = &user_type.kind else {
		return None;
	};

	let specialization = &shape.specializations[specialization_index];
	let specialization_type_id = specialization.type_id;

	let Some(&variant_index) = specialization.variants_by_name.get(name.item) else {
		drop(user_type);
		let enum_name = context.type_name(specialization_type_id);
		let error = if is_inferred {
			error!("Expected enum {enum_name} has no variant named `{}`", name.item)
		} else {
			error!("Enum {enum_name} has no variant named `{}`", name.item)
		};
		context.messages.message(error.span(name.span));
		return None;
	};

	return Some(specialization.variants[variant_index]);
}

fn lookup_union_variant<'a>(
	context: &mut Context<'a, '_, '_>,
	user_type: ReadGuard<'_, UserType<'a>>,
	specialization_index: usize,
	name: Node<&'a str>,
	is_inferred: bool,
) -> Option<Variant> {
	let UserTypeKind::Union { shape } = &user_type.kind else {
		return None;
	};

	let specialization = &shape.specializations[specialization_index];
	let specialization_type_id = specialization.type_id;

	let Some(&variant_index) = specialization.variants_by_name.get(name.item) else {
		drop(user_type);
		let union_name = context.type_name(specialization_type_id);
		let error = if is_inferred {
			error!("Expected union {union_name} has no variant named `{}`", name.item)
		} else {
			error!("Union {union_name} has no variant named `{}`", name.item)
		};
		context.messages.message(error.span(name.span));
		return None;
	};

	return Some(specialization.variants[variant_index]);
}

fn validate_dot_infer<'a>(context: &mut Context<'a, '_, '_>, dot_infer: &'a tree::DotInfer<'a>, span: Span) -> Expression<'a> {
	let Some(inferred_variant) = infer_variant(context, dot_infer.name, span) else {
		return Expression::any_collapse(context.type_store, span);
	};

	return Expression {
		span,
		type_id: context.type_store.type_type_id(),
		is_itself_mutable: true,
		is_pointer_access_mutable: true,
		yields: false,
		returns: false,
		kind: ExpressionKind::Type { type_id: inferred_variant.type_id },
		debug_location: span.debug_location(context.parsed_files),
	};
}

fn validate_dot_infer_call<'a>(
	context: &mut Context<'a, '_, '_>,
	dot_infer_call: &'a tree::DotInferCall<'a>,
	span: Span,
) -> Expression<'a> {
	let Some(inferred_variant) = infer_variant(context, dot_infer_call.name, span) else {
		return Expression::any_collapse(context.type_store, span);
	};

	validate_transparent_variant_initialization(
		context,
		dot_infer_call.name.item,
		inferred_variant,
		dot_infer_call.arguments,
		span,
	)
}

fn validate_transparent_variant_initialization<'a>(
	context: &mut Context<'a, '_, '_>,
	name: &str,
	variant: Variant,
	arguments: &'a [tree::Argument<'a>],
	span: Span,
) -> Expression<'a> {
	if !variant.is_transparent {
		let found = context.type_name(variant.type_id);
		let message = error!("Cannot construct struct-like variant {found} like a transparent variant");
		context.message(message.span(span));
		return Expression::any_collapse(context.type_store, span);
	}

	let expected_type_id = variant
		.type_id
		.as_struct(&mut context.type_store, |_, specialization| {
			specialization.fields.first().unwrap().type_id
		})
		.unwrap();

	let [argument] = arguments else {
		let count = arguments.len();
		let message = error!("Transparent variant `{name}` must be constructed with one value, found {count}");
		context.message(message.span(span));
		return Expression::any_collapse(context.type_store, span);
	};

	// This is such a silly "oh well, I guess we have to detect this"
	// It'd be so much better if transparent variant initialization didn't have to share a parser codepath with function calling
	if let Some(label) = argument.label {
		if !matches!(&argument.expression.item, tree::Expression::Read(_)) {
			let message = error!("Transparent variant initialization expression may not have a label");
			context.message(message.span(label.span));
		}
	}

	let mut scope = context.child_scope();
	scope.expected_type = Some(expected_type_id);
	let mut expression = validate_expression(&mut scope, &argument.expression);
	expression.into_value(&mut scope);
	drop(scope);

	let yields = expression.yields;
	let returns = expression.returns;

	if !context.collapse_to(expected_type_id, &mut expression).unwrap_or(true) {
		if !expected_type_id.is_any_collapse(context.type_store) {
			let expected = context.type_name(expected_type_id);
			let found = context.type_name(expression.type_id);
			let error = error!("Transparent variant initializer type mismatch, expected {expected} but got {found} instead");
			context.message(error.span(expression.span));
			return Expression::any_collapse(context.type_store, span);
		}
	}

	let initializer = FieldInitializer { expression };
	let field_initializers = vec![initializer];
	let literal = StructLiteral { type_id: variant.type_id, field_initializers };
	let kind = ExpressionKind::StructLiteral(literal);
	Expression {
		span,
		type_id: variant.type_id,
		is_itself_mutable: true,
		is_pointer_access_mutable: true,
		yields,
		returns,
		kind,
		debug_location: span.debug_location(context.parsed_files),
	}
}

fn validate_unary_operation<'a>(
	context: &mut Context<'a, '_, '_>,
	operation: &'a tree::UnaryOperation<'a>,
	span: Span,
) -> Expression<'a> {
	let mut expression = validate_expression(context, &operation.expression);
	expression.into_value(context);
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
		(UnaryOperator::Negate, ExpressionKind::NumberValue(value)) => {
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
	let yields = expression.yields;
	let returns = expression.returns;

	if matches!(op, UnaryOperator::AddressOf | UnaryOperator::AddressOfMut) {
		if type_id.is_untyped_number(context.type_store) {
			let error = error!("Cannot take address of untyped number, try casting it to a concrete type first");
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
			return Expression {
				span,
				type_id,
				is_itself_mutable: true,
				is_pointer_access_mutable: true,
				yields,
				returns,
				kind,
				debug_location: span.debug_location(context.parsed_files),
			};
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
			return Expression {
				span,
				type_id,
				is_itself_mutable: true,
				is_pointer_access_mutable: true,
				yields,
				returns,
				kind,
				debug_location: span.debug_location(context.parsed_files),
			};
		}

		UnaryOperator::AddressOf => {
			let type_id = context.type_store.pointer_to(type_id, false);
			let kind = ExpressionKind::UnaryOperation(Box::new(UnaryOperation { op, type_id, expression }));
			return Expression {
				span,
				type_id,
				is_itself_mutable: true,
				is_pointer_access_mutable: false,
				yields,
				returns,
				kind,
				debug_location: span.debug_location(context.parsed_files),
			};
		}

		UnaryOperator::AddressOfMut => {
			if !expression.is_itself_mutable {
				let error = error!("Cannot take mutable address of immutable value");
				context.message(error.span(span));
				return Expression::any_collapse(context.type_store, span);
			}

			let type_id = context.type_store.pointer_to(type_id, true);
			let kind = ExpressionKind::UnaryOperation(Box::new(UnaryOperation { op, type_id, expression }));
			return Expression {
				span,
				type_id,
				is_itself_mutable: true,
				is_pointer_access_mutable: true,
				yields,
				returns,
				kind,
				debug_location: span.debug_location(context.parsed_files),
			};
		}

		UnaryOperator::Dereference => {
			let Some((type_id, is_mutable)) = context.type_store.pointed_to(type_id) else {
				let error = error!("Cannot dereference {} as it is not a pointer", context.type_name(type_id));
				context.message(error.span(span));
				return Expression::any_collapse(context.type_store, span);
			};

			let is_mutable = is_mutable && expression.is_pointer_access_mutable;
			let kind = ExpressionKind::UnaryOperation(Box::new(UnaryOperation { op, type_id, expression }));
			return Expression {
				span,
				type_id,
				is_itself_mutable: is_mutable,
				is_pointer_access_mutable: is_mutable,
				yields,
				returns,
				kind,
				debug_location: span.debug_location(context.parsed_files),
			};
		}

		UnaryOperator::Cast { .. } | UnaryOperator::Index { .. } | UnaryOperator::RangeIndex { .. } => unreachable!(),
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
		let from_untyped = from_type_id.is_untyped_number(context.type_store);
		let to_untyped = to_type_id.is_untyped_number(context.type_store);
		if from_untyped && !to_untyped {
			_ = context.collapse_to(to_type_id, &mut expression);
		}
	} else if from_pointer && to_pointer {
	} else if from_pointer && to_numeric {
	} else if from_numeric && to_pointer {
		if matches!(&expression.kind, ExpressionKind::NumberValue(number) if !number.is_integer()) {
			let error = error!("Cannot cast untyped number decimal to a pointer");
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
			let is_untyped_number = from_type_id.is_untyped_number(context.type_store);

			if !is_i64 && !is_u64 && !is_isize && !is_usize && !is_untyped_number {
				let error = error!("Cannot cast {} to a pointer as it is too small", context.type_name(from_type_id));
				context.message(error.span(span));
			} else if from_type_id.is_untyped_number(context.type_store) {
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

	let is_pointer_access_mutable = match to_type_id.as_pointed(context.type_store) {
		Some(pointed) => pointed.mutable,
		None => true,
	};

	let type_id = to_type_id;
	let yields = expression.yields;
	let returns = expression.returns;
	let op = UnaryOperator::Cast { type_id };
	let kind = ExpressionKind::UnaryOperation(Box::new(UnaryOperation { op, type_id, expression }));
	Expression {
		span,
		type_id,
		is_itself_mutable: true,
		is_pointer_access_mutable,
		yields,
		returns,
		kind,
		debug_location: span.debug_location(context.parsed_files),
	}
}

fn validate_bracket_index<'a>(
	context: &mut Context<'a, '_, '_>,
	expression: Expression<'a>,
	index_expression: &'a Node<tree::Expression<'a>>,
	span: Span,
) -> Expression<'a> {
	let mut index_expression = validate_expression(context, index_expression);
	index_expression.into_value(context);
	let range_type_id = context.lang_items.read().range_type.unwrap();
	let is_range = context.type_store.direct_match(index_expression.type_id, range_type_id);

	let sliced_of = context.type_store.sliced_of(expression.type_id);
	let (type_id, is_mutable) = if let Some((sliced, sliced_mutable)) = sliced_of {
		let is_pointer_access_mutable = sliced_mutable && expression.is_pointer_access_mutable;

		if is_range {
			let type_id = context.type_store.slice_of(sliced, sliced_mutable);
			let yields = expression.yields || index_expression.yields;
			let returns = expression.returns || index_expression.returns;
			let op = UnaryOperator::RangeIndex { index_expression };
			let kind = ExpressionKind::UnaryOperation(Box::new(UnaryOperation { op, type_id, expression }));
			return Expression {
				span,
				type_id,
				is_itself_mutable: sliced_mutable,
				is_pointer_access_mutable,
				yields,
				returns,
				kind,
				debug_location: span.debug_location(context.parsed_files),
			};
		}

		(sliced, is_pointer_access_mutable)
	} else if expression.type_id.is_string(context.type_store) {
		if is_range {
			let type_id = context.type_store.string_type_id();
			let yields = expression.yields || index_expression.yields;
			let returns = expression.returns || index_expression.returns;
			let op = UnaryOperator::RangeIndex { index_expression };
			let kind = ExpressionKind::UnaryOperation(Box::new(UnaryOperation { op, type_id, expression }));
			return Expression {
				span,
				type_id,
				is_itself_mutable: false,
				is_pointer_access_mutable: false,
				yields,
				returns,
				kind,
				debug_location: span.debug_location(context.parsed_files),
			};
		}

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

	let yields = expression.yields || index_expression.yields;
	let returns = expression.returns || index_expression.returns;
	let op = UnaryOperator::Index { index_expression };
	let kind = ExpressionKind::UnaryOperation(Box::new(UnaryOperation { op, type_id, expression }));
	Expression {
		span,
		type_id,
		is_itself_mutable: is_mutable,
		is_pointer_access_mutable: is_mutable,
		yields,
		returns,
		kind,
		debug_location: span.debug_location(context.parsed_files),
	}
}

fn validate_binary_operation<'a>(
	context: &mut Context<'a, '_, '_>,
	operation: &'a tree::BinaryOperation<'a>,
	span: Span,
) -> Expression<'a> {
	let mut op = operation.op.item;

	let original_can_is_bind = context.can_is_bind;
	if op != BinaryOperator::LogicalAnd {
		context.can_is_bind = false;
	}

	let mut left = validate_expression(context, &operation.left);
	left.into_value(context);

	let original_expected_type = context.expected_type;
	if op == BinaryOperator::Assign {
		context.expected_type = Some(left.type_id);
	}

	let mut right = validate_expression(context, &operation.right);
	right.into_value(context);

	context.can_is_bind = original_can_is_bind;
	context.expected_type = original_expected_type;

	if let BinaryOperator::Range = op {
		let isize_type_id = context.type_store.isize_type_id();
		let mut had_error = false;

		if !context.collapse_to(isize_type_id, &mut left).unwrap_or(true) {
			let found = context.type_name(left.type_id);
			let error = error!("Cannot create a range with a left value of type {found}, expected `isize`");
			context.message(error.span(left.span));
			had_error = true;
		}

		if !context.collapse_to(isize_type_id, &mut right).unwrap_or(true) {
			let found = context.type_name(right.type_id);
			let error = error!("Cannot create a range with a right value of type {found}, expected `isize`");
			context.message(error.span(right.span));
			had_error = true;
		}

		if had_error {
			return Expression::any_collapse(context.type_store, span);
		}

		let type_id = context.lang_items.read().range_type.unwrap();
		let yields = left.yields || right.yields;
		let returns = left.returns || right.returns;
		let operation = Box::new(BinaryOperation { op, left, right, type_id });
		let kind = ExpressionKind::BinaryOperation(operation);
		return Expression {
			span,
			type_id,
			is_itself_mutable: true,
			is_pointer_access_mutable: true,
			yields,
			returns,
			kind,
			debug_location: span.debug_location(context.parsed_files),
		};
	}

	let collapsed = context.collapse_fair(&mut left, &mut right);
	let Ok(collapsed) = collapsed else {
		let left = context.type_name(left.type_id);
		let right = context.type_name(right.type_id);
		let error = error!("{} type mismatch", op.name())
			.note(note!(operation.left.span, "Left type {left}"))
			.note(note!(operation.right.span, "Right type {right}"));
		context.message(error.span(span));
		return Expression::any_collapse(context.type_store, span);
	};

	if let Some(constant_operation) = perform_constant_binary_operation(context, &left, &right, op) {
		return constant_operation;
	}

	if !left.type_id.is_any_collapse(context.type_store) {
		match op {
			BinaryOperator::Modulo | BinaryOperator::ModuloAssign => {
				if !left.type_id.is_integer(context.type_store, &left) {
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
				if !left.type_id.is_bool(context.type_store) && !left.type_id.is_integer(context.type_store, &left) {
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
				if !left.type_id.is_integer(context.type_store, &left) {
					let found = context.type_name(left.type_id);
					let error = error!("Cannot perform bitshift on non-integer type {found}");
					context.message(error.span(span));
					return Expression::any_collapse(context.type_store, span);
				}
			}

			BinaryOperator::Equals | BinaryOperator::NotEquals => {
				if !left.type_id.is_comparable(context.type_store) {
					let found = context.type_name(left.type_id);
					let error = error!("Cannot perform equality comparison on non-comparable type {found}");
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

			BinaryOperator::LogicalAnd | BinaryOperator::LogicalIsAnd | BinaryOperator::LogicalOr => {
				let boolean = context.type_store.bool_type_id();
				if !context.type_store.direct_match(left.type_id, boolean) {
					let found = context.type_name(left.type_id);
					let error = error!("Cannot perform logical operation on non-boolean type {found}");
					context.message(error.span(span));
					return Expression::any_collapse(context.type_store, span);
				}
			}

			BinaryOperator::Assign | BinaryOperator::Range => {}
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
			} else if let ExpressionKind::StaticRead(_) = &left.kind {
			} else if let ExpressionKind::FieldRead(field_read) = &left.kind {
				if !left.is_itself_mutable {
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
				if !left.is_itself_mutable {
					context.message(error!("Cannot assign to immutable memory location").span(span));
				}
			} else if matches!(&left.kind, ExpressionKind::UnaryOperation(op) if matches!(op.as_ref(), UnaryOperation { op: UnaryOperator::Index { .. }, .. }))
			{
				if !left.is_itself_mutable {
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

		BinaryOperator::Range => unreachable!(),

		_ => collapsed,
	};

	if op == BinaryOperator::LogicalAnd {
		fn contains_is(expression: &Expression) -> bool {
			use {BinaryOperator::*, ExpressionKind::*};
			let kind = &expression.kind;
			matches!(kind, CheckIs(..)) || matches!(kind, BinaryOperation(o) if matches!(o.op, LogicalIsAnd))
		}

		if contains_is(&left) || contains_is(&right) {
			op = BinaryOperator::LogicalIsAnd;
		}
	}

	let yields = left.yields || right.yields;
	let returns = left.returns || right.returns;
	let operation = Box::new(BinaryOperation { op, left, right, type_id });
	let kind = ExpressionKind::BinaryOperation(operation);
	Expression {
		span,
		type_id,
		is_itself_mutable: true,
		is_pointer_access_mutable: true,
		yields,
		returns,
		kind,
		debug_location: span.debug_location(context.parsed_files),
	}
}

fn perform_constant_binary_operation<'a>(
	context: &mut Context<'a, '_, '_>,
	left_expression: &Expression,
	right_expression: &Expression,
	op: BinaryOperator,
) -> Option<Expression<'a>> {
	if let ExpressionKind::NumberValue(left) = &left_expression.kind {
		let right = match &right_expression.kind {
			ExpressionKind::NumberValue(right) => *right,
			_ => return None,
		};

		if left.is_integer() {
			assert!(right.is_integer(), "{right:?}");

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
			let kind = ExpressionKind::NumberValue(value);
			let type_id = context.type_store.number_type_id();
			return Some(Expression {
				span,
				type_id,
				is_itself_mutable: true,
				is_pointer_access_mutable: true,
				yields: false,
				returns: false,
				kind,
				debug_location: span.debug_location(context.parsed_files),
			});
		} else {
			assert!(!right.is_integer(), "{right:?}");

			let value = match op {
				BinaryOperator::Add => left.add(context.messages, right)?,
				BinaryOperator::Sub => left.sub(context.messages, right)?,
				BinaryOperator::Mul => left.mul(context.messages, right)?,
				BinaryOperator::Div => left.div(context.messages, right)?,
				_ => return None,
			};

			let span = value.span();
			let kind = ExpressionKind::NumberValue(value);
			let type_id = context.type_store.number_type_id();
			return Some(Expression {
				span,
				type_id,
				is_itself_mutable: true,
				is_pointer_access_mutable: true,
				yields: false,
				returns: false,
				kind,
				debug_location: span.debug_location(context.parsed_files),
			});
		}
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
		return Some(Expression {
			span,
			type_id,
			is_itself_mutable: true,
			is_pointer_access_mutable: true,
			yields: false,
			returns: false,
			kind,
			debug_location: span.debug_location(context.parsed_files),
		});
	}

	None
}

fn validate_check_is<'a>(context: &mut Context<'a, '_, '_>, check: &'a tree::CheckIs<'a>, span: Span) -> Expression<'a> {
	let mut left = validate_expression(context, &check.left);
	left.into_value(context);

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

	if left.type_id.is_any_collapse(context.type_store) {
		if let Some(binding_name) = binding_name {
			let type_id = context.type_store.any_collapse_type_id();
			let used = check.binding_name.is_none();
			context.push_readable(binding_name, type_id, ReadableKind::Mut, used, left.is_pointer_access_mutable);
		}

		return Expression::any_collapse(context.type_store, span);
	}

	let (left_type_id, is_mutable) = match left.type_id.as_pointed(context.type_store) {
		Some(as_pointer) => (as_pointer.type_id, as_pointer.mutable),
		None => (left.type_id, left.is_itself_mutable),
	};

	let report_not_enum_or_union = |context: &mut Context| {
		let found = context.type_name(left_type_id);
		let error = error!("Cannot check is on type {found} as it is not an enum or union");
		context.messages.message(error.span(left.span));
	};

	let base_entry = context.type_store.type_entries.get(left_type_id);
	let (shape_index, specialization_index) = match base_entry.kind {
		TypeEntryKind::UserType { shape_index, specialization_index, .. } => (shape_index, specialization_index),

		_ => {
			report_not_enum_or_union(context);
			return Expression::any_collapse(context.type_store, span);
		}
	};

	let user_type = context.type_store.user_types.read()[shape_index].clone();
	let user_type = user_type.read();
	let base_specialization = match &user_type.kind {
		UserTypeKind::Enum { shape } => {
			let base = &shape.specializations[specialization_index];
			Some((base.variants.clone(), base.variants_by_name.clone()))
		}

		UserTypeKind::Union { shape } => {
			let base = &shape.specializations[specialization_index];
			Some((base.variants.clone(), base.variants_by_name.clone()))
		}

		UserTypeKind::Struct { .. } => {
			report_not_enum_or_union(context);
			return Expression::any_collapse(context.type_store, span);
		}
	};
	drop(user_type);

	let Some((variants, variants_by_name)) = base_specialization else {
		report_not_enum_or_union(context);
		return Expression::any_collapse(context.type_store, span);
	};

	let mut encountered_variants = Vec::<Option<Span>>::with_capacity(variants_by_name.len());
	for _ in 0..variants_by_name.len() {
		encountered_variants.push(None);
	}

	let mut variant_infos = Vec::with_capacity(check.variant_names.len());

	for variant_name in check.variant_names {
		let (variant_type_id, variant_index) = match variants_by_name.get(variant_name.item) {
			Some(&variant_index) => {
				let variant = &variants[variant_index];
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

	let binding = if let Some(binding_name) = binding_name {
		assert_eq!(check.variant_names.len(), 1);

		let type_id = if let Some(info) = variant_infos.last() {
			info.type_id
				.as_struct(&mut context.type_store, |shape, specialization| {
					if shape.is_transparent_variant {
						assert_eq!(specialization.fields.len(), 1);
						specialization.fields.first().unwrap().type_id
					} else {
						info.type_id
					}
				})
				.unwrap()
		} else {
			context.type_store.any_collapse_type_id()
		};

		let kind = match is_mutable {
			true => ReadableKind::Mut,
			false => ReadableKind::Let,
		};

		let used = check.binding_name.is_none();
		let readable_index = context.push_readable(binding_name, type_id, kind, used, left.is_pointer_access_mutable);
		Some(ResultBinding { type_id, readable_index })
	} else {
		None
	};

	let type_id = context.type_store.bool_type_id();
	let yields = left.yields;
	let returns = left.returns;
	let check_is = CheckIs { left, binding, variant_infos };
	let kind = ExpressionKind::CheckIs(Box::new(check_is));
	Expression {
		span,
		type_id,
		is_itself_mutable: true,
		is_pointer_access_mutable: true,
		yields,
		returns,
		kind,
		debug_location: span.debug_location(context.parsed_files),
	}
}
