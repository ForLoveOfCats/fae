use crate::error::*;
use crate::tree;
use tree::Node;

#[derive(Debug, Clone)]
pub struct FileLayer<'a> {
	name: String,
	children: Vec<FileLayer<'a>>,

	pub imports: Vec<Import<'a>>,
	pub symbols: Vec<Symbol<'a>>,
}

impl<'a> FileLayer<'a> {
	fn new(name: &str) -> Self {
		FileLayer {
			name: name.to_owned(),
			children: Vec::new(),
			imports: Vec::new(),
			symbols: Vec::new(),
		}
	}

	fn find(&self, name: &str) -> Option<&Symbol> {
		self.symbols.iter().find(|s| s.name == name)
	}
}

pub struct Root<'a> {
	path: Vec<&'a str>,

	pub concrete_types: Vec<Type<'a>>,

	symbols: Vec<Symbol<'a>>,
	file_layers: FileLayer<'a>,

	void_type_id: TypeId,
	reference_concrete_index: usize,
	slice_concrete_index: usize,
}

impl<'a> Root<'a> {
	pub fn new() -> Self {
		//This two-phase initialization is a little annoying
		let mut root = Root {
			path: Vec::new(),
			concrete_types: Vec::new(),
			symbols: Vec::new(),
			file_layers: FileLayer::new(""),
			void_type_id: TypeId {
				concrete_index: 0,
				specialization_index: 0,
			},
			reference_concrete_index: 0,
			slice_concrete_index: 0,
		};

		root.register_builtin_type("void", TypeKind::Primative);
		root.void_type_id = TypeId {
			concrete_index: root.concrete_types.len() - 1,
			specialization_index: 0,
		};

		//These are just to park the concrete index so we can generate valid TypeIds for them
		root.register_builtin_type("&", TypeKind::Primative);
		root.reference_concrete_index = root.concrete_types.len() - 1;
		root.register_builtin_type("[]", TypeKind::Primative);
		root.slice_concrete_index = root.concrete_types.len() - 1;

		root.register_builtin_type("i8", TypeKind::Primative);
		root.register_builtin_type("i16", TypeKind::Primative);
		root.register_builtin_type("i32", TypeKind::Primative);
		root.register_builtin_type("i64", TypeKind::Primative);

		root.register_builtin_type("u8", TypeKind::Primative);
		root.register_builtin_type("u16", TypeKind::Primative);
		root.register_builtin_type("u32", TypeKind::Primative);
		root.register_builtin_type("u64", TypeKind::Primative);

		root.register_builtin_type("f16", TypeKind::Primative);
		root.register_builtin_type("f32", TypeKind::Primative);
		root.register_builtin_type("f64", TypeKind::Primative);

		root
	}

	pub fn check_layer_for_module_path(&self, messages: &mut Messages, path: &[Node<&'a str>]) {
		let mut current_layer = &self.file_layers;

		for piece in path {
			let layer = current_layer
				.children
				.iter()
				.enumerate()
				.find(|(_, c)| c.name == piece.node);

			match layer {
				Some(layer) => current_layer = layer.1,

				None => {
					messages.error(message!("Unknown module path segment").span(piece.span));
					return;
				}
			}
		}
	}

	pub fn layer_or_create_for_module_path(&mut self, path: &[String]) -> &mut FileLayer<'a> {
		let mut current_layer = &mut self.file_layers;

		for piece in path {
			let index = current_layer
				.children
				.iter()
				.enumerate()
				.find(|(_, c)| c.name == *piece)
				.map(|(i, _)| i);

			if let Some(index) = index {
				current_layer = &mut current_layer.children[index];
			} else {
				let new_layer = FileLayer::new(&piece);
				current_layer.children.push(new_layer);
				current_layer = current_layer.children.last_mut().unwrap();
			}
		}

		current_layer
	}

	pub fn lookup_type_id_for_path(
		&mut self,
		parsed_type: &tree::Type,
		path: &[String],
	) -> Option<TypeId> {
		assert!(!path.is_empty());

		let (segments, arguments) = match parsed_type {
			tree::Type::Void => return Some(self.void_type_id),

			tree::Type::Reference(inner) => {
				let inner_id = self.lookup_type_id_for_path(&inner.node, path)?;
				let concrete_index = self.reference_concrete_index;
				let concrete = &mut self.concrete_types[concrete_index];
				let specialization_index = concrete.get_or_add_specialization(&[inner_id]);
				return Some(TypeId {
					concrete_index,
					specialization_index,
				});
			}

			tree::Type::Slice(inner) => {
				let inner_id = self.lookup_type_id_for_path(&inner.node, path)?;
				let concrete_index = self.slice_concrete_index;
				let concrete = &mut self.concrete_types[concrete_index];
				let specialization_index = concrete.get_or_add_specialization(&[inner_id]);
				return Some(TypeId {
					concrete_index,
					specialization_index,
				});
			}

			tree::Type::Path { segments, arguments } => (segments, arguments),
		};

		assert!(!segments.segments.is_empty());

		let mut current_layer = &self.file_layers;
		for piece in path {
			current_layer = current_layer
				.children
				.iter()
				.enumerate()
				.find(|(_, c)| c.name == *piece)
				.expect("Path must be valid")
				.1;
		}

		// let symbol = if segments.segments.len() == 1 {
		// 	current_layer.find(segments.segments[0].node)?
		// } else {
		// 	let mut current_layer = &self.file_symbols;
		// 	for piece in path {
		// 		current_layer = current_layer
		// 			.children
		// 			.iter()
		// 			.enumerate()
		// 			.find(|(_, c)| c.name == *piece)
		// 			.expect("Path must be valid")
		// 			.1;
		// 	}
		// }

		// let concrete_index = match symbol.kind {
		// 	SymbolKind::Type { concrete_index } => concrete_index,
		// 	_ => return None,
		// };

		// let concrete = &mut self.concrete_types[concrete_index];
		// let arguments = arguments.iter().map(|arg| self.lookup_type_id_for_path(&arg.node, path)).collect();
		// concrete.get_or_add_specialization();

		//TODO: This should handle specilization
		// Some(TypeId {
		// 	concrete_index,
		// 	specialization_index: 0,
		// })
		None
	}

	pub fn base_scope<'b>(&'b mut self) -> Scope<'a, 'b> {
		let initial_frame_state = self.frame_state();

		Scope {
			root: self,
			initial_frame_state,
		}
	}

	fn frame_state(&self) -> FrameState {
		FrameState {
			path_length: self.path.len(),
			symbol_count: self.symbols.len(),
		}
	}

	#[must_use = "Logical error to call without handling resulting symbol"]
	pub fn register_type(&mut self, name: &'a str, kind: TypeKind<'a>) -> Symbol<'a> {
		self.concrete_types.push(Type {
			path: self.path.clone(),
			name: name.to_string(),
			kind,
			specialization: Vec::new(),
		});

		let concrete_index = self.concrete_types.len() - 1;
		let symbol_kind = SymbolKind::Type { concrete_index };
		Symbol { name, kind: symbol_kind }
	}

	fn register_builtin_type(&mut self, name: &'a str, kind: TypeKind<'a>) {
		let symbol = self.register_type(name, kind);
		self.symbols.push(symbol);
	}
}

#[derive(Debug, Clone, Copy)]
struct FrameState {
	path_length: usize,
	symbol_count: usize,
}

pub struct Scope<'a, 'b> {
	root: &'b mut Root<'a>,
	initial_frame_state: FrameState,
}

impl<'a, 'b> Scope<'a, 'b> {
	pub fn child_scope<'c>(&'c mut self) -> Scope<'a, 'c> {
		let initial_frame_state = self.root.frame_state();

		Scope {
			root: self.root,
			initial_frame_state,
		}
	}

	pub fn child_namespace<'c>(&'c mut self, namespace: &'a str) -> Scope<'a, 'c> {
		let initial_frame_state = self.root.frame_state();
		self.root.path.push(namespace);

		Scope {
			root: self.root,
			initial_frame_state,
		}
	}

	pub fn register_type(&mut self, name: &'a str, kind: TypeKind<'a>) {
		let symbol = self.root.register_type(name, kind);
		self.root.symbols.push(symbol);
	}
}

impl<'a, 'b> Drop for Scope<'a, 'b> {
	fn drop(&mut self) {
		let state = self.initial_frame_state;

		self.root.symbols.truncate(state.path_length);
		self.root.symbols.truncate(state.symbol_count);
	}
}

#[derive(Debug, Clone)]
pub struct Import<'a> {
	pub segments: Vec<Node<&'a str>>,
}

#[derive(Debug, Copy, Clone)]
pub struct Symbol<'a> {
	pub name: &'a str,
	pub kind: SymbolKind,
}

#[derive(Debug, Copy, Clone)]
pub enum SymbolKind {
	Type { concrete_index: usize },
}

#[derive(Debug)]
pub struct Type<'a> {
	pub path: Vec<&'a str>, //NOTE: Does *not* include name
	pub name: String,
	pub kind: TypeKind<'a>,
	pub specialization: Vec<Specialization>,
}

impl<'a> Type<'a> {
	fn get_or_add_specialization(&mut self, arguments: &[TypeId]) -> usize {
		for (index, existing) in self.specialization.iter().enumerate() {
			if existing
				.arguments
				.iter()
				.zip(arguments)
				.all(|(a, b)| a == b)
			{
				return index;
			}
		}

		let arguments = arguments.to_vec();
		self.specialization.push(Specialization { arguments });
		self.specialization.len() - 1
	}
}

#[derive(Debug)]
pub struct Specialization {
	arguments: Vec<TypeId>,
}

#[derive(Debug)]
pub enum TypeKind<'a> {
	Primative,
	Reference,
	Struct { fields: Vec<Field<'a>> },
}

#[derive(Debug)]
pub struct Field<'a> {
	name: &'a str,
	type_id: TypeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId {
	concrete_index: usize,
	specialization_index: usize,
}
