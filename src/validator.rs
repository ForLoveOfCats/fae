use crate::error::*;
use crate::mir::*;
use crate::tree;

pub fn populate_file_symbols<'a>(root: &mut Root<'a>, files: &'a [tree::File]) {
	let mut imports = Vec::new();
	let mut registered_symbols = Vec::new();

	for file in files {
		for item in &file.items {
			match item {
				tree::Item::Using(item) => {
					let segments = item.path_segments.node.segments.clone();
					imports.push(Import { segments });
				}

				tree::Item::Struct(item) => {
					let kind = TypeKind::Struct { fields: Vec::new() };
					let symbol = root.register_type(item.name.node, kind);
					registered_symbols.push(symbol);
				}

				_ => {}
			}
		}

		let layer = root.layer_or_create_for_module_path(file.module_path);
		layer.imports.append(&mut imports);
		layer.symbols.append(&mut registered_symbols);
		// println!("for {:?} added: {:?}", file.module_path, layer.symbols);
	}
}

pub fn fill_file_symbols<'a>(
	messages: &mut Messages,
	root: &mut Root<'a>,
	files: &'a [tree::File],
) {
	for file in files {
		let layer = root
			.layer_or_create_for_module_path(file.module_path)
			.clone();

		for import in &layer.imports {
			root.check_layer_for_module_path(messages, &import.segments);
		}

		for item in &file.items {
			match item {
				tree::Item::Struct(item) => {
					let symbol = layer
						.symbols
						.iter()
						.find(|s| s.name == item.name.node)
						.unwrap();

					let mut fields = Vec::new();
					for field in &item.fields {
						let parsed_type = &field.parsed_type.node;
						let type_id = root.lookup_type_id_for_path(parsed_type, file.module_path);
					}

					let concrete_index = match symbol.kind {
						SymbolKind::Type { concrete_index } => concrete_index,
						// _ => unreachable!(),
					};

					let concrete_type = &mut root.concrete_types[concrete_index];
					let concrete_fields = match &mut concrete_type.kind {
						TypeKind::Struct { fields } => fields,
						_ => unreachable!(),
					};
					assert!(concrete_fields.is_empty());
					*concrete_fields = fields;

					// println!(
					// 	"for struct {:?} in path {:?} fields: {concrete_fields:?}",
					// 	item.name.node, file.module_path
					// );
				}

				_ => {}
			}
		}

		for message in messages.errors() {
			message.print(
				&file.source_file.path,
				&file.source_file.source,
				"File root validation error",
			);
		}
		messages.remove_errors();
	}
}

pub fn validate_parsed_file(base_scope: &mut Scope, file: &tree::File) {}
