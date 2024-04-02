use inkwell::attributes::Attribute;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, StructType};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue};
use inkwell::AddressSpace;

use crate::codegen::amd64::sysv_abi::{self, Class, ClassKind};
use crate::codegen::llvm::generator::{self, AttributeKinds, LLVMTypes};
use crate::frontend::ir::{Function, FunctionShape};
use crate::frontend::type_store::{Layout, TypeStore};

pub trait LLVMAbi<'ctx> {
	fn new() -> Self;

	fn define_function(
		&mut self,
		type_store: &TypeStore,
		context: &'ctx Context,
		module: &mut Module<'ctx>,
		builder: &mut Builder<'ctx>,
		attribute_kinds: &AttributeKinds,
		llvm_types: &LLVMTypes<'ctx>,
		function_shape: &FunctionShape,
		function: &Function,
	) -> DefinedFunction<'ctx>;

	fn call_function(
		&mut self,
		context: &'ctx Context,
		builder: &mut Builder<'ctx>,
		function: &DefinedFunction<'ctx>,
		arguments: &[Option<generator::Binding<'ctx>>],
	) -> Option<generator::Binding<'ctx>>;

	fn return_value(
		&mut self,
		context: &'ctx Context,
		builder: &mut Builder<'ctx>,
		function: &DefinedFunction<'ctx>,
		value: Option<generator::Binding<'ctx>>,
	);
}

#[derive(Clone, Copy)]
pub enum FunctionReturnType<'ctx> {
	Void,
	ByValue { value_type: BasicTypeEnum<'ctx> },
	ByPointer { pointed_type: BasicTypeEnum<'ctx>, layout: Layout }, // sret
}

pub struct DefinedFunction<'ctx> {
	pub llvm_function: FunctionValue<'ctx>,
	pub return_type: FunctionReturnType<'ctx>,
	pub parameter_information: Vec<Option<ParameterInformation<'ctx>>>,
	pub initial_values: Vec<Option<generator::Binding<'ctx>>>,
	pub entry_block: Option<BasicBlock<'ctx>>, // None for extern functions
}

struct ParameterAttribute {
	index: u32,
	attribute: Attribute,
}

#[derive(Debug, Clone)]
pub enum ParameterInformation<'ctx> {
	BareValue,
	ByPointer { pointed_type: BasicTypeEnum<'ctx>, layout: Layout },
	Composition(ParameterComposition<'ctx>),
}

#[derive(Debug, Clone)]
pub struct ParameterComposition<'ctx> {
	pub composition_struct: StructType<'ctx>,
	pub actual_type: BasicTypeEnum<'ctx>,
	pub layout: Layout,
}

pub struct SysvAbi<'ctx> {
	return_type_buffer: Vec<BasicTypeEnum<'ctx>>,
	parameter_type_buffer: Vec<BasicMetadataTypeEnum<'ctx>>,
	parameter_basic_type_buffer: Vec<BasicTypeEnum<'ctx>>,
	parameter_information_buffer: Vec<Option<ParameterInformation<'ctx>>>,
	attribute_buffer: Vec<ParameterAttribute>,
	argument_value_buffer: Vec<BasicMetadataValueEnum<'ctx>>,
}

impl<'ctx> SysvAbi<'ctx> {
	fn map_classes_into_basic_type_buffer<'a>(
		context: &'ctx Context,
		buffer: &mut Vec<BasicTypeEnum<'ctx>>,
		iterator: impl Iterator<Item = &'a Class>,
	) {
		for class in iterator {
			match class.kind {
				sysv_abi::ClassKind::Integer => {
					let llvm_type = match class.size {
						1 => context.i8_type(),
						2 => context.i16_type(),
						4 => context.i32_type(),
						8 => context.i64_type(),
						unknown_size => panic!("{unknown_size}"),
					};
					buffer.push(BasicTypeEnum::IntType(llvm_type));
				}

				sysv_abi::ClassKind::SSE | sysv_abi::ClassKind::SSEUp => {
					let llvm_type = match class.size {
						2 => context.f16_type(),
						4 => context.f32_type(),
						8 => context.f64_type(),
						unknown_size => panic!("{unknown_size}"),
					};
					buffer.push(BasicTypeEnum::FloatType(llvm_type));
				}

				sysv_abi::ClassKind::Memory => {
					assert_eq!(class.size, 8);
					let ptr_type = context.i8_type().ptr_type(AddressSpace::default());
					buffer.push(BasicTypeEnum::PointerType(ptr_type));
				}

				sysv_abi::ClassKind::X87
				| sysv_abi::ClassKind::X87Up
				| sysv_abi::ClassKind::ComplexX87
				| sysv_abi::ClassKind::NoClass => {
					unreachable!("{class:?}");
				}
			}
		}
	}

	fn map_classes_into_parameter_type_buffer<'a>(&mut self, context: &'ctx Context, iterator: impl Iterator<Item = &'a Class>) {
		for class in iterator {
			match class.kind {
				sysv_abi::ClassKind::Integer => {
					let llvm_type = match class.size {
						1 => context.i8_type(),
						2 => context.i16_type(),
						4 => context.i32_type(),
						8 => context.i64_type(),
						unknown_size => panic!("{unknown_size}"),
					};
					self.parameter_type_buffer.push(BasicMetadataTypeEnum::IntType(llvm_type));
				}

				sysv_abi::ClassKind::SSE | sysv_abi::ClassKind::SSEUp => {
					let llvm_type = match class.size {
						2 => context.f16_type(),
						4 => context.f32_type(),
						8 => context.f64_type(),
						unknown_size => panic!("{unknown_size}"),
					};
					self.parameter_type_buffer.push(BasicMetadataTypeEnum::FloatType(llvm_type));
				}

				sysv_abi::ClassKind::Memory => {
					assert_eq!(class.size, 8);
					let ptr_type = context.i8_type().ptr_type(AddressSpace::default());
					self.parameter_type_buffer.push(BasicMetadataTypeEnum::PointerType(ptr_type));
				}

				sysv_abi::ClassKind::X87
				| sysv_abi::ClassKind::X87Up
				| sysv_abi::ClassKind::ComplexX87
				| sysv_abi::ClassKind::NoClass => {
					unreachable!("{class:?}");
				}
			}
		}
	}
}

impl<'ctx> LLVMAbi<'ctx> for SysvAbi<'ctx> {
	fn new() -> Self {
		SysvAbi {
			return_type_buffer: Vec::new(),
			parameter_type_buffer: Vec::new(),
			parameter_basic_type_buffer: Vec::new(),
			parameter_information_buffer: Vec::new(),
			attribute_buffer: Vec::new(),
			argument_value_buffer: Vec::new(),
		}
	}

	fn define_function(
		&mut self,
		type_store: &TypeStore,
		context: &'ctx Context,
		module: &mut Module<'ctx>,
		builder: &mut Builder<'ctx>,
		attribute_kinds: &AttributeKinds,
		llvm_types: &LLVMTypes<'ctx>,
		function_shape: &FunctionShape,
		function: &Function,
	) -> DefinedFunction<'ctx> {
		self.return_type_buffer.clear();
		self.parameter_type_buffer.clear();
		self.attribute_buffer.clear();

		let return_type = if type_store.type_layout(function.return_type).size > 0 {
			let return_type = llvm_types.type_to_basic_type_enum(context, type_store, function.return_type);

			let mut classes_buffer = sysv_abi::classification_buffer();
			let classes = sysv_abi::classify_type(type_store, &mut classes_buffer, function.return_type);
			Self::map_classes_into_basic_type_buffer(context, &mut self.return_type_buffer, classes.iter());

			if let Some(Class { kind: ClassKind::Memory, .. }) = classes.first() {
				assert_eq!(classes.len(), 1);
				assert_eq!(self.return_type_buffer.len(), 1);

				let ptr_type = context.i8_type().ptr_type(AddressSpace::default());
				self.parameter_type_buffer.push(BasicMetadataTypeEnum::PointerType(ptr_type));

				let attribute = context.create_enum_attribute(attribute_kinds.sret, 0);
				self.attribute_buffer.push(ParameterAttribute { index: 0, attribute });
				self.return_type_buffer.clear(); // Make us use void return type

				let layout = type_store.type_layout(function.return_type);
				FunctionReturnType::ByPointer { pointed_type: return_type, layout }
			} else {
				FunctionReturnType::ByValue { value_type: return_type }
			}
		} else {
			FunctionReturnType::Void
		};

		self.parameter_basic_type_buffer.clear();
		self.parameter_information_buffer.clear();

		for parameter in &function.parameters {
			let layout = type_store.type_layout(parameter.type_id);
			if layout.size <= 0 {
				self.parameter_information_buffer.push(None);
				continue;
			}

			let mut classes_buffer = sysv_abi::classification_buffer();
			let classes = sysv_abi::classify_type(type_store, &mut classes_buffer, parameter.type_id);
			self.map_classes_into_parameter_type_buffer(context, classes.iter());

			let initial_type_len = self.parameter_basic_type_buffer.len();
			Self::map_classes_into_basic_type_buffer(context, &mut self.parameter_basic_type_buffer, classes.iter());
			let slice = &self.parameter_basic_type_buffer[initial_type_len..];

			let is_non_memory = classes.len() == 1 && classes[0].kind != ClassKind::Memory;
			let is_bare_value = is_non_memory && parameter.type_id.is_primative(type_store);
			let is_by_pointer = classes.len() == 1 && classes[0].kind == ClassKind::Memory;

			if is_bare_value {
				assert_eq!(slice.len(), 1);
				let information = Some(ParameterInformation::BareValue);
				self.parameter_information_buffer.push(information);
			} else if is_by_pointer {
				assert_eq!(slice.len(), 1);
				let pointed_type = llvm_types.type_to_basic_type_enum(context, type_store, parameter.type_id);
				let information = Some(ParameterInformation::ByPointer { pointed_type, layout });
				self.parameter_information_buffer.push(information);
			} else {
				let composition_struct = context.struct_type(slice, false);
				let actual_type = llvm_types.type_to_basic_type_enum(context, type_store, parameter.type_id);
				let composition = ParameterComposition { composition_struct, actual_type, layout };
				let information = Some(ParameterInformation::Composition(composition));
				self.parameter_information_buffer.push(information);
			}
		}

		let fn_type = if self.return_type_buffer.is_empty() {
			context.void_type().fn_type(&self.parameter_type_buffer, false)
		} else {
			if let [llvm_type] = self.return_type_buffer.as_slice() {
				llvm_type.fn_type(&self.parameter_type_buffer, false)
			} else {
				let llvm_type = context.struct_type(&self.return_type_buffer, false);
				llvm_type.fn_type(&self.parameter_type_buffer, false)
			}
		};

		if let Some(extern_attribute) = function_shape.extern_attribute {
			let name = extern_attribute.item.name;
			let llvm_function = module.add_function(name, fn_type, Some(Linkage::External));
			return DefinedFunction {
				llvm_function,
				return_type,
				parameter_information: self.parameter_information_buffer.clone(),
				initial_values: Vec::new(),
				entry_block: None,
			};
		}

		let name = if let Some(export_attribute) = function_shape.export_attribute {
			export_attribute.item.name
		} else {
			""
		};

		let llvm_function = module.add_function(name, fn_type, None);
		for attribute in &self.attribute_buffer {
			llvm_function.add_attribute(inkwell::attributes::AttributeLoc::Param(attribute.index), attribute.attribute);
		}

		let entry_block = context.append_basic_block(llvm_function, "");
		builder.position_at_end(entry_block);

		let mut initial_values = Vec::with_capacity(self.parameter_information_buffer.len());
		let mut parameter_index = if matches!(return_type, FunctionReturnType::ByPointer { .. }) {
			1 // Skip processing first parameter, it is the invisible sret pointer
		} else {
			0
		};

		for information in &self.parameter_information_buffer {
			let Some(information) = information else {
				initial_values.push(None);
				parameter_index += 1;
				continue;
			};

			let composition = match information {
				ParameterInformation::BareValue => {
					let parameter = llvm_function.get_nth_param(parameter_index as u32).unwrap();
					let binding = generator::Binding::Value(parameter);
					initial_values.push(Some(binding));
					parameter_index += 1;
					continue;
				}

				&ParameterInformation::ByPointer { pointed_type, .. } => {
					let parameter = llvm_function.get_nth_param(parameter_index as u32).unwrap();
					let pointer = parameter.into_pointer_value();
					let binding = generator::Binding::Pointer { pointer, pointed_type };
					initial_values.push(Some(binding));
					parameter_index += 1;
					continue;
				}

				ParameterInformation::Composition(composition) => composition,
			};

			assert!(composition.layout.size > 0, "{:?}", composition.layout);

			let pointer = builder.build_alloca(composition.actual_type, "").unwrap();
			let pointed_type = BasicTypeEnum::StructType(composition.composition_struct);
			let binding = generator::Binding::Pointer { pointer, pointed_type };
			initial_values.push(Some(binding));
			let composition = composition.composition_struct;

			for field_index in 0..composition.count_fields() as u32 {
				let ptr = builder.build_struct_gep(composition, pointer, field_index, "").unwrap();
				let parameter = llvm_function.get_nth_param(parameter_index as u32).unwrap();
				builder.build_store(ptr, parameter).unwrap();
				parameter_index += 1;
			}
		}

		DefinedFunction {
			llvm_function,
			return_type,
			parameter_information: self.parameter_information_buffer.clone(),
			initial_values,
			entry_block: Some(entry_block),
		}
	}

	fn call_function(
		&mut self,
		context: &'ctx Context,
		builder: &mut Builder<'ctx>,
		function: &DefinedFunction<'ctx>,
		arguments: &[Option<generator::Binding<'ctx>>],
	) -> Option<generator::Binding<'ctx>> {
		self.argument_value_buffer.clear();

		let mut sret_alloca = None;
		match function.return_type {
			FunctionReturnType::ByPointer { pointed_type, .. } => {
				let alloca = builder.build_alloca(pointed_type, "").unwrap();
				self.argument_value_buffer.push(BasicMetadataValueEnum::PointerValue(alloca));
				sret_alloca = Some(alloca);
			}

			_ => {}
		}

		assert_eq!(arguments.len(), function.parameter_information.len());
		for (&value, information) in arguments.iter().zip(function.parameter_information.iter()) {
			let (Some(value), Some(information)) = (value, information) else {
				assert_eq!(value.is_none(), information.is_none());
				continue;
			};

			let composition = match information {
				ParameterInformation::Composition(composition) => composition,

				&ParameterInformation::ByPointer { pointed_type, layout } => {
					match value {
						generator::Binding::Value(value) => {
							let alloca = builder.build_alloca(pointed_type, "").unwrap();
							builder.build_store(alloca, value).unwrap();
							let argument = BasicMetadataValueEnum::PointerValue(alloca);
							self.argument_value_buffer.push(argument);
						}

						generator::Binding::Pointer { pointer, pointed_type: ty } => {
							assert_eq!(pointed_type, ty);
							let alloca = builder.build_alloca(pointed_type, "").unwrap();
							let size = context.i64_type().const_int(layout.size as u64, false);
							let align = layout.alignment as u32;
							builder.build_memcpy(alloca, align, pointer, align, size).unwrap();
							let argument = BasicMetadataValueEnum::PointerValue(alloca);
							self.argument_value_buffer.push(argument);
						}
					};

					continue;
				}

				ParameterInformation::BareValue => {
					let value = value.to_value(builder);
					let argument = basic_value_enum_to_basic_metadata_value_enum(value);
					self.argument_value_buffer.push(argument);
					continue;
				}
			};

			let pointer = match value {
				generator::Binding::Value(value) => {
					let alloca = builder.build_alloca(value.get_type(), "").unwrap();
					builder.build_store(alloca, value).unwrap();
					alloca
				}

				generator::Binding::Pointer { pointer, .. } => pointer,
			};

			let composition_field_count = composition.composition_struct.count_fields();
			let composition = composition.composition_struct;
			for field_index in 0..composition_field_count {
				let field_type = composition.get_field_type_at_index(field_index).unwrap();
				let field_pointer = builder.build_struct_gep(composition, pointer, field_index, "").unwrap();
				let value = builder.build_load(field_type, field_pointer, "").unwrap();
				let argument = basic_value_enum_to_basic_metadata_value_enum(value);
				self.argument_value_buffer.push(argument);
			}
		}

		let callsite_value = builder
			.build_direct_call(function.llvm_function, &self.argument_value_buffer, "")
			.unwrap();

		match function.return_type {
			FunctionReturnType::Void => None,

			FunctionReturnType::ByValue { .. } => {
				let value = callsite_value.try_as_basic_value().left().unwrap();
				Some(generator::Binding::Value(value))
			}

			FunctionReturnType::ByPointer { pointed_type, .. } => {
				let pointer = sret_alloca.expect("Must be Some if returning by pointer");
				Some(generator::Binding::Pointer { pointer, pointed_type })
			}
		}
	}

	fn return_value(
		&mut self,
		context: &'ctx Context,
		builder: &mut Builder<'ctx>,
		function: &DefinedFunction<'ctx>,
		value: Option<generator::Binding<'ctx>>,
	) {
		match function.return_type {
			FunctionReturnType::Void => {
				assert!(value.is_none(), "{value:?}");
				builder.build_return(None).unwrap();
			}

			FunctionReturnType::ByValue { .. } => {
				let value = value.unwrap().to_value(builder);
				builder.build_return(Some(&value)).unwrap();
			}

			FunctionReturnType::ByPointer { pointed_type, layout } => {
				let first_parameter = function.llvm_function.get_nth_param(0).unwrap();
				let sret_pointer = first_parameter.into_pointer_value();

				match value.unwrap() {
					generator::Binding::Value(value) => {
						builder.build_store(sret_pointer, value).unwrap();
					}

					generator::Binding::Pointer { pointer, pointed_type: ty } => {
						assert_eq!(pointed_type, ty);
						let size = context.i64_type().const_int(layout.size as u64, false);
						let align = layout.alignment as u32;
						builder.build_memcpy(sret_pointer, align, pointer, align, size).unwrap();
					}
				}

				builder.build_return(None).unwrap();
			}
		}
	}
}

fn basic_value_enum_to_basic_metadata_value_enum<'ctx>(value: BasicValueEnum<'ctx>) -> BasicMetadataValueEnum<'ctx> {
	match value {
		BasicValueEnum::ArrayValue(value) => BasicMetadataValueEnum::ArrayValue(value),
		BasicValueEnum::IntValue(value) => BasicMetadataValueEnum::IntValue(value),
		BasicValueEnum::FloatValue(value) => BasicMetadataValueEnum::FloatValue(value),
		BasicValueEnum::PointerValue(value) => BasicMetadataValueEnum::PointerValue(value),
		BasicValueEnum::StructValue(value) => BasicMetadataValueEnum::StructValue(value),
		BasicValueEnum::VectorValue(value) => BasicMetadataValueEnum::VectorValue(value),
	}
}
