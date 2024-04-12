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
use crate::frontend::type_store::{Layout, NumericKind, TypeId, TypeStore};

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
		type_store: &TypeStore,
		context: &'ctx Context,
		builder: &mut Builder<'ctx>,
		llvm_types: &LLVMTypes<'ctx>,
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
	pub return_type_id: TypeId,
	pub parameter_information: Vec<Option<ParameterInformation<'ctx>>>,
	pub c_varargs: bool,
	pub initial_values: Vec<Option<generator::Binding<'ctx>>>,
	pub entry_block: Option<BasicBlock<'ctx>>, // None for extern functions
}

struct ParameterAttribute {
	index: u32,
	attribute: Attribute,
}

#[derive(Debug, Clone)]
pub enum ParameterInformation<'ctx> {
	BareValue {
		type_id: TypeId,
	},

	ByPointer {
		pointed_type: BasicTypeEnum<'ctx>,
		pointed_type_id: TypeId,
		layout: Layout,
	},

	Composition(ParameterComposition<'ctx>),
}

#[derive(Debug, Clone)]
pub struct ParameterComposition<'ctx> {
	pub composition_struct: StructType<'ctx>,
	pub actual_type: BasicTypeEnum<'ctx>,
	pub actual_type_id: TypeId,
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

				sysv_abi::ClassKind::Boolean => {
					assert_eq!(class.size, 1);
					let llvm_type = context.bool_type();
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

				sysv_abi::ClassKind::SSECombine => {
					// The element type doesn't really matter, it's going to be reinterpreted anyway
					let llvm_type = match class.size {
						4 => context.f16_type().vec_type(2),
						8 => context.f16_type().vec_type(4),
						unknown_size => panic!("{unknown_size}"),
					};
					buffer.push(BasicTypeEnum::VectorType(llvm_type));
				}

				sysv_abi::ClassKind::Pointer | sysv_abi::ClassKind::Memory => {
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

	// It would be really nice to deduplicate this with `map_classes_into_basic_type_buffer`
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

				sysv_abi::ClassKind::Boolean => {
					assert_eq!(class.size, 1);
					let llvm_type = context.bool_type();
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

				sysv_abi::ClassKind::SSECombine => {
					// The element type doesn't really matter, it's going to be reinterpreted anyway
					let llvm_type = match class.size {
						4 => context.f16_type().vec_type(2),
						8 => context.f16_type().vec_type(4),
						unknown_size => panic!("{unknown_size}"),
					};
					self.parameter_type_buffer.push(BasicMetadataTypeEnum::VectorType(llvm_type));
				}

				sysv_abi::ClassKind::Pointer | sysv_abi::ClassKind::Memory => {
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

	fn construct_parameter_information(
		&mut self,
		type_store: &TypeStore,
		context: &'ctx Context,
		llvm_types: &LLVMTypes<'ctx>,
		parameter_type_id: TypeId,
	) -> Option<ParameterInformation<'ctx>> {
		let layout = type_store.type_layout(parameter_type_id);
		if layout.size <= 0 {
			return None;
		}

		let mut classes_buffer = sysv_abi::classification_buffer();
		let classes = sysv_abi::classify_type(type_store, &mut classes_buffer, parameter_type_id);
		self.map_classes_into_parameter_type_buffer(context, classes.iter());

		self.parameter_basic_type_buffer.clear();
		Self::map_classes_into_basic_type_buffer(context, &mut self.parameter_basic_type_buffer, classes.iter());

		let is_non_memory = classes.len() == 1 && classes[0].kind != ClassKind::Memory;
		let is_bare_value = is_non_memory && parameter_type_id.is_primative(type_store);
		let is_by_pointer = classes.len() == 1 && classes[0].kind == ClassKind::Memory;

		if is_bare_value {
			assert_eq!(self.parameter_basic_type_buffer.len(), 1);
			Some(ParameterInformation::BareValue { type_id: parameter_type_id })
		} else if is_by_pointer {
			assert_eq!(self.parameter_basic_type_buffer.len(), 1);
			let pointed_type = llvm_types.type_to_basic_type_enum(context, type_store, parameter_type_id);
			Some(ParameterInformation::ByPointer { pointed_type, pointed_type_id: parameter_type_id, layout })
		} else {
			let composition_struct = context.struct_type(&self.parameter_basic_type_buffer, false);
			let actual_type = llvm_types.type_to_basic_type_enum(context, type_store, parameter_type_id);
			let actual_type_id = parameter_type_id;
			let composition = ParameterComposition { composition_struct, actual_type, actual_type_id, layout };
			Some(ParameterInformation::Composition(composition))
		}
	}

	fn generate_parameter(
		&mut self,
		context: &'ctx Context,
		builder: &mut Builder<'ctx>,
		value: generator::Binding<'ctx>,
		information: &ParameterInformation<'ctx>,
	) {
		let composition = match information {
			ParameterInformation::Composition(composition) => composition,

			&ParameterInformation::ByPointer { pointed_type, layout, .. } => {
				match value.kind {
					generator::BindingKind::Value(value) => {
						let alloca = builder.build_alloca(pointed_type, "").unwrap();
						builder.build_store(alloca, value).unwrap();
						let argument = BasicMetadataValueEnum::PointerValue(alloca);
						self.argument_value_buffer.push(argument);
					}

					generator::BindingKind::Pointer { pointer, pointed_type: ty } => {
						assert_eq!(pointed_type, ty);
						let alloca = builder.build_alloca(pointed_type, "").unwrap();
						let size = context.i64_type().const_int(layout.size as u64, false);
						let align = layout.alignment as u32;
						builder.build_memcpy(alloca, align, pointer, align, size).unwrap();
						let argument = BasicMetadataValueEnum::PointerValue(alloca);
						self.argument_value_buffer.push(argument);
					}
				};

				return;
			}

			ParameterInformation::BareValue { .. } => {
				let value = value.to_value(builder);
				let argument = basic_value_enum_to_basic_metadata_value_enum(value);
				self.argument_value_buffer.push(argument);
				return;
			}
		};

		let pointer = match value.kind {
			generator::BindingKind::Value(value) => {
				let alloca = builder.build_alloca(value.get_type(), "").unwrap();
				builder.build_store(alloca, value).unwrap();
				alloca
			}

			generator::BindingKind::Pointer { pointer, .. } => pointer,
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

		let return_type_id = function.return_type;
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
			let information = self.construct_parameter_information(type_store, context, llvm_types, parameter.type_id);
			self.parameter_information_buffer.push(information);
		}

		let varargs = function_shape.c_varargs;
		let fn_type = if self.return_type_buffer.is_empty() {
			context.void_type().fn_type(&self.parameter_type_buffer, varargs)
		} else {
			if let [llvm_type] = self.return_type_buffer.as_slice() {
				let is_aggregate = if let FunctionReturnType::ByValue { value_type } = return_type {
					value_type.is_struct_type()
				} else {
					false
				};

				if is_aggregate {
					let llvm_type = context.struct_type(&self.return_type_buffer, false);
					llvm_type.fn_type(&self.parameter_type_buffer, varargs)
				} else {
					llvm_type.fn_type(&self.parameter_type_buffer, varargs)
				}
			} else {
				let llvm_type = context.struct_type(&self.return_type_buffer, false);
				llvm_type.fn_type(&self.parameter_type_buffer, varargs)
			}
		};

		if let Some(extern_attribute) = function_shape.extern_attribute {
			let name = extern_attribute.item.name;
			let llvm_function = module.add_function(name, fn_type, Some(Linkage::External));
			return DefinedFunction {
				llvm_function,
				return_type,
				return_type_id,
				parameter_information: self.parameter_information_buffer.clone(),
				c_varargs: function_shape.c_varargs,
				initial_values: Vec::new(),
				entry_block: None,
			};
		}

		let (name, linkage) = if let Some(export_attribute) = function_shape.export_attribute {
			(export_attribute.item.name, Some(Linkage::DLLExport))
		} else if function_shape.is_main {
			("fae_user_main", Some(Linkage::Private))
		} else {
			(function_shape.name.item, Some(Linkage::Private))
		};

		let llvm_function = module.add_function(name, fn_type, linkage);
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
				&ParameterInformation::BareValue { type_id } => {
					let parameter = llvm_function.get_nth_param(parameter_index as u32).unwrap();
					let kind = generator::BindingKind::Value(parameter);
					initial_values.push(Some(generator::Binding { type_id, kind }));
					parameter_index += 1;
					continue;
				}

				&ParameterInformation::ByPointer { pointed_type, pointed_type_id, .. } => {
					let parameter = llvm_function.get_nth_param(parameter_index as u32).unwrap();
					let pointer = parameter.into_pointer_value();
					let kind = generator::BindingKind::Pointer { pointer, pointed_type };
					initial_values.push(Some(generator::Binding { type_id: pointed_type_id, kind }));
					parameter_index += 1;
					continue;
				}

				ParameterInformation::Composition(composition) => composition,
			};

			assert!(composition.layout.size > 0, "{:?}", composition.layout);

			let pointer = builder.build_alloca(composition.actual_type, "").unwrap();
			let pointed_type = BasicTypeEnum::StructType(composition.composition_struct);
			let type_id = composition.actual_type_id;
			let kind = generator::BindingKind::Pointer { pointer, pointed_type };
			initial_values.push(Some(generator::Binding { type_id, kind }));
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
			return_type_id,
			parameter_information: self.parameter_information_buffer.clone(),
			c_varargs: function_shape.c_varargs,
			initial_values,
			entry_block: Some(entry_block),
		}
	}

	fn call_function(
		&mut self,
		type_store: &TypeStore,
		context: &'ctx Context,
		builder: &mut Builder<'ctx>,
		llvm_types: &LLVMTypes<'ctx>,
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

		if function.c_varargs {
			assert!(arguments.len() >= function.parameter_information.len());
		} else {
			assert_eq!(arguments.len(), function.parameter_information.len());
		}

		for (&value, information) in arguments.iter().zip(function.parameter_information.iter()) {
			let (Some(value), Some(information)) = (value, information) else {
				assert_eq!(value.is_none(), information.is_none(), "{value:?}, {information:?}");
				continue;
			};

			self.generate_parameter(context, builder, value, information);
		}

		if function.c_varargs {
			let remaining_arguments = &arguments[function.parameter_information.len()..];
			for &argument in remaining_arguments {
				let Some(argument) = argument else {
					continue;
				};

				let constructed = self.construct_parameter_information(type_store, context, llvm_types, argument.type_id);
				let information = constructed.unwrap();

				// Some numeric varargs need to be widened before passing
				if let ParameterInformation::BareValue { type_id } = information {
					if let Some(numeric_kind) = type_id.numeric_kind(type_store) {
						match numeric_kind {
							// Sign extend
							NumericKind::I8 | NumericKind::I16 => {
								let int = argument.to_value(builder).into_int_value();
								let widened = builder.build_int_s_extend(int, context.i32_type(), "").unwrap();
								let argument = BasicMetadataValueEnum::IntValue(widened);
								self.argument_value_buffer.push(argument);
								continue;
							}

							// Zero extend
							NumericKind::U8 | NumericKind::U16 => {
								let int = argument.to_value(builder).into_int_value();
								let widened = builder.build_int_z_extend(int, context.i32_type(), "").unwrap();
								let argument = BasicMetadataValueEnum::IntValue(widened);
								self.argument_value_buffer.push(argument);
								continue;
							}

							// TODO: Add f16 once added to the language
							NumericKind::F32 => {
								let float = argument.to_value(builder).into_float_value();
								let double = builder.build_float_cast(float, context.f64_type(), "").unwrap();
								let argument = BasicMetadataValueEnum::FloatValue(double);
								self.argument_value_buffer.push(argument);
								continue;
							}

							_ => {}
						}
					}

					// Zero extend
					if type_id.is_bool(type_store) {
						let int = argument.to_value(builder).into_int_value();
						let widened = builder.build_int_z_extend(int, context.i32_type(), "").unwrap();
						let argument = BasicMetadataValueEnum::IntValue(widened);
						self.argument_value_buffer.push(argument);
						continue;
					}
				}

				self.generate_parameter(context, builder, argument, &information);
			}
		}

		let callsite_value = builder
			.build_direct_call(function.llvm_function, &self.argument_value_buffer, "")
			.unwrap();

		let type_id = function.return_type_id;
		match function.return_type {
			FunctionReturnType::Void => None,

			FunctionReturnType::ByValue { .. } => {
				let value = callsite_value.try_as_basic_value().left().unwrap();
				let kind = generator::BindingKind::Value(value);
				Some(generator::Binding { type_id, kind })
			}

			FunctionReturnType::ByPointer { pointed_type, .. } => {
				let pointer = sret_alloca.expect("Must be Some if returning by pointer");
				let kind = generator::BindingKind::Pointer { pointer, pointed_type };
				Some(generator::Binding { type_id, kind })
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

				match value.unwrap().kind {
					generator::BindingKind::Value(value) => {
						builder.build_store(sret_pointer, value).unwrap();
					}

					generator::BindingKind::Pointer { pointer, pointed_type: ty } => {
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
