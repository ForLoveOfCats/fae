use inkwell::attributes::Attribute;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, StructType};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue};
use inkwell::AddressSpace;

use crate::codegen::amd64::sysv_abi::{self, Class, ClassKind};
use crate::codegen::llvm::generator::{AttributeKinds, LLVMTypes};
use crate::ir::{Function, FunctionShape};
use crate::tree::ExternAttribute;
use crate::type_store::TypeStore;

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
		builder: &mut Builder<'ctx>,
		function: &DefinedFunction<'ctx>,
		arguments: &[BasicValueEnum<'ctx>],
	) -> Option<BasicValueEnum<'ctx>>;
}

#[derive(Clone, Copy)]
pub enum FunctionReturnType<'ctx> {
	Void,
	ByValue { value_type: BasicTypeEnum<'ctx> },
	ByPointer { pointed_type: BasicTypeEnum<'ctx> }, // sret
}

pub struct DefinedFunction<'ctx> {
	pub llvm_function: FunctionValue<'ctx>,
	pub return_type: FunctionReturnType<'ctx>,
	pub argument_values: Vec<BasicValueEnum<'ctx>>,
	pub entry_block: Option<BasicBlock<'ctx>>, // None for extern functions
}

struct ParameterAttribute {
	index: u32,
	attribute: Attribute,
}

struct ParameterComposition<'ctx> {
	composition_struct: StructType<'ctx>,
	actual_type: BasicTypeEnum<'ctx>,
}

pub struct SysvAbi<'ctx> {
	return_type_buffer: Vec<BasicTypeEnum<'ctx>>,
	parameter_type_buffer: Vec<BasicMetadataTypeEnum<'ctx>>,
	parameter_basic_type_buffer: Vec<BasicTypeEnum<'ctx>>,
	parameter_composition_buffer: Vec<ParameterComposition<'ctx>>,
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
			parameter_composition_buffer: Vec::new(),
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

				FunctionReturnType::ByPointer { pointed_type: return_type }
			} else {
				FunctionReturnType::ByValue { value_type: return_type }
			}
		} else {
			FunctionReturnType::Void
		};

		self.parameter_basic_type_buffer.clear();
		self.parameter_composition_buffer.clear();

		for parameter in &function.parameters {
			let layout = type_store.type_layout(parameter.type_id);
			if layout.size <= 0 {
				continue;
			}

			let mut classes_buffer = sysv_abi::classification_buffer();
			let classes = sysv_abi::classify_type(type_store, &mut classes_buffer, parameter.type_id);
			self.map_classes_into_parameter_type_buffer(context, classes.iter());

			let initial_type_len = self.parameter_basic_type_buffer.len();
			Self::map_classes_into_basic_type_buffer(context, &mut self.parameter_basic_type_buffer, classes.iter());
			let range = initial_type_len..self.parameter_basic_type_buffer.len();

			let composition_struct = context.struct_type(&self.parameter_basic_type_buffer[range], false);
			let actual_type = llvm_types.type_to_basic_type_enum(context, type_store, parameter.type_id);
			let composition = ParameterComposition { composition_struct, actual_type };
			self.parameter_composition_buffer.push(composition);
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
			if let ExternAttribute::Name(name) = extern_attribute.item {
				let llvm_function = module.add_function(name, fn_type, Some(Linkage::AvailableExternally));
				return DefinedFunction {
					llvm_function,
					return_type,
					argument_values: Vec::new(),
					entry_block: None,
				};
			}
			unreachable!("{function_shape:?}, {function:?}, {extern_attribute:?}");
		}

		let llvm_function = module.add_function(function_shape.name.item, fn_type, None);
		for attribute in &self.attribute_buffer {
			llvm_function.add_attribute(inkwell::attributes::AttributeLoc::Param(attribute.index), attribute.attribute);
		}

		let entry_block = context.append_basic_block(llvm_function, "");
		builder.position_at_end(entry_block);

		let mut argument_values = Vec::with_capacity(self.parameter_composition_buffer.len());
		for composition in &self.parameter_composition_buffer {
			let alloca = builder.build_alloca(composition.actual_type, "").unwrap();
			argument_values.push(BasicValueEnum::PointerValue(alloca));
			let composition = composition.composition_struct;

			for index in 0..composition.count_fields() {
				let ptr = builder.build_struct_gep(composition, alloca, index as u32, "").unwrap();
				let parameter = llvm_function.get_nth_param(index as u32).unwrap();
				builder.build_store(ptr, parameter).unwrap();
			}
		}

		DefinedFunction {
			llvm_function,
			return_type,
			argument_values,
			entry_block: Some(entry_block),
		}
	}

	fn call_function(
		&mut self,
		builder: &mut Builder<'ctx>,
		function: &DefinedFunction<'ctx>,
		arguments: &[BasicValueEnum<'ctx>],
	) -> Option<BasicValueEnum<'ctx>> {
		self.argument_value_buffer.clear();

		let mut sret_alloca = None;
		match function.return_type {
			FunctionReturnType::ByPointer { pointed_type } => {
				let alloca = builder.build_alloca(pointed_type, "").unwrap();
				self.argument_value_buffer.push(BasicMetadataValueEnum::PointerValue(alloca));
				sret_alloca = Some(alloca);
			}

			_ => {}
		}

		for &value in arguments {
			let argument = basic_value_enum_to_basic_metadata_value_enum(value);
			self.argument_value_buffer.push(argument);
		}

		let callsite_value = builder
			.build_direct_call(function.llvm_function, &self.argument_value_buffer, "")
			.unwrap();

		match function.return_type {
			FunctionReturnType::Void => None,

			FunctionReturnType::ByValue { .. } => Some(callsite_value.try_as_basic_value().left().unwrap()),

			FunctionReturnType::ByPointer { .. } => {
				Some(BasicValueEnum::PointerValue(sret_alloca.expect("Must be Some if returning by pointer")))
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
