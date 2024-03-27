use inkwell::attributes::Attribute;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::FunctionValue;
use inkwell::AddressSpace;

use crate::codegen::amd64::sysv_abi::{self, Class, ClassKind};
use crate::codegen::llvm::generator::{AttributeKinds, Location};
use crate::ir::Function;
use crate::type_store::TypeStore;

pub trait LLVMAbi<'ctx> {
	fn new() -> Self;

	fn build_function(
		&mut self,
		type_store: &TypeStore,
		context: &'ctx Context,
		module: &mut Module<'ctx>,
		builder: &mut Builder<'ctx>,
		attribute_kinds: &AttributeKinds,
		locations: &mut Vec<Location>,
		function: &Function,
		name: &str,
	) -> FunctionValue<'ctx>;
}

struct ParameterAttribute {
	index: u32,
	attribute: Attribute,
}

pub struct SysvAbi<'ctx> {
	return_type_buffer: Vec<BasicTypeEnum<'ctx>>,
	parameter_type_buffer: Vec<BasicMetadataTypeEnum<'ctx>>,
	attribute_buffer: Vec<ParameterAttribute>,
}

impl<'ctx> SysvAbi<'ctx> {
	fn map_classes_into_return_type_buffer<'a>(&mut self, context: &'ctx Context, iterator: impl Iterator<Item = &'a Class>) {
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
					self.return_type_buffer.push(BasicTypeEnum::IntType(llvm_type));
				}

				sysv_abi::ClassKind::SSE | sysv_abi::ClassKind::SSEUp => {
					let llvm_type = match class.size {
						2 => context.f16_type(),
						4 => context.f32_type(),
						8 => context.f64_type(),
						unknown_size => panic!("{unknown_size}"),
					};
					self.return_type_buffer.push(BasicTypeEnum::FloatType(llvm_type));
				}

				sysv_abi::ClassKind::Memory => {
					assert_eq!(class.size, 8);
					let ptr_type = context.i8_type().ptr_type(AddressSpace::default());
					self.return_type_buffer.push(BasicTypeEnum::PointerType(ptr_type));
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
			attribute_buffer: Vec::new(),
		}
	}

	fn build_function(
		&mut self,
		type_store: &TypeStore,
		context: &'ctx Context,
		module: &mut Module<'ctx>,
		builder: &mut Builder<'ctx>,
		attribute_kinds: &AttributeKinds,
		locations: &mut Vec<Location>,
		function: &Function,
		name: &str,
	) -> FunctionValue<'ctx> {
		self.return_type_buffer.clear();
		self.parameter_type_buffer.clear();
		self.attribute_buffer.clear();
		locations.clear();

		if !function.return_type.is_void(type_store) {
			let mut classes_buffer = sysv_abi::classification_buffer();
			let classes = sysv_abi::classify_type(type_store, &mut classes_buffer, function.return_type);

			self.map_classes_into_return_type_buffer(context, classes.iter());

			if let Some(Class { kind: ClassKind::Memory, .. }) = classes.first() {
				assert_eq!(classes.len(), 1);
				assert_eq!(self.return_type_buffer.len(), 1);

				let ptr_type = context.i8_type().ptr_type(AddressSpace::default());
				self.parameter_type_buffer.push(BasicMetadataTypeEnum::PointerType(ptr_type));

				let attribute = context.create_enum_attribute(attribute_kinds.sret, 0);
				self.attribute_buffer.push(ParameterAttribute { index: 0, attribute });
				self.return_type_buffer.clear(); // Make us use void return type
			}
		}

		for parameter in &function.parameters {
			let mut classes_buffer = sysv_abi::classification_buffer();
			let classes = sysv_abi::classify_type(type_store, &mut classes_buffer, parameter.type_id);
			self.map_classes_into_parameter_type_buffer(context, classes.iter());
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

		let llvm_function = module.add_function(name, fn_type, None);

		for attribute in &self.attribute_buffer {
			llvm_function.add_attribute(inkwell::attributes::AttributeLoc::Param(attribute.index), attribute.attribute);
		}

		llvm_function
	}
}
