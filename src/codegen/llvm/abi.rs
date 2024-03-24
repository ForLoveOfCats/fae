use inkwell::context::Context;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::AddressSpace;

use crate::codegen::amd64::sysv_abi;
use crate::ir::Function;
use crate::type_store::TypeStore;

pub trait LLVMAbi {
	fn build_parameter_types<'ctx>(
		type_store: &TypeStore,
		context: &'ctx Context,
		function: &Function,
		parameter_type_buffer: &mut Vec<BasicMetadataTypeEnum<'ctx>>,
	);
}

pub struct SysvAbi;

impl LLVMAbi for SysvAbi {
	fn build_parameter_types<'ctx>(
		type_store: &TypeStore,
		context: &'ctx Context,
		function: &Function,
		parameter_type_buffer: &mut Vec<BasicMetadataTypeEnum<'ctx>>,
	) {
		for parameter in &function.parameters {
			let mut classes_buffer = sysv_abi::classification_buffer();
			let classes = sysv_abi::classify_type(type_store, &mut classes_buffer, parameter.type_id);

			for class in classes {
				match class.kind {
					sysv_abi::ClassKind::Integer => {
						let llvm_type = match class.size {
							1 => context.i8_type(),
							2 => context.i16_type(),
							4 => context.i32_type(),
							8 => context.i64_type(),
							unknown_size => panic!("{unknown_size}"),
						};
						parameter_type_buffer.push(BasicMetadataTypeEnum::IntType(llvm_type));
					}

					sysv_abi::ClassKind::SSE | sysv_abi::ClassKind::SSEUp => {
						let llvm_type = match class.size {
							2 => context.f16_type(),
							4 => context.f32_type(),
							8 => context.f64_type(),
							unknown_size => panic!("{unknown_size}"),
						};
						parameter_type_buffer.push(BasicMetadataTypeEnum::FloatType(llvm_type));
					}

					sysv_abi::ClassKind::Memory => {
						assert_eq!(class.size, 8);
						parameter_type_buffer
							.push(BasicMetadataTypeEnum::PointerType(context.i8_type().ptr_type(AddressSpace::default())));
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
}
