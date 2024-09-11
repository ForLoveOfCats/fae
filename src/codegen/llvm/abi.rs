use std::ffi::CString;
use std::u32;

use llvm_sys::core::{
	LLVMAddAttributeAtIndex, LLVMAddCallSiteAttribute, LLVMAddFunction, LLVMAppendBasicBlockInContext, LLVMBuildAlloca,
	LLVMBuildCall2, LLVMBuildFPCast, LLVMBuildLoad2, LLVMBuildMemCpy, LLVMBuildRet, LLVMBuildRetVoid, LLVMBuildSExt,
	LLVMBuildStore, LLVMBuildStructGEP2, LLVMBuildZExt, LLVMConstInt, LLVMCountStructElementTypes, LLVMCreateTypeAttribute,
	LLVMDoubleTypeInContext, LLVMFloatTypeInContext, LLVMFunctionType, LLVMGetParam, LLVMHalfTypeInContext,
	LLVMInt16TypeInContext, LLVMInt1TypeInContext, LLVMInt32TypeInContext, LLVMInt64TypeInContext, LLVMInt8TypeInContext,
	LLVMIntTypeInContext, LLVMPointerTypeInContext, LLVMPositionBuilderAtEnd, LLVMSetLinkage, LLVMStructGetTypeAtIndex,
	LLVMStructTypeInContext, LLVMTypeOf, LLVMVectorType, LLVMVoidTypeInContext,
};
use llvm_sys::prelude::*;
use llvm_sys::{self, LLVMLinkage};

use crate::codegen::amd64::sysv_abi::{self, Class, ClassKind};
use crate::codegen::codegen;
use crate::codegen::llvm::generator::{self, AttributeKinds, BindingKind, LLVMGenerator, LLVMTypes};
use crate::frontend::ir::{Function, FunctionShape};
use crate::frontend::type_store::{Layout, NumericKind, TypeId, TypeStore};

pub trait LLVMAbi
where
	Self: Sized,
{
	fn new() -> Self;

	fn define_function(
		&mut self,
		type_store: &mut TypeStore,
		context: LLVMContextRef,
		module: LLVMModuleRef,
		builder: LLVMBuilderRef,
		attribute_kinds: &AttributeKinds,
		llvm_types: &LLVMTypes,
		function_shape: &FunctionShape,
		function: &Function,
		subroutine: LLVMMetadataRef,
	) -> DefinedFunction;

	fn call_function(
		&mut self,
		generator: &LLVMGenerator<Self>,
		type_store: &mut TypeStore,
		attribute_kinds: &AttributeKinds,
		function: &DefinedFunction,
		arguments: &[Option<generator::Binding>],
	) -> Option<generator::Binding>
	where
		Self: Sized;

	fn return_value<'a, 'b>(
		codegen_context: &mut codegen::Context<'a, 'b>,
		generator: &mut generator::LLVMGenerator<Self>,
		llvm_function: LLVMValueRef,
		return_type: FunctionReturnType,
		value: Option<generator::Binding>,
		defer_callback: impl FnOnce(&mut codegen::Context<'a, 'b>, &mut generator::LLVMGenerator<Self>),
	);
}

#[derive(Clone, Copy)]
pub enum FunctionReturnType {
	Void,
	ByValue { value_type: LLVMTypeRef, abi_type: LLVMTypeRef },
	ByPointer { pointed_type: LLVMTypeRef, layout: Layout }, // sret
}

pub struct DefinedFunction {
	pub fn_type: LLVMTypeRef,
	pub llvm_function: LLVMValueRef,
	pub return_type: FunctionReturnType,
	pub return_type_id: TypeId,
	pub parameter_information: Vec<Option<ParameterInformation>>,
	pub c_varargs: bool,
	pub parameter_bindings: Vec<Option<generator::Binding>>,
	pub alloca_block: Option<LLVMBasicBlockRef>,      // None for extern functions
	pub logic_begin_block: Option<LLVMBasicBlockRef>, // None for extern functions

	pub subroutine: LLVMMetadataRef,
	pub file_index: u32,
}

struct ParameterAttribute {
	index: u32,
	attribute: LLVMAttributeRef,
}

#[derive(Debug, Clone)]
pub enum ParameterInformation {
	BareValue {
		llvm_type: LLVMTypeRef,
		type_id: TypeId,
		needs_alloca: bool,
	},

	ByPointer {
		pointed_type: LLVMTypeRef,
		pointed_type_id: TypeId,
		layout: Layout,
		needs_alloca: bool,
	},

	Composition(ParameterComposition),
}

#[derive(Debug, Clone)]
pub struct ParameterComposition {
	pub composition_struct: LLVMTypeRef,
	pub actual_type: LLVMTypeRef,
	pub actual_type_id: TypeId,
	pub layout: Layout,
}

pub struct SysvAbi {
	return_type_buffer: Vec<LLVMTypeRef>,
	parameter_type_buffer: Vec<LLVMTypeRef>,
	parameter_composition_field_type_buffer: Vec<LLVMTypeRef>,
	parameter_information_buffer: Vec<Option<ParameterInformation>>,
	attribute_buffer: Vec<ParameterAttribute>,
	argument_value_buffer: Vec<LLVMValueRef>,
}

impl SysvAbi {
	fn map_classes_into_llvm_type_buffer<'a>(
		context: LLVMContextRef,
		buffer: &mut Vec<LLVMTypeRef>,
		iterator: impl Iterator<Item = &'a Class>,
	) {
		for class in iterator {
			match class.kind {
				sysv_abi::ClassKind::Integer => {
					let llvm_type = unsafe {
						match class.size {
							1 => LLVMInt8TypeInContext(context),
							2 => LLVMInt16TypeInContext(context),
							4 => LLVMInt32TypeInContext(context),
							8 => LLVMInt64TypeInContext(context),
							size if size < 8 => LLVMIntTypeInContext(context, size as u32 * 8),
							unknown_size => panic!("{unknown_size}"),
						}
					};
					buffer.push(llvm_type);
				}

				sysv_abi::ClassKind::Boolean => {
					assert_eq!(class.size, 1);
					let llvm_type = unsafe { LLVMInt1TypeInContext(context) };
					buffer.push(llvm_type);
				}

				sysv_abi::ClassKind::SSE | sysv_abi::ClassKind::SSEUp => {
					let llvm_type = unsafe {
						match class.size {
							2 => LLVMHalfTypeInContext(context),
							4 => LLVMFloatTypeInContext(context),
							8 => LLVMDoubleTypeInContext(context),
							unknown_size => panic!("{unknown_size}"),
						}
					};
					buffer.push(llvm_type);
				}

				sysv_abi::ClassKind::SSECombine => {
					// The element type doesn't really matter, it's going to be reinterpreted anyway
					let llvm_type = unsafe {
						match class.size {
							4 => LLVMVectorType(LLVMHalfTypeInContext(context), 2),
							8 => LLVMVectorType(LLVMHalfTypeInContext(context), 4),
							unknown_size => panic!("{unknown_size}"),
						}
					};
					buffer.push(llvm_type);
				}

				sysv_abi::ClassKind::Pointer | sysv_abi::ClassKind::Memory => {
					assert_eq!(class.size, 8);
					let ptr_type = unsafe { LLVMPointerTypeInContext(context, 0) };
					buffer.push(ptr_type);
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
		type_store: &mut TypeStore,
		context: LLVMContextRef,
		llvm_types: &LLVMTypes,
		attribute_kinds: &AttributeKinds,
		parameter_type_id: TypeId,
		is_mutable: bool,
		index: Option<u32>,
	) -> Option<ParameterInformation> {
		let layout = type_store.type_layout(parameter_type_id);
		if layout.size <= 0 {
			return None;
		}

		let mut classes_buffer = sysv_abi::classification_buffer();
		let classes = sysv_abi::classify_type(type_store, &mut classes_buffer, parameter_type_id);
		Self::map_classes_into_llvm_type_buffer(context, &mut self.parameter_type_buffer, classes.iter());

		self.parameter_composition_field_type_buffer.clear();
		Self::map_classes_into_llvm_type_buffer(context, &mut self.parameter_composition_field_type_buffer, classes.iter());

		let is_non_memory = classes.len() == 1 && classes[0].kind != ClassKind::Memory;
		let is_bare_value = is_non_memory && parameter_type_id.is_primative(type_store);
		let is_by_pointer = classes.len() == 1 && classes[0].kind == ClassKind::Memory;

		if is_bare_value {
			assert_eq!(self.parameter_composition_field_type_buffer.len(), 1);
			let llvm_type = llvm_types.type_to_llvm_type(context, type_store, parameter_type_id);
			Some(ParameterInformation::BareValue {
				llvm_type,
				type_id: parameter_type_id,
				needs_alloca: is_mutable,
			})
		} else if is_by_pointer {
			assert_eq!(self.parameter_composition_field_type_buffer.len(), 1);
			let pointed_type = llvm_types.type_to_llvm_type(context, type_store, parameter_type_id);

			if let Some(index) = index {
				let attribute = unsafe { LLVMCreateTypeAttribute(context, attribute_kinds.byval, pointed_type) };
				self.attribute_buffer.push(ParameterAttribute { index, attribute });
			}

			Some(ParameterInformation::ByPointer {
				pointed_type,
				pointed_type_id: parameter_type_id,
				layout,
				needs_alloca: is_mutable,
			})
		} else {
			let composition_struct = unsafe {
				LLVMStructTypeInContext(
					context,
					self.parameter_composition_field_type_buffer.as_mut_ptr(),
					self.parameter_composition_field_type_buffer.len() as u32,
					false as _,
				)
			};
			let actual_type = llvm_types.type_to_llvm_type(context, type_store, parameter_type_id);
			let actual_type_id = parameter_type_id;
			let composition = ParameterComposition { composition_struct, actual_type, actual_type_id, layout };
			Some(ParameterInformation::Composition(composition))
		}
	}

	fn generate_argument(
		&mut self,
		generator: &LLVMGenerator<Self>,
		attribute_kinds: &AttributeKinds,
		value: generator::Binding,
		information: &ParameterInformation,
		index: Option<u32>,
	) {
		let composition = match information {
			ParameterInformation::Composition(composition) => composition,

			&ParameterInformation::ByPointer { pointed_type, layout, .. } => {
				match value.kind {
					generator::BindingKind::Value(value) => {
						let alloca = generator.build_alloca(pointed_type, c"generate_argument.by_pointer.from_value");
						unsafe { LLVMBuildStore(generator.builder, value, alloca) };
						self.argument_value_buffer.push(alloca);
					}

					generator::BindingKind::Pointer { pointer, pointed_type: ty } => {
						assert_eq!(pointed_type, ty);
						let alloca = generator.build_alloca(pointed_type, c"generate_argument.by_pointer.from_implicit_pointer");
						unsafe {
							let size = LLVMConstInt(LLVMInt64TypeInContext(generator.context), layout.size as u64, false as _);
							let align = layout.alignment as u32;
							LLVMBuildMemCpy(generator.builder, alloca, align, pointer, align, size);
						}
						self.argument_value_buffer.push(alloca);
					}
				};

				if let Some(index) = index {
					let attribute = unsafe { LLVMCreateTypeAttribute(generator.context, attribute_kinds.byval, pointed_type) };
					self.attribute_buffer.push(ParameterAttribute { index, attribute });
				}

				return;
			}

			ParameterInformation::BareValue { .. } => {
				let value = value.to_value(generator.builder);
				self.argument_value_buffer.push(value);
				return;
			}
		};

		let pointer = match value.kind {
			generator::BindingKind::Value(value) => unsafe {
				let alloca = generator.build_alloca(LLVMTypeOf(value), c"generate_argument.composition.from_value");
				LLVMBuildStore(generator.builder, value, alloca);
				alloca
			},

			generator::BindingKind::Pointer { pointer, .. } => pointer,
		};

		let composition = composition.composition_struct;
		let composition_field_count = unsafe { LLVMCountStructElementTypes(composition) };
		for field_index in 0..composition_field_count {
			unsafe {
				let field_type = LLVMStructGetTypeAtIndex(composition, field_index);
				let field_pointer = LLVMBuildStructGEP2(
					generator.builder,
					composition,
					pointer,
					field_index,
					c"generate_argument.composition.field_pointer".as_ptr(),
				);
				let value = LLVMBuildLoad2(
					generator.builder,
					field_type,
					field_pointer,
					c"generate_argument.composition.field_value".as_ptr(),
				);
				self.argument_value_buffer.push(value);
			}
		}
	}
}

impl LLVMAbi for SysvAbi {
	fn new() -> Self {
		SysvAbi {
			return_type_buffer: Vec::new(),
			parameter_type_buffer: Vec::new(),
			parameter_composition_field_type_buffer: Vec::new(),
			parameter_information_buffer: Vec::new(),
			attribute_buffer: Vec::new(),
			argument_value_buffer: Vec::new(),
		}
	}

	fn define_function(
		&mut self,
		type_store: &mut TypeStore,
		context: LLVMContextRef,
		module: LLVMModuleRef,
		builder: LLVMBuilderRef,
		attribute_kinds: &AttributeKinds,
		llvm_types: &LLVMTypes,
		function_shape: &FunctionShape,
		function: &Function,
		subroutine: LLVMMetadataRef,
	) -> DefinedFunction {
		self.return_type_buffer.clear();
		self.parameter_type_buffer.clear();
		self.attribute_buffer.clear();

		let file_index = function_shape.name.span.file_index;

		let return_type_id = function.return_type;
		let return_type = if type_store.type_layout(function.return_type).size > 0 {
			let return_type = llvm_types.type_to_llvm_type(context, type_store, function.return_type);

			let mut classes_buffer = sysv_abi::classification_buffer();
			let classes = sysv_abi::classify_type(type_store, &mut classes_buffer, function.return_type);
			Self::map_classes_into_llvm_type_buffer(context, &mut self.return_type_buffer, classes.iter());

			if let Some(Class { kind: ClassKind::Memory, .. }) = classes.first() {
				assert_eq!(classes.len(), 1);
				assert_eq!(self.return_type_buffer.len(), 1);

				let ptr_type = unsafe { LLVMPointerTypeInContext(context, 0) };
				self.parameter_type_buffer.push(ptr_type);

				let attribute = unsafe { LLVMCreateTypeAttribute(context, attribute_kinds.sret, ptr_type) };
				self.attribute_buffer.push(ParameterAttribute { index: 0, attribute });
				self.return_type_buffer.clear(); // Make us use void return type

				let layout = type_store.type_layout(function.return_type);
				FunctionReturnType::ByPointer { pointed_type: return_type, layout }
			} else {
				let abi_type = if let [abi_type] = self.return_type_buffer.as_slice() {
					*abi_type
				} else {
					let struct_type = unsafe {
						LLVMStructTypeInContext(
							context,
							self.return_type_buffer.as_mut_ptr(),
							self.return_type_buffer.len() as u32,
							false as _,
						)
					};
					struct_type
				};

				FunctionReturnType::ByValue { value_type: return_type, abi_type }
			}
		} else {
			FunctionReturnType::Void
		};

		let has_sret = matches!(return_type, FunctionReturnType::ByPointer { .. });
		let maybe_sret = if has_sret { 1 } else { 0 };

		self.parameter_composition_field_type_buffer.clear();
		self.parameter_information_buffer.clear();

		for (index, parameter) in function.parameters.iter().enumerate() {
			let information = self.construct_parameter_information(
				type_store,
				context,
				llvm_types,
				attribute_kinds,
				parameter.type_id,
				parameter.is_mutable,
				Some(index as u32 + maybe_sret),
			);
			self.parameter_information_buffer.push(information);
		}

		let varargs = function_shape.c_varargs;
		let fn_type = match return_type {
			FunctionReturnType::Void | FunctionReturnType::ByPointer { .. } => unsafe {
				LLVMFunctionType(
					LLVMVoidTypeInContext(context),
					self.parameter_type_buffer.as_mut_ptr(),
					self.parameter_type_buffer.len() as u32,
					varargs as _,
				)
			},

			FunctionReturnType::ByValue { abi_type, .. } => unsafe {
				LLVMFunctionType(
					abi_type,
					self.parameter_type_buffer.as_mut_ptr(),
					self.parameter_type_buffer.len() as u32,
					varargs as _,
				)
			},
		};

		if let Some(extern_attribute) = function_shape.extern_attribute {
			let name = CString::new(extern_attribute.item.name).unwrap();
			let llvm_function = unsafe { LLVMAddFunction(module, name.as_ptr(), fn_type) };
			unsafe { LLVMSetLinkage(llvm_function, LLVMLinkage::LLVMExternalLinkage) };
			return DefinedFunction {
				fn_type,
				llvm_function,
				return_type,
				return_type_id,
				parameter_information: self.parameter_information_buffer.clone(),
				c_varargs: function_shape.c_varargs,
				parameter_bindings: Vec::new(),
				alloca_block: None,
				logic_begin_block: None,
				subroutine,
				file_index,
			};
		}

		let (name, linkage) = if let Some(export_attribute) = function_shape.export_attribute {
			(CString::new(export_attribute.item.name).unwrap(), LLVMLinkage::LLVMDLLExportLinkage)
		} else if function_shape.is_main {
			(CString::from(c"fae_user_main"), LLVMLinkage::LLVMDLLExportLinkage)
		} else {
			let name = format!("fae_function_{}", function_shape.name.item);
			(CString::new(name).unwrap(), LLVMLinkage::LLVMDLLExportLinkage)
		};

		let llvm_function = unsafe {
			let llvm_function = LLVMAddFunction(module, name.as_ptr(), fn_type);
			LLVMSetLinkage(llvm_function, linkage);

			for attribute in &self.attribute_buffer {
				LLVMAddAttributeAtIndex(llvm_function, attribute.index + 1, attribute.attribute);
			}

			llvm_function
		};

		let alloca_block = unsafe { LLVMAppendBasicBlockInContext(context, llvm_function, c"alloca_block".as_ptr()) };
		let logic_begin_block = unsafe { LLVMAppendBasicBlockInContext(context, llvm_function, c"logic_begin_block".as_ptr()) };
		unsafe { LLVMPositionBuilderAtEnd(builder, logic_begin_block) };

		let mut parameter_bindings = Vec::with_capacity(self.parameter_information_buffer.len());
		let mut parameter_index = if matches!(return_type, FunctionReturnType::ByPointer { .. }) {
			1 // Skip processing first parameter, it is the invisible sret pointer
		} else {
			0
		};

		for information in &self.parameter_information_buffer {
			let Some(information) = information else {
				parameter_bindings.push(None);
				parameter_index += 1;
				continue;
			};

			let composition = match information {
				&ParameterInformation::BareValue { llvm_type, type_id, needs_alloca } => {
					let parameter = unsafe { LLVMGetParam(llvm_function, parameter_index) };

					let kind = if needs_alloca {
						let alloca = unsafe {
							LLVMPositionBuilderAtEnd(builder, alloca_block);
							let alloca = LLVMBuildAlloca(builder, llvm_type, c"".as_ptr());
							LLVMPositionBuilderAtEnd(builder, logic_begin_block);
							LLVMBuildStore(builder, parameter, alloca);
							alloca
						};

						generator::BindingKind::Pointer { pointer: alloca, pointed_type: llvm_type }
					} else {
						generator::BindingKind::Value(parameter)
					};

					parameter_bindings.push(Some(generator::Binding { type_id, kind }));
					parameter_index += 1;
					continue;
				}

				&ParameterInformation::ByPointer { pointed_type, pointed_type_id, needs_alloca, .. } => {
					let parameter = unsafe { LLVMGetParam(llvm_function, parameter_index) };

					let kind = if needs_alloca {
						let alloca = unsafe {
							LLVMPositionBuilderAtEnd(builder, alloca_block);
							let alloca = LLVMBuildAlloca(builder, pointed_type, c"".as_ptr());
							LLVMPositionBuilderAtEnd(builder, logic_begin_block);
							let value = LLVMBuildLoad2(builder, pointed_type, parameter, c"".as_ptr());
							LLVMBuildStore(builder, value, alloca);
							alloca
						};

						generator::BindingKind::Pointer { pointer: alloca, pointed_type }
					} else {
						generator::BindingKind::Pointer { pointer: parameter, pointed_type }
					};

					let binding = generator::Binding { type_id: pointed_type_id, kind };
					parameter_bindings.push(Some(binding));
					parameter_index += 1;
					continue;
				}

				ParameterInformation::Composition(composition) => composition,
			};

			assert!(composition.layout.size > 0, "{:?}", composition.layout);

			let alloca = unsafe {
				LLVMPositionBuilderAtEnd(builder, alloca_block);
				let alloca = LLVMBuildAlloca(builder, composition.actual_type, c"".as_ptr());
				LLVMPositionBuilderAtEnd(builder, logic_begin_block);
				alloca
			};

			let pointed_type = composition.actual_type;
			let type_id = composition.actual_type_id;
			let kind = generator::BindingKind::Pointer { pointer: alloca, pointed_type };
			parameter_bindings.push(Some(generator::Binding { type_id, kind }));

			let composition = composition.composition_struct;
			let composition_field_count = unsafe { LLVMCountStructElementTypes(composition) };
			for field_index in 0..composition_field_count as u32 {
				unsafe {
					let ptr = LLVMBuildStructGEP2(builder, composition, alloca, field_index, c"".as_ptr());
					let parameter = LLVMGetParam(llvm_function, parameter_index);
					LLVMBuildStore(builder, parameter, ptr);
				}

				parameter_index += 1;
			}
		}

		DefinedFunction {
			fn_type,
			llvm_function,
			return_type,
			return_type_id,
			parameter_information: self.parameter_information_buffer.clone(),
			c_varargs: function_shape.c_varargs,
			parameter_bindings,
			alloca_block: Some(alloca_block),
			logic_begin_block: Some(logic_begin_block),
			subroutine,
			file_index,
		}
	}

	fn call_function(
		&mut self,
		generator: &LLVMGenerator<Self>,
		type_store: &mut TypeStore,
		attribute_kinds: &AttributeKinds,
		function: &DefinedFunction,
		arguments: &[Option<generator::Binding>],
	) -> Option<generator::Binding> {
		self.argument_value_buffer.clear();
		self.attribute_buffer.clear();
		let context = generator.context;

		let sret_alloca = if let FunctionReturnType::ByPointer { pointed_type, .. } = function.return_type {
			let alloca = generator.build_alloca(pointed_type, c"call_function.sret_argument");
			self.argument_value_buffer.push(alloca);
			Some(alloca)
		} else {
			None
		};
		let maybe_sret = if sret_alloca.is_some() { 1 } else { 0 };

		if function.c_varargs {
			assert!(arguments.len() >= function.parameter_information.len());
		} else {
			assert_eq!(arguments.len(), function.parameter_information.len());
		}

		for ((index, &value), information) in arguments.iter().enumerate().zip(function.parameter_information.iter()) {
			let (Some(value), Some(information)) = (value, information) else {
				assert_eq!(value.is_none(), information.is_none(), "{value:?}, {information:?}");
				continue;
			};

			self.generate_argument(generator, attribute_kinds, value, information, Some(index as u32 + maybe_sret));
		}

		if function.c_varargs {
			let remaining_arguments = &arguments[function.parameter_information.len()..];
			for &argument in remaining_arguments {
				let Some(argument) = argument else {
					continue;
				};

				let llvm_types = &generator.llvm_types;
				let constructed = self.construct_parameter_information(
					type_store,
					context,
					llvm_types,
					attribute_kinds,
					argument.type_id,
					false,
					None,
				);
				let information = constructed.unwrap();

				// Some numeric varargs need to be widened before passing
				if let ParameterInformation::BareValue { type_id, .. } = information {
					if let Some(numeric_kind) = type_id.numeric_kind(type_store) {
						match numeric_kind {
							// Sign extend
							NumericKind::I8 | NumericKind::I16 => {
								let int = argument.to_value(generator.builder);
								let widened = unsafe {
									LLVMBuildSExt(generator.builder, int, LLVMInt32TypeInContext(context), c"".as_ptr())
								};
								self.argument_value_buffer.push(widened);
								continue;
							}

							// Zero extend
							NumericKind::U8 | NumericKind::U16 => {
								let int = argument.to_value(generator.builder);
								let widened = unsafe {
									LLVMBuildZExt(generator.builder, int, LLVMInt32TypeInContext(context), c"".as_ptr())
								};
								self.argument_value_buffer.push(widened);
								continue;
							}

							// TODO: Add f16 once added to the language
							NumericKind::F32 => {
								let float = argument.to_value(generator.builder);
								// TODO: Should this be a FPExtend instead?
								let double = unsafe {
									LLVMBuildFPCast(generator.builder, float, LLVMDoubleTypeInContext(context), c"".as_ptr())
								};
								self.argument_value_buffer.push(double);
								continue;
							}

							_ => {}
						}
					}

					// Zero extend
					if type_id.is_bool(type_store) {
						let int = argument.to_value(generator.builder);
						let llvm_type = unsafe { LLVMInt32TypeInContext(context) };
						let widened = unsafe { LLVMBuildZExt(generator.builder, int, llvm_type, c"".as_ptr()) };
						self.argument_value_buffer.push(widened);
						continue;
					}
				}

				self.generate_argument(generator, attribute_kinds, argument, &information, None);
			}
		}

		let callsite_value = unsafe {
			LLVMBuildCall2(
				generator.builder,
				function.fn_type,
				function.llvm_function,
				self.argument_value_buffer.as_mut_ptr(),
				self.argument_value_buffer.len() as u32,
				c"".as_ptr(),
			)
		};

		for attribute in &self.attribute_buffer {
			unsafe { LLVMAddCallSiteAttribute(callsite_value, attribute.index + 1, attribute.attribute) };
		}

		let type_id = function.return_type_id;
		match function.return_type {
			FunctionReturnType::Void => None,

			FunctionReturnType::ByValue { value_type, .. } => {
				// TODO: Why does this alloca?
				let alloca = generator.build_alloca(value_type, c"call_function.return_value.by_value");
				unsafe { LLVMBuildStore(generator.builder, callsite_value, alloca) };

				let kind = generator::BindingKind::Pointer { pointer: alloca, pointed_type: value_type };
				Some(generator::Binding { type_id, kind })
			}

			FunctionReturnType::ByPointer { pointed_type, .. } => {
				let pointer = sret_alloca.expect("Must be Some if returning by pointer");
				let kind = generator::BindingKind::Pointer { pointer, pointed_type };
				Some(generator::Binding { type_id, kind })
			}
		}
	}

	fn return_value<'a, 'b>(
		codegen_context: &mut codegen::Context<'a, 'b>,
		generator: &mut LLVMGenerator<Self>,
		llvm_function: LLVMValueRef,
		return_type: FunctionReturnType,
		value: Option<generator::Binding>,
		defer_callback: impl FnOnce(&mut codegen::Context<'a, 'b>, &mut generator::LLVMGenerator<Self>),
	) {
		match return_type {
			FunctionReturnType::Void => {
				assert!(value.is_none(), "{value:?}");
				defer_callback(codegen_context, generator);
				unsafe { LLVMBuildRetVoid(generator.builder) };
			}

			FunctionReturnType::ByValue { abi_type, .. } => {
				let value = value.unwrap();
				let pointer = match value.kind {
					BindingKind::Value(value) => {
						defer_callback(codegen_context, generator);
						unsafe { LLVMBuildRet(generator.builder, value) };
						return;
					}

					BindingKind::Pointer { pointer, .. } => pointer,
				};

				unsafe {
					let value = LLVMBuildLoad2(generator.builder, abi_type, pointer, c"".as_ptr());
					defer_callback(codegen_context, generator);
					LLVMBuildRet(generator.builder, value);
				}
			}

			FunctionReturnType::ByPointer { pointed_type, layout } => {
				let sret_pointer = unsafe { LLVMGetParam(llvm_function, 0) };

				match value.unwrap().kind {
					generator::BindingKind::Value(value) => {
						unsafe { LLVMBuildStore(generator.builder, value, sret_pointer) };
					}

					generator::BindingKind::Pointer { pointer, pointed_type: ty } => unsafe {
						assert_eq!(pointed_type, ty);
						let size = LLVMConstInt(LLVMInt64TypeInContext(generator.context), layout.size as u64, false as _);
						let align = layout.alignment as u32;
						LLVMBuildMemCpy(generator.builder, sret_pointer, align, pointer, align, size);
					},
				}

				defer_callback(codegen_context, generator);
				unsafe { LLVMBuildRetVoid(generator.builder) };
			}
		}
	}
}
