use std::ffi::CString;
use std::os::unix::ffi::OsStrExt;

use llvm_sys::core::{
	LLVMAddCase, LLVMAddGlobal, LLVMAddIncoming, LLVMAppendBasicBlockInContext, LLVMArrayType2, LLVMBasicBlockAsValue,
	LLVMBuildAShr, LLVMBuildAdd, LLVMBuildAlloca, LLVMBuildAnd, LLVMBuildBr, LLVMBuildCall2, LLVMBuildCondBr, LLVMBuildFAdd,
	LLVMBuildFCmp, LLVMBuildFDiv, LLVMBuildFMul, LLVMBuildFNeg, LLVMBuildFPCast, LLVMBuildFPToSI, LLVMBuildFPToUI, LLVMBuildFSub,
	LLVMBuildGEP2, LLVMBuildICmp, LLVMBuildIntCast, LLVMBuildIntToPtr, LLVMBuildLShr, LLVMBuildLoad2, LLVMBuildMemCpy,
	LLVMBuildMul, LLVMBuildNeg, LLVMBuildNot, LLVMBuildOr, LLVMBuildPhi, LLVMBuildPtrToInt, LLVMBuildRetVoid, LLVMBuildSDiv,
	LLVMBuildSIToFP, LLVMBuildSRem, LLVMBuildSelect, LLVMBuildShl, LLVMBuildStore, LLVMBuildStructGEP2, LLVMBuildSub,
	LLVMBuildSwitch, LLVMBuildTrunc, LLVMBuildUDiv, LLVMBuildUIToFP, LLVMBuildURem, LLVMBuildUnreachable, LLVMBuildXor,
	LLVMClearInsertionPosition, LLVMConstInt, LLVMConstNamedStruct, LLVMConstNull, LLVMConstReal, LLVMConstStringInContext,
	LLVMCreateBuilderInContext, LLVMDoubleTypeInContext, LLVMFloatTypeInContext, LLVMFunctionType, LLVMGetBasicBlockParent,
	LLVMGetBasicBlockTerminator, LLVMGetEnumAttributeKindForName, LLVMGetInlineAsm, LLVMGetInsertBlock, LLVMGetIntTypeWidth,
	LLVMGetTypeKind, LLVMInt16TypeInContext, LLVMInt1TypeInContext, LLVMInt32TypeInContext, LLVMInt64TypeInContext,
	LLVMInt8TypeInContext, LLVMModuleCreateWithNameInContext, LLVMPointerTypeInContext, LLVMPositionBuilderAtEnd,
	LLVMRemoveBasicBlockFromParent, LLVMSetGlobalConstant, LLVMSetInitializer, LLVMSetLinkage, LLVMSetUnnamedAddress,
	LLVMSetValueName2, LLVMSetVisibility, LLVMStructGetTypeAtIndex, LLVMStructTypeInContext, LLVMTypeOf, LLVMVoidTypeInContext,
};
use llvm_sys::debuginfo::{
	LLVMCreateDIBuilder, LLVMDIBuilderCreateCompileUnit, LLVMDIBuilderCreateFile, LLVMDIBuilderCreateFunction,
	LLVMDIBuilderCreateSubroutineType, LLVMDIFlagPrototyped, LLVMDIFlagZero, LLVMSetSubprogram,
};
use llvm_sys::prelude::{
	LLVMBasicBlockRef, LLVMBuilderRef, LLVMContextRef, LLVMDIBuilderRef, LLVMMetadataRef, LLVMModuleRef, LLVMTypeRef,
	LLVMValueRef,
};
use llvm_sys::{LLVMIntPredicate::*, LLVMLinkage, LLVMRealPredicate, LLVMTypeKind::*, LLVMUnnamedAddr, LLVMVisibility};

use crate::codegen::codegen;
use crate::codegen::generator::Generator;
use crate::codegen::llvm::abi::{DefinedFunction, LLVMAbi};
use crate::codegen::llvm::debug_scope::DebugScope;
use crate::frontend::file::SourceFile;
use crate::frontend::function_store::FunctionStore;
use crate::frontend::ir::{Block, CheckIs, Expression, Function, FunctionId, IfElseChain};
use crate::frontend::lang_items::LangItems;
use crate::frontend::span::DebugLocation;
use crate::frontend::symbols::Statics;
use crate::frontend::tree::BinaryOperator;
use crate::frontend::type_store::{NumericKind, PrimativeKind, TypeEntryKind, TypeId, TypeStore, UserTypeKind};

pub struct AttributeKinds {
	pub sret: u32,
}

impl AttributeKinds {
	fn new() -> AttributeKinds {
		fn kind(name: &str) -> u32 {
			let kind = unsafe { LLVMGetEnumAttributeKindForName(name.as_ptr() as _, name.len()) };
			assert_ne!(kind, 0);
			kind
		}

		AttributeKinds { sret: kind("sret") }
	}
}

#[derive(Debug, PartialEq, Eq)]
enum State {
	InModule,
	InFunction { function_id: FunctionId, void_returning: bool },
}

#[derive(Debug, Clone, Copy)]
pub struct Binding {
	pub type_id: TypeId,
	pub kind: BindingKind,
}

#[derive(Debug, Clone, Copy)]
pub enum BindingKind {
	Value(LLVMValueRef),

	Pointer { pointer: LLVMValueRef, pointed_type: LLVMTypeRef },
}

struct ValuePointer {
	pointer: LLVMValueRef,
	pointed_type: LLVMTypeRef,
	type_id: TypeId,
}

impl Binding {
	pub fn to_value(self, builder: LLVMBuilderRef) -> LLVMValueRef {
		let (pointer, pointed_type) = match self.kind {
			BindingKind::Value(value) => return value,
			BindingKind::Pointer { pointer, pointed_type } => (pointer, pointed_type),
		};

		unsafe { LLVMBuildLoad2(builder, pointed_type, pointer, c"".as_ptr()) }
	}
}

#[derive(Clone, Copy)]
struct UserTypeStruct {
	actual: LLVMTypeRef,
	as_enum_shared_fields: Option<LLVMTypeRef>,
}

pub struct LLVMTypes {
	pub opaque_pointer: LLVMTypeRef,
	pub slice_struct: LLVMTypeRef,
	pub range_struct: LLVMTypeRef,

	user_type_structs: Vec<Vec<Option<UserTypeStruct>>>,
}

impl LLVMTypes {
	fn new(context: LLVMContextRef) -> Self {
		unsafe {
			let opaque_pointer = LLVMPointerTypeInContext(context, 0);
			let i64_type = LLVMInt64TypeInContext(context);

			let slice_struct = LLVMStructTypeInContext(context, [opaque_pointer, i64_type].as_mut_ptr(), 2, false as _);
			let range_struct = LLVMStructTypeInContext(context, [i64_type, i64_type].as_mut_ptr(), 2, false as _);

			LLVMTypes {
				opaque_pointer,
				slice_struct,
				range_struct,
				user_type_structs: Vec::new(),
			}
		}
	}

	pub fn size_to_int_type(context: LLVMContextRef, size: i64) -> LLVMTypeRef {
		unsafe {
			match size {
				1 => LLVMInt8TypeInContext(context),
				2 => LLVMInt16TypeInContext(context),
				4 => LLVMInt32TypeInContext(context),
				8 => LLVMInt64TypeInContext(context),
				_ => unreachable!("{size}"),
			}
		}
	}

	// Cannot accept `void` type and assumes that any struct asked for has been registered; does not follow pointers
	pub fn type_to_llvm_type(&self, context: LLVMContextRef, type_store: &mut TypeStore, type_id: TypeId) -> LLVMTypeRef {
		let entry = type_store.type_entries.get(type_id);

		match entry.kind {
			TypeEntryKind::BuiltinType { kind } => match kind {
				PrimativeKind::Numeric(numeric_kind) => {
					use NumericKind::*;
					unsafe {
						match numeric_kind {
							I8 | U8 => LLVMInt8TypeInContext(context),
							I16 | U16 => LLVMInt16TypeInContext(context),
							I32 | U32 => LLVMInt32TypeInContext(context),
							I64 | U64 | ISize | USize => LLVMInt64TypeInContext(context),
							F32 => LLVMFloatTypeInContext(context),
							F64 => LLVMDoubleTypeInContext(context),
						}
					}
				}

				PrimativeKind::Bool => unsafe { LLVMInt1TypeInContext(context) },

				PrimativeKind::String => self.slice_struct,

				PrimativeKind::AnyCollapse
				| PrimativeKind::NoReturn
				| PrimativeKind::Void
				| PrimativeKind::UntypedInteger
				| PrimativeKind::UntypedDecimal => unreachable!("{kind:?}"),
			},

			TypeEntryKind::UserType { shape_index, specialization_index } => {
				let struct_type = self.user_type_structs[shape_index][specialization_index];
				struct_type.unwrap().actual
			}

			TypeEntryKind::Pointer { .. } => self.opaque_pointer,

			TypeEntryKind::Slice(_) => self.slice_struct,

			TypeEntryKind::UserTypeGeneric { .. } | TypeEntryKind::FunctionGeneric { .. } => unreachable!("{:?}", entry.kind),
		}
	}
}

struct BlockFrame {
	intial_readables_len: usize,
}

#[derive(Clone, Copy)]
struct DebugFile {
	file_index: u32,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Architecture {
	Amd64,
	Aarch64,
}

pub struct LLVMGenerator<ABI: LLVMAbi> {
	pub context: LLVMContextRef,
	pub module: LLVMModuleRef,
	pub builder: LLVMBuilderRef,
	pub di_builder: LLVMDIBuilderRef,

	architecture: Architecture,
	abi: Option<ABI>, // I really dislike the lease pattern, oh well
	pub attribute_kinds: AttributeKinds,
	pub llvm_types: LLVMTypes,

	scope_stack: Vec<LLVMMetadataRef>,
	file: Option<DebugFile>,

	state: State,
	block_frames: Vec<BlockFrame>,
	loop_condition_blocks: Vec<LLVMBasicBlockRef>,
	loop_follow_blocks: Vec<LLVMBasicBlockRef>,
	functions: Vec<Vec<Option<DefinedFunction>>>,
	statics: Vec<Binding>,
	readables: Vec<Option<Binding>>,

	_marker: std::marker::PhantomData<ABI>,
}

impl<ABI: LLVMAbi> LLVMGenerator<ABI> {
	pub fn new(context: LLVMContextRef, architecture: Architecture, optimize_artifacts: bool) -> Self {
		let module = unsafe { LLVMModuleCreateWithNameInContext(c"fae_translation_unit_module".as_ptr(), context) };
		let builder = unsafe { LLVMCreateBuilderInContext(context) };
		let llvm_types = LLVMTypes::new(context);

		let di_builder = unsafe {
			let di_builder = LLVMCreateDIBuilder(module);

			let fae_compiler = "Fae Compiler";
			LLVMDIBuilderCreateCompileUnit(
				di_builder,
				llvm_sys::debuginfo::LLVMDWARFSourceLanguage::LLVMDWARFSourceLanguageC,
				LLVMDIBuilderCreateFile(di_builder, c"fae".as_ptr(), 3, c".".as_ptr(), 3),
				fae_compiler.as_ptr() as _,
				fae_compiler.len() as _,
				optimize_artifacts as _,
				c"".as_ptr(),
				0,
				1,
				c"".as_ptr(),
				0,
				llvm_sys::debuginfo::LLVMDWARFEmissionKind::LLVMDWARFEmissionKindFull,
				0,
				false as _,
				false as _,
				c"".as_ptr(),
				0,
				c"1".as_ptr(),
				1,
			);

			di_builder
		};

		LLVMGenerator::<ABI> {
			context,
			module,
			builder,
			di_builder,

			architecture,
			abi: Some(ABI::new()),
			attribute_kinds: AttributeKinds::new(),
			llvm_types,

			file: None,
			scope_stack: Vec::new(),

			state: State::InModule,
			block_frames: Vec::new(),
			loop_condition_blocks: Vec::new(),
			loop_follow_blocks: Vec::new(),
			functions: Vec::new(),
			statics: Vec::new(),
			readables: Vec::new(),

			_marker: std::marker::PhantomData,
		}
	}

	fn create_debug_scope(&self, debug_location: DebugLocation) -> DebugScope {
		let file = self.file.unwrap();
		assert_eq!(file.file_index, debug_location.file_index);

		let scope = self.scope_stack.last().unwrap();
		DebugScope::new(self.context, self.builder, *scope, debug_location)
	}

	fn finalize_function_if_in_function(&mut self) {
		if let State::InFunction { function_id, void_returning } = self.state {
			let terminator = unsafe { LLVMGetBasicBlockTerminator(LLVMGetInsertBlock(self.builder)) };
			if terminator.is_null() {
				if void_returning {
					unsafe { LLVMBuildRetVoid(self.builder) };
				} else {
					unsafe { LLVMBuildUnreachable(self.builder) };
				}
			}

			let function = &self.functions[function_id.function_shape_index][function_id.specialization_index];
			let defined_function = function.as_ref().unwrap();

			let alloca_block = defined_function
				.alloca_block
				.expect("Should only be None for extern functions");
			let logic_begin_block = defined_function
				.logic_begin_block
				.expect("Should only be None for extern functions");

			unsafe {
				LLVMPositionBuilderAtEnd(self.builder, alloca_block);
				LLVMBuildBr(self.builder, logic_begin_block);
				LLVMClearInsertionPosition(self.builder);
			}

			self.scope_stack.pop().unwrap();
		}

		self.file = None;
		self.state = State::InModule;
	}

	pub fn build_alloca(&self, llvm_type: LLVMTypeRef, name: impl Into<CString>) -> LLVMValueRef {
		let name: CString = name.into();
		let function_id = match self.state {
			State::InFunction { function_id, .. } => function_id,
			State::InModule => unreachable!(),
		};

		let original_block = unsafe { LLVMGetInsertBlock(self.builder) };

		let maybe = &self.functions[function_id.function_shape_index][function_id.specialization_index];
		let defined_function = maybe.as_ref().unwrap();
		let alloca_block = defined_function
			.alloca_block
			.expect("Should only be None for extern functions");

		let pointer = unsafe {
			LLVMPositionBuilderAtEnd(self.builder, alloca_block);
			LLVMBuildAlloca(self.builder, llvm_type, name.as_ptr())
		};

		unsafe { LLVMPositionBuilderAtEnd(self.builder, original_block) };
		pointer
	}

	fn value_pointer(&mut self, binding: Binding) -> LLVMValueRef {
		match binding.kind {
			BindingKind::Value(value) => {
				let pointed_type = unsafe { LLVMTypeOf(value) };
				let pointer = self.build_alloca(pointed_type, c"value_pointer");
				unsafe { LLVMBuildStore(self.builder, value, pointer) };
				pointer
			}

			BindingKind::Pointer { pointer, .. } => pointer,
		}
	}

	fn value_auto_deref_pointer(&mut self, type_store: &mut TypeStore, binding: Binding) -> ValuePointer {
		match binding.kind {
			BindingKind::Value(value) => unsafe {
				let llvm_type = LLVMTypeOf(value);
				let type_kind = LLVMGetTypeKind(llvm_type);

				if type_kind == LLVMPointerTypeKind {
					let pointed_type_id = binding.type_id.as_pointed(type_store).unwrap().type_id;
					let pointed_type = self.llvm_types.type_to_llvm_type(self.context, type_store, pointed_type_id);
					ValuePointer { pointer: value, pointed_type, type_id: pointed_type_id }
				} else {
					let pointer = self.build_alloca(llvm_type, c"value_auto_deref_pointer");
					LLVMBuildStore(self.builder, value, pointer);
					ValuePointer { pointer, pointed_type: llvm_type, type_id: binding.type_id }
				}
			},

			BindingKind::Pointer { pointer, pointed_type } => unsafe {
				if LLVMGetTypeKind(pointed_type) == LLVMPointerTypeKind {
					let pointer = LLVMBuildLoad2(self.builder, self.llvm_types.opaque_pointer, pointer, c"".as_ptr());
					let pointed_type_id = binding.type_id.as_pointed(type_store).unwrap().type_id;
					let pointed_type = self.llvm_types.type_to_llvm_type(self.context, type_store, pointed_type_id);
					ValuePointer { pointer, pointed_type, type_id: pointed_type_id }
				} else {
					ValuePointer { pointer, pointed_type, type_id: binding.type_id }
				}
			},
		}
	}
}

impl<ABI: LLVMAbi> Generator for LLVMGenerator<ABI> {
	type Binding = Binding;

	fn register_type_descriptions(&mut self, type_store: &mut TypeStore) {
		assert_eq!(self.llvm_types.user_type_structs.len(), 0);

		for shape in type_store.user_types.read().iter() {
			let specialization_count = match &shape.read().kind {
				UserTypeKind::Struct { shape } => shape.specializations.len(),
				UserTypeKind::Enum { shape } => shape.specializations.len(),
			};

			let specializations = Vec::from_iter((0..specialization_count).map(|_| None));
			self.llvm_types.user_type_structs.push(specializations);
		}

		let mut field_types_buffer = Vec::new();
		let mut shared_field_types_buffer = Vec::new();

		let user_type_generate_order = type_store.user_type_generate_order.clone();
		let user_types = type_store.user_types.clone();
		for &description in user_type_generate_order.read().iter() {
			field_types_buffer.clear();
			shared_field_types_buffer.clear();

			let user_type = user_types.read()[description.shape_index].clone();
			let user_type = user_type.read();
			match &user_type.kind {
				UserTypeKind::Struct { shape } => {
					let specialization = &shape.specializations[description.specialization_index];
					for field in specialization.fields.iter() {
						let llvm_type = self.llvm_types.type_to_llvm_type(self.context, type_store, field.type_id);
						field_types_buffer.push(llvm_type);
					}
				}

				UserTypeKind::Enum { shape } => {
					let specialization = &shape.specializations[description.specialization_index];
					let layout = specialization.layout.unwrap();
					let tag_memory_size = specialization.tag_memory_size.unwrap();

					let tag = LLVMTypes::size_to_int_type(self.context, tag_memory_size);
					field_types_buffer.push(tag);

					let variants_size = layout.size - tag_memory_size;
					assert!(variants_size >= 0);

					if variants_size > 0 {
						assert_eq!(variants_size % layout.alignment, 0);
						let count = variants_size / layout.alignment;
						let item = LLVMTypes::size_to_int_type(self.context, layout.alignment);
						for _ in 0..count {
							field_types_buffer.push(item);
						}
					}

					for (index, field) in specialization.shared_fields.iter().enumerate() {
						if index == 0 {
							shared_field_types_buffer.push(tag);
							continue;
						}

						let llvm_type = self.llvm_types.type_to_llvm_type(self.context, type_store, field.type_id);
						shared_field_types_buffer.push(llvm_type);
					}
				}
			}

			let actual = unsafe {
				LLVMStructTypeInContext(
					self.context,
					field_types_buffer.as_mut_ptr(),
					field_types_buffer.len() as u32,
					false as _,
				)
			};

			let as_enum_shared_fields = unsafe {
				if shared_field_types_buffer.is_empty() {
					None
				} else {
					Some(LLVMStructTypeInContext(
						self.context,
						shared_field_types_buffer.as_mut_ptr(),
						shared_field_types_buffer.len() as u32,
						false as _,
					))
				}
			};

			let user_type_struct = UserTypeStruct { actual, as_enum_shared_fields };
			let specializations = &mut self.llvm_types.user_type_structs[description.shape_index];
			specializations[description.specialization_index] = Some(user_type_struct);
		}
	}

	fn register_statics(&mut self, type_store: &mut TypeStore, statics: &Statics) {
		for static_instance in &statics.statics {
			let llvm_type = self
				.llvm_types
				.type_to_llvm_type(self.context, type_store, static_instance.type_id);

			let extern_attribute = static_instance.extern_attribute.unwrap();
			let name = CString::new(extern_attribute.name).unwrap();
			let global = unsafe { LLVMAddGlobal(self.module, llvm_type, name.as_ptr()) };
			unsafe { LLVMSetLinkage(global, LLVMLinkage::LLVMExternalLinkage) };

			let kind = BindingKind::Value(global);
			let binding = Binding { type_id: static_instance.type_id, kind };
			self.statics.push(binding);
		}
	}

	fn register_functions(
		&mut self,
		source_files: &[SourceFile],
		type_store: &mut TypeStore,
		function_store: &FunctionStore,
		optimizing: bool,
	) {
		assert_eq!(self.functions.len(), 0);

		let function_count = function_store.shapes.read().len();
		for function_shape_index in 0..function_count {
			let lock = function_store.shapes.read()[function_shape_index].as_ref().unwrap().clone();
			let shape = lock.read();
			if shape.intrinsic_attribute.is_some() {
				self.functions.push(Vec::new());
				continue;
			}

			let mut specializations = Vec::with_capacity(shape.specializations.len());

			for specialization_index in 0..shape.specializations.len() {
				let specialization = &shape.specializations[specialization_index];
				if specialization.generic_poisoned {
					specializations.push(None);
					continue;
				}

				let span = shape.name.span;
				let file = &source_files[span.file_index as usize];
				let file_name = file.path.file_name().unwrap();
				let directory = file.path.parent().unwrap();

				let name = shape.name.item;
				let is_definition = shape.extern_attribute.is_none();

				let file = unsafe {
					LLVMDIBuilderCreateFile(
						self.di_builder,
						file_name.as_bytes().as_ptr() as _,
						file_name.len(),
						directory.as_os_str().as_bytes().as_ptr() as _,
						directory.as_os_str().len(),
					)
				};

				assert_eq!(self.state, State::InModule);
				let function_id = FunctionId { function_shape_index, specialization_index };
				let void_returning = specialization.return_type.is_void(type_store);
				self.state = State::InFunction { function_id, void_returning };

				let debug_location = shape.name.span.debug_location();
				let subroutine = unsafe {
					// TODO: Add parameter types
					let null = std::ptr::null_mut();
					let di_function_type = LLVMDIBuilderCreateSubroutineType(self.di_builder, file, null, 0, LLVMDIFlagZero);

					LLVMDIBuilderCreateFunction(
						self.di_builder,
						file, // Scope, probably wrong?
						name.as_ptr() as _,
						name.len(),
						name.as_ptr() as _,
						name.len(),
						file,
						debug_location.line, // Line number
						di_function_type,
						false as _, // Unit local
						is_definition as _,
						debug_location.line, // Scope line
						LLVMDIFlagPrototyped,
						optimizing as _,
					)
				};

				let mut abi = self.abi.take().unwrap();
				let defined_function = abi.define_function(
					type_store,
					self.context,
					self.module,
					self.builder,
					&self.attribute_kinds,
					&self.llvm_types,
					&shape,
					specialization,
					subroutine,
				);
				self.abi = Some(abi);

				unsafe { LLVMSetSubprogram(defined_function.llvm_function, subroutine) };

				specializations.push(Some(defined_function));
				self.state = State::InModule
			}

			self.functions.push(specializations);
		}
	}

	fn start_block(&mut self) {
		let frame = BlockFrame { intial_readables_len: self.readables.len() };
		self.block_frames.push(frame);
	}

	fn end_block(&mut self) {
		let frame = self.block_frames.pop().expect("Every end must match a start");
		self.readables.truncate(frame.intial_readables_len);
	}

	fn start_function(&mut self, type_store: &TypeStore, function: &Function, function_id: FunctionId) {
		self.finalize_function_if_in_function();

		let maybe_function = &self.functions[function_id.function_shape_index][function_id.specialization_index];
		let defined_function = maybe_function.as_ref().unwrap();
		let logic_begin_block = defined_function
			.logic_begin_block
			.expect("Should only be None for extern functions");
		unsafe { LLVMPositionBuilderAtEnd(self.builder, logic_begin_block) };

		self.readables.clear();
		self.readables.extend_from_slice(&defined_function.initial_values);

		self.scope_stack.push(defined_function.subroutine);
		self.file = Some(DebugFile { file_index: defined_function.file_index });

		let void_returning = function.return_type.is_void(type_store);
		self.state = State::InFunction { function_id, void_returning };
	}

	fn generate_if_else_chain(
		&mut self,
		context: &mut codegen::Context,
		chain_expression: &IfElseChain,
		mut condition_callback: impl FnMut(&mut codegen::Context, &mut Self, &Expression) -> Self::Binding,
		mut body_callback: impl FnMut(&mut codegen::Context, &mut Self, &Block, bool),
	) {
		let original_block = unsafe { LLVMGetInsertBlock(self.builder) };
		let function = unsafe { LLVMGetBasicBlockParent(original_block) };
		let following_block = unsafe { LLVMAppendBasicBlockInContext(self.context, function, c"if_else.following".as_ptr()) };

		let mut next_condition_block =
			unsafe { LLVMAppendBasicBlockInContext(self.context, function, c"if_else.condition".as_ptr()) };
		let mut next_condition_binding_block =
			unsafe { LLVMAppendBasicBlockInContext(self.context, function, c"if_else.condition_binding_block".as_ptr()) };

		unsafe { LLVMBuildBr(self.builder, next_condition_binding_block) };

		for entry in &chain_expression.entries {
			let condition_block = next_condition_block;
			let condition_binding_block = next_condition_binding_block;
			let previous_condition_binding_block = context.if_condition_binding_block;
			context.if_condition_binding_block = Some(condition_binding_block);

			next_condition_block =
				unsafe { LLVMAppendBasicBlockInContext(self.context, function, c"if_else.condition".as_ptr()) };
			next_condition_binding_block =
				unsafe { LLVMAppendBasicBlockInContext(self.context, function, c"if_else.condition_binding_block".as_ptr()) };

			unsafe { LLVMPositionBuilderAtEnd(self.builder, condition_block) };
			let condition = condition_callback(context, self, &entry.condition);
			let final_condition_block = unsafe { LLVMGetInsertBlock(self.builder) };
			let condition = condition.to_value(self.builder);

			unsafe { LLVMPositionBuilderAtEnd(self.builder, condition_binding_block) };
			unsafe { LLVMBuildBr(self.builder, condition_block) };
			context.if_condition_binding_block = previous_condition_binding_block;

			let if_block = unsafe { LLVMAppendBasicBlockInContext(self.context, function, c"if_else.if".as_ptr()) };

			unsafe {
				LLVMPositionBuilderAtEnd(self.builder, final_condition_block);
				let zero = LLVMConstNull(LLVMTypeOf(condition));
				let flag = LLVMBuildICmp(self.builder, LLVMIntNE, condition, zero, c"if_else.flag".as_ptr());
				LLVMBuildCondBr(self.builder, flag, if_block, next_condition_binding_block);
			};

			unsafe { LLVMPositionBuilderAtEnd(self.builder, if_block) };
			body_callback(context, self, &entry.body, false);

			unsafe {
				let current_block = LLVMGetInsertBlock(self.builder);
				if LLVMGetBasicBlockTerminator(current_block).is_null() {
					LLVMBuildBr(self.builder, following_block);
				}
			}
		}

		if let Some(body) = &chain_expression.else_body {
			unsafe { LLVMRemoveBasicBlockFromParent(next_condition_block) };
			let block = next_condition_binding_block;
			let else_name = "if_else.else";
			unsafe { LLVMSetValueName2(LLVMBasicBlockAsValue(block), else_name.as_ptr() as _, else_name.len()) };

			unsafe { LLVMPositionBuilderAtEnd(self.builder, block) };
			body_callback(context, self, body, true);

			unsafe {
				let current_block = LLVMGetInsertBlock(self.builder);
				if LLVMGetBasicBlockTerminator(current_block).is_null() {
					LLVMBuildBr(self.builder, following_block);
				}
			}
		} else {
			unsafe { LLVMRemoveBasicBlockFromParent(next_condition_block) };
			let block = next_condition_binding_block;
			let else_name = "if_else.non_existant_else";
			unsafe {
				LLVMSetValueName2(LLVMBasicBlockAsValue(block), else_name.as_ptr() as _, else_name.len());
				LLVMPositionBuilderAtEnd(self.builder, block);
				LLVMBuildBr(self.builder, following_block);
			}
		}

		unsafe { LLVMPositionBuilderAtEnd(self.builder, following_block) };
	}

	fn generate_match<'a>(
		&mut self,
		context: &mut codegen::Context,
		value: Self::Binding,
		enum_shape_index: usize,
		enum_specialization_index: usize,
		match_expression: &crate::frontend::ir::Match,
		mut body_callback: impl FnMut(&mut codegen::Context, &mut Self, &Block),
	) {
		let ValuePointer { pointer, .. } = self.value_auto_deref_pointer(context.type_store, value);
		let i8_type = unsafe { LLVMInt8TypeInContext(self.context) };
		let tag = unsafe { LLVMBuildLoad2(self.builder, i8_type, pointer, c"".as_ptr()) };

		let original_block = unsafe { LLVMGetInsertBlock(self.builder) };
		let function = unsafe { LLVMGetBasicBlockParent(original_block) };
		let else_block = unsafe { LLVMAppendBasicBlockInContext(self.context, function, c"match.else".as_ptr()) };
		let following_block = unsafe { LLVMAppendBasicBlockInContext(self.context, function, c"match.following".as_ptr()) };

		let switch = unsafe { LLVMBuildSwitch(self.builder, tag, else_block, match_expression.arms.len() as u32) };

		for arm in &match_expression.arms {
			let case_block = unsafe { LLVMAppendBasicBlockInContext(self.context, function, c"case".as_ptr()) };

			for info in &arm.variant_infos {
				unsafe {
					let expected = LLVMConstInt(i8_type, info.variant_index as _, false as _);
					LLVMAddCase(switch, expected, case_block);
				}
			}

			self.start_block();

			unsafe {
				LLVMPositionBuilderAtEnd(self.builder, case_block);

				if let Some(new_binding) = &arm.binding {
					assert_eq!(self.readables.len(), new_binding.readable_index);
					let type_id = context.specialize_type_id(new_binding.type_id);
					let layout = context.type_store.type_layout(type_id);

					if layout.size <= 0 {
						self.readables.push(None);
					} else {
						let shape = &self.llvm_types.user_type_structs[enum_shape_index];
						let llvm_struct = shape[enum_specialization_index].unwrap().actual;
						let variant_pointer = LLVMBuildStructGEP2(self.builder, llvm_struct, pointer, 1, c"".as_ptr());

						let pointed_type = self.llvm_types.type_to_llvm_type(self.context, context.type_store, type_id);
						let kind = BindingKind::Pointer { pointer: variant_pointer, pointed_type };
						let binding = Binding { type_id, kind };
						self.readables.push(Some(binding));
					}
				}

				body_callback(context, self, &arm.block);

				let current_block = LLVMGetInsertBlock(self.builder);
				if LLVMGetBasicBlockTerminator(current_block).is_null() {
					LLVMBuildBr(self.builder, following_block);
				}
			}

			self.end_block();
		}

		if let Some(else_arm) = &match_expression.else_arm {
			unsafe {
				LLVMPositionBuilderAtEnd(self.builder, else_block);

				body_callback(context, self, else_arm);

				let current_block = LLVMGetInsertBlock(self.builder);
				if LLVMGetBasicBlockTerminator(current_block).is_null() {
					LLVMBuildBr(self.builder, following_block);
				}
			}
		} else {
			unsafe {
				LLVMPositionBuilderAtEnd(self.builder, else_block);
				LLVMBuildBr(self.builder, following_block);
			}
		}

		unsafe { LLVMPositionBuilderAtEnd(self.builder, following_block) };
	}

	fn generate_while(
		&mut self,
		context: &mut codegen::Context,
		debug_location: DebugLocation,
		condition_callback: impl FnOnce(&mut codegen::Context, &mut Self) -> Self::Binding,
		body_callback: impl FnOnce(&mut codegen::Context, &mut Self),
	) {
		let _debug_scope = self.create_debug_scope(debug_location);

		let original_block = unsafe { LLVMGetInsertBlock(self.builder) };
		let function = unsafe { LLVMGetBasicBlockParent(original_block) };
		let condition_block = unsafe { LLVMAppendBasicBlockInContext(self.context, function, c"while.condition".as_ptr()) };
		self.loop_condition_blocks.push(condition_block);
		let while_block = unsafe { LLVMAppendBasicBlockInContext(self.context, function, c"while.body".as_ptr()) };

		let following_block = unsafe { LLVMAppendBasicBlockInContext(self.context, function, c"while.following".as_ptr()) };
		self.loop_follow_blocks.push(following_block);

		unsafe {
			LLVMBuildBr(self.builder, condition_block);
			LLVMPositionBuilderAtEnd(self.builder, condition_block);
		}

		let condition_binding = condition_callback(context, self);
		let condition = condition_binding.to_value(self.builder);
		unsafe {
			let zero = LLVMConstNull(LLVMTypeOf(condition));
			let flag = LLVMBuildICmp(self.builder, LLVMIntNE, condition, zero, c"while.flag".as_ptr());
			LLVMBuildCondBr(self.builder, flag, while_block, following_block);
		}

		unsafe { LLVMPositionBuilderAtEnd(self.builder, while_block) };
		body_callback(context, self);

		unsafe {
			let current_block = LLVMGetInsertBlock(self.builder);
			if LLVMGetBasicBlockTerminator(current_block).is_null() {
				LLVMBuildBr(self.builder, condition_block);
			}
		}

		unsafe { LLVMPositionBuilderAtEnd(self.builder, following_block) };
		self.loop_condition_blocks.pop();
		self.loop_follow_blocks.pop();
	}

	fn generate_integer_value(&mut self, type_store: &TypeStore, type_id: TypeId, value: i128) -> Self::Binding {
		let value = unsafe {
			match type_id.numeric_kind(type_store).unwrap() {
				NumericKind::I8 | NumericKind::U8 => LLVMConstInt(LLVMInt8TypeInContext(self.context), value as u64, false as _),

				NumericKind::I16 | NumericKind::U16 => {
					LLVMConstInt(LLVMInt16TypeInContext(self.context), value as u64, false as _)
				}

				NumericKind::I32 | NumericKind::U32 => {
					LLVMConstInt(LLVMInt32TypeInContext(self.context), value as u64, false as _)
				}

				NumericKind::I64 | NumericKind::U64 | NumericKind::ISize | NumericKind::USize => {
					LLVMConstInt(LLVMInt64TypeInContext(self.context), value as u64, false as _)
				}

				NumericKind::F32 => LLVMConstReal(LLVMFloatTypeInContext(self.context), value as f64),

				NumericKind::F64 => LLVMConstReal(LLVMDoubleTypeInContext(self.context), value as f64),
			}
		};

		let kind = BindingKind::Value(value);
		Binding { type_id, kind }
	}

	fn generate_decimal_value(&mut self, type_store: &TypeStore, type_id: TypeId, value: f64) -> Self::Binding {
		let value = unsafe {
			match type_id.numeric_kind(type_store).unwrap() {
				NumericKind::F32 => LLVMConstReal(LLVMFloatTypeInContext(self.context), value),
				NumericKind::F64 => LLVMConstReal(LLVMDoubleTypeInContext(self.context), value),
				kind => unreachable!("{kind}"),
			}
		};

		let kind = BindingKind::Value(value);
		Binding { type_id, kind }
	}

	fn generate_boolean_literal(&mut self, type_store: &TypeStore, literal: bool) -> Self::Binding {
		let value = unsafe { LLVMConstInt(LLVMInt1TypeInContext(self.context), literal as u64, false as _) };
		let kind = BindingKind::Value(value);
		Binding { type_id: type_store.bool_type_id(), kind }
	}

	fn generate_string_literal(&mut self, type_store: &TypeStore, text: &str) -> Self::Binding {
		unsafe {
			let array = LLVMConstStringInContext(self.context, text.as_ptr() as _, text.len() as u32, false as _);
			let global = LLVMAddGlobal(self.module, LLVMTypeOf(array), c"string_literal.global".as_ptr());
			LLVMSetInitializer(global, array);
			LLVMSetGlobalConstant(global, true as _);
			LLVMSetVisibility(global, LLVMVisibility::LLVMHiddenVisibility);
			LLVMSetLinkage(global, LLVMLinkage::LLVMPrivateLinkage);
			LLVMSetUnnamedAddress(global, LLVMUnnamedAddr::LLVMGlobalUnnamedAddr);

			let llvm_type = self.llvm_types.slice_struct;
			let len = LLVMConstInt(LLVMInt64TypeInContext(self.context), text.len() as u64, false as _);
			let slice = LLVMConstNamedStruct(llvm_type, [global, len].as_mut_ptr(), 2);

			let type_id = type_store.string_type_id();
			let kind = BindingKind::Value(slice);
			Binding { type_id, kind }
		}
	}

	fn generate_array_literal(
		&mut self,
		type_store: &mut TypeStore,
		elements: &[Self::Binding],
		element_type_id: TypeId,
		slice_type_id: TypeId,
	) -> Self::Binding {
		assert!(!elements.is_empty());

		let element_type = self.llvm_types.type_to_llvm_type(self.context, type_store, element_type_id);
		let array_type = unsafe { LLVMArrayType2(element_type, elements.len() as u64) };
		let alloca = self.build_alloca(array_type, c"generate_array_literal.array_alloca");

		let zero = unsafe { LLVMConstInt(LLVMInt64TypeInContext(self.context), 0, false as _) };
		for (index, element) in elements.iter().enumerate() {
			unsafe {
				let index = LLVMConstInt(LLVMInt64TypeInContext(self.context), index as u64, false as _);
				let pointer = LLVMBuildGEP2(
					self.builder,
					array_type,
					alloca,
					[zero, index].as_mut_ptr(),
					2,
					c"array_literal.pointer".as_ptr(),
				);
				match element.kind {
					BindingKind::Value(value) => {
						LLVMBuildStore(self.builder, value, pointer);
					}

					BindingKind::Pointer { pointer: value_pointer, .. } => {
						let layout = type_store.type_layout(element_type_id);
						let align = layout.alignment as u32;
						let size = LLVMConstInt(LLVMInt64TypeInContext(self.context), layout.size as u64, false as _);
						LLVMBuildMemCpy(self.builder, pointer, align, value_pointer, align, size);
					}
				}
			}
		}

		let slice_type = self.llvm_types.slice_struct;
		let slice_alloca = self.build_alloca(slice_type, c"generate_array_literal.slice_alloca");

		unsafe {
			let pointer_pointer =
				LLVMBuildStructGEP2(self.builder, slice_type, slice_alloca, 0, c"array_literal.pointer_pointer".as_ptr());
			LLVMBuildStore(self.builder, alloca, pointer_pointer);

			let len_pointer =
				LLVMBuildStructGEP2(self.builder, slice_type, slice_alloca, 1, c"array_literal.len_pointer".as_ptr());
			let len = LLVMConstInt(LLVMInt64TypeInContext(self.context), elements.len() as u64, false as _);
			LLVMBuildStore(self.builder, len, len_pointer);
		}

		let pointed_type = self.llvm_types.type_to_llvm_type(self.context, type_store, slice_type_id);

		let kind = BindingKind::Pointer { pointer: slice_alloca, pointed_type };
		Binding { type_id: slice_type_id, kind }
	}

	fn generate_struct_literal(
		&mut self,
		type_id: TypeId,
		shape_index: usize,
		specialization_index: usize,
		fields: &[Self::Binding],
	) -> Self::Binding {
		let struct_type = self.llvm_types.user_type_structs[shape_index][specialization_index]
			.unwrap()
			.actual;

		let alloca = self.build_alloca(struct_type, c"generate_struct_literal.alloca");
		for (index, field) in fields.iter().enumerate() {
			let value = field.to_value(self.builder);
			unsafe {
				let field_pointer = LLVMBuildStructGEP2(
					self.builder,
					struct_type,
					alloca,
					index as u32,
					c"struct_literal.field_pointer".as_ptr(),
				);
				LLVMBuildStore(self.builder, value, field_pointer);
			}
		}

		let kind = BindingKind::Pointer { pointer: alloca, pointed_type: struct_type };
		Binding { type_id, kind }
	}

	fn generate_call(
		&mut self,
		type_store: &mut TypeStore,
		function_id: FunctionId,
		arguments: &[Option<Binding>],
		debug_location: DebugLocation,
	) -> Option<Binding> {
		let _debug_scope = self.create_debug_scope(debug_location);

		let maybe_function = &self.functions[function_id.function_shape_index][function_id.specialization_index];
		let function = maybe_function.as_ref().unwrap();

		let mut abi = self.abi.take().unwrap();
		let binding = abi.call_function(self, type_store, function, arguments);
		self.abi = Some(abi);

		binding
	}

	fn generate_method_call(
		&mut self,
		type_store: &mut TypeStore,
		function_id: FunctionId,
		base_pointer_type_id: TypeId,
		arguments: &mut [Option<Self::Binding>],
		debug_location: DebugLocation,
	) -> Option<Self::Binding> {
		let _debug_scope = self.create_debug_scope(debug_location);

		let maybe_function = &self.functions[function_id.function_shape_index][function_id.specialization_index];
		let function = maybe_function.as_ref().unwrap();

		let base = arguments[0].unwrap();
		let first_argument = match base.kind {
			BindingKind::Value(value) => unsafe {
				let type_kind = LLVMGetTypeKind(LLVMTypeOf(value));
				if type_kind == LLVMPointerTypeKind {
					let kind = BindingKind::Value(value);
					Binding { type_id: base_pointer_type_id, kind }
				} else {
					let pointed_type = LLVMTypeOf(value);
					let pointer = self.build_alloca(pointed_type, c"generate_method_call.self_pointer");
					LLVMBuildStore(self.builder, value, pointer);

					let kind = BindingKind::Value(pointer);
					Binding { type_id: base_pointer_type_id, kind }
				}
			},

			BindingKind::Pointer { pointer, pointed_type } => unsafe {
				let type_kind = LLVMGetTypeKind(pointed_type);
				if type_kind == LLVMPointerTypeKind {
					let pointer = LLVMBuildLoad2(self.builder, pointed_type, pointer, c"".as_ptr());
					let kind = BindingKind::Value(pointer);
					Binding { type_id: base_pointer_type_id, kind }
				} else {
					let kind = BindingKind::Value(pointer);
					Binding { type_id: base_pointer_type_id, kind }
				}
			},
		};
		arguments[0] = Some(first_argument);

		let mut abi = self.abi.take().unwrap();
		let binding = abi.call_function(self, type_store, function, arguments);
		self.abi = Some(abi);

		binding
	}

	fn generate_read(&mut self, readable_index: usize) -> Option<Self::Binding> {
		self.readables[readable_index]
	}

	fn generate_static_read(&mut self, static_index: usize) -> Self::Binding {
		self.statics[static_index]
	}

	fn generate_field_read(
		&mut self,
		type_store: &mut TypeStore,
		base: Self::Binding,
		field_index: usize,
	) -> Option<Self::Binding> {
		let index = field_index as u32;
		let ValuePointer { pointer, mut pointed_type, type_id } = self.value_auto_deref_pointer(type_store, base);

		let field_type;
		let field_pointer;

		let entry = type_store.type_entries.get(type_id);
		let type_id = match entry.kind {
			TypeEntryKind::UserType { shape_index, specialization_index } => {
				let user_type = type_store.user_types.read()[shape_index].clone();
				let user_type = user_type.read();
				match &user_type.kind {
					UserTypeKind::Struct { shape } => {
						field_type = unsafe { LLVMStructGetTypeAtIndex(pointed_type, index) };
						field_pointer = unsafe {
							LLVMBuildStructGEP2(
								self.builder,
								pointed_type,
								pointer,
								index,
								c"field_read.struct_field_pointer".as_ptr(),
							)
						};

						let struct_type = &shape.specializations[specialization_index];
						struct_type.fields[field_index].type_id
					}

					UserTypeKind::Enum { shape } => {
						pointed_type = self.llvm_types.user_type_structs[shape_index][specialization_index]
							.unwrap()
							.as_enum_shared_fields
							.unwrap();

						field_pointer = unsafe {
							LLVMBuildStructGEP2(
								self.builder,
								pointed_type,
								pointer,
								index,
								c"field_read.enum_field_pointer".as_ptr(),
							)
						};

						// HACK: The first field of any enum is the tag which may be represented with various sized
						// integers depending on the padding required so we need to override the type that we will
						// read that field as. Currently that is always a `u8`
						// TODO: Update once enums can set their tag type
						// TODO: This won't work on big-endian systems
						if field_index == 0 {
							field_type = unsafe { LLVMInt8TypeInContext(self.context) };
						} else {
							field_type = unsafe { LLVMStructGetTypeAtIndex(pointed_type, index) }
						}

						let enum_type = &shape.specializations[specialization_index];
						enum_type.shared_fields[field_index].type_id
					}
				}
			}

			TypeEntryKind::BuiltinType { kind: PrimativeKind::String } => {
				if index == 2 {
					field_type = self.llvm_types.slice_struct;
					field_pointer = pointer;
					type_store.u8_slice_type_id()
				} else {
					field_type = unsafe { LLVMStructGetTypeAtIndex(pointed_type, index) };
					field_pointer = unsafe {
						LLVMBuildStructGEP2(
							self.builder,
							pointed_type,
							pointer,
							index,
							c"str_field_read.slice_field_pointer".as_ptr(),
						)
					};

					type_store.usize_type_id()
				}
			}

			TypeEntryKind::Slice(_) => {
				field_type = unsafe { LLVMStructGetTypeAtIndex(pointed_type, index) };
				field_pointer = unsafe {
					LLVMBuildStructGEP2(
						self.builder,
						pointed_type,
						pointer,
						index,
						c"slice_field_read.slice_field_pointer".as_ptr(),
					)
				};

				type_store.usize_type_id()
			}

			kind => unreachable!("{kind:?}, {}", type_store.debugging_type_name(type_id)),
		};

		let kind = BindingKind::Pointer { pointer: field_pointer, pointed_type: field_type };
		Some(Binding { type_id, kind })
	}

	fn generate_negate(&mut self, value: Self::Binding, type_id: TypeId) -> Self::Binding {
		let value = value.to_value(self.builder);

		let negated = unsafe {
			let type_kind = LLVMGetTypeKind(LLVMTypeOf(value));
			if type_kind == LLVMIntegerTypeKind {
				LLVMBuildNeg(self.builder, value, c"negate.negated_integer".as_ptr())
			} else if matches!(type_kind, LLVMFloatTypeKind | LLVMDoubleTypeKind) {
				LLVMBuildFNeg(self.builder, value, c"negate.negated_floating_point".as_ptr())
			} else {
				unreachable!("{value:?}");
			}
		};

		let kind = BindingKind::Value(negated);
		Binding { type_id, kind }
	}

	fn generate_invert(&mut self, value: Self::Binding) -> Self::Binding {
		let type_id = value.type_id;
		let value = value.to_value(self.builder);
		let inverted = unsafe { LLVMBuildNot(self.builder, value, c"invert.inverted".as_ptr()) };
		let kind = BindingKind::Value(inverted);
		Binding { type_id, kind }
	}

	fn generate_address_of(&mut self, base: Self::Binding, pointer_type_id: TypeId) -> Self::Binding {
		let pointer = self.value_pointer(base);
		let kind = BindingKind::Value(pointer);
		Binding { type_id: pointer_type_id, kind }
	}

	fn generate_dereference(
		&mut self,
		type_store: &mut TypeStore,
		base: Self::Binding,
		pointed_type_id: TypeId,
	) -> Self::Binding {
		let pointer = match base.kind {
			BindingKind::Value(value) => value,

			BindingKind::Pointer { pointer, pointed_type } => unsafe {
				LLVMBuildLoad2(self.builder, pointed_type, pointer, c"dereference.loaded".as_ptr())
			},
		};

		let pointed_type = self.llvm_types.type_to_llvm_type(self.context, type_store, pointed_type_id);
		let kind = BindingKind::Pointer { pointer, pointed_type };
		Binding { type_id: pointed_type_id, kind }
	}

	fn generate_cast(&mut self, type_store: &mut TypeStore, base: Self::Binding, to: TypeId) -> Self::Binding {
		let from = base.to_value(self.builder);
		let from_type_kind = unsafe { LLVMGetTypeKind(LLVMTypeOf(from)) };
		let from_pointer = from_type_kind == LLVMPointerTypeKind;
		let to_pointer = to.is_pointer(type_store);

		// Pointer to pointer, nop
		if from_pointer && to_pointer {
			let kind = BindingKind::Value(from);
			return Binding { type_id: to, kind };
		}

		let from_int = from_type_kind == LLVMIntegerTypeKind;

		// Int to pointer
		if from_int && to_pointer {
			let int = from;
			let pointer_type = self.llvm_types.opaque_pointer;
			let pointer = unsafe { LLVMBuildIntToPtr(self.builder, int, pointer_type, c"".as_ptr()) };

			let kind = BindingKind::Value(pointer);
			return Binding { type_id: to, kind };
		}

		let to_kind = to.numeric_kind(type_store).unwrap();
		use NumericKind::*;
		let (to_int_type, to_float_type) = unsafe {
			match to_kind {
				I8 | U8 => (Some(LLVMInt8TypeInContext(self.context)), None),
				I16 | U16 => (Some(LLVMInt16TypeInContext(self.context)), None),
				I32 | U32 => (Some(LLVMInt32TypeInContext(self.context)), None),
				I64 | U64 | ISize | USize => (Some(LLVMInt64TypeInContext(self.context)), None),
				F32 => (None, Some(LLVMFloatTypeInContext(self.context))),
				F64 => (None, Some(LLVMDoubleTypeInContext(self.context))),
			}
		};

		// Pointer to int
		if from_pointer {
			if let Some(to_type) = to_int_type {
				let int = unsafe { LLVMBuildPtrToInt(self.builder, from, to_type, c"".as_ptr()) };
				let kind = BindingKind::Value(int);
				return Binding { type_id: to, kind };
			}
		}

		// Int to int
		if from_int {
			if let Some(to_type) = to_int_type {
				let int = unsafe { LLVMBuildIntCast(self.builder, from, to_type, c"".as_ptr()) };
				let kind = BindingKind::Value(int);
				return Binding { type_id: to, kind };
			}
		}

		let from_kind = base.type_id.numeric_kind(type_store).unwrap();

		// Int to float
		if from_int {
			if let Some(to_type) = to_float_type {
				let int = from;
				let float = if from_kind.is_signed() {
					unsafe { LLVMBuildSIToFP(self.builder, int, to_type, c"".as_ptr()) }
				} else {
					unsafe { LLVMBuildUIToFP(self.builder, int, to_type, c"".as_ptr()) }
				};
				let kind = BindingKind::Value(float);
				return Binding { type_id: to, kind };
			}
		}

		let from_float = matches!(from_type_kind, LLVMFloatTypeKind | LLVMDoubleTypeKind);

		// Float to int
		if from_float {
			if let Some(to_type) = to_int_type {
				let int = if to_kind.is_signed() {
					unsafe { LLVMBuildFPToSI(self.builder, from, to_type, c"".as_ptr()) }
				} else {
					unsafe { LLVMBuildFPToUI(self.builder, from, to_type, c"".as_ptr()) }
				};
				let kind = BindingKind::Value(int);
				return Binding { type_id: to, kind };
			}
		}

		// Float to float
		if from_float {
			if let Some(to_type) = to_float_type {
				let float = unsafe { LLVMBuildFPCast(self.builder, from, to_type, c"".as_ptr()) };
				let kind = BindingKind::Value(float);
				return Binding { type_id: to, kind };
			}
		}

		unreachable!("{from_type_kind:?}")
	}

	fn generate_slice_index(
		&mut self,
		lang_items: &LangItems,
		type_store: &mut TypeStore,
		item_type: TypeId,
		base: Self::Binding,
		index: Self::Binding,
		debug_location: DebugLocation,
	) -> Option<Self::Binding> {
		unsafe {
			let original_block = LLVMGetInsertBlock(self.builder);
			let function = LLVMGetBasicBlockParent(original_block);
			let failure_block = LLVMAppendBasicBlockInContext(self.context, function, c"bounds_check_failure".as_ptr());
			let success_block = LLVMAppendBasicBlockInContext(self.context, function, c"".as_ptr());

			let value_pointer = self.value_pointer(base);

			let pointer_type = self.llvm_types.opaque_pointer;
			let struct_type = self.llvm_types.slice_struct;
			let pointer_pointer = LLVMBuildStructGEP2(self.builder, struct_type, value_pointer, 0, c"".as_ptr());
			let pointer = LLVMBuildLoad2(self.builder, pointer_type, pointer_pointer, c"".as_ptr());

			let i64_type = LLVMInt64TypeInContext(self.context);
			let len_pointer = LLVMBuildStructGEP2(self.builder, struct_type, value_pointer, 1, c"".as_ptr());
			let len = LLVMBuildLoad2(self.builder, i64_type, len_pointer, c"".as_ptr());

			let index = index.to_value(self.builder);

			let zero = LLVMConstNull(i64_type);
			let greater_than_zero = LLVMBuildICmp(self.builder, LLVMIntSGE, index, zero, c"".as_ptr());
			let less_than_len = LLVMBuildICmp(self.builder, LLVMIntSLT, index, len, c"".as_ptr());
			let in_bounds = LLVMBuildAnd(self.builder, greater_than_zero, less_than_len, c"".as_ptr());

			LLVMBuildCondBr(self.builder, in_bounds, success_block, failure_block);

			LLVMPositionBuilderAtEnd(self.builder, failure_block);
			// TODO: Build some abstraction for calling lang item functions
			let failure_args = {
				let kind = BindingKind::Value(len);
				let len = Some(Binding { type_id: type_store.isize_type_id(), kind });
				let kind = BindingKind::Value(index);
				let index = Some(Binding { type_id: type_store.isize_type_id(), kind });
				[len, index]
			};
			self.generate_call(type_store, lang_items.slice_index_out_of_bounds.unwrap(), &failure_args, debug_location);
			LLVMBuildUnreachable(self.builder);

			LLVMPositionBuilderAtEnd(self.builder, success_block);

			let item_layout = type_store.type_layout(item_type);
			if item_layout.size <= 0 {
				return None;
			}

			let indicies = &mut [index];
			let pointed_type = self.llvm_types.type_to_llvm_type(self.context, type_store, item_type);
			let adjusted = LLVMBuildGEP2(
				self.builder,
				pointed_type,
				pointer,
				indicies.as_mut_ptr(),
				indicies.len() as u32,
				c"".as_ptr(),
			);

			let kind = BindingKind::Pointer { pointer: adjusted, pointed_type };
			Some(Binding { type_id: item_type, kind })
		}
	}

	fn generate_slice_slice(
		&mut self,
		lang_items: &LangItems,
		type_store: &mut TypeStore,
		item_type: TypeId,
		base: Self::Binding,
		range: Self::Binding,
		debug_location: DebugLocation,
	) -> Option<Self::Binding> {
		let slice_type_id = base.type_id;

		unsafe {
			let original_block = LLVMGetInsertBlock(self.builder);
			let function = LLVMGetBasicBlockParent(original_block);

			let range_inverted_failure_block = LLVMAppendBasicBlockInContext(
				self.context,
				function,
				c"generate_slice_slice.range_inverted_failure_block".as_ptr(),
			);
			let range_not_inverted_block = LLVMAppendBasicBlockInContext(self.context, function, c"".as_ptr());

			let range_start_out_of_bounds_failure_block = LLVMAppendBasicBlockInContext(
				self.context,
				function,
				c"generate_slice_slice.range_start_out_of_bounds_failure_block".as_ptr(),
			);
			let start_in_bounds_block = LLVMAppendBasicBlockInContext(self.context, function, c"".as_ptr());

			let range_end_out_of_bounds_failure_block = LLVMAppendBasicBlockInContext(
				self.context,
				function,
				c"generate_slice_slice.range_end_out_of_bounds_failure_block".as_ptr(),
			);
			let end_in_bounds_block = LLVMAppendBasicBlockInContext(self.context, function, c"".as_ptr());

			let i64_type = LLVMInt64TypeInContext(self.context);

			let range_pointer = self.value_pointer(range);
			let start_pointer = LLVMBuildStructGEP2(self.builder, self.llvm_types.range_struct, range_pointer, 0, c"".as_ptr());
			let start = LLVMBuildLoad2(self.builder, i64_type, start_pointer, c"generate_slice_slice.range_start".as_ptr());
			let end_pointer = LLVMBuildStructGEP2(self.builder, self.llvm_types.range_struct, range_pointer, 1, c"".as_ptr());
			let end = LLVMBuildLoad2(self.builder, i64_type, end_pointer, c"generate_slice_slice.range_end".as_ptr());

			let inverted = LLVMBuildICmp(self.builder, LLVMIntSGT, start, end, c"".as_ptr());
			LLVMBuildCondBr(self.builder, inverted, range_inverted_failure_block, range_not_inverted_block);

			LLVMPositionBuilderAtEnd(self.builder, range_inverted_failure_block);
			let failure_args = [Some(range)];
			self.generate_call(type_store, lang_items.slice_range_inverted.unwrap(), &failure_args, debug_location);
			LLVMBuildUnreachable(self.builder);

			LLVMPositionBuilderAtEnd(self.builder, range_not_inverted_block);

			let value_pointer = self.value_pointer(base);

			let pointer_type = self.llvm_types.opaque_pointer;
			let struct_type = self.llvm_types.slice_struct;
			let pointer_pointer = LLVMBuildStructGEP2(self.builder, struct_type, value_pointer, 0, c"".as_ptr());
			let pointer = LLVMBuildLoad2(self.builder, pointer_type, pointer_pointer, c"".as_ptr());

			let len_pointer = LLVMBuildStructGEP2(self.builder, struct_type, value_pointer, 1, c"".as_ptr());
			let len = LLVMBuildLoad2(self.builder, i64_type, len_pointer, c"".as_ptr());

			let zero = LLVMConstNull(i64_type);
			let start_greater_than_zero = LLVMBuildICmp(self.builder, LLVMIntSGE, start, zero, c"".as_ptr());
			let start_less_than_len = LLVMBuildICmp(self.builder, LLVMIntSLT, start, len, c"".as_ptr());
			let start_in_bounds = LLVMBuildAnd(self.builder, start_greater_than_zero, start_less_than_len, c"".as_ptr());

			LLVMBuildCondBr(
				self.builder,
				start_in_bounds,
				start_in_bounds_block,
				range_start_out_of_bounds_failure_block,
			);

			LLVMPositionBuilderAtEnd(self.builder, range_start_out_of_bounds_failure_block);
			let failure_args = {
				let kind = BindingKind::Value(len);
				let len = Some(Binding { type_id: type_store.isize_type_id(), kind });
				let kind = BindingKind::Value(start);
				let start = Some(Binding { type_id: type_store.isize_type_id(), kind });
				[len, start]
			};
			self.generate_call(
				type_store,
				lang_items.slice_range_start_out_of_bounds.unwrap(),
				&failure_args,
				debug_location,
			);
			LLVMBuildUnreachable(self.builder);

			LLVMPositionBuilderAtEnd(self.builder, start_in_bounds_block);
			let end_greater_than_zero = LLVMBuildICmp(self.builder, LLVMIntSGE, end, zero, c"".as_ptr());
			let end_less_than_equal_len = LLVMBuildICmp(self.builder, LLVMIntSLE, end, len, c"".as_ptr());
			let end_in_bounds = LLVMBuildAnd(self.builder, end_greater_than_zero, end_less_than_equal_len, c"".as_ptr());

			LLVMBuildCondBr(self.builder, end_in_bounds, end_in_bounds_block, range_end_out_of_bounds_failure_block);

			LLVMPositionBuilderAtEnd(self.builder, range_end_out_of_bounds_failure_block);
			let failure_args = {
				let kind = BindingKind::Value(len);
				let len = Some(Binding { type_id: type_store.isize_type_id(), kind });
				let kind = BindingKind::Value(end);
				let end = Some(Binding { type_id: type_store.isize_type_id(), kind });
				[len, end]
			};
			self.generate_call(
				type_store,
				lang_items.slice_range_end_out_of_bounds.unwrap(),
				&failure_args,
				debug_location,
			);
			LLVMBuildUnreachable(self.builder);

			LLVMPositionBuilderAtEnd(self.builder, end_in_bounds_block);

			let item_layout = type_store.type_layout(item_type);
			if item_layout.size <= 0 {
				return None;
			}

			let indicies = &mut [start];
			let pointed_type = self.llvm_types.type_to_llvm_type(self.context, type_store, item_type);
			let adjusted_pointer = LLVMBuildGEP2(
				self.builder,
				pointed_type,
				pointer,
				indicies.as_mut_ptr(),
				indicies.len() as u32,
				c"".as_ptr(),
			);

			let adjusted_len = LLVMBuildSub(self.builder, end, start, c"".as_ptr());

			let llvm_type = self.llvm_types.slice_struct;
			let alloca = self.build_alloca(llvm_type, c"generate_slice_slice.result_slice_alloca");

			let pointer_name = c"generate_slice_slice.result_pointer_pointer".as_ptr();
			let pointer_pointer = LLVMBuildStructGEP2(self.builder, llvm_type, alloca, 0, pointer_name);

			let length_name = c"generate_slice_slice.result_length_pointer".as_ptr();
			let length_pointer = LLVMBuildStructGEP2(self.builder, llvm_type, alloca, 1, length_name);

			LLVMBuildStore(self.builder, adjusted_pointer, pointer_pointer);
			LLVMBuildStore(self.builder, adjusted_len, length_pointer);

			let kind = BindingKind::Pointer { pointer: alloca, pointed_type: llvm_type };
			Some(Binding { type_id: slice_type_id, kind })
		}
	}

	fn generate_binary_operation(
		&mut self,
		context: &mut codegen::Context,
		left: &Expression,
		right: &Expression,
		op: BinaryOperator,
		source_type_id: TypeId,
		result_type_id: TypeId,
	) -> Option<Self::Binding> {
		if let BinaryOperator::Assign = op {
			let left = codegen::generate_expression(context, self, left).unwrap();
			let right = codegen::generate_expression(context, self, right).unwrap();

			let left = match left.kind {
				BindingKind::Pointer { pointer, .. } => pointer,
				BindingKind::Value(value) => unreachable!("{value:?}"),
			};

			match right.kind {
				BindingKind::Value(value) => unsafe {
					LLVMBuildStore(self.builder, value, left);
				},

				BindingKind::Pointer { pointer: right_pointer, .. } => unsafe {
					let layout = context.type_store.type_layout(right.type_id);
					let align = layout.alignment as u32;
					let size = LLVMConstInt(LLVMInt64TypeInContext(self.context), layout.size as u64, false as _);
					LLVMBuildMemCpy(self.builder, left, align, right_pointer, align, size);
				},
			}

			return None;
		}

		if matches!(op, BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr) {
			unsafe {
				let current_block = LLVMGetInsertBlock(self.builder);
				let function = LLVMGetBasicBlockParent(current_block);
				let mut left_block =
					LLVMAppendBasicBlockInContext(self.context, function, c"binary_operation.logical.left_block".as_ptr());
				let mut right_block =
					LLVMAppendBasicBlockInContext(self.context, function, c"binary_operation.logical.right_block".as_ptr());
				let following_block =
					LLVMAppendBasicBlockInContext(self.context, function, c"binary_operation.logical.following_block".as_ptr());

				LLVMBuildBr(self.builder, left_block);

				LLVMPositionBuilderAtEnd(self.builder, left_block);
				let left_binding = codegen::generate_expression(context, self, left).unwrap();
				let mut left = left_binding.to_value(self.builder);
				if LLVMGetIntTypeWidth(LLVMTypeOf(left)) > 1 {
					left = LLVMBuildTrunc(
						self.builder,
						left,
						LLVMInt1TypeInContext(self.context),
						c"binary_operation.logical.left_truncated".as_ptr(),
					);
				}
				left_block = LLVMGetInsertBlock(self.builder);

				match op {
					BinaryOperator::LogicalAnd => LLVMBuildCondBr(self.builder, left, right_block, following_block),
					BinaryOperator::LogicalOr => LLVMBuildCondBr(self.builder, left, following_block, right_block),
					_ => unreachable!("{op:?}"),
				};

				LLVMPositionBuilderAtEnd(self.builder, right_block);
				let right_binding = codegen::generate_expression(context, self, right).unwrap();
				let mut right = right_binding.to_value(self.builder);
				if LLVMGetIntTypeWidth(LLVMTypeOf(right)) > 1 {
					right = LLVMBuildTrunc(
						self.builder,
						right,
						LLVMInt1TypeInContext(self.context),
						c"binary_operation.logical.right_truncated".as_ptr(),
					);
				}
				right_block = LLVMGetInsertBlock(self.builder);

				LLVMBuildBr(self.builder, following_block);

				LLVMPositionBuilderAtEnd(self.builder, following_block);
				let phi = LLVMBuildPhi(
					self.builder,
					LLVMInt1TypeInContext(self.context),
					c"binary_operation.logical.result_phi".as_ptr(),
				);
				LLVMAddIncoming(phi, [left, right].as_mut_ptr(), [left_block, right_block].as_mut_ptr(), 2);

				let kind = BindingKind::Value(phi);
				return Some(Binding { type_id: result_type_id, kind });
			}
		}

		if matches!(
			op,
			BinaryOperator::AddAssign
				| BinaryOperator::SubAssign
				| BinaryOperator::MulAssign
				| BinaryOperator::DivAssign
				| BinaryOperator::ModuloAssign
				| BinaryOperator::BitshiftLeftAssign
				| BinaryOperator::BitshiftRightAssign
				| BinaryOperator::BitwiseAndAssign
				| BinaryOperator::BitwiseOrAssign
				| BinaryOperator::BitwiseXorAssign
		) {
			let left = codegen::generate_expression(context, self, left).unwrap();
			let right = codegen::generate_expression(context, self, right).unwrap();

			let target = match left.kind {
				BindingKind::Pointer { pointer, .. } => pointer,
				BindingKind::Value(value) => unreachable!("{value:?}"),
			};
			let left = left.to_value(self.builder);
			let right = right.to_value(self.builder);
			unsafe { assert_eq!(LLVMTypeOf(left), LLVMTypeOf(right)) };
			let left_is_int = unsafe { LLVMGetTypeKind(LLVMTypeOf(left)) == LLVMIntegerTypeKind };

			match op {
				BinaryOperator::AddAssign => unsafe {
					let value = if left_is_int {
						LLVMBuildAdd(self.builder, left, right, c"binary_operator.add_assign.int".as_ptr())
					} else {
						LLVMBuildFAdd(self.builder, left, right, c"binary_operator.add_assign.float".as_ptr())
					};
					LLVMBuildStore(self.builder, value, target);
					return None;
				},

				BinaryOperator::SubAssign => unsafe {
					let value = if left_is_int {
						LLVMBuildSub(self.builder, left, right, c"binary_operator.sub_assign.int".as_ptr())
					} else {
						LLVMBuildFSub(self.builder, left, right, c"binary_operator.sub_assign.float".as_ptr())
					};
					LLVMBuildStore(self.builder, value, target);
					return None;
				},

				BinaryOperator::MulAssign => unsafe {
					let value = if left_is_int {
						LLVMBuildMul(self.builder, left, right, c"binary_operator.mul_assign.int".as_ptr())
					} else {
						LLVMBuildFMul(self.builder, left, right, c"binary_operator.mul_assign.float".as_ptr())
					};
					LLVMBuildStore(self.builder, value, target);
					return None;
				},

				BinaryOperator::DivAssign => unsafe {
					let value = if left_is_int {
						if source_type_id.numeric_kind(context.type_store).unwrap().is_signed() {
							LLVMBuildSDiv(self.builder, left, right, c"binary_operator.div_assign.signed_int".as_ptr())
						} else {
							LLVMBuildUDiv(self.builder, left, right, c"binary_operator.div_assign.unsigned_int".as_ptr())
						}
					} else {
						LLVMBuildFDiv(self.builder, left, right, c"binary_operator.div_assign.float".as_ptr())
					};

					LLVMBuildStore(self.builder, value, target);
					return None;
				},

				BinaryOperator::ModuloAssign => unsafe {
					let value = if source_type_id.numeric_kind(context.type_store).unwrap().is_signed() {
						let zero = LLVMConstNull(LLVMTypeOf(left));
						let srem_result = LLVMBuildSRem(self.builder, left, right, c"".as_ptr());

						let absolute_value = {
							let negated = LLVMBuildNeg(self.builder, right, c"".as_ptr());
							let is_negative = LLVMBuildICmp(self.builder, LLVMIntSLT, right, zero, c"".as_ptr());
							let selected = LLVMBuildSelect(self.builder, is_negative, negated, right, c"".as_ptr());
							selected
						};

						let is_negative = LLVMBuildICmp(self.builder, LLVMIntSLT, srem_result, zero, c"".as_ptr());
						let addened = LLVMBuildSelect(self.builder, is_negative, absolute_value, zero, c"".as_ptr());

						LLVMBuildAdd(self.builder, srem_result, addened, c"".as_ptr())
					} else {
						LLVMBuildURem(self.builder, left, right, c"".as_ptr())
					};

					LLVMBuildStore(self.builder, value, target);
					return None;
				},

				BinaryOperator::BitshiftLeftAssign => unsafe {
					let int = LLVMBuildShl(self.builder, left, right, c"".as_ptr());
					LLVMBuildStore(self.builder, int, target);
					return None;
				},

				BinaryOperator::BitshiftRightAssign => unsafe {
					let int = if source_type_id.numeric_kind(context.type_store).unwrap().is_signed() {
						LLVMBuildAShr(self.builder, left, right, c"".as_ptr())
					} else {
						LLVMBuildLShr(self.builder, left, right, c"".as_ptr())
					};
					LLVMBuildStore(self.builder, int, target);
					return None;
				},

				BinaryOperator::BitwiseAndAssign => unsafe {
					let int = LLVMBuildAnd(self.builder, left, right, c"".as_ptr());
					LLVMBuildStore(self.builder, int, target);
					return None;
				},

				BinaryOperator::BitwiseOrAssign => unsafe {
					let int = LLVMBuildOr(self.builder, left, right, c"".as_ptr());
					LLVMBuildStore(self.builder, int, target);
					return None;
				},

				BinaryOperator::BitwiseXorAssign => unsafe {
					let int = LLVMBuildXor(self.builder, left, right, c"".as_ptr());
					LLVMBuildStore(self.builder, int, target);
					return None;
				},

				_ => unreachable!(),
			}
		}

		let left_binding = codegen::generate_expression(context, self, left).unwrap();
		let right_binding = codegen::generate_expression(context, self, right).unwrap();

		let left = left_binding.to_value(self.builder);
		let right = right_binding.to_value(self.builder);
		unsafe { assert_eq!(LLVMTypeOf(left), LLVMTypeOf(right)) };

		if let BinaryOperator::Range = op {
			unsafe {
				let llvm_type = self.llvm_types.range_struct;
				let alloca = self.build_alloca(llvm_type, c"generate_binary_operation.range_alloca");

				let start_name = c"generate_binary_operation.range_start_pointer".as_ptr();
				let start_pointer = LLVMBuildStructGEP2(self.builder, llvm_type, alloca, 0, start_name);

				let end_name = c"generate_binary_operation.range_start_pointer".as_ptr();
				let end_pointer = LLVMBuildStructGEP2(self.builder, llvm_type, alloca, 1, end_name);

				LLVMBuildStore(self.builder, left, start_pointer);
				LLVMBuildStore(self.builder, right, end_pointer);

				let type_id = context.lang_items.range_type.unwrap();
				let kind = BindingKind::Pointer { pointer: alloca, pointed_type: llvm_type };
				return Some(Binding { type_id, kind });
			}
		}

		let left_is_int = unsafe { LLVMGetTypeKind(LLVMTypeOf(left)) == LLVMIntegerTypeKind };
		let value = if left_is_int {
			let left = left;
			let right = right;

			match op {
				BinaryOperator::Add => unsafe { LLVMBuildAdd(self.builder, left, right, c"".as_ptr()) },

				BinaryOperator::Sub => unsafe { LLVMBuildSub(self.builder, left, right, c"".as_ptr()) },

				BinaryOperator::Mul => unsafe { LLVMBuildMul(self.builder, left, right, c"".as_ptr()) },

				BinaryOperator::Div => unsafe {
					if source_type_id.numeric_kind(context.type_store).unwrap().is_signed() {
						LLVMBuildSDiv(self.builder, left, right, c"".as_ptr())
					} else {
						LLVMBuildUDiv(self.builder, left, right, c"".as_ptr())
					}
				},

				BinaryOperator::Modulo => unsafe {
					if source_type_id.numeric_kind(context.type_store).unwrap().is_signed() {
						let zero = LLVMConstNull(LLVMTypeOf(left));
						let srem_result = LLVMBuildSRem(self.builder, left, right, c"".as_ptr());

						let absolute_value = {
							let negated = LLVMBuildNeg(self.builder, right, c"".as_ptr());
							let is_negative = LLVMBuildICmp(self.builder, LLVMIntSLT, right, zero, c"".as_ptr());
							LLVMBuildSelect(self.builder, is_negative, negated, right, c"".as_ptr())
						};

						let is_negative = LLVMBuildICmp(self.builder, LLVMIntSLT, srem_result, zero, c"".as_ptr());
						let addened = LLVMBuildSelect(self.builder, is_negative, absolute_value, zero, c"".as_ptr());

						LLVMBuildAdd(self.builder, srem_result, addened, c"".as_ptr())
					} else {
						LLVMBuildURem(self.builder, left, right, c"".as_ptr())
					}
				},

				BinaryOperator::BitshiftLeft => unsafe { LLVMBuildShl(self.builder, left, right, c"".as_ptr()) },

				BinaryOperator::BitshiftRight => unsafe {
					if source_type_id.numeric_kind(context.type_store).unwrap().is_signed() {
						LLVMBuildAShr(self.builder, left, right, c"".as_ptr())
					} else {
						LLVMBuildLShr(self.builder, left, right, c"".as_ptr())
					}
				},

				BinaryOperator::BitwiseAnd => unsafe { LLVMBuildAnd(self.builder, left, right, c"".as_ptr()) },

				BinaryOperator::BitwiseOr => unsafe { LLVMBuildOr(self.builder, left, right, c"".as_ptr()) },

				BinaryOperator::BitwiseXor => unsafe { LLVMBuildXor(self.builder, left, right, c"".as_ptr()) },

				BinaryOperator::Equals => unsafe { LLVMBuildICmp(self.builder, LLVMIntEQ, left, right, c"".as_ptr()) },

				BinaryOperator::NotEquals => unsafe { LLVMBuildICmp(self.builder, LLVMIntNE, left, right, c"".as_ptr()) },

				BinaryOperator::GreaterThan => unsafe {
					if source_type_id.numeric_kind(context.type_store).unwrap().is_signed() {
						LLVMBuildICmp(self.builder, LLVMIntSGT, left, right, c"".as_ptr())
					} else {
						LLVMBuildICmp(self.builder, LLVMIntUGT, left, right, c"".as_ptr())
					}
				},

				BinaryOperator::GreaterThanEquals => unsafe {
					if source_type_id.numeric_kind(context.type_store).unwrap().is_signed() {
						LLVMBuildICmp(self.builder, LLVMIntSGE, left, right, c"".as_ptr())
					} else {
						LLVMBuildICmp(self.builder, LLVMIntUGE, left, right, c"".as_ptr())
					}
				},

				BinaryOperator::LessThan => unsafe {
					if source_type_id.numeric_kind(context.type_store).unwrap().is_signed() {
						LLVMBuildICmp(self.builder, LLVMIntSLT, left, right, c"".as_ptr())
					} else {
						LLVMBuildICmp(self.builder, LLVMIntULT, left, right, c"".as_ptr())
					}
				},

				BinaryOperator::LessThanEquals => unsafe {
					if source_type_id.numeric_kind(context.type_store).unwrap().is_signed() {
						LLVMBuildICmp(self.builder, LLVMIntSLE, left, right, c"".as_ptr())
					} else {
						LLVMBuildICmp(self.builder, LLVMIntULE, left, right, c"".as_ptr())
					}
				},

				BinaryOperator::Assign
				| BinaryOperator::AddAssign
				| BinaryOperator::SubAssign
				| BinaryOperator::MulAssign
				| BinaryOperator::DivAssign
				| BinaryOperator::ModuloAssign
				| BinaryOperator::BitshiftLeftAssign
				| BinaryOperator::BitshiftRightAssign
				| BinaryOperator::BitwiseAndAssign
				| BinaryOperator::BitwiseOrAssign
				| BinaryOperator::BitwiseXorAssign
				| BinaryOperator::LogicalAnd
				| BinaryOperator::LogicalOr
				| BinaryOperator::Range => unreachable!(),
			}
		} else {
			use LLVMRealPredicate::*;
			let n = c"".as_ptr();
			match op {
				BinaryOperator::Add => unsafe { LLVMBuildFAdd(self.builder, left, right, n) },

				BinaryOperator::Sub => unsafe { LLVMBuildFSub(self.builder, left, right, n) },

				BinaryOperator::Mul => unsafe { LLVMBuildFMul(self.builder, left, right, n) },

				BinaryOperator::Div => unsafe { LLVMBuildFDiv(self.builder, left, right, n) },

				BinaryOperator::Equals => unsafe { LLVMBuildFCmp(self.builder, LLVMRealOEQ, left, right, n) },

				BinaryOperator::NotEquals => unsafe { LLVMBuildFCmp(self.builder, LLVMRealONE, left, right, n) },

				BinaryOperator::GreaterThan => unsafe { LLVMBuildFCmp(self.builder, LLVMRealOGT, left, right, n) },

				BinaryOperator::GreaterThanEquals => unsafe { LLVMBuildFCmp(self.builder, LLVMRealOGE, left, right, n) },

				BinaryOperator::LessThan => unsafe { LLVMBuildFCmp(self.builder, LLVMRealOLT, left, right, n) },

				BinaryOperator::LessThanEquals => unsafe { LLVMBuildFCmp(self.builder, LLVMRealOLE, left, right, n) },

				BinaryOperator::Assign
				| BinaryOperator::AddAssign
				| BinaryOperator::SubAssign
				| BinaryOperator::MulAssign
				| BinaryOperator::DivAssign
				| BinaryOperator::Modulo
				| BinaryOperator::ModuloAssign
				| BinaryOperator::BitshiftLeft
				| BinaryOperator::BitshiftLeftAssign
				| BinaryOperator::BitshiftRight
				| BinaryOperator::BitshiftRightAssign
				| BinaryOperator::BitwiseAnd
				| BinaryOperator::BitwiseAndAssign
				| BinaryOperator::BitwiseOr
				| BinaryOperator::BitwiseOrAssign
				| BinaryOperator::BitwiseXor
				| BinaryOperator::BitwiseXorAssign
				| BinaryOperator::LogicalAnd
				| BinaryOperator::LogicalOr
				| BinaryOperator::Range => unreachable!(),
			}
		};

		let kind = BindingKind::Value(value);
		Some(Binding { type_id: result_type_id, kind })
	}

	fn generate_check_is(
		&mut self,
		context: &mut codegen::Context,
		value: Self::Binding,
		enum_shape_index: usize,
		enum_specialization_index: usize,
		check_expression: &CheckIs,
	) -> Self::Binding {
		let ValuePointer { pointer, .. } = self.value_auto_deref_pointer(context.type_store, value);

		let result = unsafe {
			let i1_type = LLVMInt1TypeInContext(self.context);
			let i8_type = LLVMInt8TypeInContext(self.context);
			let tag = LLVMBuildLoad2(self.builder, i8_type, pointer, c"check_is.tag".as_ptr());

			let mut result = LLVMConstInt(i1_type, 0, false as _);

			for info in &check_expression.variant_infos {
				let expected = LLVMConstInt(i8_type, info.variant_index as _, false as _);
				let flag = LLVMBuildICmp(self.builder, LLVMIntEQ, tag, expected, c"".as_ptr());
				result = LLVMBuildOr(self.builder, result, flag, c"".as_ptr());
			}

			result
		};

		if let Some(new_binding) = &check_expression.binding {
			assert_eq!(self.readables.len(), new_binding.readable_index);
			let type_id = context.specialize_type_id(new_binding.type_id);
			let layout = context.type_store.type_layout(type_id);

			if layout.size <= 0 {
				self.readables.push(None);
			} else {
				let shape = &self.llvm_types.user_type_structs[enum_shape_index];
				let llvm_struct = shape[enum_specialization_index].unwrap().actual;
				let variant_pointer = unsafe {
					let original_block = LLVMGetInsertBlock(self.builder);
					LLVMPositionBuilderAtEnd(self.builder, context.if_condition_binding_block.unwrap());
					let ptr = LLVMBuildStructGEP2(self.builder, llvm_struct, pointer, 1, c"check_is.variant_pointer".as_ptr());
					LLVMPositionBuilderAtEnd(self.builder, original_block);
					ptr
				};

				let pointed_type = self.llvm_types.type_to_llvm_type(self.context, context.type_store, type_id);
				let kind = BindingKind::Pointer { pointer: variant_pointer, pointed_type };
				let binding = Binding { type_id, kind };
				self.readables.push(Some(binding));
			}
		}

		let kind = BindingKind::Value(result);
		Binding { type_id: context.type_store.bool_type_id(), kind }
	}

	fn generate_enum_variant_to_enum(
		&mut self,
		type_store: &mut TypeStore,
		enum_type_id: TypeId,
		enum_shape_index: usize,
		enum_specialization_index: usize,
		variant_index: usize,
		variant_binding: Option<Binding>,
	) -> Self::Binding {
		let shape = &self.llvm_types.user_type_structs[enum_shape_index];
		let enum_type = shape[enum_specialization_index].unwrap().actual;

		let alloca = self.build_alloca(enum_type, c"generate_enum_variant_to_enum.enum_alloca");

		unsafe {
			let i8_type = LLVMInt8TypeInContext(self.context);
			let tag_value = LLVMConstInt(i8_type, variant_index as _, false as _);
			let tag_pointer = LLVMBuildStructGEP2(self.builder, enum_type, alloca, 0, c"variant_to_enum.tag_pointer".as_ptr());
			LLVMBuildStore(self.builder, tag_value, tag_pointer);

			if let Some(variant_binding) = variant_binding {
				let variant_pointer = LLVMBuildStructGEP2(self.builder, enum_type, alloca, 1, c"".as_ptr());
				match variant_binding.kind {
					BindingKind::Value(value) => {
						LLVMBuildStore(self.builder, value, variant_pointer);
					}

					BindingKind::Pointer { pointer, .. } => {
						let layout = type_store.type_layout(variant_binding.type_id);
						let align = layout.alignment as u32;
						let size = LLVMConstInt(LLVMInt64TypeInContext(self.context), layout.size as u64, false as _);
						LLVMBuildMemCpy(self.builder, variant_pointer, align, pointer, align, size);
					}
				}
			}
		}

		let kind = BindingKind::Pointer { pointer: alloca, pointed_type: enum_type };
		Binding { type_id: enum_type_id, kind }
	}

	fn generate_binding(
		&mut self,
		readable_index: usize,
		value: Option<Self::Binding>,
		type_id: TypeId,
		name: &str,
		debug_location: DebugLocation,
	) {
		let _debug_scope = self.create_debug_scope(debug_location);

		assert_eq!(self.readables.len(), readable_index);
		let Some(value) = value else {
			self.readables.push(None);
			return;
		};

		let (pointer, pointed_type) = match value.kind {
			BindingKind::Value(value) => unsafe {
				let llvm_type = LLVMTypeOf(value);
				// This format hurts my soul
				let alloca = self.build_alloca(llvm_type, CString::new(format!("generate_binding.{}", name)).unwrap());
				LLVMBuildStore(self.builder, value, alloca);
				(alloca, llvm_type)
			},

			BindingKind::Pointer { pointer, pointed_type } => {
				// This format hurts my soul
				let alloca = self.build_alloca(pointed_type, CString::new(format!("generate_binding.{}", name)).unwrap());
				unsafe {
					let value = LLVMBuildLoad2(self.builder, pointed_type, pointer, c"generate_binding.pointer.load".as_ptr());
					LLVMBuildStore(self.builder, value, alloca);
				}
				(alloca, pointed_type)
			}
		};

		let kind = BindingKind::Pointer { pointer, pointed_type };
		let binding = Binding { type_id, kind };
		self.readables.push(Some(binding));
	}

	fn generate_break(&mut self, loop_index: usize, debug_location: DebugLocation) {
		let _debug_scope = self.create_debug_scope(debug_location);

		let follow_block = self.loop_follow_blocks[loop_index];
		unsafe { LLVMBuildBr(self.builder, follow_block) };
	}

	fn generate_continue(&mut self, loop_index: usize, debug_location: DebugLocation) {
		let _debug_scope = self.create_debug_scope(debug_location);

		let condition_block = self.loop_condition_blocks[loop_index];
		unsafe { LLVMBuildBr(self.builder, condition_block) };
	}

	fn generate_return(&mut self, function_id: FunctionId, value: Option<Self::Binding>, debug_location: DebugLocation) {
		let _debug_scope = self.create_debug_scope(debug_location);

		let maybe_function = &self.functions[function_id.function_shape_index][function_id.specialization_index];
		let function = maybe_function.as_ref().unwrap();

		let mut abi = self.abi.take().unwrap();
		abi.return_value(self.context, self.builder, function, value);
		self.abi = Some(abi);
	}

	fn generate_slice(&mut self, slice_type_id: TypeId, pointer: Self::Binding, length: Self::Binding) -> Self::Binding {
		unsafe {
			let pointer = pointer.to_value(self.builder);
			let length = length.to_value(self.builder);

			let llvm_type = self.llvm_types.slice_struct;
			let alloca = self.build_alloca(llvm_type, c"generate_slice.slice_alloca");

			let pointer_name = c"generate_slice.pointer_pointer".as_ptr();
			let pointer_pointer = LLVMBuildStructGEP2(self.builder, llvm_type, alloca, 0, pointer_name);

			let length_name = c"generate_slice.length_pointer".as_ptr();
			let length_pointer = LLVMBuildStructGEP2(self.builder, llvm_type, alloca, 1, length_name);

			LLVMBuildStore(self.builder, pointer, pointer_pointer);
			LLVMBuildStore(self.builder, length, length_pointer);

			let kind = BindingKind::Pointer { pointer: alloca, pointed_type: llvm_type };
			Binding { type_id: slice_type_id, kind }
		}
	}

	fn generate_non_null_invalid_pointer(&mut self, pointer_type_id: TypeId) -> Self::Binding {
		unsafe {
			let one = LLVMConstInt(LLVMInt64TypeInContext(self.context), 1, false as _);
			let pointer_type = self.llvm_types.opaque_pointer;
			let pointer = LLVMBuildIntToPtr(self.builder, one, pointer_type, c"non_null_invalid_pointer.pointer".as_ptr());

			let kind = BindingKind::Value(pointer);
			Binding { type_id: pointer_type_id, kind }
		}
	}

	fn generate_non_null_invalid_slice(&mut self, slice_type_id: TypeId, length: u64) -> Self::Binding {
		unsafe {
			let one = LLVMConstInt(LLVMInt64TypeInContext(self.context), 1, false as _);
			let pointer_type = self.llvm_types.opaque_pointer;

			let pointer = LLVMBuildIntToPtr(self.builder, one, pointer_type, c"non_null_invalid_slice.pointer".as_ptr());
			let length = LLVMConstInt(LLVMInt64TypeInContext(self.context), length, false as _);

			let fields = &mut [pointer, length];
			let slice = LLVMConstNamedStruct(self.llvm_types.slice_struct, fields.as_mut_ptr(), fields.len() as u32);

			let kind = BindingKind::Value(slice);
			Binding { type_id: slice_type_id, kind }
		}
	}

	fn generate_debugger_break(&mut self) {
		if self.architecture == Architecture::Amd64 {
			unsafe {
				let void = LLVMVoidTypeInContext(self.context);
				let assembly = "int 3";

				let value = LLVMGetInlineAsm(
					void,
					assembly.as_ptr() as _,
					assembly.len(),
					"".as_ptr() as _,
					0,
					false as _, // has side effects
					false as _, // is align stack, TODO: understand this?
					llvm_sys::LLVMInlineAsmDialect::LLVMInlineAsmDialectIntel,
					false as _, // can throw
				);

				let function_type = LLVMFunctionType(void, std::ptr::null_mut(), 0, false as _);
				LLVMBuildCall2(
					self.builder,
					function_type,
					value,
					std::ptr::null_mut(),
					0,
					c"generate_debugger_break".as_ptr(),
				);
			}
		}
	}

	fn finalize_generator(&mut self) {
		self.finalize_function_if_in_function();
	}
}
