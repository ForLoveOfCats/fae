use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::os::unix::ffi::OsStrExt;

use llvm_sys::core::{
	LLVMAddAttributeAtIndex, LLVMAddCase, LLVMAddFunction, LLVMAddGlobal, LLVMAddIncoming, LLVMAppendBasicBlockInContext,
	LLVMArrayType2, LLVMBasicBlockAsValue, LLVMBuildAShr, LLVMBuildAdd, LLVMBuildAlloca, LLVMBuildAnd, LLVMBuildBr,
	LLVMBuildCall2, LLVMBuildCondBr, LLVMBuildFAdd, LLVMBuildFCmp, LLVMBuildFDiv, LLVMBuildFMul, LLVMBuildFNeg, LLVMBuildFPCast,
	LLVMBuildFPToSI, LLVMBuildFPToUI, LLVMBuildFSub, LLVMBuildGEP2, LLVMBuildICmp, LLVMBuildIntCast2, LLVMBuildIntToPtr,
	LLVMBuildLShr, LLVMBuildLoad2, LLVMBuildMemCpy, LLVMBuildMul, LLVMBuildNeg, LLVMBuildNot, LLVMBuildOr, LLVMBuildPhi,
	LLVMBuildPtrToInt, LLVMBuildRetVoid, LLVMBuildSDiv, LLVMBuildSIToFP, LLVMBuildSRem, LLVMBuildSelect, LLVMBuildShl,
	LLVMBuildStore, LLVMBuildStructGEP2, LLVMBuildSub, LLVMBuildSwitch, LLVMBuildTrunc, LLVMBuildUDiv, LLVMBuildUIToFP,
	LLVMBuildURem, LLVMBuildUnreachable, LLVMBuildXor, LLVMClearInsertionPosition, LLVMConstInt, LLVMConstNamedStruct,
	LLVMConstNull, LLVMConstReal, LLVMConstStringInContext2, LLVMCreateBuilderInContext, LLVMCreateEnumAttribute,
	LLVMDoubleTypeInContext, LLVMFloatTypeInContext, LLVMFunctionType, LLVMGetBasicBlockParent, LLVMGetBasicBlockTerminator,
	LLVMGetEnumAttributeKindForName, LLVMGetInlineAsm, LLVMGetInsertBlock, LLVMGetIntTypeWidth, LLVMGetTypeKind,
	LLVMInt16TypeInContext, LLVMInt1TypeInContext, LLVMInt32TypeInContext, LLVMInt64TypeInContext, LLVMInt8TypeInContext,
	LLVMModuleCreateWithNameInContext, LLVMPointerTypeInContext, LLVMPositionBuilderAtEnd, LLVMSetGlobalConstant,
	LLVMSetInitializer, LLVMSetLinkage, LLVMSetUnnamedAddress, LLVMSetValueName2, LLVMSetVisibility, LLVMStructCreateNamed,
	LLVMStructGetTypeAtIndex, LLVMStructSetBody, LLVMStructTypeInContext, LLVMTypeOf, LLVMVoidTypeInContext,
};
use llvm_sys::debuginfo::{
	LLVMCreateDIBuilder, LLVMDIBuilderCreateCompileUnit, LLVMDIBuilderCreateFile, LLVMDIBuilderCreateFunction,
	LLVMDIBuilderCreateSubroutineType, LLVMDIFlagPrototyped, LLVMDIFlagZero, LLVMSetSubprogram,
};
use llvm_sys::prelude::{
	LLVMBasicBlockRef, LLVMBool, LLVMBuilderRef, LLVMContextRef, LLVMDIBuilderRef, LLVMMetadataRef, LLVMModuleRef, LLVMTypeRef,
	LLVMValueRef,
};
use llvm_sys::{
	LLVMAttributeFunctionIndex, LLVMIntPredicate::*, LLVMLinkage, LLVMRealPredicate, LLVMTypeKind::*, LLVMUnnamedAddr,
	LLVMVisibility,
};
use rust_decimal::prelude::ToPrimitive;
use rust_decimal::Decimal;
use rustc_hash::{FxBuildHasher, FxHashMap};

use crate::codegen::codegen;
use crate::codegen::generator::Generator;
use crate::codegen::llvm::abi::{DefinedFunction, LLVMAbi};
use crate::codegen::llvm::debug_scope::DebugScope;
use crate::frontend::function_store::FunctionStore;
use crate::frontend::ir::{Block, CheckIs, Expression, ForKind, Function, FunctionId, IfElseChain, Match};
use crate::frontend::lang_items::LangItems;
use crate::frontend::span::DebugLocation;
use crate::frontend::symbols::Statics;
use crate::frontend::tree::{self, BinaryOperator};
use crate::frontend::type_store::{Array, NumericKind, PrimativeKind, TypeEntryKind, TypeId, TypeStore, UserTypeKind};

pub struct AttributeKinds {
	pub sret: u32,
	pub noinline: u32,
	pub byval: u32,
}

impl AttributeKinds {
	fn new() -> AttributeKinds {
		fn kind(name: &str) -> u32 {
			let kind = unsafe { LLVMGetEnumAttributeKindForName(name.as_ptr() as _, name.len()) };
			assert_ne!(kind, 0);
			kind
		}

		AttributeKinds {
			sret: kind("sret"),
			noinline: kind("noinline"),
			byval: kind("byval"),
		}
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

#[derive(Clone)]
struct UserTypeStruct {
	actual: LLVMTypeRef,
	as_enum_shared_fields: Option<LLVMTypeRef>,
	as_union_variants: Vec<LLVMTypeRef>,
}

pub struct LLVMTypes {
	pub opaque_pointer: LLVMTypeRef,
	pub void_struct: LLVMTypeRef,
	pub slice_struct: LLVMTypeRef,
	pub range_struct: LLVMTypeRef,

	user_type_structs: Vec<Vec<UserTypeStruct>>,
	array_types: Vec<FxHashMap<u64, LLVMTypeRef>>,
}

impl LLVMTypes {
	fn new(context: LLVMContextRef) -> Self {
		unsafe {
			let opaque_pointer = LLVMPointerTypeInContext(context, 0);
			let i64_type = LLVMInt64TypeInContext(context);

			let void_struct = LLVMStructTypeInContext(context, [].as_mut_ptr(), 0, false as _);
			let slice_struct = LLVMStructTypeInContext(context, [opaque_pointer, i64_type].as_mut_ptr(), 2, false as _);
			let range_struct = LLVMStructTypeInContext(context, [i64_type, i64_type].as_mut_ptr(), 2, false as _);

			LLVMTypes {
				opaque_pointer,
				void_struct,
				slice_struct,
				range_struct,
				user_type_structs: Vec::new(),
				array_types: Vec::new(),
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
			TypeEntryKind::BuiltinType { kind, .. } => match kind {
				PrimativeKind::Numeric(numeric_kind) => numeric_kind_to_llvm_type(context, numeric_kind),

				PrimativeKind::Bool => unsafe { LLVMInt1TypeInContext(context) },

				PrimativeKind::String | PrimativeKind::StringMut | PrimativeKind::FormatString => self.slice_struct,

				PrimativeKind::Void => self.void_struct,

				PrimativeKind::AnyCollapse | PrimativeKind::NoReturn | PrimativeKind::UntypedNumber => unreachable!("{kind:?}"),
			},

			TypeEntryKind::UserType { shape_index, specialization_index, .. } => {
				self.user_type_structs[shape_index][specialization_index].actual
			}

			TypeEntryKind::Pointer { .. } => self.opaque_pointer,

			TypeEntryKind::Slice(_) => self.slice_struct,

			TypeEntryKind::Array(Array { length, array_type_index, .. }) => {
				*self.array_types[array_type_index].get(&length).unwrap()
			}

			TypeEntryKind::Module
			| TypeEntryKind::Type
			| TypeEntryKind::UserTypeGeneric { .. }
			| TypeEntryKind::FunctionGeneric { .. }
			| TypeEntryKind::TraitGeneric { .. } => unreachable!("{:?}", entry.kind),
		}
	}
}

pub fn numeric_kind_to_llvm_type(context: LLVMContextRef, kind: NumericKind) -> LLVMTypeRef {
	use NumericKind::*;
	unsafe {
		match kind {
			I8 | U8 => LLVMInt8TypeInContext(context),
			I16 | U16 => LLVMInt16TypeInContext(context),
			I32 | U32 => LLVMInt32TypeInContext(context),
			I64 | U64 | ISize | USize => LLVMInt64TypeInContext(context),
			F32 => LLVMFloatTypeInContext(context),
			F64 => LLVMDoubleTypeInContext(context),
		}
	}
}

#[derive(Clone, Copy)]
pub struct LLVMIntrinsicFunction {
	fn_type: LLVMTypeRef,
	llvm_function: LLVMValueRef,
}

pub struct LLVMIntrinsics {
	pub min_i8: LLVMIntrinsicFunction,
	pub min_i16: LLVMIntrinsicFunction,
	pub min_i32: LLVMIntrinsicFunction,
	pub min_i64: LLVMIntrinsicFunction,

	pub min_u8: LLVMIntrinsicFunction,
	pub min_u16: LLVMIntrinsicFunction,
	pub min_u32: LLVMIntrinsicFunction,
	pub min_u64: LLVMIntrinsicFunction,

	pub min_isize: LLVMIntrinsicFunction,
	pub min_usize: LLVMIntrinsicFunction,

	pub min_f32: LLVMIntrinsicFunction,
	pub min_f64: LLVMIntrinsicFunction,

	pub max_i8: LLVMIntrinsicFunction,
	pub max_i16: LLVMIntrinsicFunction,
	pub max_i32: LLVMIntrinsicFunction,
	pub max_i64: LLVMIntrinsicFunction,

	pub max_u8: LLVMIntrinsicFunction,
	pub max_u16: LLVMIntrinsicFunction,
	pub max_u32: LLVMIntrinsicFunction,
	pub max_u64: LLVMIntrinsicFunction,

	pub max_isize: LLVMIntrinsicFunction,
	pub max_usize: LLVMIntrinsicFunction,

	pub max_f32: LLVMIntrinsicFunction,
	pub max_f64: LLVMIntrinsicFunction,

	pub round_f32: LLVMIntrinsicFunction,
	pub round_f64: LLVMIntrinsicFunction,
}

impl LLVMIntrinsics {
	fn new(context: LLVMContextRef, module: LLVMModuleRef) -> LLVMIntrinsics {
		let t_i8 = unsafe { LLVMInt8TypeInContext(context) };
		let t_i16 = unsafe { LLVMInt16TypeInContext(context) };
		let t_i32 = unsafe { LLVMInt32TypeInContext(context) };
		let t_i64 = unsafe { LLVMInt64TypeInContext(context) };

		let float = unsafe { LLVMFloatTypeInContext(context) };
		let double = unsafe { LLVMDoubleTypeInContext(context) };

		let make_min_max =
			|parameters: &mut [LLVMTypeRef], return_type: LLVMTypeRef, llvm_name: &CStr| -> LLVMIntrinsicFunction {
				unsafe {
					let fn_type = LLVMFunctionType(return_type, parameters.as_mut_ptr(), parameters.len() as u32, false as _);
					let llvm_function = LLVMAddFunction(module, llvm_name.as_ptr(), fn_type);
					LLVMIntrinsicFunction { fn_type, llvm_function }
				}
			};

		let min_i64 = make_min_max(&mut [t_i64, t_i64], t_i64, c"llvm.smin.i64");
		let min_u64 = make_min_max(&mut [t_i64, t_i64], t_i64, c"llvm.umin.i64");

		let max_i64 = make_min_max(&mut [t_i64, t_i64], t_i64, c"llvm.smax.i64");
		let max_u64 = make_min_max(&mut [t_i64, t_i64], t_i64, c"llvm.umax.i64");

		LLVMIntrinsics {
			min_i8: make_min_max(&mut [t_i8, t_i8], t_i8, c"llvm.smin.i8"),
			min_i16: make_min_max(&mut [t_i16, t_i16], t_i16, c"llvm.smin.i16"),
			min_i32: make_min_max(&mut [t_i32, t_i32], t_i32, c"llvm.smin.i32"),
			min_i64,

			min_u8: make_min_max(&mut [t_i8, t_i8], t_i8, c"llvm.umin.i8"),
			min_u16: make_min_max(&mut [t_i16, t_i16], t_i16, c"llvm.umin.i16"),
			min_u32: make_min_max(&mut [t_i32, t_i32], t_i32, c"llvm.umin.i32"),
			min_u64,

			// TODO: Handle other pointer sizes
			min_isize: min_i64,
			min_usize: min_u64,

			min_f32: make_min_max(&mut [float, float], float, c"llvm.minnum.f32"),
			min_f64: make_min_max(&mut [double, double], double, c"llvm.minnum.f64"),

			max_i8: make_min_max(&mut [t_i8, t_i8], t_i8, c"llvm.smax.i8"),
			max_i16: make_min_max(&mut [t_i16, t_i16], t_i16, c"llvm.smax.i16"),
			max_i32: make_min_max(&mut [t_i32, t_i32], t_i32, c"llvm.smax.i32"),
			max_i64,

			max_u8: make_min_max(&mut [t_i8, t_i8], t_i8, c"llvm.umax.i8"),
			max_u16: make_min_max(&mut [t_i16, t_i16], t_i16, c"llvm.umax.i16"),
			max_u32: make_min_max(&mut [t_i32, t_i32], t_i32, c"llvm.umax.i32"),
			max_u64,

			// TODO: Handle other pointer sizes
			max_isize: max_i64,
			max_usize: max_u64,

			max_f32: make_min_max(&mut [float, float], float, c"llvm.maxnum.f32"),
			max_f64: make_min_max(&mut [double, double], double, c"llvm.maxnum.f64"),

			round_f32: unsafe {
				let mut parameters = [float];
				let fn_type = LLVMFunctionType(float, parameters.as_mut_ptr(), parameters.len() as u32, false as _);
				let llvm_function = LLVMAddFunction(module, c"llvm.round.f32".as_ptr(), fn_type);
				LLVMIntrinsicFunction { fn_type, llvm_function }
			},

			round_f64: unsafe {
				let mut parameters = [double];
				let fn_type = LLVMFunctionType(double, parameters.as_mut_ptr(), parameters.len() as u32, false as _);
				let llvm_function = LLVMAddFunction(module, c"llvm.round.f64".as_ptr(), fn_type);
				LLVMIntrinsicFunction { fn_type, llvm_function }
			},
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

#[derive(Clone, Copy)]
struct YieldTarget {
	binding: Option<YieldBinding>,
	following_block: LLVMBasicBlockRef,
}

#[derive(Clone, Copy)]
struct YieldBinding {
	type_id: TypeId,
	pointer: LLVMValueRef,
	pointed_type: LLVMTypeRef,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum InCheckIs {
	None,
	IfElse,
	WhileLoop,
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
	pub llvm_intrinsics: LLVMIntrinsics,

	scope_stack: Vec<LLVMMetadataRef>,
	file: Option<DebugFile>,

	state: State,
	block_frames: Vec<BlockFrame>,
	yield_targets: Vec<YieldTarget>,
	loop_condition_blocks: Vec<LLVMBasicBlockRef>,
	loop_follow_blocks: Vec<LLVMBasicBlockRef>,
	if_follow_blocks: Vec<LLVMBasicBlockRef>,
	in_check_is: InCheckIs,
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
		let llvm_intrinsics = LLVMIntrinsics::new(context, module);

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
			llvm_intrinsics,

			file: None,
			scope_stack: Vec::new(),

			state: State::InModule,
			block_frames: Vec::new(),
			yield_targets: Vec::new(),
			loop_condition_blocks: Vec::new(),
			loop_follow_blocks: Vec::new(),
			if_follow_blocks: Vec::new(),
			in_check_is: InCheckIs::None,
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

	fn generate_min_max_intrinsic(
		&self,
		a: Binding,
		b: Binding,
		intrinsic: LLVMIntrinsicFunction,
		type_id: TypeId,
		debug_location: DebugLocation,
	) -> Binding {
		let _debug_scope = self.create_debug_scope(debug_location);

		let mut arguments = [a.to_value(self.builder), b.to_value(self.builder)];
		let value = unsafe {
			LLVMBuildCall2(
				self.builder,
				intrinsic.fn_type,
				intrinsic.llvm_function,
				arguments.as_mut_ptr(),
				arguments.len() as u32,
				c"".as_ptr(),
			)
		};

		let kind = BindingKind::Value(value);
		Binding { type_id, kind }
	}

	fn create_user_type_descriptions(&mut self, type_store: &mut TypeStore) {
		assert_eq!(self.llvm_types.user_type_structs.len(), 0);

		for user_type in type_store.user_types.read().iter() {
			let user_type = user_type.read();
			let specialization_count = match &user_type.kind {
				UserTypeKind::Struct { shape } => shape.specializations.len(),
				UserTypeKind::Enum { shape } => shape.specializations.len(),
				UserTypeKind::Union { shape } => shape.specializations.len(),
			};

			let specializations = Vec::from_iter((0..specialization_count).map(|_| {
				let name = CString::new(user_type.name).unwrap();
				let named = unsafe { LLVMStructCreateNamed(self.context, name.as_ptr()) };
				UserTypeStruct {
					actual: named,
					as_enum_shared_fields: None,
					as_union_variants: Vec::new(),
				}
			}));
			self.llvm_types.user_type_structs.push(specializations);
		}
	}

	fn create_array_type_descriptions(&mut self, type_store: &mut TypeStore) {
		assert_eq!(self.llvm_types.array_types.len(), 0);

		for array_catagory_lock in type_store.array_types.read().iter() {
			let array_catagory = array_catagory_lock.read();
			let mut type_catagory = HashMap::with_capacity_and_hasher(100, FxBuildHasher);

			for &array_length in array_catagory.keys() {
				let named = unsafe { LLVMStructCreateNamed(self.context, c"ArrayType".as_ptr()) };
				type_catagory.insert(array_length, named);
			}

			self.llvm_types.array_types.push(type_catagory);
		}
	}

	fn fill_user_type_descriptions(&mut self, type_store: &mut TypeStore) {
		let mut field_types_buffer = Vec::new();
		let mut shared_field_types_buffer = Vec::new();
		let mut union_variant_types_buffer = Vec::new();

		let mut enum_specializations = Vec::new();
		let mut union_specializations = Vec::new();

		let user_type_len = type_store.user_types.read().len();
		for shape_index in 0..user_type_len {
			let user_type_lock = type_store.user_types.read()[shape_index].clone();
			let user_type = user_type_lock.read();
			match &user_type.kind {
				UserTypeKind::Struct { shape } => {
					for (specialization_index, specialization) in shape.specializations.iter().enumerate() {
						if specialization.generic_poisoned {
							continue;
						}

						field_types_buffer.clear();
						for field in specialization.fields.iter() {
							let llvm_type = self.llvm_types.type_to_llvm_type(self.context, type_store, field.type_id);
							field_types_buffer.push(llvm_type);
						}

						let user_type_struct = &self.llvm_types.user_type_structs[shape_index][specialization_index];

						unsafe {
							LLVMStructSetBody(
								user_type_struct.actual,
								field_types_buffer.as_mut_ptr(),
								field_types_buffer.len() as u32,
								false as _,
							);
						}
					}
				}

				UserTypeKind::Enum { shape } => {
					enum_specializations.clear();
					enum_specializations.extend_from_slice(&shape.specializations);
					let tag_kind = shape.tag.kind;
					let name = user_type.name;
					drop(user_type);

					for (specialization_index, specialization) in enum_specializations.iter().enumerate() {
						if specialization.generic_poisoned {
							continue;
						}

						field_types_buffer.clear();
						shared_field_types_buffer.clear();

						let layout = type_store.type_layout(specialization.type_id);
						let tag_memory_size = layout.tag_memory_size(tag_kind);

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

						let user_type_struct = &mut self.llvm_types.user_type_structs[shape_index][specialization_index];

						unsafe {
							LLVMStructSetBody(
								user_type_struct.actual,
								field_types_buffer.as_mut_ptr(),
								field_types_buffer.len() as u32,
								false as _,
							);
						}

						unsafe {
							let name = CString::new(format!("{}.as_enum_shared_fields", name)).unwrap();
							let named = LLVMStructCreateNamed(self.context, name.as_ptr());
							LLVMStructSetBody(
								named,
								shared_field_types_buffer.as_mut_ptr(),
								shared_field_types_buffer.len() as u32,
								false as _,
							);
							user_type_struct.as_enum_shared_fields = Some(named);
						};
					}
				}

				UserTypeKind::Union { shape } => {
					union_specializations.clear();
					union_specializations.extend_from_slice(&shape.specializations);
					drop(user_type);

					for (specialization_index, specialization) in union_specializations.iter().enumerate() {
						if specialization.generic_poisoned {
							continue;
						}

						let layout = type_store.type_layout(specialization.type_id);

						field_types_buffer.clear();
						if layout.size > 0 {
							assert_eq!(layout.size % layout.alignment, 0);
							let count = layout.size / layout.alignment;
							let item = LLVMTypes::size_to_int_type(self.context, layout.alignment);
							for _ in 0..count {
								field_types_buffer.push(item);
							}
						}

						let user_type_struct = &self.llvm_types.user_type_structs[shape_index][specialization_index];
						unsafe {
							LLVMStructSetBody(
								user_type_struct.actual,
								field_types_buffer.as_mut_ptr(),
								field_types_buffer.len() as u32,
								false as _,
							);
						}

						union_variant_types_buffer.clear();
						for field in specialization.fields.iter() {
							let llvm_type = self.llvm_types.type_to_llvm_type(self.context, type_store, field.type_id);
							union_variant_types_buffer.push(llvm_type);
						}

						let user_type_struct = &mut self.llvm_types.user_type_structs[shape_index][specialization_index];
						assert!(user_type_struct.as_union_variants.is_empty());
						user_type_struct.as_union_variants = union_variant_types_buffer.clone();
					}
				}
			}
		}
	}

	fn fill_array_type_descriptions(&mut self, type_store: &mut TypeStore) {
		let mut field_types_buffer = Vec::new();

		let array_types = type_store.array_types.clone();
		for (array_index, array_catagory_lock) in array_types.read().iter().enumerate() {
			let array_catagory = array_catagory_lock.read();
			let array_types = &self.llvm_types.array_types[array_index];

			let mut item_layout = None;

			for (&array_length, &array_type) in array_catagory.iter() {
				let item_layout = item_layout.get_or_insert_with(|| type_store.type_layout(array_type.item_type_id));

				field_types_buffer.clear();
				if item_layout.size > 0 && array_length > 0 {
					assert_eq!(item_layout.size % item_layout.alignment, 0);
					let item_integer_count = item_layout.size / item_layout.alignment;
					let item = LLVMTypes::size_to_int_type(self.context, item_layout.alignment);
					for _ in 0..item_integer_count as u64 * array_length {
						field_types_buffer.push(item);
					}
				}

				let type_ref = *array_types.get(&array_length).unwrap();
				unsafe {
					LLVMStructSetBody(type_ref, field_types_buffer.as_mut_ptr(), field_types_buffer.len() as u32, false as _);
				}
			}
		}
	}
}

impl<ABI: LLVMAbi> Generator for LLVMGenerator<ABI> {
	type Binding = Binding;

	fn register_type_descriptions(&mut self, type_store: &mut TypeStore) {
		self.create_user_type_descriptions(type_store);
		self.create_array_type_descriptions(type_store);

		self.fill_user_type_descriptions(type_store);
		self.fill_array_type_descriptions(type_store);
	}

	fn register_statics(&mut self, type_store: &mut TypeStore, statics: &Statics) {
		for static_instance in &statics.statics {
			let llvm_type = self
				.llvm_types
				.type_to_llvm_type(self.context, type_store, static_instance.type_id);

			let pointer = if let Some(extern_attribute) = static_instance.extern_attribute {
				let name = CString::new(extern_attribute.name).unwrap();
				let global = unsafe { LLVMAddGlobal(self.module, llvm_type, name.as_ptr()) };
				unsafe { LLVMSetLinkage(global, LLVMLinkage::LLVMExternalLinkage) };
				global
			} else {
				let formatted = format!("fae_static_{}", static_instance.name);
				let name = CString::new(formatted).unwrap();
				let global = unsafe { LLVMAddGlobal(self.module, llvm_type, name.as_ptr()) };
				unsafe {
					LLVMSetLinkage(global, LLVMLinkage::LLVMInternalLinkage);
					LLVMSetInitializer(global, LLVMConstNull(llvm_type));
				}
				global
			};

			let kind = BindingKind::Pointer { pointer, pointed_type: llvm_type };
			let binding = Binding { type_id: static_instance.type_id, kind };
			self.statics.push(binding);
		}
	}

	fn register_functions(
		&mut self,
		parsed_files: &[tree::File],
		type_store: &mut TypeStore,
		function_store: &FunctionStore,
		optimizing: bool,
	) {
		assert_eq!(self.functions.len(), 0);

		let function_count = function_store.shapes.read().len();
		for function_shape_index in 0..function_count {
			let lock = function_store.shapes.read()[function_shape_index].as_ref().unwrap().clone();
			let shape = lock.read();
			if shape.intrinsic_attribute.is_some() || shape.trait_method_marker.is_some() {
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
				let file = &parsed_files[span.file_index as usize];
				let file_name = file.source_file.path.file_name().unwrap();
				let directory = file.source_file.path.parent().unwrap();

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

				let debug_location = shape.name.span.debug_location(parsed_files);
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

				unsafe {
					LLVMSetSubprogram(defined_function.llvm_function, subroutine);

					if !optimizing {
						let attribute = LLVMCreateEnumAttribute(self.context, self.attribute_kinds.noinline, 0);
						LLVMAddAttributeAtIndex(defined_function.llvm_function, LLVMAttributeFunctionIndex, attribute);
					}
				}

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

	fn start_block_expression(&mut self, type_store: &mut TypeStore, yield_target_index: usize, type_id: TypeId) {
		assert_eq!(yield_target_index, self.yield_targets.len());

		let original_block = unsafe { LLVMGetInsertBlock(self.builder) };
		let function = unsafe { LLVMGetBasicBlockParent(original_block) };
		let following_block_name = c"start_block_expression.following".as_ptr();
		let following_block = unsafe { LLVMAppendBasicBlockInContext(self.context, function, following_block_name) };

		let binding = if type_store.type_layout(type_id).size > 0 {
			let pointed_type = self.llvm_types.type_to_llvm_type(self.context, type_store, type_id);
			let pointer = self.build_alloca(pointed_type, c"start_block_expression.yield_target_alloca");
			Some(YieldBinding { type_id, pointer, pointed_type })
		} else {
			None
		};

		self.yield_targets.push(YieldTarget { binding, following_block });
	}

	fn end_block_expression(&mut self, yield_target_index: usize) -> Option<Self::Binding> {
		let target = self.yield_targets.pop().unwrap();
		assert_eq!(yield_target_index, self.yield_targets.len());

		unsafe {
			let current_block = LLVMGetInsertBlock(self.builder);
			if LLVMGetBasicBlockTerminator(current_block).is_null() {
				LLVMBuildBr(self.builder, target.following_block);
			}

			LLVMPositionBuilderAtEnd(self.builder, target.following_block);
		}

		target.binding.map(|binding| Binding {
			type_id: binding.type_id,
			kind: BindingKind::Pointer { pointer: binding.pointer, pointed_type: binding.pointed_type },
		})
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
		self.readables.extend_from_slice(&defined_function.parameter_bindings);

		self.scope_stack.push(defined_function.subroutine);
		self.file = Some(DebugFile { file_index: defined_function.file_index });

		let void_returning = function.return_type.is_void(type_store);
		self.state = State::InFunction { function_id, void_returning };
	}

	fn generate_if_else_chain<'a, 'b>(
		&mut self,
		context: &mut codegen::Context<'a, 'b>,
		chain_expression: &'b IfElseChain<'a>,
		mut condition_callback: impl FnMut(&mut codegen::Context<'a, 'b>, &mut Self, &'b Expression<'a>) -> Self::Binding,
		mut body_callback: impl FnMut(&mut codegen::Context<'a, 'b>, &mut Self, &'b Block<'a>, bool),
	) {
		let original_block = unsafe { LLVMGetInsertBlock(self.builder) };
		let function = unsafe { LLVMGetBasicBlockParent(original_block) };
		let following_block = unsafe { LLVMAppendBasicBlockInContext(self.context, function, c"if_else.following".as_ptr()) };

		let mut next_condition_block = unsafe {
			let condition_block = LLVMAppendBasicBlockInContext(self.context, function, c"if_else.condition".as_ptr());
			LLVMBuildBr(self.builder, condition_block);
			condition_block
		};

		for entry in &chain_expression.entries {
			let condition_block = next_condition_block;
			next_condition_block =
				unsafe { LLVMAppendBasicBlockInContext(self.context, function, c"if_else.condition".as_ptr()) };

			let old_in_check_is = self.in_check_is;
			self.in_check_is = InCheckIs::IfElse;
			self.if_follow_blocks.push(next_condition_block);

			unsafe { LLVMPositionBuilderAtEnd(self.builder, condition_block) };
			let condition = condition_callback(context, self, &entry.condition);
			let final_condition_block = unsafe { LLVMGetInsertBlock(self.builder) };
			let condition = condition.to_value(self.builder);

			self.if_follow_blocks.pop().unwrap();
			self.in_check_is = old_in_check_is;

			let if_block = unsafe { LLVMAppendBasicBlockInContext(self.context, function, c"if_else.if".as_ptr()) };

			unsafe {
				LLVMPositionBuilderAtEnd(self.builder, final_condition_block);
				let zero = LLVMConstNull(LLVMTypeOf(condition));
				let flag = LLVMBuildICmp(self.builder, LLVMIntNE, condition, zero, c"if_else.flag".as_ptr());
				LLVMBuildCondBr(self.builder, flag, if_block, next_condition_block);
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
			let block = next_condition_block;
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
			let block = next_condition_block;
			let else_name = "if_else.non_existant_else";
			unsafe {
				LLVMSetValueName2(LLVMBasicBlockAsValue(block), else_name.as_ptr() as _, else_name.len());
				LLVMPositionBuilderAtEnd(self.builder, block);
				LLVMBuildBr(self.builder, following_block);
			}
		}

		unsafe { LLVMPositionBuilderAtEnd(self.builder, following_block) };
	}

	fn generate_match<'a, 'b>(
		&mut self,
		context: &mut codegen::Context<'a, 'b>,
		value: Self::Binding,
		enum_shape_index: usize,
		enum_specialization_index: usize,
		match_expression: &'b Match<'a>,
		mut body_callback: impl FnMut(&mut codegen::Context<'a, 'b>, &mut Self, &'b Block<'a>),
	) {
		let ValuePointer { pointer, .. } = self.value_auto_deref_pointer(context.type_store, value);

		let tag_type = {
			let user_types = context.type_store.user_types.read();
			let user_type = user_types[enum_shape_index].read();
			let UserTypeKind::Enum { shape } = &user_type.kind else {
				unreachable!();
			};
			numeric_kind_to_llvm_type(self.context, shape.tag.kind)
		};
		let tag = unsafe { LLVMBuildLoad2(self.builder, tag_type, pointer, c"".as_ptr()) };

		let original_block = unsafe { LLVMGetInsertBlock(self.builder) };
		let function = unsafe { LLVMGetBasicBlockParent(original_block) };
		let else_block = unsafe { LLVMAppendBasicBlockInContext(self.context, function, c"match.else".as_ptr()) };
		let following_block = unsafe { LLVMAppendBasicBlockInContext(self.context, function, c"match.following".as_ptr()) };

		let switch = unsafe { LLVMBuildSwitch(self.builder, tag, else_block, match_expression.arms.len() as u32) };

		for arm in &match_expression.arms {
			let case_block = unsafe { LLVMAppendBasicBlockInContext(self.context, function, c"case".as_ptr()) };

			for info in &arm.variant_infos {
				unsafe {
					let expected = LLVMConstInt(tag_type, info.tag_value as _, false as _);
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
						let llvm_struct = shape[enum_specialization_index].actual;
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

	fn generate_while<'a, 'b>(
		&mut self,
		context: &mut codegen::Context<'a, 'b>,
		debug_location: DebugLocation,
		condition_callback: impl FnOnce(&mut codegen::Context<'a, 'b>, &mut Self) -> Self::Binding,
		body_callback: impl FnOnce(&mut codegen::Context<'a, 'b>, &mut Self),
	) {
		let _debug_scope = self.create_debug_scope(debug_location);

		self.start_block();

		let original_block = unsafe { LLVMGetInsertBlock(self.builder) };
		let function = unsafe { LLVMGetBasicBlockParent(original_block) };
		let condition_block = unsafe { LLVMAppendBasicBlockInContext(self.context, function, c"while.condition".as_ptr()) };
		self.loop_condition_blocks.push(condition_block);
		let while_block = unsafe { LLVMAppendBasicBlockInContext(self.context, function, c"while.body".as_ptr()) };

		let following_block = unsafe { LLVMAppendBasicBlockInContext(self.context, function, c"while.following".as_ptr()) };
		self.loop_follow_blocks.push(following_block);
		let old_in_check_is = self.in_check_is;
		self.in_check_is = InCheckIs::WhileLoop;

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

		self.in_check_is = old_in_check_is;

		unsafe { LLVMPositionBuilderAtEnd(self.builder, while_block) };
		body_callback(context, self);

		unsafe {
			let current_block = LLVMGetInsertBlock(self.builder);
			if LLVMGetBasicBlockTerminator(current_block).is_null() {
				LLVMBuildBr(self.builder, condition_block);
			}
		}

		unsafe { LLVMPositionBuilderAtEnd(self.builder, following_block) };
		self.loop_condition_blocks.pop().unwrap();
		self.loop_follow_blocks.pop().unwrap();

		self.end_block();
	}

	fn generate_for_array<'a, 'b>(
		&mut self,
		context: &mut codegen::Context<'a, 'b>,
		statement: &'b crate::frontend::ir::For<'a>,
		initializer: Self::Binding,
		debug_location: DebugLocation,
		body_callback: impl FnOnce(&mut codegen::Context<'a, 'b>, &mut Self),
	) {
		let is_array_kind = matches!(statement.kind, ForKind::InArray | ForKind::OfArray);
		assert!(is_array_kind, "{:?}", statement.kind);
		let by_pointer = statement.kind == ForKind::OfArray;

		let _debug_scope = self.create_debug_scope(debug_location);

		self.start_block();

		let as_array = context
			.specialize_type_id(statement.initializer.type_id)
			.as_array(&mut context.type_store.type_entries)
			.unwrap();
		let array_entry_type_id = as_array.item_type_id;
		let array_entry_type = self
			.llvm_types
			.type_to_llvm_type(self.context, context.type_store, array_entry_type_id);

		unsafe {
			let original_block = LLVMGetInsertBlock(self.builder);
			let function = LLVMGetBasicBlockParent(original_block);

			let condition_block = LLVMAppendBasicBlockInContext(self.context, function, c"for_array.condition_block".as_ptr());

			let iterate_block = LLVMAppendBasicBlockInContext(self.context, function, c"for_array.iterate_block".as_ptr());
			self.loop_condition_blocks.push(iterate_block);

			let body_block = LLVMAppendBasicBlockInContext(self.context, function, c"".as_ptr());

			let following_block = LLVMAppendBasicBlockInContext(self.context, function, c"for.following".as_ptr());
			self.loop_follow_blocks.push(following_block);

			let i64_type = LLVMInt64TypeInContext(self.context);
			let i1_type = LLVMInt1TypeInContext(self.context);

			let pointer = self.value_pointer(initializer);
			let len = LLVMConstInt(i64_type, as_array.length, false as _);

			let zero = LLVMConstNull(i64_type);
			let iteration_alloca = self.build_alloca(i64_type, c"for_array.iteration_alloca");
			LLVMBuildStore(self.builder, zero, iteration_alloca);

			let item_type_id = context.specialize_type_id(statement.item.type_id);
			let item_type = self
				.llvm_types
				.type_to_llvm_type(self.context, context.type_store, item_type_id);

			let item_alloca = self.build_alloca(item_type, c"for_array.item");
			let kind = BindingKind::Pointer { pointer: item_alloca, pointed_type: item_type };
			let binding = Binding { type_id: item_type_id, kind };
			assert_eq!(self.readables.len(), statement.item.readable_index);
			self.readables.push(Some(binding));

			let index_alloca = if let Some(index) = statement.index {
				let index_alloca = self.build_alloca(i64_type, c"for_array.index_alloca");
				LLVMBuildStore(self.builder, zero, index_alloca);

				let kind = BindingKind::Pointer { pointer: index_alloca, pointed_type: i64_type };
				let binding = Binding { type_id: index.type_id, kind };
				assert_eq!(self.readables.len(), index.readable_index);
				self.readables.push(Some(binding));

				Some(index_alloca)
			} else {
				None
			};

			let is_last_alloca = if let Some(is_last) = statement.is_last {
				let false_value = LLVMConstInt(i1_type, 0, false as _);
				let is_last_alloca = self.build_alloca(i64_type, c"for_slice.is_last_alloca");
				LLVMBuildStore(self.builder, false_value, is_last_alloca);

				let kind = BindingKind::Pointer { pointer: is_last_alloca, pointed_type: i1_type };
				let binding = Binding { type_id: is_last.type_id, kind };
				assert_eq!(self.readables.len(), is_last.readable_index);
				self.readables.push(Some(binding));

				Some(is_last_alloca)
			} else {
				None
			};

			LLVMBuildBr(self.builder, condition_block);

			LLVMPositionBuilderAtEnd(self.builder, iterate_block);

			let one = LLVMConstInt(i64_type, 1, false as _);
			let original = LLVMBuildLoad2(self.builder, i64_type, iteration_alloca, c"".as_ptr());
			let added = LLVMBuildAdd(self.builder, original, one, c"".as_ptr());
			LLVMBuildStore(self.builder, added, iteration_alloca);

			if let Some(index_alloca) = index_alloca {
				let original = LLVMBuildLoad2(self.builder, i64_type, index_alloca, c"".as_ptr());
				let added = LLVMBuildAdd(self.builder, original, one, c"".as_ptr());
				LLVMBuildStore(self.builder, added, index_alloca);
			}

			LLVMBuildBr(self.builder, condition_block);

			LLVMPositionBuilderAtEnd(self.builder, condition_block);

			let iteration = LLVMBuildLoad2(self.builder, i64_type, iteration_alloca, c"".as_ptr());
			let reached_end = LLVMBuildICmp(self.builder, LLVMIntSGE, iteration, len, c"".as_ptr());

			if let Some(is_last_alloca) = is_last_alloca {
				// It's fine if this overflows as we won't use the result unless we are still iterating and we can only
				// possibly overflow if we've reached the end at which point we won't observe this value
				let next = LLVMBuildAdd(self.builder, iteration, one, c"".as_ptr());
				let reached_last = LLVMBuildICmp(self.builder, LLVMIntSGE, next, len, c"".as_ptr());
				LLVMBuildStore(self.builder, reached_last, is_last_alloca);
			}

			LLVMBuildCondBr(self.builder, reached_end, following_block, body_block);

			LLVMPositionBuilderAtEnd(self.builder, body_block);

			let iteration = LLVMBuildLoad2(self.builder, i64_type, iteration_alloca, c"".as_ptr());
			let indicies = &mut [iteration];
			let adjusted = LLVMBuildGEP2(
				self.builder,
				array_entry_type,
				pointer,
				indicies.as_mut_ptr(),
				indicies.len() as u32,
				c"".as_ptr(),
			);

			if by_pointer {
				LLVMBuildStore(self.builder, adjusted, item_alloca);
			} else {
				let value = LLVMBuildLoad2(self.builder, item_type, adjusted, c"".as_ptr());
				LLVMBuildStore(self.builder, value, item_alloca);
			}

			body_callback(context, self);

			let current_block = LLVMGetInsertBlock(self.builder);
			if LLVMGetBasicBlockTerminator(current_block).is_null() {
				LLVMBuildBr(self.builder, iterate_block);
			}

			LLVMPositionBuilderAtEnd(self.builder, following_block);
		}

		self.loop_condition_blocks.pop();
		self.loop_follow_blocks.pop();

		self.end_block();
	}

	fn generate_for_slice<'a, 'b>(
		&mut self,
		context: &mut codegen::Context<'a, 'b>,
		statement: &'b crate::frontend::ir::For<'a>,
		initializer: Self::Binding,
		debug_location: DebugLocation,
		body_callback: impl FnOnce(&mut codegen::Context<'a, 'b>, &mut Self),
	) {
		let is_slice_kind = matches!(statement.kind, ForKind::InSlice | ForKind::OfSlice);
		assert!(is_slice_kind, "{:?}", statement.kind);
		let by_pointer = statement.kind == ForKind::OfSlice;

		let _debug_scope = self.create_debug_scope(debug_location);

		self.start_block();

		let sliced_type_id = context
			.specialize_type_id(statement.initializer.type_id)
			.as_slice(&mut context.type_store.type_entries)
			.unwrap()
			.item_type_id;
		let sliced_type = self
			.llvm_types
			.type_to_llvm_type(self.context, context.type_store, sliced_type_id);

		unsafe {
			let original_block = LLVMGetInsertBlock(self.builder);
			let function = LLVMGetBasicBlockParent(original_block);

			let condition_block = LLVMAppendBasicBlockInContext(self.context, function, c"for_slice.condition_block".as_ptr());

			let iterate_block = LLVMAppendBasicBlockInContext(self.context, function, c"for_slice.iterate_block".as_ptr());
			self.loop_condition_blocks.push(iterate_block);

			let body_block = LLVMAppendBasicBlockInContext(self.context, function, c"".as_ptr());

			let following_block = LLVMAppendBasicBlockInContext(self.context, function, c"for.following".as_ptr());
			self.loop_follow_blocks.push(following_block);

			let i64_type = LLVMInt64TypeInContext(self.context);
			let i1_type = LLVMInt1TypeInContext(self.context);
			let opaque_pointer_type = self.llvm_types.opaque_pointer;

			let slice_pointer = self.value_pointer(initializer);
			let pointer_pointer = LLVMBuildStructGEP2(self.builder, self.llvm_types.slice_struct, slice_pointer, 0, c"".as_ptr());
			let pointer = LLVMBuildLoad2(self.builder, opaque_pointer_type, pointer_pointer, c"for_slice.slice_pointer".as_ptr());
			let len_pointer = LLVMBuildStructGEP2(self.builder, self.llvm_types.slice_struct, slice_pointer, 1, c"".as_ptr());
			let len = LLVMBuildLoad2(self.builder, i64_type, len_pointer, c"for_slice.slice_len".as_ptr());

			let zero = LLVMConstNull(i64_type);
			let iteration_alloca = self.build_alloca(i64_type, c"for_slice.iteration_alloca");
			LLVMBuildStore(self.builder, zero, iteration_alloca);

			let item_type_id = context.specialize_type_id(statement.item.type_id);
			let item_type = self
				.llvm_types
				.type_to_llvm_type(self.context, context.type_store, item_type_id);

			let item_alloca = self.build_alloca(item_type, c"for_slice.item");
			let kind = BindingKind::Pointer { pointer: item_alloca, pointed_type: item_type };
			let binding = Binding { type_id: item_type_id, kind };
			assert_eq!(self.readables.len(), statement.item.readable_index);
			self.readables.push(Some(binding));

			let index_alloca = if let Some(index) = statement.index {
				let index_alloca = self.build_alloca(i64_type, c"for_slice.index_alloca");
				LLVMBuildStore(self.builder, zero, index_alloca);

				let kind = BindingKind::Pointer { pointer: index_alloca, pointed_type: i64_type };
				let binding = Binding { type_id: index.type_id, kind };
				assert_eq!(self.readables.len(), index.readable_index);
				self.readables.push(Some(binding));

				Some(index_alloca)
			} else {
				None
			};

			let is_last_alloca = if let Some(is_last) = statement.is_last {
				let false_value = LLVMConstInt(i1_type, 0, false as _);
				let is_last_alloca = self.build_alloca(i64_type, c"for_slice.is_last_alloca");
				LLVMBuildStore(self.builder, false_value, is_last_alloca);

				let kind = BindingKind::Pointer { pointer: is_last_alloca, pointed_type: i1_type };
				let binding = Binding { type_id: is_last.type_id, kind };
				assert_eq!(self.readables.len(), is_last.readable_index);
				self.readables.push(Some(binding));

				Some(is_last_alloca)
			} else {
				None
			};

			LLVMBuildBr(self.builder, condition_block);

			LLVMPositionBuilderAtEnd(self.builder, iterate_block);

			let one = LLVMConstInt(i64_type, 1, false as _);
			let original = LLVMBuildLoad2(self.builder, i64_type, iteration_alloca, c"".as_ptr());
			let added = LLVMBuildAdd(self.builder, original, one, c"".as_ptr());
			LLVMBuildStore(self.builder, added, iteration_alloca);

			if let Some(index_alloca) = index_alloca {
				let original = LLVMBuildLoad2(self.builder, i64_type, index_alloca, c"".as_ptr());
				let added = LLVMBuildAdd(self.builder, original, one, c"".as_ptr());
				LLVMBuildStore(self.builder, added, index_alloca);
			}

			LLVMBuildBr(self.builder, condition_block);

			LLVMPositionBuilderAtEnd(self.builder, condition_block);

			let iteration = LLVMBuildLoad2(self.builder, i64_type, iteration_alloca, c"".as_ptr());
			let reached_end = LLVMBuildICmp(self.builder, LLVMIntSGE, iteration, len, c"".as_ptr());

			if let Some(is_last_alloca) = is_last_alloca {
				// It's fine if this overflows as we won't use the result unless we are still iterating and we can only
				// possibly overflow if we've reached the end at which point we won't observe this value
				let next = LLVMBuildAdd(self.builder, iteration, one, c"".as_ptr());
				let reached_last = LLVMBuildICmp(self.builder, LLVMIntSGE, next, len, c"".as_ptr());
				LLVMBuildStore(self.builder, reached_last, is_last_alloca);
			}

			LLVMBuildCondBr(self.builder, reached_end, following_block, body_block);

			LLVMPositionBuilderAtEnd(self.builder, body_block);

			let iteration = LLVMBuildLoad2(self.builder, i64_type, iteration_alloca, c"".as_ptr());
			let indicies = &mut [iteration];
			let adjusted = LLVMBuildGEP2(
				self.builder,
				sliced_type,
				pointer,
				indicies.as_mut_ptr(),
				indicies.len() as u32,
				c"".as_ptr(),
			);

			if by_pointer {
				LLVMBuildStore(self.builder, adjusted, item_alloca);
			} else {
				let value = LLVMBuildLoad2(self.builder, item_type, adjusted, c"".as_ptr());
				LLVMBuildStore(self.builder, value, item_alloca);
			}

			body_callback(context, self);

			let current_block = LLVMGetInsertBlock(self.builder);
			if LLVMGetBasicBlockTerminator(current_block).is_null() {
				LLVMBuildBr(self.builder, iterate_block);
			}

			LLVMPositionBuilderAtEnd(self.builder, following_block);
		}

		self.loop_condition_blocks.pop();
		self.loop_follow_blocks.pop();

		self.end_block();
	}

	fn generate_for_range<'a, 'b>(
		&mut self,
		context: &mut codegen::Context<'a, 'b>,
		statement: &'b crate::frontend::ir::For<'a>,
		initializer: Self::Binding,
		debug_location: DebugLocation,
		body_callback: impl FnOnce(&mut codegen::Context<'a, 'b>, &mut Self),
	) {
		assert_eq!(statement.kind, ForKind::Range);
		let _debug_scope = self.create_debug_scope(debug_location);

		self.start_block();

		unsafe {
			let original_block = LLVMGetInsertBlock(self.builder);
			let function = LLVMGetBasicBlockParent(original_block);

			let range_inverted_failure_block =
				LLVMAppendBasicBlockInContext(self.context, function, c"for_range.range_inverted_failure_block".as_ptr());

			let setup_block = LLVMAppendBasicBlockInContext(self.context, function, c"for_range.setup_block".as_ptr());

			let condition_block = LLVMAppendBasicBlockInContext(self.context, function, c"for_range.condition_block".as_ptr());

			let iterate_block = LLVMAppendBasicBlockInContext(self.context, function, c"for_range.iterate_block".as_ptr());
			self.loop_condition_blocks.push(iterate_block);

			let body_block = LLVMAppendBasicBlockInContext(self.context, function, c"".as_ptr());

			let following_block = LLVMAppendBasicBlockInContext(self.context, function, c"for.following".as_ptr());
			self.loop_follow_blocks.push(following_block);

			let i64_type = LLVMInt64TypeInContext(self.context);
			let i1_type = LLVMInt1TypeInContext(self.context);

			let range_pointer = self.value_pointer(initializer);
			let start_pointer = LLVMBuildStructGEP2(self.builder, self.llvm_types.range_struct, range_pointer, 0, c"".as_ptr());
			let start = LLVMBuildLoad2(self.builder, i64_type, start_pointer, c"for_range.range_start".as_ptr());
			let end_pointer = LLVMBuildStructGEP2(self.builder, self.llvm_types.range_struct, range_pointer, 1, c"".as_ptr());
			let end = LLVMBuildLoad2(self.builder, i64_type, end_pointer, c"for_range.range_end".as_ptr());

			let inverted = LLVMBuildICmp(self.builder, LLVMIntSGT, start, end, c"".as_ptr());
			LLVMBuildCondBr(self.builder, inverted, range_inverted_failure_block, setup_block);

			LLVMPositionBuilderAtEnd(self.builder, range_inverted_failure_block);
			let failure_args = [Some(initializer)];
			self.generate_call(
				context.type_store,
				context.lang_items.for_range_inverted.unwrap(),
				&failure_args,
				statement.initializer.debug_location,
			);
			LLVMBuildUnreachable(self.builder);

			LLVMPositionBuilderAtEnd(self.builder, setup_block);

			let iteration_alloca = self.build_alloca(i64_type, c"for_range.iteration_alloca");
			LLVMBuildStore(self.builder, start, iteration_alloca);

			let kind = BindingKind::Pointer { pointer: iteration_alloca, pointed_type: i64_type };
			let binding = Binding { type_id: statement.item.type_id, kind };
			assert_eq!(self.readables.len(), statement.item.readable_index);
			self.readables.push(Some(binding));

			let index_alloca = if let Some(index) = statement.index {
				let zero = LLVMConstNull(i64_type);
				let index_alloca = self.build_alloca(i64_type, c"for_range.index_alloca");
				LLVMBuildStore(self.builder, zero, index_alloca);

				let kind = BindingKind::Pointer { pointer: index_alloca, pointed_type: i64_type };
				let binding = Binding { type_id: index.type_id, kind };
				assert_eq!(self.readables.len(), index.readable_index);
				self.readables.push(Some(binding));

				Some(index_alloca)
			} else {
				None
			};

			let is_last_alloca = if let Some(is_last) = statement.is_last {
				let false_value = LLVMConstInt(i1_type, 0, false as _);
				let is_last_alloca = self.build_alloca(i64_type, c"for_range.is_last_alloca");
				LLVMBuildStore(self.builder, false_value, is_last_alloca);

				let kind = BindingKind::Pointer { pointer: is_last_alloca, pointed_type: i1_type };
				let binding = Binding { type_id: is_last.type_id, kind };
				assert_eq!(self.readables.len(), is_last.readable_index);
				self.readables.push(Some(binding));

				Some(is_last_alloca)
			} else {
				None
			};

			LLVMBuildBr(self.builder, condition_block);

			LLVMPositionBuilderAtEnd(self.builder, iterate_block);

			let one = LLVMConstInt(i64_type, 1, false as _);
			let original = LLVMBuildLoad2(self.builder, i64_type, iteration_alloca, c"".as_ptr());
			let added = LLVMBuildAdd(self.builder, original, one, c"".as_ptr());
			LLVMBuildStore(self.builder, added, iteration_alloca);

			if let Some(index_alloca) = index_alloca {
				let original = LLVMBuildLoad2(self.builder, i64_type, index_alloca, c"".as_ptr());
				let added = LLVMBuildAdd(self.builder, original, one, c"".as_ptr());
				LLVMBuildStore(self.builder, added, index_alloca);
			}

			LLVMBuildBr(self.builder, condition_block);

			LLVMPositionBuilderAtEnd(self.builder, condition_block);

			let iteration = LLVMBuildLoad2(self.builder, i64_type, iteration_alloca, c"".as_ptr());
			let reached_end = LLVMBuildICmp(self.builder, LLVMIntSGE, iteration, end, c"".as_ptr());

			if let Some(is_last_alloca) = is_last_alloca {
				// It's fine if this overflows as we won't use the result unless we are still iterating and we can only
				// possibly overflow if we've reached the end at which point we won't observe this value
				let next = LLVMBuildAdd(self.builder, iteration, one, c"".as_ptr());
				let reached_last = LLVMBuildICmp(self.builder, LLVMIntSGE, next, end, c"".as_ptr());
				LLVMBuildStore(self.builder, reached_last, is_last_alloca);
			}

			LLVMBuildCondBr(self.builder, reached_end, following_block, body_block);

			LLVMPositionBuilderAtEnd(self.builder, body_block);
			body_callback(context, self);

			let current_block = LLVMGetInsertBlock(self.builder);
			if LLVMGetBasicBlockTerminator(current_block).is_null() {
				LLVMBuildBr(self.builder, iterate_block);
			}

			LLVMPositionBuilderAtEnd(self.builder, following_block);
		}

		self.loop_condition_blocks.pop();
		self.loop_follow_blocks.pop();

		self.end_block();
	}

	fn generate_number_value(&mut self, type_store: &TypeStore, type_id: TypeId, value: Decimal) -> Self::Binding {
		let value = unsafe {
			match type_id.numeric_kind(type_store).unwrap() {
				NumericKind::I8 => LLVMConstInt(LLVMInt8TypeInContext(self.context), value.to_i64().unwrap() as u64, false as _),
				NumericKind::U8 => LLVMConstInt(LLVMInt8TypeInContext(self.context), value.to_u64().unwrap(), false as _),

				NumericKind::I16 => {
					LLVMConstInt(LLVMInt16TypeInContext(self.context), value.to_i64().unwrap() as u64, false as _)
				}
				NumericKind::U16 => LLVMConstInt(LLVMInt16TypeInContext(self.context), value.to_u64().unwrap(), false as _),

				NumericKind::I32 => {
					LLVMConstInt(LLVMInt32TypeInContext(self.context), value.to_i64().unwrap() as u64, false as _)
				}
				NumericKind::U32 => LLVMConstInt(LLVMInt32TypeInContext(self.context), value.to_u64().unwrap(), false as _),

				NumericKind::I64 | NumericKind::ISize => {
					LLVMConstInt(LLVMInt64TypeInContext(self.context), value.to_i64().unwrap() as u64, false as _)
				}
				NumericKind::U64 | NumericKind::USize => {
					LLVMConstInt(LLVMInt64TypeInContext(self.context), value.to_u64().unwrap(), false as _)
				}

				NumericKind::F32 => LLVMConstReal(LLVMFloatTypeInContext(self.context), value.to_f64().unwrap()),

				NumericKind::F64 => LLVMConstReal(LLVMDoubleTypeInContext(self.context), value.to_f64().unwrap()),
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
			let array = LLVMConstStringInContext2(self.context, text.as_ptr() as _, text.len(), false as _);
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
		array_type_id: TypeId,
		debug_location: DebugLocation,
	) -> Self::Binding {
		assert!(!elements.is_empty());
		let _debug_scope = self.create_debug_scope(debug_location);

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
					c"generate_array_literal.pointer".as_ptr(),
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

		let kind = BindingKind::Pointer { pointer: alloca, pointed_type: array_type };
		Binding { type_id: array_type_id, kind }
	}

	fn generate_slice_literal(
		&mut self,
		type_store: &mut TypeStore,
		elements: &[Self::Binding],
		element_type_id: TypeId,
		slice_type_id: TypeId,
		debug_location: DebugLocation,
	) -> Self::Binding {
		assert!(!elements.is_empty());
		let _debug_scope = self.create_debug_scope(debug_location);

		let element_type = self.llvm_types.type_to_llvm_type(self.context, type_store, element_type_id);
		let array_type = unsafe { LLVMArrayType2(element_type, elements.len() as u64) };
		let alloca = self.build_alloca(array_type, c"generate_slice_literal.array_alloca");

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
					c"generate_slice_literal.array_literal_pointer".as_ptr(),
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
		let slice_alloca = self.build_alloca(slice_type, c"generate_slice_literal.slice_alloca");

		unsafe {
			let pointer_pointer = LLVMBuildStructGEP2(
				self.builder,
				slice_type,
				slice_alloca,
				0,
				c"generate_slice_literal.pointer_pointer".as_ptr(),
			);
			LLVMBuildStore(self.builder, alloca, pointer_pointer);

			let len_pointer =
				LLVMBuildStructGEP2(self.builder, slice_type, slice_alloca, 1, c"generate_slice_literal.len_pointer".as_ptr());
			let len = LLVMConstInt(LLVMInt64TypeInContext(self.context), elements.len() as u64, false as _);
			LLVMBuildStore(self.builder, len, len_pointer);
		}

		let slice_type = self.llvm_types.slice_struct;
		let kind = BindingKind::Pointer { pointer: slice_alloca, pointed_type: slice_type };
		Binding { type_id: slice_type_id, kind }
	}

	fn generate_struct_literal(
		&mut self,
		type_id: TypeId,
		shape_index: usize,
		specialization_index: usize,
		fields: &[Option<Self::Binding>],
		debug_location: DebugLocation,
	) -> Self::Binding {
		let _debug_scope = self.create_debug_scope(debug_location);

		let struct_type = self.llvm_types.user_type_structs[shape_index][specialization_index].actual;

		let alloca = self.build_alloca(struct_type, c"generate_struct_literal.alloca");
		for (index, field) in fields.iter().enumerate() {
			let Some(field) = field else {
				continue;
			};

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

	fn generate_integer_bitflags_literal(
		&mut self,
		type_id: TypeId,
		tag_kind: NumericKind,
		shape_index: usize,
		specialization_index: usize,
		value: u64,
		debug_location: DebugLocation,
	) -> Self::Binding {
		let _debug_scope = self.create_debug_scope(debug_location);

		let struct_type = self.llvm_types.user_type_structs[shape_index][specialization_index].actual;
		let tag_type = numeric_kind_to_llvm_type(self.context, tag_kind);

		let alloca = self.build_alloca(struct_type, c"generate_integer_bitflags_literal.alloca");

		unsafe {
			let tag_value = LLVMConstInt(tag_type, value, false as _);
			let tag_pointer = LLVMBuildStructGEP2(self.builder, struct_type, alloca, 0, c"".as_ptr());
			LLVMBuildStore(self.builder, tag_value, tag_pointer);
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
		let binding = abi.call_function(self, type_store, &self.attribute_kinds, function, arguments);
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
		let binding = abi.call_function(self, type_store, &self.attribute_kinds, function, arguments);
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
		lang_items: &LangItems,
		type_store: &mut TypeStore,
		base: Self::Binding,
		field_index: usize,
		debug_location: DebugLocation,
	) -> Option<Self::Binding> {
		let _debug_scope = self.create_debug_scope(debug_location);

		let index = field_index as u32;
		let ValuePointer { pointer, mut pointed_type, type_id } = self.value_auto_deref_pointer(type_store, base);

		let field_type;
		let field_pointer;

		let entry = type_store.type_entries.get(type_id);
		let type_id = match entry.kind {
			TypeEntryKind::UserType { shape_index, specialization_index, .. } => {
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

						// The first field of any enum is the tag which may be represented with various sized integers
						// depending on the padding required so we need to override the type which we will read that field as
						if field_index == 0 {
							field_type = numeric_kind_to_llvm_type(self.context, shape.tag.kind);
						} else {
							field_type = unsafe { LLVMStructGetTypeAtIndex(pointed_type, index) }
						}

						let enum_type = &shape.specializations[specialization_index];
						enum_type.shared_fields[field_index].type_id
					}

					UserTypeKind::Union { shape } => {
						let user_type_struct = &self.llvm_types.user_type_structs[shape_index];
						let as_union_variants = &user_type_struct[specialization_index].as_union_variants;
						field_type = as_union_variants[field_index];
						field_pointer = pointer;
						let union_type = &shape.specializations[specialization_index];
						union_type.fields[field_index].type_id
					}
				}
			}

			TypeEntryKind::BuiltinType { kind: PrimativeKind::String | PrimativeKind::StringMut, .. } => {
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

			TypeEntryKind::BuiltinType { kind: PrimativeKind::FormatString, .. } => {
				assert_eq!(index, 0);
				field_type = self.llvm_types.slice_struct;
				field_pointer = pointer;
				type_store.slice_of(lang_items.format_string_item_type.unwrap(), false)
			}

			TypeEntryKind::Array(Array { item_type_id, length, .. }) => {
				match index {
					// Pointer
					0 => {
						let pointer_type_id = type_store.pointer_to(item_type_id, true);
						let kind = BindingKind::Value(pointer);
						return Some(Binding { type_id: pointer_type_id, kind });
					}

					// Length
					1 => {
						let value = unsafe { LLVMConstInt(LLVMInt64TypeInContext(self.context), length, false as _) };
						let kind = BindingKind::Value(value);
						return Some(Binding { type_id: type_store.isize_type_id(), kind });
					}

					// Slice
					2 => {
						let slice_type = self.llvm_types.slice_struct;
						let slice_alloca = self.build_alloca(slice_type, c"generate_field_read.array_to_slice");

						unsafe {
							let pointer_pointer = LLVMBuildStructGEP2(
								self.builder,
								slice_type,
								slice_alloca,
								0,
								c"generate_field_read.array_to_slice.pointer_pointer".as_ptr(),
							);
							LLVMBuildStore(self.builder, pointer, pointer_pointer);

							let i64_type = LLVMInt64TypeInContext(self.context);
							let len = LLVMConstInt(i64_type, length, false as _);

							let len_pointer = LLVMBuildStructGEP2(
								self.builder,
								slice_type,
								slice_alloca,
								1,
								c"generate_field_read.array_to_slice.len_pointer".as_ptr(),
							);
							LLVMBuildStore(self.builder, len, len_pointer);
						};

						let slice_type_id = type_store.slice_of(item_type_id, true);
						let kind = BindingKind::Pointer { pointer: slice_alloca, pointed_type: slice_type };
						return Some(Binding { type_id: slice_type_id, kind });
					}

					_ => unreachable!("Array field index {index}"),
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

	fn generate_negate(&mut self, value: Self::Binding, type_id: TypeId, debug_location: DebugLocation) -> Self::Binding {
		let _debug_scope = self.create_debug_scope(debug_location);

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

	fn generate_invert(&mut self, value: Self::Binding, debug_location: DebugLocation) -> Self::Binding {
		let _debug_scope = self.create_debug_scope(debug_location);

		let type_id = value.type_id;
		let value = value.to_value(self.builder);
		let inverted = unsafe { LLVMBuildNot(self.builder, value, c"invert.inverted".as_ptr()) };
		let kind = BindingKind::Value(inverted);
		Binding { type_id, kind }
	}

	fn generate_bitwise_not(
		&mut self,
		type_store: &mut TypeStore,
		value: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding {
		let _debug_scope = self.create_debug_scope(debug_location);

		let type_entry = type_store.type_entries.get(value.type_id);
		if let TypeEntryKind::UserType { shape_index, .. } = type_entry.kind {
			let ValuePointer { pointer, pointed_type: enum_type, .. } = self.value_auto_deref_pointer(type_store, value);

			let user_types = type_store.user_types.read();
			let user_type = user_types[shape_index].read();
			let UserTypeKind::Enum { shape } = &user_type.kind else {
				unreachable!("{:#?}", &user_type.kind);
			};

			assert!(shape.is_bitflags);
			let tag_type = numeric_kind_to_llvm_type(self.context, shape.tag.kind);

			let alloca = self.build_alloca(enum_type, c"bitwise_not.bitflags_enum_alloca");

			unsafe {
				let tag_pointer = LLVMBuildStructGEP2(self.builder, enum_type, pointer, 0, c"".as_ptr());
				let tag_value = LLVMBuildLoad2(self.builder, tag_type, tag_pointer, c"".as_ptr());
				let inverted = LLVMBuildNot(self.builder, tag_value, c"bitwise_not.inverted_tag".as_ptr());

				let tag_pointer = LLVMBuildStructGEP2(self.builder, enum_type, alloca, 0, c"".as_ptr());
				LLVMBuildStore(self.builder, inverted, tag_pointer);
			}

			let kind = BindingKind::Pointer { pointer: alloca, pointed_type: enum_type };
			return Binding { type_id: value.type_id, kind };
		}

		let type_id = value.type_id;
		let value = value.to_value(self.builder);
		let inverted = unsafe { LLVMBuildNot(self.builder, value, c"bitwise_not.inverted".as_ptr()) };
		let kind = BindingKind::Value(inverted);
		Binding { type_id, kind }
	}

	fn generate_address_of(
		&mut self,
		base: Self::Binding,
		pointer_type_id: TypeId,
		debug_location: DebugLocation,
	) -> Self::Binding {
		let _debug_scope = self.create_debug_scope(debug_location);

		let pointer = self.value_pointer(base);
		let kind = BindingKind::Value(pointer);
		Binding { type_id: pointer_type_id, kind }
	}

	fn generate_dereference(
		&mut self,
		type_store: &mut TypeStore,
		base: Self::Binding,
		pointed_type_id: TypeId,
		debug_location: DebugLocation,
	) -> Self::Binding {
		let _debug_scope = self.create_debug_scope(debug_location);

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

	fn generate_cast(
		&mut self,
		type_store: &mut TypeStore,
		base: Self::Binding,
		to: TypeId,
		debug_location: DebugLocation,
	) -> Self::Binding {
		let _debug_scope = self.create_debug_scope(debug_location);

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

		let from_kind = base.type_id.numeric_kind(type_store).unwrap();

		// Int to int
		if from_int {
			if let Some(to_type) = to_int_type {
				let signed = from_kind.is_signed() as LLVMBool;
				let int = unsafe { LLVMBuildIntCast2(self.builder, from, to_type, signed, c"".as_ptr()) };
				let kind = BindingKind::Value(int);
				return Binding { type_id: to, kind };
			}
		}

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

	fn generate_array_index(
		&mut self,
		lang_items: &LangItems,
		type_store: &mut TypeStore,
		item_type: TypeId,
		base: Option<Self::Binding>,
		base_type_id: TypeId,
		index: Self::Binding,
		debug_location: DebugLocation,
	) -> Option<Self::Binding> {
		unsafe {
			let original_block = LLVMGetInsertBlock(self.builder);
			let function = LLVMGetBasicBlockParent(original_block);
			let failure_block =
				LLVMAppendBasicBlockInContext(self.context, function, c"generate_array_index.bounds_check_failure".as_ptr());
			let success_block = LLVMAppendBasicBlockInContext(self.context, function, c"".as_ptr());

			let as_array = base_type_id.as_array(&mut type_store.type_entries).unwrap();

			let i64_type = LLVMInt64TypeInContext(self.context);
			let len = LLVMConstInt(i64_type, as_array.length, false as _);

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
			self.generate_call(type_store, lang_items.array_index_out_of_bounds.unwrap(), &failure_args, debug_location);
			LLVMBuildUnreachable(self.builder);

			LLVMPositionBuilderAtEnd(self.builder, success_block);

			let item_layout = type_store.type_layout(item_type);
			if item_layout.size <= 0 {
				return None;
			}

			let pointer = self.value_pointer(base.unwrap());

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
			let failure_block =
				LLVMAppendBasicBlockInContext(self.context, function, c"generate_slice_index.bounds_check_failure".as_ptr());
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

	fn generate_array_slice(
		&mut self,
		lang_items: &LangItems,
		type_store: &mut TypeStore,
		item_type_id: TypeId,
		base: Self::Binding,
		range: Self::Binding,
		debug_location: DebugLocation,
	) -> Option<Self::Binding> {
		let as_array = base.type_id.as_array(&mut type_store.type_entries).unwrap();
		let resultant_slice_type_id = type_store.slice_of(item_type_id, true);

		unsafe {
			let original_block = LLVMGetInsertBlock(self.builder);
			let function = LLVMGetBasicBlockParent(original_block);

			let range_inverted_failure_block = LLVMAppendBasicBlockInContext(
				self.context,
				function,
				c"generate_array_slice.range_inverted_failure_block".as_ptr(),
			);
			let range_not_inverted_block = LLVMAppendBasicBlockInContext(self.context, function, c"".as_ptr());

			let range_start_out_of_bounds_failure_block = LLVMAppendBasicBlockInContext(
				self.context,
				function,
				c"generate_array_slice.range_start_out_of_bounds_failure_block".as_ptr(),
			);
			let start_in_bounds_block = LLVMAppendBasicBlockInContext(self.context, function, c"".as_ptr());

			let range_end_out_of_bounds_failure_block = LLVMAppendBasicBlockInContext(
				self.context,
				function,
				c"generate_array_slice.range_end_out_of_bounds_failure_block".as_ptr(),
			);
			let end_in_bounds_block = LLVMAppendBasicBlockInContext(self.context, function, c"".as_ptr());

			let i64_type = LLVMInt64TypeInContext(self.context);

			let range_pointer = self.value_pointer(range);
			let start_pointer = LLVMBuildStructGEP2(self.builder, self.llvm_types.range_struct, range_pointer, 0, c"".as_ptr());
			let start = LLVMBuildLoad2(self.builder, i64_type, start_pointer, c"generate_array_slice.range_start".as_ptr());
			let end_pointer = LLVMBuildStructGEP2(self.builder, self.llvm_types.range_struct, range_pointer, 1, c"".as_ptr());
			let end = LLVMBuildLoad2(self.builder, i64_type, end_pointer, c"generate_array_slice.range_end".as_ptr());

			let inverted = LLVMBuildICmp(self.builder, LLVMIntSGT, start, end, c"".as_ptr());
			LLVMBuildCondBr(self.builder, inverted, range_inverted_failure_block, range_not_inverted_block);

			LLVMPositionBuilderAtEnd(self.builder, range_inverted_failure_block);
			let failure_args = [Some(range)];
			self.generate_call(type_store, lang_items.array_range_inverted.unwrap(), &failure_args, debug_location);
			LLVMBuildUnreachable(self.builder);

			LLVMPositionBuilderAtEnd(self.builder, range_not_inverted_block);

			let pointer = self.value_pointer(base);
			let len = LLVMConstInt(i64_type, as_array.length, false as _);

			let zero = LLVMConstNull(i64_type);
			let start_greater_than_zero = LLVMBuildICmp(self.builder, LLVMIntSGE, start, zero, c"".as_ptr());
			let start_less_equal_len = LLVMBuildICmp(self.builder, LLVMIntSLE, start, len, c"".as_ptr());
			let start_in_bounds = LLVMBuildAnd(self.builder, start_greater_than_zero, start_less_equal_len, c"".as_ptr());

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
				lang_items.array_range_start_out_of_bounds.unwrap(),
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
				lang_items.array_range_end_out_of_bounds.unwrap(),
				&failure_args,
				debug_location,
			);
			LLVMBuildUnreachable(self.builder);

			LLVMPositionBuilderAtEnd(self.builder, end_in_bounds_block);

			let item_layout = type_store.type_layout(item_type_id);
			if item_layout.size <= 0 {
				return None;
			}

			let indicies = &mut [start];
			let pointed_type = self.llvm_types.type_to_llvm_type(self.context, type_store, item_type_id);
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
			Some(Binding { type_id: resultant_slice_type_id, kind })
		}
	}

	fn generate_slice_slice(
		&mut self,
		lang_items: &LangItems,
		type_store: &mut TypeStore,
		item_type_id: TypeId,
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
			let start_less_equal_len = LLVMBuildICmp(self.builder, LLVMIntSLE, start, len, c"".as_ptr());
			let start_in_bounds = LLVMBuildAnd(self.builder, start_greater_than_zero, start_less_equal_len, c"".as_ptr());

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

			let item_layout = type_store.type_layout(item_type_id);
			if item_layout.size <= 0 {
				return None;
			}

			let indicies = &mut [start];
			let pointed_type = self.llvm_types.type_to_llvm_type(self.context, type_store, item_type_id);
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

	fn generate_binary_operation<'a, 'b>(
		&mut self,
		context: &mut codegen::Context<'a, 'b>,
		left: &'b Expression<'a>,
		right: &'b Expression<'a>,
		op: BinaryOperator,
		source_type_id: TypeId,
		result_type_id: TypeId,
		debug_location: DebugLocation,
	) -> Option<Self::Binding> {
		let _debug_scope = self.create_debug_scope(debug_location);

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

		let mut should_phi_left_and_right = true;
		if matches!(op, BinaryOperator::LogicalAnd | BinaryOperator::LogicalIsAnd | BinaryOperator::LogicalOr) {
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

					BinaryOperator::LogicalIsAnd => {
						let following_block = match self.in_check_is {
							InCheckIs::IfElse => {
								should_phi_left_and_right = false;
								*self.if_follow_blocks.last().unwrap()
							}

							InCheckIs::WhileLoop => {
								should_phi_left_and_right = false;
								*self.loop_follow_blocks.last().unwrap()
							}

							InCheckIs::None => following_block,
						};

						LLVMBuildCondBr(self.builder, left, right_block, following_block)
					}

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
				let value = if should_phi_left_and_right {
					let phi = LLVMBuildPhi(
						self.builder,
						LLVMInt1TypeInContext(self.context),
						c"binary_operation.logical.result_phi".as_ptr(),
					);
					LLVMAddIncoming(phi, [left, right].as_mut_ptr(), [left_block, right_block].as_mut_ptr(), 2);
					phi
				} else {
					right
				};

				let kind = BindingKind::Value(value);
				return Some(Binding { type_id: result_type_id, kind });
			}
		}

		let left_binding = codegen::generate_expression(context, self, left).unwrap();
		let right_binding = codegen::generate_expression(context, self, right).unwrap();

		let type_entry = context.type_store.type_entries.get(left_binding.type_id);
		if let TypeEntryKind::UserType { shape_index, .. } = type_entry.kind {
			let ValuePointer { pointer: left_pointer, pointed_type: enum_type, .. } =
				self.value_auto_deref_pointer(context.type_store, left_binding);
			let ValuePointer { pointer: right_pointer, .. } = self.value_auto_deref_pointer(context.type_store, right_binding);

			let user_types = context.type_store.user_types.read();
			let user_type = user_types[shape_index].read();
			let UserTypeKind::Enum { shape } = &user_type.kind else {
				unreachable!("{:#?}", &user_type.kind);
			};

			assert!(shape.is_bitflags);
			let tag_type = numeric_kind_to_llvm_type(self.context, shape.tag.kind);

			let left_tag_pointer = unsafe { LLVMBuildStructGEP2(self.builder, enum_type, left_pointer, 0, c"".as_ptr()) };
			let right_tag_pointer = unsafe { LLVMBuildStructGEP2(self.builder, enum_type, right_pointer, 0, c"".as_ptr()) };

			let left_tag = unsafe { LLVMBuildLoad2(self.builder, tag_type, left_tag_pointer, c"".as_ptr()) };
			let right_tag = unsafe { LLVMBuildLoad2(self.builder, tag_type, right_tag_pointer, c"".as_ptr()) };

			let alloca = match op {
				BinaryOperator::BitwiseAnd | BinaryOperator::BitwiseAndAssign => unsafe {
					let resulting_tag = LLVMBuildAnd(self.builder, left_tag, right_tag, c"".as_ptr());
					if op == BinaryOperator::BitwiseAndAssign {
						LLVMBuildStore(self.builder, resulting_tag, left_tag_pointer);
						return None;
					}

					let alloca = self.build_alloca(enum_type, c"generate_binary_operation.bitflag_and_alloca");
					let tag_pointer = LLVMBuildStructGEP2(self.builder, enum_type, alloca, 0, c"".as_ptr());
					LLVMBuildStore(self.builder, resulting_tag, tag_pointer);
					alloca
				},

				BinaryOperator::BitwiseOr | BinaryOperator::BitwiseOrAssign => unsafe {
					let resulting_tag = LLVMBuildOr(self.builder, left_tag, right_tag, c"".as_ptr());
					if op == BinaryOperator::BitwiseOrAssign {
						LLVMBuildStore(self.builder, resulting_tag, left_tag_pointer);
						return None;
					}

					let alloca = self.build_alloca(enum_type, c"generate_binary_operation.bitflag_or_alloca");
					let tag_pointer = LLVMBuildStructGEP2(self.builder, enum_type, alloca, 0, c"".as_ptr());
					LLVMBuildStore(self.builder, resulting_tag, tag_pointer);
					alloca
				},

				BinaryOperator::BitwiseXor | BinaryOperator::BitwiseXorAssign => unsafe {
					let resulting_tag = LLVMBuildXor(self.builder, left_tag, right_tag, c"".as_ptr());
					if op == BinaryOperator::BitwiseXorAssign {
						LLVMBuildStore(self.builder, resulting_tag, left_tag_pointer);
						return None;
					}

					let alloca = self.build_alloca(enum_type, c"generate_binary_operation.bitflag_xor_alloca");
					let tag_pointer = LLVMBuildStructGEP2(self.builder, enum_type, alloca, 0, c"".as_ptr());
					LLVMBuildStore(self.builder, resulting_tag, tag_pointer);
					alloca
				},

				_ => unreachable!("{op:#?}"),
			};

			let kind = BindingKind::Pointer { pointer: alloca, pointed_type: enum_type };
			return Some(Binding { type_id: left_binding.type_id, kind });
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
			let target = match left_binding.kind {
				BindingKind::Pointer { pointer, .. } => pointer,
				BindingKind::Value(value) => unreachable!("{value:?}"),
			};
			let left = left_binding.to_value(self.builder);
			let right = right_binding.to_value(self.builder);
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

		let left = left_binding.to_value(self.builder);
		let right = right_binding.to_value(self.builder);
		unsafe { assert_eq!(LLVMTypeOf(left), LLVMTypeOf(right)) };

		if op == BinaryOperator::Range {
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

		let left_type_kind = unsafe { LLVMGetTypeKind(LLVMTypeOf(left)) };

		let value = if left_type_kind == LLVMIntegerTypeKind {
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
				| BinaryOperator::LogicalIsAnd
				| BinaryOperator::LogicalOr
				| BinaryOperator::Range => unreachable!(),
			}
		} else if left_type_kind == LLVMFloatTypeKind || left_type_kind == LLVMDoubleTypeKind {
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
				| BinaryOperator::LogicalIsAnd
				| BinaryOperator::LogicalOr
				| BinaryOperator::Range => unreachable!(),
			}
		} else if left_type_kind == LLVMPointerTypeKind {
			match op {
				BinaryOperator::Equals => unsafe { LLVMBuildICmp(self.builder, LLVMIntEQ, left, right, c"".as_ptr()) },
				BinaryOperator::NotEquals => unsafe { LLVMBuildICmp(self.builder, LLVMIntNE, left, right, c"".as_ptr()) },
				_ => unreachable!("{op:?}"),
			}
		} else {
			unreachable!("{left_type_kind:?}")
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
		debug_location: DebugLocation,
	) -> Self::Binding {
		let _debug_scope = self.create_debug_scope(debug_location);

		let ValuePointer { pointer, .. } = self.value_auto_deref_pointer(context.type_store, value);

		let (tag_type, is_bitflags) = {
			let user_types = context.type_store.user_types.read();
			let user_type = user_types[enum_shape_index].read();
			let UserTypeKind::Enum { shape } = &user_type.kind else {
				unreachable!();
			};

			let tag_type = numeric_kind_to_llvm_type(self.context, shape.tag.kind);
			(tag_type, shape.is_bitflags)
		};

		let result = unsafe {
			let i1_type = LLVMInt1TypeInContext(self.context);
			let tag = LLVMBuildLoad2(self.builder, tag_type, pointer, c"check_is.tag".as_ptr());

			let mut result = LLVMConstInt(i1_type, 0, false as _);
			let zero = LLVMConstInt(tag_type, 0, false as _);

			for info in &check_expression.variant_infos {
				let expected = LLVMConstInt(tag_type, info.tag_value as _, false as _);
				let flag = if is_bitflags {
					let bitwise_result = LLVMBuildAnd(self.builder, tag, expected, c"".as_ptr());
					LLVMBuildICmp(self.builder, LLVMIntNE, bitwise_result, zero, c"".as_ptr())
				} else {
					LLVMBuildICmp(self.builder, LLVMIntEQ, tag, expected, c"".as_ptr())
				};
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
				let llvm_struct = shape[enum_specialization_index].actual;
				let variant_pointer =
					unsafe { LLVMBuildStructGEP2(self.builder, llvm_struct, pointer, 1, c"check_is.variant_pointer".as_ptr()) };

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
		tag_value: i128,
		variant_binding: Option<Binding>,
	) -> Self::Binding {
		let shape = &self.llvm_types.user_type_structs[enum_shape_index];
		let enum_type = shape[enum_specialization_index].actual;

		let tag_type = {
			let user_types = type_store.user_types.read();
			let user_type = user_types[enum_shape_index].read();
			let UserTypeKind::Enum { shape } = &user_type.kind else {
				unreachable!();
			};
			numeric_kind_to_llvm_type(self.context, shape.tag.kind)
		};

		let alloca = self.build_alloca(enum_type, c"generate_enum_variant_to_enum.enum_alloca");

		unsafe {
			// TODO: This downcast to `u64` is a huge kludge
			let tag_value = LLVMConstInt(tag_type, tag_value as u64, false as _);
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

	fn generate_union_variant_to_union(
		&mut self,
		type_store: &mut TypeStore,
		union_type_id: TypeId,
		union_shape_index: usize,
		union_specialization_index: usize,
		variant_binding: Option<Binding>,
	) -> Self::Binding {
		let shape = &self.llvm_types.user_type_structs[union_shape_index];
		let union_type = shape[union_specialization_index].actual;

		let alloca = self.build_alloca(union_type, c"generate_union_variant_to_union.union_alloca");
		unsafe { LLVMBuildStore(self.builder, LLVMConstNull(union_type), alloca) };

		unsafe {
			if let Some(variant_binding) = variant_binding {
				match variant_binding.kind {
					BindingKind::Value(value) => {
						LLVMBuildStore(self.builder, value, alloca);
					}

					BindingKind::Pointer { pointer, .. } => {
						let layout = type_store.type_layout(variant_binding.type_id);
						let align = layout.alignment as u32;
						let size = LLVMConstInt(LLVMInt64TypeInContext(self.context), layout.size as u64, false as _);
						LLVMBuildMemCpy(self.builder, alloca, align, pointer, align, size);
					}
				}
			}
		}

		let kind = BindingKind::Pointer { pointer: alloca, pointed_type: union_type };
		Binding { type_id: union_type_id, kind }
	}

	fn generate_initialized_binding(
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

	fn generate_zero_initialized_binding(
		&mut self,
		type_store: &mut TypeStore,
		readable_index: usize,
		type_id: TypeId,
		name: &str,
		debug_location: DebugLocation,
	) {
		let _debug_scope = self.create_debug_scope(debug_location);
		assert_eq!(self.readables.len(), readable_index);

		let llvm_type = self.llvm_types.type_to_llvm_type(self.context, type_store, type_id);

		// This format hurts my soul
		let alloca = self.build_alloca(llvm_type, CString::new(format!("generate_binding.{}", name)).unwrap());
		unsafe { LLVMBuildStore(self.builder, LLVMConstNull(llvm_type), alloca) };

		let kind = BindingKind::Pointer { pointer: alloca, pointed_type: llvm_type };
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

	fn generate_yield<'a, 'b>(
		&mut self,
		context: &mut codegen::Context<'a, 'b>,
		yield_target_index: usize,
		value: Option<Self::Binding>,
		debug_location: DebugLocation,
		defer_callback: impl FnOnce(&mut codegen::Context<'a, 'b>, &mut Self),
	) {
		let _debug_scope = self.create_debug_scope(debug_location);

		let target = self.yield_targets[yield_target_index];
		if let Some(value) = value {
			let value = value.to_value(self.builder);
			let binding = target.binding.unwrap();
			unsafe { LLVMBuildStore(self.builder, value, binding.pointer) };
		}

		defer_callback(context, self);

		unsafe { LLVMBuildBr(self.builder, target.following_block) };
	}

	fn generate_return<'a, 'b>(
		&mut self,
		context: &mut codegen::Context<'a, 'b>,
		function_id: FunctionId,
		value: Option<Self::Binding>,
		debug_location: DebugLocation,
		defer_callback: impl FnOnce(&mut codegen::Context<'a, 'b>, &mut Self),
	) {
		let _debug_scope = self.create_debug_scope(debug_location);

		let maybe_function = &self.functions[function_id.function_shape_index][function_id.specialization_index];
		let function = maybe_function.as_ref().unwrap();

		ABI::return_value(context, self, function.llvm_function, function.return_type, value, defer_callback);
	}

	fn generate_slice(
		&mut self,
		slice_type_id: TypeId,
		pointer: Self::Binding,
		length: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding {
		let _debug_scope = self.create_debug_scope(debug_location);

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

	fn generate_non_null_invalid_pointer(&mut self, pointer_type_id: TypeId, debug_location: DebugLocation) -> Self::Binding {
		let _debug_scope = self.create_debug_scope(debug_location);

		unsafe {
			let one = LLVMConstInt(LLVMInt64TypeInContext(self.context), 1, false as _);
			let pointer_type = self.llvm_types.opaque_pointer;
			let pointer = LLVMBuildIntToPtr(self.builder, one, pointer_type, c"non_null_invalid_pointer.pointer".as_ptr());

			let kind = BindingKind::Value(pointer);
			Binding { type_id: pointer_type_id, kind }
		}
	}

	fn generate_non_null_invalid_slice(
		&mut self,
		slice_type_id: TypeId,
		length: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding {
		let _debug_scope = self.create_debug_scope(debug_location);

		unsafe {
			let one = LLVMConstInt(LLVMInt64TypeInContext(self.context), 1, false as _);
			let pointer_type = self.llvm_types.opaque_pointer;

			let pointer = LLVMBuildIntToPtr(self.builder, one, pointer_type, c"non_null_invalid_slice.pointer".as_ptr());
			let length = length.to_value(self.builder);

			let llvm_type = self.llvm_types.slice_struct;
			let alloca = self.build_alloca(llvm_type, c"generate_non_null_invalid_slice.slice_alloca");

			let pointer_name = c"generate_non_null_invalid_slice.pointer_pointer".as_ptr();
			let pointer_pointer = LLVMBuildStructGEP2(self.builder, llvm_type, alloca, 0, pointer_name);

			let length_name = c"generate_non_null_invalid_slice.length_pointer".as_ptr();
			let length_pointer = LLVMBuildStructGEP2(self.builder, llvm_type, alloca, 1, length_name);

			LLVMBuildStore(self.builder, pointer, pointer_pointer);
			LLVMBuildStore(self.builder, length, length_pointer);

			let kind = BindingKind::Pointer { pointer: alloca, pointed_type: llvm_type };
			Binding { type_id: slice_type_id, kind }
		}
	}

	fn generate_debugger_break(&mut self, debug_location: DebugLocation) {
		let _debug_scope = self.create_debug_scope(debug_location);

		if self.architecture == Architecture::Amd64 {
			unsafe {
				// This nop allows the cpu to execute the interupt, advance the program
				// counter, and still be within the same debug location when the debugger
				// comes in and inspects the execution state, rather than appearing to
				// be "on" the line after the `debugger_break` invocation
				let assembly = "int 3; nop";

				let void = LLVMVoidTypeInContext(self.context);
				let function_type = LLVMFunctionType(void, std::ptr::null_mut(), 0, false as _);

				let value = LLVMGetInlineAsm(
					function_type,
					assembly.as_ptr() as _,
					assembly.len(),
					"".as_ptr() as _,
					0,
					true as _,  // has side effects
					false as _, // is align stack, TODO: understand this?
					llvm_sys::LLVMInlineAsmDialect::LLVMInlineAsmDialectIntel,
					false as _, // can throw
				);

				LLVMBuildCall2(self.builder, function_type, value, std::ptr::null_mut(), 0, c"".as_ptr());
			}
		} else if self.architecture == Architecture::Aarch64 {
			unsafe {
				// LLDB on macOS AArch64 doesn't *seem* to need the nop as GDB on
				// AMD64 does, but let's err on the side of making sure
				let assembly = "brk 0; nop";

				let void = LLVMVoidTypeInContext(self.context);
				let function_type = LLVMFunctionType(void, std::ptr::null_mut(), 0, false as _);

				let value = LLVMGetInlineAsm(
					function_type,
					assembly.as_ptr() as _,
					assembly.len(),
					"".as_ptr() as _,
					0,
					true as _,  // has side effects
					false as _, // is align stack, TODO: understand this?
					llvm_sys::LLVMInlineAsmDialect::LLVMInlineAsmDialectATT,
					false as _, // can throw
				);

				LLVMBuildCall2(self.builder, function_type, value, std::ptr::null_mut(), 0, c"".as_ptr());
			}
		}
	}

	fn generate_min_i8(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding {
		return self.generate_min_max_intrinsic(a, b, self.llvm_intrinsics.min_i8, type_store.i8_type_id(), debug_location);
	}

	fn generate_min_i16(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding {
		return self.generate_min_max_intrinsic(a, b, self.llvm_intrinsics.min_i16, type_store.i16_type_id(), debug_location);
	}

	fn generate_min_i32(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding {
		return self.generate_min_max_intrinsic(a, b, self.llvm_intrinsics.min_i32, type_store.i32_type_id(), debug_location);
	}

	fn generate_min_i64(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding {
		return self.generate_min_max_intrinsic(a, b, self.llvm_intrinsics.min_i64, type_store.i64_type_id(), debug_location);
	}

	fn generate_min_u8(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding {
		return self.generate_min_max_intrinsic(a, b, self.llvm_intrinsics.min_u8, type_store.u8_type_id(), debug_location);
	}

	fn generate_min_u16(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding {
		return self.generate_min_max_intrinsic(a, b, self.llvm_intrinsics.min_u16, type_store.u16_type_id(), debug_location);
	}

	fn generate_min_u32(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding {
		return self.generate_min_max_intrinsic(a, b, self.llvm_intrinsics.min_u32, type_store.u32_type_id(), debug_location);
	}

	fn generate_min_u64(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding {
		return self.generate_min_max_intrinsic(a, b, self.llvm_intrinsics.min_u64, type_store.u64_type_id(), debug_location);
	}

	fn generate_min_isize(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding {
		return self.generate_min_max_intrinsic(a, b, self.llvm_intrinsics.min_isize, type_store.isize_type_id(), debug_location);
	}

	fn generate_min_usize(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding {
		return self.generate_min_max_intrinsic(a, b, self.llvm_intrinsics.min_usize, type_store.usize_type_id(), debug_location);
	}

	fn generate_min_f32(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding {
		return self.generate_min_max_intrinsic(a, b, self.llvm_intrinsics.min_f32, type_store.f32_type_id(), debug_location);
	}

	fn generate_min_f64(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding {
		return self.generate_min_max_intrinsic(a, b, self.llvm_intrinsics.min_f64, type_store.f64_type_id(), debug_location);
	}

	fn generate_max_i8(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding {
		return self.generate_min_max_intrinsic(a, b, self.llvm_intrinsics.max_i8, type_store.i8_type_id(), debug_location);
	}

	fn generate_max_i16(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding {
		return self.generate_min_max_intrinsic(a, b, self.llvm_intrinsics.max_i16, type_store.i16_type_id(), debug_location);
	}

	fn generate_max_i32(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding {
		return self.generate_min_max_intrinsic(a, b, self.llvm_intrinsics.max_i32, type_store.i32_type_id(), debug_location);
	}

	fn generate_max_i64(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding {
		return self.generate_min_max_intrinsic(a, b, self.llvm_intrinsics.max_i64, type_store.i64_type_id(), debug_location);
	}

	fn generate_max_u8(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding {
		return self.generate_min_max_intrinsic(a, b, self.llvm_intrinsics.max_u8, type_store.u8_type_id(), debug_location);
	}

	fn generate_max_u16(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding {
		return self.generate_min_max_intrinsic(a, b, self.llvm_intrinsics.max_u16, type_store.u16_type_id(), debug_location);
	}

	fn generate_max_u32(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding {
		return self.generate_min_max_intrinsic(a, b, self.llvm_intrinsics.max_u32, type_store.u32_type_id(), debug_location);
	}

	fn generate_max_u64(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding {
		return self.generate_min_max_intrinsic(a, b, self.llvm_intrinsics.max_u64, type_store.u64_type_id(), debug_location);
	}

	fn generate_max_isize(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding {
		return self.generate_min_max_intrinsic(a, b, self.llvm_intrinsics.max_isize, type_store.isize_type_id(), debug_location);
	}

	fn generate_max_usize(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding {
		return self.generate_min_max_intrinsic(a, b, self.llvm_intrinsics.max_usize, type_store.usize_type_id(), debug_location);
	}

	fn generate_max_f32(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding {
		return self.generate_min_max_intrinsic(a, b, self.llvm_intrinsics.max_f32, type_store.f32_type_id(), debug_location);
	}

	fn generate_max_f64(
		&mut self,
		type_store: &TypeStore,
		a: Self::Binding,
		b: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding {
		return self.generate_min_max_intrinsic(a, b, self.llvm_intrinsics.max_f64, type_store.f64_type_id(), debug_location);
	}

	fn generate_round_f32(
		&mut self,
		type_store: &TypeStore,
		input: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding {
		let _debug_scope = self.create_debug_scope(debug_location);

		let intrinsic = self.llvm_intrinsics.round_f32;
		let mut arguments = [input.to_value(self.builder)];
		unsafe {
			let value = LLVMBuildCall2(
				self.builder,
				intrinsic.fn_type,
				intrinsic.llvm_function,
				arguments.as_mut_ptr(),
				arguments.len() as u32,
				c"".as_ptr(),
			);
			let kind = BindingKind::Value(value);
			Binding { type_id: type_store.f32_type_id(), kind }
		}
	}

	fn generate_round_f64(
		&mut self,
		type_store: &TypeStore,
		input: Self::Binding,
		debug_location: DebugLocation,
	) -> Self::Binding {
		let _debug_scope = self.create_debug_scope(debug_location);

		let intrinsic = self.llvm_intrinsics.round_f64;
		let mut arguments = [input.to_value(self.builder)];
		unsafe {
			let value = LLVMBuildCall2(
				self.builder,
				intrinsic.fn_type,
				intrinsic.llvm_function,
				arguments.as_mut_ptr(),
				arguments.len() as u32,
				c"".as_ptr(),
			);
			let kind = BindingKind::Value(value);
			Binding { type_id: type_store.f64_type_id(), kind }
		}
	}

	fn finalize_generator(&mut self) {
		self.finalize_function_if_in_function();
	}
}
