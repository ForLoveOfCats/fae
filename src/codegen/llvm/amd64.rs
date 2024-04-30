use std::ffi::{CStr, CString};
use std::mem::MaybeUninit;
use std::path::{Path, PathBuf};
use std::process::Command;

use llvm_sys::analysis::{LLVMVerifierFailureAction, LLVMVerifyModule};
use llvm_sys::core::{LLVMContextCreate, LLVMPrintModuleToFile};
use llvm_sys::target::{
	LLVMInitializeX86AsmParser, LLVMInitializeX86AsmPrinter, LLVMInitializeX86Disassembler, LLVMInitializeX86Target,
	LLVMInitializeX86TargetInfo, LLVMInitializeX86TargetMC,
};
use llvm_sys::target_machine::{
	LLVMCodeGenFileType, LLVMCodeGenOptLevel, LLVMCodeModel, LLVMCreateTargetMachine, LLVMGetTargetFromTriple, LLVMRelocMode,
	LLVMTargetMachineEmitToFile,
};

use crate::codegen::codegen::generate;
use crate::codegen::llvm::abi::SysvAbi;
use crate::codegen::llvm::generator::LLVMGenerator;
use crate::frontend::error::Messages;
use crate::frontend::function_store::FunctionStore;
use crate::frontend::lang_items::LangItems;
use crate::frontend::symbols::Statics;
use crate::frontend::type_store::TypeStore;

pub fn generate_code<'a>(
	messages: &mut Messages<'a>,
	lang_items: &LangItems,
	type_store: &mut TypeStore<'a>,
	function_store: &mut FunctionStore<'a>,
	statics: &Statics,
) -> PathBuf {
	unsafe {
		LLVMInitializeX86Target();
		LLVMInitializeX86TargetInfo();
		LLVMInitializeX86AsmPrinter();
		LLVMInitializeX86AsmParser();
		LLVMInitializeX86Disassembler();
		LLVMInitializeX86TargetMC();
	}

	let context = unsafe { LLVMContextCreate() };
	let mut generator = LLVMGenerator::<SysvAbi>::new(context);

	generate(messages, lang_items, type_store, function_store, statics, &mut generator);

	let triple = c"x86_64-pc-linux-gnu";
	let target = unsafe {
		let mut target = std::ptr::null_mut();

		let mut error_string = MaybeUninit::uninit();
		if LLVMGetTargetFromTriple(triple.as_ptr(), &mut target, error_string.as_mut_ptr()) == 1 {
			let error = CStr::from_ptr(error_string.assume_init());
			panic!("{error:?}");
		}

		target
	};

	let machine = unsafe {
		LLVMCreateTargetMachine(
			target,
			triple.as_ptr(),
			c"".as_ptr(),
			c"".as_ptr(),
			LLVMCodeGenOptLevel::LLVMCodeGenLevelNone,
			LLVMRelocMode::LLVMRelocDefault,
			LLVMCodeModel::LLVMCodeModelDefault,
		)
	};
	assert!(!machine.is_null());

	_ = std::fs::create_dir("./fae_target");
	unsafe {
		let path = c"./fae_target/fae.ll".as_ptr();
		let mut error_string = MaybeUninit::uninit();
		if LLVMPrintModuleToFile(generator.module, path, error_string.as_mut_ptr()) == 1 {
			let error = CStr::from_ptr(error_string.assume_init());
			panic!("{error:?}");
		}
	}

	unsafe {
		let mut error_string = MaybeUninit::uninit();
		let action = LLVMVerifierFailureAction::LLVMReturnStatusAction;
		if LLVMVerifyModule(generator.module, action, error_string.as_mut_ptr()) == 1 {
			let error = CStr::from_ptr(error_string.assume_init());
			eprintln!("{error:?}");
			std::process::exit(-1);
		}
	}

	let object_path = Path::new("./fae_target/fae_object.o");
	unsafe {
		let object_path = CString::from(c"./fae_target/fae_object.o");
		let mut error_string = MaybeUninit::uninit();
		LLVMTargetMachineEmitToFile(
			machine,
			generator.module,
			object_path.into_raw(), // Why
			LLVMCodeGenFileType::LLVMObjectFile,
			error_string.as_mut_ptr(),
		);
	}

	let path = PathBuf::from("./fae_target/fae_executable.x64");
	let mut lld = Command::new("ld.lld")
		.arg(object_path)
		.arg("/usr/lib/crt1.o")
		.arg("/usr/lib/crti.o")
		.arg("/usr/lib/crtn.o")
		.arg("/usr/lib/libc.so")
		.arg("-dynamic-linker")
		.arg("/lib64/ld-linux-x86-64.so.2")
		.arg("-o")
		.arg(&path)
		.spawn()
		.unwrap();

	let status = lld.wait().unwrap();
	assert!(status.success());

	path
}
