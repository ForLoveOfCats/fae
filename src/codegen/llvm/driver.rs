use std::ffi::{CStr, CString};
use std::io::{stderr, Write};
use std::mem::MaybeUninit;
use std::path::{Path, PathBuf};
use std::process::Command;

use llvm_sys::analysis::{LLVMVerifierFailureAction, LLVMVerifyModule};
use llvm_sys::core::{LLVMContextCreate, LLVMPrintModuleToFile};
use llvm_sys::target_machine::{
	LLVMCodeGenFileType, LLVMCodeGenOptLevel, LLVMCodeModel, LLVMCreateTargetMachine, LLVMGetTargetFromTriple, LLVMRelocMode,
	LLVMTargetMachineEmitToFile,
};
use llvm_sys::transforms::pass_builder::{LLVMCreatePassBuilderOptions, LLVMRunPasses};

use crate::cli::CliArguments;
use crate::codegen::codegen::generate;
use crate::codegen::llvm::abi::SysvAbi;
use crate::codegen::llvm::generator::LLVMGenerator;
use crate::frontend::error::Messages;
use crate::frontend::function_store::FunctionStore;
use crate::frontend::lang_items::LangItems;
use crate::frontend::symbols::Statics;
use crate::frontend::type_store::TypeStore;

pub fn generate_code<'a>(
	cli_arguments: &CliArguments,
	messages: &mut Messages<'a>,
	lang_items: &LangItems,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	statics: &Statics,
) -> PathBuf {
	#[cfg(target_os = "linux")]
	unsafe {
		llvm_sys::target::LLVMInitializeX86Target();
		llvm_sys::target::LLVMInitializeX86TargetInfo();
		llvm_sys::target::LLVMInitializeX86AsmPrinter();
		llvm_sys::target::LLVMInitializeX86AsmParser();
		llvm_sys::target::LLVMInitializeX86Disassembler();
		llvm_sys::target::LLVMInitializeX86TargetMC();
	}

	#[cfg(target_os = "macos")]
	unsafe {
		llvm_sys::target::LLVMInitializeAArch64Target();
		llvm_sys::target::LLVMInitializeAArch64TargetInfo();
		llvm_sys::target::LLVMInitializeAArch64AsmPrinter();
		llvm_sys::target::LLVMInitializeAArch64AsmParser();
		llvm_sys::target::LLVMInitializeAArch64Disassembler();
		llvm_sys::target::LLVMInitializeAArch64TargetMC();
	}

	let context = unsafe { LLVMContextCreate() };
	let mut generator = LLVMGenerator::<SysvAbi>::new(context);

	generate(messages, lang_items, type_store, function_store, statics, &mut generator);

	#[cfg(target_os = "linux")]
	let triple = c"x86_64-pc-linux-gnu";
	#[cfg(target_os = "macos")]
	let triple = c"aarch64-apple-darwin";

	let target = unsafe {
		let mut target = std::ptr::null_mut();

		let mut error_string = MaybeUninit::uninit();
		if LLVMGetTargetFromTriple(triple.as_ptr(), &mut target, error_string.as_mut_ptr()) == 1 {
			let error = CStr::from_ptr(error_string.assume_init());
			panic!("{error:?}");
		}

		target
	};

	let codegen_opt_level = match cli_arguments.optimize_artifacts {
		false => LLVMCodeGenOptLevel::LLVMCodeGenLevelNone,
		true => LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
	};

	let machine = unsafe {
		LLVMCreateTargetMachine(
			target,
			triple.as_ptr(),
			c"".as_ptr(),
			c"".as_ptr(),
			codegen_opt_level,
			LLVMRelocMode::LLVMRelocDefault,
			LLVMCodeModel::LLVMCodeModelDefault,
		)
	};
	assert!(!machine.is_null());

	if cli_arguments.optimize_artifacts {
		unsafe {
			let options = LLVMCreatePassBuilderOptions();
			LLVMRunPasses(generator.module, c"default<O2>".as_ptr(), machine, options);
		}
	}

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
			stderr().lock().write_all(error.to_bytes()).unwrap();
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
			object_path.into_raw(), // Why does this need mutable access???
			LLVMCodeGenFileType::LLVMObjectFile,
			error_string.as_mut_ptr(),
		);
	}

	#[cfg(target_os = "linux")]
	{
		let path = PathBuf::from("./fae_target/fae_executable.x64");
		let mut lld = Command::new("ld.lld")
			.arg(object_path)
			.arg("/usr/lib/crt1.o")
			.arg("/usr/lib/crti.o")
			.arg("/usr/lib/crtn.o")
			.arg("/usr/lib/libc.so")
			.arg("/usr/lib/libm.so")
			.arg("-dynamic-linker")
			.arg("/lib64/ld-linux-x86-64.so.2")
			.arg("-o")
			.arg(&path)
			.spawn()
			.unwrap();

		let status = lld.wait().unwrap();
		assert!(status.success());

		return path;
	}

	#[cfg(target_os = "macos")]
	{
		let path = PathBuf::from("./fae_target/fae_executable.aarch64");
		let mut lld = Command::new("ld64.lld")
			.arg(object_path)
			.arg("-dynamic")
			.args(["-platform_version", "macos", "14.5.0", "14.5.0"])
			.args(["-arch", "arm64"])
			.arg("/Library/Developer/CommandLineTools/SDKs/MacOSX13.sdk/usr/lib/libSystem.tbd")
			.arg("-o")
			.arg(&path)
			.spawn()
			.unwrap();

		let status = lld.wait().unwrap();
		assert!(status.success());

		return path;
	}
}
