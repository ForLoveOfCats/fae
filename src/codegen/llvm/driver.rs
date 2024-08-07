use std::ffi::{CStr, CString};
use std::io::{stderr, Write};
use std::mem::MaybeUninit;
use std::path::{Path, PathBuf};
use std::process::Command;

use llvm_sys::analysis::{LLVMVerifierFailureAction, LLVMVerifyModule};
use llvm_sys::core::{
	LLVMAddModuleFlag, LLVMConstInt, LLVMContextCreate, LLVMInt32TypeInContext, LLVMPrintModuleToFile, LLVMValueAsMetadata,
};
use llvm_sys::debuginfo::{LLVMDIBuilderFinalize, LLVMDebugMetadataVersion};
use llvm_sys::target_machine::{
	LLVMCodeGenFileType, LLVMCodeGenOptLevel, LLVMCodeModel, LLVMCreateTargetMachine, LLVMGetTargetFromTriple, LLVMRelocMode,
	LLVMTargetMachineEmitToFile,
};
use llvm_sys::transforms::pass_builder::{LLVMCreatePassBuilderOptions, LLVMRunPasses};

use crate::cli::CliArguments;
use crate::codegen::codegen::generate;
use crate::codegen::llvm::abi::SysvAbi;
use crate::codegen::llvm::generator::{Architecture, LLVMGenerator};
use crate::frontend::error::Messages;
use crate::frontend::function_store::FunctionStore;
use crate::frontend::lang_items::LangItems;
use crate::frontend::project::ProjectConfig;
use crate::frontend::symbols::Statics;
use crate::frontend::tree;
use crate::frontend::type_store::TypeStore;

pub fn generate_code<'a>(
	cli_arguments: &CliArguments,
	project_config: &ProjectConfig,
	project_path: &Path,
	parsed_files: &[tree::File],
	messages: &mut Messages<'a>,
	lang_items: &LangItems,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	statics: &Statics,
) -> PathBuf {
	#[cfg(target_os = "linux")]
	let architecture = unsafe {
		llvm_sys::target::LLVMInitializeX86Target();
		llvm_sys::target::LLVMInitializeX86TargetInfo();
		llvm_sys::target::LLVMInitializeX86AsmPrinter();
		llvm_sys::target::LLVMInitializeX86AsmParser();
		llvm_sys::target::LLVMInitializeX86Disassembler();
		llvm_sys::target::LLVMInitializeX86TargetMC();

		// TODO: Allow multiple architectures per OS
		Architecture::Amd64
	};

	#[cfg(target_os = "macos")]
	let architecture = unsafe {
		llvm_sys::target::LLVMInitializeAArch64Target();
		llvm_sys::target::LLVMInitializeAArch64TargetInfo();
		llvm_sys::target::LLVMInitializeAArch64AsmPrinter();
		llvm_sys::target::LLVMInitializeAArch64AsmParser();
		llvm_sys::target::LLVMInitializeAArch64Disassembler();
		llvm_sys::target::LLVMInitializeAArch64TargetMC();

		// TODO: Allow multiple architectures per OS
		Architecture::Aarch64
	};

	let context = unsafe { LLVMContextCreate() };
	let mut generator = LLVMGenerator::<SysvAbi>::new(context, architecture, cli_arguments.optimize_artifacts);

	generate(
		parsed_files,
		messages,
		lang_items,
		type_store,
		function_store,
		statics,
		&mut generator,
		cli_arguments.optimize_artifacts,
	);

	unsafe {
		let behavior = llvm_sys::LLVMModuleFlagBehavior::LLVMModuleFlagBehaviorWarning;
		let key = "Debug Info Version";
		let ty = LLVMInt32TypeInContext(generator.context);
		let int = LLVMConstInt(ty, LLVMDebugMetadataVersion() as _, false as _);
		let value = LLVMValueAsMetadata(int);
		LLVMAddModuleFlag(generator.module, behavior, key.as_ptr() as _, key.len(), value);

		LLVMDIBuilderFinalize(generator.di_builder);
	}

	#[cfg(target_os = "linux")]
	let triple = c"x86_64-pc-linux-gnu";
	#[cfg(target_os = "macos")]
	let triple = {
		unsafe {
			let behavior = llvm_sys::LLVMModuleFlagBehavior::LLVMModuleFlagBehaviorWarning;
			let key = "Dwarf Version";
			let ty = llvm_sys::core::LLVMInt64TypeInContext(generator.context);
			let int = LLVMConstInt(ty, 2, false as _);
			let value = LLVMValueAsMetadata(int);
			LLVMAddModuleFlag(generator.module, behavior, key.as_ptr() as _, key.len(), value);
		}

		c"arm64-apple-darwin20.1.0"
	};

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

	if cli_arguments.verify_llvm_module {
		unsafe {
			let mut error_string = MaybeUninit::uninit();
			let action = LLVMVerifierFailureAction::LLVMReturnStatusAction;
			if LLVMVerifyModule(generator.module, action, error_string.as_mut_ptr()) == 1 {
				let error = CStr::from_ptr(error_string.assume_init());
				stderr().lock().write_all(error.to_bytes()).unwrap();
				std::process::exit(-1);
			}
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
		let linker = project_config.linux_linker.as_deref().unwrap_or("ld");
		let additional_objects = if let Some(objects) = &project_config.linux_additional_linker_objects {
			let mut additional_objects = Vec::with_capacity(objects.len());
			for object in objects {
				let path = project_path.join(object);
				additional_objects.push(path);
			}
			additional_objects
		} else {
			Vec::new()
		};

		let executable_path = PathBuf::from("./fae_target/fae_executable.x64");

		let mut command = Command::new(linker)
			.arg("-export-dynamic")
			.arg("--hash-style=gnu")
			.arg("--build-id")
			.arg("--eh-frame-hdr")
			.arg("-m")
			.arg("elf_x86_64")
			.arg("/usr/lib/crt1.o")
			.arg("/usr/lib/crti.o")
			.arg(object_path)
			.arg("/usr/lib/libc.so")
			.arg("/usr/lib/libm.so")
			.arg("/usr/lib/crtn.o")
			.arg("-dynamic-linker")
			.arg("/lib64/ld-linux-x86-64.so.2")
			.arg("-o")
			.arg(&executable_path)
			.args(additional_objects)
			.spawn()
			.unwrap();

		let status = command.wait().unwrap();
		assert!(status.success());

		return executable_path;
	}

	#[cfg(target_os = "macos")]
	{
		let linker = project_config.darwin_linker.as_deref().unwrap_or("ld");
		let additional_objects = if let Some(objects) = &project_config.linux_additional_linker_objects {
			let mut additional_objects = Vec::with_capacity(objects.len());
			for object in objects {
				let path = project_path.join(object);
				additional_objects.push(path);
			}
			additional_objects
		} else {
			Vec::new()
		};

		let executable_path = PathBuf::from("./fae_target/fae_executable.aarch64");

		let sdk_version = call_xcrun("--show-sdk-version");

		let mut sdk_path = PathBuf::from(call_xcrun("--show-sdk-path"));
		sdk_path.push("usr/lib/libSystem.tbd");

		let mut command = Command::new(linker)
			.arg(object_path)
			.arg("-dynamic")
			// TODO: Allow targetting versions of macOS older than installed version
			.args(["-platform_version", "macos", &sdk_version, &sdk_version])
			.args(["-arch", "arm64"])
			.arg(sdk_path)
			.arg("-o")
			.arg(&executable_path)
			.args(additional_objects)
			.spawn()
			.unwrap();

		let status = command.wait().unwrap();
		assert!(status.success());

		return executable_path;
	}
}

#[cfg(target_os = "macos")]
fn call_xcrun(argument: &str) -> String {
	let output = match Command::new("xcrun").arg(argument).output() {
		Ok(output) => output,

		Err(err) => {
			eprintln!("Failed to run xcrun: {err:?}");
			std::process::exit(-1);
		}
	};

	String::from_utf8(output.stdout).unwrap().trim().to_string()
}
