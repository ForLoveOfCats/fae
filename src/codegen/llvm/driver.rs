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
use llvm_sys::target::{LLVMCreateTargetData, LLVMSetModuleDataLayout};
use llvm_sys::target_machine::{
	LLVMCodeGenFileType, LLVMCodeGenOptLevel, LLVMCodeModel, LLVMCreateTargetDataLayout, LLVMCreateTargetMachine,
	LLVMGetTargetFromTriple, LLVMRelocMode, LLVMTargetMachineEmitToFile,
};
use llvm_sys::transforms::pass_builder::{LLVMCreatePassBuilderOptions, LLVMRunPasses};

use crate::cli::CliArguments;
use crate::codegen::codegen::generate;
use crate::codegen::llvm::generator::{Architecture, LLVMGenerator};
use crate::frontend::error::{Messages, WriteFmt};
use crate::frontend::function_store::FunctionStore;
use crate::frontend::lang_items::LangItems;
use crate::frontend::project::ProjectConfig;
use crate::frontend::symbols::Statics;
use crate::frontend::tree;
use crate::frontend::type_store::TypeStore;
use crate::TARGET_DIR;

pub fn generate_code<'a>(
	cli_arguments: &CliArguments,
	message_output: &mut impl WriteFmt,
	project_config: &ProjectConfig,
	project_path: &Path,
	name: &str,
	parsed_files: &[tree::File],
	messages: &mut Messages<'a>,
	lang_items: &LangItems,
	type_store: &mut TypeStore<'a>,
	function_store: &FunctionStore<'a>,
	statics: &Statics,
) -> PathBuf {
	#[cfg(not(target_os = "windows"))]
	let object_name = format!("{name}.o");
	#[cfg(target_os = "windows")]
	let object_name = format!("{name}.obj");

	#[cfg(any(target_os = "linux", target_os = "windows"))]
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

	#[cfg(target_os = "linux")]
	let triple = c"x86_64-pc-linux-gnu";
	#[cfg(target_os = "macos")]
	let triple = c"arm64-apple-darwin20.1.0";
	#[cfg(target_os = "windows")]
	let triple = c"x86_64-pc-windows-msvc";

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

	let context = unsafe { LLVMContextCreate() };

	#[cfg(any(target_os = "linux", target_os = "macos"))]
	let mut generator = LLVMGenerator::<crate::codegen::amd64::sysv_classifier::SysvClassifer>::new(
		context,
		architecture,
		cli_arguments.optimize_artifacts,
	);

	#[cfg(target_os = "windows")]
	let mut generator = LLVMGenerator::<crate::codegen::amd64::windows_classifier::WindowsClassifier>::new(
		context,
		architecture,
		cli_arguments.optimize_artifacts,
	);

	#[cfg(target_os = "macos")]
	unsafe {
		let behavior = llvm_sys::LLVMModuleFlagBehavior::LLVMModuleFlagBehaviorWarning;
		let key = "Dwarf Version";
		let ty = llvm_sys::core::LLVMInt64TypeInContext(generator.context);
		let int = LLVMConstInt(ty, 2, false as _);
		let value = LLVMValueAsMetadata(int);
		LLVMAddModuleFlag(generator.module, behavior, key.as_ptr() as _, key.len(), value);
	};

	unsafe {
		let data_layout = if cfg!(target_os = "linux") {
			let s = c"e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128-a:0:0";
			LLVMCreateTargetData(s.as_ptr() as _)
		} else if cfg!(target_os = "macos") {
			LLVMCreateTargetData(c"e-m:o-i64:64-i128:128-n32:64-S128-Fn32-a:0:0".as_ptr() as _)
		} else {
			LLVMCreateTargetDataLayout(machine)
		};

		assert!(!data_layout.is_null());
		LLVMSetModuleDataLayout(generator.module, data_layout);
	}

	generate(
		parsed_files,
		messages,
		lang_items,
		type_store,
		function_store,
		statics,
		&mut generator,
		cli_arguments.optimize_artifacts,
		cli_arguments.command,
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

	if cli_arguments.optimize_artifacts {
		unsafe {
			let options = LLVMCreatePassBuilderOptions();
			LLVMRunPasses(generator.module, c"default<O2>".as_ptr(), machine, options);
		}
	}

	_ = std::fs::create_dir(TARGET_DIR);

	if cli_arguments.emit_llvm_ir {
		unsafe {
			let formatted_path = format!("{TARGET_DIR}/{name}.ll");
			let path = CString::new(formatted_path.clone()).unwrap();
			let mut error_string = MaybeUninit::uninit();
			if LLVMPrintModuleToFile(generator.module, path.as_ptr(), error_string.as_mut_ptr()) == 1 {
				let error = CStr::from_ptr(error_string.assume_init());
				panic!("{error:?}");
			}

			message_output.alertln("     Emitted LLVM IR", format_args!("{formatted_path}"));
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

	let object_path = format!("{TARGET_DIR}/{object_name}");
	unsafe {
		let object_path = CString::new(object_path.as_bytes()).unwrap();
		let mut error_string = MaybeUninit::uninit();
		LLVMTargetMachineEmitToFile(
			machine,
			generator.module,
			object_path.as_ptr(),
			LLVMCodeGenFileType::LLVMObjectFile,
			error_string.as_mut_ptr(),
		);
	}

	#[cfg(target_os = "linux")]
	{
		let linker = project_config.linux_linker.as_deref().unwrap_or("ld");
		let additional_flags = project_config.linux_additional_linker_flags.clone().unwrap_or(Vec::new());
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

		let executable_path = PathBuf::from(format!("{TARGET_DIR}")).join(name);

		let mut command = Command::new(linker)
			.arg("-export-dynamic")
			.arg("--hash-style=gnu")
			.arg("--build-id")
			.arg("--eh-frame-hdr")
			.arg("-m")
			.arg("elf_x86_64")
			.arg("/usr/lib/crt1.o")
			.arg("/usr/lib/crti.o")
			.args(additional_objects)
			.arg(object_path)
			.arg("/usr/lib/crtn.o")
			.arg("/usr/lib/libc.so")
			.arg("/usr/lib/libm.so")
			.arg("-dynamic-linker")
			.arg("/lib64/ld-linux-x86-64.so.2")
			.args(additional_flags)
			.arg("-o")
			.arg(&executable_path)
			.spawn()
			.unwrap();

		let status = command.wait().unwrap();
		assert!(status.success());

		return executable_path;
	}

	#[cfg(target_os = "macos")]
	{
		let linker = project_config.darwin_linker.as_deref().unwrap_or("ld");
		let additional_flags = project_config.darwin_additional_linker_flags.clone().unwrap_or(Vec::new());
		let additional_objects = if let Some(objects) = &project_config.darwin_additional_linker_objects {
			let mut additional_objects = Vec::with_capacity(objects.len());
			for object in objects {
				let path = project_path.join(object);
				additional_objects.push(path);
			}
			additional_objects
		} else {
			Vec::new()
		};

		let executable_path = PathBuf::from(format!("{TARGET_DIR}")).join(name);

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
			.args(additional_flags)
			.arg("-o")
			.arg(&executable_path)
			.args(additional_objects)
			.spawn()
			.unwrap();

		let status = command.wait().unwrap();
		assert!(status.success());

		return executable_path;
	}

	#[cfg(target_os = "windows")]
	{
		let default_linker = "C:\\Program Files\\Microsoft Visual Studio\\18\\Community\\VC\\Tools\\MSVC\\14.50.35717\\bin\\Hostx64\\x64\\link.exe";
		let linker = project_config.windows_linker.as_deref().unwrap_or(default_linker);
		let additional_flags = project_config.windows_additional_linker_flags.clone().unwrap_or(Vec::new());
		let additional_objects = if let Some(objects) = &project_config.windows_additional_linker_objects {
			let mut additional_objects = Vec::with_capacity(objects.len());
			for object in objects {
				let path = project_path.join(object);
				additional_objects.push(path);
			}
			additional_objects
		} else {
			Vec::new()
		};

		let executable_path = PathBuf::from(format!("{TARGET_DIR}")).join(name);
		let out_path = unsafe { str::from_utf8_unchecked(executable_path.as_os_str().as_encoded_bytes()) };

		let mut command = Command::new(linker)
			.arg("/nologo")
			.arg("/subsystem:windows")
			.arg("kernel32.lib")
			.arg("user32.lib")
			.arg("msvcrt.lib")
			.arg("libucrt.lib")
			.arg("libvcruntime.lib")
			// .arg("-export-dynamic")
			// .arg("--hash-style=gnu")
			// .arg("--build-id")
			// .arg("--eh-frame-hdr")
			// .arg("-m")
			// .arg("elf_x86_64")
			// .arg("/usr/lib/crt1.o")
			// .arg("/usr/lib/crti.o")
			.args(additional_objects)
			.arg(object_path)
			// .arg("/usr/lib/crtn.o")
			// .arg("/usr/lib/libc.so")
			// .arg("/usr/lib/libm.so")
			// .arg("-dynamic-linker")
			// .arg("/lib64/ld-linux-x86-64.so.2")
			.args(additional_flags)
			.arg(format!("/out:{out_path}"))
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
