use std::path::{Path, PathBuf};
use std::process::Command;

use inkwell::context::Context;
use inkwell::targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple};
use inkwell::OptimizationLevel;

use crate::codegen::codegen::generate;
use crate::codegen::llvm::abi::SysvAbi;
use crate::codegen::llvm::generator::LLVMGenerator;
use crate::error::Messages;
use crate::function_store::FunctionStore;
use crate::type_store::TypeStore;

pub fn generate_code<'a>(
	messages: &mut Messages<'a>,
	type_store: &mut TypeStore<'a>,
	function_store: &mut FunctionStore<'a>,
) -> PathBuf {
	Target::initialize_x86(&InitializationConfig::default());

	let context = Context::create();
	let mut generator = LLVMGenerator::<SysvAbi>::new(&context);

	generate(messages, type_store, function_store, &mut generator);

	let triple = TargetTriple::create("x86_64-pc-linux-gnu");
	let target = Target::from_triple(&triple).unwrap();
	let machine = target
		.create_target_machine(&triple, "", "", OptimizationLevel::None, RelocMode::Default, CodeModel::Default)
		.unwrap();

	generator.module.print_to_file(Path::new("./fae.ll")).unwrap();
	if let Err(error) = generator.module.verify() {
		eprintln!("{}", error.to_str().unwrap());
		std::process::exit(-1);
	}

	let object_path = Path::new("./fae_object.o");
	machine
		.write_to_file(&generator.module, FileType::Object, object_path)
		.unwrap();

	let path = PathBuf::from("./fae_executable.x64");
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
