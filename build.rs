#![allow(unreachable_code)]

fn main() {
	#[cfg(target_os = "linux")]
	{
		linux_link();
		return;
	}

	#[cfg(target_os = "macos")]
	{
		link_macos();
		return;
	}

	#[cfg(target_os = "windows")]
	{
		windows_link();
		return;
	}

	panic!("Unsupported platform")
}

#[cfg(target_os = "linux")]
fn linux_link() {
	let libcxx_libs = std::env::current_dir()
		.unwrap()
		.join("./llvm/libcxx/build/lib")
		.canonicalize()
		.unwrap();

	println!("cargo::rerun-if-changed={}", libcxx_libs.display());
	println!("cargo::rustc-link-search=native={}", libcxx_libs.display());

	let llvm_libs = std::env::current_dir()
		.unwrap()
		.join("./llvm/llvm/build/lib")
		.canonicalize()
		.unwrap();

	println!("cargo::rerun-if-changed={}", llvm_libs.display());
	println!("cargo::rustc-link-search=native={}", llvm_libs.display());

	for name in LIB_NAMES_LINUX.iter().chain(LIB_NAMES) {
		println!("cargo::rustc-link-lib=static={}", name);
	}

	println!("cargo::rustc-link-lib=static=c++");
	println!("cargo::rustc-link-lib=static=c++abi");
}

#[cfg(target_os = "macos")]
fn link_macos() {
	let libcxx_libs = std::env::current_dir()
		.unwrap()
		.join("./llvm/libcxx/build/lib")
		.canonicalize()
		.unwrap();

	println!("cargo::rerun-if-changed={}", libcxx_libs.display());
	println!("cargo::rustc-link-search=native={}", libcxx_libs.display());

	let llvm_libs = std::env::current_dir()
		.unwrap()
		.join("./llvm/llvm/build/lib")
		.canonicalize()
		.unwrap();

	println!("cargo::rerun-if-changed={}", llvm_libs.display());
	println!("cargo::rustc-link-search=native={}", llvm_libs.display());

	for name in LIB_NAMES_MACOS.iter().chain(LIB_NAMES) {
		println!("cargo::rustc-link-lib=static={}", name);
	}

	println!("cargo::rustc-link-lib=static:+whole-archive=c++");
	println!("cargo::rustc-link-lib=static:+whole-archive=c++abi");
}

#[cfg(target_os = "windows")]
fn windows_link() {
	let llvm_libs = std::env::current_dir()
		.unwrap()
		.join("./llvm/llvm/build/lib")
		.canonicalize()
		.unwrap();

	println!("cargo::rerun-if-changed={}", llvm_libs.display());
	println!("cargo::rustc-link-search=native={}", llvm_libs.display());

	for name in LIB_NAMES_WINDOWS.iter().chain(LIB_NAMES) {
		println!("cargo::rustc-link-lib=static={}", name);
	}
}

#[cfg(target_os = "linux")]
const LIB_NAMES_LINUX: &[&str] = &[
	"LLVMX86TargetMCA",
	"LLVMX86Disassembler",
	"LLVMX86AsmParser",
	"LLVMX86CodeGen",
	"LLVMX86Desc",
	"LLVMX86Info",
];

#[cfg(target_os = "macos")]
const LIB_NAMES_MACOS: &[&str] = &[
	"LLVMAArch64AsmParser",
	"LLVMAArch64Utils",
	"LLVMAArch64Info",
	"LLVMAArch64Desc",
	"LLVMAArch64Disassembler",
	"LLVMAArch64CodeGen",
];

#[cfg(target_os = "windows")]
const LIB_NAMES_WINDOWS: &[&str] = &[
	"LLVMX86TargetMCA",
	"LLVMX86Disassembler",
	"LLVMX86AsmParser",
	"LLVMX86CodeGen",
	"LLVMX86Desc",
	"LLVMX86Info",
];

const LIB_NAMES: &[&str] = &[
	"LLVMWindowsManifest",
	"LLVMXRay",
	"LLVMLibDriver",
	"LLVMDlltoolDriver",
	"LLVMTextAPIBinaryReader",
	"LLVMCoverage",
	"LLVMLineEditor",
	"LLVMOrcDebugging",
	"LLVMOrcJIT",
	"LLVMWindowsDriver",
	"LLVMMCJIT",
	"LLVMJITLink",
	"LLVMInterpreter",
	"LLVMExecutionEngine",
	"LLVMRuntimeDyld",
	"LLVMOrcTargetProcess",
	"LLVMOrcShared",
	"LLVMDWP",
	"LLVMDebugInfoLogicalView",
	"LLVMDebugInfoGSYM",
	"LLVMOption",
	"LLVMObjectYAML",
	"LLVMObjCopy",
	"LLVMMCA",
	"LLVMMCDisassembler",
	"LLVMLTO",
	"LLVMFrontendOpenACC",
	"LLVMFrontendHLSL",
	"LLVMFrontendDriver",
	"LLVMExtensions",
	"LLVMPasses",
	"LLVMHipStdPar",
	"LLVMCoroutines",
	"LLVMCFGuard",
	"LLVMipo",
	"LLVMInstrumentation",
	"LLVMVectorize",
	"LLVMLinker",
	"LLVMFrontendOpenMP",
	"LLVMFrontendOffloading",
	"LLVMDWARFLinkerParallel",
	"LLVMDWARFLinkerClassic",
	"LLVMDWARFLinker",
	"LLVMGlobalISel",
	"LLVMMIRParser",
	"LLVMAsmPrinter",
	"LLVMSelectionDAG",
	"LLVMCodeGen",
	"LLVMTarget",
	"LLVMObjCARCOpts",
	"LLVMCodeGenTypes",
	"LLVMIRPrinter",
	"LLVMInterfaceStub",
	"LLVMFileCheck",
	"LLVMFuzzMutate",
	"LLVMScalarOpts",
	"LLVMInstCombine",
	"LLVMAggressiveInstCombine",
	"LLVMTransformUtils",
	"LLVMBitWriter",
	"LLVMAnalysis",
	"LLVMProfileData",
	"LLVMSymbolize",
	"LLVMDebugInfoBTF",
	"LLVMDebugInfoPDB",
	"LLVMDebugInfoMSF",
	"LLVMDebugInfoDWARF",
	"LLVMObject",
	"LLVMTextAPI",
	"LLVMMCParser",
	"LLVMIRReader",
	"LLVMAsmParser",
	"LLVMMC",
	"LLVMDebugInfoCodeView",
	"LLVMBitReader",
	"LLVMFuzzerCLI",
	"LLVMCore",
	"LLVMRemarks",
	"LLVMBitstreamReader",
	"LLVMBinaryFormat",
	"LLVMTargetParser",
	"LLVMTableGen",
	"LLVMSupport",
	"LLVMDemangle",
];
