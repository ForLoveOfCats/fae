#![allow(unreachable_code)]

fn main() {
	#[cfg(target_os = "linux")]
	{
		linux_link();
		return;
	}

	panic!("Unsupported platform")
}

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

	for name in LINUX_LIB_NAMES {
		println!("cargo::rustc-link-lib=static={}", name);
	}

	println!("cargo::rustc-link-lib=static=c++");
	println!("cargo::rustc-link-lib=static=c++abi");
}

const LINUX_LIB_NAMES: &[&str] = &[
	"LLVMWindowsManifest",
	"LLVMXRay",
	"LLVMLibDriver",
	"LLVMDlltoolDriver",
	"LLVMTextAPIBinaryReader",
	"LLVMCoverage",
	"LLVMLineEditor",
	"LLVMX86TargetMCA",
	"LLVMX86Disassembler",
	"LLVMX86AsmParser",
	"LLVMX86CodeGen",
	"LLVMX86Desc",
	"LLVMX86Info",
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
