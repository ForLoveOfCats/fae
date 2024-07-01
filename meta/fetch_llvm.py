#!/usr/bin/env python3

import os
import shutil
import subprocess
import multiprocessing
import platform

def linux_main():
	print("This script will build a local copy of LLVM for use in developing and bundling the Fae compiler.")
	print("To accomplish this it will clone LLVM, clone musl, build musl, build libc++ against musl, and")
	print("finally build LLVM against both libc++ and musl. This process is a bit of a hack so it will be")
	print("quite noisy and may take a while.")
	print()
	print("System dependencies required: a Clang+LLD toolchain, cmake, ninja, make, git, and a posix shell.")
	print("If any of these dependencies are missing then the fetch and build will fail.")
	print()
	print("[press enter to continue]", end="")
	input()
	print()

	remove_llvm_folder_if_exists()

	cpu_count = multiprocessing.cpu_count()
	jobs = user_number("How many compiler processes to use when building libc++ and LLVM?", cpu_count)
	print("This script will require no further user input to continue; you can get a coffee now, this may take a while.")
	print()

	clone_llvm()

	print()
	print("LLVM source files fetched, fetching musl")
	print()

	os.chdir("./llvm")
	subprocess.run([
		"git",
		"-c",
		"advice.detachedHead=false",
		"clone",
		"--depth",
		"1",
		"--branch",
		"v1.2.3",
		"git://git.musl-libc.org/musl",
	])

	print()
	print("musl source files fetched, preparing to build musl")
	print()

	os.chdir("./musl")
	subprocess.run(["./configure"], env={"CC": "clang"})

	print()
	print("musl build files written")
	print()

	subprocess.run(["make"], env={"CC": "clang"})

	print()
	print("musl has been built!")
	print()

	musl_include_dir = os.path.realpath("./include")
	musl_obj_include_dir = os.path.realpath("./obj/include")
	musl_arch_include_dir = os.path.realpath("./arch/x86_64")
	musl_generic_include_dir = os.path.realpath("./arch/generic")
	musl_lib_dir = os.path.realpath("./lib")
	musl_crt1 = os.path.realpath("./lib/crt1.o")
	musl_crti = os.path.realpath("./lib/crti.o")
	musl_crtn = os.path.realpath("./lib/crtn.o")
	musl_libc = os.path.realpath("./lib/libc.a")
	musl_rt = os.path.realpath("./lib/librt.a")
	musl_pthread = os.path.realpath("./lib/libpthread.a")
	os.chdir("../")

	print("Preparing to build libc++")
	print()

	os.chdir("libcxx")
	os.makedirs("./build", exist_ok=True)
	os.chdir("./build")

	subprocess.run([
		"cmake",
		"-G",
		"Ninja",
		"-S",
		"../../runtimes",
		"-DLIBUNWIND_ENABLE_SHARED=OFF",
		"-DLIBCXX_ENABLE_SHARED=OFF",
		"-DLIBCXXABI_USE_LLVM_UNWINDER=ON",
		"-DLIBCXX_HAS_MUSL_LIBC=ON",
		"-DLLVM_ENABLE_RUNTIMES=libcxx;libcxxabi;libunwind",
		"-DCMAKE_C_COMPILER=clang",
		"-DCMAKE_CXX_COMPILER=clang++",
		f"-DCMAKE_C_FLAGS=-Wno-everything -Wno-undef -fPIC -I{musl_include_dir} -I{musl_obj_include_dir} -I{musl_arch_include_dir} -I{musl_generic_include_dir} -I/usr/include -nostdinc -nostdlib -nodefaultlibs",
		f"-DCMAKE_CXX_FLAGS=-Wno-everything -Wno-undef -fPIC -I{musl_include_dir} -I{musl_obj_include_dir} -I{musl_arch_include_dir} -I{musl_generic_include_dir} -I/usr/include -nostdinc -nostdlib -nodefaultlibs",
		f"-DCMAKE_EXE_LINKER_FLAGS={musl_crt1} {musl_crti} {musl_crtn} {musl_libc} {musl_rt}",
		f"-DLLVM_PARALLEL_COMPILE_JOBS={jobs}",
	])

	print()
	print("libc++ build files written")
	print()

	subprocess.run([
		"cmake",
		"--build",
		".",
	])

	print()
	print("libc++ has been built!")
	print("Preparing to build LLVM itself")
	print()

	os.chdir("../../llvm")
	os.makedirs("./build", exist_ok=True)
	os.chdir("./build")

	include_dir = os.path.realpath("../../libcxx/build/include/c++/v1")
	lib_dir = os.path.realpath("../../libcxx/build/lib")
	libcxx = os.path.realpath("../../libcxx/build/lib/libc++.a")
	libcxx_abi = os.path.realpath("../../libcxx/build/lib/libc++abi.a")
	libcxx_unwind = os.path.realpath("../../libcxx/build/lib/libunwind.a")

	subprocess.run([
		"cmake",
		"-G",
		"Ninja",
		"-DCMAKE_C_COMPILER=clang",
		"-DCMAKE_CXX_COMPILER=clang++",
		f"-DCMAKE_C_FLAGS=-DHAVE_GETPAGESIZE -fPIC -nostdlib -nodefaultlibs -I{musl_include_dir} -I{musl_obj_include_dir} -I{musl_arch_include_dir} -I{musl_generic_include_dir}",
		f"-DCMAKE_CXX_FLAGS=-DHAVE_GETPAGESIZE -fPIC -Qunused-arguments -nostdlib -nostdinc++ -nostdlib++ -I{include_dir} -I{musl_include_dir} -I{musl_obj_include_dir} -I{musl_arch_include_dir} -I{musl_generic_include_dir} -L {lib_dir} -L {musl_lib_dir} -Wl,-rpath,{lib_dir}",
		f"-DCMAKE_EXE_LINKER_FLAGS={libcxx} {libcxx_abi} {libcxx_unwind} {musl_crt1} {musl_crti} {musl_crtn} {musl_libc} {musl_rt} {musl_pthread}",
		f"-DCMAKE_MODULE_LINKER_FLAGS={libcxx} {libcxx_abi} {libcxx_unwind} {musl_libc} {musl_rt} {musl_pthread}",
		f"-DCMAKE_SHARED_LINKER_FLAGS={libcxx} {libcxx_abi} {libcxx_unwind} {musl_libc} {musl_rt} {musl_pthread}",
		f"-DCMAKE_STATIC_LINKER_FLAGS={libcxx} {libcxx_abi} {libcxx_unwind} {musl_libc} {musl_rt} {musl_pthread}",
		"-DLLVM_ENABLE_LLD=ON",
		"-DCMAKE_BUILD_TYPE=Release",
		f"-DLLVM_PARALLEL_COMPILE_JOBS={jobs}",
		"-DLLVM_PARALLEL_LINK_JOBS=1", # Linking can absolutely slurp memory
		"-DLLVM_ENABLE_LIBCXX=ON",
		"-DLLVM_ENABLE_ZLIB=OFF",
		"-DLLVM_ENABLE_ZSTD=OFF",
		"-DLLVM_ENABLE_TERMINFO=OFF",
		"-DLLVM_ENABLE_LIBXML2=OFF",
		"-DLLVM_BUILD_BENCHMARKS=OFF",
		"-DLLVM_INCLUDE_BENCHMARKS=OFF",
		"-DLLVM_TARGETS_TO_BUILD=X86",
		".."
	])

	print()
	print("LLVM build files written")
	print()

	subprocess.run([
		"cmake",
		"--build",
		".",
	])

	print()
	print("LLVM has been fetched and built!")

def macos_main():
	remove_llvm_folder_if_exists()

	cpu_count = multiprocessing.cpu_count()
	jobs = user_number("How many compiler processes to use when building LLVM?", cpu_count)
	print("This script will require no further user input to continue; you can get a coffee now, this may take a while.")
	print()

	clone_llvm()

	print()
	print("LLVM source files fetched")
	print()

	os.chdir("./llvm/llvm")
	os.makedirs("./build", exist_ok=True)
	os.chdir("./build")

	subprocess.run([
		"cmake",
		"-G",
		"Ninja",
		"-DCMAKE_C_COMPILER=clang",
		"-DCMAKE_CXX_COMPILER=clang++",
		"-DLLVM_ENABLE_LLD=ON",
		"-DCMAKE_BUILD_TYPE=Release",
		f"-DLLVM_PARALLEL_COMPILE_JOBS={jobs}",
		"-DLLVM_PARALLEL_LINK_JOBS=1", # Linking can absolutely slurp memory
		"-DLLVM_ENABLE_ZLIB=OFF",
		"-DLLVM_ENABLE_ZSTD=OFF",
		"-DLLVM_ENABLE_TERMINFO=OFF",
		"-DLLVM_ENABLE_LIBXML2=OFF",
		"-DLLVM_BUILD_BENCHMARKS=OFF",
		"-DLLVM_INCLUDE_BENCHMARKS=OFF",
		"-DLLVM_TARGETS_TO_BUILD=AArch64",
		".."
	])

	print()
	print("LLVM build files written")
	print()

	subprocess.run([
		"cmake",
		"--build",
		".",
	])

	print()
	print("LLVM has been fetched and built!")

def remove_llvm_folder_if_exists():
	if os.path.exists("./llvm"):
		if user_confirm("LLVM folder already present, remove and continue?"):
			shutil.rmtree("./llvm")
			print("Existing LLVM folder removed")
			print()
		else:
			return

def clone_llvm():
	subprocess.run([
		"git",
		"-c",
		"advice.detachedHead=false",
		"clone",
		"--depth",
		"1",
		"--branch",
		"llvmorg-18.1.4",
		"https://github.com/llvm/llvm-project.git",
		"llvm",
	])

def user_confirm(message):
	while True:
		print(message, end='')
		print(" (y/n): ", end='')
		answer = input()

		if answer in ["Y", "y"]:
			return True
		elif answer in ["N", "n"]:
			return False
		else:
			print(f"Unrecognized answer {answer}")

def user_number(message, default):
	while True:
		print(message, end='')
		print(f" (default {default}): ", end='')
		answer = input()

		if not answer:
			return default

		try:
			return int(answer)
		except:
			print(f"Unable to parse {answer}")

if os.path.basename(os.getcwd()) != "fae":
	print("The current directory is not named \"fae\" which implies that we are not in the repo root.")
	print("This script *MUST* be run from the repo root as it will fail otherwise, exiting.")
elif platform.system().lower() == "linux":
	linux_main()
elif platform.system().lower() == "darwin":
	macos_main()
else:
    print(f"Fetch LLVM script does not currently support {platform.system()}")
