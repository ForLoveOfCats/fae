import os
import shutil
import subprocess
import multiprocessing
import platform

def linux_main():
	print("This script will fetch and build a local copy of LLVM for use by the Fae compiler")
	print("System dependencies required for this step: an existing C++ toolchain, cmake, ninja, and git")
	print("If any of these dependencies are missing then the fetch and build will fail")
	print()

	if os.path.exists("./llvm"):
		if user_confirm("LLVM folder already present, remove and continue?"):
			shutil.rmtree("./llvm")
			print("Existing LLVM folder removed")
			print()
		else:
			return

	cpu_count = multiprocessing.cpu_count()
	jobs = user_number("How many compiler processes to use when building libc++ and LLVM?", cpu_count)
	print("This script will require no further user input to continue; you can get a coffee now, this may take a while")
	print()

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

	print()
	print("LLVM source files fetched, preparing to build libc++")
	print()

	os.chdir("./llvm")
	os.chdir("libcxx")
	os.makedirs("./build", exist_ok=True)
	os.chdir("./build")

	subprocess.run([
		"cmake",
		"-G",
		"Ninja",
		"-S",
		"../../runtimes",
		"-DLIBCXXABI_USE_LLVM_UNWINDER=OFF",
		"-DLLVM_ENABLE_RUNTIMES=libcxx;libcxxabi",
		"-DCMAKE_C_COMPILER=clang",
		"-DCMAKE_CXX_COMPILER=clang++",
		"-DCMAKE_C_FLAGS=-Wno-everything",
		"-DCMAKE_CXX_FLAGS=-Wno-everything",
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
	print("libc++ has been and built!")
	print("Preparing to build LLVM itself")
	print()

	os.chdir("../../llvm")
	os.makedirs("./build", exist_ok=True)
	os.chdir("./build")

	include_dir = os.path.realpath("../../libcxx/build/include/c++/v1")
	lib_dir = os.path.realpath("../../libcxx/build/lib")

	subprocess.run([
		"cmake",
		"-G",
		"Ninja",
		"-DCMAKE_C_COMPILER=clang",
		"-DCMAKE_CXX_COMPILER=clang++",
		"-DCMAKE_C_FLAGS=-Wno-everything",
		f"-DCMAKE_CXX_FLAGS=-Wno-everything -Qunused-arguments -nostdinc++ -nostdlib++ -isystem {include_dir} -L {lib_dir} -Wl,-rpath,{lib_dir} -lc++",
		"-DLLVM_ENABLE_LLD=ON",
		"-DCMAKE_BUILD_TYPE=Release",
		f"-DLLVM_PARALLEL_COMPILE_JOBS={jobs}",
		"-DLLVM_PARALLEL_LINK_JOBS=1", # Linking can absolutely slurp memory
		"-DLLVM_ENABLE_LIBCXX=ON",
		"-DLLVM_ENABLE_ZLIB=OFF",
		"-DLLVM_ENABLE_ZSTD=OFF",
		"-DLLVM_ENABLE_TERMINFO=OFF",
		"-DLLVM_ENABLE_LIBXML2=OFF",
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


if platform.system().lower() == "linux":
	linux_main()
else:
    print(f"Fetch LLVM script does not currently support {platform.system()}")
