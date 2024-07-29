#!/usr/bin/env python3

import os
import shutil
import subprocess
import platform
import argparse
from datetime import datetime

def linux_main():
	print("Ensuring that Rust `x86_64-unknown-linux-musl` target toolchain is installed")
	print("This requires that the Rust installation be managed with rustup")
	print()

	subprocess.run([
		"rustup",
		"target",
		"add",
		"x86_64-unknown-linux-musl",
	])

	print()
	print("Building Fae compiler with musl in release mode")
	print()

	subprocess.run([
		"cargo",
		"build",
		"--features=bundled",
		"--release",
		"--target",
		"x86_64-unknown-linux-musl",
	])

	print()
	print("Copying files")

	shutil.rmtree("./target/bundle", ignore_errors=True)
	os.makedirs("./target/bundle/fae", exist_ok=True)

	shutil.copy("./target/x86_64-unknown-linux-musl/release/fae", "./target/bundle/fae/fae")
	shutil.copytree("./lib", "./target/bundle/fae/lib")

	print("Archiving bundle")

	os.chdir("./target")
	shutil.make_archive("fae", "gztar", "./bundle")

	print("Produced `./target/fae.tar.gz`")

def macos_main():
	print("Building Fae compiler in release mode")
	print()

	subprocess.run([
		"cargo",
		"build",
		"--features=bundled",
		"--release",
	])

	print()
	print("Copying files")

	shutil.rmtree("./target/bundle", ignore_errors=True)
	os.makedirs("./target/bundle/fae", exist_ok=True)

	shutil.copy("./target/release/fae", "./target/bundle/fae/fae")
	shutil.copytree("./lib", "./target/bundle/fae/lib")

	print("Archiving bundle")

	os.chdir("./target")
	shutil.make_archive("fae", "gztar", "./bundle")

	print("Produced `./target/fae.tar.gz`")

def call_external(args):
	process = subprocess.run(args, capture_output=True)
	return process.stdout

def call_external_stderr(args):
	process = subprocess.run(args, capture_output=True)
	return process.stderr

platform = platform.system().lower()

if os.path.basename(os.getcwd()) != "fae":
	print("The current directory is not named \"fae\" which implies that we are not in the repo root.")
	print("This script *MUST* be run from the repo root as it will fail otherwise, exiting.")
elif platform == "linux":
	linux_main()
elif platform == "darwin":
	macos_main()
else:
    print(f"Bundle script does not currently support {platform.system()}")
