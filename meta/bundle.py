#!/usr/bin/env python3

import os
import shutil
import subprocess
import platform
import argparse
from datetime import datetime

def linux_main(args):
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

	if args.snapshot:
		print()
		print("Creating snapshot, this requires the GitHub cli be installed and signed in with approprate permissions")

		if call_external(["git", "rev-parse", "--abbrev-ref", "HEAD"]) != b"main\n":
			print("Safety check: Must be on main branch!")
			return

		if call_external(["git", "status", "--porcelain"]):
			print("Safety check: Must be in a fully commited state!")
			return

		if call_external_stderr(["git", "push", "-n"]) != b"Everything up-to-date\n":
			print("Safety check: Main must be fully pushed!")
			return

		snapshot_name = datetime.today().strftime("snapshot-%Y-%b-%d")
		date_time = datetime.today().strftime("%A %B %-m %Y at %-I:%M")
		print(f"Snapshot name: {snapshot_name}")

		subprocess.run([
			"gh",
			"release",
			"create",
			snapshot_name,
			"fae.tar.gz",
			"--prerelease",
			"--draft",
			"--notes",
			f"Fae programming language toolchain snapshot for Linux created {date_time}"
		])

def call_external(args):
	process = subprocess.run(args, capture_output=True)
	return process.stdout

def call_external_stderr(args):
	process = subprocess.run(args, capture_output=True)
	return process.stderr

parser = argparse.ArgumentParser(
	prog="bundle.py",
	description="Produces a redistributable bundle of the Fae programming language toolchain",
)
parser.add_argument("--snapshot", action="store_true")
args = parser.parse_args()

if os.path.basename(os.getcwd()) != "fae":
	print("The current directory is not named \"fae\" which implies that we are not in the repo root.")
	print("This script *MUST* be run from the repo root as it will fail otherwise, exiting.")
elif platform.system().lower() == "linux":
	linux_main(args)
else:
    print(f"Bundle script does not currently support {platform.system()}")
