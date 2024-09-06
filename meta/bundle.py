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
	print("Building Fae compiler with musl in release mode (requires clang)")
	print()

	env = os.environ.copy()
	env["CC"] = "clang"

	subprocess.run([
			"cargo",
			"build",
			"--profile=bundled",
			"--features=bundled",
			"--target",
			"x86_64-unknown-linux-musl",
		],
		env=env
	)

	print()
	print("Copying files")

	shutil.rmtree("./target/bundle", ignore_errors=True)
	try: os.remove("./target/Fae.tar.gz")
	except: pass
	os.makedirs("./target/bundle/fae", exist_ok=True)

	shutil.copy("./target/x86_64-unknown-linux-musl/bundled/fae", "./target/bundle/fae/fae")
	shutil.copytree("./lib", "./target/bundle/fae/lib")
	shutil.copy("./meta/bundle_license.txt", "./target/bundle/fae/license.txt")
	shutil.copy("./docs/language_reference.md", "./target/bundle/fae/language_reference.md")

	print("Archiving bundle")

	os.chdir("./target")
	shutil.make_archive("Fae", "gztar", "./bundle")

	print("Produced `./target/Fae.tar.gz`")

def macos_main(args):
	print("Building Fae compiler in release mode")
	print()

	subprocess.run([
		"cargo",
		"build",
		"--profile=bundled",
		"--features=bundled",
	])

	print()
	print("Copying files")

	shutil.rmtree("./target/bundle", ignore_errors=True)
	try: os.remove("./target/Fae.dmg")
	except: pass
	os.makedirs("./target/bundle/fae", exist_ok=True)

	shutil.copy("./target/bundled/fae", "./target/bundle/fae/fae")
	shutil.copytree("./lib", "./target/bundle/fae/lib")
	shutil.copy("./meta/bundle_license.txt", "./target/bundle/fae/license.txt")
	shutil.copy("./docs/language_reference.md", "./target/bundle/fae/language_reference.md")

	if args.identity_uuid is not None:
		print("Signing executable")
		subprocess.run([
			"codesign",
			"-s",
			args.identity_uuid,
			"-o",
			"runtime",
			"./target/bundle/fae/fae",
		])

	print("Archiving bundle")

	os.chdir("./target")
	# shutil.make_archive("fae", "zip", "./bundle")
	subprocess.run([
		"hdiutil",
		"create",
		"-volname",
		"Fae",
		"-srcfolder",
		"./bundle/fae",
		"-ov",
		"-format",
		"UDZO",
		"Fae.dmg",
	])

	print("Produced `./target/Fae.dmg`")

	if args.notarization_keychain_profile is not None:
		print("Notarizing bundle")
		subprocess.run([
			"xcrun",
			"notarytool",
			"submit",
			"./Fae.dmg",
			"--keychain-profile",
			args.notarization_keychain_profile,
			"--wait",
		])

		print("Stapling bundle")
		subprocess.run([
			"xcrun",
			"stapler",
			"staple",
			"./Fae.dmg",
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
platform = platform.system().lower()

if os.path.basename(os.getcwd()) != "fae":
	print("The current directory is not named \"fae\" which implies that we are not in the repo root.")
	print("This script *MUST* be run from the repo root as it will fail otherwise, exiting.")
elif platform == "linux":
	args = parser.parse_args()
	linux_main()
elif platform == "darwin":
	parser.add_argument("--identity-uuid", action="store", default=None)
	parser.add_argument("--notarization-keychain-profile", action="store", default=None)
	args = parser.parse_args()
	macos_main(args)
else:
    print(f"Bundle script does not currently support {platform.system()}")
