#!/usr/bin/env python3

import os
import subprocess

def main():
	print("Making sure cargo-license is installed")
	print()

	subprocess.run([
		"cargo",
		"install",
		"cargo-license",
	])

	print()
	print("Querying for licenses")
	print()

	subprocess.run([
		"cargo",
		"license",
		"--avoid-build-deps",
		"--avoid-dev-deps",
		"--features=bundled",
	])

if os.path.basename(os.getcwd()) != "fae":
	print("The current directory is not named \"fae\" which implies that we are not in the repo root.")
	print("This script *MUST* be run from the repo root as it will fail otherwise, exiting.")
else:
	main()
