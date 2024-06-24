#!/usr/bin/env python3

import subprocess

RUN_COUNT = 100

def main():
	print("Benchmark program must be generated for this script to function")
	input("Press enter to run the validator benchmark: ")

	print()

	ms_sum = 0
	for iteration in range(1, RUN_COUNT + 1):
		stderr = call_external_stderr([
			"cargo",
			"run",
			"--quiet",
			"--release",
			"c",
			"./benchmark/generated_program",
			"--disable-message-color",
		])

		for unstripped in stderr.splitlines():
			line = unstripped.strip()
			prefix = "Validated project: took "
			if line.startswith(prefix):
				line = line.removeprefix(prefix).removesuffix(" ms")
				ms = int(line)
				ms_sum += ms
				print(f"On run {iteration} the validator took {ms} ms")

	print()
	average = float(ms_sum) / float(RUN_COUNT)
	print(f"Average validator time across {RUN_COUNT} runs is {average} ms")

def call_external_stderr(args):
	process = subprocess.run(args, capture_output=True)
	return process.stderr.decode("utf-8")

main()
