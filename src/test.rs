use std::fs::read_dir;
use std::io::{stderr, Write};

use crate::cli_arguments::CliArguments;
use crate::color::*;
use crate::frontend::project::build_project;

pub fn run_tests(cli_arguments: &CliArguments) {
	let mut successes: u64 = 0;
	let mut failures = Vec::new();

	let mut entries: Vec<_> = read_dir("./tests").unwrap().map(|e| e.unwrap()).collect();
	entries.sort_by_cached_key(|e| e.file_name());

	for entry in entries {
		if !entry.path().is_dir() {
			continue;
		}

		let test_name = match entry.file_name().into_string() {
			Ok(name) => name,

			Err(..) => {
				eprintln!("\nTest {:?} has an invalid name", entry.file_name());
				std::process::exit(-1);
			}
		};

		let specified_names = &cli_arguments.compiler_test_names;
		if !specified_names.is_empty() && !specified_names.iter().any(|n| test_name.contains(n)) {
			continue;
		}

		let message = format!("  Building test {test_name}");
		let line = "─".repeat(message.len());
		eprintln!("\n┌{line}┐");
		eprintln!("{CYAN}{message}{RESET}");
		eprintln!("└{line}┘");

		let expected_error = std::fs::read_to_string(entry.path().join("error.txt")).ok();
		let expected_stdout = std::fs::read_to_string(entry.path().join("stdout.txt")).ok();
		let expected_stderr = std::fs::read_to_string(entry.path().join("stderr.txt")).ok();
		let panics = std::fs::metadata(entry.path().join("panics")).is_ok();
		let mut error_output = String::new();

		let built_project = build_project(cli_arguments, &mut error_output, &entry.path(), test_name.clone());
		let mut test_failed = false;

		if built_project.any_messages {
			if let Some(expected_error) = &expected_error {
				if error_output.trim_end() != expected_error.trim_end() {
					eprintln!("{RED}Compiler test harness: Got different error/warning messages than expected{RESET}\n");
					eprint!("{error_output}");
					test_failed = true;
				} else if built_project.binary_path.is_none() {
					successes += 1;
					continue;
				}
			} else {
				eprintln!("{RED}Compiler test harness: Got error/warning messages but did not expect any{RESET}\n");
				eprint!("{error_output}");
				test_failed = true;
			}
		} else if expected_error.is_some() {
			eprintln!("{RED}Compiler test harness: Expected error/warning messages but got none{RESET}");
			test_failed = true;
		}

		let Some(binary_path) = built_project.binary_path else {
			eprintln!("{RED}Compiler test harness: Failed to build test, did not produce a binary to run{RESET}\n");
			failures.push(test_name);
			continue;
		};

		let output = std::process::Command::new(&binary_path)
			.output()
			.expect("Failed to launch test binary, this is probably an internal bug");

		if panics && output.status.success() {
			eprintln!("{RED}Test {test_name:?} was expected to end with failure exit code but didn't{RESET}");
			test_failed = true;
		} else if !panics && !output.status.success() {
			eprintln!("{RED}Test {test_name:?} ended with failure exit code{RESET}");
			test_failed = true;
		}

		if let Some(expected_stdout) = expected_stdout {
			if output.stdout != expected_stdout.as_bytes() {
				stderr().lock().write_all(&output.stdout).unwrap();
				eprintln!("\n{RED}Compiler test harness: Got different stdout than expected{RESET}\n");
				test_failed = true;
			}
		} else if !output.stdout.is_empty() {
			stderr().lock().write_all(&output.stdout).unwrap();
			eprintln!("\n{RED}Compiler test harness: Got stdout but didn't expect any{RESET}\n");
			test_failed = true;
		}

		if let Some(expected_stderr) = expected_stderr {
			if output.stderr != expected_stderr.as_bytes() {
				stderr().lock().write_all(&output.stderr).unwrap();
				eprintln!("\n{RED}Compiler test harness: Got different stderr than expected{RESET}\n");
				failures.push(test_name);
				continue;
			}
		} else if !output.stderr.is_empty() {
			stderr().lock().write_all(&output.stderr).unwrap();
			eprintln!("\n{RED}Compiler test harness: Got stderr but didn't expect any{RESET}\n");
			test_failed = true;
		}

		if test_failed {
			failures.push(test_name);
		} else {
			successes += 1;
		}
	}

	let success = format!("  Ran {successes} test(s) successfully");
	let failure = format!("  However {} test(s) failed:", failures.len());
	let line = "─".repeat(success.len().max(failure.len()));
	eprintln!("\n┌{line}┐");
	eprintln!("{GREEN}{success}{RESET}");

	if !failures.is_empty() {
		eprintln!("{RED}{failure}{RESET}");

		for failure in failures {
			eprintln!("    ▪ {failure}");
		}
	}

	eprintln!("└{line}┘");
}
