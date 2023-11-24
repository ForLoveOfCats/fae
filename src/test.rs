use std::fs::read_dir;
use std::io::{stderr, Write};

use crate::c_codegen::DebugCodegen;
use crate::color::*;
use crate::project::build_project;

pub fn run_tests(args: Vec<String>) {
	let mut successes: u64 = 0;
	let mut failures = Vec::new();

	let mut entries: Vec<_> = read_dir("./tests").unwrap().map(|e| e.unwrap()).collect();
	entries.sort_by_cached_key(|e| e.file_name());

	for entry in entries {
		if !entry.path().is_dir() {
			continue;
		}

		let name = match entry.file_name().into_string() {
			Ok(name) => name,

			Err(..) => {
				eprintln!("\nTest {:?} has an invalid name", entry.file_name());
				std::process::exit(-1);
			}
		};

		if !args.is_empty() && !args.iter().any(|arg| name.contains(arg)) {
			continue;
		}

		let message = format!("  Building test {name}");
		let line = "─".repeat(message.len());
		eprintln!("\n┌{line}┐");
		eprintln!("{CYAN}{message}{RESET}");
		eprintln!("└{line}┘");

		let expected_error = std::fs::read_to_string(entry.path().join("error.txt")).ok();
		let expected_stdout = std::fs::read_to_string(entry.path().join("stdout.txt")).ok();
		let expected_stderr = std::fs::read_to_string(entry.path().join("stderr.txt")).ok();
		let mut error_output = String::new();

		let Some(binary_path) = build_project(&mut error_output, &entry.path(), name.clone(), DebugCodegen::OnFailure) else {
			if let Some(expected_error) = &expected_error {
				if error_output.trim_end() == expected_error.trim_end() {
					successes += 1;
					continue;
				}

				eprintln!("{RED}Compiler test harness: Got different error messages than expected{RESET}\n");
			}

			failures.push(name);
			eprint!("{error_output}");
			continue;
		};

		if expected_error.is_some() {
			eprintln!("{RED}Compiler test harness: Expected error messages but got none{RESET}");
			failures.push(name);
			continue;
		}

		let output = std::process::Command::new(&binary_path)
			.output()
			.expect("Failed to launch test binary, this is probably an internal bug");

		if !output.status.success() {
			eprintln!("{RED}Test {name:?} ended with failure exit code{RESET}");
			failures.push(name);
			continue;
		}

		if let Some(expected_stdout) = expected_stdout {
			if output.stdout != expected_stdout.as_bytes() {
				stderr().lock().write_all(&output.stdout).unwrap();
				eprintln!("\n{RED}Compiler test harness: Got different stdout than expected{RESET}\n");
				failures.push(name);
				continue;
			}
		} else if !output.stdout.is_empty() {
			stderr().lock().write_all(&output.stdout).unwrap();
			eprintln!("\n{RED}Compiler test harness: Got stdout but didn't expect any{RESET}\n");
			failures.push(name);
			continue;
		}

		if let Some(expected_stderr) = expected_stderr {
			if output.stderr != expected_stderr.as_bytes() {
				stderr().lock().write_all(&output.stderr).unwrap();
				eprintln!("\n{RED}Compiler test harness: Got different stderr than expected{RESET}\n");
				failures.push(name);
				continue;
			}
		} else if !output.stderr.is_empty() {
			stderr().lock().write_all(&output.stderr).unwrap();
			eprintln!("\n{RED}Compiler test harness: Got stderr but didn't expect any{RESET}\n");
			failures.push(name);
			continue;
		}

		successes += 1;
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
