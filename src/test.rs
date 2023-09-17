use std::fs::read_dir;

use crate::color::*;
use crate::project::build_project;

pub fn run_tests() {
	let mut successes: u64 = 0;
	let mut failures = Vec::new();

	let mut entries: Vec<_> = read_dir("./tests").unwrap().map(|e| e.unwrap()).collect();
	entries.sort_by_cached_key(|e| e.file_name());

	for entry in entries {
		if !entry.path().is_dir() {
			continue;
		}

		let name = match entry.file_name().to_str() {
			Some(name) => name.to_owned(),

			None => {
				eprintln!("\nTest {:?} has an invalid name", entry.file_name());
				std::process::exit(-1);
			}
		};

		let message = format!("  Building test {name}");
		let line = "─".repeat(message.len());
		eprintln!("\n┌{line}┐");
		eprintln!("{CYAN}{message}{RESET}");
		eprintln!("└{line}┘");

		let expected = std::fs::read_to_string(entry.path().join("expect.txt")).ok();
		let mut error_output = String::new();

		let Some(binary_path) = build_project(&mut error_output, &entry.path(), name.clone(), false) else {
			if let Some(expected) = &expected {
				if error_output.trim_end() == expected.trim_end() {
					successes += 1;
					continue;
				}

				eprintln!("{RED}Compiler test harness: Got different error messages than expected{RESET}\n");
			}

			failures.push(name);
			eprint!("{error_output}");
			continue;
		};

		if expected.is_some() {
			eprintln!("{RED}Compiler test harness: Expected error messages but got none{RESET}");
			failures.push(name);
			continue;
		}

		let status = std::process::Command::new(&binary_path)
			.spawn()
			.expect("Failed to launch test binary, this is probably an internal bug")
			.wait();

		if !matches!(status, Ok(status) if status.success()) {
			eprintln!("{RED}Test {name:?} ended with failure exit code{RESET}");
			failures.push(name);
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
