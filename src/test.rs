use std::fs::{read_dir, DirEntry};
use std::io::{stderr, Write};
use std::path::PathBuf;

use crate::cli::CliArguments;
use crate::color::*;
use crate::frontend::project::{build_project, ProjectConfig, WindowsSubsystem};

pub fn run_tests(mut cli_arguments: CliArguments) -> ! {
	let mut test_count: u64 = 0;
	let mut run_successes: u64 = 0;
	let mut run_failures = Vec::new();

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
		test_count += 1;

		if cli_arguments.optimize_artifacts {
			run_test(&cli_arguments, &test_name, &entry, &mut run_successes, &mut run_failures);
		} else {
			let test_success = run_test(&cli_arguments, &test_name, &entry, &mut run_successes, &mut run_failures);

			if test_success {
				cli_arguments.optimize_artifacts = true;
				run_test(&cli_arguments, &test_name, &entry, &mut run_successes, &mut run_failures);
				cli_arguments.optimize_artifacts = false;
			}
		}
	}

	let tests = if test_count > 1 { "tests" } else { "test" };
	let overall = format!("  Ran {test_count} {tests} overall");

	let invocations = if run_successes > 1 { "invocations" } else { "invocation" };
	let success = format!("  {run_successes} {invocations} completed successfully");

	let invocations = if run_failures.len() > 1 { "invocations" } else { "invocation" };
	let failure = format!("  However {} test {invocations} failed:", run_failures.len());

	let line = "─".repeat(overall.len().max(success.len().max(failure.len())));
	eprintln!("\n┌{line}┐");
	eprintln!("{BOLD_CYAN}{overall}{RESET}");
	eprintln!("{GREEN}{success}{RESET}");

	if !run_failures.is_empty() {
		eprintln!("{RED}{failure}{RESET}");

		for failure in &run_failures {
			eprintln!("    ▪ {failure}");
		}
	}

	eprintln!("└{line}┘");

	if !run_failures.is_empty() {
		std::process::exit(-1);
	} else {
		std::process::exit(0);
	}
}

// Returns if success or not
fn run_test(
	cli_arguments: &CliArguments,
	test_name: &str,
	entry: &DirEntry,
	successes: &mut u64,
	failures: &mut Vec<String>,
) -> bool {
	let optimized = match cli_arguments.optimize_artifacts {
		false => "unoptimized",
		true => "optimized",
	};

	let message = format!("  Building {optimized} test {test_name}");
	let line = "─".repeat(message.len());
	eprintln!("\n┌{line}┐");
	eprintln!("{CYAN}{message}{RESET}");
	eprintln!("└{line}┘");

	let expected_error = std::fs::read_to_string(entry.path().join("error.txt")).ok();
	let expected_stdout = std::fs::read_to_string(entry.path().join("stdout.txt")).ok();
	let expected_stderr = std::fs::read_to_string(entry.path().join("stderr.txt")).ok();
	let panics = std::fs::metadata(entry.path().join("panics")).is_ok();
	let mut error_output = String::new();

	let mut test_failed = false;

	let test_config_path = entry.path().join("fae.toml");
	let default_test_config = ProjectConfig {
		project_name: test_name.to_string(),
		source_directory: PathBuf::from(""),
		provide_main: true,
		linux_linker: None,
		linux_additional_linker_flags: None,
		linux_additional_linker_objects: None,
		darwin_linker: None,
		darwin_additional_linker_flags: None,
		darwin_additional_linker_objects: None,
		windows_subsystem: WindowsSubsystem::Console,
		windows_linker: None,
		windows_additional_linker_flags: None,
		windows_additional_linker_objects: None,
	};

	let test_config = if let Ok(config_file) = std::fs::read_to_string(&test_config_path) {
		match toml::from_str::<ProjectConfig>(&config_file) {
			Ok(mut test_config) => {
				test_config.project_name = default_test_config.project_name.clone();
				test_config
			}

			Err(err) => {
				eprintln!("Project config parse error {test_config_path:?}\n{err}");
				test_failed |= true;
				default_test_config
			}
		}
	} else {
		default_test_config
	};
	let expect_executable = test_config.provide_main;

	let built_project = build_project(cli_arguments, &mut error_output, &entry.path(), Some(test_config));
	let test_name = format!("{optimized} {test_name}");

	if built_project.any_messages {
		if let Some(expected_error) = &expected_error {
			if error_output.trim_end() != expected_error.trim_end() {
				eprintln!("{RED}Compiler test harness: Got different error/warning messages than expected{RESET}\n");
				eprint!("{error_output}");
				test_failed = true;
			} else if built_project.binary_path.is_none() {
				*successes += 1;
				return true;
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

	let mut actual_stdout = Vec::new();
	let mut actual_stderr = Vec::new();

	if expect_executable {
		let Some(binary_path) = built_project.binary_path else {
			eprintln!("{RED}Compiler test harness: Failed to build test, did not produce a binary to run{RESET}\n");
			failures.push(test_name);
			return false;
		};

		let output = std::process::Command::new(&binary_path)
			.output()
			.expect("Failed to launch test binary, this is probably an internal bug");

		if panics && output.status.success() {
			eprintln!("{RED}Test was expected to end with failure exit code but didn't{RESET}");
			test_failed = true;
		} else if !panics && !output.status.success() {
			eprintln!("{RED}Test ended with failure exit code{RESET}");
			test_failed = true;
		}

		actual_stdout = output.stdout;
		actual_stderr = output.stderr;
	}

	if let Some(expected_stdout) = expected_stdout {
		if !compare_output_matching(&actual_stdout, expected_stdout.as_bytes()) {
			stderr().lock().write_all(&actual_stdout).unwrap();
			eprintln!("\n{RED}Compiler test harness: Got different stdout than expected{RESET}\n");
			test_failed = true;
		}
	} else if !actual_stdout.is_empty() {
		stderr().lock().write_all(&actual_stdout).unwrap();
		eprintln!("\n{RED}Compiler test harness: Got stdout but didn't expect any{RESET}\n");
		test_failed = true;
	}

	if let Some(expected_stderr) = expected_stderr {
		if !compare_output_matching(&actual_stderr, expected_stderr.as_bytes()) {
			stderr().lock().write_all(&actual_stderr).unwrap();
			eprintln!("\n{RED}Compiler test harness: Got different stderr than expected{RESET}\n");
			failures.push(test_name);
			return false;
		}
	} else if !actual_stderr.is_empty() {
		stderr().lock().write_all(&actual_stderr).unwrap();
		eprintln!("\n{RED}Compiler test harness: Got stderr but didn't expect any{RESET}\n");
		test_failed = true;
	}

	if test_failed {
		failures.push(test_name);
		false
	} else {
		*successes += 1;
		true
	}
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn compare_output_matching(actual: &[u8], expected: &[u8]) -> bool {
	actual == expected
}

#[cfg(target_os = "windows")]
fn compare_output_matching(actual: &[u8], expected: &[u8]) -> bool {
	fn next_is(actual_iter: &mut impl Iterator<Item = u8>, expected_byte: u8) -> bool {
		actual_iter.next().map_or(false, |b| b == expected_byte)
	}

	let mut actual_iter = actual.iter().copied();

	for &expected_byte in expected {
		if expected_byte == b'\n' {
			if !(next_is(&mut actual_iter, b'\r') && next_is(&mut actual_iter, b'\n')) {
				return false;
			}
		} else {
			if !next_is(&mut actual_iter, expected_byte) {
				return false;
			}
		}
	}

	if actual_iter.next().is_some() {
		return false; // Some actual bytes left over once we have consumed all the expected
	}

	true
}
