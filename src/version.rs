use std::env::consts::{ARCH, OS};

use git_version::git_version;

const BASE_VERSION: &str = "0.0.1";

const GIT: &str = git_version!(args = ["--always", "--dirty=-modified", "--broken"]);

#[cfg(feature = "bundled")]
const BUNDLED: &str = "-bundled";
#[cfg(not(feature = "bundled"))]
const BUNDLED: &str = "";

pub fn version() -> String {
	format!("{BASE_VERSION}-{GIT}{BUNDLED} on {OS} {ARCH}")
}
