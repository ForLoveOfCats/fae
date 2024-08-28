use crate::color::{BOLD_GREEN, BOLD_YELLOW, RESET};
use crate::TARGET_DIR;

pub fn clean_workspace() -> ! {
	match std::fs::remove_dir_all(TARGET_DIR) {
		Ok(_) => eprintln!("{BOLD_GREEN}Removed target directory{RESET}"),
		Err(_) => eprintln!("{BOLD_YELLOW}No target directory to remove{RESET}"),
	};
	std::process::exit(0);
}
