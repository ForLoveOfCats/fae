macro_rules! multi_line_string {
	( $( $line:literal )* ) => {
		concat!( $( $line, "\n" ),* )
	}
}
