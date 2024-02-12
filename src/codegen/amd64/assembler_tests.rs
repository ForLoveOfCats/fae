use crate::codegen::amd64::assembler::*;
use crate::codegen::literal::Literal32;

#[test]
fn test_add_intermediate32_to_register32() {
	let v = Literal32(0x12345678_u32.to_le_bytes());

	let mut bytes = Vec::new();
	let mut assembler = Assembler::new(&mut bytes);
	assembler.add_literal32_to_register32(v, Register32::Eax);
	assembler.add_literal32_to_register32(v, Register32::Ebx);
	assembler.add_literal32_to_register32(v, Register32::Ecx);
	assembler.add_literal32_to_register32(v, Register32::Edx);
	assembler.add_literal32_to_register32(v, Register32::Esi);
	assembler.add_literal32_to_register32(v, Register32::Edi);
	assembler.add_literal32_to_register32(v, Register32::Ebp);
	assembler.add_literal32_to_register32(v, Register32::Esp);

	assembler.add_literal32_to_register32(v, Register32::R8d);
	assembler.add_literal32_to_register32(v, Register32::R9d);
	assembler.add_literal32_to_register32(v, Register32::R10d);
	assembler.add_literal32_to_register32(v, Register32::R11d);
	assembler.add_literal32_to_register32(v, Register32::R12d);
	assembler.add_literal32_to_register32(v, Register32::R13d);
	assembler.add_literal32_to_register32(v, Register32::R14d);
	assembler.add_literal32_to_register32(v, Register32::R15d);

	assembler.expect(multi_line_string!(
		"05 78 56 34 12" // 1
		"81 c3 78 56 34 12" // 2
		"81 c1 78 56 34 12" // 3
		"81 c2 78 56 34 12" // 4
		"81 c6 78 56 34 12" // 5
		"81 c7 78 56 34 12" // 6
		"81 c5 78 56 34 12" // 7
		"81 c4 78 56 34 12" // 8
		"41 81 c0 78 56 34 12" // 9
		"41 81 c1 78 56 34 12" // 10
		"41 81 c2 78 56 34 12" // 11
		"41 81 c3 78 56 34 12" // 12
		"41 81 c4 78 56 34 12" // 13
		"41 81 c5 78 56 34 12" // 14
		"41 81 c6 78 56 34 12" // 15
		"41 81 c7 78 56 34 12" // 16
	));
}
