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

#[test]
fn test_move_literal32_to_register32() {
	let v = Literal32(0x12345678_u32.to_le_bytes());

	let mut bytes = Vec::new();
	let mut assembler = Assembler::new(&mut bytes);

	assembler.move_literal32_to_register32(v, Register32::Eax);
	assembler.move_literal32_to_register32(v, Register32::Ebx);
	assembler.move_literal32_to_register32(v, Register32::Ecx);
	assembler.move_literal32_to_register32(v, Register32::Edx);
	assembler.move_literal32_to_register32(v, Register32::Esi);
	assembler.move_literal32_to_register32(v, Register32::Edi);
	assembler.move_literal32_to_register32(v, Register32::Ebp);
	assembler.move_literal32_to_register32(v, Register32::Esp);

	assembler.move_literal32_to_register32(v, Register32::R8d);
	assembler.move_literal32_to_register32(v, Register32::R9d);
	assembler.move_literal32_to_register32(v, Register32::R10d);
	assembler.move_literal32_to_register32(v, Register32::R11d);
	assembler.move_literal32_to_register32(v, Register32::R12d);
	assembler.move_literal32_to_register32(v, Register32::R13d);
	assembler.move_literal32_to_register32(v, Register32::R14d);
	assembler.move_literal32_to_register32(v, Register32::R15d);

	assembler.expect(multi_line_string!(
		"b8 78 56 34 12" // 1
		"bb 78 56 34 12" // 2
		"b9 78 56 34 12" // 3
		"ba 78 56 34 12" // 4
		"be 78 56 34 12" // 5
		"bf 78 56 34 12" // 6
		"bd 78 56 34 12" // 7
		"bc 78 56 34 12" // 8
		"41 b8 78 56 34 12" // 9
		"41 b9 78 56 34 12" // 10
		"41 ba 78 56 34 12" // 11
		"41 bb 78 56 34 12" // 12
		"41 bc 78 56 34 12" // 13
		"41 bd 78 56 34 12" // 14
		"41 be 78 56 34 12" // 15
		"41 bf 78 56 34 12" // 16
	));
}

#[test]
fn test_push_register64() {
	let mut bytes = Vec::new();
	let mut assembler = Assembler::new(&mut bytes);

	assembler.push_register64(Register64::Rax);
	assembler.push_register64(Register64::Rbx);
	assembler.push_register64(Register64::Rcx);
	assembler.push_register64(Register64::Rdx);
	assembler.push_register64(Register64::Rsi);
	assembler.push_register64(Register64::Rdi);
	assembler.push_register64(Register64::Rbp);
	assembler.push_register64(Register64::Rsp);

	assembler.push_register64(Register64::R8);
	assembler.push_register64(Register64::R9);
	assembler.push_register64(Register64::R10);
	assembler.push_register64(Register64::R11);
	assembler.push_register64(Register64::R12);
	assembler.push_register64(Register64::R13);
	assembler.push_register64(Register64::R14);
	assembler.push_register64(Register64::R15);

	assembler.expect(multi_line_string!(
		"50" // 1
		"53" // 2
		"51" // 3
		"52" // 4
		"56" // 5
		"57" // 6
		"55" // 7
		"54" // 8
		"41 50" // 9
		"41 51" // 10
		"41 52" // 11
		"41 53" // 12
		"41 54" // 13
		"41 55" // 14
		"41 56" // 15
		"41 57" // 16
	));
}

#[test]
fn test_move_register64_to_register64() {
	let mut bytes = Vec::new();
	let mut assembler = Assembler::new(&mut bytes);

	assembler.move_register64_to_register64(Register64::Rax, Register64::Rax);
	assembler.move_register64_to_register64(Register64::Rax, Register64::Rbx);
	assembler.move_register64_to_register64(Register64::Rax, Register64::Rcx);
	assembler.move_register64_to_register64(Register64::Rax, Register64::Rdx);
	assembler.move_register64_to_register64(Register64::Rax, Register64::Rsi);
	assembler.move_register64_to_register64(Register64::Rax, Register64::Rdi);
	assembler.move_register64_to_register64(Register64::Rax, Register64::Rbp);
	assembler.move_register64_to_register64(Register64::Rax, Register64::Rsp);
	assembler.move_register64_to_register64(Register64::Rax, Register64::R8);
	assembler.move_register64_to_register64(Register64::Rax, Register64::R9);
	assembler.move_register64_to_register64(Register64::Rax, Register64::R10);
	assembler.move_register64_to_register64(Register64::Rax, Register64::R11);
	assembler.move_register64_to_register64(Register64::Rax, Register64::R12);
	assembler.move_register64_to_register64(Register64::Rax, Register64::R13);
	assembler.move_register64_to_register64(Register64::Rax, Register64::R14);
	assembler.move_register64_to_register64(Register64::Rax, Register64::R15);
	assembler.move_register64_to_register64(Register64::Rbx, Register64::Rax);
	assembler.move_register64_to_register64(Register64::Rbx, Register64::Rbx);
	assembler.move_register64_to_register64(Register64::Rbx, Register64::Rcx);
	assembler.move_register64_to_register64(Register64::Rbx, Register64::Rdx);
	assembler.move_register64_to_register64(Register64::Rbx, Register64::Rsi);
	assembler.move_register64_to_register64(Register64::Rbx, Register64::Rdi);
	assembler.move_register64_to_register64(Register64::Rbx, Register64::Rbp);
	assembler.move_register64_to_register64(Register64::Rbx, Register64::Rsp);
	assembler.move_register64_to_register64(Register64::Rbx, Register64::R8);
	assembler.move_register64_to_register64(Register64::Rbx, Register64::R9);
	assembler.move_register64_to_register64(Register64::Rbx, Register64::R10);
	assembler.move_register64_to_register64(Register64::Rbx, Register64::R11);
	assembler.move_register64_to_register64(Register64::Rbx, Register64::R12);
	assembler.move_register64_to_register64(Register64::Rbx, Register64::R13);
	assembler.move_register64_to_register64(Register64::Rbx, Register64::R14);
	assembler.move_register64_to_register64(Register64::Rbx, Register64::R15);
	assembler.move_register64_to_register64(Register64::Rcx, Register64::Rax);
	assembler.move_register64_to_register64(Register64::Rcx, Register64::Rbx);
	assembler.move_register64_to_register64(Register64::Rcx, Register64::Rcx);
	assembler.move_register64_to_register64(Register64::Rcx, Register64::Rdx);
	assembler.move_register64_to_register64(Register64::Rcx, Register64::Rsi);
	assembler.move_register64_to_register64(Register64::Rcx, Register64::Rdi);
	assembler.move_register64_to_register64(Register64::Rcx, Register64::Rbp);
	assembler.move_register64_to_register64(Register64::Rcx, Register64::Rsp);
	assembler.move_register64_to_register64(Register64::Rcx, Register64::R8);
	assembler.move_register64_to_register64(Register64::Rcx, Register64::R9);
	assembler.move_register64_to_register64(Register64::Rcx, Register64::R10);
	assembler.move_register64_to_register64(Register64::Rcx, Register64::R11);
	assembler.move_register64_to_register64(Register64::Rcx, Register64::R12);
	assembler.move_register64_to_register64(Register64::Rcx, Register64::R13);
	assembler.move_register64_to_register64(Register64::Rcx, Register64::R14);
	assembler.move_register64_to_register64(Register64::Rcx, Register64::R15);
	assembler.move_register64_to_register64(Register64::Rdx, Register64::Rax);
	assembler.move_register64_to_register64(Register64::Rdx, Register64::Rbx);
	assembler.move_register64_to_register64(Register64::Rdx, Register64::Rcx);
	assembler.move_register64_to_register64(Register64::Rdx, Register64::Rdx);
	assembler.move_register64_to_register64(Register64::Rdx, Register64::Rsi);
	assembler.move_register64_to_register64(Register64::Rdx, Register64::Rdi);
	assembler.move_register64_to_register64(Register64::Rdx, Register64::Rbp);
	assembler.move_register64_to_register64(Register64::Rdx, Register64::Rsp);
	assembler.move_register64_to_register64(Register64::Rdx, Register64::R8);
	assembler.move_register64_to_register64(Register64::Rdx, Register64::R9);
	assembler.move_register64_to_register64(Register64::Rdx, Register64::R10);
	assembler.move_register64_to_register64(Register64::Rdx, Register64::R11);
	assembler.move_register64_to_register64(Register64::Rdx, Register64::R12);
	assembler.move_register64_to_register64(Register64::Rdx, Register64::R13);
	assembler.move_register64_to_register64(Register64::Rdx, Register64::R14);
	assembler.move_register64_to_register64(Register64::Rdx, Register64::R15);
	assembler.move_register64_to_register64(Register64::Rsi, Register64::Rax);
	assembler.move_register64_to_register64(Register64::Rsi, Register64::Rbx);
	assembler.move_register64_to_register64(Register64::Rsi, Register64::Rcx);
	assembler.move_register64_to_register64(Register64::Rsi, Register64::Rdx);
	assembler.move_register64_to_register64(Register64::Rsi, Register64::Rsi);
	assembler.move_register64_to_register64(Register64::Rsi, Register64::Rdi);
	assembler.move_register64_to_register64(Register64::Rsi, Register64::Rbp);
	assembler.move_register64_to_register64(Register64::Rsi, Register64::Rsp);
	assembler.move_register64_to_register64(Register64::Rsi, Register64::R8);
	assembler.move_register64_to_register64(Register64::Rsi, Register64::R9);
	assembler.move_register64_to_register64(Register64::Rsi, Register64::R10);
	assembler.move_register64_to_register64(Register64::Rsi, Register64::R11);
	assembler.move_register64_to_register64(Register64::Rsi, Register64::R12);
	assembler.move_register64_to_register64(Register64::Rsi, Register64::R13);
	assembler.move_register64_to_register64(Register64::Rsi, Register64::R14);
	assembler.move_register64_to_register64(Register64::Rsi, Register64::R15);
	assembler.move_register64_to_register64(Register64::Rdi, Register64::Rax);
	assembler.move_register64_to_register64(Register64::Rdi, Register64::Rbx);
	assembler.move_register64_to_register64(Register64::Rdi, Register64::Rcx);
	assembler.move_register64_to_register64(Register64::Rdi, Register64::Rdx);
	assembler.move_register64_to_register64(Register64::Rdi, Register64::Rsi);
	assembler.move_register64_to_register64(Register64::Rdi, Register64::Rdi);
	assembler.move_register64_to_register64(Register64::Rdi, Register64::Rbp);
	assembler.move_register64_to_register64(Register64::Rdi, Register64::Rsp);
	assembler.move_register64_to_register64(Register64::Rdi, Register64::R8);
	assembler.move_register64_to_register64(Register64::Rdi, Register64::R9);
	assembler.move_register64_to_register64(Register64::Rdi, Register64::R10);
	assembler.move_register64_to_register64(Register64::Rdi, Register64::R11);
	assembler.move_register64_to_register64(Register64::Rdi, Register64::R12);
	assembler.move_register64_to_register64(Register64::Rdi, Register64::R13);
	assembler.move_register64_to_register64(Register64::Rdi, Register64::R14);
	assembler.move_register64_to_register64(Register64::Rdi, Register64::R15);
	assembler.move_register64_to_register64(Register64::Rbp, Register64::Rax);
	assembler.move_register64_to_register64(Register64::Rbp, Register64::Rbx);
	assembler.move_register64_to_register64(Register64::Rbp, Register64::Rcx);
	assembler.move_register64_to_register64(Register64::Rbp, Register64::Rdx);
	assembler.move_register64_to_register64(Register64::Rbp, Register64::Rsi);
	assembler.move_register64_to_register64(Register64::Rbp, Register64::Rdi);
	assembler.move_register64_to_register64(Register64::Rbp, Register64::Rbp);
	assembler.move_register64_to_register64(Register64::Rbp, Register64::Rsp);
	assembler.move_register64_to_register64(Register64::Rbp, Register64::R8);
	assembler.move_register64_to_register64(Register64::Rbp, Register64::R9);
	assembler.move_register64_to_register64(Register64::Rbp, Register64::R10);
	assembler.move_register64_to_register64(Register64::Rbp, Register64::R11);
	assembler.move_register64_to_register64(Register64::Rbp, Register64::R12);
	assembler.move_register64_to_register64(Register64::Rbp, Register64::R13);
	assembler.move_register64_to_register64(Register64::Rbp, Register64::R14);
	assembler.move_register64_to_register64(Register64::Rbp, Register64::R15);
	assembler.move_register64_to_register64(Register64::Rsp, Register64::Rax);
	assembler.move_register64_to_register64(Register64::Rsp, Register64::Rbx);
	assembler.move_register64_to_register64(Register64::Rsp, Register64::Rcx);
	assembler.move_register64_to_register64(Register64::Rsp, Register64::Rdx);
	assembler.move_register64_to_register64(Register64::Rsp, Register64::Rsi);
	assembler.move_register64_to_register64(Register64::Rsp, Register64::Rdi);
	assembler.move_register64_to_register64(Register64::Rsp, Register64::Rbp);
	assembler.move_register64_to_register64(Register64::Rsp, Register64::Rsp);
	assembler.move_register64_to_register64(Register64::Rsp, Register64::R8);
	assembler.move_register64_to_register64(Register64::Rsp, Register64::R9);
	assembler.move_register64_to_register64(Register64::Rsp, Register64::R10);
	assembler.move_register64_to_register64(Register64::Rsp, Register64::R11);
	assembler.move_register64_to_register64(Register64::Rsp, Register64::R12);
	assembler.move_register64_to_register64(Register64::Rsp, Register64::R13);
	assembler.move_register64_to_register64(Register64::Rsp, Register64::R14);
	assembler.move_register64_to_register64(Register64::Rsp, Register64::R15);
	assembler.move_register64_to_register64(Register64::R8, Register64::Rax);
	assembler.move_register64_to_register64(Register64::R8, Register64::Rbx);
	assembler.move_register64_to_register64(Register64::R8, Register64::Rcx);
	assembler.move_register64_to_register64(Register64::R8, Register64::Rdx);
	assembler.move_register64_to_register64(Register64::R8, Register64::Rsi);
	assembler.move_register64_to_register64(Register64::R8, Register64::Rdi);
	assembler.move_register64_to_register64(Register64::R8, Register64::Rbp);
	assembler.move_register64_to_register64(Register64::R8, Register64::Rsp);
	assembler.move_register64_to_register64(Register64::R8, Register64::R8);
	assembler.move_register64_to_register64(Register64::R8, Register64::R9);
	assembler.move_register64_to_register64(Register64::R8, Register64::R10);
	assembler.move_register64_to_register64(Register64::R8, Register64::R11);
	assembler.move_register64_to_register64(Register64::R8, Register64::R12);
	assembler.move_register64_to_register64(Register64::R8, Register64::R13);
	assembler.move_register64_to_register64(Register64::R8, Register64::R14);
	assembler.move_register64_to_register64(Register64::R8, Register64::R15);
	assembler.move_register64_to_register64(Register64::R9, Register64::Rax);
	assembler.move_register64_to_register64(Register64::R9, Register64::Rbx);
	assembler.move_register64_to_register64(Register64::R9, Register64::Rcx);
	assembler.move_register64_to_register64(Register64::R9, Register64::Rdx);
	assembler.move_register64_to_register64(Register64::R9, Register64::Rsi);
	assembler.move_register64_to_register64(Register64::R9, Register64::Rdi);
	assembler.move_register64_to_register64(Register64::R9, Register64::Rbp);
	assembler.move_register64_to_register64(Register64::R9, Register64::Rsp);
	assembler.move_register64_to_register64(Register64::R9, Register64::R8);
	assembler.move_register64_to_register64(Register64::R9, Register64::R9);
	assembler.move_register64_to_register64(Register64::R9, Register64::R10);
	assembler.move_register64_to_register64(Register64::R9, Register64::R11);
	assembler.move_register64_to_register64(Register64::R9, Register64::R12);
	assembler.move_register64_to_register64(Register64::R9, Register64::R13);
	assembler.move_register64_to_register64(Register64::R9, Register64::R14);
	assembler.move_register64_to_register64(Register64::R9, Register64::R15);
	assembler.move_register64_to_register64(Register64::R10, Register64::Rax);
	assembler.move_register64_to_register64(Register64::R10, Register64::Rbx);
	assembler.move_register64_to_register64(Register64::R10, Register64::Rcx);
	assembler.move_register64_to_register64(Register64::R10, Register64::Rdx);
	assembler.move_register64_to_register64(Register64::R10, Register64::Rsi);
	assembler.move_register64_to_register64(Register64::R10, Register64::Rdi);
	assembler.move_register64_to_register64(Register64::R10, Register64::Rbp);
	assembler.move_register64_to_register64(Register64::R10, Register64::Rsp);
	assembler.move_register64_to_register64(Register64::R10, Register64::R8);
	assembler.move_register64_to_register64(Register64::R10, Register64::R9);
	assembler.move_register64_to_register64(Register64::R10, Register64::R10);
	assembler.move_register64_to_register64(Register64::R10, Register64::R11);
	assembler.move_register64_to_register64(Register64::R10, Register64::R12);
	assembler.move_register64_to_register64(Register64::R10, Register64::R13);
	assembler.move_register64_to_register64(Register64::R10, Register64::R14);
	assembler.move_register64_to_register64(Register64::R10, Register64::R15);
	assembler.move_register64_to_register64(Register64::R11, Register64::Rax);
	assembler.move_register64_to_register64(Register64::R11, Register64::Rbx);
	assembler.move_register64_to_register64(Register64::R11, Register64::Rcx);
	assembler.move_register64_to_register64(Register64::R11, Register64::Rdx);
	assembler.move_register64_to_register64(Register64::R11, Register64::Rsi);
	assembler.move_register64_to_register64(Register64::R11, Register64::Rdi);
	assembler.move_register64_to_register64(Register64::R11, Register64::Rbp);
	assembler.move_register64_to_register64(Register64::R11, Register64::Rsp);
	assembler.move_register64_to_register64(Register64::R11, Register64::R8);
	assembler.move_register64_to_register64(Register64::R11, Register64::R9);
	assembler.move_register64_to_register64(Register64::R11, Register64::R10);
	assembler.move_register64_to_register64(Register64::R11, Register64::R11);
	assembler.move_register64_to_register64(Register64::R11, Register64::R12);
	assembler.move_register64_to_register64(Register64::R11, Register64::R13);
	assembler.move_register64_to_register64(Register64::R11, Register64::R14);
	assembler.move_register64_to_register64(Register64::R11, Register64::R15);
	assembler.move_register64_to_register64(Register64::R12, Register64::Rax);
	assembler.move_register64_to_register64(Register64::R12, Register64::Rbx);
	assembler.move_register64_to_register64(Register64::R12, Register64::Rcx);
	assembler.move_register64_to_register64(Register64::R12, Register64::Rdx);
	assembler.move_register64_to_register64(Register64::R12, Register64::Rsi);
	assembler.move_register64_to_register64(Register64::R12, Register64::Rdi);
	assembler.move_register64_to_register64(Register64::R12, Register64::Rbp);
	assembler.move_register64_to_register64(Register64::R12, Register64::Rsp);
	assembler.move_register64_to_register64(Register64::R12, Register64::R8);
	assembler.move_register64_to_register64(Register64::R12, Register64::R9);
	assembler.move_register64_to_register64(Register64::R12, Register64::R10);
	assembler.move_register64_to_register64(Register64::R12, Register64::R11);
	assembler.move_register64_to_register64(Register64::R12, Register64::R12);
	assembler.move_register64_to_register64(Register64::R12, Register64::R13);
	assembler.move_register64_to_register64(Register64::R12, Register64::R14);
	assembler.move_register64_to_register64(Register64::R12, Register64::R15);
	assembler.move_register64_to_register64(Register64::R13, Register64::Rax);
	assembler.move_register64_to_register64(Register64::R13, Register64::Rbx);
	assembler.move_register64_to_register64(Register64::R13, Register64::Rcx);
	assembler.move_register64_to_register64(Register64::R13, Register64::Rdx);
	assembler.move_register64_to_register64(Register64::R13, Register64::Rsi);
	assembler.move_register64_to_register64(Register64::R13, Register64::Rdi);
	assembler.move_register64_to_register64(Register64::R13, Register64::Rbp);
	assembler.move_register64_to_register64(Register64::R13, Register64::Rsp);
	assembler.move_register64_to_register64(Register64::R13, Register64::R8);
	assembler.move_register64_to_register64(Register64::R13, Register64::R9);
	assembler.move_register64_to_register64(Register64::R13, Register64::R10);
	assembler.move_register64_to_register64(Register64::R13, Register64::R11);
	assembler.move_register64_to_register64(Register64::R13, Register64::R12);
	assembler.move_register64_to_register64(Register64::R13, Register64::R13);
	assembler.move_register64_to_register64(Register64::R13, Register64::R14);
	assembler.move_register64_to_register64(Register64::R13, Register64::R15);
	assembler.move_register64_to_register64(Register64::R14, Register64::Rax);
	assembler.move_register64_to_register64(Register64::R14, Register64::Rbx);
	assembler.move_register64_to_register64(Register64::R14, Register64::Rcx);
	assembler.move_register64_to_register64(Register64::R14, Register64::Rdx);
	assembler.move_register64_to_register64(Register64::R14, Register64::Rsi);
	assembler.move_register64_to_register64(Register64::R14, Register64::Rdi);
	assembler.move_register64_to_register64(Register64::R14, Register64::Rbp);
	assembler.move_register64_to_register64(Register64::R14, Register64::Rsp);
	assembler.move_register64_to_register64(Register64::R14, Register64::R8);
	assembler.move_register64_to_register64(Register64::R14, Register64::R9);
	assembler.move_register64_to_register64(Register64::R14, Register64::R10);
	assembler.move_register64_to_register64(Register64::R14, Register64::R11);
	assembler.move_register64_to_register64(Register64::R14, Register64::R12);
	assembler.move_register64_to_register64(Register64::R14, Register64::R13);
	assembler.move_register64_to_register64(Register64::R14, Register64::R14);
	assembler.move_register64_to_register64(Register64::R14, Register64::R15);
	assembler.move_register64_to_register64(Register64::R15, Register64::Rax);
	assembler.move_register64_to_register64(Register64::R15, Register64::Rbx);
	assembler.move_register64_to_register64(Register64::R15, Register64::Rcx);
	assembler.move_register64_to_register64(Register64::R15, Register64::Rdx);
	assembler.move_register64_to_register64(Register64::R15, Register64::Rsi);
	assembler.move_register64_to_register64(Register64::R15, Register64::Rdi);
	assembler.move_register64_to_register64(Register64::R15, Register64::Rbp);
	assembler.move_register64_to_register64(Register64::R15, Register64::Rsp);
	assembler.move_register64_to_register64(Register64::R15, Register64::R8);
	assembler.move_register64_to_register64(Register64::R15, Register64::R9);
	assembler.move_register64_to_register64(Register64::R15, Register64::R10);
	assembler.move_register64_to_register64(Register64::R15, Register64::R11);
	assembler.move_register64_to_register64(Register64::R15, Register64::R12);
	assembler.move_register64_to_register64(Register64::R15, Register64::R13);
	assembler.move_register64_to_register64(Register64::R15, Register64::R14);
	assembler.move_register64_to_register64(Register64::R15, Register64::R15);

	assembler.expect(multi_line_string!(
		"48 89 c0"
		"48 89 c3"
		"48 89 c1"
		"48 89 c2"
		"48 89 c6"
		"48 89 c7"
		"48 89 c5"
		"48 89 c4"
		"49 89 c0"
		"49 89 c1"
		"49 89 c2"
		"49 89 c3"
		"49 89 c4"
		"49 89 c5"
		"49 89 c6"
		"49 89 c7"
		"48 89 d8"
		"48 89 db"
		"48 89 d9"
		"48 89 da"
		"48 89 de"
		"48 89 df"
		"48 89 dd"
		"48 89 dc"
		"49 89 d8"
		"49 89 d9"
		"49 89 da"
		"49 89 db"
		"49 89 dc"
		"49 89 dd"
		"49 89 de"
		"49 89 df"
		"48 89 c8"
		"48 89 cb"
		"48 89 c9"
		"48 89 ca"
		"48 89 ce"
		"48 89 cf"
		"48 89 cd"
		"48 89 cc"
		"49 89 c8"
		"49 89 c9"
		"49 89 ca"
		"49 89 cb"
		"49 89 cc"
		"49 89 cd"
		"49 89 ce"
		"49 89 cf"
		"48 89 d0"
		"48 89 d3"
		"48 89 d1"
		"48 89 d2"
		"48 89 d6"
		"48 89 d7"
		"48 89 d5"
		"48 89 d4"
		"49 89 d0"
		"49 89 d1"
		"49 89 d2"
		"49 89 d3"
		"49 89 d4"
		"49 89 d5"
		"49 89 d6"
		"49 89 d7"
		"48 89 f0"
		"48 89 f3"
		"48 89 f1"
		"48 89 f2"
		"48 89 f6"
		"48 89 f7"
		"48 89 f5"
		"48 89 f4"
		"49 89 f0"
		"49 89 f1"
		"49 89 f2"
		"49 89 f3"
		"49 89 f4"
		"49 89 f5"
		"49 89 f6"
		"49 89 f7"
		"48 89 f8"
		"48 89 fb"
		"48 89 f9"
		"48 89 fa"
		"48 89 fe"
		"48 89 ff"
		"48 89 fd"
		"48 89 fc"
		"49 89 f8"
		"49 89 f9"
		"49 89 fa"
		"49 89 fb"
		"49 89 fc"
		"49 89 fd"
		"49 89 fe"
		"49 89 ff"
		"48 89 e8"
		"48 89 eb"
		"48 89 e9"
		"48 89 ea"
		"48 89 ee"
		"48 89 ef"
		"48 89 ed"
		"48 89 ec"
		"49 89 e8"
		"49 89 e9"
		"49 89 ea"
		"49 89 eb"
		"49 89 ec"
		"49 89 ed"
		"49 89 ee"
		"49 89 ef"
		"48 89 e0"
		"48 89 e3"
		"48 89 e1"
		"48 89 e2"
		"48 89 e6"
		"48 89 e7"
		"48 89 e5"
		"48 89 e4"
		"49 89 e0"
		"49 89 e1"
		"49 89 e2"
		"49 89 e3"
		"49 89 e4"
		"49 89 e5"
		"49 89 e6"
		"49 89 e7"
		"4c 89 c0"
		"4c 89 c3"
		"4c 89 c1"
		"4c 89 c2"
		"4c 89 c6"
		"4c 89 c7"
		"4c 89 c5"
		"4c 89 c4"
		"4d 89 c0"
		"4d 89 c1"
		"4d 89 c2"
		"4d 89 c3"
		"4d 89 c4"
		"4d 89 c5"
		"4d 89 c6"
		"4d 89 c7"
		"4c 89 c8"
		"4c 89 cb"
		"4c 89 c9"
		"4c 89 ca"
		"4c 89 ce"
		"4c 89 cf"
		"4c 89 cd"
		"4c 89 cc"
		"4d 89 c8"
		"4d 89 c9"
		"4d 89 ca"
		"4d 89 cb"
		"4d 89 cc"
		"4d 89 cd"
		"4d 89 ce"
		"4d 89 cf"
		"4c 89 d0"
		"4c 89 d3"
		"4c 89 d1"
		"4c 89 d2"
		"4c 89 d6"
		"4c 89 d7"
		"4c 89 d5"
		"4c 89 d4"
		"4d 89 d0"
		"4d 89 d1"
		"4d 89 d2"
		"4d 89 d3"
		"4d 89 d4"
		"4d 89 d5"
		"4d 89 d6"
		"4d 89 d7"
		"4c 89 d8"
		"4c 89 db"
		"4c 89 d9"
		"4c 89 da"
		"4c 89 de"
		"4c 89 df"
		"4c 89 dd"
		"4c 89 dc"
		"4d 89 d8"
		"4d 89 d9"
		"4d 89 da"
		"4d 89 db"
		"4d 89 dc"
		"4d 89 dd"
		"4d 89 de"
		"4d 89 df"
		"4c 89 e0"
		"4c 89 e3"
		"4c 89 e1"
		"4c 89 e2"
		"4c 89 e6"
		"4c 89 e7"
		"4c 89 e5"
		"4c 89 e4"
		"4d 89 e0"
		"4d 89 e1"
		"4d 89 e2"
		"4d 89 e3"
		"4d 89 e4"
		"4d 89 e5"
		"4d 89 e6"
		"4d 89 e7"
		"4c 89 e8"
		"4c 89 eb"
		"4c 89 e9"
		"4c 89 ea"
		"4c 89 ee"
		"4c 89 ef"
		"4c 89 ed"
		"4c 89 ec"
		"4d 89 e8"
		"4d 89 e9"
		"4d 89 ea"
		"4d 89 eb"
		"4d 89 ec"
		"4d 89 ed"
		"4d 89 ee"
		"4d 89 ef"
		"4c 89 f0"
		"4c 89 f3"
		"4c 89 f1"
		"4c 89 f2"
		"4c 89 f6"
		"4c 89 f7"
		"4c 89 f5"
		"4c 89 f4"
		"4d 89 f0"
		"4d 89 f1"
		"4d 89 f2"
		"4d 89 f3"
		"4d 89 f4"
		"4d 89 f5"
		"4d 89 f6"
		"4d 89 f7"
		"4c 89 f8"
		"4c 89 fb"
		"4c 89 f9"
		"4c 89 fa"
		"4c 89 fe"
		"4c 89 ff"
		"4c 89 fd"
		"4c 89 fc"
		"4d 89 f8"
		"4d 89 f9"
		"4d 89 fa"
		"4d 89 fb"
		"4d 89 fc"
		"4d 89 fd"
		"4d 89 fe"
		"4d 89 ff"
	));
}

#[test]
fn test_sub_intermediate32_to_register64() {
	let v = Literal32(0x12345678_u32.to_le_bytes());

	let mut bytes = Vec::new();
	let mut assembler = Assembler::new(&mut bytes);

	assembler.sub_literal32_to_register64(v, Register64::Rax);
	assembler.sub_literal32_to_register64(v, Register64::Rbx);
	assembler.sub_literal32_to_register64(v, Register64::Rcx);
	assembler.sub_literal32_to_register64(v, Register64::Rdx);
	assembler.sub_literal32_to_register64(v, Register64::Rsi);
	assembler.sub_literal32_to_register64(v, Register64::Rdi);
	assembler.sub_literal32_to_register64(v, Register64::Rbp);
	assembler.sub_literal32_to_register64(v, Register64::Rsp);

	assembler.sub_literal32_to_register64(v, Register64::R8);
	assembler.sub_literal32_to_register64(v, Register64::R9);
	assembler.sub_literal32_to_register64(v, Register64::R10);
	assembler.sub_literal32_to_register64(v, Register64::R11);
	assembler.sub_literal32_to_register64(v, Register64::R12);
	assembler.sub_literal32_to_register64(v, Register64::R13);
	assembler.sub_literal32_to_register64(v, Register64::R14);
	assembler.sub_literal32_to_register64(v, Register64::R15);

	assembler.expect(multi_line_string!(
		"48 2d 78 56 34 12"
		"48 81 eb 78 56 34 12"
		"48 81 e9 78 56 34 12"
		"48 81 ea 78 56 34 12"
		"48 81 ee 78 56 34 12"
		"48 81 ef 78 56 34 12"
		"48 81 ed 78 56 34 12"
		"48 81 ec 78 56 34 12"
		"49 81 e8 78 56 34 12"
		"49 81 e9 78 56 34 12"
		"49 81 ea 78 56 34 12"
		"49 81 eb 78 56 34 12"
		"49 81 ec 78 56 34 12"
		"49 81 ed 78 56 34 12"
		"49 81 ee 78 56 34 12"
		"49 81 ef 78 56 34 12"
	));
}
