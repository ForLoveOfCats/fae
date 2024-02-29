# registers = [
# "Rax",
# "Rbx",
# "Rcx",
# "Rdx",
# "Rsi",
# "Rdi",
# "Rbp",
# "Rsp",
# "R8",
# "R9",
# "R10",
# "R11",
# "R12",
# "R13",
# "R14",
# "R15",
# ]

# for source in registers:
# 	for destination in registers:
# 		print("mov", destination, ",", source)
# 		# print(f"assembler.move_register64_to_register64(Register64::{source}, Register64::{destination});")

# exit()

def format_bool(value):
	return "✅" if value else "❌"

while True:
	line = input(">>> ")
	words = line.split()
	selector = words.pop(0)

	if selector == "rex":
		byte = int(words[0], 16)

		assert(byte >> 4 == 0b0100)
		w = bool((byte >> 3) & 1)
		r = bool((byte >> 2) & 1)
		x = bool((byte >> 1) & 1)
		b = bool(byte & 1)

		print("W:", format_bool(w))
		print("R:", format_bool(r))
		print("X:", format_bool(x))
		print("B:", format_bool(b))

		print()
	elif selector == "modrm":
		byte = int(words[0], 16)
		
		mod = byte >> 6
		assert(mod == 0b11)
		print("Direct addressing mode")
		
		reg = (byte >> 3) & 0b111
		print("Reg:", reg)

		rm = byte & 0b111
		print("R/m:", rm)

		print()
	else:
		print("Unknown selector:", selector)
