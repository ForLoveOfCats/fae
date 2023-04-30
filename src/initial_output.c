#include <stddef.h>
#include <stdint.h>

typedef size_t usize;

typedef uint64_t u64;
typedef uint32_t u32;
typedef uint16_t u16;
typedef uint8_t u8;

typedef int64_t i64;
typedef int32_t i32;
typedef int16_t i16;
typedef int8_t i8;

typedef double f64;
typedef float f32;

void fae_main();

int main(int argc, char **argv) {
	(void)argc;
	(void)argv;
	fae_main();
	return 0;
}
