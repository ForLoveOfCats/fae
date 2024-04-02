#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef uint8_t bool;
#define true 1
#define false 0

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

#define check_slice_bounds(slice, index) \
	if (index < 0 || slice.fi_1 <= index) { \
		fprintf(stderr, "Index %ld is out of range of slice with length %ld\n", index, slice.fi_1); \
		exit(EXIT_FAILURE); \
	}

void fae_main(void);

int main(int argc, char **argv) {
	(void)argc;
	(void)argv;
	fae_main();
	return 0;
}