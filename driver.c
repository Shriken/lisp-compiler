#include <stdio.h>

#define FIXNUM_MASK  0x3
#define FIXNUM_TAG   0x0
#define FIXNUM_SHIFT 0x2

#define CHAR_MASK    0xff
#define CHAR_TAG     0xf
#define CHAR_SHIFT   0x8

#define EMPTY_LIST   0x2f

#define BOOL_MASK    0x7f
#define BOOL_TAG     0x1f
#define BOOL_SHIFT   0x7

int main(int argc, char **argv) {
	int val = scheme_entry();

	if ((val & FIXNUM_MASK) == FIXNUM_TAG) {
		printf("%d\n", val >> FIXNUM_SHIFT);
	} else if ((val & CHAR_MASK) == CHAR_TAG) {
		printf("%c\n", val >> CHAR_SHIFT);
	} else if (val == EMPTY_LIST) {
		printf("()\n");
	} else if ((val & BOOL_MASK) == BOOL_TAG) {
		val >>= BOOL_SHIFT;
		printf(val ? "true\n" : "false\n");
	} else {
		printf("bad value: %x\n", val);
	}

	return 0;
}
