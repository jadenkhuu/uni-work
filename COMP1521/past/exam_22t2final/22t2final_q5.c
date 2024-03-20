// COMP1521 22T2 ... final exam, question 5

#include <stdio.h>

void print_bytes(FILE *file, long n) {
	// TODO
	
	fseek(file, 0, SEEK_END);
	long length = ftell(file);
	fseek(file, 0, SEEK_SET);

	if (n > length) {
		n = length;
	}
	
	long stop = 0;
	if (n > 0) {
		fseek(file, n, SEEK_SET);
		stop = ftell(file);
	}
	if (n < 0) {
		fseek(file, n, SEEK_END);
		stop = ftell(file);
	}

	fseek(file, 0, SEEK_SET);

	int c;
	for (int i = 0; i < stop; i++) {
		c = fgetc(file);
		putchar(c);
	}

	return;
}
