// COMP1521 22T2 ... final exam, question 1

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

int count_zero_bits(uint32_t x) {
	// TODO
	
	int num_zeroes = 0;

	for (int i = 0; i < 32; i++) {
		if ((1 & (x >> i)) == 0) {
			num_zeroes++;
		}
	}

	return num_zeroes;
}
