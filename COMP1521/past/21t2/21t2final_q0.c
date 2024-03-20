// COMP1521 21T2 ... final exam, question 0

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

int
count_leading_zeroes (uint32_t x)
{
	// TODO
	int numZeroes = 0;

	for (int i = 31; i >= 0; i--) {
		if (((x >> i) & 1) == 1) {
			break;
		}
		numZeroes++;
	}

	return numZeroes;
}

