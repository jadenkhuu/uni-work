// COMP1521 21T2 ... final exam, question 1

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BITS 8

void
and (bool x[BITS], bool y[BITS], bool result[BITS])
{
	// TODO
	for (int i = 7; i >= 0; i--) {
		if ((x[i] == true) && (y[i] == true)) {
			result[i] = true;
			continue;
		}	
		result[i] = false;
	}
}

void
or (bool x[BITS], bool y[BITS], bool result[BITS])
{
	// TODO
	for (int i = 7; i >= 0; i--) {
		if ((x[i] == true) || (y[i] == true)) {
			result[i] = true;
			continue;
		}	
		result[i] = false;
	}

}

void
xor (bool x[BITS], bool y[BITS], bool result[BITS])
{
// TODO
	for (int i = 7; i >= 0; i--) {
		if ((x[i] == true) && (y[i] == true)) {
			result[i] = false;
			continue;
		}	
		else if ((x[i] == false) && (y[i] == false)) {
			result[i] = false;
			continue;
		}	
		result[i] = true;
	}
}

void
not (bool x[BITS], bool result[BITS])
{
	// TODO
	for (int i = 7; i >= 0; i--) {
		if (x[i] == true) {
			result[i] = false;
		} else {
			result[i] = true;
		}		
	}
}
