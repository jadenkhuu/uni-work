// COMP1521 21T2 ... final exam, question 3

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

void
cp (char *path_from, char *path_to)
{

	FILE *read = fopen(path_from, "r");
	FILE *write = fopen(path_to, "w");

	int c;
	while ((c = fgetc(read)) != EOF) {
		fputc(c, write);
	}

	return;

	// TODO
}

