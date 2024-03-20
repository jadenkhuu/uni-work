#include <stdio.h>
#include <stdlib.h>

void collatz(int i);

int main(int argc, char **argv)
{
	// (void) argc, (void) argv; // keep the compiler quiet, should be removed
	
	collatz(atoi(argv[1]));

	return EXIT_SUCCESS;
}

void collatz(int i) {
	
	printf("%d\n", i);

	if (i == 1) {
		return;
	} else if (i % 2 == 1) {
		i = 3*i + 1;
		collatz(i);
	} else if (i % 2 == 0) {
		i /= 2;
		collatz(i);
	}

}