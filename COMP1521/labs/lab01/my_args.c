#include <stdio.h>

int main(int argc, char *argv[]) {
//	(void) argc, (void) argv; // keep the compiler quiet, should be removed

	printf("Program name: %s\n", argv[0]);

	if (argv[1] == NULL) {
		printf("There are no other arguments\n");
		return 0;
	}

	printf("There are %d arguments:\n", (argc - 1));

	for (int i = 1; i < argc; i++) {
		printf("\tArgument %d is \"%s\"\n", i, argv[i]);
	}

	return 0;
}
