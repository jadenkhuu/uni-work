#include <stdio.h>
#include <stdlib.h>

void minimum(int length, char **array);
void maximum(int length, char **array);
void sum(int length, char **array);
void product(int length, char **array);
void mean(int length, char **array);


int main(int argc, char **argv) {
//	(void) argc, (void) argv; // keep the compiler quiet, should be removed

	minimum(argc, argv);
	maximum(argc, argv);
	sum(argc, argv);
	product(argc, argv);
	mean(argc, argv);


	return 0;
}

void minimum(int length, char **array) {
	int minimum = atoi(array[1]);

	for (int i = 1; i < length; i++) {
		if (atoi(array[i]) < minimum) {
			minimum = atoi(array[i]);
		}
	}

	printf("MIN:  %d\n", minimum);
}
void maximum(int length, char **array) {
	int max = atoi(array[1]);

	for (int i = 1; i < length; i++) {
		if (atoi(array[i]) > max) {
			max = atoi(array[i]);
		}
	}

	printf("MAX:  %d\n", max);
}
void sum(int length, char **array) {
	int sum = 0;

	for (int i = 1; i < length; i++) {
		sum += atoi(array[i]);
	}

	printf("SUM:  %d\n", sum);
}
void product(int length, char **array) {
	int prod = 1;

	for (int i = 1; i < length; i++) {
		prod *= atoi(array[i]);
	}

	printf("PROD: %d\n", prod);
}
void mean(int length, char **array) {
	
	int sum = 0;
	int mean = 0;

	for (int i = 1; i < length; i++) {
		sum += atoi(array[i]);
	}

	mean = sum / (length - 1);

	printf("MEAN: %d\n", mean);
}

