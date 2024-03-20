#include <stdio.h>
#include <string.h>

#define MAX_LENGTH 1024

void is_even(char str[MAX_LENGTH], int length);

int main(void) {

	char str[MAX_LENGTH];
	int length;

	while (fgets(str, MAX_LENGTH, stdin) != NULL) {
		length = strlen(str);
		is_even(str, length);
	}

	return 0;
}

void is_even(char str[MAX_LENGTH], int length) {
	if (length % 2 == 0) {
		fputs( str, stdout);
	} 
}