#include <stdio.h>

void check_vowel(char input);

int main(void) {

	char c;
	while (scanf("%c", &c) != EOF) {
		check_vowel(c);
	}

	return 0;
}


void check_vowel(char input) {
	if (input == 'a' ||
			input == 'e' ||
			input == 'i' ||
			input == 'o' ||
			input == 'u' ||
			input == 'A' ||
			input == 'E' ||
			input == 'I' ||
			input == 'O' ||
			input == 'U' ) {

	} else {
		printf("%c", input);
	}
}