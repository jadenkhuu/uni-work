#include <stdio.h>
#include <ctype.h>

void convert_lower(char c);

int main(void) {

	char c;
	while (scanf("%c", &c) != EOF) {
		convert_lower(c);
	}
	
	return 0;
}

void convert_lower(char c) {
	if (c > 64 && c < 91) {
		c = c + 32;
		printf("%c", c);
	} else {
		printf("%c", c);
	}
}

