#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

int main(int argc, char *argv[]) {
    FILE *stream = fopen(argv[1], "r");

    if (stream == NULL) {
        perror(argv[1]);
        return 1;
    }
    int c;
    int vowels = 0;
    while ((c = fgetc(stream)) != EOF) {
        if (
        c == 'a' ||
        c == 'e' ||
        c == 'i' ||
        c == 'o' ||
        c == 'u' ||
        c == 'A' ||
        c == 'E' ||
        c == 'I' ||
        c == 'O' ||
        c == 'U') 
        {
            vowels++;
        }
    }

    fclose(stream);
    printf("%d\n", vowels);
    return 0;
}