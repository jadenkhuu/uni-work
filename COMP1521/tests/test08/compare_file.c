#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

int main(int argc, char *argv[]) {
    FILE *stream = fopen(argv[1], "r");
    if (stream == NULL) {
        perror(argv[1]);
        return 1;
    }
    FILE *stream2 = fopen(argv[2], "r");
    if (stream2 == NULL) {
        perror(argv[2]);
        return 1;
    }

    int a;
    int b;
    int byte = 0;
    while ((a = fgetc(stream)) != EOF && (b = fgetc(stream2)) != EOF) {
        if (a != b) {
            printf("Files differ at byte %d\n", byte);
            return 0;
        }
        byte++;
    }
    if ((a = fgetc(stream)) == EOF && (b = fgetc(stream2)) == EOF) {
        printf("Files are identical\n");
        return 0;
    }

    if (feof(stream)) {
        printf("EOF on %s\n", argv[1]);
        return 0;
    } else if (feof(stream2)) {
        printf("EOF on %s\n", argv[2]);
        return 0;
    }

    fclose(stream);
    fclose(stream2);

    return 0;
}