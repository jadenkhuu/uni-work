/*
    This file is intentionally provided (almost) empty.
    Remove this comment and add your code.
*/
#include <stdio.h>
#include <stdlib.h>

int main (int argc, char *argv[]) {
    FILE *file1 = fopen(argv[1], "r");
    FILE *file2 = fopen(argv[3], "r");
    if (file1 == NULL) {
        perror(argv[1]);
        return 0;
    }
    if (file2 == NULL) {
        perror(argv[3]);
        return 0;
    }

    fseek(file1, atoi(argv[2]), SEEK_SET);
    fseek(file2, atoi(argv[4]), SEEK_SET);
    
    int c = fgetc(file1);
    int i = fgetc(file2);
    
    if (c == EOF || i == EOF) {
        printf("byte %d in %s and byte %d in %s are not the same\n", atoi(argv[2]), argv[1], atoi(argv[4]), argv[3]);
    } else if (c == i) {
        printf("byte %d in %s and byte %d in %s are the same\n", atoi(argv[2]), argv[1], atoi(argv[4]), argv[3]);
    } else {
        printf("byte %d in %s and byte %d in %s are not the same\n", atoi(argv[2]), argv[1], atoi(argv[4]), argv[3]);
    } 
}