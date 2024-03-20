// Reverse an array
// reverse_array.c
//
// This program was written by Jaden Khuu (z5416824)
// on 11/03/22
//
// reads integers line by line, and when it reaches the end of input,
// prints those integers in reverse order, line by line.

#include <stdio.h>

int main(void) {

    int array[100] = {};


    printf("Enter numbers forwards:\n");

    int i = 0;    

    while (scanf("%d", &array[i]) != EOF) {
        i++;
    }

    printf("Reversed:\n");

    i = i - 1;

    while (i > -1) {
        printf("%d\n", array[i]);

        i--;
    }


    return 0;
}