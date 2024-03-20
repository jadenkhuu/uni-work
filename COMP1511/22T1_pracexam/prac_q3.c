#include <stdio.h>
#include <stdlib.h>

#define TRUE 1
#define FALSE 0

int main(void) {

    int integer;
    int valid = TRUE;
    int array[1000] = {};
    int i = 0;


    while (valid == TRUE) {

        scanf("%d", &integer);
        if (integer != 0) {
            array[i] = integer;

            i++;
        } else {
            valid = FALSE;
        }
        
    }

    int j = 0;
    while (j < i) {
        printf("%d ", array[j]);

        j = j + 2;
    }
    
    int l = 1;
    while (l < i) {
        printf("%d ", array[l]);

        l = l + 2;
    }

    printf("\n");

    return 0;
}
