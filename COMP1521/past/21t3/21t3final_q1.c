#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// read two integers and print all the integers which have their bottom 2 bits set.

int main(void) {
    int x, y;

    scanf("%d", &x);
    scanf("%d", &y);

    x++; 

    while (x < y) {
        if (((x & 1) == 1) && (x & (1 << 1)) == (1 << 1)) {
            printf("%d\n", x);
        }
        x++;
    }

    return 0;
}
