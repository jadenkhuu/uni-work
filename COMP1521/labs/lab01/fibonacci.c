#include <stdio.h>
#include <stdlib.h>

#define SERIES_MAX 30
int fib(int i);

int main(void) {

    int num;
    while (scanf("%d", &num) != EOF) {
        num = fib(num);
        printf("%d\n", num);
    }
}

int fib(int i) {

    if (i == 0 || i == 1) {
        return i;
    } 
    int final = fib(i - 1) + fib(i - 2);

    return final;
}