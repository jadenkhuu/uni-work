// Lab03 - Wondrous
// wondrous.c
//
// This program was written by Jaden Khuu (z5416824)
// on 04/03/22
//
// Prints out the wondrous numbers of n as a graph of asterisks

#include <stdio.h>

int main(void) {

    int integer;
    int counter = 0;

    printf("What number would you like to see: ");
    scanf("%d", &integer);
    
    while (integer != 1) {
        
        while (counter < integer) {
            printf("*");

            counter = counter + 1;
        }
        
        if (integer % 2 == 0) {
            integer = integer / 2;                     
        }
        
        else if (integer % 2 != 0) {
            integer = (3*integer) + 1;
        }
        
        printf("\n");
        
        counter = 0;
    }
    
    
    return 0;
}
