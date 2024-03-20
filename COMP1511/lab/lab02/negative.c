// Lab02 Exercise - Don't Be So Negative
// Negative.c
//
// This program was written by Jaden Khuu (z5416824)
// on 23/02/22
// 
// A simple program to tell if an integer is positive, negative, or zero


#include <stdio.h>

int main(void) {

    int integer;

    scanf("%d", &integer);
    
    if (integer == 0) {
        printf("You have entered zero.\n");    
    } 
    
    if (integer < 0) {
        printf("Don't be so negative!\n");
    } 
    
    if (integer > 0) {
        printf("You have entered a positive number.\n");
    }
    
    return 0;
}
