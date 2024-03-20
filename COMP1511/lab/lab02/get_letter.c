// Lab02 Exercise - Print Letters, Given Their Numbers
// get_letter.c
//
// This program was written by Jaden Khuu (z5416824)
// on 23/02/22
// 
// A simple program to print letters, given their numbers


#include <stdio.h>

int main(void) {

    char upper_lower;
    int ascii_value = 0;
    int integer;
    int upper = 64;
    int lower = 96;
    int sum; 
    
    
    // 1. Upper or lower case letter 

    printf("Uppercase: ");
    scanf("%c", &upper_lower);
  
    if (upper_lower == 'y') {
        ascii_value = ascii_value + upper;
    } else if (upper_lower == 'n') {
        ascii_value = ascii_value + lower;
    } else {
        printf("You need to enter 'y' or 'n'\n");
        printf("Exiting the program with error code 1\n");
        return 1;
    }

    // 2. index into letter
    
    printf("Index: ");
    scanf("%d", &integer);

    
    if (integer < 1 || integer > 26) {
        printf("You need to enter a number between 1 and 26 inclusive\n");
        printf("Exiting the program with error code 2\n");
      
    }
    
    else {
        sum = ascii_value + integer;
        printf("The letter is %c\n", sum);   
    }
    
    return 0;
}
