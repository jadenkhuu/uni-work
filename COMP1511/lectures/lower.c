// Demonstrating how to play with characters in C
// Program converts an upper case letter to a lower case letter

// Jaden Khuu z5416824

// Problem: A user inputs an upper case letter, and wants 
// the computer to output the same letter, but in lower case.
// Possibilities/Breakpoints?
// Eg. input is 'C' (67), and we want to output 'c' (99)
// Solution: - 'A' to find out the distance from the first of the alphabet and then we add that distance to little 'a'

#include <stdio.h>

int main(void){

    char upper_letter;
    char lower_letter;
    int scanf_return;

    printf("Please enter an upper case letter: ");
    scanf_return = scanf(" %c", &upper_letter);
    printf("Scanf() has returned %d\n", scanf_return);
    
    if (scanf_return != 1) {
        printf("No input was taken in, the program will now exit\n");
        return 1; 
    }
    
    if (upper_letter >= 'A' && upper_letter <= 'Z') {
        lower_letter = upper_letter - 'A' + 'a';
        printf("The lower case letter is %c\n", lower_letter);
    } else {
        printf("You did not enter a capital letter\n");
        return 1;
    }   // return 1 - means that program finished running but had problems
    

// return 0 - means that program has finished running with no problems
    return 0;
}




