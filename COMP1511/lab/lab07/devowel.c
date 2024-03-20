// Devowelling Text
// devowel.c
//
// This program was written by Jaden Khuu (z5416824)
// on 31/03/22
//
// Reads characters and writes the same characters besides lower case vowels

#include <stdio.h>

void check_vowel(char letter_input);

int main(void) {

    char letter;

    while (scanf("%c", &letter) != EOF) {
        check_vowel(letter);
    }

    return 0;
}

void check_vowel(char letter_input){
    
    if (letter_input == 'a' ||
    letter_input == 'e' ||
    letter_input == 'i' ||
    letter_input == 'o' ||
    letter_input == 'u') {
        
    }
    else {
        printf("%c", letter_input);
    }
}