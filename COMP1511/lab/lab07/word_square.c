// Word Square
// word_square.c
//
// This program was written by Jaden Khuu (z5416824)
// on 31/03/22
//
// prompts the user to enter a word, and afterwards, prints that word out
// n amount of times, where n is the length of the word.

#define MAX_LENGTH 1024

#include <stdio.h>
#include <string.h>

int main(void) {

    char word[MAX_LENGTH];
    int length;

    printf("Input word: ");
    
    fgets(word, MAX_LENGTH, stdin);

    length = strlen(word);
    
    printf("\nWord square is: \n");

    int i = 1;
    while (i < length) {
        printf("%s", word);
        i++;
    }
    

    return 0;
}



