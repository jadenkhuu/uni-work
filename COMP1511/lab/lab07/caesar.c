// Encrypting text with a cypher
// caesar.c
//
// This program was written by Jaden Khuu (z5416824)
// on 01/04/2022
//
// A program which reads characters from its input and writes
// the characters to its output encrypted with a Caesar cipher.

#define MAX_VALUE 1024

#include <stdio.h>
#include <string.h>
#include <ctype.h>

int shift_boundaries(int shift);
void encrypt(int shift, char sentence[MAX_VALUE], int i);

int main(void) {

    char sentence[MAX_VALUE];
    int shift;

    scanf("%d ", &shift);
    
    shift = shift_boundaries(shift);

    while (fgets(sentence, MAX_VALUE, stdin) != NULL) {

        int i = 0;
        while (i < strlen(sentence)) {
            
            encrypt(shift, sentence, i);

            i++;
        }
    }   

    return 0;
}

void encrypt(int shift, char sentence[MAX_VALUE], int i) {
    if (sentence[i] >= 'A' && sentence[i] <= 'Z') {

        if (shift + sentence[i] > 'Z') {
            int difference;
            difference = shift - 26;
            sentence[i] = sentence[i] + difference;
        }
        else {
            sentence[i] = sentence[i] + shift;
        }
    }
    if (sentence[i] >= 'a' && sentence[i] <= 'z') {

        if (shift + sentence[i] > 'z') {
            int difference;
            difference = shift - 26;
            sentence[i] = sentence[i] + difference;
        }
        else {
            sentence[i] = sentence[i] + shift;
        }
    }

    printf("%c", sentence[i]);
}

int shift_boundaries(int shift) {
    if (shift > 26) {
        shift = shift % 26;
    }
    if (shift < 0) {
        if (shift < -26) {
            shift = -(shift) % 26;
            shift = 26 - shift;
        }
        else {
            shift = 26 + shift;
        }
    }
    return shift;
}