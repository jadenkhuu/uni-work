// String to Lower
// string_to_lower.c
//
// This program was written by Jaden Khuu (z5416824)
// on 01/04/2022
//
// takes a string and converts it to lower case 

#include <stdio.h>
#include <ctype.h>
#include <string.h>

void string_to_lower(char *string);

int main(int argc, char *argv[]) {

    char str[1024] = "Hi, mY nAmE iS sPonGEbOb sQuArePanTS.";
    string_to_lower(str);
    printf("%s\n", str);

    return 0;
}

// Convert the characters in `buffer` to lower case
void string_to_lower(char *string) {
    // YOUR CODE GOES HERE!
    
    int length;
    length = strlen(string);

    int i = 0;
    while (i < length) {
        string[i] = tolower(string[i]);
        i++;
    }

}