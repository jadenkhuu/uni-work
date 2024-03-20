// Following lecture to define constants

#include <stdio.h>

#define PI 3.1415
#define MEANING_OF_LIFE 42
#define FIRST_LETTER 'A'

int main(void){

    int number1 = 50;
    int number2 = 45;
    
    // < > == <= >= != 
    // = assigning value
    // == checking equivalence
    
    if (MEANING_OF_LIFE > number2) {
        printf("%d is not equal to %d\n", MEANING_OF_LIFE, number2);
    } else if (MEANING_OF_LIFE < number2); {
        printf("%d is less than %d\n", MEANING_OF_LIFE, number2);
    
    }
    
    
    
    /*
    int remainder;
    remainder = MEANING_OF_LIFE % number1;
    
    printf("The result is %d\n", remainder);
    */    
     
    return 0;
}

