// Create a simple calculator, reading different numbers of integers
// cs_calculator.c 
//
// This program was written by Jaden Khuu (z5416824)
// on 11/03/22
//
// Scans in instructions until EOF and prints the output as specified below

#include <stdio.h>

void squared(int input_number);
void power(int input_number1, int input_number2);

int main(void) {
    
    char instruction;
    int input1 = 0; 
    int input2 = 0;       
    
    printf("Enter instruction: ");
    
    while (scanf(" %c", &instruction) != EOF) {

        if (instruction == 's') {
            scanf("%d", &input1);

            squared(input1);

        }

        else if (instruction == 'p') {
        scanf("%d %d", &input1 , &input2);

        power(input1, input2);
        }

        printf("Enter instruction: ");

    }   

    return 0;
}



void squared(int input_number) {
    int outcome;

    outcome = input_number * input_number;

    printf("%d\n", outcome);


}

void power(int input_number1, int input_number2) {
    int outcome = input_number1;
    int i = 1;

    while (i < input_number2) {
        outcome = outcome * input_number1;

        i++;
    }

    printf("%d\n", outcome);

    
}


