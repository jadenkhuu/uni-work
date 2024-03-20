//Dice checker program
// Problem: A user rolls two dice and tell us the number on each of
// the rolled die. Our program will add the die numbers together and
// check them against a target number that only the program knows. 
// It will then report back whether the total of the dice was higher,
// equal or lower than the secret number.


//1. Scan in the two dice (scanf())
//2. Add the numbers together
//3. Check the sum against the target numbers (#define)
//4. Output greater or less than (printf())

#include<stdio.h>

#define TARGET 9

int main(void){
    int die_one;
    int die_two;
    int sum = 0;
    int scanf_return;
    
    
    printf("Enter the dice rolls (two numbers): ");
    
    scanf_return = scanf("%d %d", &die_one, &die_two);
    printf("Scanf() has returned: %d\n", scanf_return);
    
    if (scanf_return != 2) {
        printf("You have not entered two numbers for the dice!\n The program will now exit\n");
        return 1;
    }

    sum = die_one + die_two;

//if else chain to decide what text to print
    if (sum > TARGET) {
        printf("The sum of the dice is greater than the target number!\n");
    } else if (sum < TARGET) {
        printf("The sum of the dice is less than the target number!\n");
    } else {
        printf("You have guessed the target number!\n");
    }

    return 0;
}



