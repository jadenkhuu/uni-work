// Lecture 4

    #define TARGET 20
    #define LOOP 5


#include <stdio.h>

int main(void) {
    
    int die_one;
    int die_two;
    int sum = 0;
    int scanf_return;
    
    int count = 0;
    
    while (count < LOOP) {
        printf("Enter the dice rolls (two numbers): ");
        scanf_return = scanf("%d %d", &die_one, &die_two);
    
        printf("Scanf() has returned: %d\n", scanf_return);
    
        if (scanf_return != 2) {
            printf("You have not entered two numbers for the dice!\n");
            printf("The program will now exit\n");
            return 1;
        }
    
    sum = sum + die_one + die_two;
    count++; //count = count + 1
    }
    
    if (sum > TARGET) {
        printf("The sum of the dice is greater than the target number!\n");
    } else if (sum < TARGET) {
        printf("The sum of the dice is less than the target number!\n");
    } else {
        printf("You have guessed the target number!\n");
    }

    return 0;
}
