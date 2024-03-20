// Lab03 - Dice Game
// dice_game.c
//
// This program was written by Jaden Khuu (z5416824)
// on 02/03/22
//
// Reads 10 integers, representing the rolls on a 6-side die. Calculate 
// and print the sum of the dice.

#define MAX_ROLLS 10

#include <stdio.h>

int main(void) {
    
    int counter = 0;
    int roll;
    int dice_sum = 0;
    int bonus = 0;
    
    printf("Enter numbers: ");
    
    while (counter < MAX_ROLLS) {
        scanf("%d", &roll);
        
        if (roll == 1 && bonus == 0) {
            bonus = 1;
        
            dice_sum = dice_sum + roll;
        
        }
        
        else if (roll == 1 && bonus == 1) {
            dice_sum = dice_sum + (2*roll);
        }
    
        else if (roll != 1 && bonus == 1) {
            dice_sum = dice_sum + (2*roll);
            bonus = 0;
            
        }
    
        else {
            dice_sum = dice_sum + roll;
        
        }
        
        counter = counter + 1;
    }
    
    printf("The sum of dice rolls is %d.\n", dice_sum);
    
    return 0;
}
