// Lab02 Exercise - Dice Range
// dice_range.c
//
// This program was written by Jaden Khuu (z5416824)
// on 25/02/22
// 
// A simple program the number of sides on a dice and how many are being rolled

#include <stdio.h>

int main(void) {

    int sides;
    int rolls;
    double lowrange;
    double highrange;
    double avg;
    
// 1. Scan in values of sides and rolls
   
    printf("Enter the number of sides on your dice: ");
    scanf("%d", &sides);

    printf("Enter the number of dice being rolled: ");
    scanf("%d", &rolls);

    if (rolls <= 0 || sides <= 0) {
        printf("These dice will not produce a range.\n");
        return 1;
    }

// 2. Calculate and print range of dice

    lowrange = rolls;
    highrange = rolls * sides;
    
    printf("Your dice range is %.0lf to %.0lf.\n", lowrange, highrange);
   
// 3. Calculate and print average
    
    avg = (highrange + lowrange) / 2;
    
    printf("The average value is %lf\n", avg);

    return 0;
}




