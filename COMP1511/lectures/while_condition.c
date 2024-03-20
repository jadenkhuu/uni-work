// Lecture 4


#include <stdio.h>

int main(void) {
    
    int scoops;
    
    
    // control variable - initialise
    int sum = 0;
    
    while (sum <= 20) {
        printf("How many ice cream scoops did you eat just now?: ");
        scanf("%d", &scoops);
        
        sum = sum + scoops;
    
    }
    printf("You have eaten too much ice cream, have a break!\n");


    return 0;
}



