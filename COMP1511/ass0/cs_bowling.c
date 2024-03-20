// Assignment 0 - CS Bowling
// cs_bowling.c 
//
// This program was written by Jaden Khuu (z5416824)
// on 25/02/22
//
// This is a simple program to calculate bowling scores per frame


#define MAX_FRAME 10
#define TRUE 1
#define FALSE 0


#include <stdio.h>

int main(void) {

    printf("Welcome to CS Bowling!\n");
 
    int bowl1 = 0;
    int bowl2 = 0; 
    int bonus_bowl = 0;
    
    int frame_number = 1;
    int frame_score = 0;
    int total_score = 0;  
    
    int spare = FALSE;
    int strike = FALSE;
    
    
    while (frame_number <= MAX_FRAME) {
        
        //Scan in Bowl 1
        printf("Frame %d, Bowl 1: ", frame_number);
        scanf("%d", &bowl1);
        
        // Invalid Bowl 1 
        if (bowl1 < 0 || bowl1 > 10) {
            printf("Bowl 1 invalid!\n");
            bowl1 = 0; 
        }
        
        // Condition for spare bonus
        if (spare == TRUE) {
            total_score = total_score + bowl1;

            spare = FALSE;
        }
        
        // Scan in Bowl 2 if Bowl 1 is not a strike
        if (bowl1 != 10) {
            
            printf("Frame %d, Bowl 2: ", frame_number);
            scanf("%d", &bowl2);  
            
            // Invalid Bowl 2
            if (bowl2 < 0 || bowl2 > 10) {
                printf("Bowl 2 invalid!\n");
                bowl2 = 0;
            }
            
            // Invalid Bowl 2 (total > 10)
            if (bowl1 + bowl2 > 10) {
                printf("Bowl 2 invalid!\n");
                bowl2 = 0;
            }
            
            // Spare
            if (bowl1 + bowl2 == 10) {
        
                frame_score = bowl1 + bowl2;
       
                spare = TRUE; 
       
                printf("Score for Frame: %d\n", frame_score);
                printf("Spare! Bonus for this frame is next roll.\n");       
            }
            
            // Normal score
            if (bowl1 + bowl2 < 10) {
        
                frame_score = bowl1 + bowl2;

                printf("Score for Frame: %d\n", frame_score); 
            }
            
            // Condition for strike bonus
            if (strike == TRUE) {
            
                total_score = total_score + bowl1 + bowl2;
            
                strike = FALSE;
            }
            
        } 
        
        // If Bowl 1 is a strike, skip scanning in Bowl 2
        else if (bowl1 == 10) {
            
            frame_score = 10;
            strike = TRUE;
            
            printf("Score for Frame: %d\n", frame_score);
            printf("Strike! Bonus for this frame is next two rolls.\n"); 
        }
                       
        frame_number = frame_number + 1;

        total_score = total_score + frame_score;
    }
    
    // Condition for bonus bowl (Will only consider once frame 10 is finished)
    if (spare == TRUE || strike == TRUE) {
        printf("Bonus Bowl 1: ");
        scanf("%d", &bonus_bowl);
            
        if (bonus_bowl < 0 || bonus_bowl > 10) {    
            bonus_bowl = 0;
            
            printf("Bonus Bowl Invalid!\n");
        }
            
        total_score = total_score + (2*bonus_bowl);
    }        

    printf("Total Score: %d\n", total_score);
    
    return 0;
}    
        
        
        



