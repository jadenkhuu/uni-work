// Assignment 0 - CS Bowling
// cs_bowling.c 
//
// This program was written by Jaden Khuu (z5416824)
// on 25/02/22
//
// This is a simple program to calculate bowling scores

#include <stdio.h>

int main(void) {

    int bowl1;
    int bowl2; 
    int fscore;

    printf("Welcome to CS Bowling!\n");
    
    // Bowl 1 - Scan in
    printf("Frame 1, Bowl 1: ");
        scanf("%d", &bowl1);
       
        // Strike Bowl 1
    if (bowl1 == 10) {
        printf("Score for Frame: %d\n", bowl1);
        printf("Strike! Bonus for this frame is next two rolls.\n"); 
        
        return 1;
    }
    
        // Invalid Bowl 1
    if (bowl1 < 0 || bowl1 > 10) {
        printf("Bowl 1 invalid!\n");
        
        bowl1 = 0; 
    } 
    
    
    // Bowl 2 - Scan in
    printf("Frame 1, Bowl 2: ");
        scanf("%d", &bowl2);
    
    
        // Invalid Bowl 2
    if (bowl2 < 0 || bowl2 > 10) {
        
        printf("Bowl 2 invalid!\n");
        bowl2 = 0;
    }
    
        // Invalid Bowl 2 (Testing if > 10)
    if (bowl1 + bowl2 > 10) {
        
        printf("Bowl 2 invalid!\n");
        bowl2 = 0;
    }
    
    
    // Spare
    if (bowl1 + bowl2 == 10) {
        
        fscore = bowl1 + bowl2;
       
        printf("Score for Frame: %d\n", fscore);
        printf("Spare! Bonus for this frame is next roll.\n");
        
        return 1;
    }
    
    if (bowl1 + bowl2 < 10) {
        
        fscore = bowl1 + bowl2;
        printf("Score for Frame: %d\n", fscore); 
    }
    
    
    return 0;
}



// WITH ELSE INSIDE (MORE COMPLICATED)

// Assignment 0 - CS Bowling
// cs_bowling.c 
//
// This program was written by Jaden Khuu (z5416824)
// on 25/02/22
//
// This is a simple program to calculate bowling scores

#include <stdio.h>

int main(void) {

    int bowl1;
    int bowl2; 
    int fscore;

    printf("Welcome to CS Bowling!\n");
    
    // Bowl 1 - Scan in
    printf("Frame 1, Bowl 1: ");
        scanf("%d", &bowl1);
       
        // Strike Bowl 1
    if (bowl1 == 10) {
        printf("Score for Frame: %d\n", bowl1);
        printf("Strike! Bonus for this frame is next two rolls.\n"); 
        
        return 1;
    }
    
        // Invalid Bowl 1
    if (bowl1 < 0 || bowl1 > 10) {
        printf("Bowl 1 invalid!\n");
        
        bowl1 = 0; 
    } 
    
    
    // Bowl 2 - Scan in
    printf("Frame 1, Bowl 2: ");
        scanf("%d", &bowl2);
    
    
        // Invalid Bowl 2
    if (bowl2 < 0 || bowl2 > 10) {
        
        bowl2 = 0;
        
        fscore = bowl1 + bowl2;
        
        printf("Bowl 2 invalid!\n");
        printf("Score for Frame: %d\n", fscore);
        
        return 1;
    }
    
        // Invalid Bowl 2 (Testing if > 10)
    if (bowl1 + bowl2 > 10) {
    
        bowl2 = 0;
        
        fscore = bowl1 + bowl2;
        
        printf("Bowl 2 invalid!\n");
        printf("Score for Frame: %d\n", fscore);
        
        return 1;
    } 
    // Normal score for frame
    else if (bowl1 + bowl2 < 10) {
        fscore = bowl1 + bowl2;
        printf("Score for Frame: %d\n", fscore);
    }
    
    
    // Spare
    if (bowl1 + bowl2 == 10) {
        
        fscore = bowl1 + bowl2;
       
        printf("Score for Frame: %d\n", fscore);
        printf("Spare! Bonus for this frame is next roll.\n");
    }
    
    
    return 0;
}
