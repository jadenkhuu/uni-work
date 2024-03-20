// Lab03 - x
// x.c
//
// This program was written by Jaden Khuu (z5416824)
// on 04/03/22
//
// Prints an n by n sized X shape



#include <stdio.h>



int main(void) {
    
    int y = 1;
    int x = 1;
    int size = 0;
    
    printf("Enter size: ");
    scanf("%d", &size);
    
    while (y <= size) {
        
        while (x <= size) {
    
            if (x == y) {
                printf("*");
            }
            
            else if (x + y == 1 + size) {
                printf("*");
            }
            
            else {
                printf("-");
            }
        
        x = x + 1;
        }
        
        printf("\n");
        
        y = y + 1;
        
        x = 1;

    }
    


    return 0;
}
