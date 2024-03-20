// Lab03 - Count Up/Down
// count_up_down.c
//
// This program was written by Jaden Khuu (z5416824)
// on 02/03/22
//
// Counts up to 'n' value if positive or down to 'n' value if negative

#include <stdio.h>

int main(void) {

    int number = 0;
    
    printf("Enter number: ");
    scanf("%d", &number);
    
    int count = 0;
    
    printf("%d\n", count);
    
    while (count != number) {
        
        if (number > 0) {
            count = count + 1;
            
            printf("%d\n", count);
        }
        
        if (number < 0) {
            count = count - 1;
            
            printf("%d\n", count);
        }
           
    }
    
    return 0;
}
