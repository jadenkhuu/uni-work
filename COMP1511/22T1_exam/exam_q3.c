// Starter code for exam_q3.

#include <stdio.h>
#define MAX_ARRAY_LENGTH 10000
#define TRUE 1
#define FALSE 0


int main(void) {
    // Add code to the main function to implement exam_q3!
    int array[MAX_ARRAY_LENGTH] = {};
    int num_input;
    int repeater = FALSE;
    
    int i = 0;
    while (repeater == FALSE) {
        scanf("%d", &num_input);

        int j = 0;
        while (j < i) {
            if (num_input == array[j]) {
                repeater = TRUE;
            }
            j++;
        }

        if (repeater == TRUE) {
            printf("%d is repeated\n", num_input);
            return 0;
        }

        array[i] = num_input;

        i++;
    }
    return 0;
}
