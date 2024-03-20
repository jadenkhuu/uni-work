#include <stdio.h>
#include <stdlib.h>

struct coordinate {
    int x;
    int y;
};

// Return the total number of coordinates where the y coordinate is a 
// multiple of the x coordinate 
int count_multiple(int size, struct coordinate array[size]){
    //TODO: Insert your code in the function here and don't forget to change
    // the return! 
    int count = 0;

    int i = 0;
    while (i < size) {
        if (array[i].y % array[i].x == 0) {
            count = count + 1;
        }
        i++;
    }
    return count;   
}

// This is a simple main function which could be used
// to test your count_multiple function.
// It will not be marked.
// Only your count_multiple function will be marked.

#define TEST_ARRAY_SIZE 5

int main(void) {
    struct coordinate test_array[TEST_ARRAY_SIZE] = {
        { .x = 3, .y = 20},
        { .x = 10, .y = 20},
        { .x = 3, .y = 30},
        { .x = 20, .y = 10},
        { .x = 5, .y = 50}
    };
    printf("Total of coords where y is multiple of x is %d\n", count_multiple(TEST_ARRAY_SIZE, test_array));
    return 0;
}
