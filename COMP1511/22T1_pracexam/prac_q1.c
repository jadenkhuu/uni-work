#include <stdio.h>
#include <stdlib.h>


struct coordinate {
    int x;
    int y;
};


// Return the sum of all negative x coordinates 
// and all positive y coordinates as a struct 
struct coordinate sum_coords(int size, struct coordinate array[size]){

    struct coordinate result;
    result.x = 0;
    result.y = 0;

    int i = 0;
    while (i < size) {

        if (array[i].x < 0) {
            result.x = result.x + array[i].x;
        }
        if (array[i].y > 0) {
            result.y = result.y + array[i].y;
        }

        i++;
    }

    // PUT YOUR CODE HERE AND CHANGE THE RETURN VALUE
    return result;   
}

// This is a simple main function which could be used
// to test your sum_cood function.
// It will not be marked.
// Only your sum_cood function will be marked.

#define TEST_ARRAY_SIZE 3

int main(void) {
    struct coordinate test_array[TEST_ARRAY_SIZE] = {
        { .x = -3, .y = 20},
        { .x = 10, .y = -20},
        { .x = -5, .y = 50}
    };
    struct coordinate result = sum_coords(TEST_ARRAY_SIZE, test_array);
    printf("(%d, %d)\n", result.x, result.y);

    return 0;
}