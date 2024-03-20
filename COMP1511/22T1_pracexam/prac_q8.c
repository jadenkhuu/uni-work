#include <stdio.h>


int main(void) {
	// PUT YOUR CODE HERE
	return 0;
}


// You may optionally adapt the code below for this question.
//
// bubblesort rearranges an array of ints to be non-decreasing order.
// for example if  array a = {4, 3, 8, 9, 2, 3}
// after bubblesort(a, 6)  a will contain {2, 3, 3, 4, 8, 9}
// 
// You will need to modify the code below if you wish to order an arrays of a type other than int

void bubblesort(int array[], int length) {
    int swapped = 1;
    while (swapped) {
        swapped = 0;
        int i = 1;
        while (i < length) {
            if (array[i] < array[i - 1]) {
                int tmp = array[i];
                array[i] = array[i - 1];
                array[i - 1] = tmp;
                swapped = 1;
            }
            i = i + 1;
        }
    }
}

