// list_increasing
// list_increasing.c
//
// This program was written by Jaden Khuu (z5416824)
// on 18/04/22
//
// checks if a list of integers is increasing

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

struct node {
    struct node *next;
    int          data;
};

int increasing(struct node *head);
struct node *array_to_list(int len, int array[]);

#define MAX_INIT_LIST_LEN 100

// DO NOT MODIFY THIS FUNCTION!
int main() {
    // Need to read in a number of ints into an array
    printf("How many numbers in initial list?: ");
    int list_size = 0;
    scanf("%d", &list_size);
    int initial_elems[MAX_INIT_LIST_LEN] = {0};
    int n_read = 0;
    while (n_read < list_size && scanf("%d", &initial_elems[n_read])) {
        n_read++;
    }

    // create linked list from first set of inputs
    struct node *head = NULL;
    if (n_read > 0) {
        // list has elements
        head = array_to_list(n_read, initial_elems);
    }

    int result = increasing(head);
    printf("%d\n", result);

    return 0;
}


// return 1 if values in a linked list are in increasing order,
// return 0, otherwise

int increasing(struct node *head) {
    int results = 1;

    struct node *current = head;

    if (current == NULL) {
        return results;
    }

    while (current->next != NULL) {
        
        int prev_number = current->data;
        
        current = current->next;
        
        if (current->data > prev_number) {
            results = 1;
        }
        else {
            results = 0;
            return results;
        }
    }


    // PUT YOUR CODE HERE (change the next line!)
    return results;

}


// DO NOT CHANGE THIS FUNCTION

// DO NOT CHANGE THIS FUNCTION
// create linked list from array of strings
struct node *array_to_list(int len, int array[]) {
    struct node *head = NULL;
    int i = len - 1;
    while (i >= 0) {
        struct node *n = malloc(sizeof (struct node));
        assert(n != NULL);
        n->next = head;
        n->data = array[i];
        head = n;
        i -= 1;
    }   
    return head;
}