// list_get_middle
// list_get_middle.c
//
// This program was written by Jaden Khuu (z5416824)
// on 18/04/22
//
// Converts inputs into a list, returns and prints the middle
// value of the list


#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

struct node {
    struct node *next;
    int          data;
};

int get_middle(struct node *head);
struct node *array_to_list(int len, int array[]);

// DO NOT CHANGE THIS MAIN FUNCTION
#define MAX_INIT_LIST_LEN 100
int main(void) {
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

    int result = get_middle(head);
    printf("%d\n", result);

    return 0;
}


// Return middle element of a linked list
// if list contains [6,7,8,9,10]  8 is returned
// if a list has even number of elements, first of middle two elements returned
// if list contains [1,2,3,4] 2 is returned
// list can not be empty
int get_middle(struct node *head) {

    struct node *current = head;
    struct node *middle = head;
    int count = 0;
    int middle_num;

    while (current->next != NULL) {
        current = current->next;
        count++;
    }

    count = count + 1;

    if (count % 2 == 1) {
        count = count + 1;
        count = count / 2;
    }
    if (count % 2 == 0) {
        count = count / 2;
    }

    int i = 1;
    while (i < count) {
        i++;
        middle = middle->next;
    }

    middle_num = middle->data;

    // PUT YOUR CODE HERE (change the next line!)
    return middle_num;

}


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