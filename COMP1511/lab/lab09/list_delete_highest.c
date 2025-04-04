// list_delete_highest
// list_delete_highest.c
//
// This program was written by Jaden Khuu (z5416824)
// on 18/04/22
//
// deletes the highest number in the linked list

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#define MAX_LIST_LEN 100

struct node {
    struct node *next;
    int          data;
};

struct node *delete_highest(struct node *head);
struct node *array_to_list(int len, int array[]);
void print_list(struct node *head);
static void free_list(struct node *head);

// DO NOT CHANGE THIS MAIN FUNCTION

int main(void) {
    // scan in quantity of numbers
    printf("Total numbers in list: ");
    int list_size;
    scanf(" %d", &list_size);

    // scan numbers into array
    int n_read = 0;
    int numbers_list[MAX_LIST_LEN] = {0};
    while (n_read < list_size && scanf(" %d", &numbers_list[n_read])) {
        n_read++;
    }

    // create linked list from inputs
    struct node *head = array_to_list(n_read, numbers_list);

    struct node *new_head = delete_highest(head);
    print_list(new_head);
    
    free_list(new_head);

    return 0;
}


//
// Delete the node(s) in the list that contain the highest value
// The deleted node(s) are freed.
// The head of the list is returned.
//
struct node *delete_highest(struct node *head) {

    // PUT YOUR CODE HERE (change the next line!)
    int highest = 0;
    struct node *current = head;
    struct node *temporary = NULL;

    // Determine highest number

    if (head == NULL) {
        return head;
    }
    if (head->next == NULL) {
        free(head);
        return NULL;
    }

    highest = current->data;
    while (current != NULL) {
        if (highest < current->data) {
            highest = current->data;
        }
        current = current->next;
    }

    current = head;

    while (current->next != NULL) {
        if (current->next->data == highest) {
            temporary = current->next->next;
            free(current->next);

            current->next = temporary;
            current = head;
        }
        else if (current->data == highest) {
            
            temporary = current->next;
            free(current);  
            head = temporary; 
            current = head; 
        }
        else if (current->next->data == highest 
        && current->next->next == NULL) {
            
            free(current->next); 
            current->next = NULL; 
        }
        else {
            current = current->next; 
        }
    }
    if (current->data == highest) {
        free(current);
        return NULL;
    }

    return head;
}

// DO NOT CHANGE THIS FUNCTION
// Free all nodes in the supplied list
static void free_list(struct node *head) {
    if (head != NULL) {
        free_list(head->next);
        free(head);
    }
}

// DO NOT CHANGE THIS FUNCTION
// create linked list from array of ints
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

// DO NOT CHANGE THIS FUNCTION
// print linked list
void print_list(struct node *head) {
    printf("[");    
    struct node *n = head;
    while (n != NULL) {
        // If you're getting an error here,
        // you have returned an invalid list
        printf("%d", n->data);
        if (n->next != NULL) {
            printf(", ");
        }
        n = n->next;
    }
    printf("]\n");
}