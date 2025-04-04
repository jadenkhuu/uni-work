#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

struct node {
    struct node *next;
    int          data;
};

int count_favourite(struct node *head);
struct node *strings_to_list(int len, char *strings[]);

// DO NOT CHANGE THIS MAIN FUNCTION

int main(int argc, char *argv[]) {
    // create linked list from command line arguments
    struct node *head = strings_to_list(argc - 1, &argv[1]);

    int result = count_favourite(head);
    printf("%d\n", result);

    return 0;
}


// Return the number of elements divisible by 17 in the linked list
int count_favourite(struct node *head) {

    // PUT YOUR CODE HERE (change the next line!)
    struct node *current = head;
    int favourite = 0;

    while (current != NULL) {

        if (current->data % 17 == 0) {
            favourite = favourite + 1;
        }

        current = current->next;
    }

    return favourite;

}


// DO NOT CHANGE THIS FUNCTION

// create linked list from array of strings
struct node *strings_to_list(int len, char *strings[]) {
    struct node *head = NULL;
    for (int i = len - 1; i >= 0; i = i - 1) {
        struct node *n = malloc(sizeof (struct node));
        assert(n != NULL);
        n->next = head;
        n->data = atoi(strings[i]);
        head = n;
    }
    return head;
}