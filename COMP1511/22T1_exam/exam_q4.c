#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

struct node {
    struct node *next;
    int          data;
};

struct node *insert_second_last(int value, struct node *head);
struct node *read_list(void);
void print_list(struct node *head);

// Insert the given 'value' node as a second last node of the
// list
struct node *insert_second_last(int value, struct node *head) {
    // TODO: Change this function.
    
    struct node *new_node = malloc(sizeof(struct node));
    new_node->data = value;
    new_node->next = NULL;

    struct node *current = head;

    if (current == NULL) {
        return new_node;
    }

    if (current->next == NULL) {
        new_node->next = current;
        return new_node;
    }
    
    while (current->next->next != NULL) {
        current = current->next;
    }

    new_node->next = current->next;
    current->next = new_node;

    return head;
}

//
// DO NOT CHANGE ANYTHING BELOW THIS COMMENT
//

int main(int argc, char *argv[]) {
    int value;
    struct node *head = read_list();

    printf("Enter the value to be inserted: ");
    scanf("%d", &value);

    struct node *new_head = insert_second_last(value, head);
    print_list(new_head);

    return 0;
}


// Read linked list from standard input,
// stopping when -1 is read
struct node *read_list(void) {
    printf("Enter the number of elements in your list: ");
    int num_elements;
    scanf("%d", &num_elements);

    if (num_elements <= 0) {
        return NULL;
    }

    printf("Enter your list: ");
    struct node *head = NULL;
    struct node *tail = NULL;
    int num;
    while (num_elements-- > 0 && scanf("%d", &num) == 1 && num != -1) {
        struct node *new = malloc(sizeof(struct node));
        new->data = num;
        new->next = NULL;
        if (head == NULL) {
            head = new;
            tail = new;
        } else {
            tail->next = new;
            tail = new;
        }
    }
    return head;
}

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
