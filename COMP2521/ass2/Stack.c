// STACK IMPLEMENTATION 
// Referenced and Adapted from 
// COMP1511 Sasha Vassar Week09 Lecture 15

// This is the implementation file for stack.h
// This implementation makes a stack using linked list

#include <stdio.h>
#include <stdlib.h>
#include "Stack.h"

struct stack {
   int size;
   struct node *top;
};

struct node {
   int data;
   struct node *next;
};

struct stack *NewStack(void) {
    struct stack *new_stack = malloc(sizeof(struct stack));
    
    if (new_stack == NULL){
        printf("There was not enough memory to malloc\n");
        exit(1);
    }
    
    new_stack->size = 0;
    new_stack->top = NULL;
    
    return new_stack;
}

void push(struct stack *s, int item) {
    struct node *new_node = malloc(sizeof(struct node));

    new_node->data = item;
    new_node->next = s->top;

    s->size = s->size + 1;
    s->top = new_node;
    
}

int pop(struct stack *s) {

    if (s->size == 0) {
        printf("There is nothing to pop off the stack, stack is empty\n");
        return 1;
    }

    struct node *popped_head = s->top;
    int popped_number = popped_head->data;
    s->top = s->top->next;
    s->size = s->size - 1;

    free(popped_head);
    return popped_number;
}


int StackSize(struct stack *s) {
    return s->size;
};

void FreeStack(struct stack *s){
    while (s->size != 0){
        pop(s);
    }
    free(s);
}



