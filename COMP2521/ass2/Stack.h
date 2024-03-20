// STACK IMPLEMENTATION 
// Referenced and Adapted from 
// COMP1511 Sasha Vassar Week09 Lecture 15

// This is the implementation file for stack.h
// This implementation makes a stack using linked list

#define MAX 100

struct stack *NewStack(void);

void push(struct stack *s, int item);

int pop(struct stack *s);

int StackSize(struct stack *s);

void FreeStack(struct stack *s);


