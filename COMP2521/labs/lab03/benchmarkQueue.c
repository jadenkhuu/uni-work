
#include <stdio.h>
#include <stdlib.h>

#include "Queue.h"

int main(void) {
	
	Queue q = QueueNew();
	
	// TODO
	// Write a benchmark test to demonstrate the difference in
	// efficiency between ArrayQueue and CircularArrayQueue

	Item it = 1;
	// Enqueue loop
	for (int i = 0; i < 9999; i++) {
		QueueEnqueue(q, it);
	}


	// Dequeue loop
	for (int i = 0; i < 9999; i++) {
		QueueDequeue(q);
	}


	QueueFree(q);
}

