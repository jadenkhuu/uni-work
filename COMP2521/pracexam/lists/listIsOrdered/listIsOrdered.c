
#include "list.h"

#define INCREASING 1
#define DECREASING 0

bool listIsOrdered(List l) {
	// TODO
	if (l->head == NULL) {
		return true;
	} 
	if (l->head->next == NULL) {
		return true;
	}

	bool flag = true; 
	int order;

	Node current = l->head;
	
	if (current->value < current->next->value) {
		order = INCREASING;
	} else {
		order = DECREASING;
	}

	while (current->next != NULL) {

		if (order == INCREASING && current->value > current->next->value) {
			flag = false;
		}
		if (order == DECREASING && current->value < current->next->value ) {
			flag = false;
		}

		current = current->next;
	}

	return flag;
}

