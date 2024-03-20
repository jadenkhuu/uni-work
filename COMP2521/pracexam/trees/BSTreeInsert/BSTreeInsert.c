
#include <stdio.h>
#include <stdlib.h>

#include "BSTree.h"

BSTree BSTreeInsert(BSTree t, int val) {
	// TODO
	if (t == NULL) {
		BSTree new = malloc(sizeof(*new));
		new->value = val;
		new->left = NULL;
		new->right = NULL;
		return new;
	} else if (val < t->value) {
		t->left = BSTreeInsert(t->left, val);
	} else if (val > t->value) {
		t->right = BSTreeInsert(t->right, val);
	}

	return t;
}

