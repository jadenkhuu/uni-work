// Implementation of the Set ADT using a balanced BST
// COMP2521 2 s1 Assignment 1

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "Set.h"
#include "SetStructs.h"

// Own Functions
static struct node *RotateRight(struct node *t);
static struct node *RotateLeft(struct node *t);
static int TreeHeight(struct node *t);
static int Max(int x, int y);


static void TreeFree(struct node *t);
static struct node *TreeInsertAVL(struct node *t, int item);
static int TreeCountSize(struct node *t);
static bool TContains(struct node *t, int item);
static void TreeShow(struct node *t);

static struct node *CopyInsert(struct node *t, struct node *s); 
static void IntersectionInsert(struct node *s1, struct node *s2, Set new);
static void TDiff(struct node *s1, struct node *s2, Set new);
static bool TSubset(struct node *s1, struct node *s2);
static int TFloor(struct node *t, int item);
static int TCeiling(struct node *t, int item);

////////////////////////////////////////////////////////////////////////
// Basic Operations

/**
 * Creates a new empty set
 */
Set SetNew(void) {
	// TODO
	Set s = malloc(sizeof(struct set));
	s->tree = NULL;

	return s;
}

/**
 * Frees all memory associated with the given set
 */
void SetFree(Set s) {
	// TODO
	TreeFree(s->tree);
	free(s);
}
// Referenced from lab exercise BSTree.c
static void TreeFree(struct node *t) {
	if (t == NULL) {
		return;
	}
	else { 
		TreeFree(t->left);
		TreeFree(t->right);
		free(t);
	}
	
}
/**
 * Inserts an item into the set
 */
void SetInsert(Set s, int item) {
	// TODO
	if (SetContains(s, item) == false) {
		s->tree = TreeInsertAVL(s->tree, item);
	}
}
// Some parts of TreeInsertAVL() are referenced from
// https://www.geeksforgeeks.org/avl-tree-set-1-insertion/
// Including the use of a Max function and TreeHeight function
static struct node *TreeInsertAVL(struct node *t, int item) {
	// Create new node if tree is NULL
	if (t == NULL) {
		struct node *new_node = (struct node *)
    	malloc(sizeof(struct node));
		new_node->item = item;
		new_node->left = NULL;
		new_node->right = NULL;
		new_node->height = 1;
		return new_node;
	} 
	else if (item == t->item) {
		return t;
	}

	if (item < t->item) {
		t->left = TreeInsertAVL(t->left, item);
	} else if (item > t->item) {
		t->right = TreeInsertAVL(t->right, item);
	} 
	int Lheight = TreeHeight(t->left);
	int Rheight = TreeHeight(t->right);

	t->height = 1 + Max(Lheight, Rheight);

	if (Lheight - Rheight > 1) {
		if (item > t->left->item) {
			t->left = RotateLeft(t->left);
		}
		t = RotateRight(t);
	} else if (Rheight - Lheight > 1)  {
		if (item < t->right->item) {
			t->right = RotateRight(t->right);
		}
		t = RotateLeft(t);
	}
	return t;
} 
// Rotations functions were referenced from week 3 slides and
// https://www.programiz.com/dsa/avl-tree
// Adjusted slightly to store and update tree height in each node
static struct node *RotateRight(struct node *t) {
	struct node *x = t->left;
  	struct node *temp = x->right;
  	x->right = t;
  	t->left = temp;

  	t->height = Max(TreeHeight(t->left), TreeHeight(t->right)) + 1;
  	x->height = Max(TreeHeight(x->left), TreeHeight(x->right)) + 1;

  	return x;
}
static struct node *RotateLeft(struct node *t) {
	struct node *y = t->right;
  	struct node *temp = y->left;
  	y->left = t;
	t->right = temp;

  	t->height = Max(TreeHeight(t->left), TreeHeight(t->right)) + 1;
  	y->height = Max(TreeHeight(y->left), TreeHeight(y->right)) + 1;

  	return y;
}
static int TreeHeight(struct node *t) {
	if (t == NULL) {
		return 0;
	}
	return t->height;
}
static int Max(int x, int y) {
	return (x > y) ? x : y;
}

/**
 * Returns the number of elements in the set
 */
int SetSize(Set s) {
	// TODO
	return TreeCountSize(s->tree);
}
// Referenced from Lab exercise BSTree.c
static int TreeCountSize(struct node *t) {
	if (t == NULL) {
		return 0;
	} else {
		return 1 + TreeCountSize(t->left) + TreeCountSize(t->right);
	}
}

/**
 * Returns true if the set contains the given item, and false otherwise
 */
bool SetContains(Set s, int item) {
	// TODO
	return TContains(s->tree, item);
}
// Referenced from Lab exercise BSTree.c
static bool TContains(struct node *t, int item) {
	if (t == NULL) {
		return false;
	} else if (item < t->item) {
		return TContains(t->left, item);
	} else if (item > t->item) {
		return TContains(t->right, item);
	} else {
		return true;
	}
}

/**
 * Prints the given set in the format
 * {elem1, elem2, elem3}
 */
void SetShow(Set s) {
	// TODO
	printf("{");
	TreeShow(s->tree);

	if (TreeCountSize(s->tree) == 0) {
		printf("}");
	} else {
		printf("\b\b}");
	}
}
// Referenced from lecture slides 3.2
static void TreeShow(struct node *t) { 
	if (t == NULL) {
		return;	
	}
	TreeShow(t->left);
	printf("%d, ", t->item);
	TreeShow(t->right);
}


////////////////////////////////////////////////////////////////////////
// Further Operations

/**
 * Returns a new set representing the new of the two sets
 */
Set SetUnion(Set s1, Set s2) {
	// TODO
	Set new = SetNew();

	// Add first tree to new set
	new->tree = CopyInsert(new->tree, s1->tree);
	// Add second tree to new set
	new->tree = CopyInsert(new->tree, s2->tree);

	return new;
}
static struct node *CopyInsert(struct node *t, struct node *s) {
	if (s == NULL) {
		return t;
	} 
	// Only insert if item doesnt already exist in tree
	if (TContains(t, s->item) == false) {
		t = TreeInsertAVL(t, s->item);
	}
	// Recusion to traverse tree
	t = CopyInsert(t, s->left);
	t = CopyInsert(t, s->right);

	return t;
}


/**
 * Returns a new set representing the intersection of the two sets
 */
Set SetIntersection(Set s1, Set s2) {
    // TODO
    Set s = SetNew();
	IntersectionInsert(s1->tree, s2->tree, s);
    return s;
} 
static void IntersectionInsert(struct node *s1, struct node *s2, Set new) {
	if (s1 == NULL || s2 == NULL) {
        return;
    } 
	// Only adding elements which already exist in s1
	if (TContains(s1, s2->item) == true) {
        new->tree = TreeInsertAVL(new->tree, s2->item);
    } 

	IntersectionInsert(s1, s2->left, new);
    IntersectionInsert(s1, s2->right, new);
}


/**
 * Returns a new set representing the set difference s1 - s2
 */
Set SetDifference(Set s1, Set s2) {
	// TODO
	Set s = SetNew();
	TDiff(s1->tree, s2->tree, s);
	return s;
} 
static void TDiff(struct node *s1, struct node *s2, Set new) {
	if (s1 == NULL || s2 == NULL) {
        return;
    } 
	// Adding elements from s1 which do not exist in s2 to new set
	if (TContains(s2, s1->item) == false) {
        new->tree = TreeInsertAVL(new->tree, s1->item);
    } 
	TDiff(s1->left, s2, new);
    TDiff(s1->right, s2, new);
}

/**
 * Returns true if the two sets are equal, and false otherwise
 */
bool SetEquals(Set s1, Set s2) {
	bool flag1 = true;
	bool flag2 = true;

	if (SetSubset(s1, s2) == false) {
		flag1 = false;
	} 
	if (SetSubset(s2, s1) == false) {
		flag2 = false;
	}
	// Using subsets to prove equality,
	// if A is subset of B, and B is subset of A
	// then A == B
	if (flag1 == true && flag2 == true) {
		return true;
	} else {
		return false;
	}
} 

/**
 * Returns true if set s1 is a subset of set s2, and false otherwise
 */

bool SetSubset(Set s1, Set s2) {
	// TODO
	return TSubset(s1->tree, s2->tree);
} 
static bool TSubset(struct node *s1, struct node *s2) {
	// Empty set is a subset of every set
	if (s1 == NULL) { 
		return true;
	} else if (s2 == NULL) {
		return false;
	} else if (s1 == NULL && s2 == NULL) { 
		//Empty set is a subset of empty set
		return true;
	}

	// If item in s1 doesnt exist in s2, automatically not subset
	if (TContains(s2, s1->item) == false) { 
		return false;
	} else {
		// Return true only if both left and right are true
		return TSubset(s1->left, s2) && TSubset(s1->right, s2);
	}
	return true;
}
	

/**
 * Finds the floor of a given item in a set
 */
int SetFloor(Set s, int item) {
	// TODO
	return TFloor(s->tree, item);
} 
static int TFloor(struct node *t, int item) {
	if (t == NULL) {
		return UNDEFINED;
	} else if (TContains(t, item) == true) {
		return item;
	} 

	if (item < t->item) {
		return TFloor(t->left, item);
	} else {
		int i = TFloor(t->right, item);
		return (t->item >= i ? t->item : i);
	}
}

/** 
 * Returns the ceiling of a given item in a set
 */
int SetCeiling(Set s, int item) {
	// TODO
	return TCeiling(s->tree, item);
}
static int TCeiling(struct node *t, int item) {

	if (t == NULL) {
		return UNDEFINED;
	} else if (TContains(t, item) == true) {
		return item;
	} 

	if (item > t->item) {
		return TCeiling(t->right, item);
	} else {
		int i = TCeiling(t->left, item);
		return (i >= item ? i : t->item);
	}
}

////////////////////////////////////////////////////////////////////////
// Cursor Operations

/**
 * Creates a new cursor positioned at the smallest element of the set
 */
SetCursor SetCursorNew(Set s) {
	// TODO
	return NULL;
}

/**
 * Frees all memory associated with the given cursor
 */
void SetCursorFree(SetCursor cur) {
	// TODO
}

/**
 * Returns the element at the cursor's current position, and then moves
 * the cursor to the next greatest element. If there is no next greatest
 * element, then all subsequent calls to SetCursorNext on this cursor
 * should return UNDEFINED.
 */
int SetCursorNext(SetCursor cur) {
	// TODO
	return UNDEFINED;
}

////////////////////////////////////////////////////////////////////////

