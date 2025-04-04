// Implementation of the binary search tree ADT

#include <assert.h>
#include <err.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sysexits.h>

#include "BSTree.h"
#include "Queue.h"

typedef struct BSTNode {
	int value;
	BSTree left;
	BSTree right;
} BSTNode;

////////////////////////////////////////////////////////////////////////

static BSTree newBSTNode(int v);
static void showBSTreeNode(BSTree t);
static void doPrintBSTree(BSTree t, FILE *out);

////////////////////////////////////////////////////////////////////////

// Creates a new empty BSTree
BSTree BSTreeNew(void) {
	return NULL;
}

// Frees all the memory associated with the given BSTree
void BSTreeFree(BSTree t) {
	if (t == NULL) {
		return;
	}
	
	BSTreeFree(t->left);
	BSTreeFree(t->right);
	free(t);
}

// Inserts a new value into the given BSTree and returns the root of the
// updated BSTree. Does not insert duplicate values.
BSTree BSTreeInsert(BSTree t, int v) {
	if (t == NULL) {
		return newBSTNode(v);
	} else if (v < t->value) {
		t->left = BSTreeInsert(t->left, v);
	} else if (v > t->value) {
		t->right = BSTreeInsert(t->right, v);
	} else { // (v == t->value)
		/* don't insert duplicates */;
	}
	return t;
}

// Creates a new node containing the given value
static BSTree newBSTNode(int v) {
	BSTree new = malloc(sizeof(*new));
	if (new == NULL) {
		err(EX_OSERR, "couldn't allocate BST node");
	}
	
	new->value = v;
	new->left = NULL;
	new->right = NULL;
	return new;
}

// Checks whether a value is in the given BSTree
bool BSTreeFind(BSTree t, int v) {
	if (t == NULL) {
		return false;
	} else if (v < t->value) {
		return BSTreeFind(t->left, v);
	} else if (v > t->value) {
		return BSTreeFind(t->right, v);
	} else { // (v == t->value)
		return true;
	}
}

// Prints a BSTree to the given file.
// Assumes that the file is open for writing.
void BSTreePrint(BSTree t, FILE *out) {
	doPrintBSTree(t, out);
}

// Prints a BSTree to stdout.
void BSTreeShow(BSTree t) {
	BSTreePrint(t, stdout);
}

////////////////////////////////////////////////////////////////////////

// Counts the number of nodes in the given BSTree
int BSTreeNumNodes(BSTree t) {
	if (t == NULL) {
		return 0;
	} else {
		return 1 + BSTreeNumNodes(t->left) + BSTreeNumNodes(t->right);
	}
}

// Counts the number of leaves in the given BSTree
int BSTreeNumLeaves(BSTree t) {
	// TODO: Task 1 - Implement this function

	int leaves_count = 0;

	if (t == NULL) {
		return 0;
	} else if (t->left == NULL && t->right == NULL) {
		return 1;
	} else {
		int left_counter = BSTreeNumLeaves(t->left);
		int right_counter = BSTreeNumLeaves(t->right);
		leaves_count = left_counter + right_counter;
	}

	return leaves_count;
}

// Deletes all of the leaves in the given BSTree and returns the root of
// the updated BSTree
BSTree BSTreeDeleteLeaves(BSTree t) {
	// TODO: Task 2 - Implement this function

	if (t == NULL) {
		return NULL;
	} else if (t->left == NULL && t->right == NULL) {
		free(t);
		return NULL;
	} 

	t->left = BSTreeDeleteLeaves(t->left);
	t->right = BSTreeDeleteLeaves(t->right);
	
	return t;
}

////////////////////////////////////////////////////////////////////////

// Prints the value in the given node
static void showBSTreeNode(BSTree t) {
	if (t == NULL) return;
	printf("%d ", t->value);
}

// Prints the in-order traversal of the given BSTree
void BSTreeInOrder(BSTree t) {
	if (t == NULL) return;

	BSTreeInOrder(t->left);
	showBSTreeNode(t);
	BSTreeInOrder(t->right);
}

// Prints the pre-order traversal of the given BSTree
void BSTreePreOrder(BSTree t) {
	if (t == NULL) return;

	showBSTreeNode(t);
	BSTreePreOrder(t->left);
	BSTreePreOrder(t->right);
}

// Prints the post-order traversal of the given BSTree
void BSTreePostOrder(BSTree t) {
	if (t == NULL) return;

	BSTreePostOrder(t->left);
	BSTreePostOrder(t->right);
	showBSTreeNode(t);
}

// Prints the level-order traversal of the given BSTree
void BSTreeLevelOrder(BSTree t) {
	// TODO: Task 3 - Implement this function
	Queue q = QueueNew();
	QueueEnqueue(q, t);

	while (QueueSize(q) != 0) {
		Item print = QueueDequeue(q);
		printf("%d ", print->value);

		if (print->left != NULL) {
			QueueEnqueue(q, print->left);
		}
		if (print->right != NULL) {
			QueueEnqueue(q, print->right);
		}
	
	}

	QueueFree(q);
}

////////////////////////////////////////////////////////////////////////
// ASCII tree printer
// Courtesy: ponnada
// Via: http://www.openasthra.com/c-tidbits/printing-binary-trees-in-ascii/

// data structures
typedef struct asciinode_struct asciinode;
struct asciinode_struct {
	asciinode *left, *right;
	// length of the edge from this node to its children
	int edge_length;
	int height;
	int lablen;
	// -1=I am left, 0=I am root, 1=I am right
	int parent_dir;
	// max supported unit32 in dec, 10 digits max
	char label[11];
};

// functions
static void print_level(asciinode *node, int x, int level, FILE *out);
static void compute_edge_lengths(asciinode *node);
static void compute_lprofile(asciinode *node, int x, int y);
static void compute_rprofile(asciinode *node, int x, int y);
static asciinode *build_ascii_tree(BSTree t);
static void free_ascii_tree(asciinode *node);

#define MAX_HEIGHT 1000
static int lprofile[MAX_HEIGHT];
static int rprofile[MAX_HEIGHT];
#define INFINITY (1 << 20)

#define MIN(X, Y) (((X) < (Y)) ? (X) : (Y))
#define MAX(X, Y) (((X) > (Y)) ? (X) : (Y))

static int gap = 3; // gap between left and right nodes

// used for printing next node in the same level,
// this is the x coordinate of the next char printed
static int print_next;

// prints ascii tree for given Tree structure
static void doPrintBSTree(BSTree t, FILE *out)
{
	asciinode *proot;
	int xmin, i;
	if (t == NULL)
		return;
	proot = build_ascii_tree(t);
	compute_edge_lengths(proot);
	for (i = 0; i < proot->height && i < MAX_HEIGHT; i++)
		lprofile[i] = INFINITY;
	compute_lprofile(proot, 0, 0);
	xmin = 0;
	for (i = 0; i < proot->height && i < MAX_HEIGHT; i++)
		xmin = MIN(xmin, lprofile[i]);
	for (i = 0; i < proot->height; i++) {
		print_next = 0;
		print_level(proot, -xmin, i, out);
		fprintf(out, "\n");
	}
	if (proot->height >= MAX_HEIGHT) {
		fprintf(out,
			"(Tree is taller than %d; may be drawn incorrectly.)\n",
			MAX_HEIGHT);
	}
	free_ascii_tree(proot);
}

// This function prints the given level of the given tree, assuming
// that the node has the given x cordinate.
static void print_level(asciinode *node, int x, int level, FILE *out)
{
	int i, isleft;
	if (node == NULL)
		return;
	isleft = (node->parent_dir == -1);
	if (level == 0) {
		for (i = 0;
			 i < (x - print_next - ((node->lablen - isleft) / 2));
			 i++)
			fprintf(out, " ");
		print_next += i;
		fprintf(out, "%s", node->label);
		print_next += node->lablen;
	} else if (node->edge_length >= level) {
		if (node->left != NULL) {
			for (i = 0; i < (x - print_next - (level)); i++) {
				fprintf(out, " ");
			}
			print_next += i;
			fprintf(out, "/");
			print_next++;
		}
		if (node->right != NULL) {
			for (i = 0; i < (x - print_next + (level)); i++) {
				printf(" ");
			}
			print_next += i;
			fprintf(out, "\\");
			print_next++;
		}
	} else {
		print_level(
			node->left,
			x - node->edge_length - 1,
			level - node->edge_length - 1,
			out);
		print_level(
			node->right,
			x + node->edge_length + 1,
			level - node->edge_length - 1,
			out);
	}
}

// This function fills in the edge_length and
// height fields of the specified tree
static void compute_edge_lengths(asciinode *node)
{
	int h, hmin, i, delta;
	if (node == NULL)
		return;
	compute_edge_lengths(node->left);
	compute_edge_lengths(node->right);

	/* first fill in the edge_length of node */
	if (node->right == NULL && node->left == NULL)
		node->edge_length = 0;
	else {
		if (node->left == NULL)
			hmin = 0;
		else {
			for (i = 0; i < node->left->height && i < MAX_HEIGHT; i++)
				rprofile[i] = -INFINITY;
			compute_rprofile(node->left, 0, 0);
			hmin = node->left->height;
		}
		if (node->right == NULL)
			hmin = 0;
		else {
			for (i = 0; i < node->right->height && i < MAX_HEIGHT; i++)
				lprofile[i] = INFINITY;
			compute_lprofile(node->right, 0, 0);
			hmin = MIN(node->right->height, hmin);
		}
		delta = 4;
		for (i = 0; i < hmin; i++) {
			int w = gap + 1 + rprofile[i] - lprofile[i];
			delta = (delta > w) ? delta : w;
		}

		// If the node has two children of height 1, then we allow the
		// two leaves to be within 1, instead of 2
		if (((node->left != NULL && node->left->height == 1) ||
			(node->right != NULL && node->right->height == 1)) &&
			delta > 4)
			delta--;
		node->edge_length = ((delta + 1) / 2) - 1;
	}

	// now fill in the height of node
	h = 1;
	if (node->left != NULL)
		h = MAX(node->left->height + node->edge_length + 1, h);
	if (node->right != NULL)
		h = MAX(node->right->height + node->edge_length + 1, h);
	node->height = h;
}

static asciinode *build_ascii_tree_recursive(BSTree t)
{
	asciinode *node;

	if (t == NULL)
		return NULL;
	node = malloc(sizeof(asciinode));
	node->left = build_ascii_tree_recursive(t->left);
	node->right = build_ascii_tree_recursive(t->right);
	if (node->left != NULL)
		node->left->parent_dir = -1;
	if (node->right != NULL)
		node->right->parent_dir = 1;
	sprintf(node->label, "%d", t->value);
	node->lablen = (int) strlen(node->label);

	return node;
}

// Copy the tree into the ascii node structre
static asciinode *build_ascii_tree(BSTree t)
{
	asciinode *node;
	if (t == NULL)
		return NULL;
	node = build_ascii_tree_recursive(t);
	node->parent_dir = 0;
	return node;
}

// Free all the nodes of the given tree
static void free_ascii_tree(asciinode *node)
{
	if (node == NULL)
		return;
	free_ascii_tree(node->left);
	free_ascii_tree(node->right);
	free(node);
}

// The following function fills in the lprofile array for the given
// tree. It assumes that the center of the label of the root of this tree
// is located at a position(x,y).  It assumes that the edge_length
// fields have been computed for this tree.
static void compute_lprofile(asciinode *node, int x, int y)
{
	int i, isleft;
	if (node == NULL)
		return;
	isleft = (node->parent_dir == -1);
	lprofile[y] = MIN(lprofile[y], x - ((node->lablen - isleft) / 2));
	if (node->left != NULL) {
		for (i = 1; i <= node->edge_length && y + i < MAX_HEIGHT; i++)
			lprofile[y + i] = MIN(lprofile[y + i], x - i);
	}
	compute_lprofile(
		node->left,
		x - node->edge_length - 1,
		y + node->edge_length + 1);
	compute_lprofile(
		node->right,
		x + node->edge_length + 1,
		y + node->edge_length + 1);
}

static void compute_rprofile(asciinode *node, int x, int y)
{
	int i, notleft;
	if (node == NULL)
		return;
	notleft = (node->parent_dir != -1);
	rprofile[y] = MAX(rprofile[y], x + ((node->lablen - notleft) / 2));
	if (node->right != NULL) {
		for (i = 1; i <= node->edge_length && y + i < MAX_HEIGHT; i++)
			rprofile[y + i] = MAX(rprofile[y + i], x + i);
	}
	compute_rprofile(
		node->left,
		x - node->edge_length - 1,
		y + node->edge_length + 1);
	compute_rprofile(
		node->right,
		x + node->edge_length + 1,
		y + node->edge_length + 1);
}

