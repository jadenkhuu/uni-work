
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "BSTree.h"

static void maxDiffR(BSTree t, int *maxDiff, int *nodes);

int maxDiff(BSTree t) {
    if (t == NULL) {
        return 0;
    }

    int maxDiff = 0;
    int nodes = 0;

    maxDiffR(t, &maxDiff, &nodes);

    return maxDiff;
}

static void maxDiffR(BSTree t, int *maxDiff, int *nodes) {
    int diff = 0; 

    if (t == NULL) { 
        *maxDiff = diff; 
        *nodes = 0;
        return;
    }

    int ld, ln, rd, rn;
    maxDiffR(t->left, &ld, &ln);
    maxDiffR(t->right, &rd, &rn);

    diff = abs(ln - rn);
    *maxDiff = (ld > rd) ? ld : rd; 
    if (diff > *maxDiff) {
        *maxDiff = diff;
    }

    *nodes = rn + ln + 1;
}
