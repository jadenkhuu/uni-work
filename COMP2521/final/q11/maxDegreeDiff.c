
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "Graph.h"

int maxDegreeDiff(Graph g) {
    // TODO
    int largest_diff = 0;

    for (int i = 0; i < g->nV; i++) {
        for (Adjlist curr = g->edges[i]; curr->next != NULL; curr = curr->next) {
            int difference = curr->v - g->edge;
        }
    }
        
    return -1;
}

