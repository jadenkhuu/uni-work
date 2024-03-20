
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "Graph.h"

static int isInCycle(Graph g, int src, int v, int visited[]);

// Worst case time complexity of this solution: O(n^3)
void nodesNotInCycle(Graph g, int notInCycle[]) {
    int *visited = malloc(GraphNumVertices(g) * sizeof(int));
    assert(visited != NULL);

    for (Vertex v = 0; v < GraphNumVertices(g); v++) {
        for (int i = 0; i < GraphNumVertices(g); i++) {
            visited[i] = -1;
        }

        if (isInCycle(g, v, v, visited)) {
            notInCycle[v] = 0;
        } else {
            notInCycle[v] = 1;
        }
    }
}

static int isInCycle(Graph g, int src, int v, int visited[]) {
    visited[v] = 1;
    for (Vertex w = 0; w < GraphNumVertices(g); w++) {
        if (GraphIsAdjacent(g, v, w)) {
            if (w == src) {
                return 1;
            }
            if (visited[w] == -1) {
                int cycleFound = isInCycle(g, src, w, visited);
                if (cycleFound == 1) return 1;
            }
        }
    }
    return 0;
}
