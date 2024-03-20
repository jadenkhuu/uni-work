// Implementation of the Map ADT

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "Map.h"

/////////////////////////////////////////////////////////////////////////
static int validVertex(Map m, int city1);

struct map { 
    // TODO 
    int **roads; // Adj Matrix storing positive weights, -1 if not adj
    int nV;
    int nE;
    char **cityname; // Array holding city names (size of nV)
};


/**
 * Creates a new map with the given number of cities
 * Assumes that numCities is positive
 */
Map MapNew(int numCities) {
    // TODO
    assert(numCities >= 0);
    int i;
    
    Map m = malloc(sizeof(struct map));
    assert(m != NULL);

    m->nV = numCities;
    m->nE = 0;

    m->roads = malloc(numCities * sizeof(int *));
    assert(m->roads != NULL);

    m->cityname = malloc(numCities * sizeof(char *));
    assert(m->cityname != NULL);

    for (i = 0; i < numCities; i++) {
        m->roads[i] = calloc(numCities, sizeof(int));
        assert(m->roads[i] != NULL);
    }
    
    return m;
}

/**
 * Frees all memory associated with the given map
 */
void MapFree(Map m) {
    // TODO
    assert(m != NULL);

    for (int i = 0; i < m->nV; i++) {
        free(m->roads[i]);
        free(m->cityname[i]);
    }   

    free(m->roads);
    free(m->cityname);
    free(m);

}

/**
 * Returns the number of cities on the given map
 */
int MapNumCities(Map m) {
    // TODO
    return m->nV;
}

/**
 * Returns the number of roads on the given map
 */
int MapNumRoads(Map m) {
    // TODO
    return m->nE;
}

/**
 * Inserts a road between two cities with the given length
 * Does nothing if there is already a road between the two cities
 * Assumes that the cities are valid and are not the same
 * Assumes that the length of the road is positive
 */
void MapInsertRoad(Map m, int city1, int city2, int length) {
    // TODO
    if (MapContainsRoad(m, city1, city2) > 0) {
        return;
    } else {
        m->roads[city1][city2] = length;
        m->roads[city2][city1] = length;
        m->nE++;
    }
}

/**
 * Sets the name of the given city
 */
void MapSetName(Map m, int city, char *name) {
    // TODO
    m->cityname[city] = malloc(strlen(name) * sizeof(char) + 1);
    strcpy(m->cityname[city], name);

}

/**
 * Returns the name of the given city
 */
char *MapGetName(Map m, int city) {
    // TODO
    return m->cityname[city];
}

/**
 * Checks if there is a road between the two given cities
 * Returns the length of the road if there is a road, and 0 otherwise
 */
int MapContainsRoad(Map m, int city1, int city2) {
    // TODO
    assert(validVertex(m, city1));
    assert(validVertex(m, city2));

    return m->roads[city1][city2];
}
static int validVertex(Map m, int city1) {
    return city1 >= 0 && city1 < m->nV;
}

/**
 * Returns the number of roads connected to the given city and stores
 * them in the given roads array. The `from` field should be equal to
 * `city` for all the roads in the array.
 * Assumes that the roads array is large enough to store all the roads
 */
int MapGetRoadsFrom(Map m, int city, Road roads[]) {
    // TODO
    
    int j = 0;
    for (int i = 0; i < m->nV; i++) {
        roads[i].from = city;

        if (m->roads[city][i] > 0) {
            roads[j].to = i;
            roads[j].length = m->roads[city][i];
            j++;
        }
    }

    return j;
}

/**
 * Displays the map
 * !!! DO NOT EDIT THIS FUNCTION !!!
 * This function will work once the other functions are working
 */
void MapShow(Map m) {
    printf("Number of cities: %d\n", MapNumCities(m));
    printf("Number of roads: %d\n", MapNumRoads(m));
    
    Road *roads = malloc(MapNumRoads(m) * sizeof(Road));
    if (roads == NULL) {
        fprintf(stderr, "error: out of memory\n");
        exit(EXIT_FAILURE);    
    }
    
    for (int i = 0; i < MapNumCities(m); i++) {
        printf("[%d] %s has roads to:", i, MapGetName(m, i));
        int numRoads = MapGetRoadsFrom(m, i, roads);
        for (int j = 0; j < numRoads; j++) {
            if (j > 0) {
                printf(",");
            }
            printf(" [%d] %s (%d)", roads[j].to, MapGetName(m, roads[j].to),
                   roads[j].length);
        }
        printf("\n");
    }
    
    free(roads);
}

