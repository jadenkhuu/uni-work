 // Implementation of the Agent ADT

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "Agent.h"
#include "Map.h"
#include "Stack.h"


// This struct stores information about an individual agent and can be
// used to store information that the agent needs to remember.
struct agent {
    char *name;
    int startLocation;
    int location;
    int maxStamina; // max stamina
    int stamina;    // current stamina
    int strategy;
    Map map;
    
    // TODO: Add more fields here as needed
    int *times_visit;
    int *DFS_path;
    int nextDFS;

    int thief_location;
};

// CLV
static Move chooseCLVMove(Agent agent, Map m);

// DFS
static Move chooseDFSMove(Agent agent, Map m);
static int *DFSCreatePath(Map m, Agent agent, int *DFSpath);

static Move chooseRandomMove(Agent agent, Map m);
static int filterRoads(Agent agent, Road roads[], int numRoads,
                       Road legalRoads[]);

/**
 * Creates a new agent
 */
Agent AgentNew(int start, int stamina, int strategy, Map m, char *name) {
    if (start >= MapNumCities(m)) {
        fprintf(stderr, "error: starting city (%d) is invalid\n", start);
        exit(EXIT_FAILURE);
    }
    
    Agent agent = malloc(sizeof(struct agent));
    if (agent == NULL) {
        fprintf(stderr, "error: out of memory\n");
        exit(EXIT_FAILURE);
    }
    
    agent->startLocation = start;
    agent->location = start;
    agent->maxStamina = stamina;
    agent->stamina = stamina;
    agent->strategy = strategy;
    agent->map = m;
    agent->name = strdup(name);
    
    // TODO: You may need to add to this

    // Create visited int array for CLV
    agent->times_visit = calloc(MapNumCities(m), sizeof(int));
    agent->times_visit[start] = 1;
    
    // Initialise array to store DFS path
    agent->DFS_path = calloc((2 * MapNumCities(m)), sizeof(int));
    for (int i = 0; i < (2 * MapNumCities(m)); i++) {
        agent->DFS_path[i] = -1;
    }
    // Generate DFS path
    agent->DFS_path = DFSCreatePath(m, agent, agent->DFS_path);
    // DFS counter to track steps in array
    agent->nextDFS = 1;

    return agent;
}

/**
 * Frees all memory associated with the agent
 * NOTE: You should not free the map because the map is owned by the
 *       main program, and the main program will free it
 */
void AgentFree(Agent agent) {
    // TODO: You may need to update this to free any extra memory you use
    free(agent->name);
    free(agent->times_visit);
    free(agent->DFS_path);
    free(agent);
    
}

////////////////////////////////////////////////////////////////////////
// Making moves

/**
 * Calculates the agent's next move
 */
Move AgentGetNextMove(Agent agent, Map m) {
    switch (agent->strategy) {
        case STATIONARY: return (Move){agent->location, 0};
        case RANDOM:     return chooseRandomMove(agent, m);
        // TODO: Implement more strategies here
        
        case CHEAPEST_LEAST_VISITED: return chooseCLVMove(agent, m);
        case DFS: return chooseDFSMove(agent, m);
        
        default:
            printf("error: strategy not implemented yet\n");
            exit(EXIT_FAILURE);
    }
}

static Move chooseRandomMove(Agent agent, Map m) {
    Road *roads = malloc(MapNumCities(m) * sizeof(Road));
    Road *legalRoads = malloc(MapNumCities(m) * sizeof(Road));

    // Get all roads to adjacent cities
    int numRoads = MapGetRoadsFrom(m, agent->location, roads);

    // Filter out roads that the agent does not have enough stamina for
    int numLegalRoads = filterRoads(agent, roads, numRoads, legalRoads);

    Move move;
    if (numLegalRoads > 0) {
        // Sort the roads using insertion sort
        for (int i = 1; i < numLegalRoads; i++) {
            Road r = legalRoads[i];
            int j = i;
            while (j > 0 && r.to < legalRoads[j - 1].to) {
                legalRoads[j] = legalRoads[j - 1];
                j--;
            }
            legalRoads[j] = r;
        }
        
        // nextMove is randomly chosen from the legal roads
        int k = rand() % numLegalRoads;
        move = (Move){legalRoads[k].to, legalRoads[k].length};
    } else {
        // The agent must stay in the same location
        move = (Move){agent->location, 0};
    }
    
    free(legalRoads);
    free(roads);
    return move;
}

static Move chooseCLVMove(Agent agent, Map m) {
    Road *roads = malloc(MapNumCities(m) * sizeof(Road));
    Road *legalRoads = malloc(MapNumCities(m) * sizeof(Road));

    int numRoads = MapGetRoadsFrom(m, agent->location, roads);
    int numLegalRoads = filterRoads(agent, roads, numRoads, legalRoads);

    agent->times_visit[agent->location]++;

    Move move;
    if (numLegalRoads > 0) {
        // Sort roads using insertion sort
        // Referenced from provided random strategy
        for (int i = 1; i < numLegalRoads; i++) {
            Road r = legalRoads[i];
            int j = i;
            while (j > 0 && r.to < legalRoads[j - 1].to) {
                legalRoads[j] = legalRoads[j - 1];
                j--;
            }
            legalRoads[j] = r;
        }

        // Create variable for cheapest least visited road index, k
        // Update accordingly, least visits, length, city ID
        int k = 0;
        for (int i = 1; i < numLegalRoads; i++) {
            // only check extra conditions if there is more than one city with same visits
            // else, check if city has less visits
            if (agent->times_visit[legalRoads[i].to] == agent->times_visit[legalRoads[k].to]) {
                if (legalRoads[i].length == legalRoads[k].length) {
                    if (legalRoads[i].to < legalRoads[k].to) {
                        k = i;
                    } 
                } else if (legalRoads[i].length < legalRoads[k].length) {
                    k = i;
                }
            } else if (agent->times_visit[legalRoads[i].to] < agent->times_visit[legalRoads[k].to]) {
                k = i;
            }
        }

        move = (Move){legalRoads[k].to, legalRoads[k].length};
        
    } else {
        agent->times_visit[agent->location]++;
        move = (Move){agent->location, 0};
    }

    free(legalRoads);
    free(roads);
    return move;
}

static Move chooseDFSMove(Agent agent, Map m) {
    Road *roads = malloc(MapNumCities(m) * sizeof(Road));
    Road *legalRoads = malloc(MapNumCities(m) * sizeof(Road));

    int numRoads = MapGetRoadsFrom(m, agent->location, roads);
    int numLegalRoads = filterRoads(agent, roads, numRoads, legalRoads);

    // Checks if there are still steps in the DFS path,
    // If not, generate new path and restart counter
    if (agent->DFS_path[agent->nextDFS] == -1) {
        agent->DFS_path = DFSCreatePath(m, agent, agent->DFS_path);
        agent->nextDFS = 1;
    }

    Move move;

    // Checks if the next step in DFS path is a legal path
    bool exists = false;
    int i = 0;
    while (i < numLegalRoads) {
        if (legalRoads[i].to == agent->DFS_path[agent->nextDFS]) {
            exists = true;
            break;
        }
        i++;
    }

    // Choose move accordingly if DFS path is legal or not
    if (exists == true) {
        move = (Move){agent->DFS_path[agent->nextDFS], legalRoads[i].length};
        agent->nextDFS++;
    } else if (exists == false) {
        move = (Move){agent->location, 0};
    }

    free(legalRoads);
    free(roads);
    return move;
}

static int *DFSCreatePath(Map m, Agent agent, int *DFSpath) {
    int numCities = MapNumCities(m);
    int *pred = calloc(numCities, sizeof(int));
    bool *visited = calloc(numCities, sizeof(bool));
    
    // Initialises all values in the array to -1
    for (int i = 0; i < numCities; i++) {
        pred[i] = -1;
        visited[i] = false;
    }

    // Counter for num cities in DFS path
    int numDFS = 0;

    struct stack *s = NewStack();
    int current_city = agent->location;
    
    push(s, current_city);
    
    while (StackSize(s) > 0) {
        int popped_city = pop(s);

        // Check visited
        if (visited[popped_city] == true) {
            continue;
        }

        // Keeping track of DFS backtracking
        while (MapContainsRoad(m, current_city, popped_city) == 0 && 
              (current_city != popped_city)) {
            
            DFSpath[numDFS++] = pred[current_city];
            current_city = pred[current_city];
        }

        visited[popped_city] = true;
        DFSpath[numDFS++] = popped_city;

        // explore popped city's adjacent cities which havent been visited
        for (int destination = numCities - 1; destination >= 0; destination--) {

            if (MapContainsRoad(m, popped_city, destination) == 0.0) {
                continue;
            }
            if (visited[destination] == false) {
                pred[destination] = popped_city;
                push(s, destination);
            }

        }

        current_city = popped_city;
    }

    free(pred);
    free(visited);
    FreeStack(s);
    return DFSpath;
} 

// Takes an array with all the possible roads and puts the ones the agentimage.png
// has enough stamina for into the legalRoads array
// Returns the number of legal roads 
static int filterRoads(Agent agent, Road roads[], int numRoads,
                       Road legalRoads[]) {
    int numLegalRoads = 0;
    for (int i = 0; i < numRoads; i++) {
        if (roads[i].length <= agent->stamina) {
            legalRoads[numLegalRoads++] = roads[i];
        }
    }
    return numLegalRoads;
}


/**
 * Executes a given move by updating the agent's internal state
 */
void AgentMakeNextMove(Agent agent, Move move) {
    if (move.to == agent->location) {
        agent->stamina = agent->maxStamina;
    } else {
        agent->stamina -= move.staminaCost;
    }
    agent->location = move.to;
    
    // TODO: You may need to add to this to handle different strategies
}

////////////////////////////////////////////////////////////////////////
// Gets information about the agent
// NOTE: It is expected that these functions do not need to be modified

/**
 * Gets the name of the agent
 */
char *AgentName(Agent agent) {
    return agent->name;
}

/**
 * Gets the current location of the agent
 */
int AgentLocation(Agent agent) {
    return agent->location;
}

/**
 * Gets the amount of stamina the agent currently has
 */
int AgentStamina(Agent agent) {
    return agent->stamina;
}

////////////////////////////////////////////////////////////////////////
// Learning information

/**
 * Tells the agent where the thief is
 */
void AgentGainInfo(Agent agent, int thiefLocation) {
    // TODO: Stage 3
    agent->thief_location = thiefLocation;
}

////////////////////////////////////////////////////////////////////////
// Displaying state

/**
 * Prints information about the agent (for debugging purposes)
 */
void AgentShow(Agent agent) {
    // TODO: You can implement this function however you want
    //       You can leave this function blank if you want
}

////////////////////////////////////////////////////////////////////////

