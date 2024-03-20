// Assignment 1 - CS Explorer
// cs_explorer.c
//
// This program was written by YOUR-NAME-HERE (z5416824)
// on 19/03/2022
//
// Version 1.0.0 (2022-03-08): Assignment Released.
//
// TODO: Description of your program.

#include <stdio.h>

// Additional libraries here


// Provided constants 
#define SIZE 8
#define PLAYER_STARTING_ROW (SIZE - 1)
#define PLAYER_STARTING_COL 0
#define EMPTY_POINTS 0
#define EMPTY_TYPE 'E'
#define PLAYER_TYPE 'P'
#define MONSTER_TYPE 'M'
#define HEALING_TYPE 'H'
#define BOULDER_TYPE 'B'

// Your constants here
#define TRUE 1
#define FALSE 0

// Provided struct
struct location {
    char occupier;
    int points;
};

// Your structs here
struct coordinate {
    int c_row;
    int c_col;
};

// Provided functions use for game setup
// You do not need to use these functions yourself.
void init_map(struct location map[SIZE][SIZE]);
void place_player_on_starting_location(struct location map[SIZE][SIZE]);

// You will need to use these functions for stage 1.
void print_cheat_map(struct location map[SIZE][SIZE]);
void print_game_play_map(struct location map[SIZE][SIZE]);

// Your functions prototypes here
void points_in_square (int s_row, int s_col, int s_size, char s_type);

int main(void) {

    struct coordinate coord;
    struct location map[SIZE][SIZE];
    init_map(map);

    printf("Welcome Explorer!!\n");
    printf("How many game pieces are on the map?\n");
    
    // TODO: Add code to scan in the number of game pieces here.
    int n_gamepieces = 0;
    scanf("%d", &n_gamepieces);

    // TODO: Add code to scan in the details of each game piece and place them
    //       on the map
    printf("Enter the details of game pieces:\n");
    
    int point_occupier;
    int row;
    int col;

    int row_count = 0;
    while (row_count < n_gamepieces) {
        
        scanf("%d", &point_occupier);
        scanf("%d %d", &row, &col);

        if (row >= 0 && row < 8 && col >= 0 && row < 8) {
            if (point_occupier < 0 && point_occupier >= -9) {
                map[row][col].occupier = MONSTER_TYPE;
            } 
            else if (point_occupier > 0 && point_occupier <= 9) {
                map[row][col].occupier = HEALING_TYPE;
            } 
            else if (point_occupier == 0) {
                map[row][col].occupier = BOULDER_TYPE;
            }
            map[row][col].points = point_occupier;
        }
        if (row == 7 && col == 0) {
            map[row][col].occupier = PLAYER_TYPE;
        }
    row_count++;
    }
    
    // After the game pieces have been added to the map print out the map.
    print_game_play_map(map);
    printf("\nEXPLORE!\n");
    printf("Enter command: ");
    
    char enter_command;
    int p_health = 10;
    coord.c_row = PLAYER_STARTING_ROW;
    coord.c_col = PLAYER_STARTING_COL;
            
    while (scanf(" %c", &enter_command) != EOF) {
        
        if (enter_command == 'c') {
            print_cheat_map(map); 
        }
        if (enter_command == 'q') {
            printf("Exiting Program!\n");
            return 1;
        }
        if (enter_command == 'h') {
            printf("Your player is at ");
            printf("(%d, %d) ", coord.c_row, coord.c_col);
            printf("with a health of %d.\n", p_health);
        }
        if (enter_command == 'm') {
            map[coord.c_row][coord.c_col].occupier = EMPTY_TYPE;
            char direction_command;
            scanf(" %c", &direction_command);
            
            if (direction_command == 'u') {
                if (coord.c_row > 0) {
                    coord.c_row--;
                }
                if (map[coord.c_row][coord.c_col].occupier 
                == BOULDER_TYPE) {
                    coord.c_row++;
                }
            } 
            if (direction_command == 'd') {
                if (coord.c_row < 7) {
                    coord.c_row++;
                }
                if (map[coord.c_row][coord.c_col].occupier 
                == BOULDER_TYPE) {
                    coord.c_row--;
                }
            } 
            if (direction_command == 'l') {
                if(coord.c_col > 0) {
                    coord.c_col--;
                }
                if (map[coord.c_row][coord.c_col].occupier 
                == BOULDER_TYPE) {
                    coord.c_col++;
                }
            } 
            if (direction_command == 'r') {
                if(coord.c_col < 7) {
                    coord.c_col++;
                }
                if (map[coord.c_row][coord.c_col].occupier 
                == BOULDER_TYPE) {
                    coord.c_col--;
                }
            }
        }

        if (map[coord.c_row][coord.c_col].occupier == MONSTER_TYPE ||
            map[coord.c_row][coord.c_col].occupier == HEALING_TYPE) {
            
            p_health = p_health + map[coord.c_row][coord.c_col].points;
        }
        
        if (enter_command == 's') {
            int s_row = 0;
            int s_col = 0;
            int s_size = 0;
            char s_type;

            scanf("%d %d %d", &s_row, &s_col, &s_size);
            scanf(" %c", &s_type);
            points_in_square(s_row, s_col, s_size, s_type, map);
            
        }
        map[coord.c_row][coord.c_col].occupier = PLAYER_TYPE;
        print_game_play_map(map);   
        printf("Enter command: ");
    }
    // TODO: keep scanning in commands from the user until the user presses
    // ctrl + d
    
    return 0;
}


////////////////////////////////////////////////////////////////////////////////
///////////////////////////// ADDITIONAL FUNCTIONS /////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// TODO: you may need to add additional functions here

void points_in_square (int s_row, int s_col, int s_size, 
char s_type, struct location map[SIZE][SIZE]) {

    int s_total = 0;
        
    if (s_row > 7) {
        s_row = 7;
    } 
    if (s_row < 0) {
        s_row = 0;
    } 
    if (s_col > 7) {
        s_col = 7;
    } 
    if (s_col < 0) {
        s_col = 0;
    }

    int s_end_row = s_row + s_size;
    int s_end_col = s_col + s_size;
    
    if (s_end_row > 7) {
        s_end_row = 7;
    } if (s_end_col > 7) {
        s_end_col = 7;
    }
            
    while (s_row < s_end_row) {
        int s_col_start = s_col;
        while (s_col < s_end_col) {
            
            if (s_type == MONSTER_TYPE && 
                map[s_row][s_col].occupier == MONSTER_TYPE) {
                s_total = s_total + map[s_row][s_col].points;
            }
            if (s_type == HEALING_TYPE && 
                map[s_row][s_col].occupier == HEALING_TYPE) {
                s_total = s_total + map[s_row][s_col].points;
            }
            s_col++;
        }
        s_col = s_col_start;
        s_row++;
                
    }

    if (s_type == MONSTER_TYPE) {
        printf("Monsters in this section could apply ");
        printf("%d health points.\n", s_total);
    }
    if (s_type == HEALING_TYPE) {
        printf("Healing Potions in this section could apply ");
        printf("%d health points.\n", s_total);
    }
}
////////////////////////////////////////////////////////////////////////////////
////////////////////////////// PROVIDED FUNCTIONS //////////////////////////////
/////////////////////////// (DO NOT EDIT BELOW HERE) ///////////////////////////
////////////////////////////////////////////////////////////////////////////////

// Provided Function
// Initalises all elements on the map to be empty to prevent access errors.
void init_map(struct location map[SIZE][SIZE]) {
    int row = 0;
    while (row < SIZE) {
        int col = 0;
        while (col < SIZE) {
            struct location curr_loc;
            curr_loc.occupier = EMPTY_TYPE;
            curr_loc.points = EMPTY_POINTS;
            map[row][col] = curr_loc;
            col++;
        }
        row++;
    }

    place_player_on_starting_location(map);
}

// Provided Function
// Places the player in the bottom left most location.
void place_player_on_starting_location(struct location map[SIZE][SIZE]) {
    map[PLAYER_STARTING_ROW][PLAYER_STARTING_COL].occupier = PLAYER_TYPE;
}

// Provided Function
// Prints out map with alphabetic values. Monsters are represented with 'M',
// healing potions in 'H', boulders with 'B' and the player with 'P'.
void print_game_play_map(struct location map[SIZE][SIZE]) {
    printf(" -----------------\n");
    int row = 0;
    while (row < SIZE) {
        printf("| ");
        int col = 0;
        while (col < SIZE) {
            if (map[row][col].occupier == EMPTY_TYPE) {
                printf("- ");
            } else {
                printf("%c ", map[row][col].occupier);
            }
            col++;
        }
        printf("|\n");
        row++;
    }
    printf(" -----------------\n");
}

// Provided Function
// Prints out map with numerical values. Monsters are represented in red,
// healing potions in blue, boulder in green and the player in yellow.
//
// We use some functionality you have not been taught to make sure that
// colours do not appear during testing.
void print_cheat_map(struct location map[SIZE][SIZE]) {

    printf(" -----------------\n");
    int row = 0;
    while (row < SIZE) {
        printf("| ");
        int col = 0;
        while (col < SIZE) {
            if (map[row][col].occupier == PLAYER_TYPE) {
                // print the player in purple
                // ----------------------------------------
                // YOU DO NOT NEED TO UNDERSTAND THIS CODE.
                #ifndef NO_COLORS
                printf("\033[1;35m");
                #endif
                // ----------------------------------------
                printf("%c ", PLAYER_TYPE);
            } else if (map[row][col].occupier == HEALING_TYPE) {
                // print healing potion in green
                // ----------------------------------------
                // YOU DO NOT NEED TO UNDERSTAND THIS CODE.
                #ifndef NO_COLORS
                printf("\033[1;32m");
                #endif
                // ----------------------------------------
                printf("%d ", map[row][col].points);
            } else if (map[row][col].occupier == MONSTER_TYPE) {
                // print monsters in red
                // ----------------------------------------
                // YOU DO NOT NEED TO UNDERSTAND THIS CODE.
                #ifndef NO_COLORS
                printf("\033[1;31m");
                #endif
                // ----------------------------------------
                printf("%d ", -map[row][col].points);
            } else if (map[row][col].occupier == BOULDER_TYPE) {
                // print boulder in blue
                // ----------------------------------------
                // YOU DO NOT NEED TO UNDERSTAND THIS CODE.
                #ifndef NO_COLORS
                printf("\033[1;34m");
                #endif
                // ----------------------------------------
                printf("%d ", map[row][col].points);
            } else {
                // print empty squares in the default colour
                printf("- ");
            }
            // ----------------------------------------
            // YOU DO NOT NEED TO UNDERSTAND THIS CODE.
            // reset the colour back to default
            #ifndef NO_COLORS
            printf("\033[0m");
            #endif
            // ----------------------------------------
            col++;
        }
        printf("|\n");
        row++;
    }
    printf(" -----------------\n");
}
