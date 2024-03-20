// Assignment 1 - CS Explorer
// cs_explorer.c
//
// This program was written by Jaden Khuu (z5416824)
// on 19/03/2022
//
// Version 1.0.0 (2022-03-08): Assignment Released.
//
// TODO: Description of your program.
// This program will run a game where a player is on a grid board.
// 
// The player can move around and encounter monsters, healing potions
// and boulders.
// 
// The player wins if there are no more monsters on the board.
// The player loses if their health goes to 0 or if they are trapped between 
// boulders.

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
#define PLAYER_STARTING_HEALTH 10

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

struct square_scanning {
    int row;
    int col;
    int size;
    int end_row;
    int end_col;
    char type;
};

struct rectangle_scanning {
    int row;
    int col;
    int e_row;
    int e_col;
    char type;
};

struct rowcolumn {
    int row;
    int col;
};

// Provided functions use for game setup
// You do not need to use these functions yourself.
void init_map(struct location map[SIZE][SIZE]);
void place_player_on_starting_location(struct location map[SIZE][SIZE]);

// You will need to use these functions for stage 1.
void print_cheat_map(struct location map[SIZE][SIZE]);
void print_game_play_map(struct location map[SIZE][SIZE]);

// Your functions prototypes here
int movement_row(struct location map[SIZE][SIZE], 
struct coordinate coord, char command);

int movement_col(struct location map[SIZE][SIZE], 
struct coordinate coord, char command);

int check_lose(struct location map[SIZE][SIZE]);

int function_clamping(int coord);

void print_count_result(char s_type, int s_total);

int player_boulder_loss(struct location map[SIZE][SIZE]);

int boulder_trap(struct location map[SIZE][SIZE], struct coordinate coord);

int square_scan(struct location map[SIZE][SIZE], struct square_scanning s);

int rectangle_scan(struct location map[SIZE][SIZE],
struct rectangle_scanning rec);

// Main
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

        if (row >= 0 && row < 8 
        && col >= 0 && col < 8) {
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

    int p_health = PLAYER_STARTING_HEALTH;
    coord.c_row = PLAYER_STARTING_ROW;
    coord.c_col = PLAYER_STARTING_COL;
    char enter_command;

    while (scanf(" %c", &enter_command) != EOF) {

        // Print cheat Map
        if (enter_command == 'c') {
            print_cheat_map(map); 
        }

        // Quit Game
        if (enter_command == 'q') {
            printf("Exiting Program!\n");
            return 1;
        }
        
        // View health and position
        if (enter_command == 'h') {
            printf("Your player is at ");
            printf("(%d, %d) ", coord.c_row, coord.c_col);
            printf("with a health of %d.\n", p_health);
        }
        
        // Player movement
        if (enter_command == 'm') {
            map[coord.c_row][coord.c_col].occupier = EMPTY_TYPE;
            
            char direction_command;
            scanf(" %c", &direction_command);
            
            coord.c_row = movement_row(map, coord, direction_command);
            coord.c_col = movement_col(map, coord, direction_command);
        }
        if (map[coord.c_row][coord.c_col].occupier == MONSTER_TYPE ||
            map[coord.c_row][coord.c_col].occupier == HEALING_TYPE) {
            
            // Update player health
            p_health = p_health + map[coord.c_row][coord.c_col].points;
        }
        
        // Scanning Square 
        struct square_scanning squ;
        if (enter_command == 's') {
            
            scanf("%d %d %d", &squ.row, &squ.col, &squ.size);
            scanf(" %c", &squ.type);

            squ.row = function_clamping(squ.row);
            squ.col = function_clamping(squ.col);

            // Determine end row and column
            squ.end_row = squ.row + squ.size;
            squ.end_col = squ.col + squ.size;
            
            squ.end_row = function_clamping(squ.end_row);
            squ.end_col = function_clamping(squ.end_col);
            
            int s_total = 0;
            s_total = square_scan(map, squ);
            
            print_count_result(squ.type, s_total);
        }
        
        // Scanning Rectangle
        if (enter_command == 'r') {
            struct rectangle_scanning rec;
            int r_total = 0;

            scanf("%d %d %d %d", &rec.row, &rec.col, &rec.e_row, &rec.e_col);
            scanf(" %c", &rec.type);

            rec.row = function_clamping(rec.row);
            rec.col = function_clamping(rec.col);
            rec.e_row = function_clamping(rec.e_row);
            rec.e_col = function_clamping(rec.e_col);

            // Consider if end < start and swap the values
            if (rec.col > rec.e_col) {
                int swap;
                swap = rec.e_col;
                rec.e_col = rec.col;
                rec.col = swap;

            }
            if (rec.row > rec.e_row) {
                int swap;
                swap = rec.e_row;
                rec.e_row = rec.row;
                rec.row = swap;
            }

            // Calculate and print rectangle values
            r_total = rectangle_scan(map, rec);
            print_count_result(rec.type, r_total);
        }   

        // Place Player on board
        map[coord.c_row][coord.c_col].occupier = PLAYER_TYPE;
        
        int b_row; 
        int b_col;
        int p_boulder_flag = TRUE; 
        if (enter_command == 'b') {
            
            scanf("%d %d", &b_row, &b_col);
            
            b_row = function_clamping(b_row);
            b_col = function_clamping(b_col);

            map[b_row][b_col].occupier = BOULDER_TYPE;
            map[b_row][b_col].points = 0;

            // GAME LOST if player is squashed by boulder 
            // Replace 'B' back to 'P'
            p_boulder_flag = player_boulder_loss(map);
            if (p_boulder_flag == FALSE) {
                map[b_row][b_col].occupier = PLAYER_TYPE;
            }    
        }

        // Print Map
        print_game_play_map(map);   

        // GAME LOST due to health 
        if (p_health <= 0) {
            printf("GAME LOST!\n");
            return 1;
        } 
        
        // Print GAME LOST due to boulder squashing player
        if (p_boulder_flag == FALSE) {
            printf("GAME LOST!\n");
            return 1;
        }

        // GAME WIN when no more monsters
        int monster_flag;
        monster_flag = check_lose(map);
        if (monster_flag == FALSE) {
            return 1;
        }
    
        // GAME LOST to boulder trap
        int check_boulder_trap; 
        check_boulder_trap = boulder_trap(map, coord);
        if (check_boulder_trap == TRUE) {
            printf("GAME LOST!\n");
            return 1;
        }
        
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

// Movement function row
int movement_row(struct location map[SIZE][SIZE], 
struct coordinate coord, char command) {
    if (command == 'u') {
        if (coord.c_row > 0) {
            coord.c_row--;
        }
        if (map[coord.c_row][coord.c_col].occupier
        == BOULDER_TYPE) {
            coord.c_row++;
        }
    }
    if (command == 'd') {
        if (coord.c_row < 7) {
            coord.c_row++;
        }
        if (map[coord.c_row][coord.c_col].occupier 
        == BOULDER_TYPE) {
            coord.c_row--;
        }
    }
    return coord.c_row;
}
// Movement function col
int movement_col(struct location map[SIZE][SIZE],
struct coordinate coord, char command) {
    if (command == 'l') {
        if (coord.c_col > 0) {
            coord.c_col--;
        }
        if (map[coord.c_row][coord.c_col].occupier 
        == BOULDER_TYPE) {
            coord.c_col++;
        }
    } 
    if (command == 'r') {
        if (coord.c_col < 7) {
            coord.c_col++;
        }
        if (map[coord.c_row][coord.c_col].occupier 
        == BOULDER_TYPE) {
            coord.c_col--;
        }
    }
    return coord.c_col;
}

// Square Scanning 
int square_scan(struct location map[SIZE][SIZE], struct square_scanning squ) { 
    int s_total = 0;
    while (squ.row < squ.end_row) {
        int s_col_start = squ.col;
        while (squ.col < squ.end_col) {
            if (squ.type == MONSTER_TYPE &&
            map[squ.row][squ.col].occupier == MONSTER_TYPE) {
                s_total = s_total + map[squ.row][squ.col].points;
            }
            if (squ.type == HEALING_TYPE &&
            map[squ.row][squ.col].occupier == HEALING_TYPE) {
                s_total = s_total + map[squ.row][squ.col].points;
            }
            squ.col++;
        }
        squ.col = s_col_start;
        squ.row++;
    }
    return s_total;
}

// Rectangle Scanning 
int rectangle_scan(struct location map[SIZE][SIZE],
struct rectangle_scanning rec) {
    int r_total = 0;
    while (rec.row < rec.e_row + 1) {
        int r_start_col = rec.col;
        while (rec.col < rec.e_col + 1) {
            
            if (rec.type == MONSTER_TYPE &&
            map[rec.row][rec.col].occupier == MONSTER_TYPE) {
                r_total = r_total + map[rec.row][rec.col].points;
            }
            if (rec.type == HEALING_TYPE &&
            map[rec.row][rec.col].occupier == HEALING_TYPE) {
                r_total = r_total + map[rec.row][rec.col].points;
            }
            rec.col++;
        }
        rec.col = r_start_col;
        rec.row++;
    }
    return r_total;
}

//Print count result (Square and rectangle scanning)
void print_count_result(char s_type, int s_total) {
    if (s_type == MONSTER_TYPE) {
        printf("Monsters in this section could apply ");
        printf("%d health points.\n", s_total);
    }
    if (s_type == HEALING_TYPE) {
        printf("Healing Potions in this section could apply ");
        printf("%d health points.\n", s_total);
    }
}

// Function for clamping values
int function_clamping(int coord) {
    if (coord > 7) {
        coord = 7;
    }
    if (coord < 0) {
        coord = 0;
    }
    return coord;
}

// Check if any monsters are left on the field
int check_lose(struct location map[SIZE][SIZE]) {
    int monster_flag = FALSE;
    int i = 0;
    while (i < SIZE) {
        int j = 0;
        while (j < SIZE) {
            if (map[i][j].occupier == MONSTER_TYPE) {
                monster_flag = TRUE;
            }
            j++;
        }
        i++;    
    }
    if (monster_flag == FALSE) {
        printf("GAME WON!\n");
    }
    return monster_flag;
}

// Check if boulder squashed player
int player_boulder_loss(struct location map[SIZE][SIZE]) {
    int p_boulder_flag = FALSE;
    int i = 0;
    while (i < SIZE) {
        int j = 0;
        while (j < SIZE) {
            if (map[i][j].occupier == PLAYER_TYPE) {
                p_boulder_flag = TRUE;
            }
            j++;
        }
        i++;
    }
    return p_boulder_flag;
}

// Check if player is trapped by boulders
int boulder_trap(struct location map[SIZE][SIZE], struct coordinate coord) {
    int check_boulder_trap = FALSE;
    // Corners, edges, centre
    if (coord.c_row == 0 && coord.c_col == 0) {
        if (map[coord.c_row + 1][coord.c_col].occupier == BOULDER_TYPE && 
        map[coord.c_row][coord.c_col + 1].occupier == BOULDER_TYPE) {
            check_boulder_trap = TRUE;
        } 
    } 
    if (coord.c_row == 0 && coord.c_col == 7) {
        if (map[coord.c_row + 1][coord.c_col].occupier == BOULDER_TYPE && 
        map[coord.c_row][coord.c_col - 1].occupier == BOULDER_TYPE) {
            check_boulder_trap = TRUE;
        } 
    } 
    if (coord.c_row == 7 && coord.c_col == 7) {
        if (map[coord.c_row - 1][coord.c_col].occupier == BOULDER_TYPE && 
        map[coord.c_row][coord.c_col - 1].occupier == BOULDER_TYPE) {
            check_boulder_trap = TRUE;
        } 
    } 
    if (coord.c_row == 7 && coord.c_col == 0) {
        if (map[coord.c_row - 1][coord.c_col].occupier == BOULDER_TYPE && 
        map[coord.c_row][coord.c_col + 1].occupier == BOULDER_TYPE) {
            check_boulder_trap = TRUE;
        } 
    } 
    if (coord.c_row == 0 && coord.c_col > 0 && coord.c_col < 7) {
        if (map[coord.c_row][coord.c_col - 1].occupier == BOULDER_TYPE &&
        map[coord.c_row][coord.c_col + 1].occupier == BOULDER_TYPE && 
        map[coord.c_row + 1][coord.c_col].occupier == BOULDER_TYPE) {
            check_boulder_trap = TRUE;
        }
    } 
    if (coord.c_row > 0 && coord.c_row < 7 && coord.c_col == 0) {
        if (map[coord.c_row - 1][coord.c_col].occupier == BOULDER_TYPE &&
        map[coord.c_row + 1][coord.c_col].occupier == BOULDER_TYPE && 
        map[coord.c_row][coord.c_col + 1].occupier == BOULDER_TYPE) {
            check_boulder_trap = TRUE;
        }
    } 
    if (coord.c_row > 0 && coord.c_row < 7 && coord.c_col == 7) {
        if (map[coord.c_row - 1][coord.c_col].occupier == BOULDER_TYPE &&
        map[coord.c_row + 1][coord.c_col].occupier == BOULDER_TYPE && 
        map[coord.c_row][coord.c_col - 1].occupier == BOULDER_TYPE) {
            check_boulder_trap = TRUE;
        }
    } 
    if (coord.c_row == 7 && coord.c_col > 0 && coord.c_col < 7) {
        if (map[coord.c_row][coord.c_col - 1].occupier == BOULDER_TYPE &&
        map[coord.c_row][coord.c_col + 1].occupier == BOULDER_TYPE && 
        map[coord.c_row - 1][coord.c_col].occupier == BOULDER_TYPE) {
            check_boulder_trap = TRUE;
        }
    } 
    if (coord.c_row > 0 && coord.c_row < 7 && 
    coord.c_col > 0 && coord.c_col < 7) {
        if (map[coord.c_row + 1][coord.c_col].occupier == BOULDER_TYPE &&
        map[coord.c_row - 1][coord.c_col].occupier == BOULDER_TYPE && 
        map[coord.c_row][coord.c_col + 1].occupier == BOULDER_TYPE &&
        map[coord.c_row][coord.c_col - 1].occupier == BOULDER_TYPE) {
            check_boulder_trap = TRUE;
        }
    }
    return check_boulder_trap;
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
