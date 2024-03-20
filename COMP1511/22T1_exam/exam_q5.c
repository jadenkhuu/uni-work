#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define BUFFER_WIDTH        16
#define BUFFER_HEIGHT       5
#define MAX_STRING_LEN      100

struct screen_cell {
    char character;
    int start_marker;
};

// Your write_text_to_screen code here!
void write_text_to_screen(struct screen_cell screen[BUFFER_HEIGHT][BUFFER_WIDTH], char *text) {

    int i = 0;
    int j = 0;
    int x = 0;
    int y = 0;

    while (i < BUFFER_HEIGHT) {
        j = 0;
        while (j < BUFFER_WIDTH) {
            if (screen[i][j].start_marker == 1) {
                x = i;
                y = j;
            }
            j++;
        }
        i++;
    }

    int k = 0;
    while (x < BUFFER_HEIGHT) {
        if (y == BUFFER_WIDTH) {
            y = 0;
        }
        while (y < BUFFER_WIDTH) {
            if (k >= strlen(text)) {
                y++;
            }
            else {
                screen[x][y].character = text[k];
                y++;
                k++;
            }
        }
        x++;
    }
}

///////////// PROVIDED CODE ///////////////
// DO NOT MODIFY THESE FUNCTIONS

static void init_screen(struct screen_cell screen[BUFFER_HEIGHT][BUFFER_WIDTH], int starting_row, int starting_col);
static void print_screen(struct screen_cell screen[BUFFER_HEIGHT][BUFFER_WIDTH]);
static void trim_newline(char *string);
// we may use a different main function for marking
// please ensure your write_text_to_screen function is implemented. 
// DO NOT MODIFY THIS MAIN FUNCTION
int main(int argc, char *argv[])
{

	if ( argc < 3 ) {
		fprintf(stderr, "ERROR: Not enough arguments!\n");
		fprintf(stderr, "Usage ./exam_q5 start_row start_col\n");
		fprintf(stderr, "You do not have to handle this case\n");
		exit(1);
		return 1;
	}
	
	int start_row = atoi(argv[1]);
	int start_col = atoi(argv[2]);
	if (
		start_row >= BUFFER_HEIGHT || start_row < 0 || 
		start_col >= BUFFER_WIDTH || start_row < 0
	) {
		fprintf(stderr, "ERROR: Start row and column are too big or too small!\n");
		fprintf(stderr, "The max row is 4, and the max column is 15\n");
        fprintf(stderr, "You do not have to handle this case\n");

		exit(1);
       	return 1;
	}
    
    struct screen_cell screen[BUFFER_HEIGHT][BUFFER_WIDTH];
    init_screen(screen, start_row, start_col);
    
    printf("Enter Text: ");
    char text[MAX_STRING_LEN], *result;

    if ((result = fgets(text, MAX_STRING_LEN, stdin)) != NULL) {
        trim_newline(text);
        write_text_to_screen(screen, text);
        print_screen(screen);
    } else {
        fprintf(stderr, "ERROR: No text provided!\n");
        fprintf(stderr, "You do not have to handle this case\n");
        exit(1);
        return 1;
    }

    return 0;
}

void trim_newline(char *str) {
    int len = strlen(str);
    if (str[len - 1] == '\n') {
        str[len - 1] = '\0';
    }
}

void init_screen (
                    struct screen_cell screen[BUFFER_HEIGHT][BUFFER_WIDTH],
                    int starting_row, int starting_col
                 )
{
                 
    for (int row = 0; row < BUFFER_HEIGHT; row++) {
        for (int col = 0; col < BUFFER_WIDTH; col++) {
            screen[row][col].character = ' ';
            screen[row][col].start_marker = 0;
            if (row == starting_row && col == starting_col) {
                screen[row][col].start_marker = 1;
            }
        }
    }
}

void print_screen(struct screen_cell screen[BUFFER_HEIGHT][BUFFER_WIDTH]) {
    printf("\n");
    // top border
    for (int i = 0; i < BUFFER_WIDTH + 2; i++) {
        printf("-");
    }
    printf("\n");

    for (int row = 0; row < BUFFER_HEIGHT; row++) {
        // left border
        printf("|");
        for (int col = 0; col < BUFFER_WIDTH; col++) {       
            printf("%c", screen[row][col].character);
        }
        // right border
        printf("|");
        printf("\n");
    }

    // bottom border
    for (int i = 0; i < BUFFER_WIDTH + 2; i++) {
        printf("-");
    }
    printf("\n");
}
