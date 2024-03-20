// CS Airline
// cs_airline.c
//
// This program was written by Jaden Khuu (z5416824)
// on 09/04/22
//
// Version 1.0.0 2022-04-02: Initial Release.
// Version 1.0.1 2022-04-06: Fixed style issue with help function.
//
// This program is a booking system capable of managing a single flight with
// multiple stops.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "manifest.h"

////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////  CONSTANTS  /////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// The buffer length is used for reading a single line
#define MAX_STRING_LEN 100

#define COMMAND_PRINT_HELP "help"
#define COMMAND_PRINT_ROUTE "print"
#define COMMAND_SUBROUTE "subroute"
#define COMMAND_CHANGE_ORIGIN "change_origin"
#define COMMAND_BYPASS "bypass"
#define COMMAND_EMERGENCY "emergency"
#define COMMAND_CANCEL "cancel"
#define COMMAND_REVERSE "reverse"
#define COMMAND_ADD_PERSON "add_person"
#define COMMAND_PRINT_MANIFEST "print_manifest"
#define COMMAND_FLIGHT_STATS "stats"

// TODO: you may choose to add additional #defines here.
#define TRUE 1
#define FALSE 0
#define CODE_SIZE 5

////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////  STRUCTS  //////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// Airports represent 'stops' along the flight path
// These are the 'nodes' of the linked list
struct airport {
    char code[MAX_STRING_LEN];
    int arrival_time;
    int departure_time;
    struct airport *next_airport;
    struct manifest *manifest;
};

// Root flight structure
// This stores the state of the current flight
// (i.e. the head and tail of the linked list)
struct flight {
    struct airport *origin_airport;
    struct airport *dest_airport;
};

////////////////////////////////////////////////////////////////////////////////
/////////////////////////////  FUNCTION PROTOTYPES  ////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// Helper Functions
void remove_newline(char input[MAX_STRING_LEN]);
void do_print_help(void);

void interpret_line(
    char buffer[MAX_STRING_LEN],
    int *val0,
    int *val1,
    char word[MAX_STRING_LEN]
);

// Stage 1 Functions
struct flight *setup_flight(void);
void do_print_airports(struct flight *flight);
void print_one_airport(struct airport *ap);
struct airport *create_airport(
    char code[MAX_STRING_LEN],
    int arrival_time,
    int departure
);

void append_airport (
    struct flight *flight, 
    char code[MAX_STRING_LEN],
    int time_arrival,
    int time_departure
);

// Stage 2 Functions
void do_subroute(struct flight *flight);
void do_change_origin(struct flight *flight);

void do_bypass(struct flight *flight);

// Stage 3 Functions
void do_emergency(struct flight *flight);
void do_cancel(struct flight *flight);
void do_reverse(struct flight *flight);

// Stage 4 Functions
void initialise_manifest(struct flight *flight);
void do_manifest_add_person(struct flight *flight);
void do_print_manifest(struct flight *flight);

// TODO: Your functions prototypes here

////////////////////////////////////////////////////////////////////////////////
//////////////////////////  FUNCTION IMPLEMENTATIONS  //////////////////////////
////////////////////////////////////////////////////////////////////////////////

int main(void) {

    // Stage 1.1
    // TODO: Complete the setup_flight function below
    struct flight *flight = setup_flight();
    initialise_manifest(flight);

    // TODO: Fill out the while loop with relevant commands & functions
    // Move into and stay in operational mode until CTRL+D
    printf("Enter Command: ");
    char command_line[MAX_STRING_LEN];
    while (fgets(command_line, MAX_STRING_LEN, stdin) != NULL) {

        remove_newline(command_line);

        if (strcmp(COMMAND_PRINT_HELP, command_line) == 0) {
            // A help command we have implemented for you.
            do_print_help();
        } else if (strcmp(COMMAND_PRINT_ROUTE, command_line) == 0) {
            // Stage 1.2 - TODO: Complete this function below
            do_print_airports(flight);
        } else if (strcmp(COMMAND_SUBROUTE, command_line) == 0) {
            // Stage 2.1 - TODO: Complete this function below
            do_subroute(flight);
        } else if (strcmp(COMMAND_CHANGE_ORIGIN, command_line) == 0) {
            // Stage 2.2 - TODO: Complete this function below
            do_change_origin(flight);
        } else if (strcmp(COMMAND_BYPASS, command_line) == 0) {
            do_bypass(flight);
        } else if (strcmp(COMMAND_EMERGENCY, command_line) == 0) {
            do_emergency(flight);
        } else if (strcmp(COMMAND_CANCEL, command_line) == 0) {
            do_cancel(flight);
            flight = setup_flight();
            initialise_manifest(flight);
        } else if (strcmp(COMMAND_REVERSE, command_line) == 0) {
            do_reverse(flight);
        } else if (strcmp(COMMAND_ADD_PERSON, command_line) == 0) {
            do_manifest_add_person(flight);
        } else if (strcmp(COMMAND_PRINT_MANIFEST, command_line) == 0)  {
            do_print_manifest(flight);
        }

        // TODO: Add more conditions here for the remaining commands
            
        printf("Enter Command: ");
    }


    struct airport *current = flight->origin_airport;
    while (current != NULL) {
        delete_manifest(current->manifest);
        current = current->next_airport;
    }
    
    current = flight->origin_airport;
    struct airport *temp = NULL;  

    while (current != NULL) {
        temp = current;
        current = current->next_airport;
        free(temp);
    }

    free(flight);

    printf("Goodbye!\n");

    return 0;
}

// Stage 1.1
// Creates and initialises a flight by asking the user for input.
// Parameters:
//     None
// Returns:
//     Pointer to the malloc'd flight
struct flight *setup_flight(void) {

    // Create a new, empty flight
    struct flight *new_flight = malloc(sizeof(struct flight));
    new_flight->origin_airport = NULL;
    new_flight->dest_airport = NULL;

    // Read number of stops using fgets
    printf("How many stops will be enroute? ");
    char input_line[MAX_STRING_LEN] = {0};
    fgets(input_line, MAX_STRING_LEN, stdin);

    // Convert the string to a number
    int num_stops = 0;
    num_stops = atoi(input_line);

    // TODO: For each airport Use fgets to read a line and then use the
    //       provided interpret_line() function to extract information
    int time_arrival;
    int time_departure = 0;
    char code[MAX_STRING_LEN];
    
    int i = 0;
    while (i < num_stops) {
        int valid_stop = TRUE;
        int prev_departure_time = time_departure;

        // Take in airport details
        char buffer[MAX_STRING_LEN];
        fgets(buffer, MAX_STRING_LEN, stdin);
        interpret_line(buffer, &time_arrival, &time_departure, code);

        // TODO: For each airport, check conditions and 
        //       add it to the end of the flight route
        if (time_arrival > time_departure) {
            printf("Departure time cannot be before the arrival time!\n");
            valid_stop = FALSE;
        }
        if (time_arrival < prev_departure_time) {
            printf("New arrival time cannot be before the previous departure time\n");
            valid_stop = FALSE;
        }

        // Add to end of linked list if it is a valid airport
        if (valid_stop == TRUE) {
            // Add to end of list
            append_airport(new_flight, code, time_arrival, time_departure);
        }
        
        i++;
    }

    // TODO: Change the next line
    return new_flight;
}

// Creates a new airport node and adds it to the end of the list
void append_airport (
    struct flight *flight, 
    char code[MAX_STRING_LEN],
    int time_arrival,
    int time_departure
) {

    struct airport *new_airport = create_airport(code, time_arrival, time_departure);
    struct airport *current = flight->origin_airport;

    if (flight->origin_airport == NULL) {
        flight->origin_airport = new_airport;
        flight->dest_airport = new_airport;
        return;
    }
    
    // traverse to end
    while (current->next_airport != NULL) {
        current = current->next_airport;
    }

    current->next_airport = new_airport;
    flight->dest_airport = new_airport;

    return;

}

// Stage 1.2
// Given a pointer to a flight struct, prints all the info about the route
// Parameters:
//     flight = the flight which contains the flight route to print
// Returns:
//     None
void do_print_airports(struct flight *flight) {

    // TODO: Loop through the flight route and call 
    //       the provided print_one_airport function
    struct airport *current = flight->origin_airport;

    printf("ROUTE:\n");

    while (current != NULL) {
        print_one_airport(current);
        current = current->next_airport;
    }

}

// Stage 1.2
// PROVIDED FUNCTION - DO NOT CHANGE
// Given a pointer to an airport struct,
// prints all the info about the airport
// Parameters:
//     ap = the airport to print
// Returns:
//     None
void print_one_airport(struct airport *ap) {
    printf(
        "%8s:    %04d -> %04d\n",
       ap->code,
       ap->arrival_time,
       ap->departure_time
    );
}

// Stage 1.1
// Given the information about a new airport,
// Uses `malloc` to create memory for it and returns a pointer to
// that memory.
// Parameters:
//     code            = the airport ICAO code
//     arrival_time    = the arrival time of the new airport
//     departure_time  = the departure time of the new airport
// Returns:
//     A pointer to the malloc'd struct airport
struct airport *create_airport(
    char code[MAX_STRING_LEN],
    int arrival_time,
    int departure_time)
{
    // Malloc new struct
    struct airport *new_airport = malloc(sizeof(struct airport));

    // initialise airport fields
    strcpy(new_airport->code, code);
    new_airport->arrival_time = arrival_time;
    new_airport->departure_time = departure_time;
    new_airport->next_airport = NULL;

    return new_airport;
}

// TODO Stage 2.1 Calculate subroute
// Calculates the time which an airport would take
void do_subroute(struct flight *flight) {
    
    char code[5];
    struct airport *current = flight->origin_airport;

    printf("Enter airport code: ");
    fgets(code, 5, stdin);

    int subroute_start = current->departure_time;

    while (current->next_airport != NULL) { 
        current = current->next_airport;
        if (strcmp(current->code, code) == 0) {

            // Hours and minutes calculation from pseudo code
            int subroute_end = current->arrival_time;

            int hours = (subroute_end / 100) - (subroute_start / 100);
            int mins = (subroute_end % 100) - (subroute_start % 100);

            int minute_diff = hours * 60 + mins;

            if (minute_diff < 0) {
                minute_diff *= -1;
            }

            hours = minute_diff / 60;
            mins = minute_diff % 60;

            printf("Time for subroute: %d hrs & %d mins\n", hours, mins);

            getchar();
            return;
        }
    }
    
    printf("Desired subroute destination does not exist!\n");
    
    getchar();
    return;
}

// TODO
// Stage 2.2 Change origin
// Changes origin airport of the flight linked list
void do_change_origin(struct flight *flight) {
    char buffer[MAX_STRING_LEN];
    int arrival_time;
    int departure_time;
    char code[MAX_STRING_LEN];

    struct airport *current = flight->origin_airport;

    printf("Enter new origin info: ");
    fgets(buffer, MAX_STRING_LEN, stdin);
    interpret_line(buffer, &arrival_time, &departure_time, code);
    
    while (current->next_airport != NULL) {
        
        if (strcmp(current->code, code) == 0) {
            printf("New airport code is not unique!\n");

            return;
        }
        
        current = current->next_airport;
    }

    struct airport *new_origin = 
    create_airport(code, arrival_time, departure_time);

    if (departure_time < arrival_time) {
        printf("Departure time cannot be before the arrival time!\n");
        return;
    }

    if (new_origin->departure_time > flight->origin_airport->arrival_time) {
        printf("Departure of new origin cannot be ");
        printf("after the arrival time of the next airport!\n");
        return;
    }

    new_origin->next_airport = flight->origin_airport;
    flight->origin_airport = new_origin;
    new_origin->manifest = NULL;

    printf("Added: %s\n", code);

    return;
}

// Stage 2.3 Bypass airport
// Removes/bypasses an airport from the flight
void do_bypass(struct flight *flight) {

    char code[CODE_SIZE];
    int check_existing = FALSE;

    struct airport *current = flight->origin_airport;
    
    printf("Enter airport code: ");
    fgets(code, CODE_SIZE, stdin);
    getchar();

    // Check if entered code exists in list
    while (current->next_airport != NULL) {
        if (strcmp(current->code, code) == 0) {
            check_existing = TRUE;
        }

        current = current->next_airport;
    }
    
    current = flight->origin_airport;

    // Apply check_existing 
    if (check_existing == FALSE) {
        printf("No airport of that code exists!\n");
        return;
    }

    if (check_existing == TRUE) {
        while (strcmp(current->next_airport->code, code) != 0) {
            current = current->next_airport;
        }
        struct airport *new_next = current->next_airport->next_airport;
        delete_manifest(current->next_airport->manifest);
        free(current->next_airport);

        current->next_airport = new_next;
        return;
    }

    return;
}

// Stage 3.1 Emergency Landing
// Calculates if an emergency landing affects the flight 
// and creates ALT0 airport if it does
void do_emergency(struct flight *flight) {
    
    char buffer[MAX_STRING_LEN];
    int emergency_input;
    int emergency_time;
    struct airport *current = flight->origin_airport;
    struct airport *last = flight->origin_airport;

    printf("How long until emergency: ");
    fgets(buffer, MAX_STRING_LEN, stdin);
    
    //
    emergency_input = atoi(buffer);

    if (emergency_input == 0) {
        int e_departure = 0000;
        flight->origin_airport->departure_time = e_departure;
        
        flight->dest_airport = flight->origin_airport;   
        flight->origin_airport->next_airport = NULL;
        
        current = flight->origin_airport->next_airport;

        while (current != NULL) {
            delete_manifest(current->manifest);
            current = current->next_airport;
        }

        current = flight->origin_airport;
        struct airport *temp = NULL;
        while (current != NULL) {
            temp = current;
            current = current->next_airport;
            delete_manifest(temp->manifest);
            free(temp);
        }
        return;
    }

    emergency_input = emergency_input * 100;
    emergency_time = flight->origin_airport->departure_time + emergency_input;

    while (last->next_airport != NULL) {
        last = last->next_airport;
    }
    if (last->arrival_time < emergency_time) {
        printf("Flight was safe!\n");
        return;
    }

    while (current->next_airport->arrival_time < emergency_time &&
    emergency_time != current->next_airport->arrival_time) {
        current = current->next_airport;
    }

    struct airport *temp_current = current;
    char e_code[] = "ALT0";
    int e_departure = 0000;
    
    struct airport *emergency_airport = 
    create_airport(e_code, emergency_time, e_departure);

    current = current->next_airport;
    struct airport *temp = NULL;
    while (current != NULL) {
        temp = current;
        current = current->next_airport;
        delete_manifest(temp->manifest);
        free(temp);
    }

    temp_current->next_airport = emergency_airport;
    flight->dest_airport = emergency_airport;
    emergency_airport->manifest = NULL;

    return;
}

// Stage 3.2 Cancel flight 
// Cancels the current flight and promts user to create a new flight.
void do_cancel(struct flight *flight) {

    struct airport *current = flight->origin_airport;
    struct airport *temp = NULL;
    
    while (current != NULL) {
        temp = current;
        current = current->next_airport;
        delete_manifest(temp->manifest);
        free(temp);
    }

    free(flight);

    printf("Flight cancelled. Now accepting a new flight:\n");

    return;
}

// Stage 3.3 Reverse flight
// Reverses the airports in the flight
void do_reverse(struct flight *flight) {

    struct airport *prev_airport = NULL;
    struct airport *current = flight->origin_airport;
    struct airport *next;

    while (current != NULL) {
        next = current->next_airport;
        current->next_airport = prev_airport;
        prev_airport = current;
        current = next;
    }
    
    flight->origin_airport = prev_airport;
    
    return;
}

// Stage 4 Initialising airport->manifest
void initialise_manifest(struct flight *flight) {

    struct airport *current = flight->origin_airport;

    while (current != NULL) {
        
        current->manifest = NULL;

        current = current->next_airport;
    }
}


// Stage 4.1 Add people to airport
// Adds people to manifest of each airport

void do_manifest_add_person(struct flight *flight) {
    char code[CODE_SIZE];
    char name[MAX_NAME_LEN];
    int exists = FALSE;
    
    double weight;
    char buffer[MAX_STRING_LEN];

    printf("Enter the airport code: ");
    fgets(code, CODE_SIZE, stdin);
    getchar();

    struct airport *current = flight->origin_airport;
    while (current != NULL) {
        if (strcmp(code, current->code) == 0) {
            exists = TRUE;
        }
        current = current->next_airport;
    }
    
    printf("Enter the passenger name: ");
    fgets(name, MAX_NAME_LEN, stdin);

    printf("Enter the passenger weight: ");
    fgets(buffer, MAX_STRING_LEN, stdin);
    weight = atof(buffer);

    if (exists == FALSE) {
        printf("No airport of that code exists!\n");
    
        return;
    }

    current = flight->origin_airport;
    while (current != NULL) {
        if (strcmp(code, current->code) == 0 && current->manifest != NULL) {
            add_person(current->manifest, name, weight);
        }
        else if (strcmp(code, current->code) == 0) {
            current->manifest = create_manifest();
            add_person(current->manifest, name, weight);
        }

        current = current->next_airport;
    }
}

// Stage 4.2 Print manifest
// Prints the details of people in the manifests of each airport
void do_print_manifest(struct flight *flight) {
    char code[CODE_SIZE];
    int exists = FALSE;

    printf("Enter the airport code: ");
    fgets(code, CODE_SIZE, stdin);
    getchar();

    struct airport *current = flight->origin_airport;
    while (current != NULL) {
        if (strcmp(code, current->code) == 0) {
            exists = TRUE;
            print_manifest(current->manifest);
        }
        current = current->next_airport;
    }
       
    if (exists == FALSE) {
        printf("No airport of that code exists!\n");
    }

}


////////////////////////////////////////////////////////////////////////////////
//////////////////////////////  HELPER FUNCTIONS  //////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// Helper Function
// You likely do not need to change this function.
//
// Given a raw string will remove and first newline it sees.
// The newline character wil be replaced with a null terminator ('\0')
// Parameters:
//     flight  = the flight to move people along of
// Returns:
//     None
void remove_newline(char input[MAX_STRING_LEN]) {

    // Find the newline or end of string
    int index = 0;
    while (input[index] != '\n' && input[index] != '\0') {
        index++;
    }
    // Goto the last position in the array and replace with '\0'
    // Note: will have no effect if already at null terminator
    input[index] = '\0';
}


// Helper Function
// You DO NOT NEED TO UNDERSTAND THIS FUNCTION, and will not need to change it.
//
// Given a raw string in the following foramt: [integer0] [integer1] [string]
// This function will extract the relevant values into the given pointer variables.
// The function will also remove any newline characters.
//
// For example, if given: "0135 0545 YSSY"
// The function will put the integer values
//     135 into the val1 pointer
//     545 into the val2 pointer
// And will copy a null terminated string
//     "YSSY" into the 'word' array
//
// If you are interested, `strtok` is a function which takes a string,
// and splits it up into before and after a "token" (the second argument)
//
// Parameters:
//     buffer  = A null terminated string in the following format
//               [integer0] [integer1] [string]
//     val0    = A pointer to where [integer0] should be stored
//     val1    = A pointer to where [integer1] should be stored
//     word    = An array for the [string] to be copied into
// Returns:
//     None
void interpret_line(
    char buffer[MAX_STRING_LEN],
    int *val0,
    int *val1,
    char word[MAX_STRING_LEN]
) {

    // Remove extra newline
    remove_newline(buffer);

    // Extract value 1 as int
    char *val0_str = strtok(buffer, " ");
    if (val0_str != NULL) {
        *val0 = atoi(val0_str);
    } else {
        *val0 = -1;
    }

    // Extract value 2 as int
    char *val1_str = strtok(NULL, " ");
    if (val1_str != NULL) {
        *val1 = atoi(val1_str);
    } else {
        *val1 = -1;
    }

    char *word_str = strtok(NULL, " ");
    if (word_str != NULL) {
        // Extract word
        strcpy(word, word_str);
    }

    if (val0_str == NULL || val1_str == NULL || word_str == NULL) {
        // If any of these are null, there were not enough words.
        printf("Could not properly interpret line: %s.\n", buffer);
    }

}

void do_print_help(void) {
    printf("+-----------------+-------------------------------------------+\n");
    printf("|Command Name     | How to Use                                |\n");
    printf("+=================+===========================================+\n");
    printf("|PRINT ROUTE      | Enter command: print                      |\n");
    printf("+-----------------+-------------------------------------------+\n");
    printf("|SUBROUTE         | Enter command: subroute                   |\n");
    printf("|                 | Enter airport code: [AIRPORT CODE]        |\n");
    printf("+-----------------+-------------------------------------------+\n");
    printf("|CHANGE ORIGIN    | Enter command: change_origin              |\n");
    printf("|                 | Enter new origin info: [ARRIVAL TIME]...  |\n");
    printf("|                 |     ... [DEPARTURE TIME] [AIRPORT CODE]   |\n");
    printf("+-----------------+-------------------------------------------+\n");
    printf("|BYPASS           | Enter command: bypass                     |\n");
    printf("|                 | Enter airport code: [AIRPORT CODE]        |\n");
    printf("+-----------------+-------------------------------------------+\n");
    printf("|EMERGENCY        | Enter command: emergency                  |\n");
    printf("|                 | How long until emergency: [TIME IN HOURS] |\n");
    printf("+-----------------+-------------------------------------------+\n");
    printf("|CANCEL           | Enter command: cancel                     |\n");
    printf("+-----------------+-------------------------------------------+\n");
    printf("|REVERSE          | Enter command: reverse                    |\n");
    printf("+-----------------+-------------------------------------------+\n");
    printf("|                 | Enter command: add_person                 |\n");
    printf("|ADD PERSON       | Enter the airport code: [AIRPORT CODE]    |\n");
    printf("|                 | Enter the passenger name: [NAME]          |\n");
    printf("|                 | Enter the passenger weight: [WEIGHT]      |\n");
    printf("+-----------------+-------------------------------------------+\n");
    printf("|PRINT MANIFEST   | Enter command: print_manifest             |\n");
    printf("|                 | Enter the airport code: [AIRPORT CODE]    |\n");
    printf("+-----------------+-------------------------------------------+\n");
    printf("|FLIGHT STATISTICS| Enter command: stats                      |\n");
    printf("+-----------------+-------------------------------------------+\n");
}

