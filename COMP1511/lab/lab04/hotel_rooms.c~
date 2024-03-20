// Using struct arrays to help with travel plans
// hotel_rooms.c
//
// This program was written by Jaden Khuu (z5416824)
// on 11/03/22
//           
// Scans in a certain number of hotel rooms and prints their details.

#include <stdio.h>

#define MAX_ROOMS 50

struct hotel_room {
    // TODO: fill this in with the details of each hotel room
    int room_number;
    double room_price;
};

// Prints the room in the correct format when given the room_number
// and price of it.
void print_room(int room_number, double price);

int main(void) {

    int number_of_rooms;
    struct hotel_room details[MAX_ROOMS];

    printf("How many rooms? ");
    // TODO: scan in how many rooms in the hotel
    scanf("%d", &number_of_rooms);

    printf("Enter hotel rooms:\n");
    // TODO: scan in the details of each hotel room
    int i = 0;

    while (i < number_of_rooms) {
        scanf("%d %lf", &details[i].room_number, &details[i].room_price);
        
        i++;
    }

    i = 0;

    printf("Room List:\n");
    // TODO: print all the rooms scanned in
    while (i < number_of_rooms) {
        print_room(details[i].room_number, details[i].room_price);

        i++;
    }

    return 0;
}

// Prints the room in the correct format when given the room_number
// and price of it.
//
// Takes in:
// - `room_number` -- The hotel room's room number.
// - `price` -- How much the hotel room costs.
//
// Returns: nothing.
void print_room(int room_number, double price) {

    printf("Room %d @ $%.2lf\n", room_number, price);

    return;
}
