#include "bit_rotate.h"

// return the value bits rotated left n_rotations
uint16_t bit_rotate(int n_rotations, uint16_t bits) {

    // uint16_t rotate = (bits << n_rotations);

    int n;
    if (n_rotations >= 0) {
        n = n_rotations % 16;
        return (bits << n) | ((bits >> (16 - n)) & 0xffff); 
    }
    if (n_rotations < 0) {
        n = abs(n_rotations) % 16;
        return (bits >> n) | ((bits << (16 - n)) & 0xffff); 
    }
    

    //REPLACE ME WITH YOUR CODE
}
