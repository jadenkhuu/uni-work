#include "float_exp.h"

// given the 32 bits of a float return the exponent
uint32_t float_exp(uint32_t f) {

    uint32_t mask = 0x00FF;
    uint32_t exponent = mask & (f >> 23);


    return exponent; // REPLACE ME WITH YOUR CODE
}
