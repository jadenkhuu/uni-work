// Extract the 3 parts of a float using bit operations only

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>

#include "floats.h"

// separate out the 3 components of a float
float_components_t float_bits(uint32_t f) {
    // PUT YOUR CODE HERE
    float_components_t float_groups;
    uint32_t sign = 1; 
    float_groups.sign = sign & (f >> 31);  

    uint32_t exp = 0x00FF; 
    float_groups.exponent = exp & (f >> 23);

    uint32_t frac = (1 << 23) - 1; 
    float_groups.fraction = frac & f; 

    return float_groups;

}

// given the 3 components of a float
// return 1 if it is NaN, 0 otherwise
int is_nan(float_components_t f) {
    // PUT YOUR CODE HERE
    uint32_t mask = 0x00FF; 
    uint32_t mask_1 = 0; 

    if ((mask & f.exponent) == mask && (mask_1 | f.fraction) != mask_1) {
        return 1;
    }

    return 0;
}

// given the 3 components of a float
// return 1 if it is inf, 0 otherwise
int is_positive_infinity(float_components_t f) {
    // PUT YOUR CODE HERE
    uint32_t mask = (1 << 8) - 1; 
    uint32_t mask_1 = 0; 

    if ((mask & f.exponent) == mask && (mask_1 | f.fraction) == 0) {
        if (f.sign == 0) {
            return 1;
        } 
        
        
    }
    return 0;
}

// given the 3 components of a float
// return 1 if it is -inf, 0 otherwise
int is_negative_infinity(float_components_t f) {
    // PUT YOUR CODE HERE
    uint32_t mask = (1 << 8) - 1; 
    uint32_t mask_1 = 0; 

    if ((mask & f.exponent) == mask && (mask_1 | f.fraction) == 0) {
        if (f.sign == 1) {
            return 1;
        } 
    }

    return 0;
}

// given the 3 components of a float
// return 1 if it is 0 or -0, 0 otherwise
int is_zero(float_components_t f) {
    // PUT YOUR CODE HERE
    if (f.fraction == 0) {
        if (f.exponent == 0) {
            return 1;
        }
    }

    return 0;  
}