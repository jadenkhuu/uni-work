// Multiply a float by 2048 using bit operations only

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>

#include "floats.h"

// float_2048 is given the bits of a float f as a uint32_t
// it uses bit operations and + to calculate f * 2048
// and returns the bits of this value as a uint32_t
//
// if the result is too large to be represented as a float +inf or -inf is returned
//
// if f is +0, -0, +inf or -inf, or Nan it is returned unchanged
//
// float_2048 assumes f is not a denormal number
//
float_components_t float_bits(uint32_t f) {
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
// I believe Nan is all 1’s in exponent, 
// and anything except 0 in fraction, 
// and infinity is all 1’s in exponent and all 0’s in fraction 

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

// float_2048 is given the bits of a float f as a uint32_t
// it uses bit operations and + to calculate f * 2048
// and returns the bits of this value as a uint32_t
//
// if the result is too large to be represented as a float +inf or -inf is returned
//
// if f is +0, -0, +inf or -inf, or Nan it is returned unchanged
//
// float_2048 assumes f is not a denormal number
//
uint32_t float_2048(uint32_t f) {
    float_components_t float_comp = float_bits(f);

    if (is_nan(float_comp)) {
        return f; 
    }
    if (is_zero(float_comp)) {
        return f; 
    }
    if (is_negative_infinity(float_comp)) {
        return f;
    }
    if (is_positive_infinity(float_comp)) {
        return f;
    }

    float_comp.exponent = float_comp.exponent + 11; 
    
    uint32_t answer; 
    if (float_comp.exponent > ((2*2*2*2*2*2*2*2) - 1)) {
        
        answer = ((1 << 8) - 1) << 23; 

        return answer; 

    }
    uint32_t mask = 0; 
    answer = mask | float_comp.fraction;

    mask = (float_comp.exponent << 23);
    answer = answer | mask;

    mask = (float_comp.sign << 31);
    answer = answer | mask;

    return answer;
}
