#include "sign_flip.h"

// given the 32 bits of a float return it with its sign flipped
uint32_t sign_flip(uint32_t f) {

    unsigned long long value = 1;

    f = f ^ (value << 31);

    return f;
}
