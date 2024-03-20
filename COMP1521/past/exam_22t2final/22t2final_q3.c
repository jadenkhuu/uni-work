#include <stdint.h>

/**
 * Return the provided value but with its bytes reversed.
 *
 * For example, 22t2final_q3(0x12345678) => 0x78563412
 *
 * *Note* that your task is to
 * reverse the order of *bytes*,
 * *not* to reverse the order of bits.
 **/

uint32_t _22t2final_q3(uint32_t value) {
    uint32_t mask = (1 << 8) - 1;

    uint32_t section1 = (value >> 24) & mask;
    uint32_t section2 = (value >> 8) & (mask << 8);
    uint32_t section3 = (value << 8) & (mask << 16);
    uint32_t section4 = (value << 24) & (mask << 24);

    uint32_t final = (section1 | section2 | section3 | section4);

    return final;
}

