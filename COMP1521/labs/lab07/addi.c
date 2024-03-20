// generate the encoded binary for an addi instruction, including opcode and operands

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>

#include "addi.h"

// return the encoded binary MIPS for addi $t,$s, i
uint32_t addi(int t, int s, int i) {
    uint32_t answer;
    uint32_t mask = 0xFFFF;
    uint32_t mask_1 = i; 

    answer = mask & mask_1; 

    mask_1 = t << (32 - 16);

    answer = mask_1 | answer;

    mask_1 = s << (32 - 11);

    answer = mask_1 | answer;

    mask_1 = (1 << (32 - 3));

    answer = mask_1 | answer; 


    return answer;  

}
