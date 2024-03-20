// COMP1521 22T3 Assignment 2: mipsc -- a MIPS simulator
// starting point code v1.0 - 24/10/22


// PUT YOUR HEADER COMMENT HERE
// mipsc.c - Jaden Khuu (z5416824)

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

// ADD ANY ADDITIONAL #include HERE

#define MAX_LINE_LENGTH 256
#define INSTRUCTIONS_GROW 64

// ADD ANY ADDITIONAL #defines HERE
#define TRUE 1
#define FALSE 0

void execute_instructions(uint32_t n_instructions,
                          uint32_t instructions[],
                          int trace_mode);
char *process_arguments(int argc, char *argv[], int *trace_mode);
uint32_t *read_instructions(char *filename, uint32_t *n_instructions_p);
uint32_t *instructions_realloc(uint32_t *instructions, uint32_t n_instructions);

// ADD ANY ADDITIONAL FUNCTION PROTOTYPES HERE

int check_addi(uint32_t instructions);
void do_addi(uint32_t instructions, int *registersArray, int trace_mode, uint32_t pc);

int check_syscall(uint32_t instructions);
void do_syscall(uint32_t instructions, int *registersArray, int trace_mode, uint32_t pc);

int check_add(uint32_t instructions);
void do_add(uint32_t instructions, int *registersArray, int trace_mode, uint32_t pc);

int check_sub(uint32_t instructions);
void do_sub(uint32_t instructions, int *registersArray, int trace_mode, uint32_t pc);

int check_slt(uint32_t instructions);
void do_slt(uint32_t instructions, int *registersArray, int trace_mode, uint32_t pc);

int check_mul(uint32_t instructions);
void do_mul(uint32_t instructions, int *registersArray, int trace_mode, uint32_t pc); 

int check_lui(uint32_t instructions);
void do_lui(uint32_t instructions, int *registersArray, int trace_mode, uint32_t pc);

int check_bne(uint32_t instructions);
uint32_t do_bne(uint32_t instructions, int *registersArray, int trace_mode, uint32_t pc, uint32_t n_instructions);

int check_beq(uint32_t instructions);
uint32_t do_beq(uint32_t instructions, int *registersArray, int trace_mode, uint32_t pc, uint32_t n_instructions); 

int check_ori(uint32_t instructions);
void do_ori(uint32_t instructions, int *registersArray, int trace_mode, uint32_t pc);

int check_mfhi(uint32_t instructions);
void do_mfhi(uint32_t instructions, int *registersArray, int trace_mode, uint32_t pc, int64_t *hiLo);

int check_mflo(uint32_t instructions);
void do_mflo(uint32_t instructions, int *registersArray, int trace_mode, uint32_t pc, int64_t *hiLo);

int check_mult(uint32_t instructions);
void do_mult(uint32_t instructions, int *registersArray, int trace_mode, uint32_t pc, int64_t *hiLo);

int check_div(uint32_t instructions);
void do_div(uint32_t instructions, int *registersArray, int trace_mode, uint32_t pc, int64_t *hiLo);


// YOU DO NOT NEED TO CHANGE MAIN
// but you can if you really want to
int main(int argc, char *argv[]) {
    int trace_mode;
    char *filename = process_arguments(argc, argv, &trace_mode);

    uint32_t n_instructions;
    uint32_t *instructions = read_instructions(filename, &n_instructions);

    execute_instructions(n_instructions, instructions, trace_mode);

    free(instructions);
    return 0;
}


// simulate execution of  instruction codes in  instructions array
// output from syscall instruction & any error messages are printed
//
// if trace_mode != 0:
//     information is printed about each instruction as it executed
//
// execution stops if it reaches the end of the array
void execute_instructions(uint32_t n_instructions,
                          uint32_t instructions[],
                          int trace_mode) {

    // REPLACE THIS FUNCTION WITH YOUR OWN IMPLEMENTATION
    int registersArray[32] = {};
    int64_t hiLo[2] = {};

    for (uint32_t pc = 0; pc < n_instructions; pc++) {
        registersArray[0] = 0;

        if (check_mul(instructions[pc]) == TRUE) {
            do_mul(instructions[pc], registersArray, trace_mode, pc);
        } 
        else if (check_lui(instructions[pc]) == TRUE) {
            do_lui(instructions[pc], registersArray, trace_mode, pc);
        }
        else if (check_ori(instructions[pc]) == TRUE) {
            do_ori(instructions[pc], registersArray, trace_mode, pc);
        } 
        else if (check_addi(instructions[pc]) == TRUE) {
            do_addi(instructions[pc], registersArray, trace_mode, pc);
        } 
        else if (check_bne(instructions[pc]) == TRUE) {
            pc = do_bne(instructions[pc], registersArray, trace_mode, pc, n_instructions);
        }
        else if (check_beq(instructions[pc]) == TRUE) {
            pc = do_beq(instructions[pc], registersArray, trace_mode, pc, n_instructions);
        } 
        else if (check_div(instructions[pc]) == TRUE) {
            do_div(instructions[pc], registersArray, trace_mode, pc, hiLo);
        }
        else if (check_mult(instructions[pc]) == TRUE) {
            do_mult(instructions[pc], registersArray, trace_mode, pc, hiLo);
        }
        else if (check_mflo(instructions[pc]) == TRUE) {
            do_mflo(instructions[pc], registersArray, trace_mode, pc, hiLo);
        }
        else if (check_mfhi(instructions[pc]) == TRUE) {
            do_mfhi(instructions[pc], registersArray, trace_mode, pc, hiLo);
        }
        else if (check_syscall(instructions[pc]) == TRUE) {
            do_syscall(instructions[pc], registersArray, trace_mode, pc);
        }
        else if (check_slt(instructions[pc]) == TRUE) {
            do_slt(instructions[pc], registersArray, trace_mode, pc);
        } 
        else if (check_sub(instructions[pc]) == TRUE) {
            do_sub(instructions[pc], registersArray, trace_mode, pc);
        } 
        else if (check_add(instructions[pc]) == TRUE) {
            do_add(instructions[pc], registersArray, trace_mode, pc);
        } 
        else {
            fprintf(stderr, "Invalid instruction %08X at PC = %u\n", instructions[pc], pc);
            break;
        }
    }
}

// ADD YOUR FUNCTIONS HERE

int check_mult(uint32_t instructions) {
    // 	000000sssssttttt0000000000011000
    uint32_t mask = (1 << 4) | (1 << 3);
    uint32_t check = mask & instructions;

    if (check == mask) {
        return TRUE;
    }
    return FALSE;
}
void do_mult(uint32_t instructions, int *registersArray, int trace_mode, uint32_t pc, int64_t *hiLo) {
    uint32_t mask = (1 << 5) - 1;
    int s = (instructions >> 21) & mask;
    int t = (instructions >> 16) & mask;

    int64_t answer = (int64_t) registersArray[s] * (int64_t) registersArray[t];


    hiLo[0] = answer >> 32;

    int64_t maskHiLo = (1ULL << 32) - 1;    
    int lo = answer & maskHiLo;
    hiLo[1] = lo;


    if (trace_mode == 1) {
        printf("%u: 0x%08X ", pc, instructions);
        printf("mult $%d, $%d\n", s, t);
        printf(">>> HI = %ld\n", hiLo[0]);
        printf(">>> LO = %ld\n", hiLo[1]);
    } 
}

int check_div(uint32_t instructions) {
    // 	000000sssssttttt0000000000011010
    uint32_t mask = (1 << 4) | (1 << 3) | (1 << 1);
    uint32_t check = mask & instructions;

    if (check == mask) {
        return TRUE;
    }
    return FALSE;
}
void do_div(uint32_t instructions, int *registersArray, int trace_mode, uint32_t pc, int64_t *hiLo) {
    uint32_t mask = (1 << 5) - 1;
    int s = (instructions >> 21) & mask;
    int t = (instructions >> 16) & mask;

    hiLo[0] = (int64_t) registersArray[s] % (int64_t) registersArray[t];
    hiLo[1] = (int64_t) registersArray[s] / (int64_t) registersArray[t];

    if (trace_mode == 1) {
        printf("%u: 0x%08X ", pc, instructions);
        printf("div  $%d, $%d\n", s, t);
        printf(">>> HI = %ld\n", hiLo[0]);
        printf(">>> LO = %ld\n", hiLo[1]);
    } 
}

// mfhi
int check_mfhi(uint32_t instructions){
    // 	0000000000000000ddddd00000010000
    uint32_t mask = 1 << 4;
    uint32_t check = mask & instructions;

    if (check == mask) {
        return TRUE;
    }
    return FALSE;
}
void do_mfhi(uint32_t instructions, int *registersArray, int trace_mode, uint32_t pc, int64_t *hiLo) {
    uint32_t mask = ((1 << 5) - 1);
    int d = (instructions >> 11) & mask;

    registersArray[d] = hiLo[0];
    if (trace_mode == 1) {
        printf("%u: 0x%08X ", pc, instructions);
        printf("mfhi $%d\n", d);
        printf(">>> $%d = %ld\n", d, hiLo[0]);
    } 
}

// mflo
int check_mflo(uint32_t instructions) {
    // 0000000000000000ddddd00000010010
    uint32_t mask = (1 << 4) | (1 << 1);
    uint32_t check = mask & instructions;

    if (check == mask) {
        return TRUE;
    }
    return FALSE;
}
void do_mflo(uint32_t instructions, int *registersArray, int trace_mode, uint32_t pc, int64_t *hiLo){
    uint32_t mask = ((1 << 5) - 1);
    int d = (instructions >> 11) & mask;

    registersArray[d] = hiLo[1];
    if (trace_mode == 1) {
        printf("%u: 0x%08X ", pc, instructions);
        printf("mflo $%d\n", d);
        printf(">>> $%d = %ld\n", d, hiLo[1]);
    } 
}

// syscall function
int check_syscall(uint32_t instructions) {
    // mask = 00000000000000000000000000001100
    uint32_t mask = (1 << 3) | (1 << 2);
    uint32_t check = mask & instructions;

    if (check == mask) {
        return TRUE;
    }
    return FALSE;
} 
void do_syscall(uint32_t instructions, int *registersArray, int trace_mode, uint32_t pc) {
    int v = registersArray[2]; 
    int a = registersArray[4];
    if (trace_mode == 1) {
        printf("%u: 0x%08X syscall\n", pc, instructions);
        printf(">>> syscall %d\n", v); 
    }
    
    if (v == 10) {
        exit(0);
    } 
    
    if (v == 1 && trace_mode == 1) {
        printf("<<< %d\n", a);
    } 
    else if (v == 11 && trace_mode == 1) {
        a = registersArray[4] & ((1 << 8) - 1);
        printf("<<< %c\n", a);
    } 

    if (v == 1 && trace_mode == 0) {
        printf("%d", a);
    } 
    else if (v == 11 && trace_mode == 0) {
        a = registersArray[4] & ((1 << 8) - 1);
        printf("%c", a);
    }
    
    if (v != 1 && v != 11 && v != 10){
        fprintf(stderr, "Unknown system call: %d\n", v); 
        exit(0); 
    }
}

// addi - check and implementation
int check_addi(uint32_t instructions) {
    // 001000ssssstttttIIIIIIIIIIIIIIII
    //  mask = 00100000000000000000000000000000
    uint32_t mask = 1 << 29;
    uint32_t check = mask & instructions;

    if (check == mask) {
        return TRUE;
    }

    return FALSE;
}
void do_addi(uint32_t instructions, int *registersArray, int trace_mode, uint32_t pc) {

    uint32_t mask = (1 << 5) - 1;
    int s = (instructions >> 21) & mask;
    int t = (instructions >> 16) & mask;
    
    short int I = instructions & (0xFFFF);

    registersArray[t] = registersArray[s] + I;


    if (trace_mode == 1) {
        printf("%u: 0x%08X ", pc, instructions);
        printf("addi $%d, $%d, %d\n", t, s, I);
        printf(">>> $%d = %d\n", t, registersArray[t]);
    } 
}

// add - check and implementation
int check_add(uint32_t instructions) {
    // 	000000ssssstttttddddd00000100000
    uint32_t mask = 1 << 5;
    uint32_t check = mask & instructions;

    if (check == mask) {
        return TRUE;
    }
    return FALSE;
}
void do_add(uint32_t instructions, int *registersArray, int trace_mode, uint32_t pc) {
    // 000000ssssstttttddddd00000100000
    uint32_t mask = (1 << 5) - 1;
    int d = (instructions >> 11) & mask;
    int s = (instructions >> 21) & mask;
    int t = (instructions >> 16) & mask;

    registersArray[d] = registersArray[s] + registersArray[t];

    if (trace_mode == 1) {
        printf("%u: 0x%08X ", pc, instructions);
        printf("add  $%d, $%d, $%d\n", d, s, t);
        printf(">>> $%d = %d\n", d, registersArray[d]);
    }
}

// sub 
int check_sub(uint32_t instructions) {
    // 0...100010
    uint32_t mask = (1 << 5) | (1 << 1);
    uint32_t check = mask & instructions;

    if (check == mask) {
        return TRUE;
    }
    return FALSE;
}
void do_sub(uint32_t instructions, int *registersArray, int trace_mode, uint32_t pc) {
    uint32_t mask = (1 << 5) - 1;
    int d = (instructions >> 11) & mask;
    int s = (instructions >> 21) & mask;
    int t = (instructions >> 16) & mask;

    registersArray[d] = registersArray[s] - registersArray[t];
    
    if (trace_mode == 1) {
        printf("%u: 0x%08X ", pc, instructions);
        printf("sub  $%d, $%d, $%d\n", d, s, t);
        printf(">>> $%d = %d\n", d, registersArray[d]);
    }
}

// mul
int check_mul(uint32_t instructions) {
    uint32_t mask = (((1 << 3) - 1) << 28) | (1 << 1);
    uint32_t check = mask & instructions;

    if (check == mask) {
        return TRUE;
    }
    return FALSE;
}
void do_mul(uint32_t instructions, int *registersArray, int trace_mode, uint32_t pc) {
    uint32_t mask = (1 << 5) - 1;
    int d = (instructions >> 11) & mask;
    int s = (instructions >> 21) & mask;
    int t = (instructions >> 16) & mask; 

    registersArray[d] = (uint32_t) registersArray[s] * (uint32_t) registersArray[t];
    
    if (trace_mode == 1) {
        printf("%u: 0x%08X ", pc, instructions);
        printf("mul  $%d, $%d, $%d\n", d, s, t);
        printf(">>> $%d = %d\n", d, registersArray[t]);
    }
}

// slt
int check_slt(uint32_t instructions) {
    // 0...101010
    uint32_t mask = (1 << 5) | (1 << 3) | (1 << 1);
    uint32_t check = mask & instructions;

    if (check == mask) {
        return TRUE;
    }
    return FALSE;
}
void do_slt(uint32_t instructions, int *registersArray, int trace_mode, uint32_t pc) {
    uint32_t mask = (1 << 5) - 1;
    
    int d = (instructions >> 11) & mask;
    int s = (instructions >> 21) & mask;
    int t = (instructions >> 16) & mask;

    if (registersArray[s] < registersArray[t]) {
        registersArray[d] = 1;
    } else {
        registersArray[d] = 0;
    }
    
    if (trace_mode == 1) {
        printf("%u: 0x%08X ", pc, instructions);
        printf("slt  $%d, $%d, $%d\n", d, s, t);
        printf(">>> $%d = %d\n", d, registersArray[d]);
    }
}

// bne
int check_bne(uint32_t instructions) {
    uint32_t mask = (1 << 28) | (1 << 26);
    uint32_t check = mask & instructions;

    if (check == mask) {
        return TRUE;
    }
    return FALSE;
}
uint32_t do_bne(uint32_t instructions, int *registersArray, int trace_mode, uint32_t pc, uint32_t n_instructions) {
    uint32_t mask = (1 << 5) - 1;

    int s = (instructions >> 21) & mask;
    int t = (instructions >> 16) & mask;

    short int I = (instructions & 0xFFFF);

    if (trace_mode == 1) {
        printf("%u: 0x%08X ", pc, instructions);
    }

    if (registersArray[s] != registersArray[t]) {
        pc += I;
    }

    if (pc > n_instructions || pc < 0) {
        fprintf(stderr, "Illegal branch to non-instruction: PC = %d\n", pc);
        exit(0);
    }

    if (trace_mode == 1) {
        printf("bne  $%d, $%d, %d\n", s, t, I);
        if (registersArray[s] != registersArray[t]) {
            printf(">>> branch taken to PC = %d\n", pc);
            return pc - 1;
        }
        printf(">>> branch not taken\n");
    }

    if (registersArray[s] != registersArray[t]) {
        return pc - 1;
    } else {
        return pc;
    }
}

// beq
int check_beq(uint32_t instructions) {
    // 	000100ssssstttttIIIIIIIIIIIIIIII
    uint32_t mask = 1 << 28;
    uint32_t check = mask & instructions;

    if (check == mask) {
        return TRUE;
    }
    return FALSE;
}
uint32_t do_beq(uint32_t instructions, int *registersArray, int trace_mode, uint32_t pc, uint32_t n_instructions) {
    // mask = 00000000000000000000000000011111
    // s = 000000000000000000000000000sssss
    // t = 000000000000000000000000000ttttt
    // I = 0000000000000000IIIIIIIIIIIIIIII
    uint32_t mask = (1 << 5) - 1;
    int s = (instructions >> 21) & mask;
    int t = (instructions >> 16) & mask;
    short int I = (instructions & 0xFFFF);

    if (trace_mode == 1) {
        printf("%u: 0x%08X ", pc, instructions);
    }

    if (registersArray[s] == registersArray[t]) {
        pc += I;
    }

    if (pc > n_instructions || pc < 0) {
        fprintf(stderr, "Illegal branch to non-instruction: PC = %d\n", pc);
        exit(0);
    }

    if (trace_mode == 1) {
        printf("beq  $%d, $%d, %d\n", s, t, I);
        if (registersArray[s] == registersArray[t]) {
            printf(">>> branch taken to PC = %d\n", pc);
            return pc - 1;
            // return pc;
        }
        printf(">>> branch not taken\n");
    }

    if (registersArray[s] == registersArray[t]) {
        return pc - 1;
    } else {
        return pc;
    }
}

// lui
int check_lui(uint32_t instructions) {
    uint32_t mask = (1 << 29) | (1 << 28) | (1 << 27) | (1 << 26);
    uint32_t check = mask & instructions;

    if (check == mask) {
        return TRUE;
    }
    return FALSE;
}
void do_lui(uint32_t instructions, int *registersArray, int trace_mode, uint32_t pc) {
    uint32_t mask = (1 << 5) - 1;
    // 0...IIIIIIIIIIIIIIII
    int t = (instructions >> 16) & mask;
    short int I = (instructions & 0xFFFF);

    registersArray[t] = I << 16;
    
    if (trace_mode == 1) {
        printf("%u: 0x%08X ", pc, instructions);
        printf("lui  $%d, %d\n", t, I);
        printf(">>> $%d = %d\n", t, registersArray[t]);
    } 
}

// ori
int check_ori(uint32_t instructions) {
    uint32_t mask = (1 << 29) | (1 << 28) | (1 << 26);
    uint32_t check = mask & instructions;

    if (check == mask) {
        return TRUE;
    }
    return FALSE;
}
void do_ori(uint32_t instructions, int *registersArray, int trace_mode, uint32_t pc) {
    // mask = 00000000000000000000000000011111
    // s = 000000000000000000000000000sssss
    // t = 000000000000000000000000000ttttt
    // I = 0000000000000000IIIIIIIIIIIIIIII
    uint32_t mask = (1 << 5) - 1;
    int s = (instructions >> 21) & mask;
    int t = (instructions >> 16) & mask;
    unsigned short int I = (instructions & 0xFFFF);
    short int print = 0xFFFF & instructions;
    
    registersArray[t] = (uint64_t) registersArray[s] | (uint16_t) I;

    if (trace_mode == 1) {
        printf("%u: 0x%08X ", pc, instructions);
        printf("ori  $%d, $%d, %d\n", t, s, print);
        printf(">>> $%d = %d\n", t, registersArray[t]);
    }
}



// DO NOT CHANGE ANY CODE BELOW HERE


// check_arguments is given command-line arguments
// it sets *trace_mode to 0 if -r is specified
//         *trace_mode is set to 1 otherwise
// the filename specified in command-line arguments is returned
char *process_arguments(int argc, char *argv[], int *trace_mode) {
    if (
        argc < 2 ||
        argc > 3 ||
        (argc == 2 && strcmp(argv[1], "-r") == 0) ||
        (argc == 3 && strcmp(argv[1], "-r") != 0)
    ) {
        fprintf(stderr, "Usage: %s [-r] <file>\n", argv[0]);
        exit(1);
    }
    *trace_mode = (argc == 2);
    return argv[argc - 1];
}


// read hexadecimal numbers from filename one per line
// numbers are return in a malloc'ed array
// *n_instructions is set to size of the array
uint32_t *read_instructions(char *filename, uint32_t *n_instructions_p) {
    FILE *f = fopen(filename, "r");
    if (f == NULL) {
        perror(filename);
        exit(1);
    }

    uint32_t *instructions = NULL;
    uint32_t n_instructions = 0;
    char line[MAX_LINE_LENGTH + 1];
    while (fgets(line, sizeof line, f) != NULL) {

        // grow instructions array in steps of INSTRUCTIONS_GROW elements
        if (n_instructions % INSTRUCTIONS_GROW == 0) {
            instructions = instructions_realloc(instructions, n_instructions + INSTRUCTIONS_GROW);
        }

        char *endptr;
        instructions[n_instructions] = (uint32_t)strtoul(line, &endptr, 16);
        if (*endptr != '\n' && *endptr != '\r' && *endptr != '\0') {
            fprintf(stderr, "line %d: invalid hexadecimal number: %s",
                    n_instructions + 1, line);
            exit(1);
        }
        if (instructions[n_instructions] != strtoul(line, &endptr, 16)) {
            fprintf(stderr, "line %d: number too large: %s",
                    n_instructions + 1, line);
            exit(1);
        }
        n_instructions++;
    }
    fclose(f);
    *n_instructions_p = n_instructions;
    // shrink instructions array to correct size
    instructions = instructions_realloc(instructions, n_instructions);
    return instructions;
}


// instructions_realloc is wrapper for realloc
// it calls realloc to grow/shrink the instructions array
// to the specified size
// it exits if realloc fails
// otherwise it returns the new instructions array
uint32_t *instructions_realloc(uint32_t *instructions, uint32_t n_instructions) {
    instructions = realloc(instructions, n_instructions * sizeof *instructions);
    if (instructions == NULL) {
        fprintf(stderr, "out of memory");
        exit(1);
    }
    return instructions;
}
