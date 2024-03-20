EXERCISES	+= 21t2final_q8
CLEAN_FILES	+= 21t2final_q8 21t2final_q8.o

21t2final_q8:	21t2final_q8.o 21t2final_q8_main.o
21t2final_q8.o:	21t2final_q8.c 21t2final_q8.h 21t2final_q8_opcodes.h
21t2final_q8_main.o: 21t2final_q8_main.c 21t2final_q8.h
