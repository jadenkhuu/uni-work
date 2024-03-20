#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <ctype.h> 

int main(int argc, char *argv[]) {
  FILE *output = fopen(argv[1], "r");
  if (output == NULL) {
    perror(argv[1]);
    return 0;
  }

  long int i = 0;
  int c;

  while ((c = fgetc(output)) != EOF) {
    if (isprint(c) == 0) {
      printf("byte %4ld: %3d 0x%02x ", i, c, c);
      printf("\n");
    } else {
      printf("byte %4ld: %3d 0x%02x ", i, c, c);
      printf("'%c'\n", c);
    }
    i++;
  }

  fclose(output);

  return 0;
}