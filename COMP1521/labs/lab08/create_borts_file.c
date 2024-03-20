#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

int main (int argc, char *argv[]) {

  FILE *output_stream = fopen(argv[1], "w");
  if (output_stream == NULL) {
    perror(argv[1]);
    return 0;
  }

  for (int num = atoi(argv[2]); num <= atoi(argv[3]); num++) {
    uint16_t new = num >> 8;
    fputc(new, output_stream);

    uint16_t mask = 0xFF;
    new = mask & num;
    fputc(new, output_stream);
  }

  fclose(output_stream);
  return 0;

}