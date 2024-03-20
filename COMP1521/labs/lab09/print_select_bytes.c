#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {

  FILE* input_stream = fopen(argv[1], "rb");

  int c;
  int position;


  fseek(input_stream, 0, SEEK_END);
  // long nBytes = ftell(input_stream);

  for (int i = 2; i < argc; i++) {
    position = atoi(argv[i]);
    fseek(input_stream, position, SEEK_SET);
    c = getc(input_stream);
    printf("%d - 0x%X - '%c'\n", c, c, c);
  }

  fclose(input_stream);

  return 0;
}