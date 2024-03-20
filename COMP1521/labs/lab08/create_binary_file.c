#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {

  FILE *output = fopen(argv[1], "w");
  if (output == NULL) {
    perror(argv[1]);
    return 0;
  }

  for (int i = 2; i < argc; i++) {
    int j = atoi(argv[i]);
    fputc(j, output);
  }

  return 0;
}