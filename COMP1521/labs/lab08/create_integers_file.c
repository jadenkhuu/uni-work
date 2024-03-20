#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {

  FILE *output = fopen(argv[1], "w");
  if (output == NULL) {
    perror(argv[1]);
    return 1;
  }

  for (int i = atoi(argv[2]); i <= atoi(argv[3]); i++) {
    fprintf(output, "%d\n", i);
  }

  fclose(output);

  return 0;
}