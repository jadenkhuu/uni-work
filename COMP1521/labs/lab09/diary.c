#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {

  char diaryPath[1000];
  // environment
  char *directoryPath = getenv("HOME");


  // file path name
  snprintf(diaryPath, 1000, "%s/.diary", directoryPath);

  // file path in append mode
  FILE *file;
  file = fopen(diaryPath, "a");

  int i = 1;

  while (i < argc) {
    fprintf(file, "%s ", argv[i]);
    i++;
  }

  fprintf(file, "%s", "\n");

  return 0;
}