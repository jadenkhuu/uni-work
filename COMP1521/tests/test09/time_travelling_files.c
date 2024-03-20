/*
    This file is intentionally provided (almost) empty.
    Remove this comment and add your code.
*/
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main(int argc, char *argv[]) {
    
    time_t now = time(NULL);

    struct stat s;
    for (int i = 1; i < argc; i++) {
        if (stat(argv[i], &s) != 0) {
            perror(argv[i]);
            exit(1);
        }
        if (s.st_atime > now || s.st_mtime > now) {
            printf("%s has a timestamp that is in the future\n", argv[i]);
        }
    }
    return 0;
}