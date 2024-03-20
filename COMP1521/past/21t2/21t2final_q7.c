// COMP1521 21T2 ... final exam, question 7

#include <sys/types.h>
#include <sys/stat.h>

#include <dirent.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const mode_t NEW_DIR_MODE = S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH;

void
cp_directory (char *dir_from, char *dir_to)
{
	// TODO
	// hint:
	// - `man 3 opendir`
	// - `man 3 readdir`
	// - `man 3 closedir`
	// - `man 2 mkdir`
}
