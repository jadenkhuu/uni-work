#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {

    FILE *output_stream = fopen(argv[1], "r");
    if (output_stream == NULL) {
        perror(argv[1]); 
        return 0; 
    }

    FILE *temp_file = fopen("temp", "w"); 
    if (temp_file == NULL) {
        perror("temp"); 
        return 0; 
    }

    int c;
    while ((c = fgetc(output_stream)) != EOF) {
        if (c <= 127 && c >= 0) {
            fputc(c, temp_file); 
        }
    }

    fclose(output_stream);
    fclose(temp_file);

    FILE *output = fopen(argv[1], "w");
    FILE *temp1 = fopen("temp", "r"); 

    int j;
    while((j = fgetc(temp1)) != EOF) {
        fputc(j, output);
    }
    return 0; 
} 