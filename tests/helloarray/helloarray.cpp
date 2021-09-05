#include <string.h>
#include <stdio.h>
#include <malloc.h>

int main (void) {

    char* array = (char*)malloc(256);

    for (int i=0; i<256; i++) {
        array[i] = i;
    }

    int* int_array = (int*)malloc(256);

    for (int i=0; i<64; i++) {
        int_array[i] = i;
    }

    printf("Hello array!\n");

    free(array);
    free(int_array);

    return 0;
}

