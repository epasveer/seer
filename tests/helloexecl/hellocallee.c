#include <stdio.h>

int main(int argc, char *argv[0]) {

    for (int i = 1; i < argc; ++i) {
        if (i != 1) printf(" ");
        printf("%s", argv[i]);
    }

    printf("\n");

    return 0;
}

