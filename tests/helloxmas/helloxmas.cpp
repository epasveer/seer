#include <stdio.h>
#include <stdlib.h>

int main(void){

    long long test[] = {7512884309367350605,
                        2410377348306397554,
                        8481};

    printf("%s\n", (char *) &test);

    return EXIT_SUCCESS;
}

