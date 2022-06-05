#include <string.h>
#include <stdio.h>
#include <malloc.h>
#include <math.h>
#include <iostream>

int main (void) {

    float xyz[] = {
        0.0, 0.0, 3.0,
        1.0, 4.0, 0.0,
        0.0, 1.0, 0.0,
        4.0, 0.0, 3.0,
        1.0, 4.0, 7.0,
        0.0, 6.0, 0.0
    };

    int npoints = sizeof(xyz) / (3 * sizeof(float));

    printf("Hello surface!\n");
    printf("# points = %d\n", npoints);

    for (int i=0,j=0; i<npoints; i++) {
        printf ("%3.1f %3.1f %3.1f\n", xyz[j], xyz[j+1], xyz[j+2]);

        j += 3;
    }

    return 0;
}

