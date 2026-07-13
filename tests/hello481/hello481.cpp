#include <string>
#include <iostream>

int main () {

    double pos[40];
    double vit[40];

    double posv = 1.0;

    for (int i=0; i<sizeof(pos); i++) {

        pos[i] = posv;
        posv -= (1.0 - 0.0) / double(sizeof(pos));
    }

    return 0;
}

