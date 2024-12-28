#include <string>
#include <iostream>
#include <iomanip>
#include <stdio.h>
#include <stdarg.h>

extern "C" {
    void fortranfunction_ (int* count);
}

int main (int argc, char** argv) {

    int count = 10;

    fortranfunction_(&count);

    return 0;
}

