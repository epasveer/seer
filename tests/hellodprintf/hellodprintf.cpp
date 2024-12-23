#include <string>
#include <iostream>
#include <iomanip>
#include <stdio.h>
#include <stdarg.h>

int main (int argc, char** argv) {

    int count = 10;
    int nfact = 1;
    int n;

    std::cout << " C++ loop" << std::endl;
    for (n=1; n<=count; n++) {
        nfact = nfact * n;
        // printing the value of n and its factorial
        std::cout << std::setw(12)  << n << std::setw(14) << nfact << std::endl;
    }

    return 0;
}

