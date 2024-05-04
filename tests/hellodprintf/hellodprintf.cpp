#include <string>
#include <iostream>
#include <iomanip>

extern "C" {
void fortranfunction_ (int* count);
}

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

    fortranfunction_(&count);

    return 0;
}

