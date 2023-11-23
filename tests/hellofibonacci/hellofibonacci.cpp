#include <iostream>
#include <stdlib.h>

int main (int argc, char* argv[]) {

    if (argc != 2) {
        std::cout << "usage: " << argv[0] << " N" << std::endl;
        return 1;
    }

    int t1       = 0;
    int t2       = 1;
    int nextTerm = 0;
    int n        = atoi(argv[1]);

    // Check N
    if (n < 0) {
        std::cout << "Bad value of 'N': " << argv[1] << std::endl;
        return 1;
    }

    // Displays the first two terms which is always 0 and 1
    std::cout << "Fibonacci Series: " << t1 << ", " << t2 << ", ";

    // Display next terms.
    nextTerm = t1 + t2;

    while (nextTerm <= n) {
        std::cout << nextTerm << ", ";
        t1 = t2;
        t2 = nextTerm;
        nextTerm = t1 + t2;
    }

    // Final EOL
    std::cout << std::endl;

    return 0;
}

