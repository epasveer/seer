#include "function1.h"
#include <string>
#include <iostream>

int main (int argc, char** argv) {

    for (int i=0; i<argc; i++) {
        std::cout << "XX: " << i << " " << argv[i] << std::endl;
    }

    std::string message = "Hello, World!";

    function1(message);

    return 0;
}

