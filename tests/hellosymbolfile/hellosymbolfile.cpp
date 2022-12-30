#include <string>
#include <iostream>

void function1 (const std::string& message);

int main (int argc, char** argv) {

    int j = 0;

    for (int i=0; i<argc; i++) {
        std::cout << "XX: "
                  << i
                  << " "
                  << argv[i]
                  << std::endl;
    }

    std::string message = "Hello, World!";

    j++;

    function1(message);

    return 0;
}

