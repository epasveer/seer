#include <string>
#include <iostream>

int main (int argc, char* argv[]) {

    int j = 0;

    for (int i=0; i<argc; i++) {
        std::cout << "XX: "
                  << i
                  << " "
                  << argv[i]
                  << std::endl;
    }

    std::string message = "Hello, World!";

    std::cout << std::endl;
    std::cout << message << std::endl;
    std::cout << std::endl;

    return 0;
}

