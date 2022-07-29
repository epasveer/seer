#include <string>
#include <iostream>

int main (int argc, char** argv) {

    int i=0;

    for (i=0; i<argc; i++) {
        std::cout << "XX: " << i << " " << argv[i] << std::endl;
    }

    std::cout << "Address of 'i' == " << &i << std::endl;

    return 0;
}

