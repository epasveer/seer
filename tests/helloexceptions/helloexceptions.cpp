#include <iostream>
#include <stdexcept>

int AddPositiveIntegers (int a, int b) {

    if (a < 0 || b < 0) {
        throw std::invalid_argument("AddPositiveIntegers arguments must be positive");
    }

    return a + b;
}

int main() {

    try {

        std::cout << AddPositiveIntegers(-1, 2); //exception

    } catch (std::invalid_argument& e) {

        std::cerr << e.what() << std::endl;

        return -1;
    }

    return 0;
}

