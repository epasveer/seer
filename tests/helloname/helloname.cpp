#include <string>
#include <iostream>

int main (int argc, char** argv) {

    std::string name;

    std::cout << "Hello. Please enter your name : ";

    std::cin >> name;

    std::cout << "Thanks " << name << "!" << std::endl;

    return 0;
}

