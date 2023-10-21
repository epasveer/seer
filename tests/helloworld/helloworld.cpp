
#include <string>
#include <iostream>

// argc         == number of arguments passed to the program.
// argv         == argument array.
// j            == some misc integer.
// i            == loop counter for printing arguments.
// message      == say hi to the world.
// function1    == a function to call to simulate work.

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

