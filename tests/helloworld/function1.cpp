#include "function1.h"
#include <iostream>
#include <unistd.h>

void function1 (const std::string& text) {

    int i = 42;

    sleep(2); // Simulate doing work.

    std::cout << text << i << std::endl;

    sleep(3); // Simulate doing work.
}

