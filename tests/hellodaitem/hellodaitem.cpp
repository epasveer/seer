#include "DaItem.h"
#include <iostream>

void function1 (DaItem& item) {

    std::cout << item.name() << std::endl;
}


int main (int argc, char** argv) {

    DaItem item("TEST", 100, DT::IntegerType, 1000, 2.0);

    function1(item);

    return argc;
}

