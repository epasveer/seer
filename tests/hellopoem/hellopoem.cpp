#include <iostream>
#include <stdlib.h>

int main (int argc, char* argv[]) {

    std::string poem = "'Treasure' by Lucillius\n\n"
                       "\tThey call thee rich; I deem thee poor;\n"
                       "\tSince, if thou dares not use thy store,\n"
                       "\tBut saves only for thine heirs,\n"
                       "\tThe treasure is not thine, but theirs.\n";

    // Print it.
    std::cout << poem << std::endl;

    return 0;
}

