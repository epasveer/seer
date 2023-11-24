#include <iostream>
#include <string>

int main (int argc, char* argv[]) {

    std::string name = "";

    name = "ernie";

    // Create the breakpoint with a string condition.
    //
    // -break-insert -c 'name.c_str() == "ernie"' gdbmi_condition.cpp:16             // Failed parsing.
    // -break-insert -c '$_streq(name.c_str(), "ernie")' gdbmi_condition.cpp:16      // Failed parsing.
    // -break-insert -c 'strcmp("xxxxx", name.c_str() == 0' gdbmi_condition.cpp:16   // Is accepted but breaks eventhough strings are different.

    std::cout << "Name is: " << name << std::endl;

    return 0;
}

