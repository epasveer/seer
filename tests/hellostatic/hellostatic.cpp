#include <string>
#include <iostream>
#include <string.h>


struct Location {
    std::string     city;
    std::string     state;
    int             zip;
};

struct Person {
    std::string     name;
    int             age;
    float           salary;
    struct Location location;
};

static Person me;

int main (int argc, char** argv) {


    me.name            = "Pasveer, Ernie";
    me.age             = 60;
    me.salary          = 0.25;
    me.location.city   = "Houston";
    me.location.state  = "Texas";
    me.location.zip    = 77063;

    std::cout << "'" << me.name << "', from '" << me.location.city << "', is " << me.age << " years old and makes " << me.salary << " per year." << std::endl;

    return 0;
}

