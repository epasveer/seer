#include <string>
#include <iostream>
#include <string.h>


struct Cell {
    unsigned int     number;
};

struct Location {
    std::string      city;
    std::string      state;
    int              zip;
    struct Cell*     cell;
};

struct Person {
    std::string      name;
    int              age;
    float            salary;
    struct Location  location;
};


int main (int argc, char** argv) {

    Person me;

    me.name                  = "Pasveer, Ernie";
    me.age                   = 60;
    me.salary                = 0.25;
    me.location.city         = "Houston";
    me.location.state        = "Texas";
    me.location.zip          = 77063;
    me.location.cell         = (Cell*)malloc(sizeof(Cell));
    me.location.cell->number = 2813877580;

    std::cout << "'" << me.name << "', from '" << me.location.city << "', is " << me.age << " years old and makes " << me.salary << " per year." << std::endl;

    return 0;
}

