#include <string>

struct Location {
    std::string      city;
    std::string      state;
    int              zip;
    unsigned int     cell;
};

struct Person {
    std::string      name;
    int              age;
    float            salary;
    struct Location  location;
};

int main (int argc, char** argv) {

    Location where;
    where.city         = "Houston";
    where.state        = "Texas";
    where.zip          = 77063;
    where.cell         = 2226669999;


    Person me;
    me.name            = "Pasveer, Ernie";
    me.age             = 60;
    me.salary          = 0.25;
    me.location.city   = "Houston";
    me.location.state  = "Texas";
    me.location.zip    = 77063;
    me.location.cell   = 2226669999;

    return 0;
}

