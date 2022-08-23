#include <string>
#include <iostream>
#include <string.h>
#include <malloc.h>


struct Location {
    std::string      city;
    std::string      state;
    int              zip;
};

struct Person {
    std::string      name;
    int              age;
    float            salary;
    struct Location* location;
};


int main (int argc, char** argv) {

    Person me;

    me.name            = "Pasveer, Ernie";
    me.age             = 60;
    me.salary          = 0.25;

  //me.location = (struct Location*)malloc(sizeof(struct Location));
    me.location = new Location;

    me.location->city   = "Houston";
    me.location->state  = "Texas";
    me.location->zip    = 77063;

    std::cout << "'" << me.name << "', from '" << me.location->city << "', is " << me.age << " years old and makes " << me.salary << " per year." << std::endl;

    return 0;
}

