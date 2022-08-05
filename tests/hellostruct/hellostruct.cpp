#include <string>
#include <iostream>
#include <string.h>

struct Person {
    char            name[50];
    std::string     city;
    int             age;
    float           salary;
};


int main (int argc, char** argv) {

    Person me;

    strcpy(me.name, "Ernie Pasveer");
    me.city   = "Houston";
    me.age    = 60;
    me.salary = 0.25;

    std::cout << "'" << me.name << "', from '" << me.city << "', is " << me.age << " years old and makes " << me.salary << " per year." << std::endl;

    return 0;
}

