#ifndef ADVANCED_H
#define ADVANCED_H
#include <iostream>
#include <iostream>
#include <chrono>
#include <unistd.h>
#include <string>
struct childStruct
{
    int childId;
    std::string childString;
};

struct structTetst
{
    int id;
    std::string b;
    childStruct child;
};

int factorial(int n);
int fibonacci(int n);

#endif
