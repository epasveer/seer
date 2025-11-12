#ifndef ADVANCED_H
#define ADVANCED_H
#include <iostream>
#include <iostream>
#include <chrono>
#include <unistd.h>
#include <string>
#include <vector>

struct childStruct
{
    int age;
    std::string name;
};

struct parentStruct
{
    int age;
    std::string name;
    childStruct childArr[100];
};

struct familyStruct
{
    parentStruct dad;
    int numberOfNumber;
    std::string familyName;
    parentStruct mom;
};

int factorial(int n);
int fibonacci(int n);

#endif
