#ifndef ADVANCED_H
#define ADVANCED_H

typedef struct {
    int age;
    char name[32];
} childStruct;

typedef struct {
    int age;
    char name[32];
    childStruct child;
} parentStruct;

typedef struct {
    parentStruct dad;
    int numberOfMember;
    char familyName[32];
    parentStruct mom;
} familyStruct;

int factorial(int n);
int fibonacci(int n);

#endif
