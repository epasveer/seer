#include <string.h>
#include <pthread.h>
#include <stdio.h>
#include <unistd.h>

// Global variable:
int i = 2;

int bar (int v) {

    printf("bar: %i\n", v);

    v += 10;

    sleep(10);

    v += 10;

    return v;
}

int foo (int p) {
    // Print value received as argument:
    printf("foo: Value recevied as argument in starting routine: ");
    printf("%i\n", p);

    return bar(p);
}

int main (void) {

    int j;

    j = foo(1);

    printf("Value recevied by parent from function: ");
    printf("%i\n", j);
}

