#include <string.h>
#include <pthread.h>
#include <stdio.h>
#include <unistd.h>

// Global variable:
int i = 2;

void bar (int v) {
    printf("bar: %i\n", v);
    sleep(20);
    printf("bar: %i\n", v);
}

void* foo (void* p) {
    // Print value received as argument:
    printf("foo: Value recevied as argument in starting routine: ");
    printf("%i\n", *(int*)p);

    bar(*(int*)p);

    // Return reference to global variable:
    pthread_exit(&i);
}

int main (void) {
    // Declare variable for thread's ID:
    pthread_t id;

    int j = 1;
    pthread_create(&id, NULL, foo, &j);

    int* ptr;

    // Wait for foo() and retrieve value in ptr;
    pthread_join(id, (void**)&ptr);

    printf("Value recevied by parent from child: ");
    printf("%i\n", *ptr);
}

