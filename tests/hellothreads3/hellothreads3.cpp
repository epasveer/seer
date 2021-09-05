#include <string.h>
#include <pthread.h>
#include <stdio.h>
#include <unistd.h>

// Global variable:
int iA = 2;
int iB = 5;

void barA (int v) {
    printf("barA: %i\n", v);
    sleep(20);
    printf("barA: %i\n", v);
}

void barB (int v) {
    printf("barB: %i\n", v);
    sleep(20);
    printf("barB: %i\n", v);
}


void* fooA (void* p) {
    // Print value received as argument:
    printf("fooA: Value recevied as argument in starting routine: ");
    printf("%i\n", *(int*)p);

    barA(*(int*)p);

    // Return reference to global variable:
    pthread_exit(&iA);
}

void* fooB (void* p) {
    // Print value received as argument:
    printf("fooB: Value recevied as argument in starting routine: ");
    printf("%i\n", *(int*)p);

    barB(*(int*)p);

    // Return reference to global variable:
    pthread_exit(&iB);
}

int main (void) {
    // Declare variable for thread's ID:
    pthread_t idA;
    pthread_t idB;

    int jA = 1;
    int jB = 4;

    pthread_create(&idA, NULL, fooA, &jA);
    pthread_create(&idB, NULL, fooB, &jB);

    int* ptrA;
    int* ptrB;

    // Wait for fooA() and retrieve value in ptr;
    pthread_join(idA, (void**)&ptrA);

    // Wait for fooB() and retrieve value in ptr;
    pthread_join(idB, (void**)&ptrB);

    printf("Value recevied by parent from child: ");
    printf("%i\n", *ptrA);

    printf("Value recevied by parent from child: ");
    printf("%i\n", *ptrB);

    return 0;
}

