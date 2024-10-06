#include <stdio.h>
#include <stdlib.h>

int main() {
    char stack_string[10] = "stack";
    int x = 10;
    char *heap_string;

    heap_string = malloc(50);

    printf("Enter a string for the stack: ");
    gets(stack_string);
    printf("Enter a string for the heap: ");
    gets(heap_string);
    printf("Stack string is: %s\n", stack_string);
    printf("Heap string is: %s\n", heap_string);
    printf("x is: %d\n", x);
}

