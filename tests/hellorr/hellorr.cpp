#include <stdio.h>

void print_loop(void) {
    printf("Hello!\n");
}

int main() {

    for(int i = 0; i < 5; i++) {
        print_loop();
    }

    return 0;
}

