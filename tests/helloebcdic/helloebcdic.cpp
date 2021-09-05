#include <string.h>
#include <stdio.h>
#include <malloc.h>

int main (void) {

    // Should show up as "Hello there!" in Seer Memory Visualizer in ebcdic mode.

    static char str[] = "\xc8\x85\x93\x93\x96\x40\xa3\x88\x85\x99\x85\x5a";

    return 0;
}

