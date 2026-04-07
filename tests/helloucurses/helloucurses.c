#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "uCurses.h"
#include "uC_terminfo.h"
#include "uC_attribs.h"

int main (int argc, char* argv[]) {

    // not defining any windows, just used to position cursor
    // on the screen
    uCurses_init();
    uC_clear();
    uC_cup(5,5);
    uC_terminfo_flush();

    write(1, "Hello", 5);

    uC_console_reset_attrs();
    uC_cup(10, 0);

    uCurses_deInit();
}

