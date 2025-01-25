#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

    uC_console_reset_attrs();
    uC_clear();
    uC_cup(10, 0);

    uCurses_deInit();
}

