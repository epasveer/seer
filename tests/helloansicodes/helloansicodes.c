#include "ANSI-color-codes.h"
#include <stdio.h>

int main() {

    printf("STARTING ANSI CODE TESTS.\n\n");

    printf(BRED "Hey this is the color red, and it's bold! \n" CRESET);
    printf(RED "If " BLU "you " YEL "are " GRN "bored " CYN "do " MAG "this! \n" CRESET);
    printf(BRED "If " BBLU "you " BYEL "are " BGRN "bored " BCYN "do " BMAG "this! \n" CRESET);
    printf(URED "If " UBLU "you " UYEL "are " UGRN "bored " UCYN "do " UMAG "this! \n" CRESET);

    printf("\n");
    printf(" \033[4;31mLight Colors\033[0m \t\t\t  \033[1;4;31mDark Colors\033[0m\n");
    printf(" \033[0;30;47m Black     \033[0m   0;30m \t\t \033[1;30;40m Dark Gray   \033[0m  1;30m\n");
    printf(" \033[0;31;47m Red       \033[0m   0;31m \t\t \033[1;31;40m Dark Red    \033[0m  1;31m\n");
    printf(" \033[0;32;47m Green     \033[0m   0;32m \t\t \033[1;32;40m Dark Green  \033[0m  1;32m\n");
    printf(" \033[0;33;47m Brown     \033[0m   0;33m \t\t \033[1;33;40m Yellow      \033[0m  1;33m\n");
    printf(" \033[0;34;47m Blue      \033[0m   0;34m \t\t \033[1;34;40m Dark Blue   \033[0m  1;34m\n");
    printf(" \033[0;35;47m Magenta   \033[0m   0;35m \t\t \033[1;35;40m Dark Magenta\033[0m  1;35m\n");
    printf(" \033[0;36;47m Cyan      \033[0m   0;36m \t\t \033[1;36;40m Dark Cyan   \033[0m  1;36m\n");
    printf(" \033[0;37;47m Light Gray\033[0m   0;37m \t\t \033[1;37;40m White       \033[0m  1;37m\n");

    return 0;
}

