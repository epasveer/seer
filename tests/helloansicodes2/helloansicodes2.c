#include "printfcolor.h"

int main()
{
    printf("FOREGROUND\n");
    printf("----------\n");
    printfColorFg(GRAY, "Gray text\n");
    printfColorFg(RED, "Red text\n");
    printfColorFg(GREEN, "Green text\n");
    printfColorFg(BLUE, "Blue text\n");
    printfColorFg(CYAN, "Cyan text\n");
    printfColorFg(MAGENTA, "Magenta text\n");
    printfColorFg(YELLOW, "Yellow text\n");
    printfColorFg(WHITE, "White text\n\n");

    printf("BACKGROUND\n");
    printf("----------\n");
    printfColorBg(GRAY, "Gray background\n");
    printfColorBg(RED, "Red background\n");
    printfColorBg(GREEN, "Green background\n");
    printfColorBg(BLUE, "Blue background\n");
    printfColorBg(CYAN, "Cyan background\n");
    printfColorBg(MAGENTA, "Magenta background\n");
    printfColorBg(YELLOW, "Yellow background\n");
    printfColorBg(WHITE, "White background\n\n");

    printf("BACKGROUND AND FOREGROUND\n");
    printf("-------------------------\n");
    printfColor(WHITE, BLUE, "White on blue\n");
    printfColor(RED, YELLOW, "Red on yellow\n");
    printfColor(GRAY, GREEN, "Gray on green\n");
    printfColor(MAGENTA, CYAN, "Magenta on cyan\n");
    printfColor(GREEN, WHITE, "Green on white\n");
    printfColor(GRAY, RED, "Gray on red\n");
}