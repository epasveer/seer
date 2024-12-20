#include <string>
#include <iostream>
#include <iomanip>
#include <stdio.h>
#include <stdarg.h>

FILE* logfile = NULL;

extern "C" {
    int log_msg (const char* format, ...) {
        int n;
        va_list args;
        // init the variable length argument list
        va_start(args, format);

        n = vfprintf(logfile, format, args);

        // cleanup the variable length argument list
        va_end(args);

        return n;
    }
}

int main (int argc, char** argv) {

    const char* filename = "logfile.txt";
    logfile = fopen(filename, "w");
    if (logfile == NULL) {
        fprintf(stderr, "Failed to open logfile: %s\n", filename);
        return 1;
    }

    int count = 10;
    int nfact = 1;
    int n;

    std::cout << " C++ loop" << std::endl;
    for (n=1; n<=count; n++) {
        nfact = nfact * n;
        // printing the value of n and its factorial
        std::cout << std::setw(12)  << n << std::setw(14) << nfact << std::endl;
    }

    fclose(logfile);

    return 0;
}

