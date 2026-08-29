#include <cstdio>

#define NROWS 100
#define NCOLS 100

// A large matrix, stored row by row (C layout).
float big[NROWS][NCOLS];

// A small 3x4 matrix with speaking values: the tens digit is the row (1..3),
// the units digit is the column (1..4). Stored row by row:
//   11 12 13 14
//   21 22 23 24
//   31 32 33 34
double smallRowMajor[12];

// The very same 3x4 matrix, but stored column by column (Fortran, Eigen).
double smallColMajor[12];

void fillSmall () {

    for (int r=0; r<3; r++) {
        for (int c=0; c<4; c++) {
            smallRowMajor[r*4 + c] = (r+1)*10 + (c+1);
            smallColMajor[c*3 + r] = (r+1)*10 + (c+1);
        }
    }
}

void fillBig (int step) {

    for (int r=0; r<NROWS; r++) {
        for (int c=0; c<NCOLS; c++) {
            big[r][c] = (float)((r*NCOLS + c + step) % 1000);
        }
    }
}

//
// Exercises the Matrix Visualizer with a large table and with both memory
// layouts.
//
// Set a breakpoint on the printf below, then open the Matrix Visualizer and
// try, one at a time:
//
//  * '&big[0][0]', rows 100, columns 100, type 'float32', 'row-major'.
//    Check 'Auto' and 'Continue' a few times: each stop refreshes a
//    10000 cell table.
//
//  * The same 10000 elements shown as a wide matrix: rows 10 and
//    columns 1000 (or rows 5 and columns 2000). Exercises a table with
//    many columns, which stresses the rebuild differently than the
//    square layout.
//
//  * '&smallRowMajor[0]', rows 3, columns 4, type 'float64', 'row-major'.
//    The table must read 11 12 13 14 / 21 22 23 24 / 31 32 33 34.
//
//  * '&smallColMajor[0]', rows 3, columns 4, type 'float64',
//    'column-major'. It must show exactly the same table as smallRowMajor.
//
int main (void) {

    fillSmall();

    for (int step=0; step<10; step++) {

        fillBig(step);

        printf("step %d: big[0][0]=%.0f big[%d][%d]=%.0f\n", step, big[0][0], NROWS-1, NCOLS-1, big[NROWS-1][NCOLS-1]);
    }

    return 0;
}
