/* Factored discrete Fourier transform, or FFT, and its inverse iFFT */

#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#define q   15          /* for 2^3 points */
#define N   (1<<q)      /* N-point FFT, iFFT */

typedef float real;
typedef struct{real Re; real Im;} complex;

#ifndef PI
# define PI 3.14159265358979323846264338327950288
#endif


/* Print a vector of complexes as ordered pairs. */
static void print_vector( const char *title, complex *x, int n) {

    int i;

    printf("%s (dim=%d):", title, n);

    for(i=0; i<(n<8?n:8); i++ ) {
        printf(" %5.2f,%5.2f ", x[i].Re,x[i].Im);
    }

    putchar('\n');

    return;
}

/* 
   fft(v,N):
   [0] If N==1 then return.
   [1] For k = 0 to N/2-1, let ve[k] = v[2*k]
   [2] Compute fft(ve, N/2);
   [3] For k = 0 to N/2-1, let vo[k] = v[2*k+1]
   [4] Compute fft(vo, N/2);
   [5] For m = 0 to N/2-1, do [6] through [9]
   [6]   Let w.re = cos(2*PI*m/N)
   [7]   Let w.im = -sin(2*PI*m/N)
   [8]   Let v[m] = ve[m] + w*vo[m]
   [9]   Let v[m+N/2] = ve[m] - w*vo[m]
 */
void fft( complex *v, int n, complex *tmp ) {

    if(n>1) {         /* otherwise, do nothing and return */
        int k,m;    complex z, w, *vo, *ve;
        ve = tmp; vo = tmp+n/2;
        for(k=0; k<n/2; k++) {
            ve[k] = v[2*k];
            vo[k] = v[2*k+1];
        }
        fft( ve, n/2, v );      /* FFT on even-indexed elements of v[] */
        fft( vo, n/2, v );      /* FFT on odd-indexed elements of v[] */
        for(m=0; m<n/2; m++) {
            w.Re = cos(2*PI*m/(double)n);
            w.Im = -sin(2*PI*m/(double)n);
            z.Re = w.Re*vo[m].Re - w.Im*vo[m].Im; /* Re(w*vo[m]) */
            z.Im = w.Re*vo[m].Im + w.Im*vo[m].Re; /* Im(w*vo[m]) */
            v[  m  ].Re = ve[m].Re + z.Re;
            v[  m  ].Im = ve[m].Im + z.Im;
            v[m+n/2].Re = ve[m].Re - z.Re;
            v[m+n/2].Im = ve[m].Im - z.Im;
        }
    }
    return;
}

/* 
   ifft(v,N):
   [0] If N==1 then return.
   [1] For k = 0 to N/2-1, let ve[k] = v[2*k]
   [2] Compute ifft(ve, N/2);
   [3] For k = 0 to N/2-1, let vo[k] = v[2*k+1]
   [4] Compute ifft(vo, N/2);
   [5] For m = 0 to N/2-1, do [6] through [9]
   [6]   Let w.re = cos(2*PI*m/N)
   [7]   Let w.im = sin(2*PI*m/N)
   [8]   Let v[m] = ve[m] + w*vo[m]
   [9]   Let v[m+N/2] = ve[m] - w*vo[m]
 */
void ifft( complex *v, int n, complex *tmp ) {

    if(n>1) {         /* otherwise, do nothing and return */
        int k,m;    complex z, w, *vo, *ve;
        ve = tmp; vo = tmp+n/2;
        for(k=0; k<n/2; k++) {
            ve[k] = v[2*k];
            vo[k] = v[2*k+1];
        }
        ifft( ve, n/2, v );     /* FFT on even-indexed elements of v[] */
        ifft( vo, n/2, v );     /* FFT on odd-indexed elements of v[] */
        for(m=0; m<n/2; m++) {
            w.Re = cos(2*PI*m/(double)n);
            w.Im = sin(2*PI*m/(double)n);
            z.Re = w.Re*vo[m].Re - w.Im*vo[m].Im; /* Re(w*vo[m]) */
            z.Im = w.Re*vo[m].Im + w.Im*vo[m].Re; /* Im(w*vo[m]) */
            v[  m  ].Re = ve[m].Re + z.Re;
            v[  m  ].Im = ve[m].Im + z.Im;
            v[m+n/2].Re = ve[m].Re - z.Re;
            v[m+n/2].Im = ve[m].Im - z.Im;
        }
    }
    return;
}

int main(void) {

    complex v[N], v1[N], scratch[N];
    int     k;

    for (int x=0; x<10000; x++) {
        /* Fill v[] with a function of known FFT: */
        for(k=0; k<N; k++) {
            v[k].Re  =  0.125 * cos(2*PI*k/(double)N);
            v[k].Im  =  0.125 * sin(2*PI*k/(double)N);
            v1[k].Re =  0.3   * cos(2*PI*k/(double)N);
            v1[k].Im = -0.3   * sin(2*PI*k/(double)N);
        }

        /* FFT, iFFT of v[]: */
        print_vector("Orig", v, N);
        fft( v, N, scratch );
        print_vector(" FFT", v, N);
        ifft( v, N, scratch );
        print_vector("iFFT", v, N);

        /* FFT, iFFT of v1[]: */
        print_vector("Orig", v1, N);
        fft( v1, N, scratch );
        print_vector(" FFT", v1, N);
        ifft( v1, N, scratch );
        print_vector("iFFT", v1, N);
    }

    exit(EXIT_SUCCESS);
}

