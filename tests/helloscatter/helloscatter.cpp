#include <cstdio>

#define NPOINTS 4000

double pos[NPOINTS];
double vel[NPOINTS];

//
// Fill pos/vel with the trajectory of a damped harmonic oscillator
// (semi-implicit Euler integration).
//
void integrate (double damping) {

    double x  = 1.0;
    double v  = 0.0;
    double w  = 1.0;
    double dt = 0.005;

    for (int i=0; i<NPOINTS; i++) {
        v += (-w*w*x - damping*v) * dt;
        x += v * dt;

        pos[i] = x;
        vel[i] = v;
    }
}

//
// Exercises the Array Visualizer with a large two-array scatter plot.
//
// Open the Array Visualizer, enter 'pos' as array A and 'vel' as array B,
// length 4000, select B's axis as 'Y', scatter mode, and check 'Auto'.
// Then set a breakpoint on the printf below and 'Continue' a few times:
// each stop recomputes the trajectory with a different damping and
// refreshes the 4000-point phase portrait.
//
int main (void) {

    for (int run=0; run<10; run++) {

        double damping = 0.05 + 0.05*run;

        integrate(damping);

        printf("run %d: damping=%.2f  pos[end]=%f\n", run, damping, pos[NPOINTS-1]);
    }

    return 0;
}
