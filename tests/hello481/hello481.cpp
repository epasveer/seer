// damped_oscillator
//
// Simulates a damped harmonic oscillator:
//     x'' + 2*zeta*omega0*x' + omega0^2*x = 0
//
// Stores a 4000-point dataset directly into two double arrays:
//     X[i] = position at step i
//     Y[i] = velocity at step i
//

#include <iostream>
#include <cmath>
#include <iomanip>

const int N = 4000;   // number of data points

double X[N];  // position array
double Y[N];  // velocity array

struct State {
    double x;  // position
    double v;  // velocity
};

class DampedOscillator {
    public:
        DampedOscillator(double omega0, double zeta) : omega0_(omega0), zeta_(zeta) {}

        // dx/dt, dv/dt
        State derivatives(const State& s) const {
            State d;
            d.x = s.v;
            d.v = -2.0 * zeta_ * omega0_ * s.v - omega0_ * omega0_ * s.x;
            return d;
        }

        // Single RK4 step
        State step(const State& s, double dt) const {
            State k1 = derivatives(s);

            State s2{ s.x + 0.5 * dt * k1.x, s.v + 0.5 * dt * k1.v };
            State k2 = derivatives(s2);

            State s3{ s.x + 0.5 * dt * k2.x, s.v + 0.5 * dt * k2.v };
            State k3 = derivatives(s3);

            State s4{ s.x + dt * k3.x, s.v + dt * k3.v };
            State k4 = derivatives(s4);

            State result;
            result.x = s.x + (dt / 6.0) * (k1.x + 2.0 * k2.x + 2.0 * k3.x + k4.x);
            result.v = s.v + (dt / 6.0) * (k1.v + 2.0 * k2.v + 2.0 * k3.v + k4.v);
            return result;
        }

    private:
        double omega0_; // natural angular frequency (rad/s)
        double zeta_;   // damping ratio (0 = undamped, 1 = critically damped)
};

int main() {

    // ---- Simulation parameters (tweak as you like) ----
    const double dt      = 0.01;   // time step (s)
    const double omega0  = 2.0;    // natural angular frequency (rad/s)
    const double zeta    = 0.05;   // damping ratio (underdamped)
    const double x0      = 1.0;    // initial position
    const double v0      = 0.0;    // initial velocity

    DampedOscillator osc(omega0, zeta);
    State state{ x0, v0 };

    // Fill the arrays instead of writing to a file
    for (int i = 0; i < N; ++i) {
        X[i] = state.x;
        Y[i] = state.v;
        state = osc.step(state, dt);
    }

    // Optional: print a few samples to confirm it worked
    std::cout << std::fixed << std::setprecision(6);
    std::cout << "First 5 points (X = position, Y = velocity):\n";

    for (int i = 0; i < 5; ++i) {
        std::cout << "  X[" << i << "]=" << X[i] << "  Y[" << i << "]=" << Y[i] << "\n";
    }

    std::cout << "Last point: X[" << N-1 << "]=" << X[N-1] << "  Y[" << N-1 << "]=" << Y[N-1] << "\n";

    return 0;
}

