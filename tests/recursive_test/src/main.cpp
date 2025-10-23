#include <iostream>
#include "arithmetic.h"
#include "advanced.h"
#include <chrono>
#include <thread>
#include <unistd.h>
using namespace std;

int main() {
    int i =0;
    int a = 10, b = 3;
    pid_t pid = getpid();
    std::cout << "Process ID: " << pid << std::endl;
    while (i<100)
    {
        i++;
        cout << "Basic Arithmetic:" << endl;
        cout << a << " + " << b << " = " << add(a, b) << endl;
        cout << a << " - " << b << " = " << subtract(a, b) << endl;
        cout << a << " * " << b << " = " << multiply(a, b) << endl;
        cout << a << " / " << b << " = " << divide(a, b) << endl;

        cout << "\nAdvanced Math:" << endl;
        cout << "Factorial of 5 = " << factorial(5) << endl;
        cout << "Fibonacci of 7 = " << fibonacci(7) << endl;
        std::this_thread::sleep_for(std::chrono::seconds(20));
    }
    return 0;
}