#include <iostream>
#include "arithmetic.h"
#include "advanced.h"
#include <chrono>
#include <thread>
#include <unistd.h>
using namespace std;


struct structTetst globalStruct = {-2, "test", {-1, "childTest"}};
/***
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 */
int main() {
    globalStruct.id =0;
    int a = 10, b = 3;
    pid_t pid = getpid();
    std::cout << "Process ID: " << pid << std::endl;
    while (globalStruct.id < 100)
    {
        globalStruct.id++;
        globalStruct.child.childId = globalStruct.id;
        globalStruct.child.childString = "Child String " + std::to_string(globalStruct.id);
        cout << "Basic Arithmetic:" << endl;
        cout << a << " + " << b << " = " << add(a, b) << endl;
        cout << a << " - " << b << " = " << subtract(a, b) << endl;
        cout << a << " * " << b << " = " << multiply(a, b) << endl;
        cout << a << " / " << b << " = " << divide(a, b) << endl;

        cout << "\nAdvanced Math:" << endl;
        cout << "Factorial of 5 = " << factorial(5) << endl;
        cout << "Fibonacci of 7 = " << fibonacci(7) << endl;
        globalStruct.child.childId ++;
        std::this_thread::sleep_for(std::chrono::seconds(2));
    }
    return 0;
}